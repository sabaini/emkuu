import threading
import time
import logging
import pdb
import collections
import Queue
import socket
from xml import sax
from emkuupy import ct
from emkuupy import messages

DEFAULTPORT=23456
MAXQ_SIZE=64*1024 # nr. of items
SKT_TIMEOUT=0.5 # sec
ACK_TIMEOUT=5.0 # sec, after that an error is thrown
RECVBUF=4096 # bytes

log = logging.getLogger("emkuupy")

class EmkuuException(Exception):
    """Error in emkuu client"""

class MyThread(threading.Thread):
    def __init__(self, name=None):
        threading.Thread.__init__(self, name=name)
        self.setDaemon(True)
        self.stopevt = threading.Event()

    def stop(self):
        print "stop %s" % self.name
        self.stopevt.set()

class _ReaderThread(MyThread):
    def __init__(self, skt):
        MyThread.__init__(self, name='_ReaderThread')
        self.skt = skt
        self.recvq = Queue.Queue(MAXQ_SIZE)
        self.okq = Queue.Queue(MAXQ_SIZE)
        self.initq = Queue.Queue(1)
        self.parser = sax.make_parser()
        self.parser.setFeature(sax.handler.feature_namespaces, 1)
        def msghandler(msg):
            if isinstance(msg, messages.OkMsg):
                log.debug("got ok %s" % msg.msgid)
                self.okq.put(msg.msgid, True)
            else:
                log.debug("got msg %s" % msg.msgid)
                self.recvq.put(msg, True)
        def inithdlr(s):
            log.debug("got init %s" % s)
            idtxt, idnum = s.split(":")
            self.initq.put([idtxt, long(idnum)])
        hdlr = ct.EmkuuContentHandler(msghandler, inithdlr)
        self.parser.setContentHandler(hdlr)
        self.start()
        log.debug("init done")

    def get(self):
        return self.recvq.get(True)

    def run(self):
        while not self.stopevt.isSet():
            try:
                data = self.skt.recv(4096)
                self.parser.feed(data)
            except socket.timeout:
                pass
            except Exception, det:
                log.info("read/parse exc: %s", det)

class _WriterThread(MyThread):
    def __init__(self, skt):
        MyThread.__init__(self, name='_WriterThread')
        self.skt = skt
        self.sendq = Queue.Queue(MAXQ_SIZE)
        self.start()
        log.debug("init done")

    def put(self, msg):
        self.sendq.put(msg, True)

    def run(self):
        q = self.sendq
        while not self.stopevt.isSet():
            try:
                msg = q.get(True)
                self.skt.sendall(msg.to_xml())
            except socket.timeout:
                pass
            except:
                log.info("socket error", exc_info=1)
                self.stop()

class _CbHandler:

    def __init__(self):
        self.cbmap = {}
    def __call__(self, msg):
        log.debug("handle msgid %s, corrid %s" % (msg.msgid, msg.corrid))
        cb = self.cbmap.get(msg.corrid)
        log.debug("got cb: %s" % cb)
        # xxx hm, we should probably delete the mapping now...
        if callable(cb):
            try:
                log.debug("is callable")
                cb(msg)
            except:
                logging.error("Exception in callback", exc_info=1)
        else:
            #import pdb; pdb.set_trace()
            return msg


class _WorkerThread(MyThread):

    handler = lambda n: n # dummy handler

    def __init__(self, source, writer):
        MyThread.__init__(self, name='_WorkerThread')
        self.source = source
        self.writer = writer
        self.listeners = [_CbHandler()]
        self.lck = threading.RLock()
        self.start()

    def set_callback(self, corrid, cb):
        try:
            self.lck.acquire()
            log.debug("set callback for corrid %s to %s" % (corrid, cb))
            self.listeners[0].cbmap[corrid] = cb
        finally:
            self.lck.release()

    def add_listeners(self, l):
        try:
            self.lck.acquire()
            self.listeners.append(l)
        finally:
            self.lck.release()

    def sndok(self, msgid):
        okmsg = messages.OkMsg()
        okmsg.msgid = msgid
        self.writer.put(okmsg)

    def run(self):
        while not self.stopevt.isSet():
            msg = self.source.get()
            msgid = msg.msgid
            log.debug("handling %s" % msgid)
            listeners = collections.deque(self.listeners)
            log.debug("got %s listeners" % len(listeners))
            while msg and listeners:
                cb = listeners.popleft()
                try:
                    log.debug("calling %s" % cb)
                    msg = cb(msg)
                    log.debug("back: %s" % msg)
                except:
                    logging.error("Exception in callback", exc_info=1)
            self.sndok(msgid)

class EmkuuConnection(object):

    def __init__(self, host='localhost', port=DEFAULTPORT):
        self.lck = threading.RLock()
        self.host = host
        self.port = port
        self._initskt()
        log.debug("alloc reader, writer")
        self.reader = _ReaderThread(self.skt)
        self.writer = _WriterThread(self.skt)
        log.debug("getting currentmsgid")
        self.currentmsgid = self.reader.initq.get()
        log.debug("set up listeners")
        self.listeners = _WorkerThread(self.reader, self.writer)
        self.okmap = {}
        log.debug("init done")

    def _initskt(self):
        log.debug("_initskt")
        try:
            self.lck.acquire()
            skt = socket.socket()
            skt.connect((self.host, self.port))
            skt.settimeout(SKT_TIMEOUT)
            skt.sendall(ct.STREAMHEAD)
            self.skt = skt
        finally:
            self.lck.release()

    def _finishskt(self):
        try:
            log.debug("closing...")
            self.skt.sendall(ct.STREAMTAIL)
            self.skt.close()
            log.debug("done.")
        except:
            log.debug("failed to close socket: %s", exc_info=1)

    def reconnect(self):
        # xxx doesnt work reliably
        try:
            self.lck.acquire()
            self._finishskt()
            del self.skt
            del self.reader.skt
            del self.writer.skt
            self._initskt()
            self.reader.skt = self.skt
            self.writer.skt = self.skt
        finally:
            self.lck.release()

    def send(self, msg, cb=None):
        try:
            self.lck.acquire()
            msg.msgid = "%s:%s" % tuple(self.currentmsgid)
            if msg.corrid == -1L:
                msg.corrid = self.currentmsgid[1]
            if cb is not None:
                self.listeners.set_callback(msg.corrid, cb)
            log.debug("acq. sendlock...")
            self.writer.put(msg)
            self.currentmsgid[1] += 1
            msgid = unicode(msg.msgid)
            log.debug("sent %s" % msgid)
            okmap = self.okmap
            if okmap.has_key(msgid):
                log.debug("okmap has %s", msgid)
                del okmap[msgid]
            else:
                # ok not already acked, poll queue
                while True:
                    ackid = self.reader.okq.get(True, ACK_TIMEOUT)
                    if ackid == msgid:
                        # done, we got ack
                        break
                    # no, need to stow this away and try again
                    okmap[ackid] = True
        finally: self.lck.release()

    def add_listener(self, listener):
        try:
            self.lck.acquire()
            self.listeners.add_listeners(listener)
        finally:
            self.lck.release()

    def cur_msgid_tuple(self):
        return tuple(self.currentmsgid)
    def cur_msgid_str(self):
        return "%s:%s" % tuple(self.currentmsgid)

    def close(self):
        log.debug("conn: closing...")
        try:
            self.reader.stop()
            self.writer.stop()
            self.listeners.stop()
            time.sleep(1)
            del self.reader
            del self.writer
            del self.listeners
            time.sleep(0.1)
            self._finishskt()
        except Exception, det:
            log.debug("close exc: %s", det)

    def __del__(self):
        self.close()







