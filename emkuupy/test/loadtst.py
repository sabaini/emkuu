import cProfile
import socket
import threading
import time
import random
import Queue

from xml import sax
from emkuupy import ct
from emkuupy import messages
from emkuupy import connection

from StringIO import StringIO

PORT=43210
TIMEOUT=0.4
connection.ACK_TIMEOUT = 2000.0


class TestSrv(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.stopevt = threading.Event()
        self.skt = socket.socket()
        self.skt.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.skt.bind(("localhost", PORT))
        self.skt.listen(1)
        self.inqueue = Queue.Queue()
        self.outqueue = Queue.Queue()
        self.start()

    def collect(self):
        buf = StringIO()
        while not self.outqueue.empty():
            buf.write(self.outqueue.get())
        return buf.getvalue()

    def run(self):
        try:
            cs, caddr = self.skt.accept()
            #print "testsrv got client"
            cs.settimeout(TIMEOUT)
            while not self.stopevt.isSet():
                try:
                    d = self.inqueue.get(True, TIMEOUT)
                    #print "testsrv got data... ",
                    cs.sendall(d)
                    #print " sent."
                except:
                    pass
                try:
                    ret = cs.recv(16000)
                    self.outqueue.put(ret)
                except socket.timeout:
                    pass
                self.stopevt.wait(TIMEOUT)
            cs.shutdown(socket.SHUT_RDWR)
            cs.close()
        finally:
            self.skt.close()


class Loader(object):

    def setUp(self):
        self.testsrv = TestSrv()
        self.conn = connection.EmkuuConnection(port=PORT)

    def stoptestsrv(self):
        self.conn.stop()
        self.testsrv.stopevt.set()
        self.testsrv.join()

    def tearDown(self):
        try:
            self.stoptestsrv()
            del self.conn
            del self.testsrv
        except: pass


    def test_parallel_load(self):
        nthreads = 5
        nmsg = 10
        msgt = '<ok msgid="msg-%s-%s" />'
        self.testsrv.inqueue.put('<stream>')
        for nth in xrange(nthreads):
            for i in xrange(nmsg):
                self.testsrv.inqueue.put(msgt % (nth, i))
        #for nth in xrange(nthreads):
        #    for i in xrange(nmsg):
        #        self.testsrv.inqueue.put('<state msgid="1" corrid="1">a</state>')
        def do(nth):
            msg = messages.StateMsg()
            msg.corrid = 1
            for i in xrange(nmsg):
                msg.msgid = "msg-%s-%s" % (nth, i)
                self.conn.send(msg)
        tlist = []
        for i in xrange(nthreads):
            t = threading.Thread(target=do, name="testthread-%s" % i, args=(i,))
            t.start()
            tlist.append(t)
        for t in tlist:
            t.join()

    def test_serial_load(self):
        nmsg = 50
        msgt = '<ok msgid="msg-0-%s" />'
        self.testsrv.inqueue.put('<stream>')
        for i in xrange(nmsg):
            self.testsrv.inqueue.put(msgt % i)
        #for nth in xrange(nthreads):
        #    for i in xrange(nmsg):
        #        self.testsrv.inqueue.put('<state msgid="1" corrid="1">a</state>')

        msg = messages.StateMsg()
        msg.corrid = 1
        for i in xrange(nmsg):
            msg.msgid = "msg-0-%s" % i
            #self.conn.send(msg, collector.append)
            self.conn.send(msg)

if __name__ == '__main__':
    l = Loader()
    l.setUp()
    cProfile.run('l.test_parallel_load()', 'loadpara.prof')
    time.sleep(1)
    cProfile.run('l.test_serial_load()', 'loadserial.prof')
    time.sleep(1)
    l.stoptestsrv()
    time.sleep(1)
    l.tearDown()





