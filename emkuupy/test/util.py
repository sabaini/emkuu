import processing
import socket
from StringIO import StringIO
from emkuupy import ct

TIMEOUT=0.4
PORT=43210
INITIALMSGID = "test:1"
STREAMHEAD="<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" " \
+ "xmlns:e=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\" current_msgid=\"" \
+ INITIALMSGID + "\">"

class TestSrv(processing.Process):
    def __init__(self):
        processing.Process.__init__(self)
        self.stopevt = processing.Event()
        self.skt = socket.socket()
        self.skt.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.skt.bind(("localhost", PORT))
        self.skt.listen(1)
        self.inqueue = processing.Queue()
        self.outqueue = processing.Queue()
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
            cs.sendall(STREAMHEAD)
            while not self.stopevt.isSet():
                try:
                    #print "wait for data"
                    d = self.inqueue.get(True, TIMEOUT)
                    #print "testsrv got data... %s " % d
                    cs.sendall(d)
                    #print " sent."
                except:
                    pass
                try:
                    ret = cs.recv(16000)
                    self.outqueue.put(ret)
                except socket.timeout:
                    pass
                self.stopevt.wait(0.04)
            cs.shutdown(socket.SHUT_RDWR)
            cs.close()
        finally:
            self.skt.shutdown(socket.SHUT_RDWR)
            self.skt.close()

    def stop(self):
        self.stopevt.set()



