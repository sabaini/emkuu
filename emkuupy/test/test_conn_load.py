import unittest
import socket
import threading
import time
import random


from emkuupy import ct
from emkuupy import messages
from emkuupy import connection
from emkuupy.test import util

connection.ACK_TIMEOUT = 20.0

class ConnTest(unittest.TestCase):

    def setUp(self):
        self.testsrv = util.TestSrv()
        self.conn = connection.EmkuuConnection(port=util.PORT)

    def stoptestsrv(self):
        self.conn.close()
        self.testsrv.stop()
        self.testsrv.join()

    def tearDown(self):
        try:
            self.stoptestsrv()
            self.testsrv.terminate()
            del self.conn
            del self.testsrv
        except: pass

    def run_sendcb(self, nthreads, nmsg, timeinterval):
        idtxt, idnum = self.conn.cur_msgid_tuple()
        collector = []
        doneEvt = threading.Event()
        def cb(msg):
            collector.append(msg)
            #print "appending"
            if len(collector) >= (nmsg * nthreads):
                doneEvt.set()
        def do(nth):
            random.seed()
            msg = messages.StateMsg()
            msg.corrid = 1
            for i in xrange(nmsg):
                self.conn.send(msg, cb)
                slp = float(random.randint(0, timeinterval)) / 1000.0
                if slp > 0.007: time.sleep(slp)
        tlist = []
        for i in xrange(nthreads):
            t = threading.Thread(target=do, name="testthread-%s" % i, args=(i,))
            t.start()
            tlist.append(t)
        msgt = '<e:ok msgid="%s:%s" /><e:state corrid="1">a</e:state>'
        n = idnum
        for nth in xrange(nthreads):
            for i in xrange(nmsg):
                self.testsrv.inqueue.put(msgt % (idtxt, n))
                n += 1
        self.testsrv.inqueue.put("\n</stream:stream>\n")
        for t in tlist:
            t.join()
        self.stoptestsrv()
        doneEvt.wait(10)
        self.assertEquals(len(collector), nmsg * nthreads)

    def test_singlethread(self):
        self.run_sendcb(1, 2, 10)

    def test_parallel_load(self):
        self.run_sendcb(5, 20, 50)

    def test_parallel_massive(self):
        self.run_sendcb(50, 5, 10)

    def test_parallel_longsleep(self):
        self.run_sendcb(3, 4, 500)


if __name__ == '__main__':
    unittest.main()

