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
from emkuupy.test import util

from StringIO import StringIO


PORT=1234
TIMEOUT=0.4
connection.ACK_TIMEOUT = 2000.0

class Loader(object):

    def setUp(self):
        self.conn = connection.EmkuuConnection(port=PORT)

    def test_parallel_load(self, nthreads, nmsg):
        def do(nth):
            #import pdb; pdb.set_trace()
            msg = messages.StateMsg()
            msg.corrid = 1
            for i in xrange(nmsg):
                self.conn.send(msg)
        tlist = []
        for i in xrange(nthreads):
            t = threading.Thread(target=do, name="testthread-%s" % i, args=(i,))
            t.start()
            tlist.append(t)
        for t in tlist:
            t.join()

    def test_serial_load(self, ntotal):
        msg = messages.StateMsg()
        msg.corrid = 1
        for i in xrange(ntotal):
            self.conn.send(msg)

if __name__ == '__main__':
    l = Loader()
    l.setUp()
    nthread = 5
    nmsg = 200
    #l.test_parallel_load(nthread, nmsg)
    #cProfile.run('l.test_parallel_load(nthread, nmsg)', 'loadpara.prof')
    ntotal = 1000
    cProfile.run('l.test_serial_load(ntotal)', 'loadserial.prof')







