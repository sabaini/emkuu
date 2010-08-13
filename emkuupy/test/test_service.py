import unittest
from xml import sax
from emkuupy import ct
from emkuupy import messages
from emkuupy.test import util
from emkuupy.service import EmkuuManagedConnection
import time

class stor:
    L = []

    def __init__(self, con, mgr):
        self.con = con
        self.mgr = mgr
        m = messages.StateMsg()
        m.fro = "emkuu://emkuu/foo"
        m.to = "emkuu://emkuu/foo"
        m.corrid = 1
        m.msgid = con.cur_msgid_str()
        m.body = 123
        con.send(m)

    def __call__(self, msg):
        stor.L.append(msg)
        #print "here: ", msg
        self.mgr.stop()



class TestBase(unittest.TestCase):

    def setUp(self):
        self.testsrv = util.TestSrv()

    def stoptestsrv(self):
        self.testsrv.stop()
        self.testsrv.join()

    def tearDown(self):
        try:
            self.stoptestsrv()
            self.testsrv.terminate()
            del self.testsrv
        except: pass

    def test_sendrcv(self):
        self.testsrv.inqueue.put('<e:ok msgid="%s" />' % (util.INITIALMSGID))
        svc = EmkuuManagedConnection(port=util.PORT, listener_factory=stor)
        svc.start()
        msgid = svc.connection.cur_msgid_str()
        self.testsrv.inqueue.put('<e:get msgid="%s" corrid="1" />%s' % (msgid, ct.STREAMTAIL))
        time.sleep(0.2)
        self.stoptestsrv()
        svc.stop()
        self.assertEquals(stor.L[0].corrid, 1)


if __name__ == '__main__':
    unittest.main()
