import unittest
import socket
import time
import random
import Queue

from xml import sax
from emkuupy.test import util
from emkuupy import ct
from emkuupy import messages
from emkuupy import connection
from emkuupy.test import util

from StringIO import StringIO

connection.ACK_TIMEOUT = 20

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

    def getresult(self):
        r = self.testsrv.collect().strip()
        r = r[len(ct.STREAMHEAD):-len(ct.STREAMTAIL)].strip()
        return r

    def test_setup(self):
        self.assert_(self.conn)

    def test_nullmsg(self):
        self.testsrv.inqueue.put(ct.STREAMTAIL)

    def test_recvmsg(self):
        msgid = self.conn.cur_msgid_str()
        self.testsrv.inqueue.put('<e:ok msgid="%s" />%s' % (msgid, ct.STREAMTAIL))
        m = self.conn.reader.okq.get()
        self.assertEquals(m, msgid)

    def test_sendmsg(self):
        msgid = self.conn.cur_msgid_str()
        d, msgcnt = self.conn.cur_msgid_tuple()
        m = messages.GetMsg()
        self.testsrv.inqueue.put('<e:ok msgid="%s" />%s' % (msgid, ct.STREAMTAIL))
        self.conn.send(m)
        time.sleep(1)
        self.stoptestsrv()
        r = self.getresult()
        self.assertEquals(r, '<e:get corrid="%s" msgid="%s" />' % (msgcnt, msgid))

    def test_ack_in_order(self):
        idtxt, idnum = self.conn.cur_msgid_tuple()
        msgid = "%s:%s" % (idtxt, idnum)
        m = messages.GetMsg()
        self.testsrv.inqueue.put('<e:ok msgid="%s" />' % msgid)
        self.conn.send(m)
        idnum += 1
        msgid2 = "%s:%s" % (idtxt, idnum)
        self.testsrv.inqueue.put('<e:ok msgid="%s" />%s' % (msgid2, ct.STREAMTAIL))
        self.conn.send(m)
        self.stoptestsrv()
        time.sleep(1.0)
        r = self.getresult()
        self.assertEquals(r,
          '<e:get corrid="1" msgid="%s" />\n<e:get corrid="1" msgid="%s" />' % (msgid, msgid2))

    def test_wrong_ack(self):
        msgid = self.conn.cur_msgid_str()
        self.testsrv.inqueue.put('<e:ok msgid="gugu" />%s' % ct.STREAMTAIL)
        m = messages.GetMsg()
        # we acked wrong value  --> should timeout
        self.assertRaises(Queue.Empty, self.conn.send, m)
        self.stoptestsrv()
        r = self.getresult()
        self.assertEquals(r, '<e:get corrid="1" msgid="%s" />' % msgid)

    def test_ack_not_in_order(self):
        idtxt, idnum = self.conn.cur_msgid_tuple()
        msgid = "%s:%s" % (idtxt, idnum)
        msgid2 = "%s:%s" % (idtxt, idnum+1)
        self.testsrv.inqueue.put('<e:ok msgid="%s" /><e:ok msgid="%s" />' % (msgid2, msgid))
        m = messages.GetMsg()
        self.conn.send(m)
        self.conn.send(m) #
        time.sleep(0.5) # grch
        self.stoptestsrv()
        r = self.getresult()
        self.assertEquals(r, '<e:get corrid="1" msgid="%s" />\n<e:get corrid="1" msgid="%s" />' % (msgid, msgid2))

    def test_acks_many(self):
        nr = 1000
        idtxt, idnum = self.conn.cur_msgid_tuple()
        okt = '<e:ok msgid="%s:%s" />'
        s = ''.join([okt % (idtxt, idnum + n) for n in xrange(nr)])
        self.testsrv.inqueue.put(s + (80*' ' ) + '</stream:stream>')
        def f(n):
            m = messages.PostMsg()
            self.conn.send(m)
        map(f, xrange(nr-1, -1, -1))
        time.sleep(2.0)
        self.stoptestsrv()
        r = self.testsrv.collect()
        cnt = r.count("<e:post")
        self.assertEquals(cnt, nr)
        self.assertEquals(self.conn.okmap, {})

    def test_sendcb(self):
        msgid = self.conn.cur_msgid_str()
        self.testsrv.inqueue.put(
            util.STREAMHEAD + '<e:ok msgid="%s" /><e:state corrid="7">a</e:state></stream:stream>' % msgid
        )
        m = messages.PutMsg()
        m.corrid = 7
        d = []
        self.conn.send(m, d.append)
        self.stoptestsrv()
        self.assertEquals(len(d), 1)
        self.assert_(isinstance(d[0], messages.StateMsg))


    def test_sendcb_many(self):
        nr = 1000
        idtxt, idnum = self.conn.cur_msgid_tuple()
        okt = '<e:ok msgid="%s:%s" />'
        s = ''.join([okt % (idtxt, idnum + n) for n in xrange(nr)])
        statestr = '<e:state corrid="7">a</e:state>'
        self.testsrv.inqueue.put(s + (statestr * nr) + (80*' ' ) + '</stream:stream>')
        d = []
        def f(n):
            m = messages.GetMsg()
            m.corrid = 7
            self.conn.send(m, d.append)
        map(f, xrange(nr-1, -1, -1))
        time.sleep(1.3)
        self.stoptestsrv()
        self.assertEquals(len(d), nr)
        self.assert_(isinstance(d[nr-1], messages.StateMsg))


    def test_sendcb_badcb(self):
        self.testsrv.inqueue.put(
            util.STREAMHEAD + '''<e:ok msgid="%s" /><e:state corrid="7">a</e:state>''' % util.INITIALMSGID
        )
        m = messages.PutMsg()
        m.corrid = 7
        self.conn.send(m, lambda _: 1/0)
        msgid = self.conn.cur_msgid_str()
        self.testsrv.inqueue.put(
            '''<e:ok msgid="%s" /><e:state corrid="8">a</e:state>
            </stream:stream>''' % msgid
        )
        m.corrid = 8
        d = []
        self.conn.send(m, d.append)
        self.stoptestsrv()
        self.assertEquals(len(d), 1)

    def test_listener(self):
        d = []
        self.conn.add_listener(d.append)
        self.testsrv.inqueue.put('<e:state corrid="7">a</e:state></stream:stream>')
        time.sleep(0.8)
        self.stoptestsrv()
        self.assertEquals(len(d), 1)
        self.assertEquals(d[0].corrid, 7)

    def test_listener2(self):
        class dum(list):
            def grab(self, msg):
                self.append(msg)
                return msg
        d1 = dum()
        d2 = []
        self.conn.add_listener(d1.grab)
        self.conn.add_listener(d2.append)
        self.testsrv.inqueue.put('<e:state corrid="7">a</e:state></stream:stream>')
        self.stoptestsrv()
        time.sleep(0.7)
        self.assertEquals(len(d1), 1)
        self.assertEquals(d1[0].corrid, 7)
        self.assertEquals(len(d2), 1)
        self.assertEquals(d2[0].corrid, 7)



if __name__ == '__main__':
    unittest.main()
