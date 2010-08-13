import unittest
from emkuupy import pathlistener, messages


class TestL(pathlistener.PathListener):

    def handleMatch(self, msg, pathrest):
        self.msg = msg
        self.pathrest = pathrest

class TestMessageKindL(pathlistener.MessageKindListener):

    def handlePostMsg(self, msg, pathrest):
        self.msg = msg
        self.pathrest = pathrest

    def handleGetMsg(self, msg, pathrest):
        self.msg = msg
        self.pathrest = pathrest

class TestBase(unittest.TestCase):

    def setUp(self):
        self.msg = messages.PostMsg()
        self.getmsg = messages.GetMsg()

    def test_pathl_match(self):
        l = TestL(None, None, "/a")
        self.msg.to = messages.Uri(string="/emkuu/a/b")
        res = l(self.msg)
        self.assertEquals(l.msg, self.msg)
        self.assertEquals(l.pathrest, "/b")
        self.assertEquals(res, None)

    def test_pathl_nomatch(self):
        l = TestL(None, None, "/somestuff")
        self.msg.to = messages.Uri(string="/emkuu/a/b")
        res = l(self.msg)
        self.assertEquals(res, self.msg)
        self.assertEquals(getattr(l, "msg", None), None)

    def test_mkindl_match(self):
        l = TestMessageKindL(None, None, "/foo")
        self.msg.to = messages.Uri(string="/emkuu/foo/bar")
        res = l(self.msg)
        self.assertEquals(l.msg, self.msg)
        self.assertEquals(l.pathrest, "/bar")
        self.assertEquals(res, None)
        self.getmsg.to = messages.Uri(string="/emkuu/foo/quux")
        res = l(self.getmsg)
        self.assertEquals(l.msg, self.getmsg)
        self.assertEquals(l.pathrest, "/quux")
        self.assertEquals(res, None)

    def test_mkindl_nohdlr(self):
        l = TestMessageKindL(None, None, "/foo")
        msg = messages.DeleteMsg()
        msg.to = messages.Uri(string="/emkuu/foo/bar")
        res = l(msg)
        self.assertEquals(res, msg)
        self.assertEquals(getattr(l, "msg", None), None)


