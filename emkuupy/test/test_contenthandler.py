import unittest
from xml import sax
from emkuupy import ct
from emkuupy import messages
from emkuupy.test import util


from StringIO import StringIO

class Cb:
    msg = None
    def __call__(self, msg):
        self.msg = msg


class ContentHandlerTest(unittest.TestCase):

    def setUp(self):
        self.cb = Cb()
        self.emkuuct = ct.EmkuuContentHandler(self.cb, lambda a: None)
        self.parser = sax.make_parser()
        self.parser.setContentHandler(self.emkuuct)
        self.parser.setFeature(sax.handler.feature_namespaces, 1)

    def test_setup(self):
        self.setUp()
        self.assertEquals(messages.StateMsg,
                          self.emkuuct.classnames['state'])

    def test_parse(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '</stream:stream>'))

    def test_nostartdoc(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:get fro="gugu" /></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)


    def test_doelem(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:get fro="gugu" /></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)
        msg = self.cb.msg
        self.assertEquals(msg.__class__, messages.GetMsg)

    def test_doelem2(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:put>b</e:put></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)
        msg = self.cb.msg
        self.assertEquals(msg.__class__, messages.PutMsg)
        self.assertEquals(msg.body, u'b')

    def test_nested(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:put><doc>a</doc></e:put></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)
        msg = self.cb.msg
        self.assertEquals(msg.__class__, messages.PutMsg)
        self.assertEquals(msg.body, u"<doc>a</doc>")

    def test_nested_empty(self):
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:put><doc /></e:put></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)
        msg = self.cb.msg
        self.assertEquals(msg.__class__, messages.PutMsg)
        self.assertEquals(msg.body, u"<doc />")

    def test_nested_ns(self):
        # laziness: stream is already registered, so we use that
        self.parser.parse(StringIO(util.STREAMHEAD + '<e:put><stream:doc>a</stream:doc></e:put></stream:stream>'))
        st = self.emkuuct.stack
        self.assertEquals(len(st), 0)
        msg = self.cb.msg
        self.assertEquals(msg.__class__, messages.PutMsg)
        self.assertEquals(msg.body, u"<stream:doc>a</stream:doc>")


    def test_routemsg(self):
        self.parser.parse(StringIO(
            util.STREAMHEAD + '<e:route msgid="2"><e:body>rb</e:body><e:enclosed><e:post corrid="3">pb</e:post></e:enclosed></e:route></stream:stream>'))
        msg = self.cb.msg
        self.assert_(isinstance(msg, messages.RouteMsg))
        self.assertEquals(msg.msgid, "2")
        self.assertEquals(msg.body, u'rb')
        self.assert_(isinstance(msg.enclosed, messages.PostMsg))
        self.assertEquals(msg.enclosed.body, u'pb')
        self.assertEquals(msg.enclosed.corrid, 3L)

    def test_okmsg(self):
        self.parser.parse(StringIO(
            util.STREAMHEAD + '<e:ok msgid="2" /></stream:stream>'))
        msg = self.cb.msg
        self.assert_(isinstance(msg, messages.OkMsg))
        self.assertEquals(msg.msgid, "2")



