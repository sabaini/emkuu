import unittest
from emkuupy import messages
from StringIO import StringIO


class TestBase(unittest.TestCase):

    def test_uri_create(self):
        u = messages.Uri()
        u.path = ['a']
        self.assertEquals(str(u), 'emkuu://emkuu/a')

    def test_uri_create2(self):
        u = messages.Uri()
        u.path = ['a', 'b']
        u.qry = 'foo=bar'
        self.assertEquals(str(u), 'emkuu://emkuu/a/b?foo=bar')

    def test_uri_parse(self):
        u = messages.Uri(string='emkuu://emkuu/a/b?foo=bar')
        self.assertEquals(u.path, ['a', 'b'])
        self.assertEquals(u.qry, 'foo=bar')

    def test_uri_parse2(self):
        u = messages.Uri(string='/emkuu/a/b')
        self.assertEquals(u.path, ['a', 'b'])
        self.assertEquals(u.scheme, 'emkuu')

    def test_uri_path_string(self):
        u = messages.Uri(string='/emkuu/a/b')
        p = u.path_string()
        self.assertEquals(p, "/a/b")

    def test_uri_cluster_string(self):
        u = messages.Uri(string='/emkuu/a/b')
        p = u.cluster_path()
        self.assertEquals(p, "/emkuu/a/b")

    def test_create(self):
        m = messages.DeleteMsg()
        self.assertEquals(m._tagname, 'delete')

    def test_create2(self):
        m = messages.DeleteMsg()
        m.authorization = 1
        m2 = messages.DeleteMsg()
        m2.authorization = 2
        self.assertEquals(m._tagname, 'delete')
        self.assertEquals(m.authorization, 1)
        self.assertEquals(m2.authorization, 2)

    def test_attr_names(self):
        k = messages.PutMsg
        m = k()
        l1 = k.attr_names()
        l1.sort()
        self.assertEquals(l1[0], "authorization")
        l2 = m.attr_names()
        l2.sort()
        self.assertEquals(l2, l1)

    def test_toxml(self):
        m = messages.GetMsg()
        m.corrid = 12L
        s = m.to_xml().strip()
        self.assertEquals(s, '<e:get corrid="12" />')

    def test_toxml2(self):
        m = messages.StateMsg()
        m.body = u"body"
        m.contentlen = 23
        m.fragment = True
        s = m.to_xml().strip()
        self.assertEquals(
            s,
            '<e:state contentlen="23" fragment="True" status="200">body</e:state>')

    def test_toxml_route(self):
        p = messages.PostMsg()
        p.body = u"body"
        p.contentlen = 12
        m = messages.RouteMsg()
        m.body = u"routedata"
        m.enclosed = p
        m.msgid = 7
        s = m.to_xml().strip()
        r = '<e:route msgid="7"><e:body>routedata</e:body><e:enclosed>'
        r += '<e:post contentlen="12">body</e:post>\n'
        r += '</e:enclosed></e:route>'
        self.assertEquals(s, r)

    def test_toxml_route2(self):
        p = messages.PostMsg()
        p.body = u"<foo />"
        p.contentlen = 12
        m = messages.RouteMsg()
        m.body = u"<bar />"
        m.enclosed = p
        m.msgid = 7
        s = m.to_xml().strip()
        r = '<e:route msgid="7"><e:body>&lt;bar /&gt;</e:body><e:enclosed>'
        r += '<e:post contentlen="12">&lt;foo /&gt;</e:post>\n'
        r += '</e:enclosed></e:route>'
        self.assertEquals(s, r)


if __name__ == '__main__':
    unittest.main()
