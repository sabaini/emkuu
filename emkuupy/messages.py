import types
from zope.interface import implements
import interfaces

class Uri(object):
    scheme = "emkuu"
    cluster = "emkuu"
    path = []
    qry = ''

    def __init__(self, string=None):
        if string is not None:
            if string[0] == '/':
                rest = string[1:]
            else:
                l = string.split("://")
                rest = l.pop()
                if l: self.scheme = l[0]
            l = rest.split("?")
            p = l.pop(0).split("/")
            self.cluster = p.pop(0)
            self.path = p
            if l: self.qry = l[0]

    def __str__(self):
        p = "/".join(self.path)
        if self.qry:
            s =  "%s://%s/%s?%s" % (
                self.scheme, self.cluster,
                p, self.qry)
        else:
            s =  "%s://%s/%s" % (
                self.scheme, self.cluster, p)
        return s

    def path_string(self):
        return "/" + "/".join(self.path)

    def cluster_path(self):
        return "/" + self.cluster + self.path_string()



class AbstractMessage(object):
    implements(interfaces.IMessage)
    
    msgid = ""
    fro = None
    to = None
    corrid = -1L
    date = ""

    @classmethod
    def attr_names(cls):
        return [n for n in dir(cls)
                if n[0] != '_' and not callable(getattr(cls, n))]

    def _writeattrs(self, writefun):
        def f(n):
            v = getattr(self, n)
            if not v: return
            writefun(' %s="%s"' % (n, v))
        map(f, self.attr_names())

    def to_xml(self):
        buf = []
        write = buf.append
        write("<e:" + self._tagname)
        self._writeattrs(write)
        write(" />\n")
        return ''.join(buf)

    __str__ = to_xml


class OkMsg(AbstractMessage):
    # xxx actuall only has msgid attr
    _tagname = "ok"

class DeleteMsg(AbstractMessage):
    _tagname = 'delete'
    authorization = ""
    replyto = ""


class EntityMessage(AbstractMessage):
    body = ""
    fragment = False
    contentlen = 0L
    contenttype = ""

    @classmethod
    def attr_names(cls):
        return [n for n in dir(cls)
                if n[0] != '_' and
                not callable(getattr(cls, n)) and
                n != 'body']

    def to_xml(self):
        buf = []
        write = buf.append
        write("<e:" + self._tagname)
        self._writeattrs(write)
        write(">")
        body = unicode(self.body)
        s = body.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").encode('ascii', 'xmlcharrefreplace')
        write(s)
        write("</e:%s>\n" % (self._tagname))
        return ''.join(buf)
    __str__ = to_xml

class GetMsg(AbstractMessage):
    _tagname = 'get'
    authorization = ""
    replyto = ""
    accept = ""
    cachecontrol = ""
    ifmod = ""
    lastmod = ""
    omitbody = False

class PostMsg(EntityMessage):
    _tagname = 'post'
    authorization = ""
    replyto = ""

class PutMsg(EntityMessage):
    _tagname = 'put'
    authorization = ""
    replyto = ""


class RouteMsg(EntityMessage):
    _tagname = 'route'
    authorization = ""
    replyto = ""
    enclosed = None

    @classmethod
    def attr_names(cls):
        return [n for n in dir(cls)
                if n[0] != '_' and
                not callable(getattr(cls, n)) and
                n not in ('body', 'enclosed')]

    def to_xml(self):
        buf = []
        write = buf.append
        write("<e:" + self._tagname)
        self._writeattrs(write)
        write("><e:body>")
        body = unicode(self.body)
        s = body.replace("<", "&lt;").replace(">", "&gt;").encode('ascii', 'xmlcharrefreplace')
        write(s)
        write("</e:body><e:enclosed>")
        write(str(self.enclosed))
        write("</e:enclosed></e:%s>\n" % (self._tagname))
        return ''.join(buf)
    __str__ = to_xml

class StateMsg(EntityMessage):
    _tagname = 'state'
    status=200
    etag = ""
    expires = ""
    location = ""
    locktimeout = ""

class SubscribeMsg(AbstractMessage):
    _tagname = 'subscribe'
    authorization = ""
    replyto = ""
    accept = ""
    cachecontrol = ""
    ifmod = ""
    lastmod = ""
    omitbody = False
    subscribeuntil = ""

