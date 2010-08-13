from xml import sax
import types
from emkuupy import messages

EMKUUNS=u"http://sabaini.at/protocol/emkuu-0.1"
STREAMNS=u"http://etherx.jabber.org/streams"
STREAMHEAD="<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns:e=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\">"
STREAMTAIL="</stream:stream>"

_marker = object()

class EmkuuContentHandler(sax.ContentHandler):

    def __init__(self, msgcallback, initcb):
        self.prefixes = {}
        self.classnames = {}
        def mapcl(klassname):
            klass = getattr(messages, klassname)
            if not hasattr(klass, '_tagname'): return
            self.classnames[klass._tagname] = klass
        map(mapcl, dir(messages))
        self.msgcallback = msgcallback
        self.initcb = initcb

    def startDocument(self):
        self.isStarted = True
        self.stack = []
        self.content = []

    def endDocument(self):
        self.isStarted = False

    def startPrefixMapping(self, prefix, uri):
        self.prefixes[uri] = prefix
    def endPrefixMapping(self, prefix):
        try:
            del self.prefixes[prefix]
        except: pass

    def startElementNS(self, name, qname, attrs):
        #print "startElemNS: ", name, qname, attrs
        if not self.isStarted:
            return
        if name == (STREAMNS, "stream"):
            s = attrs.get((None, "current_msgid"))
            if s is None:
                raise sax.SAXException("Missing init info")
            else:
                self.initcb(s)
            return
        if name[0] == EMKUUNS:
            self._startEmkuu(name[1], attrs)
        else:
            self._startOther(name, attrs)

    def _startEmkuu(self, name, attrs):
        #print "_startEmkuu", name, attrs
        if(self.stack and
           isinstance(self.stack[0], messages.RouteMsg) and
           name in ('body', 'enclosed')
           ):
            return
        klass = self.classnames.get(name)
        if klass is None:
            raise sax.SAXException("Unknown tag: %s" % name)
        msg = klass()
        knames = [n for n in klass.attr_names()]
        def st(n):
            a = attrs.get((None, n))
            if a is None: return
            if n=='corrid':
                a = long(a)
            elif n=='status':
                a = int(a)
            elif n in ('to', 'fro'):
                a = messages.Uri(string=a)
            setattr(msg, n, a)
        map(st, knames)
        self.stack.append(msg)

    def _mkTag(self, name):
        uri, localname = name
        if uri is None:
            return localname
        else:
            pre = self.prefixes[uri]
            return "%s:%s" % (pre, localname)

    def _attrs2str(self, attrs):
        def f(t):
            (uri, name), v = t
            if uri is None:
                return ' %s="%s"' % (name, v)
            else:
                pre = self.prefixes[uri]
                return ' %s:%s="%s"' % (pre, name, v)
        return ''.join(map(f, attrs.items()))

    def _startOther(self, name, attrs):
        tag = self._mkTag(name)
        attrstr = self._attrs2str(attrs)
        self.content.append("<" + tag + attrstr)
        self.content.append(_marker)

    def endElementNS(self, name, qname):
        #print "endElementNS: ", name
        if not self.isStarted or not self.stack or name[1]=="stream":
            return
        if name[0] == EMKUUNS:
            self._endEmkuu(name[1])
        else:
            self._endOther(name)

    def _endEmkuu(self, name):
        top = self.stack[0]
        if(name == 'body' and isinstance(top, messages.RouteMsg)):
            top.body = u''.join(self.content)
            self.content = []
            return

        top = self.stack.pop()
        txt = u''.join(self.content)
        self.content = []
        if(isinstance(top, messages.EntityMessage)):
            if(isinstance(top, messages.RouteMsg)):
                top.enclosed = txt
            else:
                top.body = txt
        while(self.stack):
            a = self.stack.pop()
            if( not isinstance(a, messages.RouteMsg) ):
                raise sax.SAXException("Illegal nesting: %s" % a)
            a.enclosed = top
            top = a
        self.msgcallback(top)

    def _endOther(self, name):
        contentlist = self.content
        if contentlist[-1] is _marker:
            del contentlist[-1:]
            contentlist.append(" />")
        else:
            tag = self._mkTag(name)
            contentlist.append("</" + tag + ">")

    def characters(self, content):
        contentlist = self.content
        if contentlist and contentlist[-1] is _marker:
            del contentlist[-1:]
            contentlist.append(">")
        contentlist.append(content)

