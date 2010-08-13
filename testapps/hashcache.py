"""Test client: store key-value pairs in memory
"""
import shelve
import uuid
from nevow import loaders, rend, tags as T
from twisted.internet import reactor
from twisted.words.xish import xpath
from twisted.words.protocols.jabber import xmlstream
from twisted.python import log
import authenticator
import constants
import messageutils
import urischemes

MYURL = "emkuu://emkuu/hashcache"
# url scheme: emkuu://emkuu/hashcache/key
PORT = 2347
DBNAME = "/tmp/hashcache.db"

from nevow import loaders, rend, tags as T

class EntryLister(rend.Page):
    docFactory = loaders.stan(
        T.html[
            T.ul(render=T.directive('sequence'))[
                T.li(pattern='item', render=T.directive('item')),
            ]
        ]
    )
    def render_item(self, context, entry):
        key, val = entry
        return context.tag[
            T.a(href=(MYURL + '/' + key)) [ val ]
            ]

class ClientProto(xmlstream.XMPPHandler):

    _setup = False

    def __init__(self):
        xmlstream.XMPPHandler.__init__(self)
        self.cache = shelve.open(DBNAME, writeback=True)
        self.posttokens = {}

    def connectionMade(self):
        if not self._setup:
            print "ClientProto.connectionMade()"
            self.xmlstream.addObserver('/get', self.onGet)
            self.xmlstream.addObserver('/put', self.onPut)
            self.xmlstream.addObserver('/create', self.onCreate)
            self.xmlstream.addObserver('/post', self.onPost)
            self.xmlstream.addObserver('/delete', self.onDelete)
            self.xmlstream.namespace = constants.NS_EMKUU
            self._setup = True

    def my404(self, to, corrid):
        e = messageutils.InfoElement(404, fro=MYURL)
        e['to'] = to
        e['corrid'] = corrid
        return e

    def getpath(self, uri):
        s = urischemes.parse_uri(uri).path
        l = s.split('/')
        l2 = filter(lambda e: e, l)
        return l2

    def errparams(self, msg, rqheaders=[]):
        name = self.getpath(msg['to'])[0]
        missingattrs = filter(lambda a: msg.attributes.get(a) is None, rqheaders)
        if name != 'hashcache' or missingattrs:
            e = messageutils.InfoElement(
                400, fro=MYURL, to=msg['fro'], corrid=msg['corrid'])
            self.xmlstream.send(e)
            return True
        return False
    def has_key(self, msg):
        path = self.getpath(msg['to'])
        if len(path) < 2: return False
        if self.cache.has_key(str(path[1])): return True
        return False

    def onGetList(self, msg):
        page = EntryLister(self.cache.items())
        doc = page.renderSynchronously()
        e = messageutils.StateElement(
            body=doc, fro=msg['to'], to=msg['fro'],
            corrid=msg['corrid'])
        self.xmlstream.send(e)

    def onGetKey(self, msg):
        if not self.has_key(msg):
            self.xmlstream.send(self.my404(msg['fro'], msg['corrid']))
            return
        key = str(self.getpath(msg['to'])[1])
        val = self.cache[key]
        e = messageutils.StateElement(
            body=val, fro=msg['to'], to=msg['fro'],
            corrid=msg['corrid'])
        self.xmlstream.send(e)
        print "sent %s" % e.toXml()

    def onGet(self, msg):
        # sub-dispatch on path len
        if self.errparams(msg): return
        path = self.getpath(msg['to'])
        if len(path) == 1:
            return self.onGetList(msg)
        else:
            return self.onGetKey(msg)

    def onPut(self, msg):
        print msg
        if self.errparams(msg): return
        if not self.has_key(msg):
            self.xmlstream.send(self.my404(msg['fro'], msg['corrid']))
            return
        to = urischemes.parse_uri(msg['to'])
        key = str(to.path.split('/')[-1])
        newval = str(msg) # str(Element) retrieves 1st text node
        self.cache[key] = newval
        self.cache.sync()
        e = messageutils.StateElement(fro=msg['to'], to=msg['fro'],
            corrid=msg['corrid'])
        e['status' ] = 201
        self.xmlstream.send(e)
        print "sent %s" % e.toXml()

    def onCreate(self, msg):
        print msg
        if self.errparams(msg): return
        to = urischemes.parse_uri(msg['to'])
        tok = str(uuid.uuid4())
        if msg.attributes.get('location'):
            key = str(urischemes.parse_uri(msg['location']).path.split('/')[-1])
        else:
            key = tok
        if self.cache.has_key(key): # won't create existing entry
            e = messageutils.InfoElement(
                409, fro=MYURL, to=msg['fro'], corrid=msg['corrid'])
            self.xmlstream.send(e)
            return
        self.posttokens[tok] = msg['fro']
        e = messageutils.InfoElement(
            201, fro=MYURL, to=msg['fro'], corrid=msg['corrid'])
        e['location'] = MYURL + '/' + key
        e['posttoken'] = tok
        self.xmlstream.send(e)
        print "sent %s" % e.toXml()

    def onPost(self, msg):
        print msg # xxx statt location to nehmen!
        if self.errparams(msg, rqheaders=['location', 'posttoken']):
            return
        tok = self.posttokens.get(msg['posttoken'])
        if not tok or tok!=msg['fro']:
            self.xmlstream.send(self.my404(msg['fro'], msg['corrid']))
            return
        loc = urischemes.parse_uri(msg['location'])
        key = str(loc.path.split('/')[-1])
        self.cache[key] = str(msg)
        self.cache.sync()
        e = messageutils.InfoElement(
            204, fro=(MYURL + '/' + key), to=msg['fro'], corrid=msg['corrid'])
        self.xmlstream.send(e)
        print "sent %s" % e.toXml()

    def onDelete(self, msg):
        print msg
        if self.errparams(msg): return
        if not self.has_key(msg): # xxx ???
            self.xmlstream.send(self.my404(msg['fro'], msg['corrid']))
            return
        to = urischemes.parse_uri(msg['to'])
        key = str(to.path.split('/')[-1])

        del self.cache[key]
        self.cache.sync()

        e = messageutils.InfoElement(
            204, fro=(MYURL + '/' + key), to=msg['fro'], corrid=msg['corrid'])
        self.xmlstream.send(e)
        print "sent %s" % e.toXml()




factory = xmlstream.XmlStreamFactory(authenticator.SimpleConnectAuth())

def streamManagerFactory():
    manager = xmlstream.StreamManager(factory)
    hdlr = ClientProto()
    manager.addHandler(hdlr)
    hdlr.setHandlerParent(manager)
    return manager

if __name__ == '__main__':
    m = streamManagerFactory()
    m.logTraffic = True
    reactor.connectTCP('localhost', PORT, m.factory)
    reactor.run()
    # perform cleanup, xmlstream.sendFooter() or similar
