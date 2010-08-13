"""Test client: random data repository
"""
import random
from twisted.internet import reactor
from twisted.words.xish import xpath
from twisted.words.protocols.jabber import xmlstream
from twisted.python import log
import authenticator
import constants
import messageutils

MYURL = "emkuu://emkuu/random"
PORT = 2345

class ClientProto(xmlstream.XMPPHandler):

    _setup = False

    def connectionMade(self):
        if not self._setup:
            print "ClientProto.connectionMade()"
            #def p(d):
            #    print d
            self.xmlstream.addObserver('/get[@to="%s"]' % MYURL,
                                       self.onGet)
            self.xmlstream.namespace = constants.NS_EMKUU
            self._setup = True

    def onGet(self, msg):
        print msg
        # msg = msg.firstChildElement()
        msgid = msg.attributes['msgid']
        print msg['corrid']
        e = messageutils.StateElement(body=str(random.random()),
                                     fro=MYURL,
                                     to=msg['fro'],
                                     corrid=msg['corrid'])
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
