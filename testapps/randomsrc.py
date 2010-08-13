#!/usr/bin/python
"""Test client: random data source
"""
import sys
import random
from twisted.internet import reactor, task
from twisted.words.xish import xpath
from twisted.words.protocols.jabber import xmlstream
from twisted.python import log
import authenticator
import constants
import messageutils

MYURL = "emkuu://emkuu/randomsrc"
PORT = 2348

class ClientProto(xmlstream.XMPPHandler):

    _setup = False

    def __init__(self, interv=1.0):
        self.interval = interv
        xmlstream.XMPPHandler.__init__(self)
        self.loop = task.LoopingCall(self.emit)

    def connectionMade(self):
        if not self._setup:
            print "ClientProto.connectionMade()"
            self.xmlstream.namespace = constants.NS_EMKUU
            self._setup = True
            self.loop.start(self.interval)

    def emit(self):
        e = messageutils.StateElement(body=str(random.random()), fro=MYURL,)
        self.xmlstream.send(e)
        print '.',


factory = xmlstream.XmlStreamFactory(authenticator.SimpleConnectAuth())

def streamManagerFactory(i):
    manager = xmlstream.StreamManager(factory)
    hdlr = ClientProto(i)
    manager.addHandler(hdlr)
    hdlr.setHandlerParent(manager)
    return manager

if __name__ == '__main__':
    try:
        i = float(sys.argv[1])
    except:
        i = 1.0
    m = streamManagerFactory(i)
    m.logTraffic = True
    reactor.connectTCP('localhost', PORT, m.factory)
    reactor.run()

