#!/usr/bin/python
import sys
import functools
import collections
from twisted.internet import reactor, task
from twisted.words.xish import xpath
from twisted.words.protocols.jabber import xmlstream
from twisted.cred import portal, checkers, credentials
from twisted.conch import manhole, manhole_ssh
from twisted.python import log
import authenticator
import constants
import messageutils

MYURL = "emkuu://emkuu/emkuushell"
PORT = 2349
SSHPORT = 2322

# a place where shells and xmpp streams come to mingle
theShellNamespace = {
    'xmlstream' : None, # nasty global so we can reconnect
}

settings = {
    'classdict' : { # convenient names for classes
                    'state': messageutils.StateElement,
                    'get' : messageutils.GetElement,
                    'put' : messageutils.PutElement,
                    'create' : messageutils.CreateElement,
                    'post' : messageutils.PostElement,
                    'delete' : messageutils.DeleteElement,
                    'options' : messageutils.OptionsElement,
                    'subscribe' : messageutils.SubscribeElement,
                    'unsubscribe' : messageutils.UnSubscribeElement,
                    'route' : messageutils.RouteElement,
                    },
    'printout' : 'xml' # xml|body|off
}

def printmode(mode='xml'):
    settings['printout'] = mode
    print "print mode set to: %s" % mode

def messageFac(msgtype, fro, *args, **kw):
    klass = settings['classdict'][msgtype]
    def overloadedsend(self):
        if theShellNamespace['xmlstream'] is None:
            print "Not connected"
        else:
            theShellNamespace['xmlstream'].send(self)
    # evil monkey patch
    klass.__invert__ = overloadedsend
    if kw.has_key('to'):
        kw['to'] = "emkuu://emkuu" + kw['to']
    o = klass(*args, **kw)
    o.xmlstream = theShellNamespace['xmlstream']
    o['fro'] = fro
    return o

msgbuffer = collections.deque()
class ClientProto(xmlstream.XMPPHandler):

    def connectionMade(self):
        print "ClientProto.connectionMade()"
        self.xmlstream.addObserver('/*', self.onMsg)
        self.xmlstream.namespace = constants.NS_EMKUU
        theShellNamespace['xmlstream'] = self.xmlstream
        self.prepareShellNamespace()
        self.msgcnt = 0

    def onMsg(self, msg):
        self.msgcnt += 1
        msgbuffer.append(msg)
        if self.msgcnt > 1000:
            msgbuffer.popleft()
        pm = settings['printout']
        if pm=='xml':
            sys.stdout.write(msg.toXml() + "\r\n")
        elif pm=='body': sys.stdout.write(str(msg) + '\r\n')
    def recon(self):
        self.xmlstream.transport.loseConnection()
    def prepareShellNamespace(self):
        theShellNamespace['recon'] = self.recon
        for k in settings['classdict'].keys():
            theShellNamespace[k] = functools.partial(
                messageFac, k, MYURL)


def getManholeFactory(namespace, **passwords):
    realm = manhole_ssh.TerminalRealm()
    def getManhole(_): return manhole.Manhole(namespace)
    realm.chainedProtocolFactory.protocolFactory = getManhole
    p = portal.Portal(realm)
    p.registerChecker(
        checkers.InMemoryUsernamePasswordDatabaseDontUse(**passwords)
    )
    f = manhole_ssh.ConchFactory(p)
    return f

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
    theShellNamespace.update(globals())
    reactor.listenTCP(SSHPORT, getManholeFactory(theShellNamespace,
                                                 sabaini="aaa"))
    reactor.run()

