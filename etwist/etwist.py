import logging
from xml import sax

from zope.interface import implements
from zope.component import getGlobalSiteManager, getUtility, queryUtility

import twisted
from twisted.internet.protocol import Protocol, ClientFactory
from twisted.internet import reactor, defer
from twisted.python import components

import emkuupy, emkuupy.ct
import interfaces

class NullHandler(logging.Handler):
    def emit(self, record):
        pass
log = logging.getLogger("etwist")
log.addHandler(NullHandler())

class EmkuuProtocol(Protocol):
    implements(interfaces.IEmkuuMessageSource, interfaces.IEmkuuMessageSink)
    
    # NOP; override or set this
    messageCallback = lambda a: a
    # The messageCallback may return a Deferred in which case ack of the message 
    # will be sent when the Deferred fires
    
    def __init__(self, parser):
        self.parser = parser
        self.okmap = {}
        self.cbmap = {}
        
    def connectionMade(self):
        log.debug("con made")
        self.transport.write(emkuupy.ct.STREAMHEAD)
        
    def initCallback(self, inimsg):
        log.debug("inimsg: %s", inimsg)
        appname, cnt = inimsg.split(':')
        self.msgid = [appname, int(cnt)]
        
    def _sendOk(self, msg):
        okmsg = emkuupy.messages.OkMsg()
        okmsg.corrid = None
        okmsg.msgid = msg.msgid
        log.debug("send: %s : %s", okmsg, msg)
        self.transport.write(str(okmsg.to_xml()))
        
    def msgCallback(self, msg):
        if isinstance(msg, emkuupy.messages.OkMsg):
            msgid = msg.msgid
            log.debug("got ok %s", msgid)
            if self.okmap.has_key(msgid): 
                d = self.okmap.get(msgid)
                d.callback(msgid)
                del self.okmap[msgid]
        else:
            log.debug("regular msg: %s", msg)
            # handle message callbacks
            corrid = msg.corrid
            if self.cbmap.has_key(corrid):
                origmsg, cb = self.cbmap[corrid]
                cb(origmsg, msg)
                if not isinstance(emkuupy.messages.SubscribeMsg):
                    del self.cbmap[corrid]
            # pass it on to the message receiver
            d = defer.maybeDeferred(self.messageCallback, msg)
            d.pause()
            d.addCallback(self._sendOk)
            d.unpause()
            
    def dataReceived(self, data):
        log.debug("datarcvd: %s", data)
        self.parser.feed(data)
        
    def send(self, msg, correlationCallback=None):
        log.debug("sending %s, msgid: %s", msg, self.msgid)
        msg.msgid = "%s:%s" % tuple(self.msgid)
        if msg.corrid < 0:
            msg.corrid = self.msgid[1]
        self.msgid[1] += 1
        self.transport.write(str(msg.to_xml()))
        if correlationCallback:
            self.cbmap[msg.corrid] = (msg, correlationCallback)
        ackDefer = defer.Deferred()
        self.okmap[msg.msgid] = ackDefer
        return ackDefer
        
class EmkuuClientFactory(ClientFactory):
    
    appname = "(set me)"
    
    def buildProtocol(self, addr):
        log.debug("buildproto: %s", addr)
        parser = sax.make_parser()
        parser.setFeature(sax.handler.feature_namespaces, 1)
        self.proto = proto = EmkuuProtocol(parser)
        proto.contentHandler = emkuupy.ct.EmkuuContentHandler(proto.msgCallback, proto.initCallback)
        parser.setContentHandler(proto.contentHandler)
        gsm = getGlobalSiteManager()
        gsm.registerUtility(proto, interfaces.IEmkuuMessageSink, self.appname)
        gsm.registerUtility(proto, interfaces.IEmkuuMessageSource, self.appname)
        return proto

    
if __name__ == '__main__':    
    LOGFILE="/tmp/etwist.log"
    FORMAT="%(levelname)s %(asctime)s %(process)d %(name)s %(funcName)s %(message)s"
    logging.basicConfig(filename=LOGFILE,
                        level=logging.DEBUG,
                        format=FORMAT)    
    log = logging.getLogger("etwist")
    log.debug("etwist start")
    observer = twisted.python.log.PythonLoggingObserver()
    observer.start()
    f = EmkuuClientFactory()
    f.appname = 'etwist'
        
    reactor.connectTCP("localhost", 2354, f)
    
    def later():
        def g(msg):
            log.debug("he said: %s", getattr(msg, 'body', 'no body'))
            return msg   
        sink = getUtility(interfaces.IEmkuuMessageSink, f.appname)
        source = getUtility(interfaces.IEmkuuMessageSource, f.appname)
        sink.messageCallback = g
        msg = emkuupy.messages.GetMsg()
        msg.to = emkuupy.messages.Uri("/emkuu/foo")
        d = source.send(msg)
        d.addCallback(log.debug)
        
    reactor.callLater(2, later)
    reactor.run()
