"""Some basic authenticators for general use
"""

from twisted.words.protocols.jabber import xmlstream
import constants

def debug(elem=None):
    if elem is None:
        print None
        return
    print elem.toXml().encode('utf-8')
    print "="*20



class ListenAuth(xmlstream.ListenAuthenticator):

    namespace = constants.NS_EMKUU

    def connectionMade(self):
        "incoming client connection"
        self.xmlstream.namespace = self.namespace
        self.xmlstream.sendHeader()


    def streamStarted(self, root):
        #print "in started, root: ", debug(root)
        #xmlstream.ListenAuthenticator.streamStarted(self, root)
        xmlstream.Authenticator.streamStarted(self, root)
        self.xmlstream.namespace = self.namespace
        self.xmlstream.dispatch(self, xmlstream.STREAM_AUTHD_EVENT)

    def associateWithStream(self, xs):
        #print "in assoc"
        xmlstream.ListenAuthenticator.associateWithStream(self, xs)


class SimpleConnectAuth(xmlstream.Authenticator):

    namespace = constants.NS_EMKUU

    def connectionMade(self):
        print "connectionMade()"
        self.xmlstream.namespace = self.namespace
        self.xmlstream.sendHeader()

    def streamStarted(self, root):
        print "streamStarted()"
        xmlstream.Authenticator.streamStarted(self, root)
        self.xmlstream.dispatch(self, xmlstream.STREAM_AUTHD_EVENT)
