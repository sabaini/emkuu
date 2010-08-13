"""Interface definitions
"""

from zope.interface import Interface, Attribute

class IEmkuuMessageSource(Interface):
    """I produce messages and pass them to a callback which can be 
    set externally 
    """
    messageCallback = Attribute(
        """Callable which gets called when a new message arrives
        It has to return the message again
        """)
    
class IEmkuuMessageSink(Interface):
    """I accept messages and send them on to the emkuu broker
    """

    def send(msg, correlationCallback=None):
        """Send a message msg. Optionally pass in a callback func 
        which gets called when a correlated message is recvd.
        Return a deferred which is fired when the message is acked 
        """

