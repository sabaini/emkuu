"""Interfaces
"""

from zope.interface import Interface, Attribute

class IMessage(Interface):
    """I am a message
    """
    
    msgid = Attribute(
        """Message identifier, a string of the form 'appname:counter'
        """)
    