"""Message handling utils
"""

from twisted.words.xish import domish
import datetime

def rfc3339now():
    s = datetime.datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')
    return s

class GenericElement(domish.Element):
    """Convenience for constructing domish.Elements"""
    def __init__(self, kind, body=None, fro=None, to=None,
                 corrid=None):
        domish.Element.__init__(self, (None, kind))
        if body is not None: self.addContent(body)
        if fro is not None: self['fro'] = fro
        if to is not None: self['to'] = to
        if corrid is not None: self['corrid'] = corrid
        self['date'] = rfc3339now()


class StateElement(GenericElement):
    def __init__(self, body=None, fro=None, to=None,
                 corrid=None):
        GenericElement.__init__(self, 'state', body, fro, to, corrid)
        self['status'] = str(200)

class GetElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'get', None, fro, to, corrid)

class PutElement(GenericElement):
    def __init__(self, body=None, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'put', body, fro, to, corrid)

class CreateElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None,
                 location=None):
        GenericElement.__init__(self, 'create', None, fro, to, corrid)
        if location is not None: self['location'] = location

class PostElement(GenericElement):
    def __init__(self, body=None, fro=None, to=None, corrid=None,
                 location=None, posttoken=None):
        GenericElement.__init__(self, 'post', body, fro, to, corrid)
        if location is not None: self['location'] = location
        if posttoken is not None: self['posttoken'] = posttoken

class DeleteElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'delete', None, fro, to, corrid)

class OptionsElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'options', None, fro, to, corrid)

# lock
# unlock

class SubscribeElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'subscribe', None, fro, to, corrid)

class UnSubscribeElement(GenericElement):
    def __init__(self, fro=None, to=None, corrid=None):
        GenericElement.__init__(self, 'unsubscribe', None, fro, to, corrid)

class RouteElement(GenericElement):
    def __init__(self, body=None, fro=None, to=None, corrid=None,
                 location=None, posttoken=None):
        GenericElement.__init__(self, 'route', None, fro, to, corrid)
        if body is not None:
            self.addChild(body)



# fragment






class InfoElement(StateElement):
    """Convenience: for simple info messages
    """
    infomsg = {
        201 : "Created",
        204 : "No Content",
        400 : "Invalid request",
        404 : "Not found",
        409 : "Conflict",
        500 : "Application error",
        }
    def __init__(self, code, fro=None, to=None, corrid=None):
        b = self.infomsg[code]
        StateElement.__init__(self, body=b, fro=fro, to=to, corrid=corrid)
        self['status'] = str(code)

