"""PathListeners

Some base classes for convenient listener implementation
"""

import re
import logging
import pdb

log = logging.getLogger("emkuupy")

class PathListener(object):
    """Handle messages for a specific path
    """
    def __init__(self, con, svc, path):
        self.con = con
        self.svc = svc
        self.pathre = re.compile(path)
        log.debug("init done")

    def __call__(self, msg):
        try:
            p = msg.to.path_string()
            log.debug("called w/ path %s" % p)
            match = self.pathre.match(p)
            if match:
                r = p[match.end():]
                return self.handleMatch(msg, r)
            else:
                return msg
        except Exception, detail:
            log.error("error handling msg:\n%s\nreraising" % msg.to_xml())
            raise

    def handleMatch(self, msg, pathrest):
        raise Exception("override handleMatch()")


class MessageKindListener(PathListener):
    """Handlers for different message kinds
    Handlers are methods following the nameing scheme
    'handle' + Message classname, eg.
    'handlePostMsg'
    """

    def handleMatch(self, msg, pathrest):
        log.debug("handle %s, pathrest %s" % (msg.msgid, pathrest))
        hdlrname = "handle" + msg.__class__.__name__
        hdlr = getattr(self, hdlrname)
        return hdlr(msg, pathrest)
    def handleDeleteMsg(self, msg, pathrest):
        return msg
    def handleGetMsg(self, msg, pathrest):
        return msg
    def handlePostMsg(self, msg, pathrest):
        return msg
    def handlePutMsg(self, msg, pathrest):
        return msg
    def handleRouteMsg(self, msg, pathrest):
        return msg
    def handleStateMsg(self, msg, pathrest):
        return msg
    def handleSubscribeMsg(self, msg, pathrest):
        return msg
