import threading
from emkuupy import connection
import logging

PORT = 2346

class NullHandler(logging.Handler):
    def emit(self, record):
        pass
log = logging.getLogger("emkuupy")
log.addHandler(NullHandler())


class EmkuuManagedConnection(threading.Thread):
    """A connection management class"""

    #----------------------------------------------------------------------
    def __init__(self, host="localhost", port=None, listener_factory=None, listener_args=[]):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.connection = connection.EmkuuConnection(
            host=host, port=port)
        log.debug("con created")
        try:
            l = listener_factory(*([self.connection, self] + listener_args))
            self.listener = l
            self.connection.add_listener(l)
        except Exception, det:
            print det
        self.stopevt = threading.Event()

    def run(self):
        log.debug("running...")
        while 1:
            self.stopevt.wait(2)

    def stop(self):
        log.debug("stopping...")
        self.connection.close()
        self.stopevt.set()
        del self.connection

    def __del__(self):
        self.stop()


