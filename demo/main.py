"""Web server that provides interactive demo page.

Usage: 

    python main.py [port]

"""

import errno
import httplib
import os
import re
import sys
import tornado.ioloop
import tornado.web

from subprocess import PIPE, Popen
from threading  import Thread
from Queue import Queue, Empty

class ComputerPlayerDaemon(object):
    """Daemon that maintains a queue of game scenarios and allows the computer
    player (running as a separate process) to respond to them.
    """
   
    # Number of attempts that will be made (per request) to get a response
    # from the separate computer player process before the request is aborted.
    ATTEMPTS = 3

    # Number of seconds to wait on the queue for a new scenario before checking
    # the 'stop_signal' flag.
    TIMEOUT = 3

    # Game state returned from the computer player process if the scenario 
    # is illegal.
    ILLEGAL_STATE = '4'

    # Set to True to shutdown the daemon. 
    stop_signal = False
    
    # Queue of game scenarios awaiting response.
    q = Queue()

    @classmethod
    def stop(cls):
        """Signal to the daemon that it needs to shut itself down."""
        cls.stop_signal = True

    @classmethod
    def put(cls, scenario, handler):
        """Add a scenario (and its request handler) to the queue."""
        cls.q.put((scenario, handler))

    @classmethod
    def start(cls):
        player = cls.connect()
        while True:

            # Block on queue, waiting for next scenario to respond to. 
            # Periodically checking to see if the 'stop_signal' has been set.
            try:
                scenario, handler = cls.q.get(timeout=cls.TIMEOUT)
            except Empty:
                if cls.stop_signal:     return
                else:                   continue

            # Attempt to get a response to the scenario from the computer
            # player. If the connection to the computer player has been broken
            # an attempt will be made to reestablish it (but only a finite
            # number of times).
            for _ in range(cls.ATTEMPTS):

                try:
                    player.stdin.write(scenario+'\n')
                except IOError as e:
                    if e.errno == errno.EPIPE:
                        print "Connection to player went down."
                        player = cls.connect()
                        continue
                    else:
                        raise

                # Check that the computer player didn't reject the scenario
                # as illegal.
                response = player.stdout.readline()
                if response[0] == cls.ILLEGAL_STATE:
                    handler.send_error(status_code=httplib.BAD_REQUEST)
                else: 
                    handler.write(response)
                    handler.finish()
                break

            else:
                print "Connection to player couldn't be established."
                handler.send_error(status_code=httplib.SERVICE_UNAVAILABLE)

    @staticmethod
    def connect():
        """Create a new computer player process and establish a connection to
        it's stdin and stdout channels.
        """
        return Popen(['../Player'], shell=False, stdin=PIPE, stdout=PIPE)


class MainHandler(tornado.web.RequestHandler):
    """Handler to return single HTML page."""
    def get(self):
        self.render('index.html')


class GameHandler(tornado.web.RequestHandler):
    """Handler to allow computer player to respond to game scenarios."""

    validator = re.compile('[123]{9}$')

    @tornado.web.asynchronous
    def get(self):
        scenario = self.get_argument('q')
        if self.validator.match(scenario):  
            ComputerPlayerDaemon.put(scenario, self)
        else:
            self.send_error(status_code=httplib.BAD_REQUEST)


if __name__ == "__main__":
    application = tornado.web.Application([
        (r"/",      MainHandler),
        (r"/game",  GameHandler),
        ], 
        static_path=os.path.join(os.path.dirname(__file__), "static")) 
    application.listen(port=int(sys.argv[1]))
    Thread(target=ComputerPlayerDaemon.start).start()
    try:
        tornado.ioloop.IOLoop.instance().start()
    except KeyboardInterrupt:
        ComputerPlayerDaemon.stop()
        raise
