import errno
import socket
import threading

class Scanner(threading.Thread):
    timeouts = 0
    errors = []
    def __init__(self, address, port):
        self.address = address
        self.port = port
        self.is_active = False
        super(Scanner, self).__init__()

    def run( self ):
        sd = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sd.settimeout(3.0)
        try:
            sd.connect((self.address, self.port))
        except socket.timeout, e:
            Scanner.timeouts += 1
        except socket.error, e:
            self.errors.append(e)
        else:
            self.is_active = True
        finally:
            sd.close()
            del sd

def scan(net, port):
    pool = []
    hosts = []

    for x in range(1,255):
        address = net + '.' + str( x )
        th = Scanner(address, port)
        th.start()
        pool.append(th)

    for th in pool:
        th.join()
        if th.is_active :
            hosts.append(th.address)

    return hosts

if __name__ == '__main__' :
    hosts = scan('192.168.0' , 22)
    print hosts
    print 'Timeouts:', Scanner.timeouts
    print 'Errors:', len(Scanner.errors)
