# Copyright (C) 2011 by Enrico Franchi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


import os
import time
import random

import functools

import tkMessageBox

from ScrolledText import ScrolledText

from twisted.protocols import basic
from twisted.internet import protocol, reactor, tksupport, defer
from twisted.application import service, internet

import Tkinter as tk

### CONF ###


STANDARD_PORT = 11000 + random.randint(0, 5000)


class CommandInterpreter(basic.LineReceiver):
    def lineReceived(self, line):
        try:
            command_name, rest = line.split(' ', 1)
        except ValueError:
            command_name = line.strip()
            self.call(command_name)
        else:
            self.call(command_name, rest)

### SERVER STUFF ###

class Unregistered(object):
    def __init__(self, auth_server):
        self.auth_server = auth_server

    def login(self, handle, user, pwd):
        if self.auth_server.valid(user, pwd):
            handle.user_name = user
            handle.password = pwd
            self.auth_server.finalize_login(handle)
            return '!login_success'
        else:
            return '!login_failure Invalid username or password'

    def logout(self, handle):
        return '!logout_failure Cannot logout, not logged in'

class LoggedIn(object):
    def __init__(self, auth_server):
        self.auth_server = auth_server

    def login(self, _handle, _user, _pwd):
        return '!login_failure Already logged in.'

    def logout(self, handle):
        self.auth_server.finalize_logout(handle)
        return '!logout_success'

class ServerHandle(CommandInterpreter):
    commands = ['login', 'msg', 'logout']
    
    def call(self, command_name, rest=None):
        if self.is_command(command_name):
            getattr(self, command_name[1:])(rest)

    def connectionMade(self):
        # bad
        self.state = Unregistered(self.factory)
        self.factory.unregistered_clients.add(self)

    def connectionLost(self, _reason):
        self.factory.forget(self)

    def login(self, rest):
        user, pwd = rest.split(' ', 1)
        pwd = pwd.strip()
        self.sendLine(self.state.login(self, user, pwd))

    def logout(self, _rest):
        self.sendLine(self.state.logout(self))

    def msg(self, rest):
        self.factory.tell_all(rest)

    def send_message(self, msg):
        self.sendLine('!msg %s %s' % (self.user_name, msg))

    def is_command(self, command_name):
        return (command_name.startswith('/') and
                command_name[1:] in self.commands)


class Server(protocol.ServerFactory):
    protocol = ServerHandle

    def __init__(self, auth_db):
        self.unregistered_clients = set()
        self.clients = set()
        self.auth_db = auth_db

    def valid(self, user, pwd):
        try:
            if self.auth_db[user] == pwd:
                return True
        except KeyError:
            pass
        return False

    def forget(self, handle):
        self.finalize_logout(handle)
        try:
            self.unregistered_clients.remove(handle)
        except KeyError: pass
        ## THIS SHUTS DOWN EVERYTHING WHEN THE LAST CLIENT DROPS
        if not (self.clients or self.unregistered_clients):
            reactor.stop()

    def finalize_login(self, handle):
        try:
            self.unregistered_clients.remove(handle)
        except KeyError:
            print 'Something bad happened.'
        self.clients.add(handle)
        handle.state = LoggedIn(self)

    def finalize_logout(self, handle):
        try:
            self.clients.remove(handle)
        except KeyError:
            pass
        self.unregistered_clients.add(handle)
        handle.state = Unregistered(self)

    def tell_all(self, msg):
        for handle in self.clients:
            handle.send_message(msg)

def make_serv(auth_db = {}):
    factory = Server(auth_db)

    reactor.listenTCP(STANDARD_PORT, factory)
    reactor.run()


### CLIENT STUFF ###


class LoginWindow(object):
    def __init__(self, parent):
        self.parent = parent
        self.build_window()

    def build_window(self):
        f = tk.Frame(self.parent)
        f.pack(padx=10,pady=10)

        self.username = tk.Entry(f, text="username")
        self.username.pack(side=tk.TOP, padx=10, pady=12)
        self.username.focus_force()

        self.password = tk.Entry(f,text="******")
        self.password.pack(side=tk.TOP, padx=10, pady=12)

        self.act = tk.Button(f, text="Login", command=self.login)
        self.act.pack(side=tk.BOTTOM, padx=10, pady=10)
        self.unlock()

    def login(self, _evt=None):
        self.handle.login(self.username.get(), self.password.get())

    def lock(self):
        self.username.config(state=tk.DISABLED)
        self.password.config(state=tk.DISABLED)
        self.act.config(state=tk.DISABLED)
        self.parent.unbind('<Return>')

    def unlock(self):
        self.username.config(state=tk.NORMAL)
        self.password.config(state=tk.NORMAL)
        self.act.config(state=tk.NORMAL)
        self.parent.bind('<Return>', self.login)

class ChatWindow(object):
    def __init__(self, parent, line_sender):
        self.parent = parent
        self.main = self.build_window()
        self.line_sender = line_sender

    def build_window(self):
        self.toplevel = tk.Toplevel(self.parent)
        f = tk.Frame(self.toplevel)
        f.pack()
        
        self.area = ScrolledText(f)
        self.area.pack(side=tk.TOP)
        self.area.config(state=tk.DISABLED)

        self.line = tk.Entry(f)
        self.line.pack(side=tk.BOTTOM, padx=10, pady=10)
        self.line.focus_force()

        self.toplevel.bind('<Return>', self.send_line)
        return f

    def send_line(self, _evt):
        line = self.line.get()
        self.line.delete(0, tk.END)
        self.line_sender(line)

    def add_message(self, msg):
        self.area.config(state=tk.NORMAL)
        self.area.insert(tk.END, '%s\n' % msg)
        self.area.config(state=tk.DISABLED)

    def set_dispose_callback(self, callback):
        self.toplevel.protocol('WM_DELETE_WINDOW', callback)

class ClientHandle(CommandInterpreter):
    commands = ['login_failure', 'login_success', 'logout_success', 'logout_failure', 'msg']
    
    def __init__(self, tkroot, on_connect, on_login, on_logout):
        self.tkroot = tkroot
        self.on_connect = on_connect
        self.on_login = on_login
        self.on_logout = on_logout
        
    def login(self, user_name, pwd):
        self.sendLine('/login %s %s' % (user_name, pwd))

    def logout(self):
        self.sendLine('/logout')

    def send_message(self, msg):
        self.sendLine('/msg %s' % msg)

    def call(self, command_name, rest=None):
        if self.is_command(command_name):
            getattr(self, command_name[1:])(rest)

    def is_command(self, command_name):
        return (command_name.startswith('!') and
                command_name[1:] in self.commands)

    def connectionMade(self):
        self.tkroot.protocol('WM_DELETE_WINDOW', reactor.stop)
        self.on_connect(self)

    def login_failure(self, rest):
        tkMessageBox.showerror(
            "Login Error!",
            rest.strip()
        )

    def login_success(self, rest):
        self.chat_window = ChatWindow(self.tkroot, self.send_message)
        self.chat_window.set_dispose_callback(self.logout)
        self.on_login()

    def logout_failure(self, rest):
        tkMessageBox.showerror(
            "Logout Error!",
            rest.strip()
        )

    def logout_success(self, rest):
        self.on_logout()

    def msg(self, rest):
        user_name, message = rest.split(' ', 1)
        self.chat_window.add_message('%s: %s' % (user_name, message))

class ClientFactory(protocol.ClientFactory):
    protocol = ClientHandle

    def __init__(self, tkroot, on_connect, on_login, on_logout):
        self.root = tkroot
        self.on_connect = on_connect
        self.on_login = on_login
        self.on_logout = on_logout

    def buildProtocol(self, _addr):
        return self.protocol(self.root,
                             self.on_connect,
                             self.on_login,
                             self.on_logout)

    def clientConnectionFailed(self, connector, reason):
        print 'connection failed:', reason.getErrorMessage()
        reactor.stop()

    def clientConnectionLost(self, connector, reason):
        self.on_logout()

def make_client():
    root = tk.Tk()
    root.title('Tkwidgets application')
        
    login_window = LoginWindow(root)

    factory = ClientFactory(
            root,
            on_connect=functools.partial(setattr, login_window, 'handle'),
            on_login=login_window.lock,
            on_logout=login_window.unlock
    )

    reactor.connectTCP('localhost', STANDARD_PORT, factory)

    tksupport.install(root)

    reactor.run()


if __name__ == '__main__':
    if os.fork():
        ## CLIENT CODE
        time.sleep(0.5) # wait for the server to be up
        print 'STARTING CLIENT'
        make_client()
        print 'STOPPED CLIENT'
    else:
        print 'STARTING SERVER'
        make_serv(dict(foo='bar'))
        print 'STOPPED SERVER'


