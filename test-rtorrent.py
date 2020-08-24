#!/usr/bin/env python2

# This handy Python script checks if your rtorrent XMLRPC
# setup is working without relying on Emacs.

# Run as follows:
# python rtorrent-test.py


# Copyright (C) 2016-2020 Stefan Kangas.
#
# This file is NOT part of GNU Emacs.
#
# Including code from:
# rtorrent_xmlrpc
# (c) 2011 Roger Que <alerante@bellsouth.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
#
# Portions based on Python's xmlrpclib:
#
# Copyright (c) 1999-2002 by Secret Labs AB
# Copyright (c) 1999-2002 by Fredrik Lundh
#
# By obtaining, using, and/or copying this software and/or its
# associated documentation, you agree that you have read, understood,
# and will comply with the following terms and conditions:
#
# Permission to use, copy, modify, and distribute this software and
# its associated documentation for any purpose and without fee is
# hereby granted, provided that the above copyright notice appears in
# all copies, and that both that copyright notice and this permission
# notice appear in supporting documentation, and that the name of
# Secret Labs AB or the author not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# SECRET LABS AB AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
# TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANT-
# ABILITY AND FITNESS.  IN NO EVENT SHALL SECRET LABS AB OR THE AUTHOR
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.

import argparse
import re
import socket
import urllib
import xmlrpclib

class SCGITransport(xmlrpclib.Transport):

    # Usage: server = SCGIServerProxy('scgi://localhost:5000/')
    #        server = SCGIServerProxy('scgi:///path/to/scgi.sock')
    #        print server.system.listMethods()
    #        mc = xmlrpclib.MultiCall(server)
    #        mc.get_up_rate()
    #        mc.get_down_rate()
    #        print mc()

    def single_request(self, host, handler, request_body, verbose=0):
        # Add SCGI headers to the request.
        headers = {'CONTENT_LENGTH': str(len(request_body)), 'SCGI': '1'}
        header = '\x00'.join(('%s\x00%s' % item for item in headers.iteritems())) + '\x00'
        header = '%d:%s' % (len(header), header)
        request_body = '%s,%s' % (header, request_body)

        sock = None

        try:
            if host:
                host, port = urllib.splitport(host)
                addrinfo = socket.getaddrinfo(host, port, socket.AF_INET,
                                              socket.SOCK_STREAM)
                sock = socket.socket(*addrinfo[0][:3])
                sock.connect(addrinfo[0][4])
            else:
                sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                sock.connect(handler)

            self.verbose = verbose

            sock.send(request_body)
            return self.parse_response(sock.makefile())
        finally:
            if sock:
                sock.close()

    def parse_response(self, response):
        p, u = self.getparser()

        response_body = ''
        while True:
            data = response.read(1024)
            if not data:
                break
            response_body += data

        # Remove SCGI headers from the response.
        response_header, response_body = re.split(r'\n\s*?\n', response_body,
                                                  maxsplit=1)

        if self.verbose:
            print 'body:', repr(response_body)

        p.feed(response_body)
        p.close()

        return u.close()


class SCGIServerProxy(xmlrpclib.ServerProxy):
    def __init__(self, uri, transport=None, encoding=None, verbose=False,
                 allow_none=False, use_datetime=False):
        type, uri = urllib.splittype(uri)
        if type not in ('scgi'):
            raise IOError('unsupported XML-RPC protocol')
        self.__host, self.__handler = urllib.splithost(uri)
        if not self.__handler:
            self.__handler = '/'

        if transport is None:
            transport = SCGITransport(use_datetime=use_datetime)
        self.__transport = transport

        self.__encoding = encoding
        self.__verbose = verbose
        self.__allow_none = allow_none

    def __close(self):
        self.__transport.close()

    def __request(self, methodname, params):
        # call a method on the remote server

        request = xmlrpclib.dumps(params, methodname, encoding=self.__encoding,
                                  allow_none=self.__allow_none)

        response = self.__transport.request(
            self.__host,
            self.__handler,
            request,
            verbose=self.__verbose
            )

        if len(response) == 1:
            response = response[0]

        return response

    def __repr__(self):
        return (
            "<SCGIServerProxy for %s%s>" %
            (self.__host, self.__handler)
            )

    __str__ = __repr__

    def __getattr__(self, name):
        # magic method dispatcher
        return xmlrpclib._Method(self.__request, name)

    # note: to call a remote object with an non-standard name, use
    # result getattr(server, "strange-python-name")(args)

    def __call__(self, attr):
        """A workaround to get special attributes on the ServerProxy
           without interfering with the magic __getattr__
        """
        if attr == "close":
            return self.__close
        elif attr == "transport":
            return self.__transport
        raise AttributeError("Attribute %r not found" % (attr,))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Check if rtorrent xmlrpc is setup properly.")
    parser.add_argument('--addr', help="Address to your scgi",
                        default='scgi://127.0.0.1:5000/')
    parser.add_argument('--method', default=None,
                        help="Print result of custom method (e.g. system.listMethods")
    parser.add_argument('-v', dest='verbose',
                        help="Toggle verbosity", action='store_true')
    args = parser.parse_args()

    if args.addr[0:7] == 'scgi://':
        s = SCGIServerProxy(args.addr, verbose=args.verbose)
    else:
        s = xmlrpclib.ServerProxy(args.addr, verbose=args.verbose)

    cv = s.system.client_version()
    lv = s.system.library_version()
    av = s.system.api_version()
    print "rtorrent %s (%s) running api version %s on %s" % (cv, lv, av, args.addr)

    if args.method is not None:
        print getattr(s, args.method)()

    # methods = s.system.listMethods()
    # print "Methods: %s" % methods
