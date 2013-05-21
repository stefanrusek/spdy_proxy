spdy_proxy
==========

A simple, scalable SPDY -> HTTP proxy
-------------------------------------

spdy_proxy is a lightweight SPDY server. Its primary purpose is to sit
betwen HAProxy (1.5 and later) and any web server. HAProxy provides
SSL and spdy_proxy provides unencrypted SPDY.

FAQ
---

Q: Wouldn't this just defeat the purpose of SPDY?

A: Actually, no, it doesn't. There are numerous advantages to SPDY, but
the two big advantages over HTTPS (that is secure HTTP), are multi-
channel communications, and server->client streams.

spdy_proxy proxies at the SPDY channel level, so each SPDY stream is
proxied to a separate HTTP request. This means that spdy_proxy gives
you multi-stream abilities out of the box.

For the additional stream features of SPDY, we hope to add X-SPDY-*
response headers that you can use to tell spdy_proxy to perform
different operations. For example, you could use X-SPDY-MAX-STREAMS
to tell spdy_proxy to tell the client the maximum number of active
streams that it is allowed to create.

Installation
------------

NOTE: Installation is not compelete yet, but this is how it will work
when it works. ;)

spdy_proxy requires erlang to be installed, additionally git is
required to build spdy_proxy.

On Ubuntu:

    sudo apt-get install erlang
    make
    sudo make install

This will install spdy_proxy and put a reasonable default
spdy_proxy.config file in /etc/spdy_proxy/.

On Windows:

First ensure erlang and git are in your path.

    cd install
    install.ps1

This will install spdy_proxy as a service.

HAProxy
-------

Once you have HAProxy serving SSL, adding SPDY is quite simple.

First install spdy_proxy on each of your web servers. Then make the
following changes to your haproxy.cfg file.

1. Add a spdy backend, with an entry for each over your servers:

    backend spdy_back
        balance roundrobin
        mode tcp
        server spdy1 server1:9999 weight 1 maxconn 25000 check

2. Add "npn http/1.1,http1.0,spdy/3,spdy/2" to the end of your ssl
directive.

3. Add the following to your https frontend:

    use_backend spdy_back if { ssl_fc_npn -i spdy/3 }
    use_backend spdy_back if { ssl_fc_npn -i spdy/2 }

Without HAProxy
---------------

Though untested, it should be possible to add your ssl cert info to
your spdy_proxy.config file and use erlang's ssl libraries:

    {next_protocols_advertised, [<<"spdy/3">>, <<"spdy/2">>]}.
    {keyfile, "server.key"}.
    {certfile, "server.crt"}.
    {cacertfile, "gd_bundle.crt"}.
    
LICENSE
-------

Copyright (c) 2013 Stefan Rusek

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
