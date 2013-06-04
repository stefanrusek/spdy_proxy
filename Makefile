
all:
	./rebar get-deps compile escriptize

clean:
	./rebar clean

install: ALWAYS
	install spdy_proxy /usr/local/bin/spdy_proxy
	install install/init /etc/init.d/spdy_proxy
	install -d /etc/spdy_proxy
	install install/spdy_proxy.config /etc/spdy_proxy/spdy_proxy.config
   	update-rc.d spdy_proxy defaults

ALWAYS:
