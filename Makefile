
all:
	./rebar get-deps compile escriptize

clean:
	./rebar clean

install: ALWAYS
	install install/init /etc/init.d/spdy_proxy
   	update-rc.d spdy_proxy defaults

ALWAYS:
