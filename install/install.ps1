
cd ..

escript get-deps compile escriptize

erlsrv add spdy_proxy `
    -onfail restart `
    -workdir "$(pwd)" `
    -args '-pa ebin deps/*/ebin -eval "spdy_proxy:main([])."' `
    -comment 'spdy_proxy is a simple, scalable SPDY -> HTTP proxy'

erlsrv start spdy_proxy

