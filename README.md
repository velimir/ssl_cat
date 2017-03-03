# SSL cat #

Small application which was built to reproduce issue with ssl sockets, when messages are received one by one using `[{active, once}]` method.

## Build and run ##

    $ rebar3 get-deps
    $ rebar3 compile
    $ rebar3 shell --apps ssl_cat

## Steps to reproduce ##

Application starts 2 listeners on ports 8443 (`gen_tcp`) and 9443 (`ssl`). Both of them has the same protocol, which will be described later. The only difference is 8443 proxies requests to 9443 and back to requester.

Listeners receive file names, which are read from `priv` directory and sent back to requester.

### Working example ###

Application works fine before Erlang 19.0, hence if you need to see that it works as it's supposed to work, you need to use Erlang version before 19.0, (let's say 18.3, on which it was tested).

Run `ssl_cat` in one of terminals, as it's said above. Service will produce some traces, that it's started:

```
    (18.3)bash-3.2$ rebar3 shell --apps ssl_cat
    ===> Verifying dependencies...
    ===> Compiling ssl_cat
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V7.3  (abort with ^G)
    1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using rel
    eases (http://www.rebar3.org/docs/releases)
    14:15:05.373 [info] Application lager started on node nonode@nohost
    ===> Booted syntax_tools
    14:15:05.377 [info] Application ssl_cat started on node nonode@nohost
    ===> Booted compiler
    ===> Booted goldrush
    ===> Booted lager
    ===> Booted ssl_cat
    14:15:05.395 [info] gen tcp is listening on port: 8443
    14:15:05.429 [info] ssl is listening on port 9443
```

Once it's started ask to read and reply back `small.json` file from `priv` directory:

```
    $ telnet 127.0.0.1 8443 > out.json
    small.json
    Connection closed by foreign host.
```

`ssl_cat` produce something similar to this in the log:

```
    14:17:38.353 [info] tcp connection accepted
    14:17:38.385 [info] ssl transport accepted
    14:17:38.483 [info] ssl accepted
    14:17:38.483 [info] connected to ssl reader
    14:17:43.967 [info] received data from frontend
    14:17:43.968 [info] ssl received: <<"small.json\r\n">>
    14:17:43.968 [info] reading file: "< ... >/ssl_cat/_build/default/lib/ssl_cat/priv/small.json"
    14:17:43.998 [info] file is sent
    14:17:43.998 [info] received data from backed: 16384, total: 16384
    14:17:43.999 [info] received data from backed: 16384, total: 32768
    14:17:44.000 [info] received data from backed: 16384, total: 49152
    ...... skipped ....
    14:17:44.268 [info] received data from backed: 11391, total: 1895551
    14:17:44.269 [info] backend closed connection: ssl_closed
```

Notice that total bytes sent is equal to the size of small.json file:

```
-rw-r--r--  1 user  group  1895551 Feb 22 13:57 priv/small.json
```

You can check, that `out.json` (output from telnet) contains the whole content from small.json.

### Erlang 19 issue ###

Try to do the same steps, but use Erlang 19.(0|1|2).
Ask for `small.json` file once again, you'll see something like that in the output:

```
    14:23:41.071 [info] tcp connection accepted
    14:23:41.103 [info] ssl transport accepted
    14:23:41.212 [info] ssl accepted
    14:23:41.212 [info] connected to ssl reader
    14:23:43.789 [info] received data from frontend
    14:23:43.789 [info] ssl received: <<"small.json\r\n">>
    14:23:43.789 [info] reading file: "< ... >/ssl_cat/_build/default/lib/ssl_cat/priv/small.json"
    14:23:43.796 [info] file is sent
    14:23:43.796 [info] received data from backed: 16384, total: 16384
    ...... skipped ....
    14:23:43.875 [info] received data from backed: 16384, total: 1884160
    14:23:43.875 [info] ssl closed
    14:23:43.880 [info] backend closed connection: ssl_closed
```

Notice, that `total` size is smaller, than `small.json` file. You can verify, that `out.json` now has a cut version of `small.json` file.
Issue is `{ssl_closed, Socket}` message is received earlier, than other buffer (packet) from socket even though according to tcp trace and Erlang trace the whole file was successfully transferred from `9443` to requester (`ssl_cat_listener`).

## Notes ##

Issue is reproducible when:
* Application is run on Erlang 19
* `ssl_cat_ssl_reader` uses ssl under the hood (`gen_tcp` works fine)
* `ssl_cat_listner` uses `{active, once}` method in a loop. If `{active, true}` is set issue is not reproducible.

    ```erlang
        proxy_request(FSocket, BSocket) ->
            %% -------------------vvvvvvvvvvvvvvv
            inet:setopts(FSocket, [{active, once}]),
            ssl:setopts(BSocket, [{active, once}]),
            %% -------------------^^^^^^^^^^^^^^^
            receive
                {_, FSocket, Data} ->
                    ok = ssl:send(BSocket, Data),
                    proxy_request(FSocket, BSocket);
                {_, BSocket, Data} ->
                    ok = gen_tcp:send(FSocket, Data),
                    proxy_request(FSocket, BSocket);
                %% ....
            end.
    ```
