%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <gstarinkin@alertlogic.com>
%%% @copyright (C) 2017, Grigory Starinkin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2017 by Grigory Starinkin <gstarinkin@alertlogic.com>
%%%-------------------------------------------------------------------
-module(ssl_cat_ssl_reader).

%% API
-export([start_link/0, init/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Pid = proc_lib:spawn_link(?MODULE, init, []),
    {ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    Port = 9443,
    CertFile = filename:join(code:priv_dir(ssl_cat), "server.crt"),
    KeyFile = filename:join(code:priv_dir(ssl_cat), "server.key"),
    ListenOpts = [binary,
                  {active, false},
                  {backlog, 1024}, 
                  {packet, raw}, 
                  {recbuf, 8192},
                  {reuseaddr, true},
                  {certfile, CertFile},
                  {keyfile, KeyFile}],
    {ok, Socket} = ssl:listen(Port, ListenOpts),
    lager:info("ssl is listening on port ~p", [Port]),
    loop(Socket).

loop(LSocket) ->
    {ok, CSocket} = ssl:transport_accept(LSocket),
    lager:info("ssl transport accepted"),
    ok = ssl:ssl_accept(CSocket),
    lager:info("ssl accepted"),
    Filename = receive_file_name(CSocket),
    {ok, FileContent} = read_file(Filename),
    ok = ssl:send(CSocket, FileContent),
    lager:info("file is sent"),
    ok = ssl:close(CSocket),
    lager:info("ssl closed"),
    loop(LSocket).

receive_file_name(Socket) ->
    {ok, Data} = ssl:recv(Socket, 0),
    lager:info("ssl received: ~p", [Data]),
    string:strip(binary_to_list(trim(Data))).

trim(Bin) ->
    re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).

read_file(FileName) ->
    PrivDir = code:priv_dir(ssl_cat),
    FFName = filename:join(PrivDir, FileName),
    lager:info("reading file: ~p", [FFName]),
    file:read_file(FFName).
