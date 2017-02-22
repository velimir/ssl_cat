%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <gstarinkin@alertlogic.com>
%%% @copyright (C) 2017, Grigory Starinkin
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2017 by Grigory Starinkin <gstarinkin@alertlogic.com>
%%%-------------------------------------------------------------------
-module(ssl_cat_listner).

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
    Port = 8443,
    ListenOpts = [binary,
                  {active, false},
                  {backlog, 1024}, 
                  {packet, raw}, 
                  {recbuf, 8192},
                  {reuseaddr, true}],
    {ok, Socket} = gen_tcp:listen(Port, ListenOpts),
    lager:info("gen tcp is listening on port: ~p", [Port]),
    loop(Socket).

loop(LSocket) ->
    {ok, CSocket} = gen_tcp:accept(LSocket),
    lager:info("tcp connection accepted"),
    {ok, BSocket} = connect_to_reader(),
    lager:info("connected to ssl reader"),
    proxy_request(CSocket, BSocket, 0),
    loop(LSocket).

connect_to_reader() ->
    ConnectOpts = [binary, {active, false}, {packet, raw}, {nodelay, true}],
    ssl:connect("127.0.0.1", 9443, ConnectOpts, 10000).

proxy_request(FSocket, BSocket, ReceivedBytesTotal) ->
    inet:setopts(FSocket, [{active, once}]),
    ssl:setopts(BSocket, [{active, once}]),

    receive
        {_, FSocket, Data} ->
            lager:info("received data from frontend"),
            ok = ssl:send(BSocket, Data),
            proxy_request(FSocket, BSocket, ReceivedBytesTotal);
        {_, BSocket, Data} ->
            ReceivedBytesSize = byte_size(Data),
            Total = ReceivedBytesSize + ReceivedBytesTotal,
            lager:info("received data from backed: ~p, total: ~p",
                       [ReceivedBytesSize, Total]),
            ok = gen_tcp:send(FSocket, Data),
            proxy_request(FSocket, BSocket, Total);
        {Reason, FSocket} ->
            lager:info("frontend closed connection: ~p", [Reason]),
            ok = ssl:close(BSocket);
        {Reason, BSocket} ->
            lager:info("backend closed connection: ~p", [Reason]),
            ok = gen_tcp:close(FSocket)
    end.
    
    
