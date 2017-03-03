%%%-------------------------------------------------------------------
%% @doc ssl_cat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ssl_cat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildrenSpec = [
                    {ssl_cat_ssl_reader, {ssl_cat_ssl_reader, start_link, []},
                     permanent, 1000, worker, [ssl_cat_ssl_reader_srv]},
                    {ssl_cat_listener, {ssl_cat_listener, start_link, []},
                     permanent, 1000, worker, [ssl_cat_listener]}
                   ],
    {ok, {{one_for_one, 5, 10}, ChildrenSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
