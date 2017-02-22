%%%-------------------------------------------------------------------
%% @doc scat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scat_sup).

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
                    {scat_ssl_reader, {scat_ssl_reader, start_link, []},
                     permanent, 1000, worker, [scat_ssl_reader_srv]},
                    {scat_listner, {scat_listner, start_link, []},
                     permanent, 1000, worker, [scat_listner]}
                   ],
    {ok, {{one_for_one, 5, 10}, ChildrenSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
