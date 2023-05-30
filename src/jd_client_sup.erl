%%% @doc
%%% Jex Daemon Client Supervisor
%%%
%%% This process supervises the client socket handlers themselves. It is a peer of the
%%% jd_client_man (the manager interface to this network service component),
%%% and a child of the supervisor named jd_clients.
%%%
%%% Because we don't know (or care) how many client connections the server may end up
%%% handling this is a simple_one_for_one supervisor which can spawn and manage as
%%% many identically defined workers as required, but cannot supervise any other types
%%% of processes (one of the tradeoffs of the "simple" in `simple_one_for_one').
%%%
%%% http://erlang.org/doc/design_principles/sup_princ.html#id79244
%%% @end

-module(jd_client_sup).
-vsn("0.1.0").
-behaviour(supervisor).
-author("Peter Harpending <peter.harpending@gmail.com>").
-copyright("Peter Harpending <peter.harpending@gmail.com>").
-license("MIT").


-export([start_acceptor/1]).
-export([start_link/0]).
-export([init/1]).



-spec start_acceptor(ListenSocket) -> Result
    when ListenSocket :: gen_tcp:socket(),
         Result       :: {ok, pid()}
                       | {error, Reason},
         Reason       :: {already_started, pid()}
                       | {shutdown, term()}
                       | term().
%% @private
%% Spawns the first listener at the request of the jd_client_man when
%% jexd:listen/1 is called, or the next listener at the request of the
%% currently listening jd_client when a connection is made.
%%
%% Error conditions, supervision strategies and other important issues are
%% explained in the supervisor module docs:
%% http://erlang.org/doc/man/supervisor.html

start_acceptor(ListenSocket) ->
    supervisor:start_child(?MODULE, [ListenSocket]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, none).


-spec init(none) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init(none) ->
    RestartStrategy = {simple_one_for_one, 1, 60},
    Client    = {jd_client,
                 {jd_client, start_link, []},
                 temporary,
                 brutal_kill,
                 worker,
                 [jd_client]},
    {ok, {RestartStrategy, [Client]}}.
