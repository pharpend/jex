%%% @doc
%%% Jex Daemon Top-level Supervisor
%%%
%%% The very top level supervisor in the system. It only has one service branch: the
%%% client handling service. In a more complex system the client handling service would
%%% only be one part of a larger system. Were this a game system, for example, the
%%% item data management service would be a peer, as would a login credential provision
%%% service, game world event handling, and so on.
%%%
%%% See: http://erlang.org/doc/design_principles/applications.html
%%% See: http://zxq9.com/archives/1311
%%% @end

-module(jd_sup).
-vsn("0.1.0").
-behaviour(supervisor).
-author("Peter Harpending <peter.harpending@gmail.com>").
-copyright("Peter Harpending <peter.harpending@gmail.com>").
-license("MIT").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init([]) ->
    RestartStrategy = {one_for_one, 1, 60},
    Clients   = {jd_clients,
                 {jd_clients, start_link, []},
                 permanent,
                 5000,
                 supervisor,
                 [jd_clients]},
    Children  = [Clients],
    {ok, {RestartStrategy, Children}}.
