%%% @doc
%%% Jex Daemon Client Service Supervisor
%%%
%%% This is the service-level supervisor of the system. It is the parent of both the
%%% client connection handlers and the client manager (which manages the client
%%% connection handlers). This is the child of jd_sup.
%%%
%%% See: http://erlang.org/doc/apps/kernel/application.html
%%% @end

-module(jd_clients).
-vsn("0.1.0").
-behavior(supervisor).
-author("Peter Harpending <peter.harpending@gmail.com>").
-copyright("Peter Harpending <peter.harpending@gmail.com>").
-license("MIT").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, none).

-spec init(none) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init(none) ->
    RestartStrategy = {rest_for_one, 1, 60},
    ClientMan = {jd_client_man,
                 {jd_client_man, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [jd_client_man]},
    ClientSup = {jd_client_sup,
                 {jd_client_sup, start_link, []},
                 permanent,
                 5000,
                 supervisor,
                 [jd_client_sup]},
    Children  = [ClientSup, ClientMan],
    {ok, {RestartStrategy, Children}}.
