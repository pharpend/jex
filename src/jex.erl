%%% @doc
%%% jex: simple JS package manager
%%% @end

-module(jex).
-vsn("0.2.0").
-export([start/1]).
-compile([export_all, nowarn_export_all]).

-include("$zx_include/zx_logger.hrl").



-record(jex,
        {vsn :: semver(),
         vd  :: jex_versioned_data()}).

-record(jex_vd_0_2_0,
        {}).

-type jex()                :: #jex{}.
-type jexs()               :: [jex()].
-type jex_vd_0_2_0()       :: #jex_vd_0_2_0{}.
-type jex_versioned_data() :: jex_vd_0_2_0().
-type semver()             :: {Major :: integer(),
                               Minor :: integer(),
                               Patch :: integer()}.



-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    ok = log(info, "ArgV: ~tp", [ArgV]),
    ok = dispatch(ArgV),
    %ok.
    zx:silent_stop().



-spec dispatch(ArgV) -> ok
    when ArgV :: [string()].

%% this is for decoding the state and barfing to stdout
dispatch(["barf"]) ->
    barf();
%% this is for decoding the state and barfing to stdout
dispatch(["--version"]) ->
    '--version'();
dispatch(["--semver"]) ->
    '--semver'();
dispatch(_) ->
    io:format("maid yourself~n", []).



-spec barf() -> ok.
%% @private
%% first thing we have to do is load our storage from a file
%% let's go with ~/.jex/jexs.eterms

barf() ->
    io:format("hi~n"),
    case file:read_file(jexs_filepath()) of
        {ok, Binary} ->
            barf2(Binary);
        %% file does not exist
        {error, enoent} ->
            tell(info, "~/.jex/jexs.eterms does not exist", []),
            ok;
        %% some other error
        Error ->
            tell(error, "there was an error reading ~/.jex/jexs.eterms: ~tp", [Error]),
            error(Error)
    end.

barf2(Binary) ->
    tell(info, "~tp", [erlang:binary_to_term(Binary)]).



-spec '--version'() -> ok.
%% @private
%% print the string version to the console

'--version'() ->
    tell(info, "~ts", [jex_vsn_str()]).




-spec '--semver'() -> ok.
%% @private
%% print the tuple version to the console

'--semver'() ->
    tell(info, "~tp", [jex_vsn_semver()]).



-spec jexs_filepath() -> JexsDotEtermsFilePath
    when JexsDotEtermsFilePath :: string().
%% @private
%% get the location of ~/.jex/jexs.eterms

jexs_filepath() ->
    filename:join([os:getenv("HOME"), ".jex", "jexs.eterms"]).



-spec jex_vsn_str() -> string().
%% @private
%% get the string version of the running version of jex

jex_vsn_str() ->
    proplists:get_value(vsn, ?MODULE:module_info(attributes)).



-spec jex_vsn_semver() -> semver().
%% @private

jex_vsn_semver() ->
    jvs(jex_vsn_str()).

jvs(VersionString) ->
    [Major, Minor, Patch] = lists:map(fun erlang:list_to_integer/1,
                                      string:lexemes(VersionString, ".")),
    {Major, Minor, Patch}.


