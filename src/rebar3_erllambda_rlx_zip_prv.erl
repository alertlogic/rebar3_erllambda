%%%---------------------------------------------------------------------------
%% @doc rebar3_erllambda_rlx_zip_prv - Relx provider for zip packaging
%%
%% This module will package an erllambda function into a .zip file, suitable
%% for deployment to AWS lambda.
%%
%%
%% @copyright 2018 Alert Logic, Inc
%% Licensed under the MIT License. See LICENSE file in the project
%% root for full license information.
%%%---------------------------------------------------------------------------
-module(rebar3_erllambda_rlx_zip_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, erllambda_zip).
-define(DEPS, [resolve_release]).

%%============================================================================
%% API
%%============================================================================

-spec init(rlx_state:t()) -> {ok, rlx_state:t()}.
init(State) ->
    State1 = rlx_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {deps, ?DEPS}])),
    {ok, State1}.

-spec do(rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(State) ->
    {RelName, RelVsn} = rlx_state:default_configured_release(State),
    Release = rlx_state:get_realized_release(State, RelName, RelVsn),
    BaseDir = rlx_state:base_output_dir(State),
    OutputDir = rlx_state:output_dir(State),
    make_zip(Release, BaseDir, OutputDir),
    {ok, State}.

-spec format_error(ErrorDetail::term()) -> iolist().
format_error(ErrorDetail) ->
    rebar3_erllambda:format_error("~p", [ErrorDetail]).

%%============================================================================
%% Internal Functions
%%============================================================================

make_zip(Release, BaseDir, OutputDir) ->
    Name = atom_to_list(rlx_release:name(Release)),
    Vsn = rlx_release:vsn(Release),
    ArchiveFile = filename:join(BaseDir, Name ++ "-" ++ Vsn ++ ".zip"),
    Targets = [filename:join(OutputDir, T) || T <- archive_targets(Release)],
    make_zip(ArchiveFile, OutputDir, Targets).

%% Erlang zip library does not preserve execution flags, which is
%% important for bootstrap file
make_zip(ArchiveFile, SrcDir, Targets) ->
    Command = zip_cmd(ArchiveFile, SrcDir, Targets),
    case os_cmd(Command) of
        0 ->
            ok;
        Status ->
            throw({zip_generate_failed, Status})
    end.

zip_cmd(ArchiveFile, SrcDir, Targets) ->
    Script = zip_script(),
    lists:join(
      " ", [Script, ArchiveFile, SrcDir | Targets]).

zip_script() ->
    PrivDir = code:priv_dir(rebar3_erllambda),
    filename:join(PrivDir, "make_zip").

archive_targets(Release) ->
    ErtsVersion = rlx_release:erts(Release),
    ErtsDir = "erts-" ++ ErtsVersion,
    [ErtsDir, "bin", "lib", "releases", "bootstrap"].

os_cmd(Command) ->
    rebar_api:info("Executing: ~s", [Command]),
    Port = open_port({spawn, Command}, [exit_status, in, stderr_to_stdout]),
    os_cmd_receive(Port).

os_cmd_receive(Port) ->
    receive
	{Port, {data, _Output}} ->
            os_cmd_receive(Port);
	{Port, {exit_status, Status}} ->
            Status
    end.
