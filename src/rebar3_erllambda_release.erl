%%%---------------------------------------------------------------------------
%% @doc rebar3_erllambda_release - Build a erlang lambda release
%%
%% This module will build an erllambda release on top of a standard relx
%% release so that the result can just be started.
%%
%%
%% @copyright 2017 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(rebar3_erllambda_release).
-author('Paul Fisher <pfisher@alertlogic.com>').

-behaviour(provider).
-export([init/1, do/1, format_error/1]).


%%============================================================================
%% Constant Definitions
%%============================================================================
-define(PROVIDER, release).
-define(NAMESPACE, erllambda).
-define(DEPS, [{default, compile}]).


%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec init( State :: rebar_state:t() ) -> {ok, rebar_state:t()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the release provider
%%
init( State ) ->
    Options = [
               {name, ?PROVIDER},
               {module, ?MODULE},
               {namespace, ?NAMESPACE},
               {bare, true},
               {deps, ?DEPS},
               {example, "rebar3 erllambda release"},
               {opts, relx:opt_spec_list()},
               {short_desc, "Rebar3 erllambda release provider"},
               {desc,
                "Performs erllamba specific release generation on top of the "
                "standard rebar3 release generation."}
              ],
    Provider = providers:create( Options ),
    {ok, rebar_state:add_provider(State, Provider)}.


%%%---------------------------------------------------------------------------
-spec do( State :: rebar_state:t() ) ->
                {ok, rebar_state:t()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the release provider
%%
do( State ) ->
    try
        rebar_api:info("running erllambda release generator", []),
        ErllambdaDir = rebar3_erllambda:erllambda_dir( State ),
        StartScript = start_script( ErllambdaDir ),
        {Command, _} = HandlerInfo = handler_info( State ),
        TargetDir = rebar3_erllambda:target_dir( State ),
        generate_start_script( TargetDir, Command, StartScript ),
        {ok, State}
    catch
        throw:Error ->
            {error, format_error(Error)}
    end.


%%%---------------------------------------------------------------------------
-spec format_error( Error :: any() ) -> iolist().
%%%---------------------------------------------------------------------------
%% @doc Format error for output
%%
format_error( Error ) ->
    rebar3_erllambda:format_error( Error ).


%%============================================================================
%% Internal Functions
%%============================================================================
generate_start_script( Dir, Command, Script ) ->
    rebar_api:info( "generating start script bin/~s", [Command] ),
    Filename = filename:join( [Dir, rebar3_erllambda:list(Command)] ),
    case file:write_file( Filename, Script ) of
        ok ->
            ok = generate_start_script( Dir, Filename ),
            %% create necessary symlink
            ok = file:make_symlink( "/var/task/" ++ rebar3_erllambda:list(Command), filename:join( [Dir, "bootstrap"]));
        {error, Reason} -> throw( {generate_start_script_failed, Reason} )
    end.            

generate_start_script( Dir, Filename ) ->
    Mode = 8#00755,
    case file:change_mode( Filename, Mode ) of
        ok -> ok;
        {error, Reason} -> throw( {generate_start_script_failed, Reason} )
    end.            


handler_info( State ) ->
    DefaultName = rebar3_erllambda:release_name( State ),
    Config = rebar_state:get(State, erllambda, []),
    Module = proplists:get_value( module, Config, DefaultName ),
    {["bin/", DefaultName], Module}.


start_script( ErllambdaDir ) ->
    ScriptFile = filename:join( [ErllambdaDir, "priv", "erlang-start"] ),
    case file:read_file( ScriptFile ) of
        {ok, Script} -> Script;
        {error, Reason} ->
            throw( {erllambda_script_missing, Reason} )
    end.
