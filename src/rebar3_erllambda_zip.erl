%%%---------------------------------------------------------------------------
%% @doc rebar3_erllambda_zip - Package an erlang lambda function
%%
%% This module will package an erllambda function into a .zip file, suitable
%% for deployment to AWS lambda.
%%
%%
%% @copyright 2017 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(rebar3_erllambda_zip).
-author('Paul Fisher <pfisher@alertlogic.com>').

-export([init/1, do/1, format_error/1]).


%%============================================================================
%% Constant Definitions
%%============================================================================
-define(PROVIDER, zip).
-define(NAMESPACE, erllambda).
-define(DEPS, [{?NAMESPACE, release}]).


%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec init( State :: rebar_state:t() ) -> {ok, rebar_state:t()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the zip provider
%%
init( State ) ->
    Options = [
               {name, ?PROVIDER},
               {module, ?MODULE},
               {namespace, ?NAMESPACE},
               {bare, true},
               {deps, []},
               {example, "rebar3 erllambda zip"},
               {opts, rebar3_erllambda:opt_spec_list()},
               {short_desc, "Rebar3 erllambda zip provider"},
               {desc, "Generates a deployable AWS lambda zip file."}
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
        rebar_api:info( "generating erllambda zip package", [] ),
        ErllambdaDir = rebar3_erllambda:erllambda_dir( State ),
        Zippath = rebar3_erllambda:zip_path( State ),
        TargetDir = rebar3_erllambda:target_dir( State ),
        Command = [ErllambdaDir, "/priv/lambda-zip ", Zippath, $ , TargetDir],
        rebar_api:info( "executing: ~s", [Command] ),
        case rebar3_erllambda:os_cmd( Command ) of
            0 -> {ok, State};
            Status -> throw( {zip_generate_failed, Status} )
        end
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

