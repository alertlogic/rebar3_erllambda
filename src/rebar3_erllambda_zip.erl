%%%---------------------------------------------------------------------------
%% @doc rebar3_erllambda_zip - Package an erlang lambda function
%%
%% This module will call relx provider for packaging lambda function.
%%
%%
%% @copyright 2017 Alert Logic, Inc
%% Licensed under the MIT License. See LICENSE file in the project
%% root for full license information.
%%%---------------------------------------------------------------------------
-module(rebar3_erllambda_zip).

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
               {deps, ?DEPS},
               {example, "rebar3 erllambda zip"},
               {opts, relx:opt_spec_list()},
               {short_desc, "Generates a deployable AWS lambda zip file."}
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
    rebar_api:info( "generating erllambda zip package", [] ),
    State1 = rebar3_erllambda:add_property(
               State, relx, add_providers, rebar3_erllambda_rlx_zip_prv),
    %% Internal tooling expects artifacts in this specific location, to be fixed
    set_archive_dir(State1),
    rebar_relx:do(rebar3_erllambda_rlx_prv, "erllambda_zip", ?PROVIDER, State1).

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
set_archive_dir(State) ->
    BaseDir = rebar_dir:base_dir(State),
    put({rebar3_erllambda_rlx_zip_prv, archive_dir}, BaseDir).
