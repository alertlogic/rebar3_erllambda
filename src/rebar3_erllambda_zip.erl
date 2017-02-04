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
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec init( State :: rebar_state:t() ) -> {ok, rebar_state:t()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the zip provider
%%
init( State ) ->
    rebar3_erllambda_release:init(
      rebar3_erllambda_zip:init( State ) ).


%%%---------------------------------------------------------------------------
-spec do( State :: rebar_state:t() ) ->
                {ok, rebar_state:t()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the release provider
%%
do( State ) ->
    {ok, State}.


%%%---------------------------------------------------------------------------
-spec format_error( Error :: any() ) -> iolist().
%%%---------------------------------------------------------------------------
%% @doc Format error for output
%%
format_error( Error ) ->
    io_lib:format( "~s: ~p", [?MODULE, Error] ).


%%============================================================================
%% Internal Functions
%%============================================================================
