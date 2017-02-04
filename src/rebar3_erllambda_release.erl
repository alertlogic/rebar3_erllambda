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

-export([init/1, do/1, format_error/1]).


%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec init( State :: rebar_state:t() ) -> {ok, rebar_state:t()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the release provider
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
