%%%---------------------------------------------------------------------------
%% @doc rebar3_erllambda - Rebar3 plugin for erllambda functions
%%
%% This module initializes the rebar3 plugin.
%%
%%
%% @copyright 2017 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(rebar3_erllambda).
-author('Paul Fisher <pfisher@alertlogic.com>').

-export([init/1]).

init( State ) ->
    {ok, ReleaseState} = rebar3_erllambda_release:init( State ),
    rebar3_erllambda_zip:init( ReleaseState ).
