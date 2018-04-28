-module({{name}}).
-behavior(erllambda).

-export([handle/2]).


%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( _Event, _Context ) ->
    erllambda:message( "Hello World!" ),
    ok.
