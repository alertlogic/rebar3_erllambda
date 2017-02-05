-module({{name}}).
-behavior(erllambda).

-export([handle/2]).


%%%---------------------------------------------------------------------------
-spec handle( Event :: map(), Context :: map() ) -> ok | none().
%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( _Event, _Context ) ->
    erllambda:message( "Hello World!" ),
    ok.
