-module({{name}}).
-behavior(erllambda).

-export([handle/2]).

%%---------------------------------------------------------------------------
-spec init(Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.
%%%---------------------------------------------------------------------------
%% @doc Init lambda callback
%%
init( _Context ) ->
    ok.

%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( _Event, _Context ) ->
    erllambda:message( "Hello World!" ),
    ok.
