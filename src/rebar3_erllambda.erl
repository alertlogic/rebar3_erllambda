-module(rebar3_erllambda).

-export([init/1, format_error/1, add_property/4]).

%%============================================================================
%% Callback Functions
%%============================================================================
init( State ) ->
    {ok, ReleaseState} = rebar3_erllambda_release:init( State ),
    rebar3_erllambda_zip:init( ReleaseState ).


%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec format_error( Error :: any() ) -> iolist().
%%%---------------------------------------------------------------------------
%% @doc Format error for output
%%
format_error( {Error, Info, Reason} ) ->
    io_lib:format( "~s: ~s ~s because ~p", [?MODULE, Error, Info, Reason] );
format_error( {Error, Reason} ) ->
    io_lib:format( "~s: ~s because ~p", [?MODULE, Error, Reason] );
format_error( Error ) ->
    io_lib:format( "~s: ~s", [?MODULE, Error] ).


%%%---------------------------------------------------------------------------
-spec add_property(
    State :: rebar_state:t(),
    ConfigKey :: atom(),
    Key :: atom(),
    Value :: term()) -> rebar_state:t().
%%%---------------------------------------------------------------------------
%% @doc add at the beginning of existing list of properties or add a new one
%%
add_property(State, ConfigKey, Key, Value) ->
    Config = rebar_state:get(State, ConfigKey, []),
    Properties = proplists:get_value(Key, Config, []),
    Config1 = property_store(Key, Value, Properties, Config),
    rebar_state:set(State, ConfigKey, Config1).

%%============================================================================
%% Internal functions
%%============================================================================
property_store(Key, Value, Properties, Config) when is_list(Value) ->
    lists:keystore(Key, 1, Config, {Key, Value ++ Properties});
property_store(Key, Value, Properties, Config)  ->
    lists:keystore(Key, 1, Config, {Key, [Value | Properties]}).
