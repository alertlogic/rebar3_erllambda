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

-export([format_error/1,
         release_name/1, target_dir/1, erllambda_dir/1, zip_path/1,
         list/1, os_cmd/1]).

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


release_name( State ) ->
    case lists:keyfind( release, 1, rebar_state:get(State, relx, []) ) of
        false -> throw( {relx_release_undefined, undefined} );
        {release, {Name, _Vsn}, _} -> atom_to_list(Name)
    end.


target_dir( State ) ->
    ReleaseDir = filename:join( rebar_dir:base_dir(State), "rel" ),
    ReleaseName = release_name( State ),
    TargetDir = filename:join( [ReleaseDir, ReleaseName] ),
    case filelib:is_dir( TargetDir ) of
        true -> TargetDir;
        false -> throw( {target_director_does_not_exist, TargetDir, undefined} )
    end.


zip_path( State ) ->
    ProfileDir = rebar_dir:base_dir(State),
    Version = git_version(),
    ReleaseName = [release_name( State ), $-, Version, ".zip"],
    filename:join( [ProfileDir, ReleaseName] ).


git_version() ->
    string:strip( os:cmd( "git describe" ), right, $\n ).


erllambda_dir( State ) ->
    ChkDir = filename:join( ["_checkouts", "erllambda"] ),
    ProfDir = filename:join( [rebar_dir:base_dir(State), "lib", "erllambda"] ),
    case {filelib:is_dir( ChkDir ), filelib:is_dir( ProfDir )} of
        {true, _} -> filename:absname(ChkDir);
        {_, true} -> ProfDir;
        _Otherwise -> throw( erllambda_dep_missing )
    end.


list( V ) when is_atom(V) -> atom_to_list(V);
list( V ) -> V.


os_cmd( Command ) ->
    Port = open_port( {spawn, Command}, [exit_status, in, stderr_to_stdout] ),
    os_cmd_receive( Port ).

os_cmd_receive( Port ) ->
    receive
	{Port, {data, _Output}} -> os_cmd_receive( Port );
	{Port, {exit_status, Status}} -> Status
    end.
