-module(rebar3_erllambda_release).

-behaviour(provider).
-export([init/1, do/1, format_error/1]).


%%============================================================================
%% Constant Definitions
%%============================================================================
-define(PROVIDER, release).
-define(NAMESPACE, erllambda).
-define(DEPS, [{default, compile}]).


%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec init( State :: rebar_state:t() ) -> {ok, rebar_state:t()}.
%%%---------------------------------------------------------------------------
%% @doc Initialize the release provider
%%
init( State ) ->
    Options = [
               {name, ?PROVIDER},
               {module, ?MODULE},
               {namespace, ?NAMESPACE},
               {bare, true},
               {deps, ?DEPS},
               {example, "rebar3 erllambda release"},
               {opts, relx:opt_spec_list()},
               {short_desc, "Build lambda release of project"},
               {desc,
                "Performs erllamba specific release generation on top of the "
                "standard rebar3 release generation."}
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
    rebar_api:info("running erllambda release generator", []),
    Overlay = overlay(State),
    State1 = rebar3_erllambda:add_property(State, relx, overlay, Overlay),
    rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State1).


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
overlay(State) ->
    Config = rebar_state:get(State, rebar3_erllambda, []),
    boot_overlay() ++ custom_hooks(Config).

custom_hooks(Config) ->
    Hooks = proplists:get_value(hooks, Config, []),
    PreStartHooks = proplists:get_value(pre_start, Hooks, []),
    HookDir = "bin/pre-start-hooks",
    HooksOverlay = hooks_overlay(PreStartHooks, HookDir),
    case HooksOverlay of
        [] ->
            [];
        _ ->
            [{mkdir, HookDir} | HooksOverlay]
    end.

hooks_overlay(Hooks, Directory) ->
    [{template, Hook, filename:join(Directory, filename:basename(Hook))}
     || Hook <- Hooks].

boot_overlay() ->
    PrivDir = code:priv_dir(rebar3_erllambda),
    %% relx can't symlink
    [{template, filename:join(PrivDir, "bootstrap"), "bootstrap"},
     {template, filename:join(PrivDir, "bin"), "bin/{{ release_name }}"}].
