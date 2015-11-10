%%%    Copyright (C) 2013 Enrique Martin-Martin <emartinm@fdi.ucm.es>
%%%    This file is part of Erlang Declarative Debugger.
%%%
%%%    Erlang Declarative Debugger is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU General Public License as published by
%%%    the Free Software Foundation, either version 3 of the License, or
%%%    (at your option) any later version.
%%%
%%%    Erlang Declarative Debugger is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU General Public License for more details.
%%%
%%%    You should have received a copy of the GNU General Public License
%%%    along with Erlang Declarative Debugger.  If not, see <http://www.gnu.org/licenses/>.

%%%-----------------------------------------------------------------------------
%%% @author Salvador Tamarit <stamarit@dsic.upv.es>
%%% @copyright 2013 Salvador Tamarit
%%% @version 0.1
%%% @doc Erlang Declarative Debugger tracer
%%% @end
%%%-----------------------------------------------------------------------------

% TODO: We should instrument the arguments of the initial call (p.e. if it is a fun_expr)

-module(edd_trace_new).

-export([trace/2]).

trace(InitialCall, Timeout) -> 
    ModName = get_mod_name(InitialCall),
    instrument_and_reload(ModName),
    PidMain = self(),
    PidCall = execute_call(InitialCall, self()),
    PidTrace = 
    spawn(
        fun() ->
        receive_loop(0, [],[ModName], dict:new(), PidMain)
        end),
    register(edd_tracer, PidTrace),
    PidCall!start,
    receive 
        {result,Result} ->
            io:format("Execution result: ~p\n",[Result])
    after 
        Timeout ->
            io:format("Tracing timeout\n")
    end,
    PidTrace!stop,
    unregister(edd_tracer),
    Trace = 
        receive 
            {trace,Trace0} ->
                lists:reverse(Trace0)
        end,
    Loaded = 
        receive 
            {loaded,Loaded0} ->
                Loaded0
        end,
    [undo_instrument_and_reload(Mod) || Mod <- Loaded],
    DictFun = 
        receive 
            {fun_dict,FunDict0} ->
                FunDict0
        end,
    % io:format("~p\n",[dict:to_list(DictFun)]),

    % build_graph(Trace, DictFun, PidCall), 

    % io:format("~p\n",[dict:to_list(DictTraces)]),
    % io:format("Initial PID: ~p\n",[PidCall]),
    % Trace,
    % ok.
    {Trace, DictFun, PidCall}.

receive_loop(Current, Trace, Loaded, FunDict, PidMain) ->
    receive 
    TraceItem = {edd_trace, _, _, _} ->
        receive_loop(
            Current + 1, 
            [{Current,TraceItem} | Trace],
            Loaded, FunDict, PidMain);
    {edd_load_module, Module, PidAnswer} ->
        NLoaded = 
            case lists:member(Module, Loaded) of 
                true ->
                    PidAnswer!loaded,
                    Loaded;
                false ->
                    % io:format("Load module " ++ atom_to_list(Module) ++ "\n"),
                   instrument_and_reload(Module),
                   PidAnswer!loaded,
                   [Module | Loaded] 
            end, 
        receive_loop(Current, Trace, NLoaded, FunDict, PidMain);
    {edd_store_fun, Name, FunInfo} ->
        NFunDict = 
            case dict:is_key(Name, FunDict) of 
                true ->
                    FunDict;
                false ->
                    dict:append(Name, FunInfo, FunDict) 
            end, 
        receive_loop(Current, Trace, Loaded, NFunDict, PidMain);
    stop -> 
        PidMain!{trace, Trace},
        PidMain!{loaded, Loaded},
        PidMain!{fun_dict, FunDict};
    Other -> 
        io:format("Untracked msg ~p\n", [Other]),
        receive_loop(Current, Trace, Loaded, FunDict, PidMain)
    end.


execute_call(Call,PidParent) ->
    M1 = smerl:new(foo),
    {ok, M2} = 
    smerl:add_func(M1, "bar() -> try " ++ Call ++ " catch E1:E2 -> {E1,E2} end."),
    smerl:compile(M2,[nowarn_format]),
    spawn(
        fun() -> 
            receive 
                start -> ok 
            end,
            Res = foo:bar(), 
            PidParent!{result,Res} 
        end).

get_mod_name(InitialCall) ->
    AExpr = 
        case is_list(InitialCall) of 
            true ->
                hd(parse_expr(InitialCall++"."));
            false ->
                InitialCall
        end,
    {call,_,{remote,_,{atom,_,ModName},_},_} = AExpr,
    ModName.

instrument_and_reload(ModName) ->
    % io:format("~p\n", [ModName]),
    {ok,ModName,Binary} = 
        compile:file(atom_to_list(ModName) ++ ".erl", [{parse_transform,edd_con_pt}, binary]),
    reload_module(ModName, Binary).

undo_instrument_and_reload(ModName) ->
    {ok,ModName,Binary} = 
        compile:file(atom_to_list(ModName) ++ ".erl", [binary]),
    reload_module(ModName, Binary).

reload_module(ModName, Binary) ->
    try
        erlang:purge_module(ModName)
    catch 
        _:_ -> ok
    end,
    code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary).
    % code:load_abs(atom_to_list(ModName)).

parse_expr(Func) ->
    case erl_scan:string(Func) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, _Term} ->
                    _Term;
                _Err ->
                    {error, parse_error}
            end;
        _Err ->
            {error, parse_error}
    end.
