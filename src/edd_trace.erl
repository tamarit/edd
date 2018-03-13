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

-module(edd_trace).

-export([trace/4]).

trace(InitialCall, Timeout, PidAnswer, Dir) -> 
    ModName = get_mod_name(InitialCall),
    % io:format("~p\n~p\n", [ModName, Dir]),
    instrument_and_reload(ModName, Dir),
    PidMain = self(),
    PidCall = execute_call(InitialCall, self()),
    TimeoutServer = Timeout,
    PidTrace = 
    spawn(
        fun() ->
            receive_loop(0, [],[ModName], dict:new(), PidMain, TimeoutServer, Dir)
        end),
    register(edd_tracer, PidTrace),
    PidCall!start,
    receive 
        {result,Result} ->
            % io:format("TimeoutServer: ~p\n", [TimeoutServer]),
            receive 
                idle ->
                    % io:format("Recibe IDLE\n"),
                    ok
            end,
            io:format("\nExecution result: ~p\n",[Result])
    after 
        Timeout ->
            io:format("\nTracing timeout\n")
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
    [undo_instrument_and_reload(Mod, Dir) || Mod <- Loaded],
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
    PidAnswer!{Trace, DictFun, PidCall}.

receive_loop(Current, Trace, Loaded, FunDict, PidMain, Timeout, Dir) ->
    % io:format("Itera\n"),
    receive 
        TraceItem = {edd_trace, _, _, _} ->
            NTraceItem = 
                case TraceItem of 
                    {edd_trace, send_sent, Pid,  {PidReceive, Msg, PosAndPP}} when is_atom(PidReceive) -> 
                        {edd_trace, send_sent, Pid, {whereis(PidReceive), Msg, PosAndPP}};
                    _ -> 
                        TraceItem
                end,
            % io:format("Trace ~p\n", [TraceItem]),
            receive_loop(
                Current + 1, 
                [{Current,NTraceItem} | Trace],
                Loaded, FunDict, PidMain, Timeout, Dir);
        {edd_load_module, Module, PidAnswer} ->
            % io:format("Load module " ++ atom_to_list(Module) ++ "\n"),
            NLoaded = 
                case lists:member(Module, Loaded) of 
                    true ->
                        PidAnswer!loaded,
                        Loaded;
                    false ->
                       instrument_and_reload(Module, Dir),
                       PidAnswer!loaded,
                       [Module | Loaded] 
                end, 
            receive_loop(Current, Trace, NLoaded, FunDict, PidMain, Timeout, Dir);
        {edd_store_fun, Name, FunInfo} ->
            NFunDict = 
                case dict:is_key(Name, FunDict) of 
                    true ->
                        FunDict;
                    false ->
                        dict:append(Name, FunInfo, FunDict) 
                end, 
            receive_loop(Current, Trace, Loaded, NFunDict, PidMain, Timeout, Dir);
        stop -> 
            PidMain!{trace, Trace},
            PidMain!{loaded, Loaded},
            PidMain!{fun_dict, FunDict};
        Other -> 
            io:format("Untracked msg ~p\n", [Other]),
            receive_loop(Current, Trace, Loaded, FunDict, PidMain, Timeout, Dir)
    after 
        Timeout ->
            PidMain!idle,
            receive_loop(Current, Trace, Loaded, FunDict, PidMain, Timeout, Dir)
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

get_file_path(ModName, Dir) ->
    case Dir of 
        none -> 
            atom_to_list(ModName) ++ ".erl";
        _ ->
            Dir ++ "/" ++ atom_to_list(ModName) ++ ".erl"
    end.

instrument_and_reload(ModName, Dir) ->
    % try 
    CompileOpts = 
         [{parse_transform,edd_con_pt}, binary, {i,Dir}, {outdir,Dir}, return],
    % io:format("~p\n", [get_file_path(ModName, Dir)]),
    % io:format("~p\n", [CompileOpts]),
    {ok,ModName,Binary,_} = 
        case compile:file(get_file_path(ModName, Dir), CompileOpts) of 
            {ok,_,_,_} = Res ->
                Res
            %     ;
            % Other ->
            %     io:format("~p\n", [Other])
            % _ ->
            %     io:format("~p\n", [element(1, filename:find_src(ModName))]),
            %     Res = compile:file(element(1, filename:find_src(ModName)) ++ ".erl", CompileOpts),
            %     io:format("~p\n", [Res]),
            %     Res 
        end,

        % io:format("~p\n", [get_file_path(ModName, Dir)]),
        % io:format("~p\n", [filename:find_src(ModName)]),
        % io:format("~p\n", [ file:get_cwd()]),
        %  = 
        %     compile:file(get_file_path(ModName, Dir),),
    reload_module(ModName, Binary)
    % catch 
    %     _:_ -> ok 
    % end.
    ,ok.

undo_instrument_and_reload(ModName, Dir) ->
    {ok,ModName,Binary} = 
        compile:file(get_file_path(ModName, Dir), [binary, {i,Dir}, {outdir,Dir}]),
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
