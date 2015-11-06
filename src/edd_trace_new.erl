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

-record(graph_pid_info, 
    {
        pid,
        call_stack,
        all_sends,
        all_spawns,
        first_call,
        final_value,
        current_receive,
        free,
        last_receive_conected,
        system_sends
    }).

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

    build_graph(Trace, DictFun, PidCall), 

    % io:format("~p\n",[dict:to_list(DictTraces)]),
    % io:format("Initial PID: ~p\n",[PidCall]),
    % Trace,
    ok.

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

separate_by_pid([],Dict) ->
    Dict;
separate_by_pid([Trace = {_, {edd_trace, _, Pid, _}} |T],Dict) ->
    NDict = get_new_dict(Pid,Dict,Trace),
    separate_by_pid(T,NDict).

get_new_dict(Pid,Dict,Trace) ->
    case dict:find(Pid,Dict) of
        {ok, PidTrace} ->
            dict:store(Pid, PidTrace ++ [Trace], Dict);
        error ->
            dict:store(Pid, [Trace], Dict) 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_graph(Trace, _, PidInit) ->

    DictTraces = 
        separate_by_pid(Trace, dict:new()),

    GraphServer = 
        spawn(fun() -> digraph_server() end),
    try 
        unregister(edd_graph)
    catch 
        _:_ -> ok 
    end,
    register(edd_graph, GraphServer),
    edd_graph!{add_vertex, 0},
    SystemSends = 
        lists:sort(
            lists:flatten(
                lists:map(
                    fun get_send/1, Trace))),
    % io:format("~p\n", [SystemSends]),

    {GraphInfo, _} = 
        lists:mapfoldl(
            fun(Pid, CFree) ->
                build_graph_pids(Pid, CFree, DictTraces, SystemSends)
            end,
            1,
            dict:fetch_keys(DictTraces)),
    % AllSends = 
    %     lists:flatten(
    %         lists:map(
    %             fun extract_send/1,
    %             GraphInfo0)),
    % GraphInfo = 
    %     lists:map(
    %         fun(GraphItem) ->
    %             complete_blocked_info(GraphItem, AllSends)
    %         end,
    %         GraphInfo0),
    Summary = 
        [
            begin
                CallStr = build_call_string(FirstCall),
                    % build_call_string(CallInfo),
                Spawned = 
                    lists:map(fun get_pid_from_spwan_info/1, Spawns),
                Sent = 
                    lists:map(fun get_pid_msg_from_send_info/1, Sends),
                {PidSummary,CallStr,Spawned,Sent}
            end 
         || {PidSummary, Sends, Spawns, FirstCall, _} <- GraphInfo ],
    {InitialCallStr, InitialCallResult} = 
        hd([{build_call_string(FirstCall), FinalValue} 
             || {PidSummary, _, _, FirstCall, FinalValue} <- GraphInfo,
                 PidSummary == PidInit]),
    %     hd([
    %         {
    %             build_call_string(CallInfo),
    %             get_result(
    %                 lists:last(Nodes), 
    %                 lists:last(lists:sort(dict:fetch(PidInit, DictTraces))), 
    %                 CallInfo)
    %         } 
    %      || 
    %         {PidSummary, Nodes, CallInfo, _, _} <- GraphInfo, 
    %         PidSummary ==  PidInit]),
    % io:format("~p\n", [GraphInfo]),
    edd_graph!
        {add_vertex, 
            0, 
            {{root, InitialCallStr, InitialCallResult,Summary},
             lists:flatten(io_lib:format("~p",[PidInit]))}},
    % lists:foldl(fun build_pid_nodes/2, 1, GraphInfo), 
    edd_graph!{get,self()},
    G = 
        receive 
            G0 -> G0
        end,
    edd_con_lib:dot_graph_file(G, "prova"),
    edd_graph!stop,
    unregister(edd_graph).
    % io:format("~p\n", [{digraph:vertices(G), digraph:edges(G)}]).

% build_pid_nodes({Pid, Nodes, _, _, _}, Free) ->
%     {NFree, _} = 
%         lists:foldl(
%             fun(Node, {CFree, CTransition}) -> 
%                 build_pid_node(Node, CFree, Pid, CTransition) 
%             end, 
%             {Free, {}}, Nodes),
%     NFree.

% build_pid_node({reached, { {pos_info,{_, File, Line, StrReceive}} }, CallStack}, Free, Pid, Transition) ->
%     {NFree, _} = 
%         lists:foldl(
%             fun(Node, CFreeLastNode) -> 
%                 build_pid_node_call_to_receive(Node, CFreeLastNode, {StrReceive, File, Line, Pid, Transition}) 
%             end, 
%             {Free, 0}, lists:reverse(CallStack)),
%     {NFree, Transition};
% % build_pid_node({blocked, { {pos_info,{_, File, Line, StrReceive}} }, _}, Free, Pid) ->
% build_pid_node({blocked, _, _}, Free, Pid, _) ->
%     {Free + 1, {}};
%     % {NFree, _} = 
%     %     lists:foldl(
%     %         fun(Node, CFreeLastNode) -> 
%     %             build_pid_node_call_to_value(Node, CFreeLastNode, {StrReceive, File, Line, Pid}) 
%     %         end, 
%     %         {Free, 0}, lists:reverse(CallStack)),
%     % NFree;
% build_pid_node({finished, _, _}, Free, _, _) ->
%     {Free + 1, {}};
% build_pid_node({process_finished, _}, Free, _, _) ->
%     {Free, {}}.


get_send({Order, {edd_trace,send_sent,Pid,Info}} ) ->
    [{Order, Pid, Info}];
get_send(_) ->
    [].   

build_pid_node_call_to_receive(Blocked, {InfoCall, Sends, Spawns, FinishedCalls}, {Free, LastNode},  {StrReceive, File, Line, Pid, CurrentReceive}) ->
    Spawned = 
        lists:map(fun get_pid_from_spwan_info/1, Spawns),
    Sent = 
        lists:map(fun get_pid_msg_from_send_info/1, Sends),
    Transition =
        get_transition(CurrentReceive),
    Content = 
        case Blocked of 
            true -> 
                {
                   {
                        to_value, 
                        {call, {build_call_string(InfoCall), stuck_receive},
                         none, none, Transition},
                        0, Sent, Spawned
                    },
                    lists:flatten(io_lib:format("~p",[Pid]))
                };
            false -> 
                {
                    {
                        to_receive, StrReceive, 
                        {'call', build_call_string(InfoCall), none, none}, 
                         File, Line, Sent, Spawned, Transition
                     }, 
                     lists:flatten(io_lib:format("~p",[Pid]))
                }
        end,
    edd_graph!{add_vertex, Free, Content},
    edd_graph!{add_edge, LastNode, Free},
    [edd_graph!{add_edge, Free, NodeCall} || NodeCall <- FinishedCalls],
    % io:format("~p\n",[Free]),
    {Free + 1, Free}.

% extract_send({Pid, _, _, Sends, _}) ->
%     lists:map(fun get_pid_msg_from_send_info/1, Sends).

% build_question({{to_value,Expr, Clause, Sent, Spawned},Pid}) ->
%     case Expr of 
%             {'receive',{AExpr,Value,Line,File}, MsgSender,Context,Bindings,DiscardedMessages} ->
%                 question_to_receive_value({'receive',{AExpr,Value,Line,File},Clause,MsgSender,Context,none,none,none,DiscardedMessages},Value,Pid,Sent,Spawned,{},Bindings);
%             {call,{ACall,Value},File,Line,Transition} ->
%                 question_to_receive_value({'call',ACall,File,Line},Value,Pid,Sent,Spawned,Transition,[])


get_pid_from_spwan_info({_,_,_, NewPid,_}) ->
    NewPid;
get_pid_from_spwan_info({_,NewPid,_}) ->
    NewPid.

get_pid_msg_from_send_info({Pid, Msg, _}) ->
    {Pid, lists:flatten(io_lib:format("~p", [Msg]))};
get_pid_msg_from_send_info({Pid, Msg}) ->
    {Pid, lists:flatten(io_lib:format("~p", [Msg]))}.

build_call_string({ModFun,IdFun,ArgsFun}) ->
    case IdFun of 
        {pos_info,{_,File,Line,StrFun}} ->
            StrFun ++ 
            lists:flatten(
                io_lib:format(
                    "(~s)\nfun location: (~s, line ~p)",
                    [build_args_list_string(ArgsFun),File, Line]));
        _ ->
            lists:flatten(
                io_lib:format(
                    "~p:~p(~s)", 
                    [ModFun,IdFun,build_args_list_string(ArgsFun)]))
    end.

build_args_list_string([]) ->
    "";
build_args_list_string([E]) ->
    lists:flatten(io_lib:format("~p", [E]));
build_args_list_string([E | Rest]) ->
    lists:flatten(io_lib:format("~p, ~s", [E, build_args_list_string(Rest)])).

% get_result({blocked, _, _}, _, _) ->
%     stuck_receive;
% get_result({finished, _, _}, {_, {edd_trace,Tag,_,Info}}, FirstCallInfo) ->
%     case {Tag, element(1, Info) == FirstCallInfo} of
%         {end_call, true} -> 
%             lists:flatten(io_lib:format("~p", [element(2, Info)]));
%         _ ->
%             not_computed
%     end.


digraph_server() ->
    digraph_server(digraph:new([acyclic])).

digraph_server(G) ->
    receive
        {add_vertex,V} -> 
            digraph:add_vertex(G,V),
            digraph_server(G);
        {add_vertex,V,L} -> 
            digraph:add_vertex(G,V,L),
            digraph_server(G);
        {add_edge,V1,V2} ->
            digraph:add_edge(G,V1,V2),
            digraph_server(G);  
        {get,Pid} ->
            Pid!G,
            digraph_server(G);  
        stop ->
            ok
    end. 

build_graph_pids(Pid, Free, Dict, SystemSends) ->
    TracePid = 
        lists:sort(dict:fetch(Pid, Dict)),
    % {_, {edd_trace,start_call,_,InfoFirstCall}} = 
    %     hd(TracePid),
    % % [].
    % {Pid, lists:reverse(build_info_pid(TracePid, [{Info, [], []}], []))}.
    {Sends, Spawns, NFree, FirstCall, FinalValue} = 
        build_graph_pid(TracePid, 
            #graph_pid_info{
                pid = Pid,
                call_stack = [],
                all_sends = [],
                all_spawns = [],
                first_call = none,
                final_value = none,
                current_receive = {},
                free = Free,
                last_receive_conected = false,
                system_sends = SystemSends
            }),
    % NNodes = lists:reverse(complete_nodes(Nodes, FinishedCall)),
    {{Pid, Sends, Spawns, FirstCall, FinalValue}, NFree}.

% complete_nodes([{reached, Info, CallStack} | Rest], FinishedCall)->
%     [{blocked, Info, CallStack}, {reached, Info, CallStack} | complete_nodes(Rest, [])];
% complete_nodes([F = {finished, _, _}, R = {reached, _, _} | Rest], FinishedCall)->
%     [{process_finished, FinishedCall}, F, R | complete_nodes(Rest, [])];
% complete_nodes([], FinishedCall)->
%     [].

% complete_blocked_info({Pid, Nodes, InfoFirstCall, Sends, Spawns}, AllSends) ->
%     Consumed = 
%         lists:map( 
%             fun(Msg) -> 
%                 lists:flatten(io_lib:format("~p", [Msg]) ) 
%             end,
%             lists:foldl(fun get_consumed_message/2, [], Nodes)),
%     NotConsumed = 
%         get_no_consumed(AllSends, Pid, Consumed, []),
%     io:format("~p\n", [{Pid, NotConsumed}]),
%     NLast = 
%         case lists:last(Nodes) of 
%             {blocked, Info, CallStack} -> 
%                 {blocked, {NotConsumed, Info}, CallStack};
%             Other ->
%                 Other
%         end,
%     NNodes = lists:droplast(Nodes) ++ [NLast],
%     {Pid, NNodes, InfoFirstCall, Sends, Spawns}.


% get_no_consumed([{PidSend, MsgSend} | Rest], Pid, Consumed, Acc) ->
%     % io:format("~p\n", [{PidSend, MsgSend, Consumed}]),
%     % io:format("~p\n", [{PidSend == Pid, lists:member(MsgSend, Consumed)}]),
%     % io:format("~p\n", [Consumed -- MsgSend]),
%     case {PidSend == Pid, lists:member(MsgSend, Consumed)} of 
%             {true, true} ->
%                 get_no_consumed(Rest, Pid, Consumed -- [MsgSend], Acc);
%             {true, false} ->
%                 get_no_consumed(Rest, Pid, Consumed, [{PidSend, MsgSend} | Acc]);
%             {false, false} ->
%                 get_no_consumed(Rest, Pid, Consumed, Acc)
%     end;
% get_no_consumed([], _, [], Acc) ->
%     Acc.


% get_consumed_message({finished, {Msg, _, _}, _}, Acc) ->
%     [Msg | Acc];
% get_consumed_message(_, Acc) ->
%     Acc.

build_graph_pid([{_, {edd_trace,Tag,_,Info}} | Rest], 
        GraphPidInfo = 
            #graph_pid_info{
                pid = Pid,
                call_stack = CallStack,
                all_sends = AllSends,
                all_spawns = AllSpawns,
                first_call = FirstCall,
                final_value = FinalValue,
                current_receive = CurrentReceive,
                free = Free,
                last_receive_conected = LastReceiveConected,
                system_sends = SystemSends
            }) ->
    % io:format("Tag ~p\n", [Tag]),
    % io:format("CallStack ~p\n", [CallStack]),
    % TO_VALUE:
    % {{to_value,Expr, Clause, Sent, Spawned},Pid}
    % {'receive',{AExpr,Value,Line,File}, MsgSender,Context,Bindings,DiscardedMessages}
    % {call,{ACall,Value},File,Line,Transition}
    % {NCallStack, NAllSends, NAllSpawns, NCurrentReceive, NFree, NLastReceiveConected, NSystemSends} = 
    NGraphPidInfo = 
        case Tag of 
            start_call ->
                % io:format("Start ~p\n", [Info]), 
                NFirstCall = 
                    case FirstCall of 
                        none -> 
                            Info;
                        _ ->
                            FirstCall
                    end,
                GraphPidInfo#graph_pid_info{
                    call_stack = [{Info, [], [], []} | CallStack],
                    first_call = NFirstCall
                };
            end_call ->
                % io:format("End ~p\n", [CallStack]), 
                {_, Result} = Info,
                case CallStack of 
                    [{InfoCall ,SentCall, SpawnedCall, FinishedCalls}| _] -> 
                        edd_graph!
                            {add_vertex, 
                                Free, 
                                {
                                    {to_value,
                                        {call, {build_call_string(InfoCall), lists:flatten(io_lib:format("~p", [Result]))},
                                         none, none, get_transition(CurrentReceive)}, 0,
                                        lists:map(fun get_pid_msg_from_send_info/1, SentCall), 
                                        lists:map(fun get_pid_from_spwan_info/1, SpawnedCall)},
                                        lists:flatten(io_lib:format("~p",[Pid]))}},
                        [edd_graph!{add_edge, Free, NodeCall} || NodeCall <- FinishedCalls];
                    [] -> 
                        []
                end,
                NLastReceiveConected0 = 
                    case {get_transition(CurrentReceive), no_more_receives(Rest), LastReceiveConected} of 
                        {{_, _, _, NodeReceive, _}, true, false} ->
                            edd_graph!{add_edge,Free , NodeReceive},
                            true;
                        {_,_,_} ->
                            LastReceiveConected
                    end,
                NCallStack0 = 
                    case tl(CallStack) of 
                        [{PrevInfoCall ,PrevSentCall, PrevSpawnedCall, PrevFinished} | T] ->
                            [{PrevInfoCall ,PrevSentCall, PrevSpawnedCall, [Free|PrevFinished]} | T];
                        [] -> 
                            edd_graph!{add_edge, 0, Free},
                            []
                    end,
                NFinalValue = 
                    case Rest of 
                        [] -> 
                           Result;
                        _ -> 
                           FinalValue
                    end, 
                GraphPidInfo#graph_pid_info{
                    call_stack = NCallStack0,
                    free = Free + 1,
                    last_receive_conected = NLastReceiveConected0,
                    final_value = NFinalValue
                };
            made_spawn ->
                {CInfo, CSents, CSpawns, CFinished} = hd(CallStack),
                GraphPidInfo#graph_pid_info{
                    call_stack = [{CInfo, CSents, [Info | CSpawns], CFinished} | tl(CallStack)],
                    all_spawns = [Info | AllSpawns]
                };
            send_sent ->
                {CInfo, CSents, CSpawns, CFinished} = hd(CallStack),
                GraphPidInfo#graph_pid_info{
                    call_stack = [{CInfo, [Info | CSents], CSpawns, CFinished} | tl(CallStack)],
                    all_sends = [Info | AllSends]
                };
            receive_reached ->
                {{pos_info,{_, File, Line, StrReceive}}} = Info,
                {NFree0, _} = 
                    lists:foldl(
                        fun(Node, CFreeLastNode) -> 
                            build_pid_node_call_to_receive(false, Node, CFreeLastNode, {StrReceive, File, Line, Pid, CurrentReceive}) 
                        end, 
                    {Free, 0}, lists:reverse(CallStack)),
                case get_transition(CurrentReceive) of 
                    {} ->
                        ok;
                    {_, _, _, NodeReceive, _} ->
                        edd_graph!{add_edge,NFree0 -1 , NodeReceive}
                end,
                NCurrentReceive0 = 
                    {reached, {{StrReceive,Line,File}, none, {unknown,"",0}, none, stuck_receive}},
                NCallStack0 = 
                    [{CInfo, [], [], []} || {CInfo, _, _, _}  <- CallStack],
                {NFree1, NFinalValue} = 
                    case Rest of
                        % Is blocked
                        [] -> 
                            {NFree1_, _} = 
                                lists:foldl(
                                    fun(Node, CFreeLastNode) -> 
                                        build_pid_node_call_to_receive(true, Node, CFreeLastNode, {StrReceive, File, Line, Pid, NCurrentReceive0}) 
                                    end, 
                                {NFree0, 0}, lists:reverse(NCallStack0)),
                            Discarded = 
                                [{PidSender, Msg} || {_,PidSender, {PidMsg,Msg,_}}  <- SystemSends, PidMsg == Pid],
                            edd_graph!
                                {add_vertex, 
                                    NFree1_, 
                                    {
                                        {to_value,
                                            {'receive',{StrReceive, stuck_receive, Line, File},{unknown,"",0} ,[], [], Discarded}, 
                                            none,
                                            [], 
                                            []},
                                            lists:flatten(io_lib:format("~p",[Pid]))}},
                                edd_graph!{add_edge,NFree1_ - 1 , NFree1_},
                            {NFree1_ + 1, stuck_receive};
                        _ ->
                            {NFree0, FinalValue}
                    end, 
                GraphPidInfo#graph_pid_info{
                    call_stack = NCallStack0,
                    current_receive = NCurrentReceive0,
                    free =  NFree1,
                    final_value = NFinalValue
                };
            receive_evaluated ->
                {Msg, Context, Bindings,Clause} = Info,
                {Sender, Discarded, NSystemSends0} = 
                    get_sender_and_discarded(Pid, Msg, SystemSends),
                % io:format("~p\n", [Bindings]),
                {reached, {ReceiveInfo = {StrReceive,Line,File}, _, _, _, _}} = 
                    CurrentReceive,
                edd_graph!
                    {add_vertex, 
                        Free, 
                        {
                            {to_value,
                                {'receive',{StrReceive, none, Line, File},{Sender,lists:flatten(io_lib:format("~p",[Msg])),0} ,Context, Bindings, Discarded},
                                Clause,
                                [], 
                                []},
                                lists:flatten(io_lib:format("~p",[Pid]))}},
                NCurrentReceive0 = 
                    {evaluated, {ReceiveInfo, Clause, {Sender, lists:flatten(io_lib:format("~p",[Msg])) , 0}, Free, none}, Context, Bindings},
                GraphPidInfo#graph_pid_info{
                    current_receive = NCurrentReceive0,
                    free =  Free + 1,
                    system_sends = NSystemSends0
                };
            receive_finished ->
                NCurrentReceive0 = 
                    case CurrentReceive of 
                        {evaluated, {ReceiveInfo = {StrReceive,Line,File}, Clause, SenderMsg, NodeReceive, none}, Context, Bindings} ->   
                            {_, CSents, CSpawns, CFinished} = hd(CallStack),
                            {Value} = Info,
                            ValueStr = 
                                lists:flatten(io_lib:format("~p", [Value])),
                            edd_graph!
                                {add_vertex, 
                                    NodeReceive, 
                                    {
                                        {to_value,
                                            {'receive',{StrReceive, ValueStr, Line, File}, SenderMsg ,Context, Bindings, []},
                                            Clause,
                                            lists:map(fun get_pid_msg_from_send_info/1, CSents), 
                                            lists:map(fun get_pid_from_spwan_info/1, CSpawns)},
                                            lists:flatten(io_lib:format("~p",[Pid]))}},
                            [edd_graph!{add_edge, NodeReceive, NodeCall} || NodeCall <- CFinished],
                            {evaluated, {ReceiveInfo, Clause, SenderMsg, NodeReceive, ValueStr}, Context, Bindings};
                        _ ->
                            CurrentReceive
                    end,
                GraphPidInfo#graph_pid_info{
                    current_receive = NCurrentReceive0
                }
        end,
    build_graph_pid(Rest, NGraphPidInfo);
build_graph_pid([],
        #graph_pid_info{
                all_sends = AllSends,
                all_spawns = AllSpawns,
                first_call = FirstCall,
                final_value = FinalValue,
                free = Free
        }) ->
    {AllSends, AllSpawns, Free, FirstCall, FinalValue}.

get_transition(CurrentReceive) ->
    % io:format("~p\n", [CurrentReceive]),
    case CurrentReceive of 
        {} -> 
            {};
        {reached, Transition0} -> 
            Transition0;
        {evaluated, Transition0, _, _} ->
            Transition0
    end.

no_more_receives([{_, {edd_trace,Tag,_,_}} | Rest]) -> 
    case Tag of 
        receive_reached -> 
            false;
        receive_evaluated ->
            false;
        receive_finished -> 
            false;
        _ -> 
            no_more_receives(Rest)
    end;
no_more_receives([]) -> 
    true.


get_sender_and_discarded(Pid, Msg, [{_,PidSender, {Pid,Msg,_}} | Tail]) ->
    {PidSender, [], Tail};
get_sender_and_discarded(Pid, Msg, [{_,PidSender, {Pid,OtherMsg,_}} | Tail]) ->
    {PidSenderMsg, DiscardedTail, Rest} = get_sender_and_discarded(Pid, Msg, Tail),
    {PidSenderMsg, [{PidSender, OtherMsg} | DiscardedTail], Rest};
get_sender_and_discarded(Pid, Msg, [_ | Tail]) ->
    get_sender_and_discarded(Pid, Msg, Tail);
get_sender_and_discarded(_, _, []) ->
    {unknown, [], []}.


% G!{add_vertex,Node,{{to_receive,AExpr,NCall,File,Line,
%      %[{Pid,Msg} || {Pid,Msg,Id} <- Sent, Id == IdCall], [Pid || {Pid,Id} <- Spawned, Id == IdCall],
%      [{Pid,Msg} || {Pid,Msg,Id} <- Sent, lists:member(IdCall,Id)], [Pid || {Pid,Id} <- Spawned, lists:member(IdCall,Id)],
%      Transition},lists:flatten(io_lib:format("~p",[PidTrace]))}}


% G!{add_vertex,FreeV,{{to_value,{call,AMatch,FileCall,LineCall,TransitionCall}, SelectedClause, SentCall, SpawnedCall},lists:flatten(io_lib:format("~p",[PidTrace]))}},

% G!{add_vertex,NodeReceive,{{to_value,{'receive',FinishedRecieve, SenderMsg,Context,Bindings,DiscardedMessages}, Clause, SentCall, SpawnedCall},lists:flatten(io_lib:format("~p",[PidTrace]))}},

% PidG!{add_vertex,0,{{root,Expr,PrintValue,PrettySummary},lists:flatten(io_lib:format("~p",[FirstPid]))}}

% build_question({{to_receive,AExpr,ACall,File,Line,Sent,Spawned,Transition_},Pid}) ->
%     question_to_receive_value(ACall,{'receive',AExpr,File,Line},Pid,Sent,Spawned,Transition_,[]);
%     % io_lib:format("Pid ~p",[Pid]) ++
%     % case ACall of 
%     %   {'receive',{AReceive,LineR,FileR},Clause,Consumed} ->
%     %       "evaluates " ++ build_receive(AReceive,FileR,LineR,Clause,Consumed);
%     %   _ ->
%     %       " calls " ++ erl_prettypr:format(ACall)
%     % end ++
%     % " and then reaches the receive expression:\n" ++
%     % erl_prettypr:format(AExpr,[{paper, 300},{ribbon, 300}]) ++
%     % io_lib:format("\nlocation (~s, line ~p)\n",[File,Line]) ++ 
%     % build_transition(Transition) ++ 
%     % print_sent_spawned(Sent, Spawned);
% build_question({{to_value,Expr, Clause, Sent, Spawned},Pid}) ->
%     case Expr of 
%             {'receive',{AExpr,Value,Line,File}, MsgSender,Context,Bindings,DiscardedMessages} ->
%                 question_to_receive_value({'receive',{AExpr,Value,Line,File},Clause,MsgSender,Context,none,none,none,DiscardedMessages},Value,Pid,Sent,Spawned,{},Bindings);
%             {call,{ACall,Value},File,Line,Transition} ->
%                 question_to_receive_value({'call',ACall,File,Line},Value,Pid,Sent,Spawned,Transition,[])

% build_transition({}) ->
%     "";
% build_transition({{AReceive,Line,File},Clause,{Sender,Msg,_},_NodeReceive,Value}) ->
%     build_receive(AReceive,File,Line,Clause,{Sender,Msg}) ++ 
%     case Value of 
%         stuck_receive ->
%             "\nthat is blocked.";
%         none ->
%             "";
%         _ ->
%             "\nthat evalautes to " ++ erl_prettypr:format(Value) ++ "."
%     end.
