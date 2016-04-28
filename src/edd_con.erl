%%%    Copyright (C) 2013 Salvador Tamarit <stamarit@dsic.upv.es>
%%%
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
%%% @doc The Erlang Declarative Debugger module for concurrency. 
%%% @end
%%%-----------------------------------------------------------------------------

-module(edd_con).
-export([ddc/2, ddc/4, ddc_server/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% State to build the evaluation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

ddc(Expr,Timeout) ->
	ddc(Expr,Timeout,divide_query,indet).

ddc_server(Expr, Dir, Timeout) ->
    code:add_patha(Dir), 
    % io:format("PATHS: ~p\n",[code:get_path()]),
    {_, {Pid, Comm, G}} = 
        ddc_internal_core(
            Expr, 
            Timeout, 
            fun(X) -> edd_lib:core_module(atom_to_list(X) ++ ".erl", Dir) end,
            Dir),
    {Pid, Comm, tupled_graph(G)}.



ddc(Expr,Timeout,Strategy,Priority) ->
    Graph = true,
    {{Trace, DictFun, PidCall},_} = 
        ddc_internal_core(
            Expr, 
            Timeout, 
            fun(X) -> edd_lib:core_module(atom_to_list(X)++".erl") end,
            none),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	%%%% REMOVE COMMENTS TO ENABLE DEBUGGING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	edd_graph!new,
	build_graph_old(Trace, DictFun, PidCall), 
	edd_graph!{get,self()},
	receive 
		G -> ok
	end,
	%io:format("G: ~p\n",[G]),
	case Graph of
	     true ->
	       edd_con_lib_new:dot_graph_file(G,"dbg");
	     false -> 
	       ok
	end,
 	edd_con_lib_new:ask(G,Strategy,Priority),

 	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	%%%% REMOVE COMMENTS TO ENABLE DEBUGGING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % PidG!stop,
    edd_graph!stop,
    unregister(edd_graph),
    ok.

ddc_internal_core(Expr, Timeout, FunCore, Dir) ->
    {ok,[AExpr|_]} = edd_lib:parse_expr(Expr++"."),
    M1 = smerl:new(foo),
    {ok, M2} = smerl:add_func(M1,"bar() ->" ++ Expr ++ " ."),
    %Obtiene el CORE de la expresión Expr
    {ok,_,CoreP} = smerl:compile2(M2,[to_core,binary,no_copt]), 
    InitialCall = extract_call(CoreP),
    %io:format("InitialCall: ~p\n",[InitialCall]),
    FunOperator = erl_syntax:application_operator(AExpr),
    {remote,_,{atom,_,ModName},_} = FunOperator,
    Core = FunCore(ModName),
    %io:format("Core: ~p\n",[Core]),
    
    %to get the .core file. ONLY FOR DEBUGGING 
    %compile:file(atom_to_list(ModName)++".erl",[to_core,no_copt]),
    
    FunsInitialCall = get_funs_from_abstract(AExpr,-1),
    compile:file(atom_to_list(ModName)++".erl"),
    % {{first_pid,FirstPid},{traces,Traces0}} = 
    Self = self(),
    spawn(fun() -> edd_trace:trace(Expr,Timeout, Self, Dir) end),
    receive 
        {Trace, DictFun, PidCall} ->
            ok
    end,
    % Traces = dict_keys_to_str(Traces0),

    PidG = spawn(fun() -> digraph_server() end),
    try 
        unregister(edd_graph)
    catch 
        _:_ -> ok 
    end,
    register(edd_graph, PidG),
    PidInfo_Comms_GQA = build_graph(Trace, DictFun, PidCall),
    {{Trace, DictFun, PidCall}, PidInfo_Comms_GQA}.

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
			% io:format("G SERVER: ~p \n", [G]),
			Pid!G,
			digraph_server(G);
		del_disconected_vertices ->
			[case digraph:in_degree(G, V) +  digraph:out_degree(G, V) of 
				0 -> 
					digraph:del_vertex(G, V);
				_ ->
					ok
			 end
			 || V <- digraph:vertices(G)],
			digraph_server(G);
		new -> 
			digraph_server(digraph:new());
		stop ->
			ok
	end. 

% Core es el CORE del módulo foo que tiene bar() -> Expr
% extract_call devuelve únicamente el CORE de Expr (o quizá del case que lo contiene)
extract_call(Core) ->
	{c_module,_,_,_,_,FunDefs} = Core,
	[{c_fun,_,_,FunBody}|_] = 
		[FunBody_ || {{c_var,_,FunName},FunBody_} <- FunDefs, FunName == {bar,0}],
	[{c_clause,_,_,_,Call}|_] = cerl:case_clauses(FunBody),
	Call.	

get_funs_from_abstract(Abstract,Line) -> 
	erl_syntax_lib:fold(fun(Tree,Acc) -> 
	               		case Tree of 
	               	    	     {'fun',Line,{clauses,_}} ->
	               	    		[Tree|Acc];
	               	             {'fun',_,{clauses,_}} when Line =:= -1 ->
	               	    		[Tree|Acc];
	               	    	     _ -> 
	               	    		Acc
	               		end
		    	     end, [], Abstract).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Old graph building functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_graph_old(Trace, _, PidInit) ->

    DictTraces = 
        separate_by_pid(Trace, dict:new()),
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
    % io:format("~p\n", [{digraph:vertices(G), digraph:edges(G)}]).

    edd_graph!del_disconected_vertices,
    ok.

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
    end;
% TODO: This clause is temporal. Should be removed
build_call_string(_) ->
    "".

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


% digraph_server() ->
%     digraph_server(digraph:new([acyclic])).

% digraph_server(G) ->
%     receive
%         {add_vertex,V} -> 
%             digraph:add_vertex(G,V),
%             digraph_server(G);
%         {add_vertex,V,L} -> 
%             digraph:add_vertex(G,V,L),
%             digraph_server(G);
%         {add_edge,V1,V2} ->
%             digraph:add_edge(G,V1,V2),
%             digraph_server(G);  
%         {get,Pid} ->
%             Pid!G,
%             digraph_server(G);  
%         stop ->
%             ok
%     end. 

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


% get_no_consumed([{PidSend, MsgSend} | Rest], Pid, Consumed, Acc) ->
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
                {_Call, Result, _Context} = Info,
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
                    call_stack = [{CInfo, CSents, [Info | CSpawns], CFinished} | tl(CallStack)],
                    all_spawns = [Info | AllSpawns]
                };
            send_sent ->
                {CInfo, CSents, CSpawns, CFinished} = hd(CallStack),
                GraphPidInfo#graph_pid_info{
                    call_stack = [{CInfo, [Info | CSents], CSpawns, CFinished} | tl(CallStack)],
                    all_sends = [Info | AllSends]
                };
            receive_reached ->
                {_Context, {{pos_info,{_, File, Line, StrReceive}}}} = Info,
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
                            {Value, _ContextEnd} = Info,
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Records for evaluation tree node's info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(evaltree_state,
	{
		pids_info = [],
		communication = [], % History of send, receive and spawn events
		free = 0
	}).


-record(pid_info, 
    {
    	pid = none,
    	first_call = none,
    	spawned = [],
    	sent = [],
    	result = none,
    	% received = [],
    	% consumed = [],
    	is_first = false,
    	callrec_stack = [],
    	snapshots = []
    }).

-record(message_info,
	{
		from = none,
		to = none,
		msg = none,
        trace = none
	}).

-record(spawn_info,
	{
		spawner = none,
		spawned = none,
        trace = none
	}).

-record(call_info,
	{
		call = none,
		pos_pp = none
	}).

-record(receive_info,
	{
		pos_pp = none,
		context = [],
		bindings = [],
		clause = none
	}).

-record(callrec_stack_item,
	{
		origin_callrec = none,
		reached_rec_val = none,
		context = [],
		spawned = [],
    	sent =[],
    	received = [],
    	consumed = [],
    	% children_nodes = [],
    	result = none,
        trace = none
	}).	

-record(snapshot_info,
	{
		top = none,
		rest = none
	}).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Newest graph building functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_graph(Trace, DictFuns, PidInit) ->
    DictTraces = 
        separate_by_pid(Trace, dict:new()),
    % io:format("~p\n", [dict:to_list(DictTraces) ]), 
    InitialState = 
    	#evaltree_state{
    		free = 1,
    		pids_info = [#pid_info{pid = PidInit, is_first = true}]
    	},
    FinalState0 = 
  		lists:foldl(fun build_graph_trace/2, InitialState, Trace),

  	FinalState1 = 
  		add_spawn_sent_info(FinalState0),

  	FinalState = 
  		add_result_info(FinalState1),

  	{_, DictPids} = 
  		lists:foldl(fun assign_node_num/2, {0, dict:new()}, FinalState#evaltree_state.pids_info),


  	PidsTree = digraph:new(),
  	[build_pids_tree(PidInfo, DictPids, DictFuns, PidsTree) 
  	 || PidInfo <- FinalState#evaltree_state.pids_info],


   %  io:format("~p\n", [dict:to_list(DictPids)]),
  	% io:format("~p\n", [FinalState]),


	dot_graph_file_int(PidsTree, "pids_tree", fun dot_vertex_pids_tree/1),

	
	communication_sequence_diagram(
		FinalState#evaltree_state.pids_info,
		FinalState#evaltree_state.communication),

	PidsSummary = 
		[ {Pid, Call, Sent, Spawned, Result}
		|| #pid_info{
				pid = Pid,
				first_call = Call,
				sent = Sent,
				spawned = Spawned,
				result = Result
		   } <- lists:reverse(FinalState#evaltree_state.pids_info)],
    edd_graph!{add_vertex, 0, {PidsSummary, lists:reverse(FinalState#evaltree_state.communication)}},
    {_,DictNodes} = lists:foldl(fun build_eval_tree/2, {1, dict:new()} , lists:reverse(FinalState#evaltree_state.pids_info)),
    io:format("~p\n", [dict:to_list(DictNodes)]), 
    edd_graph!{get,self()},
	receive 
		G -> ok
	end,
    {DictQuestions, DictTrace} = build_questions(G, DictNodes),
    io:format("~p\n", [lists:sort(dict:to_list(DictTrace))]), 


    % DictQuestions = [],
	% io:format("G: ~p\n", [G]),
    dot_graph_file_int(G, "eval_tree", fun(V) -> dot_vertex_eval_tree(V, DictQuestions) end),
    % edd_graph!del_disconected_vertices,
    {FinalState#evaltree_state.pids_info, FinalState#evaltree_state.communication, {G, DictQuestions}}.


build_graph_trace(
		{TraceId, {edd_trace, start_call, Pid, {Call, Context}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Store the first call. Only for first process, rest of process will use spawn to fill thi info
	NPidsInfo0 = 
		case PidsInfo of 
			[PidInfo = #pid_info{pid = Pid, first_call = none}] ->
				[PidInfo#pid_info{first_call = #call_info{call = Call}}]; 
			_ -> 
				PidsInfo
		end,
	% Add call to the process stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> add_new_callrec_stack(PI, {Pid, #call_info{call = Call}, Context}, TraceId) end,
			NPidsInfo0),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{TraceId, {edd_trace, end_call, Pid, {_Call, Result, Context}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add call to the snapshot and remove it from the stack. 
	% If the stack is empty, assign the final value to pid
	NPidsInfo = 
		lists:map(
			fun(PI) -> remove_callrec_stack(PI, {Pid, Result, Context}, TraceId) end,
			PidsInfo),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{TraceId, {edd_trace, made_spawn, Pid, Info }}, 
		State = #evaltree_state{pids_info = PidsInfo, communication = Communication}) ->
	{Args, NewPid, PosAndPP} = Info,
	% NPidsInfo0 = 
	% 	lists:map(fun(PI) -> add_spawn(PI, {Pid, NewPid}) end, PidsInfo),

	SpawnRecord = 
		#spawn_info{spawner = Pid, spawned = NewPid, trace = TraceId},

	% Add to the communication history
	NCommunication = 
		[{spawned, SpawnRecord} | Communication],
	% Add pid and call info of the spawned process
	NPidsInfo0 = 
		[#pid_info{
			pid = NewPid, 
			first_call = #call_info{call = Args, pos_pp = PosAndPP}
		} 
		| PidsInfo],
	% Add spawn info to the stack 
	NPidsInfo = 
		lists:map(fun(PI) -> add_spawn_stack(PI, {Pid, SpawnRecord}) end, NPidsInfo0),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo,
		communication = NCommunication
	};
build_graph_trace(
		{TraceId, {edd_trace, send_sent, Pid, {PidReceive, Msg, _PosAndPP}}}, 
		State = #evaltree_state{pids_info = PidsInfo, communication = Communication}) ->
	MessageRecord = 
		#message_info{from = Pid, to = PidReceive, msg = Msg, trace = TraceId},
	% Add to the communication history
	NCommunication = 
		[{sent,MessageRecord} | Communication],
	% Add sent info to the stack 
	NPidsInfo0 = 
		lists:map(fun(PI) -> add_msg_sent_stack(PI, {Pid, MessageRecord}) end, PidsInfo),
	% Add received info to the stack 
	NPidsInfo = 
		lists:map(fun(PI) -> add_msg_received_stack(PI, {PidReceive, MessageRecord}) end, NPidsInfo0),
	% New state
	State#evaltree_state{
		communication = NCommunication,
		pids_info = NPidsInfo
	};
build_graph_trace(
		{TraceId, {edd_trace, receive_reached, Pid, {Context, Receive = {{pos_info,{_Module, _File, _Line, _StrReceive}}}}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add receive to the stack
	NPidsInfo0 = 
		lists:map(
			fun(PI) -> add_new_callrec_stack(PI, {Pid, #receive_info{pos_pp = Receive}, Context}, TraceId) end,
			PidsInfo),
	% Store a snapshot of the current stack
	NPidsInfo1 = 
		lists:map(
			fun(PI) -> add_snapshot(PI, Pid) end,
			NPidsInfo0),
	% Empty recursively sent,received,consumed and spwan info in the stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> empty_info_stack(PI, Pid) end,
			NPidsInfo1),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{TraceId, {edd_trace, receive_evaluated, Pid, {Msg, Context, Bindings, Clause}}}, 
		State = #evaltree_state{pids_info = PidsInfo, communication = Communication}) ->
	MsgSender = 
		[PidSenderCom || 
			{sent, #message_info{from = PidSenderCom, to = PidReceiverCom, msg = MsgCom}} <- Communication,
			MsgCom == Msg, PidReceiverCom == Pid],
	{NCommunication, NPidsInfo} = 
		case MsgSender of 
			% Only if a sender for the consumed message is found
			[MsgSender_] ->
				MsgRecord = 
					#message_info{from = MsgSender_, to = Pid, msg = Msg, trace = TraceId},
				{
					% Add to the communication history
					[{received, MsgRecord} | Communication], 
					% Add consumed info to the stack 
					lists:map(
						fun(PI) -> add_msg_consumed_stack(PI, {Pid, MsgRecord, Context, Bindings, Clause}, TraceId) end, 
						PidsInfo)
				};
			[] ->
				{Communication, PidsInfo}
		end,	
	% New state
	State#evaltree_state{
		communication = NCommunication,
		pids_info = NPidsInfo
	};
build_graph_trace(
		{TraceId, {edd_trace, receive_finished, Pid, {Result, Context}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add call to the snapshot and remove it from the stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> remove_callrec_stack(PI, {Pid, Result, Context}, TraceId) end,
			PidsInfo),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	}.
% build_graph_trace(_, State) ->	
% 	State.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% State modifiers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% add_spawn(
% 		PidInfo = #pid_info{pid = Pid, spawned = Spawned}, 
% 		{Pid, NewPid})->
% 	PidInfo#pid_info{spawned = [NewPid|Spawned]};
% add_spawn(PidInfo, _)->
% 	PidInfo.

add_new_callrec_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = CallRecStack}, 
		{Pid, CallRec, Context},
        TraceId) ->
    NCallRecStack = 
        [CSI#callrec_stack_item{reached_rec_val = TraceId} || CSI <-  CallRecStack],
	PidInfo#pid_info{
		callrec_stack=		
			[ #callrec_stack_item{origin_callrec = CallRec, context = Context, trace = TraceId}
			| NCallRecStack] 
	};
add_new_callrec_stack(PidInfo, _, _)->
	PidInfo.

remove_callrec_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[ CallRec 
				| CallRecStack],
			snapshots = Snapshots}, 
		{Pid, Result, Context},
        TraceId) ->
	PidInfo#pid_info{
		callrec_stack =	CallRecStack,
		snapshots = 
			Snapshots ++ 
			[#snapshot_info{
				top = CallRec#callrec_stack_item{result = Result, context = Context, trace = TraceId}, 
				rest = CallRecStack
			}],
		result = 
			case CallRecStack of 
				[] -> 
					Result;
				_ -> 
					none 
			end
	};
remove_callrec_stack(PidInfo, _, _)->
	PidInfo.


add_snapshot(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = CallRecStack,
			snapshots = Snapshots}, 
		Pid) ->
	PidInfo#pid_info{
		snapshots = 
			Snapshots ++ [#snapshot_info{rest = CallRecStack}]
	};
add_snapshot(PidInfo, _)->
	PidInfo.

add_msg_sent_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[HeadStack = #callrec_stack_item{sent = Sent} 
				| TailCallStack]}, 
		{Pid, Msg}) ->
	PidInfo#pid_info{
		callrec_stack=		
			[ HeadStack#callrec_stack_item{sent = Sent ++ [Msg]}
			| TailCallStack] 
	};
add_msg_sent_stack(PidInfo, _)->
	PidInfo.

add_msg_received_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[HeadStack = #callrec_stack_item{received = Received} 
				| TailCallStack]}, 
		{Pid, Msg}) ->
	PidInfo#pid_info{
		callrec_stack =		
			[ HeadStack#callrec_stack_item{received = Received ++ [Msg]}
			| TailCallStack] 
	};
add_msg_received_stack(PidInfo, _)->
	PidInfo.

add_msg_consumed_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[HeadStack = 
					#callrec_stack_item{
						consumed = Consumed, 
						origin_callrec = CallRec
					} 
				| TailCallStack]}, 
		{Pid, Msg, Context, Bindings, Clause},
        TraceId) ->
	PidInfo#pid_info{
		callrec_stack =		
			[ HeadStack#callrec_stack_item{
				consumed = Consumed ++ [Msg],
				origin_callrec = 
					CallRec#receive_info{
						context = Context,
						bindings = Bindings,
						clause = Clause
					},
                trace = TraceId
				}
			| TailCallStack] 
	};
add_msg_consumed_stack(PidInfo, _, _)->
	PidInfo.

add_spawn_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[HeadStack = #callrec_stack_item{spawned = Spawned} 
				| TailCallStack]}, 
		{Pid, SpawnInfo}) ->
	PidInfo#pid_info{
		callrec_stack =		
			[ HeadStack#callrec_stack_item{spawned = Spawned ++ [SpawnInfo]}
			| TailCallStack] 
	};
add_spawn_stack(PidInfo, _)->
	PidInfo.

empty_info_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = CallStack}, 
		Pid) ->
	NCallStack = 
		[CallStackItem#callrec_stack_item{
			consumed = [],
			spawned = [],
			sent = [],
			received = []
		 }
		 || CallStackItem <- CallStack],
	PidInfo#pid_info{
		callrec_stack =	NCallStack 
	};
empty_info_stack(PidInfo, _)->
	PidInfo.

add_spawn_sent_info(
		State = #evaltree_state{
			pids_info = PidsInfo, 
			communication = Communications
		}) ->
	NPidsInfo = 
		lists:map(
			fun(PI) -> add_spawn_sent_info_pid(PI,Communications) end, 
			PidsInfo),
	State#evaltree_state{
		pids_info = NPidsInfo
	}.

add_spawn_sent_info_pid(PidInfo = #pid_info{pid = Pid}, Communications) ->
	PidInfo#pid_info{
		sent = 
			[MsgInfo
			 || {sent, MsgInfo = #message_info{from = From}} <-  Communications, 
			    From == Pid],
		spawned = 
			[Spawned 
			 || {spawned, 
			 		SpawnInfo = #spawn_info{
			 			spawner = Spawner, 
						spawned = Spawned
					}
				} <-  Communications, 
			    Spawner == Pid]
	}.

add_result_info(State = #evaltree_state{pids_info = PidsInfo}) ->
	NPidsInfo = 
		lists:map(
			fun add_result_info_pid/1, 
			PidsInfo),
	State#evaltree_state{
		pids_info = NPidsInfo
	}.

add_result_info_pid(
		PidInfo = #pid_info{
			pid = Pid,
			result = Result,
			callrec_stack = Stack,
            snapshots = Snapshots
		}) ->
	{NResult, NSnapshots} = 
		case Result of 
			none -> 
				case Stack of 
					[Head = #callrec_stack_item{
						origin_callrec = #receive_info{clause = none},
                        trace = TraceId
					}| _] ->
						{stuck_receive, Snapshots ++ stuck_receive_snapshots(Stack, TraceId, [$s|integer_to_list(TraceId)])};
					_ ->
						{none, Snapshots}
				end;
			_ ->
				{Result, Snapshots}
		end,
	PidInfo#pid_info{result = NResult, snapshots = NSnapshots}.


stuck_receive_snapshots(
        [Head = 
            #callrec_stack_item{
                reached_rec_val = ReachedRec,
                trace = CurrTraceId
            }| Tail], 
        PrevReachedRec, TraceId) ->
    NSnapshot =  
        #snapshot_info{
            top = 
                Head#callrec_stack_item{
                    result = stuck_receive, 
                    trace = 
                        case CurrTraceId of 
                            PrevReachedRec ->
                                TraceId;
                            _ ->
                                CurrTraceId

                        end,
                    reached_rec_val = 
                        case ReachedRec of 
                            PrevReachedRec ->
                                TraceId;
                            _ ->
                                ReachedRec
                        end
                }, 
            rest = Tail
        },  
    [NSnapshot | stuck_receive_snapshots(Tail, PrevReachedRec, TraceId)];
stuck_receive_snapshots([], _, _) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for drawing process trees 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


assign_node_num(#pid_info{pid = Pid},{Num, Dict}) ->
	{Num + 1, dict:append(Pid, Num, Dict)}.

build_pids_tree(#pid_info{pid = Pid, spawned = Spawned, first_call = #call_info{call = FunCall}}, DictPids, DictFuns, PidsTree) ->
	[V] = dict:fetch(Pid, DictPids),
	StrFun = 
		case FunCall of 
			{M,F,As} ->
				build_call_string(FunCall);
			{AnoFun} -> 
				PosInfo = {pos_info,{Mod,_,_,_}} = hd(dict:fetch(AnoFun, DictFuns)),
				build_call_string({Mod, PosInfo, []})
		end,
	Label = 
		lists:flatten(io_lib:format("~p\n~s", [Pid, StrFun])),
	digraph:add_vertex(PidsTree, V, Label),
	[digraph:add_edge(PidsTree, V, hd(dict:fetch(PidSpawned, DictPids))) 
		|| PidSpawned <- Spawned],
	ok.

dot_vertex_pids_tree({V,L}) ->
	integer_to_list(V) ++ " " ++ "[shape=ellipse, label=\""
	++ L
	++"\"];\n".   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General DOT tree functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dot_graph_file_int(G, Name, FunVertex) ->
	file:write_file(Name++".dot", list_to_binary("digraph PDG {\n"++dot_graph(G, FunVertex)++"}")),
	os:cmd("dot -Tpdf "++ Name ++".dot > "++ Name ++".pdf").	

dot_graph(G, FunVertex)->
	Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
	Edges = [{V1,V2}||V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
	lists:flatten(lists:map(FunVertex,Vertices))++
	lists:flatten(lists:map(fun dot_edge/1,Edges)).  
	    
dot_edge({V1,V2}) -> 
	integer_to_list(V1)++" -> "++integer_to_list(V2)
	++" [color=black, penwidth=3];\n".	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Drawing communication sequence diagram
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lines(Lines) ->
	lists:foldl(
		fun(Line,Acc) -> Acc ++ Line ++ "\n" end 
		, "", Lines).

quote_enclosing(Str) ->
	"\"" ++ Str ++ "\"".

str_term(Pid) ->
	lists:flatten(io_lib:format("~p", [Pid]) ).

build_pid_line(Pid, Call, NumPoints) ->
	Steps = 
		lists:foldl(
			fun(Num, Acc) -> Acc ++ " -> " ++ quote_enclosing(Pid ++ integer_to_list(Num)) end
			, "", lists:seq(1, NumPoints)), 
	lines(
		["{" 
		,"  rank=\"same\";" 
		,"  " ++ quote_enclosing(Pid) ++ "[shape=\"plaintext\", label=" ++ quote_enclosing(Pid ++"\n" ++ build_call_string(Call) ) ++ "] "
		,"  "++ quote_enclosing(Pid) ++ Steps ++ ";"
		,"} "]).


divide_list(Pred, [H|T], Prev) ->
	case Pred(H) of 
		true -> 
			{Prev, T};
		false ->
			divide_list(Pred, T, Prev ++ [H])
	end;
divide_list(Pred, [], Prev) ->
	{Prev,[]}.

acc_search_in_list(Pred, [H|T]) ->
	case Pred(H) of 
		true -> 
			{[H],true};
		false ->
			{List ,Found} = acc_search_in_list(Pred, T),
			{[H|List] ,Found}
	end;
acc_search_in_list(Pred, []) ->
	{[],false}.

distribute_edge([Pid]) ->
	quote_enclosing(Pid) ++ "[style=invis];\n";
distribute_edge([Pid| Tail]) ->
	quote_enclosing(Pid) ++ " -> " ++ distribute_edge(Tail).

build_line_until_to([N1, N2], CurrentStep, CommonEdgeProperties, LastEdgeProperties) ->
	[	
			quote_enclosing(N1 ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(N2 ++ integer_to_list(CurrentStep)) 
		++  " [" ++ LastEdgeProperties  ++ "];"];
build_line_until_to([N1, N2 | Tail], CurrentStep, CommonEdgeProperties, LastEdgeProperties) ->
	[	
			quote_enclosing(N1 ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(N2 ++ integer_to_list(CurrentStep)) 
		++ 	" [" ++ CommonEdgeProperties  ++ "];"
	| build_line_until_to([N2 | Tail], CurrentStep, CommonEdgeProperties, LastEdgeProperties)];
build_line_until_to(_, _, _, _) ->
	[].

build_transitive_edge(From, From, CommonEdgeProperties, LastEdgeProperties, Pids, CurrentStep) ->
	OtherSteps = 
		[quote_enclosing(Pid ++ integer_to_list(CurrentStep)) 
		 ++ 	" -> " 
		 ++ 	quote_enclosing(Pid ++ integer_to_list(CurrentStep)) 
		 ++  " [" ++ LastEdgeProperties ++ ", style=invis];" 
		 || Pid <- Pids, Pid /= str_term(From)],
	LoopEdge = 
		quote_enclosing(str_term(From) ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(str_term(From) ++ integer_to_list(CurrentStep)) 
		++  " [" ++ LastEdgeProperties ++ "];",
	[LoopEdge | OtherSteps];
build_transitive_edge(From, To, CommonEdgeProperties, LastEdgeProperties, Pids, CurrentStep) ->
	{Before0, After} = 
		divide_list(fun(Pid) -> str_term(From)  ==  Pid end, Pids, []),
	Before = 
		lists:reverse(Before0), 
	PredTo = 
		fun(Pid) -> str_term(To)  ==  Pid end,
	{ToInBefore, FoundBefore} = acc_search_in_list(PredTo, Before),
	{ToInAfter, FoundAfter} = acc_search_in_list(PredTo, After),
	ListWhereIsTo = 
		case {FoundBefore, FoundAfter} of 
			{true, _} ->
				ToInBefore;
			{_, true} ->
				 ToInAfter
		end,
	% io:format("ListWhereIsTo: ~p\n",[ListWhereIsTo]),
	build_line_until_to([str_term(From) | ListWhereIsTo], CurrentStep, CommonEdgeProperties, LastEdgeProperties).

communication_lines(
		{sent, #message_info{from = From , to = To, msg = Msg}}, 
		Pids, CurrentStep) -> 
	LastEdgeProperties = 
		"label=" ++ quote_enclosing(str_term(Msg)) ++ ", arrowhead=\"normal\"",
	CommonEdgeProperties = 
		"",
	{build_transitive_edge(From, To, CommonEdgeProperties, LastEdgeProperties, Pids, CurrentStep), CurrentStep + 1};
communication_lines(
		{received, #message_info{from = From , to = To, msg = Msg}}, 
		Pids, CurrentStep) -> 
	Label = 
		lists:flatten(io_lib:format("~p (from ~p)", [Msg, From]) ), 
	ReceiveEdge = 
		[	
			quote_enclosing(str_term(To) ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(str_term(To) ++ integer_to_list(CurrentStep + 1)) 
		++  " [label=" ++ quote_enclosing(Label) ++ "];"],
	{ReceiveEdge, CurrentStep + 2};
communication_lines(
		{spawned, #spawn_info{spawner = Spawner, spawned = Spawned}}, 
		Pids, CurrentStep) -> 
	LastEdgeProperties = 
		"label=" ++ "\"\"" ++ ", penwidth = 3, color = red, arrowhead=\"normal\"",
	CommonEdgeProperties = 
		"label=" ++ "\"\"" ++ ", penwidth = 3, color = red",
	{build_transitive_edge(Spawner, Spawned, CommonEdgeProperties, LastEdgeProperties, Pids, CurrentStep), CurrentStep + 1}.

communication_sequence_diagram(PidsInfo, Communications) when length(PidsInfo) > 1 ->
	Header = 
		["digraph G {"
		,"  rankdir=\"LR\";"
		,"  node[shape=\"point\"];"
		,"  edge[arrowhead=\"none\"]"],
	LengthReceives = 
		length([0||{received,_} <- Communications]),
	NumPoints = 
		(length(Communications) - LengthReceives)
		+ (2 * LengthReceives),
	{PidsStr, PidsCall} = 
		lists:unzip([{str_term(Pid), FirsCall} || #pid_info{pid = Pid, first_call = #call_info{call = FirsCall}} <- PidsInfo]),
	PidLines = 
		lists:map(
			fun({Pid, Call}) -> build_pid_line(Pid, Call, NumPoints) end 
			, lists:zip(PidsStr, PidsCall) ),
	Distribution = distribute_edge(PidsStr), 
	{CommLines0,_} = 
		lists:mapfoldl(
			fun(Com, CurrentStep) -> communication_lines(Com, PidsStr, CurrentStep) end,
			1,
			lists:reverse(Communications)),
	CommLines = lists:concat(CommLines0),

	Closer = "}",
	DotContent = 
		lines(Header ++ PidLines ++ [Distribution] ++ CommLines ++ [Closer]),

	Name = "comm_seq_diag",
	file:write_file(Name ++ ".dot", list_to_binary(DotContent)),
	os:cmd("dot -Tpdf "++ Name ++ ".dot > " ++ Name ++ ".pdf");
communication_sequence_diagram(_, _) ->
	ok. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build questions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(question,
    {
        text = none,
        answers = [],
        str_callrec = none
    }).

-record(answer,
    {
        text = none,
        when_chosen = none
    }).

any2str(Any) ->
    format("~p", [Any]).

build_answer(Str, Beh) ->
    #answer{
        text = Str,
        when_chosen = Beh
    }.

question_list(Text, []) ->
    "no " ++ Text;
question_list(Text, List) ->
    PPList = 
        lists:foldl(fun(E, Acc) -> Acc ++ [$\t|pp_item(E)] ++ "\n" end, "", List), 
    Text ++ ":\n" ++ lists:droplast(PPList).

tab_lines(String) ->
    Lines = string:tokens(String, "\n"),
    NLines = [[$\t|L] || L <- Lines],
    string:join(NLines, "\n").

list_ans(#answer{text = Text}) ->
    Text ++ "\n".

pp_item(#message_info{from = From, to = To, msg = Msg}) ->
    format("~p (from ~p to ~p)", [Msg, From, To]);
pp_item(#spawn_info{spawner = Spawner, spawned = Spawned}) ->
    format("~p", [Spawned]);
pp_item({Var, Value}) ->
    format("~p = ~p", [Var, Value]).

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).


prev_recieve_answer(PrevRec, DictNodes, G) ->
    % io:format("\nPREV: ~p\n", [PrevRec]),
    case dict:find(PrevRec, DictNodes) of 
        {ok, NodeDests} ->
            NodeDest = lists:last(NodeDests),
            {NodeDest, NodeInfo} = digraph:vertex(G, NodeDest),
            {#question{
                str_callrec = StrPrevReceive,
                answers = AnsPrevReceive
            }, _} = 
                build_question(NodeInfo, DictNodes, G, NodeDest, dict:new()),
            PreRecInfoStr = 
                StrPrevReceive ++ "\n" 
                ++ lists:flatten(
                        lists:map(
                            fun list_ans/1,
                            lists:droplast(AnsPrevReceive))),
            [build_answer(
                "previous evaluated receive:\n" 
                ++ tab_lines(PreRecInfoStr), 
                {correct, {goto, NodeDest}})];       
        _ ->
            []   
    end.

behaviour_question(SentSpawned, DictNodes, Node, BehEmptySame, BehOther) ->
    case SentSpawned of 
        [] ->
            BehEmptySame;
        _ ->
            #question{
                text = "Which one is not expected?",
                answers = 
                [ 
                    begin
                        {ok, [NodeDest]} = dict:find(MS, DictNodes),
                        case NodeDest of 
                            Node -> 
                                build_answer(pp_item(MS), BehEmptySame);
                            _ ->
                                build_answer(pp_item(MS), {BehOther, {goto, NodeDest}}) 
                        end
                    end
                || MS <- SentSpawned
                ]
            } 
    end.


reached_value_answer(none, none) ->
    [];
reached_value_answer(none, PrevRec) ->
    % {ok, [NodeReceive]} = 
    %     dict:find(PrevRec, DictNodes),
    % {_,#callrec_stack_item{
    %     origin_callrec = 
    %         #receive_info{
    %             pos_pp = {{pos_info,{_, _, _, ReceiveStr}}}
    %         }}} = 
    %     element(2,digraph:vertex(G, NodeReceive)), 
    % [build_answer("Reached receive\n" ++ ReceiveStr, {goto, NodeReceive})];
    [build_answer("reached receive:\n" ++ PrevRec, incorrect)];
reached_value_answer(stuck_receive, _) ->
    [build_answer("blocked because it is waiting for a message", incorrect)];
reached_value_answer(Val, _) ->
    [build_answer("evaluated to value: " ++ any2str(Val), incorrect)].

build_questions(G, DictNodes) ->
    DictsQuestionsTrace = 
        lists:foldl(
            fun(V, {CurrDictQuestions, CurrDictTrace}) ->
                NodeInfo = element(2,digraph:vertex(G, V)), 
                {Question, NDictTrace} = build_question(NodeInfo, DictNodes, G, V, CurrDictTrace),
                {dict:append(V, Question, CurrDictQuestions), NDictTrace}
            end, 
            {dict:new(), dict:new()}, 
            digraph:vertices(G)),
    % io:format("Questions: ~p\n~p\n", [dict:size(DictQuestions), lists:sort(dict:to_list(DictQuestions)) ]),
    DictsQuestionsTrace. 


% * Pregunta:
% La función ____ ha alcanzado el receive/valor ____, enviando por el camino
% los mensajes ___ y creando los procesos ___. ¿Qué es incorrecto?
% 1. La expresión alcanzada.
% 2. Los mensajes enviados.
% 3. Los procesos creados.
% 4. Nada
build_question(
    {Pid,#callrec_stack_item{
            origin_callrec = #call_info{call = {M,F,A}, pos_pp = Pos},
            reached_rec_val = PrevRec,
            context = Context,
            spawned = Spawned,
            sent = Sent,
            received = Received,
            consumed = Consumed,
            result = Result,
            trace = TraceId}
        },
        DictNodes, G, Node, DictTraces) -> 
    % StrList0 = 
    %     lists:foldl(fun(A_, Acc) -> Acc ++ any2str(A_) ++ ", " end, "", A), 
    % StrList = 
    %     case StrList0 of 
    %         "" ->
    %             "";
    %         _ ->
    %             lists:droplast(lists:droplast(StrList0))
    %     end,
    % io:format("~s\n", [StrList]),         
    CallStr = 
        lists:flatten(
            io_lib:format(
                "~p:~p(~s)", 
                [M,F,string:join(lists:map(fun any2str/1, A), ", ") ]
            )
        ),
    Question = 
        "Process " ++ any2str(Pid) ++ " called " 
        ++ CallStr ++ ".\nWhat is wrong?",
    % io:format(Question),
    PrevReceive = 
        prev_recieve_answer(PrevRec, DictNodes, G),
    Answers = 
        reached_value_answer(Result, PrevRec) ++ 
        PrevReceive ++ 
        [
         build_answer(question_list("sent messages",Sent), behaviour_question(Sent, DictNodes, Node, incorrect, correct)),
         build_answer(question_list("created processes",Spawned), behaviour_question(Spawned, DictNodes, Node, incorrect, correct)),
         build_answer("nothing", correct)
        ],
    % Question ++ "\n" ++ any2str(Answers);
    % NDictTraces0 = 
    %     [Spawned]
    NDictTraces  = 
        add_to_trace_dict(DictTraces, Spawned ++ Sent ++ Consumed, Node),
    {#question{
        text = Question,
        answers = Answers,
        str_callrec = CallStr
    }, NDictTraces};
% Al receive ___ le han llegado estos mensajes, con lo que ha procesado el mensaje ___ usando
% la rama i y ha alcanzado la expresión ___ enviando por el camino estos mensajes y creado
% estos procesos, dado el contexto ___. ¿Qué está mal?
% 1. El contexto.
% 2. La lista de mensajes.
% 3. El mensaje consumido.
% 4. La expresión alcanzada.
% 5. Los mensajes generados.
% 6. Los procesos generados.
% 7. Nada.
build_question(
    {Pid,#callrec_stack_item{
            origin_callrec = 
                #receive_info{
                    clause = Clause,
                    context = ContextRec,
                    bindings = Bindings, 
                    pos_pp = {{pos_info,{M, F, L, ReceiveStr0}}}
                },
            reached_rec_val = PrevRec,
            context = Context,
            spawned = Spawned,
            sent = Sent,
            received = Received,
            consumed = Consumed,
            result = Result,
            trace = TraceId}
        },
        DictNodes, G, Node, DictTraces) ->    
    ReceiveStr = 
        lists:flatten(io_lib:format("~s\nin ~s:~p", [ReceiveStr0, F, L])),
    Question = 
        "Process " ++ any2str(Pid) ++ " evaluated\n" 
        ++ ReceiveStr ++ "\nWhat is wrong?",
    PrevReceive = 
        prev_recieve_answer(PrevRec, DictNodes, G),
    Answers = 
        PrevReceive ++
        [
         build_answer(question_list("context",Context), correct),
         % build_answer("ContextRec: " ++ any2str(ContextRec), correct),
         build_answer(question_list("received messages",Received),behaviour_question(Received, DictNodes, Node, correct, correct)),
         build_answer(question_list("consumed messages",Consumed), incorrect)
        ]
        ++ reached_value_answer(Result, PrevRec) ++
        [
         build_answer(question_list("sent messages",Sent), behaviour_question(Sent, DictNodes, Node, incorrect, correct)),
         build_answer(question_list("created processes",Spawned), behaviour_question(Spawned, DictNodes, Node, incorrect, correct)),
         build_answer("nothing", correct)
        ],
    NDictTraces  = 
        add_to_trace_dict(DictTraces, Spawned ++ Sent ++ Consumed, Node),
    {#question{
        text = Question,
        answers = Answers,
        str_callrec = ReceiveStr
    }, NDictTraces};
build_question(NodeInfo, _, _, _, DictTraces) -> 
    {#question{}, DictTraces}.

list_answers( 
        #answer{
            text = Text,
            when_chosen = Beh
        }, {Opt, Acc}) ->
    StrAnswer = 
        io_lib:format("~p. - ~s (Behaviour: ~p)\n", [Opt, Text, Beh]),
    {Opt + 1, lists:flatten(Acc ++ StrAnswer)}.

add_to_trace_dict(DictTraces, SpawnedMsgs, V) ->
    lists:foldl(
        fun(E, CurrDict) -> 
            TraceId = 
                case E of 
                    #spawn_info{trace = Trace0} ->
                        Trace0;
                    #message_info{trace = Trace0} ->
                        Trace0
                end,
            dict:append(TraceId, V,CurrDict)
        end,
        DictTraces,
        SpawnedMsgs
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build the evaluation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_eval_tree(#pid_info{
            pid = Pid, 
            result = Result, 
            snapshots = Snapshots}, {Free, Dict}) ->
    build_eval_tree_snap(Free, Dict, Pid, Snapshots, Result, [], [], [], none).


build_eval_tree_snap(Free, Dict, Pid, [#snapshot_info{top = Top, rest = Rest} | Snapshots], Result, Pending, PrevSent, PrevSpawned, PrevEvalRec) ->
    % {NFree, NPending, NDict} = 
    % io:format("Top: ~p. - ~p\n", [Free, Top]),
    % io:format("PrevEvalRec: ~p\n", [PrevEvalRec]),
    Res = 
        case Top of 
            none -> 
                {build_stack_nodes(Free, Dict, Pid, Rest, none, Pending, none, [], []), 
                 [], [], none};
            #callrec_stack_item{origin_callrec = CallRec, sent = Sent, spawned = Spawned, trace = TraceId} ->
                NPrevSent_ = PrevSent ++ Sent,
                NPrevSpawned_ = PrevSpawned ++ Spawned,
                NTop0 = Top#callrec_stack_item{sent = NPrevSent_, spawned = NPrevSpawned_},
                NTop = 
                    case PrevEvalRec of 
                        none -> 
                            NTop0;
                        _ ->
                           NTop0#callrec_stack_item{reached_rec_val = PrevEvalRec}
                    end,
                edd_graph!{add_vertex, Free, {Pid, NTop}},
                [edd_graph!{add_edge, Free, Node} || {Node, _} <- Pending],
                NDict0 = 
                    lists:foldl(
                        fun(M, CurrDict) -> dict:append(M, Free, CurrDict) end,
                        Dict, Sent ++ Spawned),
                NDict1 = 
                    case TraceId of 
                        none ->
                            NDict0;
                        _ ->
                            dict:append(TraceId, Free, NDict0)
                    end,
                NPrevEvalRec_ = 
                    case CallRec of 
                        #receive_info{} ->
                            TraceId;
                        _ ->
                            PrevEvalRec
                    end,
                {{Free + 1, [{Free, length(Rest)}], NDict1}, NPrevSent_, NPrevSpawned_, NPrevEvalRec_}
        end,
    % io:format("~p\n", [Res]),
    {{NFree, NPending, NDict}, NPrevSent, NPrevSpawned, NPrevEvalRec} = Res,
    build_eval_tree_snap(NFree, NDict, Pid, Snapshots, Result, NPending, NPrevSent, NPrevSpawned, NPrevEvalRec);
build_eval_tree_snap(Free, Dict, Pid, [], _, Pending, _ , _, _) ->
    [edd_graph!{add_edge, 0, Node} || {Node, _} <- Pending],
    {Free, Dict}.


build_stack_nodes(
        Free, Dict, Pid,  
        Stack = 
            [H = 
                #callrec_stack_item{
                    origin_callrec = 
                        #receive_info{
                            pos_pp = {{pos_info,{M, F, L, ReceiveStr0}}}
                        }
                }
            | T], 
        Previous, Pending, none, PrevSent, PrevSpawnwed) ->
    % io:format("H: ~p. - ~p\n", [none, H]),
    ReceiveStr = 
        lists:flatten(io_lib:format("~s\n ~s:~p", [ReceiveStr0, F, L])),
    build_stack_nodes(Free, Dict, Pid, T, none, Pending, ReceiveStr, PrevSent, PrevSpawnwed);
build_stack_nodes(
        Free, Dict, Pid,  
        Stack = 
            [H = #callrec_stack_item{sent = Sent, spawned = Spawned, trace = TraceId} | T], 
        Previous, Pending, ReachedRec, PrevSent, PrevSpawnwed) ->
    % io:format("H: ~p. - ~p\n", [Free, H]),
    NSent = PrevSent ++ Sent, 
    NSpawned = PrevSpawnwed ++ Spawned,
    edd_graph!
        {
            add_vertex, 
            Free, 
            {
                Pid, 
                H#callrec_stack_item{
                    reached_rec_val = ReachedRec,
                    sent = NSent,
                    spawned = NSpawned
                }
            }
        },
    case Previous of 
        none -> 
            ok;
        _ -> 
            edd_graph!{add_edge, Free, Previous}
    end,
    NDict0 = 
        lists:foldl(
            fun(M, CurrDict) -> dict:append(M, Free, CurrDict) end,
            Dict, Sent ++ Spawned),
    NDict = 
        dict:append(TraceId, Free, NDict0),
    % io:format("Pending: ~p -> ~p\n", [Pending, length(Stack)]),
    PendingSameSize = 
        [NodeInfo || NodeInfo = {_, SizeStack} <- Pending, length(Stack) == SizeStack],
    NPending = Pending -- PendingSameSize,
    [edd_graph!{add_edge, Free, Node} || {Node, _} <- PendingSameSize],
    build_stack_nodes(Free + 1, NDict, Pid, T, Free, NPending, ReachedRec, NSent, NSpawned);
build_stack_nodes(Free, Dict, _, [], Previous, Pending, _, _, _) ->
    edd_graph!{add_edge, 0, Previous},
    {Free, Pending, Dict}.


dot_vertex_eval_tree({V,NodeInfo}, DictQuestion) ->
    {ok, [#question{text = Question, answers = Answers}]} = 
        dict:find(V, DictQuestion),
    {_, StrAnswers} = 
        lists:foldl(fun list_answers/2, {1,""}, Answers),
    StrQuestion = 
        case Question of 
            none ->
                lists:flatten(io_lib:format("~p", [NodeInfo])) ;
            _ ->
                Question ++ "\n" ++ StrAnswers
        end,
    integer_to_list(V) ++ " " ++ "[shape=ellipse, label=\""
    ++ change_new_lines(lists:flatten(io_lib:format("~p. -\n ~s", [V, StrQuestion]) ))
    % ++ change_new_lines(lists:flatten(io_lib:format("~p. -\n ~p", [V, NodeInfo]) ))
    ++ "\"];\n".  

change_new_lines([10|Chars]) ->
    [$\\,$l|change_new_lines(Chars)];
change_new_lines([$"|Chars]) ->
    [$\\,$"|change_new_lines(Chars)];
change_new_lines([Other|Chars]) ->
    [Other|change_new_lines(Chars)];
change_new_lines([]) ->
    [].
     
tupled_graph({G, DictQA})->
    Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
    Edges = [{V1,V2} || V1 <- digraph:vertices(G),V2 <- digraph:out_neighbours(G, V1)],
    Tupled_Erlang = 
        {
            {vertices, 
                lists:map(
                    fun(V) -> 
                        tupled_vertex(G, V, DictQA) 
                    end,
                    Vertices)},
            {edges,
                lists:map(fun tupled_edge/1,Edges)}
        },
    % io:format("Tupled_Erlang: ~p\n", [Tupled_Erlang]),
    Tupled_Erlang.
    


tupled_vertex(G, {V,Info}, DictQA) ->
    {ok, [Question = #question{}]} = 
        dict:find(V, DictQA),
    {
        {id, V},
        {question, Question},
        {info, Info}
    }.     
        
tupled_edge({V1,V2}) -> 
    {V1, V2}.