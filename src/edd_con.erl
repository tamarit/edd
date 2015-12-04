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
-export([ddc/2,ddc/4]).


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

ddc(Expr,Timeout,Strategy,Priority) ->
	Graph = true,
	{ok,[AExpr|_]} = edd_lib:parse_expr(Expr++"."),
	M1 = smerl:new(foo),
	{ok, M2} = smerl:add_func(M1,"bar() ->" ++ Expr ++ " ."),
	%Obtiene el CORE de la expresión Expr
	{ok,_,CoreP} = smerl:compile2(M2,[to_core,binary,no_copt]), 
	InitialCall = extract_call(CoreP),
	%io:format("InitialCall: ~p\n",[InitialCall]),
	FunOperator = erl_syntax:application_operator(AExpr),
	{remote,_,{atom,_,ModName},_} = FunOperator,
	Core = edd_lib:core_module(atom_to_list(ModName)++".erl"),
	%io:format("Core: ~p\n",[Core]),
	
	%to get the .core file. ONLY FOR DEBUGGING 
	%compile:file(atom_to_list(ModName)++".erl",[to_core,no_copt]),
	
	FunsInitialCall = get_funs_from_abstract(AExpr,-1),
	compile:file(atom_to_list(ModName)++".erl"),
	% {{first_pid,FirstPid},{traces,Traces0}} = 
	Self = self(),
	spawn(fun() -> edd_trace:trace(Expr,Timeout, Self) end),
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
	build_graph(Trace, DictFun, PidCall), 


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
	       edd_con_lib:dot_graph_file(G,"dbg");
	     false -> 
	       ok
	end,
 	edd_con_lib:ask(G,Strategy,Priority),

 	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	%%%% REMOVE COMMENTS TO ENABLE DEBUGGING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    PidG!stop,
    unregister(edd_graph),
    ok.

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
    	% receipt = [],
    	% consumed = [],
    	is_first = false,
    	callrec_stack = [],
    	snapshots = []
    }).

-record(message_info,
	{
		from = none,
		to = none,
		msg = none
	}).

-record(spawn_info,
	{
		spawner = none,
		spawned = none
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
    	receipt = [],
    	consumed = [],
    	% children_nodes = [],
    	result = none
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

  	io:format("~p\n", [dict:to_list(DictPids)]),
  	PidsTree = digraph:new(),
  	[build_pids_tree(PidInfo, DictPids, DictFuns, PidsTree) 
  	 || PidInfo <- FinalState#evaltree_state.pids_info],
  	io:format("~p\n", [FinalState]),
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
    lists:foldl(fun build_eval_tree/2, 1 , lists:reverse(FinalState#evaltree_state.pids_info)),
    edd_graph!{get,self()},
	receive 
		G -> ok
	end,
	% io:format("G: ~p\n", [G]),
    dot_graph_file_int(G, "eval_tree", fun dot_vertex_eval_tree/1),
    % edd_graph!del_disconected_vertices,
    ok.


build_graph_trace(
		{_, {edd_trace, start_call, Pid, Call}}, 
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
			fun(PI) -> add_new_callrec_stack(PI, {Pid, #call_info{call = Call}}) end,
			NPidsInfo0),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{_, {edd_trace, end_call, Pid, {_Call, Result}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add call to the snapshot and remove it from the stack. 
	% If the stack is empty, assign the final value to pid
	NPidsInfo = 
		lists:map(
			fun(PI) -> remove_callrec_stack(PI, {Pid, Result}) end,
			PidsInfo),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{_, {edd_trace, made_spawn, Pid, Info }}, 
		State = #evaltree_state{pids_info = PidsInfo, communication = Communication}) ->
	{Args, NewPid, PosAndPP} = Info,
	% NPidsInfo0 = 
	% 	lists:map(fun(PI) -> add_spawn(PI, {Pid, NewPid}) end, PidsInfo),

	SpawnRecord = 
		#spawn_info{spawner = Pid, spawned = NewPid},

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
		{_, {edd_trace, send_sent, Pid, {PidReceive, Msg, _PosAndPP}}}, 
		State = #evaltree_state{pids_info = PidsInfo, communication = Communication}) ->
	MessageRecord = 
		#message_info{from = Pid, to = PidReceive, msg = Msg},
	% Add to the communication history
	NCommunication = 
		[{sent,MessageRecord} | Communication],
	% Add sent info to the stack 
	NPidsInfo0 = 
		lists:map(fun(PI) -> add_msg_sent_stack(PI, {Pid, MessageRecord}) end, PidsInfo),
	% Add receipt info to the stack 
	NPidsInfo = 
		lists:map(fun(PI) -> add_msg_receipt_stack(PI, {PidReceive, MessageRecord}) end, NPidsInfo0),
	% New state
	State#evaltree_state{
		communication = NCommunication,
		pids_info = NPidsInfo
	};
build_graph_trace(
		{_, {edd_trace, receive_reached, Pid, Receive = {{pos_info,{_Module, _File, _Line, _StrReceive}}}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add receive to the stack
	NPidsInfo0 = 
		lists:map(
			fun(PI) -> add_new_callrec_stack(PI, {Pid, #receive_info{pos_pp = Receive}}) end,
			PidsInfo),
	% Store a snapshot of the current stack
	NPidsInfo1 = 
		lists:map(
			fun(PI) -> add_snapshot(PI, Pid) end,
			NPidsInfo0),
	% Empty recursively sent,receipt,consumed and spwan info in the stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> empty_info_stack(PI, Pid) end,
			NPidsInfo1),
	% New state
	State#evaltree_state{
		pids_info = NPidsInfo
	};
build_graph_trace(
		{_, {edd_trace, receive_evaluated, Pid, {Msg, Context, Bindings, Clause}}}, 
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
					#message_info{from = MsgSender_, to = Pid, msg = Msg},
				{
					% Add to the communication history
					[{received, MsgRecord} | Communication], 
					% Add consumed info to the stack 
					lists:map(
						fun(PI) -> add_msg_consumed_stack(PI, {Pid, MsgRecord, Context, Bindings, Clause}) end, 
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
		{_, {edd_trace, receive_finished, Pid, {Result}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Add call to the snapshot and remove it from the stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> remove_callrec_stack(PI, {Pid, Result}) end,
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
		{Pid, CallRec}) ->
	PidInfo#pid_info{
		callrec_stack=		
			[ #callrec_stack_item{origin_callrec = CallRec}
			| CallRecStack] 
	};
add_new_callrec_stack(PidInfo, _)->
	PidInfo.

remove_callrec_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[ CallRec 
				| CallRecStack],
			snapshots = Snapshots}, 
		{Pid, Result}) ->
	PidInfo#pid_info{
		callrec_stack =	CallRecStack,
		snapshots = 
			Snapshots ++ 
			[#snapshot_info{
				top = CallRec#callrec_stack_item{result = Result}, 
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
remove_callrec_stack(PidInfo, _)->
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

add_msg_receipt_stack(
		PidInfo = #pid_info{
			pid = Pid, 
			callrec_stack = 
				[HeadStack = #callrec_stack_item{receipt = Receipt} 
				| TailCallStack]}, 
		{Pid, Msg}) ->
	PidInfo#pid_info{
		callrec_stack =		
			[ HeadStack#callrec_stack_item{receipt = Receipt ++ [Msg]}
			| TailCallStack] 
	};
add_msg_receipt_stack(PidInfo, _)->
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
		{Pid, Msg, Context, Bindings, Clause}) ->
	PidInfo#pid_info{
		callrec_stack =		
			[ HeadStack#callrec_stack_item{
				consumed = Consumed ++ [Msg],
				origin_callrec = 
					CallRec#receive_info{
						context = Context,
						bindings = Bindings,
						clause = Clause
					} 
				}
			| TailCallStack] 
	};
add_msg_consumed_stack(PidInfo, _)->
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
			receipt = []
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
			callrec_stack = Stack
		}) ->
	NResult = 
		case Result of 
			none -> 
				case Stack of 
					[#callrec_stack_item{
						origin_callrec = #receive_info{clause = none}
					}|_] ->
						stuck_receive;
					_ ->
						none 
				end;
			_ ->
				Result 
		end,
	PidInfo#pid_info{result = NResult}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Drawing pids tree functions
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
		++  LastEdgeProperties];
build_line_until_to([N1, N2 | Tail], CurrentStep, CommonEdgeProperties, LastEdgeProperties) ->
	[	
			quote_enclosing(N1 ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(N2 ++ integer_to_list(CurrentStep)) 
		++ 	CommonEdgeProperties
	| build_line_until_to([N2 | Tail], CurrentStep, CommonEdgeProperties, LastEdgeProperties)];
build_line_until_to(_, _, _, _) ->
	[].


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
		" [label=" ++ quote_enclosing(str_term(Msg)) ++ ", arrowhead=\"normal\"];",
	CommonEdgeProperties = 
		";",
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
		" [label=" ++ "\"\"" ++ ", penwidth = 3, color = red, arrowhead=\"normal\"];",
	CommonEdgeProperties = 
		" [label=" ++ "\"\"" ++ ", penwidth = 3, color = red];",
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
% Build the evaluation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_eval_tree(#pid_info{pid = Pid, snapshots = Snapshots}, Free) ->
	build_eval_tree_snap(Free, Pid, Snapshots, []).


build_eval_tree_snap(Free, Pid, [#snapshot_info{top = Top, rest = Rest} | Snapshots], Pending) ->
	{NFree, NPending} = 
		case Top of 
			none -> 
				build_stack_nodes(Free, Pid, Rest, none, Pending);
			_ ->
				edd_graph!{add_vertex, Free, {Pid, Top}},
				[edd_graph!{add_edge, Free, Node} || {Node, _} <- Pending],
				{Free + 1, [{Free, length(Rest)}]}
		end,
	build_eval_tree_snap(NFree, Pid, Snapshots, NPending);
build_eval_tree_snap(Free, Pid, [], Pending) ->
	[edd_graph!{add_edge, 0, Node} || {Node, _} <- Pending],
	Free.


build_stack_nodes(Free, Pid, Stack = [H | T], Previous, Pending) ->
	edd_graph!{add_vertex, Free, {Pid, H}},
	case Previous of 
		none -> 
			ok;
		_ -> 
			edd_graph!{add_edge, Free, Previous}
	end,
	% io:format("Pending: ~p -> ~p\n", [Pending, length(Stack)]),
	PendingSameSize = 
		[NodeInfo || NodeInfo = {_, SizeStack} <- Pending, length(Stack) == SizeStack],
	NPending = Pending -- PendingSameSize,
	[edd_graph!{add_edge, Free, Node} || {Node, _} <- PendingSameSize],
	build_stack_nodes(Free + 1, Pid, T, Free, NPending);
build_stack_nodes(Free, _, [], Previous, Pending) ->
	edd_graph!{add_edge, 0, Previous},
	{Free, Pending}.


dot_vertex_eval_tree({V,L}) ->
	integer_to_list(V) ++ " " ++ "[shape=ellipse, label=\""
	++ change_new_lines(lists:flatten(io_lib:format("~p. -\n ~p", [V, L]) ))
	++ "\"];\n".  

change_new_lines([10|Chars]) ->
	[$\\,$l|change_new_lines(Chars)];
change_new_lines([$"|Chars]) ->
	[$\\,$"|change_new_lines(Chars)];
change_new_lines([Other|Chars]) ->
	[Other|change_new_lines(Chars)];
change_new_lines([]) ->
	[].
