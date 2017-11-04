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
-export([cdd/2, cdd/4, cdd_server/3, summarizes_pidinfo/1, pp_item/1, complexity_term/1]).

-include_lib("edd_con.hrl").


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

cdd(Expr,Timeout) ->
	cdd(Expr,Timeout,divide_query,indet).

cdd_server(Expr, Dir, Timeout) ->
    code:add_patha(Dir), 
    % io:format("PATHS: ~p\n",[code:get_path()]),
    {_, {Pid, Comm, G, DictTrace}} = 
        cdd_internal_core(
            Expr, 
            Timeout, 
            fun(X) -> edd_lib:core_module(atom_to_list(X) ++ ".erl", Dir) end,
            Dir),
    {Pid, Comm, G, tupled_graph(G), DictTrace}.



cdd(Expr,Timeout,Strategy,Priority) ->
    {_,Res} = 
        cdd_internal_core(
            Expr, 
            Timeout, 
            fun(X) -> edd_lib:core_module(atom_to_list(X)++".erl") end,
            none),
    edd_con_lib:ask(Res,Strategy,Priority),

    % Graph = true,
    % {{Trace, DictFun, PidCall},_} = 
    %     cdd_internal_core(
    %         Expr, 
    %         Timeout, 
    %         fun(X) -> edd_lib:core_module(atom_to_list(X)++".erl") end,
    %         none),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	%%%% REMOVE COMMENTS TO ENABLE DEBUGGING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% edd_graph!new,
	% build_graph_old(Trace, DictFun, PidCall), 
	% edd_graph!{get,self()},
	% receive 
	% 	G -> ok
	% end,
	% %io:format("G: ~p\n",[G]),
	% case Graph of
	%      true ->
	%        edd_con_lib:dot_graph_file(G,"dbg");
	%      false -> 
	%        ok
	% end,
 % 	edd_con_lib:ask(G,Strategy,Priority),

 	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	%%%% REMOVE COMMENTS TO ENABLE DEBUGGING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % PidG!stop,
    edd_graph!stop,
    unregister(edd_graph),
    ok.

cdd_internal_core(Expr, Timeout, FunCore, Dir) ->
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
    % io:format("TRACE: ~p\n", [Trace]),
    % Traces = dict_keys_to_str(Traces0),

    PidG = spawn(fun() -> digraph_server() end),
    try 
        unregister(edd_graph)
    catch 
        _:_ -> ok 
    end,
    register(edd_graph, PidG),
    PidInfo_Comms_GQA_DictTrace = build_graph(Trace, DictFun, PidCall),
    {{Trace, DictFun, PidCall}, PidInfo_Comms_GQA_DictTrace}.

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

-record(snapshot_info,
	{
		top = none,
		rest = none
	}).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph building functions
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
  	FinalState2 = 
  		add_result_info(FinalState1),
    FinalState = 
        FinalState2#evaltree_state{
            pids_info = lists:sort(FinalState2#evaltree_state.pids_info)
        },


  	{_, DictPids} = 
  		lists:foldl(
  			fun assign_node_num/2, 
  				{0, dict:new()}, 
  				FinalState#evaltree_state.pids_info),


  	PidsTree = digraph:new(),
    % io:format("~p\n", [FinalState#evaltree_state.pids_info]),
  	[build_pids_tree(PidInfo, DictPids, DictFuns, PidsTree) 
  	 || PidInfo <- lists:reverse(lists:sort(FinalState#evaltree_state.pids_info))],


   %  io:format("~p\n", [dict:to_list(DictPids)]),
  	% io:format("~p\n", [FinalState]),


	dot_graph_file_int(PidsTree, "pids_tree", fun dot_vertex_pids_tree/1, false),

	
	communication_sequence_diagram(
		FinalState#evaltree_state.pids_info,
		FinalState#evaltree_state.communication),


	TimeStartEvalTree = os:timestamp(),
	PidsSummary = 
        summarizes_pidinfo(
            lists:reverse(FinalState#evaltree_state.pids_info)),

    edd_graph!{add_vertex, 0, {PidsSummary, lists:reverse(FinalState#evaltree_state.communication)}},
    {_,DictNodes} = lists:foldl(fun build_eval_tree/2, {1, dict:new()} , lists:reverse(FinalState#evaltree_state.pids_info)),
    % io:format("~p\n", [dict:to_list(DictNodes)]), 
    edd_graph!{get,self()},
	receive 
		G -> ok
	end,
    {DictQuestions, DictTrace} = build_questions(G, DictNodes),
    % io:format("~p\n", [lists:sort(dict:to_list(DictTrace))]), 

    % io:format("~p\n", [FinalState#evaltree_state.communication]),
    % DictQuestions = [],
	% io:format("G: ~p\n", [G]),
	TimeTotalEvalTree = 
		timer:now_diff(os:timestamp(), TimeStartEvalTree), %/1000000
	% io:format("Time to create evaluation tree: ~p microseconds\n", [TimeTotalEvalTree]),
	put(eval_tree_time, TimeTotalEvalTree),
    dot_graph_file_int(G, "eval_tree", fun(V) -> dot_vertex_eval_tree(V, DictQuestions) end, true),
    % edd_graph!del_disconected_vertices,
    {lists:reverse(FinalState#evaltree_state.pids_info), FinalState#evaltree_state.communication, {G, DictQuestions}, DictTrace}.


build_graph_trace(
		{TraceId, {edd_trace, start_call, Pid, {Call, Context, PosAndPP}}}, 
		State = #evaltree_state{pids_info = PidsInfo}) ->
	% Store the first call. Only for first process, rest of process will use spawn to fill thi info
    % io:format("~p\n", [{Call, Context, PosAndPP}]),
    % {pos_info,{merge_con,"merge_con.erl",65,
    %                   "(N, [H | T]) -> [H | take(N - 1, T)]"}}
	NPidsInfo0 = 
		case PidsInfo of 
			[PidInfo = #pid_info{pid = Pid, first_call = none}] ->
				[PidInfo#pid_info{first_call = #call_info{call = Call, pos_pp = PosAndPP}}]; 
			_ -> 
				PidsInfo
		end,
	% Add call to the process stack
	NPidsInfo = 
		lists:map(
			fun(PI) -> add_new_callrec_stack(PI, {Pid, #call_info{call = Call, pos_pp = PosAndPP}, Context}, TraceId) end,
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
	% io:format("BGT: ~p\n", [{Pid, PidReceive, Msg, TraceId}]),
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
			{sent, #message_info{from = PidSenderCom, to = PidReceiverCom, msg = MsgCom, trace = TraceIdSent}} <- Communication,
			MsgCom == Msg, PidReceiverCom == Pid, TraceIdSent < TraceId],
	% io:format("{Msg, Pid}: ~p\n", [{Msg, Pid}]),
	{NCommunication, NPidsInfo} = 
		case lists:usort(MsgSender) of 
			% Only if a sender for the consumed message is found
			[MsgSender_|_] ->
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

summarizes_pidinfo(PidsInfo) -> 
    [ {Pid, Call, Sent, Spawned, Result}
        || #pid_info{
                pid = Pid,
                first_call = Call,
                sent = Sent,
                spawned = Spawned,
                result = Result
           } <- PidsInfo].


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
				edd_con_lib:build_call_string(FunCall);
			{AnoFun} -> 
				PosInfo = {pos_info,{Mod,_,_,_}} = hd(dict:fetch(AnoFun, DictFuns)),
				edd_con_lib:build_call_string({Mod, PosInfo, []})
		end,
	Label = 
		lists:flatten(io_lib:format("~p\n~s", [Pid, StrFun])),
	digraph:add_vertex(PidsTree, V, Label),
    % io:format("Edges: ~p\n", [digraph:no_edges(PidsTree)]),
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

dot_graph_file_int(G, Name, FunVertex, ShowInfo) ->
	DotContent = 
		"digraph PDG {\n"++dot_graph(G, FunVertex)++"}",
	case ShowInfo of 
		true -> 
			% io:format("Memory size: ~p bytes\n", [length(DotContent)]),
			% io:format("Nodes: ~p\n", [length(digraph:vertices(G))]);
			put(eval_tree_memory, length(DotContent)),
			put(eval_tree_nodes, length(digraph:vertices(G)));
		false -> 
			ok
	end,
	file:write_file(Name++".dot", list_to_binary(DotContent)),
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
	% lists:foldl(
	% 	fun(Line,Acc) -> Acc ++ Line ++ "\n" end 
	% 	, "", Lines).
    string:join(Lines, "\n").

quote_enclosing(Str) ->
	"\"" ++ Str ++ "\"".

str_term(Pid) ->
	lists:flatten(io_lib:format("~p", [Pid]) ).

build_pid_line(Pid, Call, Code, NumPoints) ->
	Steps = 
		lists:foldl(
			fun(Num, Acc) -> Acc ++ " -> " ++ quote_enclosing(Pid ++ integer_to_list(Num)) end
			, "", lists:seq(1, NumPoints)), 
	lines(
		["{" 
		,"  rank=\"same\";" 
		,"  " ++ quote_enclosing(Pid) ++ "[shape=\"plaintext\", label=" ++ quote_enclosing(Pid ++"\n" ++ edd_con_lib:build_call_string(Call) ) ++ "]; "
		,"  " ++ quote_enclosing(Pid) ++ Steps ++ ";"
        ,"  " ++ quote_enclosing(Pid ++ "code") ++ "[shape=\"plaintext\", label=" ++ quote_enclosing(integer_to_list(Code)) ++ ", fontcolor = \"grey\", fontsize = 20];"
        ,"  " ++ quote_enclosing(Pid ++ integer_to_list(NumPoints)) ++ " -> " ++  quote_enclosing(Pid ++ "code") ++ " [penwidth = 1, style = dotted, color = grey];"
		,"} "]).

dot_spaces() ->
    "           ".

build_code_line(NumPoints, JoinPid) ->
    Steps = 
        lists:foldl(
            fun(Num, Acc) -> Acc ++ " -> " ++ quote_enclosing("code" ++ integer_to_list(Num)) end
            , "", lists:seq(1, NumPoints)), 
    Nodes = 
        lists:foldl(
            fun(Num, Acc) -> [quote_enclosing("code" ++ integer_to_list(Num)) ++ "[style=invis]; "| Acc] end
            , [], lists:reverse(lists:seq(1, NumPoints))),
    Link = 
        lists:foldl(
            fun(Num, Acc) -> 
                ["  " 
                ++ quote_enclosing("code" ++ integer_to_list(Num))
                ++ " -> " 
                ++ quote_enclosing(JoinPid ++ integer_to_list(Num))
                ++ " [taillabel="
                ++ quote_enclosing(dot_spaces() ++ integer_to_list(Num))
                ++ ", penwidth = 1, style = dotted, color = grey, labelfontsize = 20, labelfontcolor = grey,labelangle=0, labeldistance=0];"
                | Acc] end
            , [], lists:reverse(lists:seq(1, NumPoints))),
    lines(
        ["{" 
        ,"  rank=\"same\";"
        , "  " ++ quote_enclosing("code") ++ "[shape=\"plaintext\", label=" ++ quote_enclosing(dot_spaces() ++ "Code") ++ ", fontcolor = grey, fontsize = 20]; "]
        ++ Nodes 
        ++ ["  "++ quote_enclosing("code") ++ Steps ++ "[style=invis];"
        ,"} "]
        ++ Link
        ).


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
	quote_enclosing(Pid) ++ "-> code [style=invis];\n";
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
	% io:format("Before: ~p\n", [Before]),
	% io:format("After: ~p\n", [After]),
	% io:format("str_term(To): ~p\n", [str_term(To)]),
	% try io:format("str_term(To): ~p\n", [whereis(To)]) catch _:_ -> ok end,
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
	%  TODO: Should be  the same CurrentStep for both. 
	ReceiveEdge = 
		[	
			quote_enclosing(str_term(To) ++ integer_to_list(CurrentStep)) 
		++ 	" -> " 
		++ 	quote_enclosing(str_term(To) ++ integer_to_list(CurrentStep + 1)) 
		++  " [label=" ++ quote_enclosing(Label) ++ "];"],
	%  TODO: Related to previous TODO. Should only be incremented by one.
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
	TimeStart = 
		os:timestamp(),
	Header = 
		["digraph G {"
		,"  rankdir=\"LR\";"
		,"  node[shape=\"point\"];"
		,"  edge[arrowhead=\"none\"]"],
	LengthReceives = 
		length([0 || {received,_} <- Communications]),
	NumPoints = 
		(length(Communications) - LengthReceives)
		+ (2 * LengthReceives),
	{PidsStr, PidsCall} = 
		lists:unzip(
            % lists:sort(
                [{str_term(Pid), FirsCall} 
                || #pid_info{
                        pid = Pid, 
                        first_call = #call_info{call = FirsCall}
                    } <- PidsInfo
                ]
            % )
        ),
	PidLines = 
		lists:map(
			fun({Code, Pid, Call}) -> build_pid_line(Pid, Call, Code, NumPoints) end,
			lists:zip3(lists:seq(NumPoints + 1, NumPoints + length(PidsStr)), PidsStr, PidsCall) ),
    CodeLines = [build_code_line(NumPoints, lists:last(PidsStr))],
	Distribution = distribute_edge(PidsStr), 
	% io:format("lists:reverse(Communications):\n~p\n", [lists:reverse(Communications)]),
	{CommLines0,_} = 
		lists:mapfoldl(
			fun(Com, CurrentStep) -> communication_lines(Com, PidsStr, CurrentStep) end,
			1,
			lists:reverse(Communications)),
	CommLines = lists:concat(CommLines0),

	Closer = "}",
	DotContent = 
		lines(Header ++ PidLines ++ CodeLines ++ [Distribution] ++ CommLines ++ [Closer]),
	TimeTotal = 
		timer:now_diff(os:timestamp(), TimeStart), %/1000000
	% io:format("Time to create sequence diagram: ~p microseconds\n", [TimeTotal]),	
	% io:format("Memory size: ~p bytes\n", [length(DotContent)]),
	% io:format("Events: ~p\n", [NumPoints]),
	% io:format("Events + last: ~p\n", [NumPoints + length(PidsStr)]),
	put(seq_diag_time, TimeTotal),
	put(seq_diag_memory, length(DotContent)),
	put(seq_diag_events, NumPoints),
	put(seq_diag_events_lasts, NumPoints + length(PidsStr)),
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



build_answer(Str, Beh) ->
    #answer{
        text = Str,
        when_chosen = Beh,
        complexity = 1
    }.

build_answer(Str, Beh, Comp) ->
    (build_answer(Str, Beh))#answer{
        complexity = Comp
    }.

list_ans(#answer{text = Text}) ->
    Text ++ "\n".

pp_item(#message_info{from = From, to = To, msg = Msg}) ->
    edd_con_lib:format("~p (from ~p to ~p)", [Msg, From, To]);
pp_item(#spawn_info{spawner = Spawner, spawned = Spawned}) ->
    edd_con_lib:format("~p", [Spawned]);
pp_item({Var, Value}) ->
    edd_con_lib:format("~p = ~p", [Var, Value]);
pp_item(Other) ->
    edd_con_lib:format("~p", [Other]).

prev_recieve_answer(PrevRec, DictNodes, G) ->
    % io:format("\nPREV: ~p\n", [PrevRec]),
    case dict:find(PrevRec, DictNodes) of 
        {ok, NodeDests} ->
            NodeDest = lists:last(NodeDests),
            {NodeDest, NodeInfo} = digraph:vertex(G, NodeDest),
            {_,CallStackItem} =
            	NodeInfo,
            {{pos_info,{_, _, _, StrPrevReceiveWithoutLine}}} =
            	(CallStackItem#callrec_stack_item.origin_callrec)#receive_info.pos_pp,
            {#question{
                str_callrec = StrPrevReceive,
                answers = AnsPrevReceive
            }, _} = 
                build_question(NodeInfo, DictNodes, G, NodeDest, dict:new()),
        	CleanAnsPrevReceive = 
	        	lists:foldl(
	                fun(_, Acc) -> lists:droplast(Acc) end,
	                AnsPrevReceive,
	                lists:seq(1,4)),
            PreRecInfoStr = 
                StrPrevReceive ++ "\n" 
                ++ lists:flatten(
                        lists:map(
                            fun list_ans/1,
                            CleanAnsPrevReceive)),
                            % lists:droplast(AnsPrevReceive))),
            Complexity = 
            	complexity_receive(
            		StrPrevReceiveWithoutLine, 
            		CleanAnsPrevReceive),
            % io:format("Complexity: ~p\n", [Complexity]),
            [build_answer(
                "Previous evaluated receive:\n" 
                ++ edd_con_lib:tab_lines(PreRecInfoStr), 
                {correct, {goto, NodeDest}},
                Complexity)];       
        _ ->
            []   
    end.

behavior_question(SentSpawned, DictNodes, Node, BehEmptySame, BehOther) ->
    case SentSpawned of 
        [] ->
            BehEmptySame;
        _ ->
        	% The complexity of all these answers is put to 0 since they have been questioned previously.
            #question{
                text = "Which one is not expected?",
                answers = 
                [ 
                    begin
                        % io:format("Res: ~p\n", [dict:find(MS, DictNodes)]),
                        % io:format("pp_item(MS): ~p\n", [MS]),
                        case dict:find(MS, DictNodes) of 
                            {ok, [Node]} -> 
                                % build_answer(pp_item(MS), BehEmptySame, complexity_term(MS));
                                build_answer(pp_item(MS), BehEmptySame, 0); 
                            {ok, [NodeDest]} ->
                                % build_answer(pp_item(MS), {BehOther, {goto, NodeDest}}, complexity_term(MS));
                                build_answer(pp_item(MS), {BehOther, {goto, NodeDest}}, 0);
                            error ->
                                % TODO: Not sure whether this is the expected behavior
                                % build_answer(pp_item(MS), BehEmptySame, complexity_term(MS))
                                build_answer(pp_item(MS), BehEmptySame, 0)
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
    [build_answer(
    	"Reached receive:\n" ++ PrevRec, 
    	incorrect,
    	complexity_receive(PrevRec))];
reached_value_answer(stuck_receive, _) ->
    [build_answer(
    	"Blocked because it is waiting for a message", 
    	incorrect)];
reached_value_answer(Val, _) ->
    [build_answer(
    	"Evaluated to value: " ++ edd_con_lib:any2str(Val), 
    	incorrect, 
    	complexity_term(Val) + 1)].

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
    %     lists:foldl(fun(A_, Acc) -> Acc ++ edd_con_lib:any2str(A_) ++ ", " end, "", A), 
    % StrList = 
    %     case StrList0 of 
    %         "" ->
    %             "";
    %         _ ->
    %             lists:droplast(lists:droplast(StrList0))
    %     end,
    % io:format("~s\n", [StrList]),         
    CallStr = 
        edd_con_lib:format(
                "~p:~p(~s)", 
                [M,F,string:join(lists:map(fun edd_con_lib:any2str/1, A), ", ") ]
            ),
    Question = 
        "Process " ++ edd_con_lib:any2str(Pid) ++ " called " 
        ++ CallStr ++ ".\nWhat is wrong?",
    % io:format(Question),
    PrevReceive = 
        prev_recieve_answer(PrevRec, DictNodes, G),
    Answers = 
        PrevReceive ++ 
        reached_value_answer(Result, PrevRec) ++ 
        [
         build_answer(
         	edd_con_lib:question_list("sent messages",Sent), 
         	behavior_question(Sent, DictNodes, Node, incorrect, correct),
         	complexity_term(Sent)),
         build_answer(
         	edd_con_lib:question_list("created processes",Spawned), 
         	behavior_question(Spawned, DictNodes, Node, incorrect, correct),
         	complexity_term(Spawned)),
         build_answer("Nothing", correct)
        ],
    % Question ++ "\n" ++ edd_con_lib:any2str(Answers);
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
        edd_con_lib:format("~s\nin ~s:~p", [ReceiveStr0, F, L]),
    Question = 
        "Process " ++ edd_con_lib:any2str(Pid) ++ " evaluated\n" 
        ++ ReceiveStr ++ "\nWhat is wrong?",
    PrevReceive = 
        prev_recieve_answer(PrevRec, DictNodes, G),
    Answers = 
        PrevReceive ++
        [
         build_answer(
         	edd_con_lib:question_list("Context",Context), 
         	correct, 
         	complexity_term(Context) - length(Context)),
         % build_answer("ContextRec: " ++ edd_con_lib:any2str(ContextRec), correct),
         build_answer(
         	edd_con_lib:question_list("received messages",Received),
         	behavior_question(Received, DictNodes, Node, correct, correct),
         	complexity_term(Received)),
         build_answer(
         	edd_con_lib:question_list("consumed message",Consumed), 
         	incorrect,
         	complexity_term(Consumed))
        ]
        ++ reached_value_answer(Result, PrevRec) ++
        [
         build_answer(
         	edd_con_lib:question_list("sent messages",Sent), 
         	behavior_question(Sent, DictNodes, Node, incorrect, correct),
         	complexity_term(Sent)),
         build_answer(
         	edd_con_lib:question_list("created processes",Spawned), 
         	behavior_question(Spawned, DictNodes, Node, incorrect, correct),
         	complexity_term(Spawned)),
         build_answer("Nothing", correct)
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
        io_lib:format("~p. - ~s (behavior: ~p)\n", [Opt, Text, Beh]),
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
            dict:append(TraceId, V, CurrDict)
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

complexity_term(#message_info{msg = Msg}) ->
	2 + complexity_term(Msg);
complexity_term(#spawn_info{}) ->
	1;
complexity_term(Term) ->
	case io_lib:printable_list(Term) of % is_string/1 Erlang's way
		true -> 
			1;
		false -> 
			case is_list(Term) of 
				true -> 
					1 + lists:sum(
						lists:map(
							fun complexity_term/1, 
							Term));
				false -> 
					case is_tuple(Term) of 
						true -> 
							1 + lists:sum(
								lists:map(
									fun complexity_term/1, 
									tuple_to_list(Term)));
						false -> 
							1
					end
			end
	end.

complexity_receive(StrReceive, AnsReceive) ->
	% io:format("StrReceive: ~s\n", [StrReceive]),
	% io:format("AnsReceive: ~p\n", [AnsReceive]),
	{ok, Toks, _} = 
		erl_scan:string(StrReceive ++ "."),
	{ok, [AExpr | _]} = 
		erl_parse:parse_exprs(Toks),
	Clauses = 
		erl_syntax:receive_expr_clauses(AExpr),
	ClausesPatternsComplexities = 
		lists:map(
			fun(Clause) ->
				[Pattern] = 
					erl_syntax:clause_patterns(Clause),
				PatternWithoutVars = 
					erl_syntax_lib:map(
						fun(N) ->
							case erl_syntax:type(N) of 
								variable -> 
									erl_syntax:integer(1);
								_ ->
									N 
							end
						end,
						Pattern),
				complexity_term(erl_syntax:concrete(PatternWithoutVars))
			end,
			Clauses),
	ComplexityAfter = 
		case erl_syntax:receive_expr_timeout(AExpr) of 
			none -> 
				0;
			_ -> 
				2 % 1 because the after clause, and 1 because the term defining the timeout
		end,
	ComplexityAns = 
		[Ans#answer.complexity || Ans <- AnsReceive],
	% io:format("Clauses: ~p\n", [length(Clauses) ]),
	% io:format("ClausesPatternsComplexities: ~p\n", [lists:sum(ClausesPatternsComplexities) ]),
	% io:format("ComplexityAfter: ~p\n", [ComplexityAfter]),
	% io:format("ComplexityAns: ~p\n", [lists:sum(ComplexityAns)]),
		length(Clauses) 
	+ 	lists:sum(ClausesPatternsComplexities) 
	+	ComplexityAfter
	+ 	lists:sum(ComplexityAns)
	+ 	1.

complexity_receive(StrReceive) ->
	ReversedStrReceive = 
		lists:reverse(StrReceive),
	{true, CleanedStrReceive} = 
		lists:foldl(
			fun
				(Char, {true, Acc})->
					{true, [Char | Acc]};
				($\n, {false, []}) ->
					{true, []};
				(_ , {false, []}) ->
					{false, []}
			end,
			{false, []},
			ReversedStrReceive),
	% io:format("CleanedStrReceive: ~s\n", [CleanedStrReceive]),
	complexity_receive(CleanedStrReceive, []).





