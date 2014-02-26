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


-record(scheduler_state, 
	{mnf, funs_initial_call, case_id, cases, traces, 
	 free_v, g, main_pid, first_call, first_pid, 
	 main_pid_answer, main_result, pid_info,
	 iteration}).

-record(tree_state, 
	{expr, env, core, trusted, scheduler_pid, 
	 pid_trace, parent}).

-record(pid_state, 
	{trace, pid_continue = 0, stack = [], finished_stack = [],
	 sent = [], receipt = [], spawned = [], 
	 temp_sent = [], temp_spawned = [],
	 transition = {}, values_transition = dict:new(), temp_receipt = [],
	 first_call = none,
	 current_stack_id = 1}).

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
	{{first_pid,FirstPid},{traces,Traces}} = 
		edd_trace:trace(Expr,Timeout),
	%TO CHANGE
	%io:format("FunsInitialCall: ~p\n",[FunsInitialCall]),
	Self = self(),
	%io:format("Traces: ~p\n",[dict:to_list(Traces)]),
	% put(funs_initial_call,FunsInitialCall),
	% put(case_id,0),
	% put(traces,Traces),
	%
	PidG = spawn(fun() -> digraph_server() end),
	PidG!{add_vertex,0},
	Env = ets:new(env,[bag,public]),
	FirstTreeState = 
		#tree_state{
			expr = InitialCall,
			env = Env,
			core = Core,
			trusted = false,
			scheduler_pid = 0, %It will be set inside the scheduler
			pid_trace = FirstPid,
			parent = 0
		},
	TraceFirst = 
		case dict:find(FirstPid,Traces) of 
			error -> [];
			{ok, TraceFirst_} -> TraceFirst_
		end,
	FirstState = 
		#scheduler_state{
			mnf=[FirstTreeState],
			funs_initial_call = FunsInitialCall,
			case_id = 0,
			cases = [],
			traces = Traces,
			free_v = 1,
			g = PidG,
			main_pid = Self,
			first_call = true,
			first_pid = FirstPid,
			main_pid_answer = 0,
			main_result = [],
			pid_info = dict:store(FirstPid, #pid_state{trace = TraceFirst}, dict:new()),
			iteration = 0
		},
	PidScheduler = 
		spawn(
			fun() -> 
				scheduler(FirstState)
				%scheduler([{mnf,InitialCall,FirstPid,1,Env,Core,[],[]}],FunsInitialCall,0,[],Traces,0,PidG,Self,true,0,[],[{FirstPid,1}]) 
			end),
	% {Value,FreeV,Roots} = 
	%   get_whole_tree([{mnf,InitialCall,FirstPid,1,[],[]}],ets:new(env,[bag]),G,Core,0,false),
	  %get_tree(InitialCall,ets:new(env,[bag]),G,Core,0,false),
	PidScheduler!next_mnf,
	%io:format("antes result\n"),
	receive 
		{result,Value,Summary} ->
			ok
	end,
	%io:format("despues result\n"),
	%Caso especial si la llamada inicial tiene funciones anidadas como f(g(3)), que
	%será traducido a CORE con un let x = g(3) in f(x)
	%io:format("InitialCall: ~p\n",[InitialCall]),
	%io:format("FreeV: ~p\n",[FreeV]),
 	%io:format("Roots: ~w\n",[Roots]),
	FunAddVertexAndEdges = 
		fun(Call) ->
			case cerl:concrete(cerl:call_module(Call)) of
 	     	     'erlang' -> 
 	     	     	ok;
 	     	     _ ->
 	     	     	PrintValue = 
	 	     	     	case Value of 
	 	     	     		stuck_receive -> 
	 	     	     			Value;
	 	     	     		_ -> 
	 	     	     			io_lib:format("~p",[cerl:concrete(Value)])
	 	     	     	end,
	 	     	     %io:format("Summary: ~p\n",[Summary]),
	 	     	     PrettySummary = 
						[{PidSummary,erl_prettypr:format(CallSum,[{paper, 300},{ribbon, 300}]) ,Spawned,
						 	[{PidSent, erl_prettypr:format(get_abstract_form(Msg,none),[{paper, 300},{ribbon, 300}])} 
						 	 || {PidSent,Msg} <- Sent]}
				 		 || {PidSummary,CallSum,Spawned,Sent} <- Summary],
				 	%io:format("PrettySummary: ~p\n",[PrettySummary]),
 	     	     	PidG!{add_vertex,0,{root,Expr,PrintValue,PrettySummary,FirstPid}}
	 	     	                           %get_from_scheduler(PidScheduler,{get_case_id,0,self()}),none,1}}
 	     	       	% digraph:add_vertex(G,FreeV,
	 	     	       %                    {Expr ++ " = "
	 	     	       %                     ++ io_lib:format("~p",[cerl:concrete(Value)]),
	 	     	       %                     get(0),none,1}),
					% [digraph:add_edge(G,FreeV,Root) || Root <- Roots]
					%[PidG!{add_edge,FreeV,Root} || Root <- Roots]
 	     	end
 	     end,
	case cerl:type(InitialCall) of 
	     'let' ->
	     	case cerl:type(cerl:let_arg(InitialCall)) of
	     	     'call' ->
	     	     	FunAddVertexAndEdges(cerl:let_arg(InitialCall));
	     	     _ -> ok
	     	end;
	     _ -> 
	     	%ok
	     	FunAddVertexAndEdges(InitialCall)
	end,
	PidG!{get,self()},
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
    %edd_lib:ask(G,{top_down,old}),
    %edd_lib:ask(G,{top_down,new}),
    PidG!stop,
    %ets:delete(Env),
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
			Pid!G,
			digraph_server(G);	
		stop ->
			ok
	end. 

get_from_scheduler(PidScheduler,Msg) ->
	PidScheduler!Msg,
	%io:format("Sent: ~p\n",[Msg]),
	receive 
		Answer -> 
			%io:format("Received: ~p\n",[Answer]),
			Answer
	end.

% scheduler(State) ->
% 	scheduler(
% 		State#scheduler_state.mnf,
% 		State#scheduler_state.funs_initial_call,
% 		State#scheduler_state.case_id,
% 		State#scheduler_state.cases,
% 		State#scheduler_state.traces,
% 		State#scheduler_state.free_v,
% 		State#scheduler_state.g,
% 		State#scheduler_state.main_pid,
% 		State#scheduler_state.first_call,
% 		State#scheduler_state.main_pid_answer,
% 		State#scheduler_state.main_result,
% 		State#scheduler_state.pid_info
% 	).


% -record(tree_state, 
% 	{expr, env, core, trusted, scheduler_pid, 
% 	 pid_trace, parent}).

scheduler(State0) ->
	%io:format("State: ~p\n",[State]),
	%io:format("State: ~p\n",[dict:to_list(State#scheduler_state.pid_info)]),
	State = State0#scheduler_state{iteration = State0#scheduler_state.iteration + 1},
	%io:format("Scheduler iteration: ~p\n",[State#scheduler_state.iteration]),
	receive 
		next_mnf -> 
			%io:format("antes1\n"),
			TreeState = hd(State#scheduler_state.mnf),
			%io:format("Pasa control a ~p\n",[TreeState#tree_state.pid_trace]),
			case cerl:type(TreeState#tree_state.expr) of 
				'receive' ->
					Pid_info = dict:fetch(TreeState#tree_state.pid_trace, State#scheduler_state.pid_info),
					%io:format("Trace: ~p\n",[Pid_info#pid_state.trace]),
					%io:format("Trace: ~p\n",[Pid_info#pid_state.pid_continue]),
					%io:format("Trace: ~p\n",[Pid_info#pid_state.trace]),
					%io:format("mnf: ~p\n",[State#scheduler_state.mnf]),
					Pid_info#pid_state.pid_continue!continue,
					scheduler(State#scheduler_state{
						mnf = tl(State#scheduler_state.mnf)});
				_ ->
					Self = self(),
					PidSpawn = spawn(fun() -> get_tree(TreeState#tree_state{scheduler_pid = Self},Self) end),
					NMainPidAnswer = 
						case State#scheduler_state.first_call of 
							true ->
								PidSpawn;
							false ->
								State#scheduler_state.main_pid_answer
						end,
					scheduler(State#scheduler_state{
						mnf = tl(State#scheduler_state.mnf), 
						first_call = false,
						main_pid_answer = NMainPidAnswer})
			end;
		{add_spawn,MNF_item = TreeState,PidWhoSpawn, NTracePidWhoSpawn} ->
			%io:format("antes2\n"),
			Pid_Info_WhoSpawn = 
				dict:fetch(PidWhoSpawn, State#scheduler_state.pid_info),
			Stack = Pid_Info_WhoSpawn#pid_state.stack,
			ParentStack = 
				case Stack of
					[_|_] ->
						[Pid || {Pid,_,_} <- Stack];
					[] ->
						none
				end,
			NPidInfo = 
				dict:store(
					PidWhoSpawn,
					Pid_Info_WhoSpawn#pid_state{
						trace = NTracePidWhoSpawn,
						temp_spawned = Pid_Info_WhoSpawn#pid_state.temp_spawned ++ [{TreeState#tree_state.pid_trace,ParentStack}]},
					State#scheduler_state.pid_info),
			Pid_trace_start_trace = 
				case dict:find(TreeState#tree_state.pid_trace,State#scheduler_state.traces) of 
					error -> [];
					{ok,Pid_trace_start_trace_} -> Pid_trace_start_trace_
				end,
			scheduler(State#scheduler_state{
				mnf = State#scheduler_state.mnf ++ [MNF_item], 
				pid_info = dict:store(
						TreeState#tree_state.pid_trace,
						#pid_state{trace = Pid_trace_start_trace},
						NPidInfo)});
		{add_receive,MNF_item = TreeState,PidContinue} ->
			%io:format("antes3\n"),
			Pid_info = 
				dict:fetch(TreeState#tree_state.pid_trace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{
				mnf = State#scheduler_state.mnf ++ [MNF_item], 
				pid_info = dict:store(
					TreeState#tree_state.pid_trace, 
					Pid_info#pid_state{pid_continue = PidContinue}, 
					State#scheduler_state.pid_info)});
		{set_trace,PidTrace,NCurrentTrace} ->
			%io:format("antes4\n"),
			Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{ 
				pid_info = dict:store(
						PidTrace,
						Pid_info#pid_state{trace = NCurrentTrace},
						State#scheduler_state.pid_info)});
		{get_trace,PidTrace,PidAnswer} ->
			%io:format("antes5\n"),
     		PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.trace,
     		scheduler(State);
     	{set_receipt,PidTrace,NReceipt} ->
     		%io:format("antes6\n"),
			Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{ 
				pid_info = dict:store(
						PidTrace,
						Pid_info#pid_state{receipt = NReceipt},
						State#scheduler_state.pid_info)});
		{get_receipt,PidTrace,PidAnswer} ->
			%io:format("antes7\n"),
			PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.receipt,
     		scheduler(State);
     	{add_message,From,To,Msg} ->
     		%io:format("antes8\n"),
     		Pid_info_from = 
				dict:fetch(From, State#scheduler_state.pid_info),
			Stack = 
				Pid_info_from#pid_state.stack,
			ParentStack = 
				case Stack of
					[_|_] ->
						[Pid || {Pid,_,_} <- Stack];
					[] ->
						none
				end,
			Pid_info_to = 
				dict:fetch(To, State#scheduler_state.pid_info),
			NTrace = 
				case get_next_sent(Pid_info_from#pid_state.trace,[]) of 
					not_found -> Pid_info_from#pid_state.trace;
					NTrace_ -> NTrace_
				end,
			NPidInfo1 = 
				case From of 
					To -> 
						dict:store(
							To,
							Pid_info_to#pid_state{
								receipt = Pid_info_to#pid_state.receipt ++ [{From,Msg}],
								temp_sent = Pid_info_from#pid_state.temp_sent ++ [{To,Msg,ParentStack}],
								trace = NTrace},
							State#scheduler_state.pid_info);
					_ ->
						NPidInfo0 = 
							dict:store(
									From,
									Pid_info_from#pid_state{
										temp_sent = Pid_info_from#pid_state.temp_sent ++ [{To,Msg,ParentStack}],
										trace = NTrace},
									State#scheduler_state.pid_info),
						dict:store(
								To,
								Pid_info_to#pid_state{receipt = Pid_info_to#pid_state.receipt ++ [{From,Msg}]},
								NPidInfo0)
				end,
			scheduler(State#scheduler_state{ 
				pid_info = NPidInfo1});
		% {get_traces_pid,PidTrace,PidAnswer} ->
		% 	case dict:find(PidTrace,State#scheduler_state.traces) of 
		% 		{ok,PidTraces} ->
		% 			PidAnswer!PidTraces;
		% 		error ->
		% 			throw({error,io_lib:format("Not existing pid ~p",PidTrace),PidTrace}),
		% 			PidAnswer![]
  %    		end,
  %    		scheduler(State);
		{get_calls_stack,PidTrace,PidAnswer} ->
     		%io:format("antes10\n"),
     		%Stack = (dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.stack,
     		%{NumberedStack,_} = lists:mapfoldl(fun(E,Id) -> {{Id,E}, Id + 1} end,1,lists:reverse(Stack)),
     		%PidAnswer!lists:reverse(NumberedStack),
     		PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.stack,
			scheduler(State);
		{get_finished_calls_stack,PidTrace,PidAnswer} ->
     		PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.finished_stack,
			scheduler(State);
		{substitute_stack,PidTrace,IdCall,New} ->
     		Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			Stack = Pid_info#pid_state.stack,
			{NStack,NFinishedStack} = 
				case [Elem || Elem = {IdCall_,_,_} <- Stack, IdCall_ == IdCall] of 
					[] ->
						{Stack,Pid_info#pid_state.finished_stack};
					[Old = {IdCall,ParentStack,_} |_] -> 
						{Stack -- [Old],
						 [{IdCall,ParentStack,New} | Pid_info#pid_state.finished_stack]}
				end,
			scheduler(State#scheduler_state{
				pid_info = dict:store(
						PidTrace,
						Pid_info#pid_state{
							finished_stack = NFinishedStack,
							stack = NStack},
						State#scheduler_state.pid_info)});
		{clean_stack,PidTrace} ->
     		Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			{_,NStack} = clean_stack(Pid_info#pid_state.stack),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_info#pid_state{
							finished_stack = [],
							stack = NStack},
						State#scheduler_state.pid_info)});
		{store_call,PidTrace, Call} ->
     		Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			NFirstCall = 
				case Pid_info#pid_state.first_call of 
					none -> 
						Call;
					Other ->
						Other 
				end,
			%io:format("Pid: ~p\nOld: ~p\nCandidate: ~p\nNew: ~p\n",[PidTrace,Pid_info#pid_state.first_call,Call,NFirstCall]),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_info#pid_state{
							first_call = NFirstCall},
						State#scheduler_state.pid_info)});
		{add_call_stack,PidTrace,Call,PidAnswer} ->
     		%io:format("antes9\n"),
     		Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			CurrentStackId = Pid_info#pid_state.current_stack_id,
			PidAnswer!CurrentStackId,
			ParentStack = 
				case Pid_info#pid_state.stack of
					[{Pid,_,_}|_] ->
						Pid;
					[] ->
						none
				end,
			scheduler(State#scheduler_state{ 
				pid_info = dict:store(
						PidTrace,
						Pid_info#pid_state{
							stack = [{CurrentStackId,ParentStack,Call} | Pid_info#pid_state.stack],
							current_stack_id = CurrentStackId + 1},
						State#scheduler_state.pid_info)});
		{set_transition,PidTrace,Receive,Clause,{Pid,Msg},NodeReceive} ->
			Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			CurrentStack = [Pid_ || {Pid_,_,_} <- Pid_info#pid_state.stack],
			% NMsg = 
			% 	case Msg of 
			% 		none -> 
			% 			Msg;
			% 		_ -> 
			% 			erl_prettypr:format(get_abstract_form(Msg,none),[{paper, 300},{ribbon, 300}])
			% 	end,
			%io:format("~p\n",[{Receive,Clause,NPidMsg}]),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_info#pid_state{
							transition = {{Receive,Clause,{Pid,Msg},NodeReceive},CurrentStack},
							values_transition = dict:store(NodeReceive,none,Pid_info#pid_state.values_transition)},
						State#scheduler_state.pid_info)});
		{get_transition,PidTrace,PidAnswer} ->
			Pid_info = dict:fetch(PidTrace, State#scheduler_state.pid_info),
				case Pid_info#pid_state.transition of 
					{{Receive,Clause,{Pid,Msg},NodeReceive},CurrentStack} ->
						%io:format("Dict: ~p\n",[dict:to_list(Pid_info#pid_state.values_transition)]),
						Value = dict:fetch(NodeReceive,Pid_info#pid_state.values_transition),
						PidAnswer!{{Receive,Clause,{Pid,Msg},NodeReceive,Value},CurrentStack};
					{} ->
						PidAnswer!{}
				end,
			scheduler(State);
		{value_transition,PidTrace,NodeReceive,Value} ->
			Pid_info = dict:fetch(PidTrace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_info#pid_state{
							values_transition = dict:store(NodeReceive,Value,Pid_info#pid_state.values_transition)},
						State#scheduler_state.pid_info)});
		{get_sent,PidTrace,PidAnswer} ->
			PrettySent = 
				[{Pid, erl_prettypr:format(get_abstract_form(Msg,none),[{paper, 300},{ribbon, 300}])}
				 || {Pid,Msg} <- (dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.sent],
			%io:format("PrettySent: ~p\n",[PrettySent]),
			PidAnswer!PrettySent,
			scheduler(State);
		{get_spawned,PidTrace,PidAnswer} ->
			PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.spawned,
			scheduler(State);
		{get_temp_sent,PidTrace,PidAnswer} ->
			PrettySent = 
				[{Pid, erl_prettypr:format(get_abstract_form(Msg,none),[{paper, 300},{ribbon, 300}]),IdCall}
				 || {Pid,Msg,IdCall} <- (dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.temp_sent],
			%io:format("PrettySent: ~p\n",[PrettySent]),
			PidAnswer!PrettySent,
			scheduler(State);
		{get_temp_spawned,PidTrace,PidAnswer} ->
			%io:format("spawned: ~p\n",[(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.temp_spawned]),
			PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.temp_spawned,
			scheduler(State);
		{store_receive,PidTrace,Node} ->
			Pid_Info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_Info#pid_state{
							temp_receipt = Pid_Info#pid_state.temp_receipt ++ [{Node,length(Pid_Info#pid_state.stack)}]},
						State#scheduler_state.pid_info)});
		{get_temp_receipt,PidTrace,PidAnswer} ->
			PidAnswer!(dict:fetch(PidTrace, State#scheduler_state.pid_info))#pid_state.temp_receipt,
			scheduler(State);
		{store_temp_data,PidTrace} ->
			Pid_info = 
				dict:fetch(PidTrace, State#scheduler_state.pid_info),
			scheduler(State#scheduler_state{
				pid_info = 
					dict:store(
						PidTrace,
						Pid_info#pid_state{
							sent = Pid_info#pid_state.sent ++ [{Pid,Msg} || {Pid,Msg,_} <- Pid_info#pid_state.temp_sent],
							spawned = Pid_info#pid_state.spawned ++ [Pid || {Pid,_} <- Pid_info#pid_state.temp_spawned],
							temp_spawned = [],
							temp_sent = [],
							temp_receipt = []},
						State#scheduler_state.pid_info)});
		{result, PidFrom, Result,Env} ->
			%io:format("rep result\n"),
			case {State#scheduler_state.main_pid_answer, State#scheduler_state.mnf} of 
				{PidFrom,[]} -> 
					%io:format("despues1\n"),
					%Clause = hd([Clause_ ||{0,Clause_} <- State#scheduler_state.cases]),
					%Sent = (dict:fetch(State#scheduler_state.first_pid, State#scheduler_state.pid_info))#pid_state.sent,
					%Spawned = (dict:fetch(State#scheduler_state.first_pid, State#scheduler_state.pid_info))#pid_state.spawned,
					catch ets:delete(Env),
					Summary = 
						[{Pid,PidInfo#pid_state.first_call,PidInfo#pid_state.spawned,PidInfo#pid_state.sent} 
						 || {Pid,PidInfo} <- dict:to_list(State#scheduler_state.pid_info)],
					State#scheduler_state.main_pid!{result,Result,Summary};
				{_,[]} -> 
					%io:format("despues2\n"),
					%Clause = hd([Clause_ ||{0,Clause_} <- State#scheduler_state.cases]),
					%Sent = (dict:fetch(State#scheduler_state.first_pid, State#scheduler_state.pid_info))#pid_state.sent,
					%Spawned = (dict:fetch(State#scheduler_state.first_pid, State#scheduler_state.pid_info))#pid_state.spawned,
					catch ets:delete(Env),
					Summary = 
						[{Pid,PidInfo#pid_state.first_call,PidInfo#pid_state.spawned,PidInfo#pid_state.sent} 
						 || {Pid,PidInfo} <- dict:to_list(State#scheduler_state.pid_info)],
					State#scheduler_state.main_pid!{result,State#scheduler_state.main_result,Summary};
				{PidFrom,_} -> 
					%io:format("despues3\n"),
					catch ets:delete(Env),
					self()!next_mnf,
					scheduler(State#scheduler_state{main_result = Result});
				{_,_} -> 
					%io:format("despues4\n"),
					catch ets:delete(Env),
					self()!next_mnf,
					scheduler(State)
			end;
		{get_case_id,PidAnswer} ->
			PidAnswer!State#scheduler_state.case_id,
			scheduler(State);
		{get_case_id,Case_Id_Wanted,PidAnswer} ->
			Clause = hd([Clause_ ||{Case_Id_,Clause_} <- State#scheduler_state.cases, Case_Id_Wanted == Case_Id_]),
			PidAnswer!Clause,
			scheduler(State);
		{set_case_id,Case_id,Clause} ->
			scheduler(State#scheduler_state{
				case_id = State#scheduler_state.case_id + 1, 
				cases = [{Case_id,Clause}|State#scheduler_state.cases]});
		{create_env,PidAnswer} ->
			PidAnswer!ets:new(env, [bag,public]),
			scheduler(State);
		{create_env,Name,PidAnswer} ->
			PidAnswer!ets:new(Name,[set,public]),
			scheduler(State);
		{get_funs_initial_call,PidAnswer} -> 
			PidAnswer!State#scheduler_state.funs_initial_call,
			scheduler(State);
		{get_free_v,PidAnswer} -> 
			PidAnswer!State#scheduler_state.free_v,
			scheduler(State);
		increment_free_v ->
			scheduler(State#scheduler_state{free_v = State#scheduler_state.free_v + 1});
		{get_graph,PidAnswer} ->
			PidAnswer!State#scheduler_state.g,
			scheduler(State);
		_Msg -> 
			io:format("Unknown message ~p\n",[_Msg])
	end.

% get_whole_tree([{mnf,Expr,Pid,CurrentTrace,Spawned,Sent} | Pending],Env,G,Core,FreeV,Trusted) ->
% 	get_tree(Expr,Env,G,Core,FreeV,Trusted).



get_tree(State) ->
	Self = self(),
	Pid = spawn(fun() -> get_tree(State,Self) end),
	receive 
		{result, Pid, Result,_} ->
			%io:format("Recibido ~p\n",[Result]),
			Result
	end.

get_tree(State,Pid) ->
	Expr = State#tree_state.expr,
	Env = State#tree_state.env,
	Core = State#tree_state.core,
	SchedulerPid = State#tree_state.scheduler_pid,
	PidTrace = State#tree_state.pid_trace,
	%io:format("Expr: ~p\n",[Expr]),
	%io:format("Env: ~p\n",[ets:tab2list(Env)]),
	Result = 
		case cerl:type(Expr) of
			'apply' ->
				get_tree_apply(State);
			'case' ->
				get_tree_case(State);
			'let' ->
				Vars = cerl:let_vars(Expr),
				LetArg = cerl:let_arg(Expr),
				ValueArg =
				   get_tree(State#tree_state{expr = LetArg}),
				%io:format("Vars: ~p\n",[Vars]),
				%io:format("ValueArg: ~p\n",[ValueArg]),
				case check_errors(ValueArg) of
				     true -> 
				     	ValueArg;
				     _ -> 
				     	case ValueArg of 
				     		{c_values,_,ValuesArg} ->
				     			VarsValues = lists:zip(Vars,ValuesArg),
				     			add_bindings_to_env(VarsValues,Env);
				     		_ -> 
								lists:map(fun(Var) -> add_bindings_to_env([{Var,ValueArg}],Env) end,Vars)
						end,
						%io:format("Env: ~p\n",[ets:tab2list(Env)]),
						LetBody = cerl:let_body(Expr),
						get_tree(State#tree_state{expr = LetBody})
				% Se devuelven las raices de los árboles de cómputo de cada expresión
				% en el argumento del let (RootArgs) además de las raíces de los árboles
				% de cómputo del cuerpo (RootArgs)
				end;
			'letrec' ->
				NewDefs_ = cerl:letrec_defs(Expr),
				NewDefs = 
				   [{V,apply_substitution(FunDef,Env,[])} || {V,FunDef} <- NewDefs_],
				NCore =	cerl:c_module(cerl:module_name(Core), 
						 	cerl:module_exports(Core),
						 	cerl:module_attrs(Core),
						 	NewDefs ++ cerl:module_defs(Core)),
				get_tree(State#tree_state{expr = cerl:letrec_body(Expr), core = NCore});
				% Genero un nuevo CORE de un módulo que es igual a 'Core' pero que tiene
				% la función declarada en el letrec y genero el arbol del cuerpo del letrec
		    'fun' -> 
				[{id,{_,_,FunName}},Line,{file,File}] = cerl:get_ann(Expr),
				NExpr = apply_substitution(Expr,Env,[]), % Sustituye las variables libres
				% de la fun por sus valores
				% ets:insert(EnvAF,{FunName,{anonymous,NExpr,File,Line,Env}}),
				% % Guardo la función anónima en el entorno por si luego me aparece saber
				% % dónde estaba
				EnvFun = get_from_scheduler(SchedulerPid,{create_env,FunName,self()}),
				ets:insert(EnvFun,ets:tab2list(Env)),
				{anonymous_function,FunName,NExpr,File,Line,EnvFun};
			'call' ->
				case Expr of
				     {c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},_} ->
				     	Expr;
				     %self/1 needs special treatment
				     {c_call,_,{c_literal,_,erlang},{c_literal,_,'self'},[]} ->
				     	cerl:abstract(PidTrace);
				     %Send
				     {c_call,_,{c_literal,_,erlang},{c_literal,_,'!'},[ToPid,Msg]} ->
				     	VToPid = get_tree(State#tree_state{expr = ToPid}),
				     	VMsg = get_tree(State#tree_state{expr = Msg}),
				     	%io:format("PidTrace:~p\nVToPid: ~p\nVMsg: ~p\n",[PidTrace,VToPid,VMsg]),
				     	SchedulerPid!{add_message,PidTrace,cerl:concrete(VToPid),VMsg},
				     	%io:format("Sent: ~p\n",[get_from_scheduler(SchedulerPid,{get_sent,PidTrace,self()})]),
				     	VMsg;
				     %Spawn
				     {c_call,_,{c_literal,_,erlang},{c_literal,_,spawn},Args} ->
						SpawnCall = 
							case Args of 
								[Arg] ->
									cerl:ann_c_apply(cerl:get_ann(Expr),Arg,[]);
								_ ->
									[SpawnMod, SpawnFun | SpawnArgs] = Args,
									% {[SpawnMod,SpawnFun,SpawnArgs],NFreeV,Roots} = 
									% 	get_tree_list(Args,Env,G,Core,FreeV,Trusted,SchedulerPid,PidTrace),
									%io:format("SpawnArgs: ~p\n",[SpawnArgs]),
									NSpawnArgs = get_args_from_core_args(hd(SpawnArgs)),
									cerl:ann_c_call(cerl:get_ann(Expr),SpawnMod, SpawnFun, NSpawnArgs)
							end,
						%PidTraces = get_from_scheduler(SchedulerPid,{get_traces_pid,PidTrace,self()}),
						CurrentTrace = get_from_scheduler(SchedulerPid,{get_trace,PidTrace,self()}),
						%io:format("SpawnCall: ~p\nPidTrace: ~p\nCurrentTrace: ~p\n",[SpawnCall,PidTrace,CurrentTrace]),
						{SpawnPid, NCurrentTrace} = get_next_spawned(CurrentTrace,[]),
						case SpawnPid of 
							no_pid ->
								throw({error,io_lib:format("Pid ~p does not spawn more processes in the trace.",[Pid]),Expr}),
	     						SpawnCall;
	     					_ ->
								SchedulerPid!{add_spawn,State#tree_state{expr = SpawnCall, pid_trace = SpawnPid},PidTrace,NCurrentTrace},
								%io:format("SpawnPid: ~p\n",[cerl:abstract(SpawnPid)]),
								cerl:abstract(SpawnPid)
								
						end;
				     _ -> 
				     	get_tree_call(State)
				end;
			'cons' ->
				[NHd,NTl] = 
					get_tree_list(State#tree_state{expr = [cerl:cons_hd(Expr),cerl:cons_tl(Expr)]}),
				cerl:c_cons(NHd,NTl);
			'tuple' ->
				NExps = 
				   get_tree_list(State#tree_state{expr = cerl:tuple_es(Expr)}),
				cerl:c_tuple(NExps);
			'try' -> 
				ValueArg = 
				   get_tree(State#tree_state{expr = cerl:try_arg(Expr)}),
				case ValueArg of
				     {c_literal,[],{error,TypeError}} -> 
				     	add_bindings_to_env(
				     	   lists:zip(cerl:try_evars(Expr),
				     	             [cerl:abstract(Lit) 
				     	              || Lit <- [error,TypeError,[]]]),Env),
				     	get_tree(State#tree_state{expr = cerl:try_handler(Expr)});
				     _ ->
				        lists:map(fun(Var) -> 
				                    add_bindings_to_env([{Var,ValueArg}],Env) 
				                  end,cerl:try_vars(Expr)),
				     	get_tree(State#tree_state{expr = cerl:try_body(Expr)})
				end;
			'catch' ->
			  % Genera la expresión azucar sintáctico equivalente y computa su árbol
			  	FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
				TempVar = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV))),
				TempVar1 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+1))),
				TempVar2 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+2))),
				TempVar3 = cerl:c_var(list_to_atom("_cor_catch"++integer_to_list(FreeV+3))),
				EqTry = 
				  cerl:c_try(cerl:catch_body(Expr),
				             [TempVar],TempVar,[TempVar1,TempVar2,TempVar3], 
				             cerl:c_case(
				                TempVar1,
				           	[cerl:c_clause([cerl:abstract('throw')],TempVar2),
				                 cerl:c_clause([cerl:abstract('exit')],
				                               cerl:c_tuple([cerl:abstract('EXIT'),TempVar2])),
				           	 cerl:c_clause([cerl:abstract('error')],
				           	               cerl:c_tuple([cerl:abstract('EXIT'),
				           	                             cerl:c_tuple(
				           	                               [TempVar2,TempVar3])]))
				           	])),
				get_tree(State#tree_state{expr = EqTry});  
			'bitstr' ->
				Value = get_tree(State#tree_state{expr = cerl:bitstr_val(Expr)}),
				cerl:c_bitstr(Value,cerl:bitstr_size(Expr),
				              cerl:bitstr_unit(Expr),cerl:bitstr_type(Expr),
				              cerl:bitstr_flags(Expr));
			'binary' ->
				NExps = get_tree_list(State#tree_state{expr = cerl:binary_segments(Expr)}),
				cerl:c_binary(NExps);
			'seq' ->
		         Value =
		           get_tree(State#tree_state{expr = cerl:seq_arg(Expr)}),
		         case check_errors(Value) of
		              true -> 
		              	Value;
		              _ ->
		              	get_tree(State#tree_state{expr = cerl:seq_body(Expr)})
		         end;
			'literal' ->
				Expr;
			'var' -> 
				bind_vars(Expr,Env);
			'values' -> 
				bind_vars(Expr,Env);
			'primop' ->
			    {c_literal,[],{error,cerl:concrete(cerl:primop_name(Expr))}};
			'receive' ->
				%anar consumint receives fins trobar uno que cuadre. Si no hi ha cap, entonces el proces se considera colgat. Si si hi ha guardar la resta d'alguna manera (xq pot ser que altres els consumixquen) i guardar la traza de fins aon hem llegit
				get_tree_receive(State);
			_ -> 
				throw({error,"Non treated expression",Expr}),
			    Expr
		end,
	Pid!{result,self(),Result,Env}.

get_tree_list(State) ->
	case State#tree_state.expr of 
		[E|Es] ->
			NE = get_tree(State#tree_state{expr = bind_vars(E,State#tree_state.env)}),
			NEs = get_tree_list(State#tree_state{expr = Es}),
			[NE|NEs];
		[] -> 
			[]
	end.

get_tree_apply(State)->
	Apply = State#tree_state.expr,
	%io:format("entra apply ~p\n",[Apply]),
	Env0 = State#tree_state.env,
	SchedulerPid = State#tree_state.scheduler_pid,
	%Core,Trusted,PidTrace,Parent
	FunVar = cerl:apply_op(Apply),
	%io:format("Apply: ~p\nFunVar: ~p\nModule: ~p\nEnv0: ~p\nTrusted: ~p\n",[Apply,FunVar,cerl:module_name(Core),ets:tab2list(Env0),Trusted]),
	Pars = cerl:apply_args(Apply),
	NPars = lists:map(fun(Par) -> bind_vars(Par,Env0) end,Pars),
	FunDefs = cerl:module_defs(State#tree_state.core),
	%io:format("FunDefs: ~p\n",[FunDefs]),
	case FunVar of 
		{anonymous_function,_,{c_fun,_,Args,FunBody},_,_,_} ->
			get_tree_applyFun(State#tree_state{expr = FunBody}, Args, NPars, FunVar, FunVar);
		_ -> 
			case cerl:type(FunVar) of
				'var' ->
					case cerl:var_name(FunVar) of
					     {FunName,_} ->
							case [FunBody_ || {{c_var,_,FunName_},FunBody_} <- FunDefs, 
							                  FunName_ == cerl:var_name(FunVar)] of
							     [{c_fun,_,Args,FunBody}|_] -> % apply 'd'/1 (3)
							     	get_tree_applyFun(State#tree_state{expr = FunBody}, Args, NPars, FunVar, FunName);
							     _ -> % Apply de ¿?
							     	get_tree_call(State#tree_state{
							     		expr = cerl:ann_c_call(cerl:get_ann(Apply),
							     	              {c_literal,[],extract_module_from_ann(cerl:get_ann(Apply))},
							     	              {c_literal,[],FunName},Pars)})
							end;
					     _ -> % Apply de una variable
					     	BFunVar = bind_vars(FunVar,Env0),
					     	case BFunVar of
					     	     {anonymous_function,_,{c_fun,_,Args,FunBody},_,_,_} -> % Se enlaza a una función anónima
					     	        % {anonymous,{c_fun,_,Args,FunBody},_,_,_} = 
					     	        %         get_anon_func(EnvAF,FunName,FunCore),
					           get_tree_applyFun(State#tree_state{expr = FunBody}, Args, NPars, FunVar, BFunVar);
					     	     _ -> % Caso de un make_fun
					     	     %io:format("antes1\n"),
						     	{ModName,FunName,_} = get_MFA(BFunVar),
						     	NEnv = get_from_scheduler(SchedulerPid,{create_env,self()}),
						     	Return = 
						     		get_tree_call(State#tree_state{
							     		expr = {c_call,cerl:get_ann(Apply),
							     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
							     	    env = NEnv}),
						     	ets:delete(NEnv),
						     	Return
							end
					end;
				_ -> 
					%io:format("antes2\n"),
					{ModName,FunName,_} = get_MFA(FunVar),
					NEnv = get_from_scheduler(SchedulerPid,{create_env,self()}),
					Return = 
				     	get_tree_call(State#tree_state{
				     		expr = {c_call,cerl:get_ann(Apply),
				     	            {c_literal,[],ModName},{c_literal,[],FunName},NPars},
				     	    env = NEnv}),
			     	ets:delete(NEnv),
			     	Return
			end
		end.
	
get_tree_applyFun(State,Args,NPars,FunVar,FunName) ->
	SchedulerPid = State#tree_state.scheduler_pid,
	PidTrace = State#tree_state.pid_trace,
	%Core,FunBody,Trusted,Env0,SchedulerPid,PidTrace,Parent
	Env = get_from_scheduler(SchedulerPid,{create_env,self()}),
	create_new_env(Args, NPars, Env),
	%io:format("Env0: ~p\nEnv: ~p\n",[State#tree_state.env,ets:tab2list(Env)]),
	%add_bindings_to_env(State#tree_state.env,Env),
	CaseId = get_from_scheduler(SchedulerPid,{get_case_id,self()}),
	%CaseId = get(case_id),
	IsLC = % Detecta si se trata de una list comprehension
		case FunVar of 
		  {anonymous_function,_,_,_,_,_} ->
		  	false;
		  _ ->
			  case cerl:var_name(FunVar) of
			       {FunName,_} ->
			     	  SFunName = atom_to_list(FunName), 
					  case SFunName of
						[$l,$c,$$,$^|_] -> true;
						_ -> false
					  end;
				       _ -> false
			  end
		end,
	%io:format("[Value|NPars]: ~p\n", [[Value|NPars]]),
	APars = 
	  lists:map(fun (CF) -> get_abstract_form(CF,SchedulerPid) end ,NPars),
	{AApply,FileCall,LineCall} =  
	  case FunVar of 
	     {anonymous_function,_,AFunCore,_,_,_} ->
	     	AApply_ = {call,1,get_abstract_form(FunName,SchedulerPid),APars},
			case cerl:get_ann(AFunCore) of 
				[_,Line,{file,File}] ->
					{AApply_,File,Line};
				_ ->
					{AApply_,none,1}
			end;
	     _ ->
			  case  cerl:var_name(FunVar) of
				{FunName,_} ->
					case IsLC of
					     true ->
					     	{"",none,1};
					     _ -> 
					     	case State#tree_state.trusted of
					     	     true -> 
					     	     	{"",none,1};
					     	     _ -> 
									{{call,1,{remote,1,
									          {atom,1,cerl:concrete(cerl:module_name(State#tree_state.core))},
									          {atom,1,FunName}},APars},
									 none,1}
						end
					end;
				_ -> 
					%io:format("~p\n",[cerl:get_ann(AFunCore)]),
					AApply_ = {call,1,get_abstract_form(FunName,SchedulerPid),APars},
					case FunName of 
						{anonymous_function,_,AFunCore,_,_,_} ->
							case cerl:get_ann(AFunCore) of 
								[_,Line,{file,File}] ->
									{AApply_,File,Line};
								_ ->
									{AApply_,none,1}
							end;
						_ ->
							{AApply_,none,1}
					end
		  	end
	   end,
	IdCall =
		case AApply of 
			"" ->
				none;
			_ ->
				SchedulerPid!{store_call, PidTrace, AApply},
				get_from_scheduler(SchedulerPid,{add_call_stack,PidTrace,{'call',AApply,FileCall,LineCall},self()})
		end,
	%Value = get_tree(State#tree_state{env = Env, parent = FreeV}),
	Value = get_tree(State#tree_state{env = Env}),
	%FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
	%G!{add_vertex,FreeV},
	%SchedulerPid!increment_free_v,
	AValue = 
		case Value of 
			stuck_receive -> 
				Value;
			_ ->
				get_abstract_form(Value,SchedulerPid)
		end,
	%io:format("{Value,NFreeV,Roots}: ~p\n",[{Value,NFreeV,Roots} ]),
	case AApply of 
	     "" -> 
	     	ok;
	     _ -> 
	     	SelectedClause = get_from_scheduler(SchedulerPid,{get_case_id,CaseId,self()}),
	     	Transition = 
	     		case get_from_scheduler(SchedulerPid,{get_transition,PidTrace,self()}) of 
	     			{} ->
	     				{};
	     			{Transition_,StackTransition} ->
	     				case lists:member(IdCall,StackTransition) of 
	     					true ->
	     						Transition_;
	     					false ->
	     						{}
	     				end
	     		end,
	     	SchedulerPid!{substitute_stack,
	     		PidTrace,IdCall,
	     		{finished_call,{AApply,AValue},
	     			SelectedClause,FileCall,LineCall,PidTrace,Transition}},
	     	CallsStack = get_from_scheduler(SchedulerPid,{get_calls_stack,PidTrace,self()}),
	     	FinishedCallsStack = get_from_scheduler(SchedulerPid,{get_finished_calls_stack,PidTrace,self()}),
	     	case CallsStack of 
	     		[] -> 
	     			G = get_from_scheduler(SchedulerPid,{get_graph,self()}),
	     			NodeParent = 
		     			% case Transition of 
		     			% 	{_,_,_,NodeParent_}  ->
		     			% 		NodeParent_;
		     			% 	_ ->
		     					State#tree_state.parent,
		     			% end,
	     			%io:format("Call : ~p\nTransition:~p\n",[FinishedCall,Transition]),
	     			Sent = get_from_scheduler(SchedulerPid,{get_temp_sent,PidTrace,self()}),
	     			Spawned = get_from_scheduler(SchedulerPid,{get_temp_spawned,PidTrace,self()}),
	     			Receipt = get_from_scheduler(SchedulerPid,{get_temp_receipt,PidTrace,self()}),
	     			%io:format("Receipt: ~p\n",[Receipt]),
	     			%io:format("CallsStack: ~p\n",[CallsStack]),
	     			%io:format("FinishedCallsStack: ~p\n",[FinishedCallsStack]),
	     			FirstStack = [Elem || Elem = {Id,_,_} <- FinishedCallsStack, Id == 1],
	     			store_tree_calls(FirstStack,FinishedCallsStack,G,NodeParent,SchedulerPid,Transition,Sent,Spawned,Receipt,PidTrace,[]),
	     			SchedulerPid!{store_temp_data,PidTrace},
	     			SchedulerPid!{clean_stack,PidTrace};
	     		_ ->
	     			ok
	     	end
			% G!{add_vertex,FreeV,{erl_prettypr:format({match,1,AApply,AValue},
			%                                        [{paper, 300},{ribbon, 300}]),
			%                     SelectedClause,FileCall,LineCall}},
			% G!{add_edge,State#tree_state.parent,FreeV},
			% digraph:add_vertex(G,NFreeV,
			%                    {erl_prettypr:format({match,1,AApply,AValue},
			%                                        [{paper, 300},{ribbon, 300}]),
			%                     SelectedClause,FileCall,LineCall}),
			% [digraph:add_edge(G,NFreeV,Root) || Root <- Roots],
			%Value
	end,
	%ets:delete(Env),
	Value.

get_tree_call(State) -> 
	%Call,Env0,Core,SchedulerPid,PidTrace,Parent
	Call = State#tree_state.expr,
	%io:format("entra call ~p\n",[Call]),
	Env0 = State#tree_state.env,
	ModName = cerl:concrete(bind_vars(cerl:call_module(Call),Env0)),
	FunName = cerl:concrete(bind_vars(cerl:call_name(Call),Env0)),
	FunArity = cerl:call_arity(Call),
	Args = cerl:call_args(Call),
	%io:format("FUNCTION CALL ~p:~p/~p\n",[ModName,FunName,FunArity]),
%	io:format("~p\n",[Call]),
	FileAdress = code:where_is_file(atom_to_list(ModName)++".erl"),
	% Busca el erl. Si no está, busca el beam (libreria de sistema) y tunea la ruta
	% para apuntar al ebin/ correspondiente
	NFileAdress = 
	   case FileAdress of
	        non_existing -> 
	     		NFileAdress_ = code:where_is_file(atom_to_list(ModName)++".beam"),
	     		%io:format("antes1\n"),
	     		case NFileAdress_ of
	     		     non_existing -> 
	     		     	throw({error,"Non existing module",ModName});
	     		     _ -> 
	     		     	RelPath = "ebin/" ++ atom_to_list(ModName) ++ ".beam",
	     		     	NRelPath = "src/" ++ atom_to_list(ModName) ++ ".erl",
	     		     	%io:format("antes2\n"),
	     		     	PrevPath = 
	     		     	   lists:sublist(NFileAdress_,1,
	     		     	                 length(NFileAdress_)-length(RelPath)),
	     		     	%io:format("despues\n"),
	     		     	PrevPath ++ NRelPath
	     		end;
	     	_ -> FileAdress
	   end,
	[FirstChar|_] = NFileAdress,
	Trusted = 
	   case FirstChar of
	        $. -> false;
	        _ -> true
	   end,
	%io:format("Current module: ~p\n",[cerl:module_name(State#tree_state.core)]),
	case {cerl:concrete(cerl:module_name(State#tree_state.core)),Trusted} of
	     {ModName,false} -> % Call de una función en el mismo módulo
	     	get_tree_apply(State#tree_state{
	     		expr = cerl:ann_c_apply(cerl:get_ann(Call),
	     	    		cerl:c_var({FunName,FunArity}), Args),
	     		trusted = false});
	     {_,false} -> % Call de una función en un módulo distinto
	     	ModCore = edd_lib:core_module(NFileAdress),
	        get_tree_apply(State#tree_state{
	        	expr = cerl:ann_c_apply(cerl:get_ann(Call),
	                     cerl:c_var({FunName,FunArity}),Args),
	        	core = ModCore,
	        	trusted = false});
	     _ -> % Es trusted
	     	BArgs = lists:map(fun(Arg) -> bind_vars(Arg,Env0) end,Args),
	     	%io:format("Args: ~p\nBArgs: ~p\n",[Args, BArgs]),
	     	NoLits = 
		     	case [BArg || BArg = {anonymous_function,_,_,_,_,_} <- BArgs] of
		     	     [] ->
		     	        [BArg || BArg <- BArgs,
		     	                 not cerl:is_literal(cerl:fold_literal(BArg))];
		     	     List -> List
		     	end,
	     	case {NoLits} of
	     	    {[]} -> 
	     	     	ABArgs = 
	     	     	   [get_abstract_from_core_literal(cerl: fold_literal(BArg)) 
	     	     	    || BArg <- BArgs],
	     	     	Value =  
						try
		           % Posible problema si el ya existe un módulo foo
						   M1 = smerl:new(foo),
						   {ok, M2} = 
						      smerl:add_func(M1, 
						          {function,1,bar,0,
					                    [{clause,1,[],[],
					                     [{call,1,{remote,1,{atom,1,ModName},
						                               {atom,1,FunName}},ABArgs}]}]}),
	                         smerl:compile(M2,[nowarn_format]),
		                     foo:bar() %Genera el valor utilizando el módulo creado
				                               %on de fly
						catch
						   Exception:Reason -> {Exception,Reason}
						end,
					AValue = cerl:abstract(Value),
					AValue;
			     _ -> 
			        case {ModName,FunName} of
			             {'erlang','is_function'} ->
			                case BArgs of
			                     [Function = {c_call,_,{c_literal,_,erlang},
			                      {c_literal,_,make_fun},_} | MayBeArity] ->
			                      	   %io:format("antes3\n"),
				                       {_,_,CFunArity} = get_MFA(Function),
				                       case lists:map(fun cerl:concrete/1,MayBeArity) of
		     	                            [] ->  {c_literal,[],true};
		     	                            [CFunArity] -> {c_literal,[],true};
		     	                            _ -> {c_literal,[],false}
			     	                    end;
			                     [{anonymous_function,_,{c_fun,_,AFunArgs,_},_,_,_} | MayBeArity] ->
			                       % {anonymous,{c_fun,_,AFunArgs,_},_,_,_} = 
		     	                  %         get_anon_func(EnvAF,AFunName,AFunCore),
		     	                       AFunArity = length(AFunArgs),
		     	                       case lists:map(fun cerl:concrete/1,MayBeArity) of
		     	                            [] ->  {c_literal,[],true};
		     	                            [AFunArity] -> {c_literal,[],true};
		     	                            _ -> {c_literal,[],false}
		     	                       end; 
			                     _ ->
			                       {c_literal,[],false}
			                end;
			             _ ->
				               ModCore = edd_lib:core_module(NFileAdress),
				               get_tree_apply(State#tree_state{
			               			expr = cerl:ann_c_apply(cerl:get_ann(Call),
			                               cerl:c_var({FunName,FunArity}),Args),
			               			core = ModCore})

			        end
		        
		 end
	end.
	
get_tree_case(State) -> 
	Expr = State#tree_state.expr,
	Env = State#tree_state.env,
	SchedulerPid = State#tree_state.scheduler_pid,
	% Expr,Env,Core,Trusted,SchedulerPid,PidTrace,Parent
	Args = cerl:case_arg(Expr),
	ArgsValue = get_tree(State#tree_state{expr = Args}),
	BArgs_ = bind_vars(ArgsValue,Env),
	BArgs = 
		case BArgs_ of
			{anonymous_function,_,_,_,_,_} ->
				[BArgs_];
			_ ->
				case cerl:type(BArgs_) of
					values -> cerl:values_es(BArgs_);
					_ -> [BArgs_]
				end
		end,
    Clauses = cerl:case_clauses(Expr),
    case get_clause_body(Clauses,BArgs,State,[],[],1) of
    	{Clause,ClauseBody,Bindings,_,_} -> 
    		Case_Id = get_from_scheduler(SchedulerPid,{get_case_id,self()}),
			SchedulerPid!{set_case_id,Case_Id,get_clause_number(Clauses,Clause,1)},
			%put(get(case_id),get_clause_number(Clauses,Clause,1)),
			%put(case_id,get(case_id)+1),
			add_bindings_to_env(Bindings,Env),
			get_tree(State#tree_state{expr = ClauseBody});
    	{none,_} -> 
    		throw({error,"Non matching clause exists"}) 
    end.
	
	
% 1.- Construye la 1ª cláusula sin guardas, el resto igual
% 2.- Mira si ha elegido la 1ª cláusula
% 3.- a) Si el la primera, comprueba que la guarda se cumple. Si es así, 
%        devuelve esa cláusula como la elegida. Si no, prueba con la siguiente.
% 3.- b) Si no, prueba con la siguiente
%% Explicación: cerl_clauses:reduce(), que devuelve la cláusula que se elige
%% en un case, no funciona con las guardas
get_clause_body([Clause | Clauses],BArgs,State,DepsBefore,InfoFails,NumClause) ->
	NClause = cerl:c_clause(cerl:clause_pats(Clause), cerl:clause_body(Clause)),
	NNumClause = NumClause + 1,
	SchedulerPid = State#tree_state.scheduler_pid,
	%io:format("NClause: ~p\nBArgs: ~p\n",[NClause,BArgs]),
	case cerl_clauses:reduce([NClause| Clauses], BArgs) of
	     {true, {{c_clause, _, _, _, ClauseBody}, Bindings}} -> 
			case cerl:clause_body(Clause) of
			     ClauseBody -> 
			     	%io:format("antes: ~p\n",[ets:tab2list(State#tree_state.env) ++ [{VarName, Value} || {{c_var,_,VarName},Value} <- Bindings]]),
			     	TempEnv = get_from_scheduler(SchedulerPid,{create_env,self()}),
					ets:insert(TempEnv,ets:tab2list(State#tree_state.env) ++ [{VarName, Value} || {{c_var,_,VarName},Value} <- Bindings]),
					%{VarsPattern,ValuesPattern} = lists:unzip(Bindings),
					%add_bindings_to_env([ {Var, Value, [], null} || {Var,Value} <- Bindings],TempEnv),
					%io:format("Guarda: ~p\n",[cerl:get_ann(cerl:clause_guard(Clause))]),
			     	GuardValue = 
			     		get_tree(State#tree_state{
			     			expr = cerl:clause_guard(Clause),
			     			env = TempEnv}),
			     	DepsAfter = 
			     		get_dependences(cerl_trees:variables(State#tree_state.expr), TempEnv, SchedulerPid),

			     	ets:delete(TempEnv),
			     	VarsPat = 
			     		case cerl:clause_pats(Clause) of 
			     			[] ->
			     				[];
			     			[FirstPat|_] ->
			     				%io:format("FirstPat: ~p\n",[FirstPat]),
			     				%io:format("VarsGuard: ~p\nVarsPat: ~p\n",[cerl_trees:variables(cerl:clause_guard(Clause)),cerl_trees:variables(FirstPat)]),
			     				get_core_vars(cerl_trees:variables(FirstPat))
			     		end,
			     	VarsGuard = get_core_vars(cerl_trees:variables(cerl:clause_guard(Clause))),
			     	%io:format("VarsGuard: ~p\nVarsPat: ~p\n",[VarsGuard,VarsPat]),
			     	DepsGuard = DepsAfter -- DepsBefore,
			     	Reason = 
			     		case {(VarsGuard -- VarsPat),{c_literal,[],true} == cerl:clause_guard(Clause)} of 
			     			{VarsGuard,false} ->
			     				{guard,DepsGuard};
			     			_ ->
			     				case cerl:concrete(GuardValue) of
			     					true -> {pat, DepsGuard};
			     					false -> pat 
			     				end
			     		end,
			     	%io:format("Reason: ~p\n",[Reason]),
			     	case cerl:concrete(GuardValue) of
			     	     true -> {Clause,ClauseBody,Bindings,InfoFails,Reason};
			     	     false -> get_clause_body(Clauses,BArgs,State,DepsBefore,[{NumClause,Reason} | InfoFails],NNumClause)
			     	end;
			     _ -> 
			     	get_clause_body(Clauses,BArgs,State,DepsBefore,[{NumClause,pat} | InfoFails],NNumClause)
			end;
	      _ -> 
	      	get_clause_body(Clauses,BArgs,State,DepsBefore,[{NumClause,pat} | InfoFails],NNumClause)
	end;
get_clause_body([],_,_,_,InfoFails,_) -> {none,InfoFails}.

get_tree_receive(State) ->
	Expr = State#tree_state.expr,
	SchedulerPid = State#tree_state.scheduler_pid,
	PidTrace = State#tree_state.pid_trace,
	G = get_from_scheduler(SchedulerPid,{get_graph,self()}),
	%Parent = State#tree_state.parent,
	Transition = get_from_scheduler(SchedulerPid,{get_transition,PidTrace,self()}),
 	Parent = 
		% case Transition of 
		% 	{_,_,_,NodeParent_}  ->
		% 		NodeParent_;
		% 	_ ->
				State#tree_state.parent,
		% end,
	%io:format("Entra a tree receive ~p\n",[PidTrace]),
	LineFile = get_file_line(Expr),
	case LineFile of 
		[Line,File] -> 
			[AExpr|_] = get_expression_from_abstract(File,Line,'receive'),
			CallsStack = get_from_scheduler(SchedulerPid,{get_calls_stack,PidTrace,self()}),
			FinishedCallsStack = get_from_scheduler(SchedulerPid,{get_finished_calls_stack,PidTrace,self()}),
			Sent = get_from_scheduler(SchedulerPid,{get_temp_sent,PidTrace,self()}),
	     	Spawned = get_from_scheduler(SchedulerPid,{get_temp_spawned,PidTrace,self()}),
	     	TempReceipt = get_from_scheduler(SchedulerPid,{get_temp_receipt,PidTrace,self()}),
	     	%io:format("TempReceipt: ~p\n",[TempReceipt]),
	     	%io:format("CallsStack: ~p\n",[CallsStack]),
	     	%io:format("FinishedCallsStack: ~p\n",[FinishedCallsStack]),
	     	%io:format("antes\n"),
			store_tree_receive(lists:reverse(CallsStack),FinishedCallsStack,G,0,AExpr,SchedulerPid,PidTrace,File,Line,Sent,Spawned,TempReceipt,Transition),
			%io:format("despues\n"),
			SchedulerPid!{store_temp_data,PidTrace},
			SchedulerPid!{clean_stack,PidTrace};
		_ -> 
			AExpr = none,
			{Line,File} = {0,0}
	end,
	%io:format("ANTES_1\n"),
	wait_for_scheduling(SchedulerPid,State),
	%PidTraces = get_from_scheduler(SchedulerPid,{get_traces_pid,PidTrace,self()}),
	CurrentTrace = get_from_scheduler(SchedulerPid,{get_trace,PidTrace,self()}),
	Receipt = get_receipt(PidTrace,CurrentTrace,SchedulerPid,State),
	Clauses = cerl:receive_clauses(Expr),
	DepsBefore = get_dependences(cerl_trees:variables(Expr),State#tree_state.env, SchedulerPid),
	%io:format("LineFile: ~p\n",[LineFile]),
	%io:format("Receipt: ~p\n",[Receipt]),
	case get_clause_body_receive(CurrentTrace,Receipt,Clauses,SchedulerPid,State,DepsBefore,[]) of 
		{Clause,ClauseBody,NTrace,Consumed,CoreConsumed,InfoFails,Reason} ->
			%io:format("InfoFails: ~p\n",[InfoFails]),
			SchedulerPid!{set_trace, PidTrace, NTrace},
			SchedulerPid!{set_receipt, PidTrace, [Item || Item <- Receipt, Item /= CoreConsumed]},
			% Parent = 
			% 	case get_from_scheduler(SchedulerPid,{get_free_v,self()}) of 
			% 		FreeV ->
			% 			State#tree_state.parent;
			% 		_ -> 
			% 			FreeV 
			% 	end,
			% get_tree(State#tree_state{expr = ClauseBody,parent = Parent});
			{IdReceive,NodeReceive,Context} = 
				case AExpr of 
					none ->
						%{ConsumedSender,NConsumedMsg} = {none,none},
						{none,Parent,[]};
					_ ->
						%io:format("\nSTORE\nPid: ~pInfo: ~p\n",[PidTrace,{AExpr,Line,File}]),
						NodeReceive_ = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
						SchedulerPid!{set_transition,PidTrace,{AExpr,Line,File},get_clause_number(Clauses,Clause,1),Consumed,NodeReceive_},
						%G!{add_vertex,NodeReceive_},
						SchedulerPid!increment_free_v,
						Context_ = get_dependences(cerl_trees:variables(Expr),State#tree_state.env, SchedulerPid),
						IdReceive_ =
							get_from_scheduler(SchedulerPid,{add_call_stack,PidTrace,{'receive',{AExpr,undefined,Line,File},get_clause_number(Clauses,Clause,1),Consumed,Context_,InfoFails,Reason,NodeReceive_},self()}),
						{IdReceive_,NodeReceive_,Context_}
				end,
			Value = get_tree(State#tree_state{expr = ClauseBody}),
			AValue = 
				case Value of 
					stuck_receive -> 
						Value;
					_ ->
						case Value of 
							{c_values,_,[FirstValue,_]} ->
								get_abstract_form(FirstValue,SchedulerPid);
							_ ->
								get_abstract_form(Value,SchedulerPid)
						end
				end,
			case AExpr of 
					none ->
						ok;
					_ ->
						SchedulerPid!{value_transition,PidTrace,NodeReceive,AValue},
						Bindings = get_dependences(cerl_trees:variables(Expr),State#tree_state.env, SchedulerPid) -- Context,
						%SchedulerPid!{set_transition,PidTrace,{AExpr,Line,File},get_clause_number(Clauses,Clause,1),Consumed,NodeReceive},
						SchedulerPid!{substitute_stack,
		     				PidTrace,IdReceive,
		     				{finished_recieve,{AExpr,AValue,Line,File},NodeReceive,get_clause_number(Clauses,Clause,1),Consumed,Context,Bindings,{},InfoFails,Reason}},
						%store_tree_whole_receive(NodeReceive,AExpr,AValue,File,Line,G,get_clause_number(Clauses,Clause,1),PidTrace,SchedulerPid,Parent,{ConsumedSender,NConsumedMsg}),
						SchedulerPid!{store_receive,PidTrace,NodeReceive}
			end,
			%io:format("ix\n"),
			Value;
		{Value,InfoFails} ->
			case AExpr of 
				none ->
					ok;
				_ ->
					%io:format("InfoFails: ~p\n",[InfoFails]),
					%io:format("\nSTORE\nPid: ~pInfo: ~p\n",[PidTrace,{AExpr,Line,File}]),
					NodeReceive = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
					SchedulerPid!increment_free_v,
					SchedulerPid!{set_transition,PidTrace,{AExpr,Line,File},none,{none,none},NodeReceive},
					SchedulerPid!{value_transition,PidTrace,NodeReceive,Value},
					Deps = get_dependences(cerl_trees:variables(Expr),State#tree_state.env, SchedulerPid),
					IdReceive =
						get_from_scheduler(SchedulerPid,{add_call_stack,PidTrace,{'receive',{AExpr,undefined,Line,File},none,{none,none},Deps,InfoFails,none,NodeReceive},self()}),
					SchedulerPid!{substitute_stack,
		     				PidTrace,IdReceive,
		     				{finished_recieve,{AExpr,Value,Line,File},NodeReceive,none,{none,none},Deps,[],{},InfoFails,none}},
					%store_tree_whole_receive(NodeReceive,AExpr,Value,File,Line,G,none,PidTrace,SchedulerPid,Parent,none),
					SchedulerPid!{store_receive,PidTrace,NodeReceive}
			end,
			%io:format("ix\n"),
			Value
	end.
	% {_,ClauseBody,NTrace,Consumed} = 
	% 	get_clause_body_receive(CurrentTrace,Receipt,Clauses,SchedulerPid,State),


%There is not a register of recepit messages
get_clause_body_receive(_,[],_,_,_,_,InfoFails) ->
	{stuck_receive,InfoFails};
%There is a register of recepit messages, but the trace is empty, so it is not read
get_clause_body_receive([],_,_,_,_,_,InfoFails) ->
	{stuck_receive,InfoFails};
	%throw({error,"Something bad has happened while receiving messages."});
get_clause_body_receive(Trace,[{Sender,Msg}|Receipt],Clauses,SchedulerPid,State,DepsBefore,InfoFails) ->
	%io:format("Entra\n"),
	NTrace = get_next_receipt(Trace,[]),
	%io:format("{Sender,Msg}: ~p\n",[{Sender,Msg}]),
	%io:format("Trace: ~p\nNTrace: ~p\n",[Trace,NTrace]),
	case NTrace of 
		not_found ->
			%io:format("\n\nEspera!!\n\n"),
			%io:format("ANTES_2\n"),
			wait_for_scheduling(SchedulerPid,State),
			get_clause_body_receive(Trace,[{Sender,Msg}|Receipt],Clauses,SchedulerPid,State,DepsBefore,InfoFails);
		_ -> 
			%io:format("\n\nTrobat!!\n~p -> ~p\n\n",[Trace,NTrace]),
			PrettyMsg = erl_prettypr:format(get_abstract_form(Msg,none),[{paper, 300},{ribbon, 300}]),
			case get_clause_body(Clauses,[Msg],State,DepsBefore,[],1) of 
				{none,InfoFailsReceive} ->
					NInfoFailsReceive = [{Sender,PrettyMsg,InfoFailClause} || InfoFailClause <- InfoFailsReceive],
					get_clause_body_receive(NTrace,Receipt,Clauses,SchedulerPid,State,DepsBefore,InfoFails ++ NInfoFailsReceive) ;
				{Clause,ClauseBody,Bindings,InfoFailsReceive,Reason}  ->
					%io:format("InfoFails: ~p\n",[InfoFails]),
					add_bindings_to_env(Bindings,State#tree_state.env),
					NInfoFailsReceive = [{Sender,PrettyMsg,InfoFailClause} || InfoFailClause <- InfoFailsReceive],
					{Clause,ClauseBody,NTrace,{Sender,PrettyMsg},{Sender,Msg},InfoFails ++ NInfoFailsReceive,Reason}
			end
	end.

get_receipt(PidTrace,Trace,SchedulerPid,State) ->
	case get_from_scheduler(SchedulerPid,{get_receipt,PidTrace,self()}) of 
		[] ->
			NTrace = get_next_receipt(Trace,[]),
			%io:format("Trace : ~p\nNTrace: ~p\n",[Trace,NTrace]),
			case NTrace of 
				not_found ->
					[];
				_ ->
					%io:format("ANTES 3\n"),
					wait_for_scheduling(SchedulerPid,State),
					get_receipt(PidTrace,Trace,SchedulerPid,State)
			end;
		Other -> 
			Other 
	end.

wait_for_scheduling(SchedulerPid,State) ->
	SchedulerPid!{add_receive,State,self()},
	SchedulerPid!next_mnf,
	%io:format("WAITING\n"),
	receive 
		continue -> ok
	end.


store_tree_receive([],_,_,_,_,_,_,_,_,_,_,_,_) ->
	ok;
store_tree_receive([{IdCall, _, Call}|Stack],FinishedCallsStack,G,Parent,AExpr,SchedulerPid,PidTrace,File,Line,Sent,Spawned,Receipt,Transition_) ->
	%io:format("Transition_: ~p\n",[Transition_]),
	Transition = 
		case Transition_ of 
 			{} ->
 				{};
 			{Transition__,StackTransition} ->
 				case lists:member(IdCall,StackTransition) of 
 					true ->
 						Transition__;
 					false ->
 						{}
 				end
 		end,
 	{Node, _, NCall} = 
 		case Call of 
 			{'receive',A1,A2,SenderMsg,Context_,InfoFails,A3,NodeReceive_} -> 
 				DiscardedMessages = 
					lists:usort([{SenderF,MsgF}  || {SenderF,MsgF,_} <- InfoFails]) -- [SenderMsg], 
				%io:format("ContextTR: ~p\n",[Context_]),
 				{NodeReceive_,Context_,{'receive',A1,A2,SenderMsg,Context_,InfoFails,A3,NodeReceive_,DiscardedMessages}};
 			_ ->
		 		FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
				SchedulerPid!increment_free_v,
				{FreeV,[],Call}
		end,
	% case Call of 
	% 	{finished_call,AMatch,SelectedClause,FileCall,LineCall,PidTraceCall,TransitionCall} ->
	% 		G!{add_vertex,FreeV,{AMatch,SelectedClause,FileCall,LineCall,PidTraceCall,[{Pid,Msg} || {Pid,Msg,Id} <- Sent, Id == IdCall], [Pid || {Pid,Id} <- Spawned, Id == IdCall],TransitionCall}};
	% 	_ ->
	G!{add_vertex,Node,{to_receive,AExpr,NCall,File,Line,PidTrace,
	 %[{Pid,Msg} || {Pid,Msg,Id} <- Sent, Id == IdCall], [Pid || {Pid,Id} <- Spawned, Id == IdCall],
	 [{Pid,Msg} || {Pid,Msg,Id} <- Sent, lists:member(IdCall,Id)], [Pid || {Pid,Id} <- Spawned, lists:member(IdCall,Id)],
	 Transition}},
	%end,
	%io:format("Call ~p\n",[Call]),
	% case Call of 
	% 	{'receive',Receive,Clause,SenderMsg,Context,InfoFails,Reason,NodeReceive} ->
	% 		%io:format("InfoFails: ~p\n{Clause,Consumed,Reason}: ~p\nNodeReceive:~p\n",[InfoFails,{Clause,Consumed,Reason},NodeReceive]);
	% 		%store_tree_succeed_fail(G,PidTrace,SchedulerPid,NodeReceive,Receive,InfoFails,{Clause,Consumed,Reason},Context);
	% 	_ ->
	% 		ok 
	% end,
	FinishedChildren = 
		[Elem || Elem = {_,Id,_} <- FinishedCallsStack, Id == IdCall],
	store_tree_calls(FinishedChildren,FinishedCallsStack,G,Node,SchedulerPid,Transition,Sent,Spawned,Receipt,PidTrace,[]),
	%[io:format("entra\n") || {NR,Id} <- Receipt, Id == IdCall],
	%[G!{add_edge,FreeV,NR} || {NR,Id} <- Receipt, Id == IdCall],
	G!{add_edge,Parent,Node},
	store_tree_receive(Stack,FinishedCallsStack,G,Node,AExpr,SchedulerPid,PidTrace,File,Line,Sent,Spawned,Receipt,Transition_).

% store_tree_whole_receive(NodeReceive,AExpr,Value,File,Line,G,SelectedClause,PidTrace,SchedulerPid,_,Consumed) ->
% 	Sent = get_from_scheduler(SchedulerPid,{get_sent,PidTrace,self()}),
% 	Spawned = get_from_scheduler(SchedulerPid,{get_spawned,PidTrace,self()}),
% 	G!{add_vertex,NodeReceive,{{'receive',AExpr,Value,File,Line,Consumed,SelectedClause},SelectedClause,none,none,PidTrace,Sent,Spawned,{}}}.
% 	%G!{add_edge,Parent,NodeReceive}.

store_tree_calls([],_,_,_,_,_,_,_,_,_,_) ->
	ok;
store_tree_calls([{IdCall, IdParent, FinishedCallReceive}|CurrentStack],FinishedCallsStack,G,Parent,SchedulerPid,Transition,Sent,Spawned,Receipt,PidTrace,IdStackNode) ->
	SentCall = [{Pid,Msg} || {Pid,Msg,Id} <- Sent, lists:member(IdCall,Id)],
	SpawnedCall = [Pid || {Pid,Id} <- Spawned, lists:member(IdCall,Id)],
	%io:format("FinishedCallReceive:~p\n",[FinishedCallReceive]),
	NodeId = 
		case FinishedCallReceive of 
			{finished_call,AMatch,SelectedClause,FileCall,LineCall,_,TransitionCall} ->
				FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
				SchedulerPid!increment_free_v,
				G!{add_vertex,FreeV,{to_value,{call,AMatch,FileCall,LineCall,TransitionCall},PidTrace, SelectedClause, SentCall, SpawnedCall}},
				FreeV;
			{finished_recieve,FinishedRecieve,NodeReceive,Clause,SenderMsg,Context,Bindings, {},InfoFails,_} ->
				%io:format("ContextTC: ~p\n",[Context]),
				DiscardedMessages = 
					lists:usort([{SenderF,MsgF}  || {SenderF,MsgF,_} <- InfoFails]) -- [SenderMsg],
				G!{add_vertex,NodeReceive,{to_value,{'receive',FinishedRecieve, SenderMsg,Context,Bindings,DiscardedMessages},PidTrace, Clause, SentCall, SpawnedCall}},
				%store_tree_succeed_fail(G,PidTrace,SchedulerPid,NodeReceive,FinishedRecieve,InfoFails,{Clause,MsgSender,Reason},Context),
				NodeReceive
		end,
	RealParent = 
		case IdParent of 
			none -> 
				Parent;
			_ ->
				%io:format("IdStackNode: ~p\n",[IdStackNode]),
				case [Node || {Id,Node} <- IdStackNode, Id == IdParent] of 
					[] ->
						Parent;
				    [Parent_|_] ->
				    	Parent_ 
				end
		end,
	%io:format("IdCall: ~p\nRealParent: ~p\nIdStackNode: ~p\n",[IdCall,RealParent,IdStackNode]),
	G!{add_edge,RealParent,NodeId},
	%[io:format("entra\n") || {NR,Id} <- Receipt, Id == IdCall],
	%[G!{add_edge,FreeV,NR} || {NR,Id} <- Receipt, Id == IdCall],
	NIdStackNode = [{IdCall,NodeId}|IdStackNode],
	Children = [Elem || Elem = {_,Id,_} <- FinishedCallsStack, Id == IdCall],
	store_tree_calls(CurrentStack ++ Children,FinishedCallsStack,G,Parent,SchedulerPid,Transition,Sent,Spawned,Receipt,PidTrace,NIdStackNode).
    
% store_tree_succeed_fail(G,PidTrace,SchedulerPid,Parent,Recieve,[{Sender,Msg,{Clause,Reason}} | InfoFails],InfoSucceed,Context) ->
% 	FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
% 	SchedulerPid!increment_free_v,
% 	case Reason of 
% 		pat ->
% 			G!{add_vertex,FreeV,{receive_clause,pattern,fail,Recieve,PidTrace, {Sender,Msg}, Clause, [],Context}},
% 			G!{add_edge,Parent,FreeV};
% 		{guard, Bindings} ->
% 			G!{add_vertex,FreeV,{receive_clause,pattern,succeed,Recieve,PidTrace, {Sender,Msg}, Clause, Bindings,Context}},
% 			G!{add_edge,Parent,FreeV},
% 			NFreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
% 			SchedulerPid!increment_free_v,
% 			G!{add_vertex,NFreeV,{receive_clause,guard,fail,Recieve,PidTrace, {Sender,Msg}, Clause, Bindings,Context}},
% 			G!{add_edge,FreeV,NFreeV}
% 	end,
% 	store_tree_succeed_fail(G,PidTrace,SchedulerPid,Parent,Recieve,InfoFails,InfoSucceed,Context);
% store_tree_succeed_fail(G,PidTrace,SchedulerPid,Parent,Recieve,[],{Clause,MsgSender,Reason},Context) ->
% 	case Clause of 
% 		none ->
% 			ok;
% 		_ -> 
% 			case Reason of 
% 				{pat, Bindings} ->
% 					FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
% 					SchedulerPid!increment_free_v,
% 					G!{add_vertex,FreeV,{receive_clause,pattern,succeed,Recieve,PidTrace, MsgSender, Clause, Bindings,Context}},
% 					G!{add_edge,Parent,FreeV};
% 				{guard, Bindings} ->
% 					FreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
% 					SchedulerPid!increment_free_v,
% 					G!{add_vertex,FreeV,{receive_clause,pattern,succeed,Recieve,PidTrace, MsgSender, Clause, Bindings,Context}},
% 					G!{add_edge,Parent,FreeV},
% 					NFreeV = get_from_scheduler(SchedulerPid,{get_free_v,self()}),
% 					SchedulerPid!increment_free_v,
% 					G!{add_vertex,NFreeV,{receive_clause,guard,succeed,Recieve,PidTrace, MsgSender, Clause, Bindings,Context}},
% 					G!{add_edge,FreeV,NFreeV}
% 			end
% 	end.

% Core es el CORE del módulo foo que tiene bar() -> Expr
% extract_call devuelve únicamente el CORE de Expr (o quizá del case que lo contiene)
extract_call(Core) ->
	{c_module,_,_,_,_,FunDefs} = Core,
	[{c_fun,_,_,FunBody}|_] = 
		[FunBody_ || {{c_var,_,FunName},FunBody_} <- FunDefs, FunName == {bar,0}],
	[{c_clause,_,_,_,Call}|_] = cerl:case_clauses(FunBody),
	Call.	


get_abstract_form({anonymous_function,_,_,File,Line,Env},PidScheduler) -> 
	get_fun_from_file(File,Line,Env,PidScheduler);
get_abstract_form(stuck_receive,_) -> 
	get_abstract_from_core_literal(cerl:abstract(stuck_receive));
get_abstract_form(Par_,_) -> 
	Par = cerl:fold_literal(Par_),
	case cerl:is_literal(Par) or is_pid(Par) of
		true -> 
			get_abstract_from_core_literal(Par);
		_ -> 
			%io:format("antes4 ~p\n",[Par]),
			{ModName_,FunName_,FunArity_} = get_MFA(Par),
			{'fun',1,
				{function,
				{atom,1,ModName_},
				{atom,1,FunName_},
				{integer,1,FunArity_}}}
	end. 

get_fun_from_file(File,Line,Env,PidScheduler) -> 
	AnoFun = 
		case smerl:for_file(File) of
		     {ok,Abstract} ->
			hd(lists:flatten([get_funs_from_abstract(Form,Line) 
			                  || Form <- smerl:get_forms(Abstract)]));
		     {error, {invalid_module, _}} -> 
		     	hd(get_from_scheduler(PidScheduler,{get_funs_initial_call,self()}))
		end,
	PatternsClause = 
		[erl_syntax:clause_patterns(Clause) || Clause <- erl_syntax:fun_expr_clauses(AnoFun)],
	BodyClause = 
		[erl_syntax:clause_body(Clause) || Clause <- erl_syntax:fun_expr_clauses(AnoFun)],
	VariablesPat = 
		sets:union([erl_syntax_lib:variables(Pattern) 
						|| Patterns <- PatternsClause, Pattern <- Patterns]),
	VariablesBody = 
		sets:union([erl_syntax_lib:variables(BodyExpression) 
						|| Body <- BodyClause, BodyExpression <- Body]),
	VarsOnlyInBody = sets:to_list(sets:subtract(VariablesBody, VariablesPat)),
	% io:format("VarsOnlyInBody: ~p\n",[VarsOnlyInBody]),
	% io:format("VariablesPat: ~p\n",[sets:to_list(VariablesPat)]),
	% io:format("VariablesBody: ~p\n",[sets:to_list(VariablesBody)]),
	FunChangeVar =
		fun(T) ->
			case erl_syntax:type(T) of 
				variable -> 
					VarName = erl_syntax:variable_name(T),
					case lists:member(VarName,VarsOnlyInBody) of 
						true ->
							BVars = bind_vars(cerl:c_var(VarName),Env),
							case is_atom(BVars) of
								true ->
									erl_syntax:variable(BVars);
								false ->
									get_abstract_form(BVars,PidScheduler)
							end;
						false ->
							T
					end;
				_ ->
					T
			end
		end,
	erl_syntax_lib:map(FunChangeVar,AnoFun).    	
    
bind_vars(Expr,Env) ->
	%io:format("Expr: ~p\nEnv: ~p\n",[Expr,ets:tab2list(Env)]),
	case Expr of
	     {anonymous_function,_,_,_,_,_} -> Expr;
	     _ ->
		case cerl:type(Expr) of
		     'var' -> 
		     	VarName = cerl:var_name(Expr),
		     	case VarName of
		     	     {FunName,Arity} -> 
		     	     	[_,{file,FileName},_] = cerl:get_ann(Expr),
		     	     	{ModName,_} = lists:split(length(FileName)-4,FileName),
		     	     	MFArgs = 
		     	     	    [{c_literal,[],list_to_atom(ModName)},
           			     {c_literal,[],FunName},
          		             {c_literal,[],Arity}],
		     	     	{c_call,[],{c_literal,[],erlang},{c_literal,[],make_fun},MFArgs};
		     	     _ ->
		     	     	%io:format("~p\n",[VarName]),
		     	     	%io:format("Env: ~p\n",[ets:tab2list(Env)]),
		     	     	case ets:lookup(Env,VarName) of 
		     	     		[{VarName,Value}|_] ->
				     			case Value of
				     	             {anonymous_function,_,_,_,_,_} -> Value;
				     	             _ -> cerl:unfold_literal(Value)
				     	        end;
				     	    _ ->
				     	    	%Esto se anyade porque get_fun_from_file no tiene en cuenta las variables que se declaran dentro del cuerpo de la función y intenta buscarlas
				     	    	VarName
				     	end
		     	end;
		     'values' -> 
		     	Values = cerl:values_es(Expr),
		     	BValues = lists:map(fun(Var) -> bind_vars(Var,Env) end,Values),
		     	{c_values,cerl:get_ann(Expr),BValues};
		    'tuple' -> 
		    	NExps = [bind_vars(E,Env) || E <- cerl:tuple_es(Expr)],
		    	cerl:c_tuple(NExps);
		    'cons' -> 
		    	[NHd,NTl] = 
		    	  [bind_vars(E,Env) || 
		    	   E <- [cerl:cons_hd(Expr),cerl:cons_tl(Expr)]],
		    	cerl:c_cons(NHd,NTl);
		     _ -> Expr
		 end
	 end.

get_abstract_from_core_literal(Lit) ->
	{c_literal,_,Lit_} = Lit, 
	%io:format("Lit: ~p\nis_pid(Lit): ~p\n",[Lit_,is_pid(Lit_)]),
	case is_pid(Lit_) of 
		true -> 
			ALit = {string,1,pid_to_list(Lit_)};
		false -> 
			case is_list(Lit_) of 
				true ->
					case Lit_ of 
						[H|T] ->
							ALit = 
								{cons,1,
									get_abstract_from_core_literal(cerl:abstract(H)),
									get_abstract_from_core_literal(cerl:abstract(T))};
						[] ->
							ALit = {nil,1}
					end;
				false ->
					case is_tuple(Lit_) of 
						true ->
							ALit = 
								{tuple,1,
									[get_abstract_from_core_literal(cerl:abstract(Elem)) 
									 || Elem <- tuple_to_list(Lit_)]};
						false -> 
							{ok,[ALit|_]} = edd_lib:parse_expr(lists:flatten(io_lib:format("~w",[Lit_])++".")),
							ALit
					end
			end	
	end,
	ALit.

get_MFA(Function) ->
	{c_call,_,{c_literal,_,erlang},{c_literal,_,make_fun},MFA} = Function,
	[{c_literal,_,ModName},{c_literal,_,FunName},{c_literal,_,FunArity}] = MFA,
	{ModName,FunName,FunArity}.
	      	
create_new_env([{c_var,_,VarName}|TailArgs],[Value|TailPars],Env) ->
	ets:insert(Env,{VarName,Value}),
	create_new_env(TailArgs,TailPars,Env);
create_new_env([],[],_) -> ok.

add_bindings_to_env([{{c_var,_,VarName},Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([{VarName,Value}| TailBindings],Env) ->
	ets:insert(Env,{VarName,Value}),
	add_bindings_to_env(TailBindings,Env);
add_bindings_to_env([],_) -> ok.
	
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
		    	    

extract_module_from_ann([_,{file,File}]) ->
	[_,_,_,_|ModName_] = lists:reverse(File),
	list_to_atom(lists:reverse(ModName_)).
	
check_errors(Value) -> 
        %possiblemente faltan mas tipos de errores.
	case Value of
	     {c_literal,[],{error,_}} -> true;
	     stuck_receive -> true;
	     _ -> false
	end.
	
% get_anon_func(EnvAF,FunName,FunCore) ->
% 	get_anon_func(ets:lookup(EnvAF,FunName),FunCore).
	
% get_anon_func([{_,AF = {anonymous,FunCore,_,_,_}}|_],FunCore) -> AF;
% get_anon_func([_|AFs],FunCore) -> get_anon_func(AFs,FunCore);
% get_anon_func([],_)->{}.

get_clause_number([Clause|_],Clause,N) -> N;
get_clause_number([_|Clauses],Clause,N) -> get_clause_number(Clauses,Clause,N+1);
get_clause_number([],_,_) -> -1.

apply_substitution(Expr,Env,Bindings) ->
	cerl_trees:map(
		fun(T)->
			case cerl:type(T) of
			     'var' -> 
			     	case cerl:var_name(T) of
			     	     {_,_} -> T;
			     	     VN ->
			     	     	case ets:lookup(Env,VN) of
			     	     	     [] -> 
			     	     	     	Values = 
			     	     	     	  [Value || {{c_var,_,VarName},Value} <- Bindings,
			     	     	     	            VarName =:= VN],
			     	     	     	case Values of
			     	     	     	     [] -> T;
			     	     	     	     [Value|_] -> 
			     	     	     	     	Value
			     	     	     	end;
			     	     	     [{_,Value}|_] ->
			     	     	     	Value
			     	     	end
			     	end;
			     _ -> T
			end
		end,Expr).

get_expression_from_abstract(File,Line,Type) ->
	case smerl:for_file(File) of 
		{ok,Abstract} ->
			lists:flatten(
				[erl_syntax_lib:fold(
					fun(Tree,Acc) -> 
						%io:format("{Line,Tree}: ~p\n",[{Line,Tree}]),
			       		case Tree of 
			       			{'receive',Line,_} when Type == 'receive' ->
								[Tree|Acc];
							{'receive',Line,_,_,_} when Type == 'receive' ->
								[Tree|Acc];
			     %   			{'var',Line,_} when Type == 'var' ->
								% [Tree|Acc];
			    %    			{'integer',Line,_} when Type == 'literal' ->
							% 	[Tree|Acc];
							% {'float',Line,_} when Type == 'literal' ->
							% 	[Tree|Acc];
							% {'string',Line,_} when Type == 'literal' ->
							% 	[Tree|Acc];
							% {'atom',Line,_} when Type == 'literal' ->
							% 	[Tree|Acc];
							% {'nil',Line} when Type == 'literal' ->
							% 	[Tree|Acc];
							% {'call',Line,_,_} when Type == 'call' ->
							% 	[Tree|Acc];
							% {'op',Line,_,_,_} when Type == 'call' ->
							% 	[Tree|Acc];
							% {'op',Line,_,_} when Type == 'call' ->
							% 	[Tree|Acc];
			    %    			{'cons',Line,_,_} when Type == 'cons' ->
							% 	[Tree|Acc];
							% {'tuple',Line,_} when Type == 'tuple' ->
							% 	[Tree|Acc];
							% {'match',Line,_,_}  when Type == 'match' ->
							% 	[Tree|Acc];
							% {'case',Line,_,_} when Type == 'case' ->
							% 	[{Tree,"case"}|Acc];
							% {'if',Line,_} when Type == 'case' ->
							% 	[{Tree,"if"}|Acc];
							% {'lc',Line,_,_} when Type == 'lc' ->
							% 	[Tree|Acc];
							_ -> 
								Acc
			       		end
				     end, [], Form) || Form<-smerl:get_forms(Abstract)]);
		_ -> 
			[]
	end.

get_file_line(Expr) ->
	case cerl:get_ann(Expr) of 
		[_,Line,{file,File}] ->
			 [Line,File];
		[Line,{file,File}] ->
			[Line,File];
		[Line,{file,File}|_] ->
			[Line,File];
		[compiler_generated] ->
			[];
		[] ->
			[]
	end.

get_next_spawned([],Acc) ->
	{no_pid,Acc};
get_next_spawned([H|T],Acc) ->
	case H of 
		{trace,_,_,spawn,SpawnPid_,_} ->
			{SpawnPid_,Acc ++ T};
		_ ->
			get_next_spawned(T,Acc ++ [H])
	end.

get_next_sent([], _) ->
	not_found;
get_next_sent([H|T],Acc) ->
	case H of 
		 {trace,_,_,send,_,_} ->
			Acc ++ T;
		_ ->
			get_next_sent(T, Acc ++ [H])
	end.

get_next_receipt([], _) ->
	not_found;
get_next_receipt([H|T],Acc) ->
	case H of 
		{trace,_,_,'receive',_} ->
			Acc ++ T;
		_ ->
			get_next_receipt(T, Acc ++ [H])
	end.


get_args_from_core_args(SpawnArgs) ->
	%io:format("SpawnArgs: ~p\n", [SpawnArgs]),
	case cerl:type(SpawnArgs) of 
		cons ->
			Tail = 
				case get_args_from_core_args(cerl:cons_tl(SpawnArgs)) of 
					{c_literal,_,[]} ->
						[];
					Tail_ ->
						Tail_
				end,
			[cerl:cons_hd(SpawnArgs) | Tail];
		nil -> 
			[];
		literal ->
			case cerl:concrete(SpawnArgs) of 
				List = [_|_] -> 
					[cerl:abstract(Elem) || Elem <- List];
				Other -> 
					cerl:abstract(Other)
			end;
		_ -> 
			throw({error,"Args from spawn badly formatted",SpawnArgs})
	end.

clean_stack([]) ->
	{none,[]};
clean_stack([{_,IdParent,{'receive',_,_,_,_,_,_,_}}|T]) ->
	{IdParent,T};
clean_stack([{Id,IdParent,CallReceive}|T]) ->
	{NIdParent,NT} = clean_stack(T),
	case NIdParent of 
		none -> 
			{none,[{Id,IdParent,CallReceive}|NT]};
		_ -> 
			{none,[{Id,NIdParent,CallReceive}|NT]}
	end.

get_core_vars([Var|T]) ->
	case atom_to_list(Var) of 
		[$c,$o,$r | _] -> 
			[Var|get_core_vars(T)];
		_ ->
			get_core_vars(T)
	end;
get_core_vars([]) ->
	[].


get_dependences(Vars, Env, PidScheduler) ->
	get_dependences(Vars, Env, PidScheduler, []).

get_dependences([Var | Vars], Env, PidScheduler, Acc) ->
	VarName = 
		case Var of
			{c_var,_,VarName_} ->
				VarName_;
			_ -> 
				Var
		end,
	%io:format("Variable: ~p\nValues: ~p\n",[VarName,ets:lookup(Env,VarName)]),
	Deps = 
		case ets:lookup(Env,VarName) of 
			[] -> [];
			[{_,Value}|_] ->
				case atom_to_list(VarName) of 
					[$c,$o,$r | _] -> 
						[];
					[$r,$e,$c | _] -> 
						[];
					_ -> 
						ConcreteValue = 
							case Value of 
								{anonymous_function,_,_,_,_,_} -> 
									get_abstract_form(Value, PidScheduler);
								{c_call,_,_,_,_} -> 
									get_abstract_form(Value, PidScheduler);
								_ -> 
									%io:format("Value: ~p\n",[Value]),
									cerl:concrete(Value)
							end,
						[{VarName, ConcreteValue}]
				end
		end,
	get_dependences(Vars, Env, PidScheduler, Acc ++ Deps);
get_dependences([], _, _, Acc) ->
	%io:format("Acc: ~p\n", [Acc]),
	lists:usort(lists:flatten(Acc)).