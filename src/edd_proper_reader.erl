-module(edd_proper_reader).

-export([
		read/1, read_file/1, 
		read_from_clause/1, put_attributes/1, 
		get_initial_set_of_nodes/4]).

% Test check: proper:check(rat_eqc:floor_1(),[{Arg1, Arg2}]). 
% Type check: proper_typeserver:demo_is_instance(1, rat, "integer()").

read(Module) -> 
	read_file(atom_to_list(Module) ++ ".erl").

read_file(File) -> 
	io:format("File: ~p\n", [File]),
	{ok,Forms} = 
		epp:parse_file(File, [], []), 
	put_attributes(Forms),
	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	AllTestsCandidates = 
		lists:reverse(lists:foldl(F, [], Forms)),
	ProperTest = 
		lists:foldl(
			fun separeteUsefulDiscarded/2,
			{[], []},
			AllTestsCandidates),
	% {ok, IODeviceW} = 
	% 	file:open("result.txt",[write]),
	% file:write(IODeviceW, list_to_binary(lists:flatten(io_lib:format("~p\n", [ProperTest])))),
	% file:close(IODeviceW),
	ProperTest.

separeteUsefulDiscarded({Fun, _, []}, {U, NU}) ->
	{U, [Fun | NU]};
separeteUsefulDiscarded(Fun, {U, NU}) ->
	{[Fun | U], NU}.

put_attributes(Forms) ->
	Module = 
		hd([Mod || {attribute,_,module,Mod} <- Forms]),
	put(module, Module),
	Exports = 
		lists:concat([Exports || {attribute,_,export,Exports} <- Forms]),
	put(exports, Exports),
	AllFuns = 
		[{Name, Arity} || {function, _, Name, Arity, _} <-  Forms],
	put(all_funs, AllFuns).

form({function, _, Name, 0, Clauses}, Acc) ->
	Mod = get(module),
	SuffixComplete = 
		"_complete",
	StrName = 
		atom_to_list(Name),
	IsComplete = 
		case length(StrName) < length(SuffixComplete) of 
			true -> 
				false;
			false -> 
				case string:sub_string(StrName, length(StrName) - length(SuffixComplete) + 1,  length(StrName)) of 
					SuffixComplete ->
						true;
					_ -> 
						false

				end
		end,
	case Clauses of 
		[Clause] -> 
			[{{Mod,Name,0}, IsComplete, read_from_clause(Clause)}] ++ Acc;
		_ -> 
			Acc 
	end;
form(_, Acc) ->
    Acc.

read_from_clause(Clause) ->
	read_from_clause(Clause, []).

read_from_clause(Clause0, Acc) -> 
	Clause = 
		% erl_syntax_lib:map(fun remotize/1, Clause0),
		Clause0,
	lists:foldl(
		fun extract_forAll/2, 
		Acc, 
		erl_syntax:clause_body(Clause)).

extract_forAll(Application, Acc) ->
	try
		ForAllFun = 
			erl_syntax:application_operator(Application),
		[TypeDescAST, FunTest] = 
			erl_syntax:application_arguments(Application),
		proper = 
			erl_syntax:atom_value(
				erl_syntax:module_qualifier_argument(ForAllFun)),
		forall = 
			erl_syntax:atom_value(
				erl_syntax:module_qualifier_body(ForAllFun)),
		% The first parameter (the type descriptors) can only be a call or a tuples of calls
		TypeDesc = 
			get_type_descriptors(TypeDescAST),
		[ClauseFunTest] = 
			erl_syntax:fun_expr_clauses(FunTest),
		[PatternFunTest] = 
			erl_syntax:clause_patterns(ClauseFunTest),
		VarsPattern = 
			[VarName || {var, _, VarName} <- get_vars_test(PatternFunTest)],
		TypesArgs = 
			lists:zip(VarsPattern, TypeDesc),
		[BodyTest] = 
			erl_syntax:clause_body(ClauseFunTest),
		Calls = 
			extract_calls(BodyTest),
		AllFunsCalled = 
			[{erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))} 
			|| C <- Calls, not(is_trusted_fun({erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))}))],
		UsableCalls = 
			lists:foldl(
				fun(Call, CurrentUsableCalls) -> 
					usable_calls(Call, CurrentUsableCalls, VarsPattern, AllFunsCalled) 
				end, 
				[], 
				Calls),
		case UsableCalls of 
			[] -> 
				Acc;
			_ ->
				[{TypesArgs, UsableCalls}| Acc]
		end
	catch 
		_:_ -> 
			% io:format("Failing: ~p\n", [Application]),
			Acc
	end.

get_type_descriptors(N) ->
	case erl_syntax:type(N) of 
		tuple ->
			lists:concat( 
				lists:map(
					fun get_type_descriptors/1,
					erl_syntax:tuple_elements(N)));
		application ->
			[N]
	end.

get_vars_test(N) ->
	case erl_syntax:type(N) of 
		tuple ->
			lists:concat( 
				lists:map(
					fun get_vars_test/1,
					erl_syntax:tuple_elements(N)));
		variable ->
			[N]
	end.

extract_calls(Node) -> 
	erl_syntax_lib:fold(fun add_if_call/2, [], Node).

add_if_call(Node, Acc) -> 
	case erl_syntax:type(Node) of 
		application -> 
			[Node | Acc];
		_ ->
			% io:format("Node: ~p\nType: ~p\n", [Node, erl_syntax:type(Node)]), 
			Acc
	end.

usable_calls(Call, Acc, VarsPattern, AllFunsCalled) ->
	Args = 
		erl_syntax:application_arguments(Call),
	OnlyVarsArgs = 
		[V || {var,_,V} <- Args],
	WithoutVarsArgs = 
		lists:foldl(
			fun add_if_does_not_have_vars/2,
			[],
			Args),
	case length(Args) =:= (length(OnlyVarsArgs) + length(WithoutVarsArgs)) of 
		true ->
			case lists:usort(VarsPattern) == lists:usort(OnlyVarsArgs) of 
				true -> 
					Op = 
						erl_syntax:application_operator(Call),
					% io:format("~p\n~p\n",[Op, is_trusted_fun({Op, length(Args)})]),
					case is_trusted_fun({Op, length(Args)}) of 
						true -> 
							Acc;
						false ->
							RestOfFuns =  
								lists:usort(AllFunsCalled -- [{Op, length(Args)}]),
							[{Op, Args, RestOfFuns} | Acc]
					end;
				false -> 
					Acc 
			end;
		false -> 
			Acc
	end.

add_if_does_not_have_vars(N, Acc) ->
	VarsInN = 
		erl_syntax_lib:fold(
			fun get_vars/2,
			[],
			N),
	case VarsInN of 
		[] ->
			[N | Acc];
		_ -> 
			Acc
	end.

get_vars(N, Acc) -> 
	case erl_syntax:type(N) of 
		variable ->
			[N | Acc];
		_ ->
			Acc
	end.

is_trusted_fun({FunName, Arity}) -> 
	case erl_syntax:type(FunName) of 
		atom -> 
			FunNameAtom = 
				erl_syntax:atom_value(FunName),
			AllFuns = 
				get(all_funs),
			case lists:member({FunNameAtom, Arity}, AllFuns) of 
				true -> 
					false;
				false -> 
					true 
			end;
		module_qualifier ->
			ModNameAtom = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(FunName)),
			ModName = 
				atom_to_list(ModNameAtom),
			FileAdress = 
				code:where_is_file(ModName ++ ".erl"),
			NFileAdress = 
			   	case FileAdress of
			        non_existing -> 
			     		NFileAdress_ = 
			     			code:where_is_file(ModName ++ ".beam"),
			     		case NFileAdress_ of
			     		     non_existing -> 
			     		     	% io:format("PATHS: ~p\n",[code:get_path()]),
			     		     	throw({error,"Non existing module", ModName});
			     		     _ -> 
			     		     	RelPath = 
			     		     		"ebin/" ++ ModName ++ ".beam",
			     		     	NRelPath = 
			     		     		"src/" ++ ModName ++ ".erl",
			     		     	PrevPath = 
			     		     	   lists:sublist(
			     		     	   		NFileAdress_,1,
										length(NFileAdress_) - length(RelPath)),
			     		     	PrevPath ++ NRelPath
			     		end;
			     	_ -> 
			     		FileAdress
			   end,
			lists:prefix(code:lib_dir(), NFileAdress)
	end.

get_initial_set_of_nodes(G, TrustedFunctions, Root, TestFiles) -> 
	Modules = 
		lists:usort(
			[element(1, edd_lib:get_MFA_Label(G,V)) 
		 	 || V <- digraph:vertices(G)]) -- 
		lists:usort(
			[case element(1, edd_lib:get_MFA_Label(G,V)) of 
				Elem = {'fun',_,_} -> 
					Elem; 
				_ ->
					ok 
			 end
		 	 || V <- digraph:vertices(G)]),
	% io:format("Loading from files ~p and modules ~p\n", [TestFiles, Modules]),
	Tests = 
		lists:flatten(
			[element(1, edd_proper_reader:read(Module) )
			 || Module <- Modules]
			++ [ element(1, edd_proper_reader:read_file(File))
			 || File <- TestFiles]),
	io:format("Tests : ~p\n", [Tests]),
	io:format("TrustedFunctions : ~p\n", [TrustedFunctions]),
	VerticesValidity = 
		lists:flatten(
			[
				begin 
					case get_call_vertex(G, V) of 
						none -> 
							[{V, unknown}];
						FunAndArgs -> 
							Validity = 
								get_validity_from_proper(
									FunAndArgs, TrustedFunctions, Tests),
							io:format("{V, Validity}: ~p\n", [{V, Validity}]),
							{V, Validity}
					end
				end 
			|| V <- digraph:vertices(G)]),
	edd_lib:dot_graph_file_colors(
		G, 
		"colors", 
		[V || {V, valid} <- VerticesValidity], 
		[V || {V, no_valid} <- VerticesValidity]),
	% % io:format("VerticesInTests : ~p\n", [VerticesInTests]),
	% VerticesNotValidFromPositiveTests = 
	% 	lists:flatten([begin 
	% 		{CallV,ValueV} = edd_lib:get_call_value_string(G,V),
	% 		[V 
	% 			|| 	{CallT, ValueT, equal} <- Tests,
	% 			 	CallV == CallT, ValueV /= ValueT] 
	% 	 end || V <- digraph:vertices(G)]),
	% % io:format("VerticesNotValidFromPositiveTests : ~p\n", [VerticesNotValidFromPositiveTests]),
	% ValidFromTest = 
	% 	[V || {V, equal} <- VerticesInTests],
	% NotValidFromTest = 
	% 	[V || {V, not_equal} <- VerticesInTests] 
	% 	++ VerticesNotValidFromPositiveTests,
	% % io:format("{ValidFromTest,NotValidFromTest} : ~p\n", [{ValidFromTest,NotValidFromTest}]),
	% IniValid_ = lists:usort(ValidFromTest ++ ValidTrusted),
	% IniNotValid_ = lists:usort([Root | NotValidFromTest]),
	% NewValidTests = 
	% 	case lists:usort(ValidFromTest) of 
	% 		IniValid_ ->
	% 			% Trusted nodes were already as test cases
	% 			[];
	% 		_ ->
	% 			lists:usort(ValidTrusted) -- lists:usort(ValidFromTest)
	% 	end,
	% NewNotValidTests = 
	% 	case lists:usort(NotValidFromTest) of 
	% 		IniNotValid_ ->
	% 			% Root was already as a test case
	% 			[];
	% 		_ ->
	% 			[Root]
	% 	end,
	% put(test_to_NOT_store, {IniValid_ -- NewValidTests, IniNotValid_ -- NewNotValidTests}),
	% {IniValid_, IniNotValid_}.
	ok.

check_call_with_property([{ModuleTest, FunTest, IsComplete, Pars, Dict} | Tests]) -> 
	% proper:check(rat_eqc:floor_1(),[{Arg1, Arg2}]). 
	ArgsValue = 
		case Pars of 
			[] -> 
				erl_syntax:list();
			[Var] ->
				erl_syntax:list(
					[element(2,hd(Dict))]);
			_ ->
				erl_syntax:list([
					erl_syntax:tuple(
						lists:map(
							fun({Var,_}) -> 
								{Var, Expr} = 
									lists:keyfind(Var, 1, Dict),
								Expr
							end,
							Pars)
					)]
				)
		end,
	CheckCall = 
		erl_syntax:application(
			erl_syntax:atom(proper),
			erl_syntax:atom(check),
			[
				erl_syntax:application(
					erl_syntax:atom(ModuleTest), 
					erl_syntax:atom(FunTest), 
					[]),
				ArgsValue,
				erl_syntax:list(
					[erl_syntax:atom(quiet)])
			]
		),
		% Check the property value and decide what to do depending on whether is complete or not
	% io:format("~s\n", [erl_prettypr:format(CheckCall) ]),
	{value, Result, _}	=
		erl_eval:expr(erl_syntax:revert(CheckCall), []),
	% io:format("~p\n", [{Result, IsComplete}]),
	case {Result, IsComplete} of 
		{true, true} ->
			valid;
		{false, _} ->
			no_valid;
		{true, _} ->
			check_call_with_property(Tests)
	end;
check_call_with_property([]) ->
	unknown.
	

get_validity_from_proper(FunAndArgs, TrustedFunctions, Tests) -> 
	lists:foldl(
		fun
			(_, valid) ->
				valid;
			(_, no_valid) ->
				no_valid;
			(T, unknown) -> 
				TestBindings = 
					get_test_bindings(
						FunAndArgs, T, TrustedFunctions),
				check_call_with_property(TestBindings)
		end,
		unknown,
		Tests).

get_test_bindings(
		FunAndArgs, 
		{{ModuleTest,FunTest,0}, IsComplete, [{Pars, UsableCalls}]}, 
		TrustedFunctions) -> 
	compile:file(atom_to_list(ModuleTest) ++ ".erl", [debug_info]),
	Dicts = 
		lists:foldl(
			fun(UsableCall, CAcc) -> 
				get_compatible_usable_tests(
					ModuleTest, FunTest, Pars, UsableCall, 
					FunAndArgs, TrustedFunctions, CAcc)
			end,
			[],
			UsableCalls),
	[{ModuleTest, FunTest, IsComplete, Pars, Dict} 
	 || Dict <- Dicts].


get_compatible_usable_tests(
		ModuleTest,
		FunTest, 
		ParsTest,
		{FunUsable, ArgsUsable, RestOfFuns}, 
		{FunVertex, ArgsVertex}, 
		TrustedFunctions, 
		Acc) -> 
	% 	io:format("A: ~p\nB: ~p\n", [{FunVertex, ArgsVertex}, {FunUsable, ArgsUsable}]),
	case same_fun(ModuleTest, FunVertex, FunUsable) of 
		true ->
			% io:format("Equal_A: ~p\nEqual_B: ~p\n", [FunVertex, FunUsable]),
			% io:format("RestOfFuns; ~p\n", [RestOfFuns]),
			TupledRestOfFuns = 
				[tuple_function(ModuleTest, NeededFun, Arity) 
				 || {NeededFun, Arity} <- RestOfFuns],
			case (TupledRestOfFuns -- TrustedFunctions) of 
				[] -> 
					% io:format("Needed functions are trusted\n"),
					% io:format("Arguments:\n~p\n", [{ArgsVertex, ArgsUsable}]), 
					case compatible_args(lists:zip(ArgsVertex, ArgsUsable), ModuleTest, []) of 
						false -> 
							% io:format("The arguments are NOT compatible\n", []), 
							Acc;
						{true, Dict} ->
							% io:format("The arguments are compatible:\n~p\n", [Dict]), 
       						DictWithTypes = 
       							[begin 
       								{Var, Type} = 
       									lists:keyfind(Var, 1, ParsTest),
       								{Var, Type, Value}
       							end
       							|| {Var, Value} <- Dict],
       						TypeCheckCalls = 
       							[
       								% Type check: proper_typeserver:demo_is_instance(1, rat, "integer()").
		       						erl_syntax:application(
										erl_syntax:atom(proper_typeserver),
										erl_syntax:atom(demo_is_instance),
										[
											Value,
										 	erl_syntax:atom(ModuleTest),
										 	erl_syntax:string(erl_prettypr:format(Type))
										])
		       					|| {_, Type, Value} <- DictWithTypes],
		       				% [
		       				% 	io:format(erl_prettypr:format(Call) ++ "\n") 
		       				% || Call <- TypeCheckCalls],
		       				AllTypesCorrect = 
		       					lists:all(
		       						fun
		       							(true) -> 
		       								true;
		       							(_) ->
		       								false 
		       						end,
				       				[
				       					begin 
				       						% TODO: Proper does not find the user-defined types
					       					{value, TypeCorrect, _}	= 
					       						erl_eval:expr(erl_syntax:revert(Call), []),
					       					% io:format("~p\n", [TypeCorrect]),
					       					TypeCorrect
				       					end 
				       				|| Call <- TypeCheckCalls]),
		       				% io:format("All: ~p\n", [AllTypesCorrect]),
		       				case AllTypesCorrect of 
		       					true -> 
		       						% io:format("Type constraints are hold."),
		       						[Dict | Acc];
		       					false ->
		       						% io:format("Type constraints are NOT hold."),
		       						Acc
		       				end
					end;
				_ ->
					% io:format("Needed functions are NOT trusted\n"),
					Acc 
			end;
		false ->
			Acc 
	end.

same_fun(ModuleTest, FunVertex, FunUsable) -> 
	ModuleVertex = 
		erl_syntax:atom_value(
			erl_syntax:module_qualifier_argument(FunVertex)),
	FunNameVertex = 
		erl_syntax:atom_value(
			erl_syntax:module_qualifier_body(FunVertex)),		
	case {erl_syntax:type(FunUsable), ModuleVertex == ModuleTest} of 
		{atom, true} -> 
			FunNameUsable = 
				erl_syntax:atom_value(FunUsable),
			FunNameVertex == FunNameUsable;
		{module_qualifier, _} -> 
			ModuleUsable = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(FunUsable)),
			FunNameUsable = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_body(FunUsable)),
			(ModuleVertex  == ModuleUsable) and (FunNameVertex == FunNameUsable)
	end. 

compatible_args([{ArgV, ArgT} | Args], ModuleTest, Dict) -> 
	case erl_syntax:type(ArgT) of
		variable -> 
			Var = 
				erl_syntax:variable_name(ArgT),
			case [Item || Item = {Var_, _} <- Dict, Var_ == Var] of 
				[] ->
					compatible_args(Args, ModuleTest, [{Var, ArgV} | Dict]);
				[{Var, PreviousValue}]  -> 
					StrArgV = 
						erl_prettypr:format(ArgV),
					StrArgPV = 
						erl_prettypr:format(PreviousValue),
					% io:format("~s == ~s\n", [StrArgV, StrArgPV]),
					case (StrArgV == StrArgPV) of 
						true -> 
							compatible_args(Args, ModuleTest, Dict);	
						false -> 
							false
					end
			end;
		_ ->
			StrArgV = 
				erl_prettypr:format(ArgV),
			StrArgT = 
				erl_prettypr:format(
					remotize_implicit_fun(ArgT, ModuleTest)),
			% io:format("~s == ~s\n", [StrArgV, StrArgT]),
			case (StrArgV == StrArgT) of 
				true -> 
					compatible_args(Args, ModuleTest, Dict);
				false -> 
					false
			end
	end;
compatible_args([], ModuleTest, Dict) ->
	{true, Dict}.

remotize_implicit_fun(Node, ModuleTest) ->
	case erl_syntax:type(Node) of 
		implicit_fun -> 
			FunNameArity = 
				erl_syntax:implicit_fun_name(Node),
			FunName = 
				erl_syntax:arity_qualifier_body(FunNameArity),
			Arity = 
				erl_syntax:arity_qualifier_argument(FunNameArity),
			case erl_syntax:type(FunName) of 
				atom -> 
					erl_syntax:implicit_fun(
						erl_syntax:arity_qualifier(
							erl_syntax:module_qualifier(
								erl_syntax:atom(ModuleTest),
								FunName),
							Arity
						)
					);
				module_qualifier -> 
					Node
			end;
		_ ->
			Node
	end.

tuple_function(ModuleTest, NeededFun, Arity) -> 
	case erl_syntax:type(NeededFun) of 
		atom -> 
			FunNameNeeded = 
				erl_syntax:atom_value(NeededFun),
			{ModuleTest, FunNameNeeded, Arity};
		module_qualifier -> 
			ModuleNeeded = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_argument(NeededFun)),
			FunNameNeeded = 
				erl_syntax:atom_value(
					erl_syntax:module_qualifier_body(NeededFun)),
			{ModuleNeeded, FunNameNeeded, Arity}
	end.	

get_call_vertex(G, V) -> 
	{Vertex,{Label,_,File,Line}} = 
		digraph:vertex(G, V),
	{ok,Toks,_} = 
		erl_scan:string(lists:flatten(Label)++"."),
	{ok,[Aexpr|_]} = 
		erl_parse:parse_exprs(Toks),
	{match,_,Call = {call, _, Called, Args},_} = 
		Aexpr,
	case Called of 
		{remote,_,_,_} ->
			{Called, Args};
		_ ->
			none
	end.
