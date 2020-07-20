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
	% io:format("File: ~p\n", [File]),
	{ok,Forms} = 
		epp:parse_file(File, [], []), 
	put_attributes(Forms),
	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	AllTestsCandidates = 
		lists:reverse(lists:foldl(F, [], Forms)),
	% io:format("~p\n", [AllTestsCandidates]),
	ProperTest = 
		lists:foldl(
			fun separeteUsefulDiscarded/2,
			{[], []},
			AllTestsCandidates),
	% io:format("~p\n", [ProperTest]),
	{ok, IODeviceW} = 
		file:open(File ++ "_result.txt",[write]),
	file:write(IODeviceW, list_to_binary(lists:flatten(io_lib:format("~p\n", [ProperTest])))),
	file:close(IODeviceW),
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
	% io:format("Function: ~p\n", [Name]),
	Mod = get(module),
	SuffixComplete = 
		"_complete",
	StrName = 
		atom_to_list(Name),
	IsComplete0 = 
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
	SuffixSubset = 
		"_subset",
	IsComplete =
		case IsComplete0 of 
			true -> 
				true;
			false -> 
				case length(StrName) < length(SuffixSubset) of 
					true -> 
						false;
					false -> 
						case string:sub_string(StrName, length(StrName) - length(SuffixSubset) + 1,  length(StrName)) of 
							SuffixSubset ->
								true;
							_ -> 
								false

						end
				end
		end,
	case Clauses of 
		[Clause] -> 
			[{{Mod,Name,0}, IsComplete, read_from_clause(Clause)} | Acc];
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
	% io:format("Application: ~p\n", [Application]),
	try
		% io:format("Application: ~p\n", [Application]),
		ForAllFun = 
			erl_syntax:application_operator(Application),
		% io:format("ForAllFun: ~p\n", [ForAllFun]),
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
		% io:format("TypeDesc: ~p\n", [TypeDesc]),
		[ClauseFunTest] = 
			erl_syntax:fun_expr_clauses(FunTest),
		% io:format("ClauseFunTest: ~p\n", [ClauseFunTest]),
		[PatternFunTest] = 
			erl_syntax:clause_patterns(ClauseFunTest),
		VarsPattern = 
			[VarName || {var, _, VarName} <- get_vars_test(PatternFunTest)],
		TypesArgs = 
			lists:zip(VarsPattern, TypeDesc),
		[BodyTest] = 
			erl_syntax:clause_body(ClauseFunTest),
		% io:format("BodyTest: ~p\n", [BodyTest]),
		Calls = 
			extract_calls(BodyTest),
		% io:format("Calls: ~p\n", [Calls]),
		CallNotInFun = 
			extract_calls_not_in_fun(BodyTest),
		% io:format("CallNotInFun: ~p\n", [CallNotInFun]),
		AllFunsCalled = 
			[{erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))} 
			|| C <- CallNotInFun, not(is_trusted_fun({erl_syntax:application_operator(C), length(erl_syntax:application_arguments(C))}))],
		% io:format("AllFunsCalled: ~p\n", [AllFunsCalled]),
		UsableCalls = 
			lists:foldl(
				fun(Call, CurrentUsableCalls) -> 
					usable_calls(Call, CurrentUsableCalls, VarsPattern, AllFunsCalled) 
				end, 
				[], 
				Calls),
		% io:format("UsableCalls: ~p\n", [UsableCalls]),
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

extract_calls_not_in_fun(Node) -> 
	erl_syntax_lib:fold(fun add_if_call_no_in_fun/2, [], Node).

add_if_call_no_in_fun(Node, Acc) -> 
	case erl_syntax:type(Node) of 
		fun_expr -> 
			Acc -- lists:append([extract_calls(C) || C <- erl_syntax:fun_expr_clauses(Node)]);
		application -> 
			[Node | Acc];
		_ ->
			% io:format("Node: ~p\nType: ~p\n", [Node, erl_syntax:type(Node)]), 
			Acc
	end.

usable_calls(Call, Acc, VarsPattern, AllFunsCalled) ->
	% io:format("Call: ~p\n", [Call]),
	% {record,53,complex,
    %                  [{record_field,53,{atom,53,real},{var,53,'R'}},
    %                   {record_field,53,{atom,53,img},{var,53,'I'}}]}
	Args = 
		erl_syntax:application_arguments(Call),
	FieldsVarsArgs = 
		lists:foldl(
			fun
				({record,_,_,FieldList}, AccVars) -> 
					% io:format("FieldList: ~p\n", [FieldList]),
					VarsInFields = [V || {record_field,_,{atom,_,_},{var,_,V}} <- FieldList],
					case length(FieldList) == length(VarsInFields) of 
						true -> 
							[VarsInFields|AccVars]; 
						false -> 
							AccVars
					end;
				(_, AccVars) -> 
					AccVars 
			end,
			[],
			Args),
		% [FieldList || {record,_,_,FieldList} <- Args],
	OnlyVarsArgs = 
		[V || {var,_,V} <- Args],
	WithoutVarsArgs = 
		lists:foldl(
			fun add_if_does_not_have_vars/2,
			[],
			Args),
	% io:format("FieldsVarsArgs: ~p\n", [FieldsVarsArgs]),
	% io:format("OnlyVarsArgs: ~p\n", [OnlyVarsArgs]),
	% io:format("WithoutVarsArgs: ~p\n", [WithoutVarsArgs]),
	case length(Args) =:= (length(OnlyVarsArgs) + length(WithoutVarsArgs) + length(FieldsVarsArgs))  of 
		true ->
			% io:format("~p\n", [lists:concat(FieldsVarsArgs)]),
			% io:format("~p\n", [lists:usort(VarsPattern)]),
			% io:format("~p\n", [lists:usort(OnlyVarsArgs ++ lists:concat(FieldsVarsArgs))]),
			case lists:usort(VarsPattern) == lists:usort(OnlyVarsArgs ++ lists:concat(FieldsVarsArgs)) of 
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
			% io:format("LD: ~s\nFA: ~s\n" , [code:lib_dir(), NFileAdress]),
			case ModNameAtom of 
				proper -> 
					true;
				_ -> 
					lists:prefix(code:lib_dir(), NFileAdress)
			end
	end.

get_initial_set_of_nodes(G, TrustedFunctions, TestFiles, {Valid0, NoValid0}) -> 
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
	% io:format("Tests: ~p\n", [Tests]),
	% io:format("TrustedFunctions : ~p\n", [TrustedFunctions]),
	VerticesValidity = 
		lists:flatten(
			[
				case lists:member(V, Valid0) of 
					true -> 
						{V, valid};
					false -> 
						case lists:member(V, NoValid0) of 
							true -> 
								{V, no_valid};
							false -> 
								case get_call_vertex(G, V) of 
									none -> 
										[{V, unknown}];
									FunAndArgs -> 
										Validity = 
											get_validity_from_proper(
												FunAndArgs, TrustedFunctions, Tests),
										% io:format("{V, Validity}: ~p\n", [{V, Validity}]),
										{V, Validity}
								end
						end
				end
			|| V <- digraph:vertices(G)]),
	Valid = 
		[V || {V, valid} <- VerticesValidity],
	NotValid = 
		[V || {V, no_valid} <- VerticesValidity],
	edd_lib:dot_graph_file_colors(
		G, 
		"colors", 
		Valid -- Valid0, 
		NotValid -- NoValid0),
	{Valid, NotValid}.

check_call_with_property([{ModuleTest, FunTest, IsComplete, Pars, Dict, NonTrustedFunsBody} | Tests]) -> 
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
	case {Result, IsComplete, NonTrustedFunsBody} of 
		{true, true, _} ->
			valid;
		{false, _, []} ->
			no_valid;
		{_, _, _} ->
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
	[{ModuleTest, FunTest, IsComplete, Pars, Dict, NonTrustedFunsBody} 
	 || {Dict, NonTrustedFunsBody} <- Dicts].


get_compatible_usable_tests(
		ModuleTest,
		FunTest, 
		ParsTest,
		{FunUsable, ArgsUsable, RestOfFuns}, 
		{FunVertex, ArgsVertex}, 
		TrustedFunctions, 
		Acc) -> 
	% io:format("A: ~p\nB: ~p\n", [{FunVertex, ArgsVertex}, {FunUsable, ArgsUsable}]),
	case same_fun(ModuleTest, FunVertex, FunUsable) of 
		true ->
			% io:format("Equal_A: ~p\nEqual_B: ~p\n", [FunVertex, FunUsable]),
			% io:format("RestOfFuns; ~p\n", [RestOfFuns]),
			TupledRestOfFuns = 
				[tuple_function(ModuleTest, NeededFun, Arity) 
				 || {NeededFun, Arity} <- RestOfFuns],
			case length(ArgsVertex) == length(ArgsUsable) of 
				true -> 
					% io:format("Same length\n"),
					% [io:format(erl_prettypr:format(AV) ++ "\n") || AV <- ArgsVertex],
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
									begin
										PPType0 = erl_prettypr:format(Type),
										PPType1 = re:replace(PPType0, "range\\(.*?\\)", "integer()", [ {return, list}]),
										PPType = re:replace(PPType1, "integer\\(.*?,.*?\\)", "integer()", [ {return, list}]),
										% Type check: proper_typeserver:demo_is_instance(1, rat, "integer()").
										{
											erl_syntax:application(
												erl_syntax:atom(proper_typeserver),
												erl_syntax:atom(demo_is_instance),
												[
													Value,
													erl_syntax:atom(ModuleTest),
													erl_syntax:string(PPType)
												]),
											PPType
										}
									end
		       					|| {_, Type, Value} <- DictWithTypes],
		       				% [
		       				% 	io:format(erl_prettypr:format(Call) ++ "\n") 
		       				% || {Call, _} <- TypeCheckCalls],
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
											% io:format(Type),
											% io:format("\n"),
											% % io:format("~p\n", [string:str("range(", Type)]),
											% CallCorrected = 
											% 	case string:str(Type, "range(") > 0 of 
											% 		true -> 
											% 			% range is integer (this solution should be reused in other types)
											% 			true;
											% 		false -> 
											% 	end,
											{value, TypeCorrect, _}	= 
												erl_eval:expr(erl_syntax:revert(Call), []),
											% io:format(erl_prettypr:format(Call) ++ "\n"),
											% io:format("~p\n", [TypeCorrect]),
											TypeCorrect
				       					end 
				       				|| {Call, _} <- TypeCheckCalls]),
		       				% io:format("All: ~p\n", [AllTypesCorrect]),
		       				case AllTypesCorrect of 
		       					true -> 
		       						% io:format("Type constraints are hold.\n"),
		       						[{Dict, TupledRestOfFuns -- TrustedFunctions} | Acc];
		       					false ->
		       						% io:format("Type constraints are NOT hold.\n"),
		       						Acc
		       				end
					end;
				false ->
					% io:format("No matchin length (maybe it is calling to a function with the same name but with a sdifferent arity)\n"),
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
	% io:format("~p\n", [{ArgV, ArgT}]),
	FunRecords = 
		fun(NameRecordT, ArgsFieldT) -> 
			% io:format("~p\n", [{NameRecordT, ArgsFieldT}]),
			case erl_syntax:type(ArgV) of 
				record_expr -> 
					{record, _, NameRecordV, ListFields} = ArgV,
					ArgsFieldV = [ArgField || {record_field, _, {atom, _, _}, ArgField} <- ListFields];
				tuple -> 
					{tuple, _, [{atom, 1, NameRecordV} | ArgsFieldV]} = ArgV;
				_ -> 
					NameRecordV = "THISISNOTGOINGTOMATCH",
					ArgsFieldV = ArgsFieldT
			end,
			case NameRecordV == NameRecordT of 
				true -> 
					compatible_args(lists:zip(ArgsFieldV, ArgsFieldT) ++ Args, ModuleTest, Dict);
				false -> 
					false
			end
		end,
	% io:format("~p\n", [erl_syntax:type(ArgT)]),
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
		record_expr ->
			{record, _, NameRecordT, ListFields} = ArgT,
			ArgsFieldT = [ArgField || {record_field, _, {atom, _, _}, ArgField} <- ListFields],
			FunRecords(NameRecordT, ArgsFieldT);
		tuple -> 
			{tuple, _, [{atom, 1, NameRecordT} | ArgsFieldT]} = ArgT,
			FunRecords(NameRecordT, ArgsFieldT);
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
