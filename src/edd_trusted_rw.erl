-module(edd_trusted_rw).

-export([read/1, write/1]).

read(Module) -> 
	read_from_file(atom_to_list(Module) ++ ".erl").

read_from_file(File) -> 
	{ok,Forms} = 
		epp:parse_file(File, [], []), 
	F = 
		fun (Form, Acc) ->
			form(Form, Acc)
		end,
	lists:reverse(lists:foldl(F, [], Forms)).

form({function, _L, edd_trusted, 0, [Clause]}, Acc) ->
	get_trusted_funs(erl_syntax:clause_body(Clause)) ++ Acc;
form(_, Acc) ->
    Acc.

get_trusted_funs(Body) ->
	try
		Elements = 
			erl_syntax:list_elements(hd(Body)),
		[ 
			begin
				[Mod, Fun, Ari] = 
					erl_syntax:tuple_elements(E),
				{
					erl_syntax:atom_value(Mod), 
					erl_syntax:atom_value(Fun), 
					erl_syntax:integer_value(Ari)
				}
	    	end
		|| E <- Elements]
	catch 
		_:_ -> 
			[]
	end.


write(Module) ->
	StrModule = 
		atom_to_list(Module) ++ ".erl", 
	{ok, IODeviceR} = 
		file:open(StrModule, [read]),
	Read0 = 
		read_file(IODeviceR),
	file:close(IODeviceR),
	Read = 
		remove_previous_info(Read0, []),
	{ok, IODeviceW} = 
		file:open(StrModule, [write]),
	FunTrusted = 
		"\nedd_trusted() ->\n\t[" 
		++  lists:join(
				", ", 
				lists:map(
					fun(TI) -> 
						edd_con_lib:format("~p", [TI])
					end,
					get(trusted_functions)
				)
			)
		++"].\n",
	StrIni = 
		"\n% begin edd trusted\n",
	StrEnd = 
		"\n% end edd trusted\n",
	StrProgram = 
		Read ++ StrIni ++ FunTrusted ++ StrEnd,
	file:write(IODeviceW, list_to_binary(StrProgram)),
	file:close(IODeviceW),
	ok.


remove_previous_info([$%,$ ,$b,$e,$g,$i,$n,$ ,$e,$d,$d,$ ,$t,$r,$u,$s,$t,$e,$d | Tail], Acc) ->
	remove_previous_info_until(Tail, Acc);
remove_previous_info([Other | Content],Acc) ->
	remove_previous_info(Content, [Other|Acc]);
remove_previous_info([],Acc) ->
	lists:reverse(Acc).

remove_previous_info_until([$%,$ ,$e,$n,$d,$ ,$e,$d,$d,$ ,$t,$r,$u,$s,$t,$e,$d | Tail], Acc) ->
	lists:reverse(lists:reverse(Tail) ++ Acc);
remove_previous_info_until([_ | Content],Acc) ->
	remove_previous_info_until(Content, Acc);
remove_previous_info_until([],Acc) ->
	lists:reverse(Acc).



read_file(IODevice) -> 
	read_file(IODevice,[],[]).
	
read_file(IODevice,PrevData,Acc) ->
	% io:format("A: ~p\nB: ~p\n",[PrevData,Acc]),
	case io:request(IODevice, {get_line, ''}) of
	     eof -> Acc++PrevData;
	     Data -> read_file(IODevice,Data,Acc++PrevData)
	     
	end.

