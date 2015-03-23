%%%=============================================================================
%%% @doc
%%% @author Salvador Tamarit <tamarit27@gmail.com> 
%%% @end
%%%=============================================================================

-module(edd_server).

%% Exports
-export([start/0]).
-export([server_entry_point/1]).

-define(DEF_PORT, 		6667).


%%%_* application callbacks ----------------------------------------------------

%% @doc Start a chat server API in port 
-spec start() -> ok.
start() ->
	server(?DEF_PORT).

%%%_* External functions -------------------------------------------------------

%% @doc Chat server control entry point
-spec server_entry_point(port()) -> ok.
server_entry_point(Socket) ->
	% {ok, Name, Users} = 
	% 	gen_server:call(chat_control, {connect, Socket}),
	% gen_server:cast(chat_control, 
	% 	{individual, Name, pack([?CONNECT, ?OK, Name])}),
	% gen_server:cast(chat_control, 
	% 	{individual, Name, pack([?USERSLIST, Users])}),
	% gen_server:cast(chat_control, 
	% 	{broadcast, Name, pack([?JOIN, Name])}),
    server_listener(Socket).

%%%_* Internal functions -------------------------------------------------------

%% @doc Start a chat server API in a given port
-spec server(integer()) -> ok.
server(Port) ->
	edd_control:start(),
    edd_tcp:start(?MODULE, Port, fun server_entry_point/1),
    ok.

%% @doc Server loop listing in the socket corresponding to a user
% -spec server_listener(port(), string()) -> ok.
server_listener(Socket) ->
	Dir = 
	    case gen_tcp:recv(Socket, 0) of
	        {ok, Data} ->
	            Dir0 = binary_to_list(Data),
	            io:format("Data: ~s~n", [Dir0]),
	            Dir0;
	        % Diconnect procedure
	        {error, closed} ->
	            disconnect(Socket)
	    end,
    case gen_tcp:send(Socket, "ok") of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end,
    BuggyCall = 
	    case gen_tcp:recv(Socket, 0) of
	        {ok, Data2} ->
	            BuggyCall0 = binary_to_list(Data2),
	            io:format("Data: ~s~n", [BuggyCall0]),
	 			BuggyCall0;
	        % Diconnect procedure
	        {error, closed} ->
	            disconnect(Socket)
	    end,
    case gen_tcp:send(Socket, "ok") of
        ok -> 
            ok;
        {error,closed} ->
            disconnect(Socket)
    end,
    Return = edd:dd_server(BuggyCall,Dir),
    io:format("Return: ~s\n",[edd_lib:json_graph(Return)]).


% %% @doc Broadcast a message in the chat
% -spec say(port(), string(), string()) -> ok.
% say(Socket, Name, Content) ->
%     gen_server:cast(chat_control, 
%     	{broadcast, Name, pack([?SAID, Name, Content])}),
%     server_listener(Socket, Name).

% %% @doc Disconnect a user from the chat
% -spec disconnect(port(), string()) -> ok.
disconnect(_Socket) ->
	% gen_server:call(chat_control, {disconnect, Name}),
	% gen_server:cast(chat_control, 
	% 	{individual, Name, pack([?CLOSE])}),
	% gen_server:cast(chat_control, 
	% 	{broadcast, Name, pack([?LEFT, Name])}), 
    ok.

% %% @doc Changes the name to a user
% -spec change_name(port(), string(), string()) -> ok.
% change_name(Socket, CurrentName, NewName) ->
% 	FinalName = 
% 		case string:tokens(NewName, ":") of 
% 			[_] ->  
% 				case gen_server:call(chat_control, 
% 						{change_name, CurrentName, NewName}) of 
% 					ok ->
% 						gen_server:cast(chat_control, 
% 							{individual, NewName, 
% 								pack([?NAME_CHANGE, ?OK])}), 
% 						gen_server:cast(chat_control, 
% 							{broadcast, NewName, 
% 								pack([?NAME_CHANGE, CurrentName, NewName])}),
% 						NewName;
% 					{error,used_name} ->
% 						gen_server:cast(chat_control, 
% 							{individual, CurrentName, 
% 								pack([?NAME_CHANGE, ?ERROR,?E_IN_USE])}), 
% 						CurrentName
% 				end;
% 			% The name contains character $:, so it's not valid
% 			_ -> 
% 				gen_server:cast(chat_control, 
% 					{individual, CurrentName, 
% 						pack([?NAME_CHANGE, ?ERROR,?E_NOT_VALID])}), 
% 				CurrentName
% 		end,
% 	server_listener(Socket, FinalName).

% %% @doc Sends a private message to a user
% -spec private_message(port(), string(), string(), string()) -> ok.
% private_message(Socket, Name, To, Msg) ->
% 	case gen_server:call(chat_control, {private_message, Name, To}) of 
% 		ok ->
% 			gen_server:cast(chat_control, 
% 				{individual, Name, pack([?PRIVATE, ?OK])}),
% 			gen_server:cast(chat_control, 
% 				{individual, To, pack([?PRIVATE, ?SAID, Name, Msg])});
% 		{error,same_user} ->
% 			gen_server:cast(chat_control, 
% 				{individual, Name, pack([?PRIVATE, ?ERROR, ?E_SAME_USER])});
% 		{error,user_not_exists} ->
% 			gen_server:cast(chat_control, 
% 				{individual, Name, pack([?PRIVATE, ?ERROR, ?E_NOT_EXISTS])})
% 	end,
% 	server_listener(Socket, Name).

% %% @doc Format a list of string to be sent to a client
% -spec pack([string()]) -> string().
% pack(List) ->
% 	string:join(List, ":") ++ "\n".

% %% @doc Remove the carriage return from the messages
% -spec remove_cr(string()) -> string().
% remove_cr(String) ->
% 	string:strip(String, right, $\n).
