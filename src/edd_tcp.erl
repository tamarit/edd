%%%=============================================================================
%%% @doc
%%% @author Salvador Tamarit <tamarit27@gmail.com> 
%%% @end
%%%=============================================================================

-module(edd_tcp).

-behavior(gen_server).

%% Server API
-export([start/3]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Exports
-export([accept_loop/1]).

%% TCP server state
-record(tcp_server_state, 
    { port          :: integer()
    , socket        :: port()
    , control       :: fun()
    }).

%% TCP options used when creating the socket
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%%_* API ----------------------------------------------------------------------
%% @doc Server API
-spec start(atom(), integer(), fun()) -> ok.
start(Name, Port, Control) ->
    State = #tcp_server_state{port = Port, control = Control},
    gen_server:start_link({local, Name}, ?MODULE, State, []),
    ok.

%%%_* gen_server callbacks -----------------------------------------------------

init(State = 
		#tcp_server_state{
			port = Port
		}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, Socket} ->
            {ok, accept(State#tcp_server_state{socket = Socket})};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#tcp_server_state{}) ->
    {noreply, accept(State)}.

% Definitions to avoid gen_server compile warnings
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%_* External functions -------------------------------------------------------

accept_loop({Server, Socket, Control}) ->
    {ok, Client_Socket} = gen_tcp:accept(Socket),
    gen_server:cast(Server, {accepted, self()}),
    Control(Client_Socket).

%%%_* Internal functions -------------------------------------------------------

accept(State = 
		#tcp_server_state{
			socket = Socket, 
			control = Control
		}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), Socket, Control}]),
    State.
