%%%=============================================================================
%%% @doc
%%% @author Salvador Tamarit <tamarit27@gmail.com> 
%%% @end
%%%=============================================================================

-module(edd_control).

-behaviour(gen_server).

%% Server API
-export([start/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Control server state
-record(control_state, 
    {}).


%%%_* API ----------------------------------------------------------------------
%% @doc Server API
-spec start() -> ok.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok.

%%%_* gen_server callbacks -----------------------------------------------------

init([]) ->
    {ok, #control_state{}}.

%% User connection
% handle_call({connect, Socket}, _From, State) ->
%     {UserName, NewState} = create_new_user(State, Socket),
%     {reply, {ok,UserName, user_list(NewState)}, NewState};
% %% User disconnection
% handle_call({disconnect, Name}, _From, 
%         State = #control_state{users = Users}) ->
%     {ok,[{Id,_Socket}|_]} = dict:find(Name, Users),
%     {reply, ok, remove_user(Id, Name, State)};
% %% Try to change the name to a user
% handle_call({change_name, CurrentName, NewName}, _From, 
%         State = #control_state{users = Users}) ->
%     {NewState,Reply} = 
%         case dict:is_key(NewName, Users) of 
%             false ->
%                 {ok,[{Id,Socket}|_]} = 
%                     dict:find(CurrentName, Users),
%                 NewState0 = reuse_id(Id, State),
%                 NewUsers = 
%                     dict:append(NewName, {none,Socket}, 
%                         dict:erase(CurrentName, Users)),
%                 {NewState0#control_state{users = NewUsers},ok};
%             true ->
%                 {State,{error,used_name}}
%         end,
%     {reply, Reply, NewState};
% %% Try to send a private message to another user
% handle_call({private_message, Name, To}, _From, 
%         State = #control_state{users = Users}) ->
%     Reply = 
%         case {dict:is_key(To, Users),Name == To} of
%             {true,false} ->
%                 ok;
%             {_,true} ->
%                 {error,same_user};
%             {false,_} ->
%                 {error,user_not_exists}
%         end,
%     {reply, Reply, State};
handle_call(_Message, _From, State) ->
    {reply, error, State}.


%% For messages to a single user
% handle_cast({individual, Name, Msg}, State) ->
%     NewState = individual(Name, Msg, State),
%     {noreply, NewState};
% %% For messages to all the user but Name
% handle_cast({broadcast, Name, Msg}, State) ->
%     NewState = broadcast(Name, Msg, State),
%     {noreply, NewState};
handle_cast(_Message, State) ->
    {noreply, State}.

% Definitions to avoid gen_server compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%%_* Internal functions -------------------------------------------------------


