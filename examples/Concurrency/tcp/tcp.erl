-module(tcp).
-export([main/0, server_fun/3, client_fun/4, ack/5]).



main() -> 
  Server_PID = spawn(?MODULE, server_fun, [self(), 50, 500]), 
  spawn(?MODULE, client_fun, [Server_PID, 57, 100, client1]),
  spawn(?MODULE, client_fun, [Server_PID, 50, 200, client2]), 
  receive {data,D} -> D end.

server_fun(Main_PID, Port, Seq) ->
  receive 
    {Client_PID, {syn, Port, SeqCl}} ->
      Ack_PID = spawn(?MODULE, ack, [Main_PID, Port, SeqCl+1, Seq+1, Client_PID]),
      Client_PID ! {Ack_PID, {syn_ack, SeqCl+1, Seq}}, 
      server_fun(Main_PID, Port, Seq+1);
    {Client_PID, {syn, _, _}} ->
      Client_PID ! rst%, server_fun(Main_PID,Port,Seq+1) % BUG
      %Client_PID ! rst, server_fun(Main_PID,Port,Seq+1) % OK
  end. 

ack(Main_PID, Port, Ack, Seq, Client_PID) -> 
  receive
    {Client_PID, {ack, Port, Seq, Ack}} ->
      receive 
        {Client_PID, {data, Port, D}} -> Main_PID ! {data, D};
        _ -> Main_PID ! {data, error_data} 
      end;
    _ -> Main_PID ! {data, error_ack} 
  end. 

client_fun(Server_PID, Port, Seq, Data) -> 
  Server_PID ! {self(), {syn, Port, Seq}}, syn_ack(Port, Data, Seq+1).

syn_ack(Port, Data, Ack) ->
  receive 
    rst -> {port_rejected, Port} ; 
    {Ack_PID, {syn_ack, Ack, Seq}} -> 
      Ack_PID ! {self(),  {Ack, Port, Seq+1, ack}}, % BUG
      %Ack_PID ! {self(), {ack, Port, Seq+1, Ack}}, % OK
      Ack_PID ! {self(), {data, Port, Data}},
      {Seq+1, Ack, Port, Data}
  end. 

