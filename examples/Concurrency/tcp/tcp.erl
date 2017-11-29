
-module(tcp).
-export([main/0, server/3, client/4, ack/5]).



main() -> 
   Server_PID = spawn(?MODULE,server,[self(),50,500]),
   spawn(?MODULE,client,[Server_PID,57,100,client1]),
   spawn(?MODULE,client,[Server_PID,50,200,client2]),   
   receive 
     {data, D} ->   {D}
   end.
   
server(Main_ID,Port, Seq) -> 
 receive 
	  {Client_PID,Port,syn,SeqCl} -> Ack_ID = spawn(?MODULE,ack,[Main_ID,Port,SeqCl+1,Seq,Client_PID]),   
                                     Client_PID ! {Ack_ID,syn_ack,SeqCl+1 ,Seq},
                                     server( Main_ID,Port, Seq+1) ;
	 %{Client_PID,_,syn,_SeqCl}   ->   Client_PID ! rst,  server( Main_ID,Port, Seq+1)   
   {Client_PID,_,syn,_SeqCl}   ->   Client_PID ! rst%,  server( Main_ID,Port, Seq+1)   
      
 end.
   
ack(Server_ID,Port,Ack,Seq, Client_PID) -> 
    receive 
    {Client_PID,Port, ack, Ack, Seq}  ->  ok ;
    _  -> Server_ID ! {data, error_ack}
    end,
    receive 
    {Client_PID,Port, data, D}  ->   Server_ID ! {data, D};
    _  -> Server_ID ! {data,  error_data}
    end.
    
   

client(Server,Port,Seq, ID) -> 
   Server ! {self(),Port,syn,Seq},
   syn_ack(Port,ID, Seq+1),
   ok.

syn_ack(Port,ID,Ack) ->
   receive 
     rst -> {Port, rejected} ;
     {Ack_ID,syn_ack,Ack ,Seq} -> 
        %Ack_ID ! {self(),Port, ack, Ack, Seq }, 
        Ack_ID ! {self(),Port, Ack, ack, Seq},
        Ack_ID ! {self(),Port, data, ID}
   end.

