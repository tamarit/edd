import com.ericsson.otp.erlang.*;

import java.io.*;

public class EDDJInterface
{

    public static void main(String[] argv) throws Exception
    {

        OtpErlangObject myObject;
        OtpErlangTuple myMsg;
        OtpErlangAtom command;
        OtpErlangObject[] reply;
        OtpErlangTuple myTuple;

        ErlangServer erlServer = new ErlangServer();
        erlServer.start();

        System.out.println("Erlang EDD server running...");
        OtpNode myNode = new OtpNode("eddjava@localhost");
        OtpMbox myMbox = myNode.createMbox("edd");

        // Receive PID from erlag server
        myObject = myMbox.receive();
        myMsg = (OtpErlangTuple) myObject;
        command = (OtpErlangAtom) myMsg.elementAt(0);
        OtpErlangPid from = (OtpErlangPid) myMsg.elementAt(1);
        // Should be 'ready'
        System.out.println("Command received: " + command.toString() );

        // Send reply with buggy call and its location
        reply = new OtpErlangObject[3];
        reply[0] =  new OtpErlangAtom("buggy_call");
        reply[1] = new OtpErlangString(argv[0]);
        reply[2] = new OtpErlangString(argv[1]);
        myTuple = new OtpErlangTuple(reply);
        myMbox.send(from, myTuple);

        // Receive debugging tree
        myObject = myMbox.receive();
        myMsg = (OtpErlangTuple) myObject;
        command = (OtpErlangAtom) myMsg.elementAt(0);
        OtpErlangString dbg_tree = (OtpErlangString) myMsg.elementAt(1);
        // Should be 'ready'
        System.out.println("Command received: " + command.toString() );
        System.out.println("Data received: " + dbg_tree.toString() );

        getBuggyNode(myMbox, from);

    }

    private static int getBuggyNode(OtpMbox myMbox, OtpErlangPid from) throws OtpErlangExit, OtpErlangRangeException, OtpErlangDecodeException
    {
        OtpErlangObject myObject;
        OtpErlangTuple myMsg;
        OtpErlangAtom command;
        OtpErlangObject[] reply;
        OtpErlangTuple myTuple;

        // Receive question
        myObject = myMbox.receive();
        myMsg = (OtpErlangTuple) myObject;
        command = (OtpErlangAtom) myMsg.elementAt(0);
        // Should be 'ready'
        System.out.println("Command received: " + command.toString() );

        if(command.toString().equals("question"))
        {
            OtpErlangLong question = (OtpErlangLong) myMsg.elementAt(1);
            System.out.println("Question: " + question.longValue() );

            OtpErlangTuple state = (OtpErlangTuple) myMsg.elementAt(2);
            // System.out.println("State: " + state.toString() );

            // Send reply with answer
            reply = new OtpErlangObject[2];
            reply[0] =  new OtpErlangAtom("answer");
            reply[1] = new OtpErlangAtom("n");
            myTuple = new OtpErlangTuple(reply);
            myMbox.send(from, myTuple);

            return getBuggyNode(myMbox, from);
        }

        if(command.toString().equals("buggy_node"))
        {
            OtpErlangLong buggyNode = (OtpErlangLong) myMsg.elementAt(1);
            System.out.println("Buggy node: " + buggyNode.intValue() );
            
            return buggyNode.intValue();
        }

        if(command.toString().equals("aborted"))
        {   
            System.out.println("Aborted.");
        }

        return -1;

    }

}

class ErlangServer extends Thread
{
    public void run()
    {
        try
        {
            Process cmdProc = Runtime.getRuntime().exec("./load_erlang_server.sh");


            BufferedReader stdoutReader = new BufferedReader(
                     new InputStreamReader(cmdProc.getInputStream()));
            String line;
            while ((line = stdoutReader.readLine()) != null) {
               System.out.println(line);
            }

            BufferedReader stderrReader = new BufferedReader(
                     new InputStreamReader(cmdProc.getErrorStream()));
            while ((line = stderrReader.readLine()) != null) {
               System.out.println(line);
            }

            int retValue = cmdProc.exitValue();
            if(retValue != 0)
                new RuntimeException(new Exception("Can't load erlang module"));
        }
        catch(Exception e)
        {
            System.out.println("Can't load erlang module");
        }
    }
}