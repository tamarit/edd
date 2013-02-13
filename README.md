Erlang Declarative Debugger (edd)
=================================

A declarative (algorithmic) debugger for Erlang. This debugger asks the 
programmers questions about the expected results of function calls and points 
out the program fragment that is causing the bug. 

The Erlang Declarative Debugger uses the module 'smerl.erl' for metaprogramming 
in Erlang. This module has been written by Yariv Sadan for Erlyweb 
(https://github.com/yariv/erlyweb).
  
We have included this file in the repository only to make the installation of
edd easier, since it does not require a complete Erlyweb installed in the 
system to run edd but only the smerl.erl file.

Getting edd
----------------
The code of 'edd' is contained in a GIT repository on GitHub. The URL is 
https://github.com/tamarit/edd

To get a copy of the repository you only have to write in your Linux/MacOS 
console:

    $ git clone https://github.com/tamarit/edd

This will create a folder called 'edd' in your current directory containing the
files of the Erlang Declarative Debugger.


Compiling edd
-------------

The Erlang Declarative Debugger is written in Erlang, so you will need an Erlang
system installed in you computer. To compile the 'edd' source first move to the
directory of the repository (for example /home/john/edd) and open an Erlang 
emulator:

    $ erl
    Erlang R15B02 (erts-5.9.2) [source] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
   
    Eshell V5.9.2  (abort with ^G)
    1> 

Write the following commands to compile, load and run the compiler and 
installer: 

    1> c(edd_comp).
    {ok,edd_comp}
    2> edd_comp:compile().
    ok

The 'edd_comp:compile()' function simply automates the process. The files 
src/smerl.erl, src/edd_lib.erl and src/edd.erl will be compiled into the 'ebin' 
directory, and the edoc documentation will be generated into the folder 'edoc'.


Using edd
---------

To debug an expression that is returning an incorrect value, simply call the
edd:dd/1 function with the string of the problematic expression. For that, the
module edd must be loaded. To easiest way is including the ebin directory to the
code path when running the Erlang emulator. For example, if the edd folder is in
/home/john/edd/ the command will be:

    $ erl -pa /home/john/edd/
    Erlang R15B02 (erts-5.9.2) [source] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.9.2  (abort with ^G)
    1> l(edd).
    {module,edd}
    >
 
The 'edd' directory contains a 'examples' folder with some buggy programs to 
debug. For example, if you compile and load the 'merge.erl' program you can observe 
that the mergesort/2 function is buggy:

    > merge:mergesort([b,a], fun merge:comp/2).
    [b,a]

To debug this function just call edd:dd/1 with the buggy expression. The 
following lines show a complete debugging session:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)").
    Please, insert a list of trusted functions [m1:f1/a1, m2:f2/a2 ...]: 
    merge:merge([b], [a], fun merge:comp/2) = [b, a]? [y/n/t/d/i/u/a]: n
    merge:comp(b, a) = false? [y/n/t/d/i/u/a]: t
    merge:merge([a], [], fun merge:comp/2) = [a]? [y/n/t/d/i/u/a]: y
    Call to a function that contains an error: merge:merge([b], [a], fun merge:comp/2) = [b, a]
    Please, revise the fourth clause:
    merge([H1 | T1], [H2 | T2], Comp) ->
        case Comp(H1, H2) of
          false -> [H1 | merge([H2 | T1], T2, Comp)];
          true -> [H1 | merge(T1, [H2 | T2], Comp)]
        end.
    ok
    > 
 
First, the debugger asks for a list of trusted functions, i.e., functions that 
the programmer is sure are correct, so that 'edd' will never ask about them. 
This functions must be introduced as a comma-separated lists of function 
signatures 
  
    module1:function1/arity1, module2:function2/arity2, ...
  
If you do not want to mark any function as trusted, simply press 'Enter'. 
After the trusted functions the debugger will ask some question about function 
applications of the shape

    fun(val1, ..., valn) = val? [y/n/t/d/i/u/a]: 
  
to know whether the function 'fun' applied to values 'val1', ..., 'valn' must 
return the value 'val' or not. The possible answers to these questions are:
 * Yes (y): the evaluation is correct.
 * No (n): the evaluation is incorrect.
 * Trusted (t): the evaluation is correct and the function is trusted, so any 
                subsequent application of the function will be considered as
                correct without asking.
 * Don't know (d): the question is too hard to answer.
 * Inadmissible (i): the function call has no sense with those values as
                     arguments.
 * Undo (u): the debugger forgets the last answer and asks again the previous
             question.
 * Abort (a): finishes the debugging session.                

After asking some questions, the debugger will point out what is the function 
call that is causing the bug and it will also shows the concrete function rule
responsible. In the example:

    Call to a function that contains an error: merge:merge([b], [a], fun merge:comp/2) = [b, a]
    Please, revise the fourth clause:
    merge([H1 | T1], [H2 | T2], Comp) ->
        case Comp(H1, H2) of
          false -> [H1 | merge([H2 | T1], T2, Comp)];
          true -> [H1 | merge(T1, [H2 | T2], Comp)]
        end.

Options for edd
---------------

The Erlang Declarative Debugger has two different strategies to traverse the
proof tree looking for the buggy node: "Divide & Query" and "Top Down". The
"Divide & Query" strategy tries to make the minimum number of questions, and selects
those nodes that split the tree into two of about the same size. On the other hand,
"Top Down" traverses the tree from the root to the leaves until a buggy node is 
found. The default strategy is "Divide & Query", so a debugging session using 
dd/1 will use this strategy. To use the "Top Down" strategy, use the 'top_down'
argument in a dd/2 call. For example:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2), top_down").
    
The Erlang Declarative Debugger can also generate the proof tree in a DOT file
(http://en.wikipedia.org/wiki/DOT_language) and a PDF file using the 'dot'
command. To activate this option, use the 'tree' argument in dd/2. For
example the call:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", tree).
    
will start a debugging session using the default strategy "Divide & Query" and
generate the proof tree of the evaluation of the expression in a file called 
"dbg.dot" and also in "dbg.pdf" in the current directory. If your system does not
have the 'dot' command to create PDF from DOT files, the PDF will not be create
but no error will be raised. 

To generate the proof tree file and also use the "Top Down" strategy, use the
dd/3 function:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", tree, top_down).
    
or 

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", top_down, tree).



