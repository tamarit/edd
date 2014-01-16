Erlang Declarative Debugger (edd)
=================================

A declarative (algorithmic) debugger for Erlang. This debugger asks the 
programmers questions about the expected results of some evaluations (function 
and fun calls, evalulation of case expressions, etc.) and points out the program 
fragment that is causing the bug. 

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
files of the Erlang Declarative Debugger. If you do not have **git** installed
in your system (or you are using Windows) you can also download a copy of the
repository in a ZIP file using the button **Download ZIP** in the upper right 
part of the https://github.com/tamarit/edd page, or following the link 
https://github.com/tamarit/edd/archive/master.zip.


Compiling edd
-------------

The Erlang Declarative Debugger is written in Erlang, so you will need an Erlang
system installed in you computer. There are two ways to compile 'edd': the 
simplest and the simple.

1. **The simplest**: Move to the directory of the repository (for example /home/john/edd) and type *make*:	

        $ make
        erlc edd_comp.erl
        erl -run edd_comp compile -noshell -s erlang halt
        
2. **The simple**: Move to the directory of the repository (for example /home/john/edd) and open an Erlang 
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
*edd:dd/1* function with the string of the problematic expression. For that, the
modules of edd must be loaded. The 'edd_comp:load()' function load all the needed
modules. You can call it when the shell is initialized using:

        $ erl -run edd_comp load
 
The 'edd' directory contains a 'examples' folder with some buggy programs to 
debug. For example, if you compile and load the 'merge.erl' program you can observe 
that the mergesort/2 function is buggy:

    > cd("examples/mergesort"), c(merge).
    {ok,merge}
    > merge:mergesort([b,a], fun merge:comp/2).
    [b,a]

To debug this function just call edd:dd/1 with the buggy expression. The 
following lines show a complete debugging session:

    > edd:dd( "merge:mergesort([b,a], fun merge:comp/2)", tree ).
    Please, insert a list of trusted functions [m1:f1/a1, m2:f2/a2 ...]: 
    merge:merge([b], [a], fun merge:comp/2) = [b, a]? [y/n/t/d/i/s/u/a]: n
    merge:comp(b, a) = false? [y/n/t/d/i/s/u/a]: t
    merge:merge([a], [], fun merge:comp/2) = [a]? [y/n/t/d/i/s/u/a]: y
    Call to a function that contains an error:
    merge:merge([b], [a], fun merge:comp/2) = [b, a]
    Please, revise the fourth clause:
    merge([H1 | T1], [H2 | T2], Comp) ->
        case Comp(H1, H2) of
          false -> [H1 | merge([H2 | T1], T2, Comp)];
          true -> [H1 | merge(T1, [H2 | T2], Comp)]
        end.
    Do you want to follow the debugging session inside this function? [y/n]: y
 
First, the debugger asks for a list of trusted functions, i.e., functions that 
the programmer is sure are correct, so that 'edd' will never ask about them. 
This functions must be introduced as a comma-separated lists of function 
signatures 
  
    module1:function1/arity1, module2:function2/arity2, ...
  
If you do not want to mark any function as trusted, simply press 'Enter'. 
After the trusted functions the debugger will ask some question about function 
and 'fun' applications of the shape

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
        
After the debugger finds the wrong function, it can follow the debugging session 
inside this function to find the the part of the function that is causing the
erroneous behavior. In the 'merge.erl' example, the session continues as follows:

    Do you want to follow the debugging session inside this function? [y/n]: y
    For the case expression:
    case Comp(H1, H2) of
      false -> [H1 | merge([H2 | T1], T2, Comp)];
      true -> [H1 | merge(T1, [H2 | T2], Comp)]
    end
    Is there anything incorrect?
    1.- The context:
            Comp = fun merge:comp/2
            H1 = b
            H2 = a
            T1 = []
            T2 = []
    2.- The argument value: false.
    3.- Enter in the first clause.
    4.- The final value: [b,a].
    5.- Nothing.
    [1/2/3/4/5/d/s/u/a]? 4
    
    
    This is the reason for the error:
    Value [b,a] for the final expression [H1 | merge([H2 | T1], T2, Comp)] (Line 27) is not correct.
    
In this second step of debuggind, the debugger will not ask about function/fun
applications but about the evaluation of different constructions like guards,
case/if expressions, bindings, etc. 

edd for concurrent programs
---------------------------

The Erlang Declarative Debugger allow the user to debug concurrent programs in
a similar way that sequential ones. To debug a function with concurrent feautures, 
the user would use 'edd:ddc/2', where the first parameter is the call to the
buggy function, and the second is the timeout (in miliseconds) for the tracing 
process needed in this kind of debugging. 
    

Options for edd
---------------

The Erlang Declarative Debugger has two different strategies to traverse the
proof tree looking for the buggy node: "Divide & Query" and "Top Down". The
"Divide & Query" strategy tries to make the minimum number of questions, and selects
those nodes that split the tree into two of about the same size. On the other hand,
"Top Down" traverses the tree from the root to the leaves until a buggy node is 
found. The default strategy for the first step of debugging (to find a wrong
function) is "Divide & Query", and the default strategy for the second step is 
"Top Down". To use the "Top Down" strategy in both debugging steps, use the 
'top_down' argument in the dd/2 call. For example:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", top_down).
    
Note that you can change the strategy at any point of the debugging session by 
pressing the key 's'.
    
The Erlang Declarative Debugger can also generate the proof tree in a DOT file
(http://en.wikipedia.org/wiki/DOT_language) and a PDF file using the 'dot'
command. To activate this option, use the 'tree' argument in dd/2. For
example the call:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", tree).
    
will start a debugging session using the default strategy "Divide & Query" and
generate the proof tree of the evaluation of the expression in the files 
"dbg.dot"/"dbg.pdf" for the first step of the debugging session, and 
"dbg_zoom.dot"/"dbg_zoom.pdf" for the second one. These files will be stored in
the current directory. If your system does not have the 'dot' command to create 
PDF from DOT files, the PDF will not be create but no error will be raised. 

To generate the proof tree file and also use the "Top Down" strategy, use the
dd/3 function:

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", tree, top_down).
    
or 

    > edd:dd("merge:mergesort([b,a], fun merge:comp/2)", top_down, tree).



