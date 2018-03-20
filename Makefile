# Compiles Erlang Declarative Debugger source files and edoc

all:
	erlc edd_comp.erl
	erl -run edd_comp compile -noshell -s erlang halt

load:
	erl -run edd_comp load -rsh ssh -sname edd_main -setcookie edd_cookie

# --- remove compiled beam and edoc
clean:
	rm ebin/*.beam doc/*.png doc/*.css doc/*.html doc/edoc-info
