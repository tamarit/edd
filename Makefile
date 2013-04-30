# Compiles Erlang Declarative Debugger source files and edoc

all:
	erlc edd_comp.erl
	erl -run edd_comp compile -noshell -s erlang halt

# --- remove compiled beam and edoc
clean:
	rm ebin/*.beam doc/*.png doc/*.css doc/*.html doc/edoc-info
