#!/bin/bash

cd ../ebin 
erl -sname edderlang@localhost -run edd_jserver start -noshell -s erlang halt