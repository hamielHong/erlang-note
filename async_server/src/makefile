.SUFFIXES: .erl .beam

.erl.beam:
	erlc -o ../ebin -W $<

ERL = erl -pa ../ebin -boot start_sasl

MODS = tcp_server_app tcp_server_sup tcp_listener_sup tcp_listener \
		tcp_acceptor_sup tcp_acceptor tcp_client_sup tcp_echo_fsm \
		connect_test connect_client

all: compile
	${ERL} -s application start tcp_server

compile: ${MODS:%=%.beam}

clean: rm -rf *.beam erl_crash.dump