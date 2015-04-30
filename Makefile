PROJECT = conferl

DEPS = lager cowboy sync sumo 
dep_lager = git https://github.com/basho/lager.git 2.1.0
dep_sync =  git https://github.com/inaka/sync.git 0.1.3
dep_sumo =  git https://github.com/inaka/sumo_db.git 0.3.5
dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.0

PLT_APPS := inets
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wrace_conditions

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS = -erl_args -config rel/sys.config

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config rel/sys.config

testshell: 
	erl -pa ebin -pa deps/*/ebin -pa test -config rel/sys.config -s sync

quicktests: app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
