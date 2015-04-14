PROJECT = conferl

DEPS = lager sync sumo
dep_lager = git https://github.com/basho/lager.git 2.1.0
dep_sync =  git https://github.com/inaka/sync.git 0.1.3
dep_sumo =  git https://github.com/inaka/sumo_db.git 0.3.5

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS = -erl_args -config rel/sys.config

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config rel/sys.config
