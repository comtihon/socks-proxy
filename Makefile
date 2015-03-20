PROJECT = socks_proxy

# deps
DEPS = lager ranch
# deps urls
dep_lager = git https://github.com/basho/lager.git 2.1.1
dep_ranch = git https://github.com/ninenines/ranch.git 1.1.0

# Compiler options.
ERLC_OPTS ?= -W1
ERLC_OPTS += +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1024}'

include erlang.mk