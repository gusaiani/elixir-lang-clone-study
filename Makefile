REBAR ?= "$(CURDIR)/rebar"
PREFIX ?= /usr/local
SHARE_PREFIX ?= $(PREFIX)/share
CANONICAL := master/
ELIXIRC := bin/elixirc --verbose --ignore-module-conflict
ERLC := erlc -I lib/elixir/include
ERL := erl -I lib/elixir/include -noshell -pa lib/elixir/ebin
VERSION := $(strip $(shell cat VERSION))
Q := @
LIBDIR := lib
BINDIR := bin
INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_DATA = $(INSTALL) -m644
INSTALL_PROGRAM = $(INSTALL) -m755
GIT_REVISION = $(strip $(shell git rev-parse HEAD 2> /dev/null ))
GIT_TAG = $(strip $(shell head="$(call GIT_REVISION)"; git tag --points-at $$head 2> /dev/null | tail -1) )

.PHONY: install compile erlang elixir build_plt clean_plt dialyze test clean clean_residual_files install_man clean_man docs Docs.zip Precompiled.zip zips
.NOTPARALLEL: compile

#==> Functions

define CHECK_ERLANG_RELEASE
	erl -noshell -eval '{V,_} = string:to_integer(erlang:system_info(otp_release)), io:fwrite("~s", [is_integer(V) and (V >= 19)])' -s erlang halt | grep -q '^true'; \
		if [ $$? != 0 ]; then \
		  echo "At least Erlang 19.0 is required to build Elixir"; \
		  exit 1; \
		fi
endef
