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

define APP_TEMPLATE
$(1): lib/$(1)/ebin/Elixir.$(2).beam lib/$(1)/ebin/$(1).app

lib/$(1)/ebin/$(1).app: lib/$(1)/mix.exs
	$(Q) mkdir -p lib/$(1)/_build/shared/lib/$(1)
	$(Q) cp -R lib/$(1)/ebin lib/$(1)/_build/shared/lib/$(1)/
	$(Q) cd lib/$(1) && ../../bin/elixir -e 'Mix.start(:permanent, [])' -r mix.exs -e 'Mix.Task.run("compile.app")'
	$(Q) cp lib/$(1)/_build/shared/lib/$(1)/ebin/$(1).app lib/$(1)/ebin/$(1).app
	$(Q) rm -rf lib/$(1)/_build

lib/$(1)/ebin/Elixir.$(2).beam: $(wildcard lib/$(1)/lib/*.ex) $(wildcard lib/$(1)/lib/*/*.ex) $(wildcard lib/$(1)/lib/*/*/*.ex)
	@ echo "==> $(1) (compile)"
	@ rm -rf lib/$(1)/ebin
	$(Q) cd lib/$(1) && ../../$$(ELIXIRC) "lib/**/*.ex" -o ebin

test_$(1): compile $(1)
	@ echo "==> $(1) (exunit)"
	$(Q) cd lib/$(1) && ../../bin/elixir -r "test/test_helper.exs" -pr "test/**/*_test.exs";
endef

#==> Compilation tasks

KERNEL:=lib/elixir/ebin/Elixir.Kernel.beam
UNICODE:=lib/elixir/ebin/Elixir.String.Unicode.beam

default: compile

compile: erlang elixir

erlang:
	$(Q) cd lib/elixir && $(REBAR) compile

# Since Mix depends on EEx and EEx depends on Mix,
# we first compile EEx without the .app file,
# then mix and then compile EEx fully
elixir: stdlib lib/eex/ebin/Elixir.EEx.beam mix ex_unit logger eex iex

stdlib: $(KERNEL) VERSION
$(KERNEL): lib/elixir/lib/*.ex lib/elixir/lib/*/*.ex lib/elixir/lib/*/*/*.ex
	$(Q) if [ ! -f $(KERNEL) ]; then \
	  $(call CHECK_ERLANG_RELEASE); \
		echo "==> bootstrap (compile)"; \
		$(ERL) -s elixir_compiler bootstrap -s erlang halt; \
	fi
	@ echo "==> elixir (compile)";
	$(Q) cd lib/elixir && ../../$(ELIXIRC) "lib/kernel.ex" -o ebin;
	$(Q) cd lib/elixir && ../../$(ELIXIRC) "lib/**/*.ex" -o ebin;
	$(Q) $(MAKE) unicode
	$(Q) rm -f lib/elixir/ebin/elixir.app
	$(Q) cd lib/elixir && $(REBAR) compile

unicode: $(UNICODE)
$(UNICODE): lib/elixir/unicode/*
	@ echo "==> unicode (compile)";
	$(Q) $(ELIXIRC) lib/elixir/unicode/unicode.ex -o lib/elixir/ebin;
	$(Q) $(ELIXIRC) lib/elixir/unicode/properties.ex -o lib/elixir/ebin;
	$(Q) $(ELIXIRC) lib/elixir/unicode/tokenizer.ex -o lib/elixir/ebin;

$(eval $(call APP_TEMPLATE,ex_unit,ExUnit))
$(eval $(call APP_TEMPLATE,logger,Logger))
$(eval $(call APP_TEMPLATE,eex,EEx))
$(eval $(call APP_TEMPLATE,mix,Mix))
$(eval $(call APP_TEMPLATE,iex,IEx))

install: compile
	@ echo "==> elixir (install)"
	$(Q) for dir in lib/*; do \
		rm -rf $(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin; \
		$(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin"; \
		$(INSTALL_DATA) $$dir/ebin/* "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/$$dir/ebin"; \
	done
	$(Q) $(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/bin"
	$(Q) $(INSTALL_PROGRAM) $(filter-out %.ps1, $(filter-out %.bat, $(wildcard bin/*))) "$(DESTDIR)$(PREFIX)/$(LIBDIR)/elixir/bin"
	$(Q) $(INSTALL_DIR) "$(DESTDIR)$(PREFIX)/$(BINDIR)"
	$(Q) for file in "$(DESTDIR)$(PREFIX)"/$(LIBDIR)/elixir/bin/*; do \
		ln -sf "../$(LIBDIR)/elixir/bin/$${file##*/}" "$(DESTDIR)$(PREFIX)/$(BINDIR)/"; \
	done
	$(MAKE) install_man

clean:
	cd lib/elixir && $(REBAR) clean
	rm -rf ebin
	rm -rf lib/*/ebin
	$(Q) $(MAKE) clean_residual_files

clean_elixir:
	$(Q) rm -f lib/*/ebin/Elixir.*.beam

clean_residual_files:
	rm -rf lib/*/_build/
	rm -rf lib/*/tmp/
	rm -rf lib/elixir/test/ebin/
	rm -rf lib/mix/test/fixtures/deps_on_git_repo/
	rm -rf lib/mix/test/fixtures/git_rebar/
	rm -rf lib/mix/test/fixtures/git_repo/
	rm -rf lib/mix/test/fixtures/git_sparse_repo/
	rm -f erl_crash.dump
	$(Q) $(MAKE) clean_man

#==> Documentation tasks

LOGO_PATH = $(shell test -f ../docs/logo.png && echo "--logo ../docs/logo.png")
SOURCE_REF = $(shell tag="$(call GIT_TAG)" revision="$(call GIT_REVISION)"; echo "$${tag:-$$revision}\c")
DOCS_FORMAT = html
COMPILE_DOCS = bin/elixir ../ex_doc/bin/ex_doc "$(1)" "$(VERSION)" "lib/$(2)/ebin" -m "$(3)" -u "https://github.com/elixir-lang/elixir" --source-ref "$(call SOURCE_REF)" $(call LOGO_PATH) -o doc/$(2) -n https://hexdocs.pm/$(2)/$(CANONICAL) -p http://elixir-lang.org/docs.html -f "$(DOCS_FORMAT)" $(4)
