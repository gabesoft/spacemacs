default: build

NPM = npm
GEM = gem

all: build

.PHONY: release test loc clean no_targets__ help

no_targets__:
help:
	@sh -c "$(MAKE) -rpn no_targets__ | awk -F':' '/^[a-zA-Z0-9][^\$$#\/\\t=]*:([^=]|$$)/ {split(\$$1,A,/ /);for(i in A)print A[i]}' | grep -v '__\$$' | grep -v 'Makefile' | grep -v 'make\[1\]' | sort"

build-js:
	@$(NPM) install -g tern eslint babel-eslint eslint-plugin-react eslint-config-airbnb js-beautify

build-ruby:
	@$(GEM) install pry

build: build-js build-ruby
