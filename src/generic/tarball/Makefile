# Simple wrapper makefile for the platform build and install scripts

default: build.stamp

config.status:
	./configure

build.stamp: config.status
	scripts/build.sh
	@touch build.stamp

install: build.stamp
	scripts/install.sh

clean:
	rm -f config.status build.stamp packages/package.conf.inplace scripts/config
	rm -rf packages/*/dist/
