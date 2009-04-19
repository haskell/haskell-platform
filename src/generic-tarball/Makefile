# Simple wrapper makefile for the platform build and install scripts

default: build.stamp

config.status:
	./configure

build.stamp: config.status
	scripts/build.sh
	@touch build.stamp

install: build.stamp
	scrupts/install.sh

clean:
	rm -f config.status build.stamp packages/package.conf.inplace
	rm -rf packages/*/dist/
