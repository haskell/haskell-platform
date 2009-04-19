# Simple wrapper makefile for the platform build and install scripts

default: build.stamp

config.status:
	./configure

build.stamp: config.status
	./build.sh
	@touch build.stamp

install: build.stamp
	./install.sh

clean:
	rm -f config.status build.stamp package.conf.inplace
	rm -rf ./*/dist/
