# Simple wrapper makefile for the platform build and install scripts

default: build.stamp

build.stamp:
	./build.sh
	touch build.stamp

install: build.stamp
	./install.sh
