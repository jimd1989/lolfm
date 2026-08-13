.POSIX:
PREFIX = /usr/local

.SUFFIXES:
.PHONY: all install uninstall analysis
all:
	cargo build --release
install:
	mkdir -p $(PREFIX)/bin
	cp target/release/lolfm $(PREFIX)/bin
uninstall:
	rm $(PREFIX)/bin/lolfm
analysis:
	chicken-csi \
	analysis/helpers/prelude.scm \
	analysis/helpers/sorted-slices.scm \
	analysis/helpers/transducers.scm \
	analysis/repos/common.scm \
	analysis/repos/*.scm \
	analysis/html/common.scm \
	analysis/html/*.scm \
	analysis/transformers/*.scm \
	analysis/main.scm

