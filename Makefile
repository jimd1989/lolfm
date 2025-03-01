.POSIX:
PREFIX = /usr/local

.SUFFIXES:
all:
	cargo build --release
install:
	mkdir -p $(PREFIX)/bin
	cp target/release/lolfm $(PREFIX)/bin
uninstall:
	rm $(PREFIX)/bin/lolfm
