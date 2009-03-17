all: ebin
	(cd src;$(MAKE))

ebin:
	mkdir -p ebin

clean:
	(cd src;$(MAKE) clean)
