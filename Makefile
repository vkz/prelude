PACKAGE=prelude

setup:
	raco setup --pkgs $(PACKAGE)

test:
	raco test -x -c prelude prelude-tests
