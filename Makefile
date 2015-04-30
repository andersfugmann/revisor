.PHONY: all clean

all:
	omake -w

clean:
	rm -fr revisor* _build *omc .omakedb*
	find . -name \*~ | xargs -r rm

test-clean:
	rm -fr log/* run/*
