CFLAGS += -I/home/dbanas/ghc-6.12.3/includes/

dummy: all

all: AMIParseTest

AMIParseTest: AMIParse_stub.o AMIParseTest.o AMIParse.o
	ghc -no-hs-main -o $@ $^

AMIParse.o, AMIParse_stub.o: AMIParse.hs
	ghc --make $^

