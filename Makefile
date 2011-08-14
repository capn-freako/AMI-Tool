CFLAGS += -I/home/dbanas/ghc-6.12.3/includes/

dummy: all

all: ami_test

ami_test: AMIModel_stub.o AMIModel.o ami_model.o ami_test.o AMIParse.o
	ghc -package parsec -no-hs-main -o $@ $^

AMIModel.o, AMIModel_stub.o: AMIModel.hs
	ghc --make $^

AMIParse.o: AMIParse.hs
	ghc --make $^

