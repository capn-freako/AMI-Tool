GCC = gcc
CFLAGS += -I/usr/lib/ghc-6.12.3/include/ -g -fPIC

HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec
HC_LOPTS = -no-hs-main -shared -package parsec
#GHCOPTS := -prof -auto-all -caf-all

HSRCS = AMIParse.hs AMIModel.hs ApplicativeParsec.hs
CSRCS = ami_model.c ami_test.c
SRCS  = $(HSRCS) $(CSRCS)
OBJS = AMIParse.o  AMIModel.o  ami_model.o AMIModel_stub.o ApplicativeParsec.o

.SUFFIXES : .o .hs .hi .lhs .hc .s .c
.PHONY : all depend rebuild clean

all: ami_test

ami_test: ami_test.o libami.so
	$(HC) -dynamic -o $@ -L. -lami ami_test.o

libami.so : $(OBJS)
	rm -f $@
	$(HC) -o $@ $(HC_LOPTS) $^

depend:
	$(HC) -M $(HC_OPTS) $(HSRCS)

rebuild:
	$(MAKE) clean
	$(MAKE) all

clean:
	rm -f *.hi *.o *.out ami_test *.so

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot:
	@:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

# Individual cases
AMIModel_stub.o: AMIModel.hs
	$(HC) -c $< $(HC_OPTS)

# DO NOT DELETE: Beginning of Haskell dependencies
ApplicativeParsec.o : ApplicativeParsec.hs
AMIParse.o : AMIParse.hs
AMIParse.o : ApplicativeParsec.hi
AMIModel.o : AMIModel.hs
AMIModel.o : AMIParse.hi
# DO NOT DELETE: End of Haskell dependencies
