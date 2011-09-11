CC = gcc
CFLAGS += -I/usr/lib/ghc-6.12.3/include/ -g -fPIC

HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec-3.1.1 -package dsp -dynamic -fPIC
HC_LOPTS = -shared -dynamic -package parsec-3.1.1 -package dsp -lHSrts -L/usr/lib/ghc-6.12.3/ -lm -lffi -lrt
#GHCOPTS := -prof -auto-all -caf-all

HSRCS = AMIParse.hs AMIModel.hs ApplicativeParsec.hs ExmplUsrModel.hs
CSRCS = ami_model.c ami_test.c
SRCS  = $(HSRCS) $(CSRCS)
OBJS = AMIParse.o  AMIModel.o  ami_model.o AMIModel_stub.o ApplicativeParsec.o ExmplUsrModel.o

.SUFFIXES : .o .hs .hi .lhs .hc .s .c
.PHONY : all depend rebuild clean

all: ami_test libami.so

ami_test: ami_test.o 
	$(CC) -rdynamic -o $@ ami_test.o -ldl

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
ExmplUsrModel.o : ExmplUsrModel.hs
ApplicativeParsec.o : ApplicativeParsec.hs
AMIParse.o : AMIParse.hs
AMIParse.o : ApplicativeParsec.hi
AMIModel.o : AMIModel.hs
AMIModel.o : ExmplUsrModel.hi
AMIModel.o : AMIParse.hi
# DO NOT DELETE: End of Haskell dependencies
