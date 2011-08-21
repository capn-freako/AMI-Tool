GCC = gcc
CFLAGS += -I/usr/lib/ghc-6.12.3/include/ -g -fPIC

HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec -shared
HC_LOPTS = -no-hs-main # -shared
#GHCOPTS := -prof -auto-all -caf-all

HSRCS = AMIParse.hs AMIModel.hs
CSRCS = ami_model.c ami_test.c
SRCS  = $(HSRCS) $(CSRCS)
OBJS = AMIParse.o  AMIModel.o  ami_model.o AMIModel_stub.o 

.SUFFIXES : .o .hs .hi .lhs .hc .s .c

all: ami_test

ami_test: ami_test.o | libami.so
	$(HC) $(HC_LOPTS) -dynamic -o $@ -L. -lami $^

libami.so : $(OBJS)
	rm -f $@
	$(HC) -o $@ $(HC_OPTS) $(HC_LOPTS) $^

dummy: all depend rebuild

depend:
	$(HC) -M $(HC_OPTS) $(HSRCS)

rebuild:
	touch $(SRCS)
	$(MAKE) all

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
AMIParse.o : AMIParse.hs
AMIModel.o : AMIModel.hs
AMIModel.o : AMIParse.hi
# DO NOT DELETE: End of Haskell dependencies
