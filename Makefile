CFLAGS += -I/home/dbanas/ghc-6.12.3/includes/ -g

HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec -ho-hs-main
#GHCOPTS := -prof -auto-all -caf-all

SRCS = AMIParse.hs AMIModel.hs ami_model.c ami_test.c
OBJS = AMIParse.o  AMIModel.o  ami_model.o ami_test.o AMIModel_stub.o 

.SUFFIXES : .o .hs .hi .lhs .hc .s .c

dummy: all depend

all: ami_test

depend:
        $(HC) -M $(HC_OPTS) $(SRCS)

ami_test: $(OBJS)
        rm -f $@
        $(HC) -o $@ $(HC_OPTS) $^

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

