CC = gcc
CFLAGS += -I/usr/lib/ghc-7.4.2/include/ -g -fPIC

HC      = ghc -B/usr/lib/ghc-7.4.2
HC_OPTS = -cpp -O3 $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec -dynamic -fPIC
HC_LSUFFIX = -ghc7.4.2
HC_LOPTS = -rtsopts -shared -dynamic -package parsec -lHSrts -lm -lffi -lrt

HSRCS = AMIParse.hs AMIModel.hs ExmplUsrModel.hs Filter.hs
CSRCS = ami_model.c ami_test.c
SRCS  = $(HSRCS) $(CSRCS)
OBJS = AMIParse.o  AMIModel.o  ami_model.o ExmplUsrModel.o Filter.o
SO_FILE = libami.so

PARSE_CHK_EXEC = ./ami_test
PARSE_CHK_INPUT = test.ami
TEST_EXEC = ./IBIS_AMI_test
INIT_CFG = test.ami.csv
TEST_OPTS = -i $(INIT_CFG) -f $(SO_FILE) -c

.SUFFIXES : .o .hs .hi .lhs .hc .s .c
.PHONY : all depend rebuild clean

all: $(PARSE_CHK_EXEC) $(SO_FILE)

$(PARSE_CHK_EXEC): $(PARSE_CHK_EXEC).c
	$(CC) -rdynamic -o $@ $^ -ldl

$(SO_FILE) : $(OBJS)
	rm -f $@
	$(HC) -o $@ $(HC_LOPTS) $^

depend:
	$(HC) -M $(HC_OPTS) $(HSRCS)

rebuild:
	$(MAKE) clean
	$(MAKE) all

clean:
	rm -f *_stub.h *.hi *.o *.out $(PARSE_CHK_EXEC) $(SO_FILE)

parse_chk: $(PARSE_CHK_EXEC) $(SO_FILE)
	$(PARSE_CHK_EXEC) $(SO_FILE) $(PARSE_CHK_INPUT)

test: $(SO_FILE)
	$(TEST_EXEC) $(TEST_OPTS)

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
AMIModel_stub.h: AMIModel.o
	@:

ami_model.o: ami_model.c AMIModel_stub.h
	$(HC) -c $< $(HC_OPTS)

# DO NOT DELETE: Beginning of Haskell dependencies
Filter.o : Filter.hs
AMIParse.o : AMIParse.hs
ExmplUsrModel.o : ExmplUsrModel.hs
ExmplUsrModel.o : Filter.hi
ExmplUsrModel.o : AMIParse.hi
AMIModel.o : AMIModel.hs
AMIModel.o : ExmplUsrModel.hi
AMIModel.o : AMIParse.hi
# DO NOT DELETE: End of Haskell dependencies
