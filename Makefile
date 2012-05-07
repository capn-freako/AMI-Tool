CC = gcc
CFLAGS += -I/usr/lib/ghc-7.0.3/include/ -g -fPIC

HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -package parsec -package dsp -package arrows -dynamic -fPIC
HC_LOPTS = -shared -dynamic -package parsec -package dsp -package arrows -lHSrts -L/usr/lib/ghc-7.0.3/ -lm -lffi -lrt
#GHCOPTS := -prof -auto-all -caf-all

#HSRCS = AMIParse.hs AMIModel.hs ApplicativeParsec.hs ExmplUsrModel.hs MaybeT.hs Filter.hs
HSRCS = AMIParse.hs AMIModel.hs ExmplUsrModel.hs Filter.hs
CSRCS = ami_model.c ami_test.c
SRCS  = $(HSRCS) $(CSRCS)
#OBJS = AMIParse.o  AMIModel.o  ami_model.o AMIModel_stub.o ApplicativeParsec.o ExmplUsrModel.o MaybeT.o Filter.o
OBJS = AMIParse.o  AMIModel.o  ami_model.o AMIModel_stub.o ExmplUsrModel.o Filter.o
SO_FILE = libami.so

PARSE_CHK_EXEC = ./ami_test
PARSE_CHK_INPUT = test.ami
TEST_EXEC = ./IBIS_AMI_test
INIT_CFG = test.ami.csv
TEST_OPTS = -i $(INIT_CFG) -f $(SO_FILE) -c

.SUFFIXES : .o .hs .hi .lhs .hc .s .c
.PHONY : all depend rebuild clean

#all: parse_chk test
all: $(PARSE_CHK_EXEC) $(SO_FILE)

$(PARSE_CHK_EXEC): $(PARSE_CHK_EXEC).o 
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
	rm -f *.hi *.o *.out $(PARSE_CHK_EXEC) *.so

parse_chk: $(PARSE_CHK_EXEC) $(SO_FILE)
#	$(PARSE_CHK_EXEC) $(PARSE_CHK_INPUT) >$@
	$(PARSE_CHK_EXEC) $(SO_FILE) $(PARSE_CHK_INPUT)

test: $(SO_FILE)
#	$(TEST_EXEC) $(TEST_OPTS) >$@
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
AMIModel_stub.o: AMIModel.hs
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
