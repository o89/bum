#LEAN_HOME = # May be set here
LEAN_PATH = $(LEAN_HOME)/lib/lean/:$(shell realpath src)
export LEAN_PATH

CPP = src/bum/bindings
LEAN = src/bum/auxiliary src/bum/parser src/bum/types src/bum/configparser src/bum/configconverter src/bum/io src/bum/bum

OBJS = $(shell for path in $(addsuffix .o, $(CPP) $(LEAN)); do echo $$path; done | tac)

CXX = c++
CFLAGS = -g -Wall -D LEAN_MULTI_THREAD -Wno-unused-command-line-argument -pthread
LIBS = -no-pie -Wl,--start-group -lleancpp -lInit -lStd -lLean -Wl,--end-group -lgmp -ldl

RES = bum

$(RES): $(addsuffix .o,$(LEAN) $(CPP))
	$(CXX) -L$(LEAN_HOME)/lib/lean -o $(RES) $(CFLAGS) $(OBJS) $(LIBS)

$(addsuffix .o,$(LEAN) $(CPP)): %.o: %.cpp
	$(CXX) -c -I$(LEAN_HOME)/include $< -o $@

$(addsuffix .cpp,$(LEAN)): %.cpp: %.olean
	(cd src; $(LEAN_HOME)/bin/lean -c $(@:src/%=%) $(patsubst src/%,%,$(<:.olean=.lean)))

$(addsuffix .olean,$(LEAN)): %.olean: %.lean
	$(LEAN_HOME)/bin/lean -o $(<:.lean=.olean) $<

clean:
	rm -f $(addsuffix .cpp,$(LEAN)) $(addsuffix .olean,$(LEAN))
	rm -f $(addsuffix .o,$(LEAN)) $(addsuffix .o,$(CPP))
	rm -f $(RES)
