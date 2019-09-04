#LEAN_DIR = # May be set here
LEAN_PATH = $(LEAN_DIR)/library:./src
export LEAN_PATH

CPP = src/bum/bindings
LEAN = src/bum/auxiliary src/bum/types src/bum/parser src/bum/io src/bum/bum
FLAGS = -g -Wall

RES = bum

$(RES): $(addsuffix .cpp,$(LEAN) $(CPP))
	$(LEAN_DIR)/bin/leanc -o $(RES) $(addsuffix .cpp,$(LEAN) $(CPP))

$(addsuffix .cpp,$(LEAN)): %.cpp: %.olean
	$(LEAN_DIR)/bin/lean -c $@ $(<:.olean=.lean)

$(addsuffix .olean,$(LEAN)): %.olean: %.lean
	$(LEAN_DIR)/bin/lean --make $<

clean:
	rm -f $(addsuffix .cpp,$(LEAN)) $(addsuffix .olean,$(LEAN))
	rm -f $(RES)
