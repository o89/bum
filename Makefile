#LEAN_DIR = # May be set here
LEAN_PATH = $(LEAN_DIR)/library:./src
export LEAN_PATH

CPP = src/bump/bindings
LEAN = src/bump/aux src/bump/parser src/bump/types src/bump/configparser src/bump/configconverter src/bump/io src/bump/bump
FLAGS = -g -Wall

RES = bump

$(RES): $(addsuffix .cpp,$(LEAN) $(CPP))
	$(LEAN_DIR)/bin/leanc -o $(RES) $(addsuffix .cpp,$(LEAN) $(CPP))

$(addsuffix .cpp,$(LEAN)): %.cpp: %.olean
	$(LEAN_DIR)/bin/lean -c $@ $(<:.olean=.lean)

$(addsuffix .olean,$(LEAN)): %.olean: %.lean
	$(LEAN_DIR)/bin/lean --make $<

clean:
	rm -f $(addsuffix .cpp,$(LEAN)) $(addsuffix .olean,$(LEAN))
	rm -f $(RES)
