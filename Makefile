
CPPFLAGS=-std=c++11 -g

all: cl

cl: cl.o structures.o operations.cpp Makefile
	$(CXX) $(CPPFLAGS) -o $@ cl.o structures.o

.PHONY: clean
clean:
	rm -f *.o cl

