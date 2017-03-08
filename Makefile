
CPPFLAGS=-std=c++11 -g -Wall -Wextra -Wno-unused-parameter -O3

all: cl

cl: cl.o structures.o operations.cpp Makefile
	$(CXX) $(CPPFLAGS) -o $@ cl.o structures.o

.PHONY: clean
clean:
	rm -f *.o cl

