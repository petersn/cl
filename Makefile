
CPPFLAGS=-std=c++11 -g -Wall -Wextra -Wno-unused-parameter -fPIC

all: cl cl.so

cl: cl.o structures.o operations.cpp Makefile
	$(CXX) $(CPPFLAGS) -o $@ cl.o structures.o

cl.so: cl.o structures.o operations.cpp Makefile
	$(CXX) -shared -Wl,-soname,$@ -o $@ cl.o structures.o

.PHONY: clean
clean:
	rm -f *.o cl

