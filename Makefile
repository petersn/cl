
CXXFLAGS=-std=c++11 -g -Wall -Wextra -Wno-unused-parameter -fPIC

all: cl cl.so

cl: cl.o structures.o operations.cpp Makefile
	$(CXX) $(CXXFLAGS) -o $@ cl.o structures.o

cl.so: cl.o structures.o operations.cpp Makefile
	$(CXX) $(CXXFLAGS) -shared -Wl,-soname,$@ -o $@ cl.o structures.o

bootstrap/bootstrap_base: bootstrap/bootstrap_base.o cl.o structures.o operations.cpp Makefile
	$(CXX) $(CXXFLAGS) -o $@ bootstrap/bootstrap_base.o cl.o structures.o

.PHONY: clean
clean:
	rm -f *.o cl cl.so bootstrap/bootstrap_base

