
CPPFLAGS=-std=c++11 -g

all: cl

cl: cl.o structures.o
	$(CXX) $(CPPFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o cl

