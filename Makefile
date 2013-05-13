CPPFLAGS=-O2 -g
LIBS=
INCS=

hello: main.cpp
	$(CXX) -o $@ -std=c++0x $(LIBS) $(INCS) main.cpp -lglut -lgl
