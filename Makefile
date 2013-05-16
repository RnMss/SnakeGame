CPPFLAGS=-O2 -g

CPPFLAGS+=
LFLAGS+=

ifeq ($(shell uname),Darwin)
	CPPFLAGS+= -framework GLUT -framework OpenGL
	CPPFLAGS+= -stdlib=libc++
	#CPPFLAGS+= -static
	CPPFLAGS+= -I/usr/lib/c++/v1
else
	LFLAGS+=-lglut -lgl
endif

hello: main.cpp
	$(CXX) -o $@ --std=c++11 $(CPPFLAGS) main.cpp $(LFLAGS)
