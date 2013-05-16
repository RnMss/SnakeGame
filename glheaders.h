#pragma once

#if defined(__APPLE__) || defined(MACOSX)
#include <GLUT/glut.h>
#else
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#ifndef glutCloseFunc
#define glutCloseFunc glutWMCloseFunc
#endif