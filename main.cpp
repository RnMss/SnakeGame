#ifndef CALLBACK
#include <windef.h>
#endif

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/freeglut.h>

#include <cstdio>
#include <unistd.h>

void onWindowDraw()
{
    glClearColor (0.0, 0.0, 0.0, 0.0);
    glClear (GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0, 1.0, 1.0, 0.0, -1e5, 1e5);

    glColor3d(1.0, 1.0, 1.0);
    glBegin(GL_POLYGON);
        glVertex2d(0.25, 0.00);
        glVertex2d(0.00, 0.75);
        glVertex2d(1.00, 1.00);
    glEnd();
    glFlush();

    glutSwapBuffers();
}

void onWindowResize(int w, int h)
{
    glViewport(0, 0, w, h);
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);

    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

    glutInitWindowSize(250, 250);
    glutInitWindowPosition(100, 100);

    glutCreateWindow("Hello Snake");

    //glutKeyboardFunc(Key);

    glutCloseFunc([] {
        for (int i = 0; i < 3; ++i) {
            puts("bye?");
            sleep(1);
        }
        puts("BYE.");
    });

    glutDisplayFunc(onWindowDraw);
    glutReshapeFunc(onWindowResize);

    glutMainLoop();
    return 0;
}