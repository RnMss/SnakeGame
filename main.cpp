#include "glheaders.h"

#include "SnakeGame.hpp"

#include <random>

#include <cstdio>
#include <unistd.h>

int operation = 0;
SnakeGame *game = nullptr;
std::default_random_engine randGen;

void newGameIO()
{
    randGen.seed(std::random_device()());

    delete game;
    game = SnakeGame::randGame(15, 15, randGen);
}

void Box()
{
    glColor3d(1.0, 1.0, 1.0);
    glBegin(GL_LINE_LOOP);
        glVertex2d(0.05, 0.05);
        glVertex2d(0.05, 0.95);
        glVertex2d(0.95, 0.95);
        glVertex2d(0.95, 0.05);
    glEnd();
}

void onWindowDrawIO()
{
    glClearColor (0.0, 0.0, 0.0, 0.0);
    glClear (GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glOrtho(0.0, 1.0, 1.0, 0.0, -1.0, 1.0);

    Box();
    renderGameIO(game);

    glFlush();
    glutSwapBuffers();
}

void onWindowResizeIO(int w, int h)
{
    glViewport(0, 0, w, h);
    glutPostRedisplay();
}

void onKeyboardIO(unsigned char key, int x, int y)
{
    switch (key) {
        case 'a': case 'A': {
            operation = 1;
            break;
        }
        case 's': case 'S': {
            operation = 2;
            break;
        }
        case 'd': case 'D': {
            operation = 3;
            break;
        }
        case 'w': case 'W': {
            operation = 0;
            break;
        }
        case 'o': case 'O': {
            newGameIO();
            break;
        }
        default: {

        }
    }
}

void onTimerIO()
{
    game->next(operation, randGen);
    if (game->isGameOver()) {
        glutSetWindowTitle("Dead Snake");
    } else {
        glutSetWindowTitle("Live Snake");
    }
    glutPostRedisplay();
}

void wrapperTimerIO(int param)
{
    onTimerIO();
    glutTimerFunc(230, wrapperTimerIO, param+1);
}

bool g_mainLoop = true;
int main(int argc, char *argv[])
{
    glutInit(&argc, argv);

    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

    glutInitWindowSize(480, 480);

    glutCreateWindow("Hello Snake");

    glutKeyboardFunc(onKeyboardIO);
    glutDisplayFunc(onWindowDrawIO);
    glutReshapeFunc(onWindowResizeIO);

    newGameIO();

    glutTimerFunc(300, wrapperTimerIO, 0);

    glutCloseFunc([] { g_mainLoop = false; });
    while (g_mainLoop) { glutCheckLoop(); }

    return 0;
}
