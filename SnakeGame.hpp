#pragma once

#include <vector>
#include <list>
#include <complex>
#include <algorithm>

#include "glheaders.h"

struct Vec2 {
	int x, y;

	Vec2() : x(0), y(0) {}
	Vec2(int _x, int _y) : x(_x), y(_y) {}
	Vec2 operator + (const Vec2& r) const {
		return Vec2(x + r.x, y + r.y);
	}
	Vec2 operator - (const Vec2& r) const {
		return Vec2(x - r.x, y - r.y);
	}
	Vec2& operator += (const Vec2& r) {
		x += r.x; y += r.y;
		return *this;
	}
};

class SnakeGame {

	enum State { Wall, Air, Food, Snake };

	State outOfMap;

	static const Vec2 offsets[4];

	std::vector<State> map;

	Vec2 pos;
	Vec2 size;

	bool dead;

	std::list<Vec2> snakePoints;
	std::vector<Vec2> airPoints;

	int snakeLen, snakeToGrow;

	State& _map(int x, int y) {
		if (  x < 0 || y < 0
		   || x >= size.x || y >= size.y )
			{ return outOfMap; }

		return map[x * size.y + y];
	}
	State& _map(Vec2 pos) {
		return _map(pos.x, pos.y);
	}

public:

	SnakeGame
		( int sizeX, int sizeY
		, int startX, int startY
		, int foodX, int foodY )
	{
		map.assign(sizeX * sizeY, Air);
		dead = false;
		snakeLen = 3;
		outOfMap = Wall;
		pos = Vec2(startX, startY);
		size= Vec2(sizeX , sizeY);
		_map(startX, startY) = Snake;
		_map( foodX,  foodY) = Food;

		snakePoints.push_back(pos);
		for (int x=0; x<sizeX; ++x) {
			for (int y=0; y<sizeY; ++y) {
				airPoints.push_back(Vec2(x, y));
			}
		}
	}

	template <class Rand>
	static SnakeGame* randGame
		( int sizeX, int sizeY, Rand rand)
	{
		int x0 = sizeX / 3;
		int x1 = sizeX - x0*2;

		int y0 = sizeY / 3;
		int y1 = sizeY - y0*2;

		int px = rand()%x1 + x0;
		int py = rand()%y1 + y0;

		int fx = px;
		int fy = py;
		
		while (fx == px && fy == py) {
			fx = rand()%x1 + x0;
			fy = rand()%y1 + y0;		
		}

		return new SnakeGame(
			sizeX, sizeY,
			px, py,
			fx, fy
		);
	} 

	bool isGameOver() {
		return dead;
	}

	template <class Rand>
	void next(int operation, Rand rand) {
		if (dead) return;

		int genFood = 0;
		Vec2 nextPos = pos + offsets[operation];
		switch (_map(nextPos)) {
			case Snake:
			case Wall: {
				dead = true;
				return;
			}
			case Food: {
				snakeLen += 3;
				genFood = 1;
				break;
			}
			case Air: {
				break;
			}
		}

		_map(nextPos) = Snake;
		snakePoints.push_back(nextPos);
		while (snakePoints.size() > snakeLen) {
			Vec2 p = snakePoints.front();
			_map(p) = Air;
			airPoints.push_back(p);
			snakePoints.pop_front();
		}
		while (genFood-- > 0) {
			int i = rand() % airPoints.size();
			Vec2 foodPos = airPoints[i];
			airPoints[i] = airPoints.back();
			airPoints.pop_back();
			_map(foodPos) = Food;
		}

		pos = nextPos;
	}

	friend void renderGameIO(SnakeGame *game);

};

const Vec2 SnakeGame::offsets[4] = {
	Vec2(0, -1), Vec2(-1, 0),
	Vec2(0,  1), Vec2( 1, 0) 
};

void renderGameIO(SnakeGame *game)
{
	auto drawSnake = [] {
		glColor3d(1.0, 1.0, 1.0);
	    glBegin(GL_POLYGON);
	        glVertex2d(0.05, 0.05);
	        glVertex2d(0.05, 0.95);
	        glVertex2d(0.95, 0.95);
	        glVertex2d(0.95, 0.05);
	    glEnd();
	};

	auto drawFood = [] {
		glColor3d(0.2, 0.8, 0.7);
	    glBegin(GL_TRIANGLES);
	        glVertex2d(0.50, 0.05);
	        glVertex2d(0.10, 0.95);
	        glVertex2d(0.90, 0.95);
	    glEnd();		
	};

	glPushMatrix(); 

	extern void Box();

	int sizeX = game->size.x;
	int sizeY = game->size.y;
	glScaled(1.0/sizeX, 1.0/sizeY, 1.0);

	for (int x=0; x<sizeX; ++x) {
		for (int y=0; y<sizeY; ++y) {
			glPushMatrix(); 

			glTranslated(x, y, 0);

			switch (game->_map(x, y)) {
				case SnakeGame::Snake:
					drawSnake();
					break;
				case SnakeGame::Food:
					drawFood();
					break;
				default: ;
			}
			
			glPopMatrix();			
		}
	}
	glPopMatrix();
}