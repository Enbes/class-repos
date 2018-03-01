/*
	Corbin Dotson
	11/17/2016
	CS2401 Project 6: Othello
	Spaces to fill the game board
*/

#ifndef SPACE
#define SPACE

using namespace std;

class space {
	public:
		space() {stat = 0;}
		void flip();
		void place(int color) {stat = color;}
		int status()const {return stat;}
		int other()const;

	private:
		int stat; //0 for empty, 1 for black, 2 for white
};

#endif
