/*
	Corbin Dotson
	11/17/2016
	CS2401 Project 5: Othello
	Othello class derived from game class, contains everything needed to manipulate the board
*/

#ifndef OTHELLO
#define OTHELLO

#include "game.h"
#include "space.h"

using namespace std;

class Othello: public main_savitch_14::game {
	public:
		Othello(){pass_count = 0;}
		//inhereted
    	void make_move(const string& move);
    	void restart();
    	main_savitch_14::game* clone()const;
    	void compute_moves(queue<string>& moves)const;
    	void display_status()const;
    	int evaluate()const;
    	bool is_game_over()const;
    	bool is_legal(const string& move)const;
    	who winning()const;

	private:	
		space board[8][8];
		int pass_count;

		int to_int(const char& ch)const;
		bool is_flanking(const int& row, const int& col, const int& change_x, const int& change_y, const int& ori)const;
		void flank(const int& row, const int& col, const int& change_x, const int& change_y, const int& ori);
		bool can_pass()const;
};

#endif
