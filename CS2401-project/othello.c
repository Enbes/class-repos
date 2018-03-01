/*
	Corbin Dotson
	11/17/2016
	CS2401 Project 6: Othello
	Othello class implementation
*/

#include <iostream>
#include <string>
#include <cstdlib>
#include "game.h"	
#include "othello.h"
#include "colors.h"

using namespace std;

// Have the next player make a specified move:
void Othello::make_move(const string& move) {
	if (move != "pass") {
		int col = to_int(toupper(move[0]) ), row = to_int(move[1]);
		int spc, other;
		bool cp, cm;

		if (next_mover() == HUMAN) {
			board[row][col].place(1);
			spc = 1;
			other = 2;
		}

		else {
			board[row][col].place(2);
			spc = 2;
			other = 1;
		}

			if (next_mover() == HUMAN) {
				spc = 1;
				other = 2;
			}
			else {
				spc = 2;
				other = 1;
			}

		//flanking area, watch your back
		if (col+1 != 8) {
			cp = true;
			//right
			if (board[row][col+1].status() == other) {
				if (is_flanking(row, col+1, 0, 1, spc) )
					flank(row, col+1, 0, 1, spc);
			}
		}
		if (col-1 != -1) {
			cm = true;
			//left
			if (board[row][col-1].status() == other) {
				if (is_flanking(row, col-1, 0, -1, spc) )
					flank(row, col-1, 0, -1, spc);
			}
		}
		if (row+1 != 8) {
			//bottom
			if (board[row+1][col].status() == other) {
				if (is_flanking(row+1, col, 1, 0, spc) )
				flank(row+1, col, 1, 0, spc);
			}
			//bottom-right
			if (cp == true) {
				if (board[row+1][col+1].status() == other) {
					if (is_flanking(row+1, col+1, 1, 1, spc) )
						flank(row+1, col+1, 1, 1, spc);
				}
			}
			//bottom-left
			if (cm == true) {
				if (board[row+1][col-1].status() == other) {
					if (is_flanking(row+1, col-1, 1, -1, spc) )
						flank(row+1, col-1, 1, -1, spc);
						}	
			}				
		}
		if (row-1 != -1) {
			//top
			if (board[row-1][col].status() == other) {
				if (is_flanking(row-1, col, -1, 0, spc) )
				flank(row-1, col, -1, 0, spc );
			}
			//top-right	
			if (cp == true) {
				if (board[row-1][col+1].status() == other) {
					if (is_flanking(row-1, col+1, -1, 1, spc) )
						flank(row-1, col+1, -1, 1, spc );
				}
			}
			//top-left
			if (cm == true) {
				if (board[row-1][col-1].status() == other) {
					if (is_flanking(row-1, col-1, -1, -1, spc) )
						flank(row, col-1, -1, -1, spc );
				}
			}	
		}
		if (pass_count == 1)
			pass_count--;
	}
	else
		pass_count++;

	game::make_move(move);
}

// Restart the game from the beginning:
void Othello::restart() {
	for (int i = 0; i < 8; ++i) {
		for (int j = 0; j < 8; ++j)
			board[i][j].place(0);
	}
	board[3][3].place(2);
	board[3][4].place(1);	
	board[4][3].place(1);
	board[4][4].place(2);			
}

// Return a pointer to a copy of myself:
main_savitch_14::game* Othello::clone()const {
	return new Othello(*this);
}

// Compute all the moves that the next player can make:
void Othello::compute_moves(queue<string>& moves)const {
	string str;
	for (char i = 'A'; i < 'I'; i++) {
		for (char j = '1'; j < '9'; j++) {
			str = i;
			str += j;
			if (is_legal(str) )
				moves.push(str);
		}
	}
	if (can_pass() )
		moves.push("pass");
}

// Display the status of the current game:
void Othello::display_status()const {
	bool alt = false;
	cout << " A B C D E F G H\n";
	for (int i = 0; i < 8; ++i) {
		cout << i+1;
		for (int j = 0; j < 8; ++j) {
			if (alt == false) {
				if (board[i][j].status() == 0)
					cout << B_BLUE << "  " << RESET;
				else if (board[i][j].status() == 1)
					cout << B_BLUE << BLACK << "O " << RESET;
				else
					cout << B_BLUE << WHITE << "O " << RESET;
			}
			else {
				if (board[i][j].status() == 0)
					cout << B_RED << "  " << RESET;
				else if (board[i][j].status() == 1)
					cout << B_RED << BLACK << "O " << RESET;
				else
					cout << B_RED << WHITE << "O " << RESET;
			}

			if (alt == false)
				alt = true;
			else
				alt = false;
		}
		cout << i+1;
		if (alt == false)
			alt = true;
		else
			alt = false;		
		cout << endl;
	}
	cout << " A B C D E F G H\n";
}

// Evaluate a board position:
int Othello::evaluate()const {
	int p_moves = 0, black = 0, white = 0;	//p_moves is the value of possible next moves, so that it's less likely to have to pass
	string str;
	for (char i = 'A'; i < 'I'; i++) {
		for (char j = '1'; j < '9'; j++) {
			str = i;
			str += j;
			if (is_legal(str) ) {	//since it's not guarenteed to get the space, each is worth 1, edges and corners don't increase the value very much
				p_moves++;
				if (str[0] == 'A' || str[0] == 'H')
					p_moves++;
				if (str[1] == '1' || str[1] == '8')
					p_moves++;
			}
		}
	}
	if (p_moves == 0) //again, to make sure it tries to avoid passing, it matters less as the board fills up
		p_moves = -15;

	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			if (board[i][j].status() == 1) {	//every space is worth 1, edges are 3, corners are 5
				black++;
				if (i == 0 || i == 7)
					black += 2;
				if (j == 0 || j == 7)
					black += 2;
			}
			else if (board[i][j].status() == 2) {
				white++;
				if (i == 0 || i == 7)
					white += 2;
				if (j == 0 || j == 7)
					white += 2;
			}
		}
	}

	srand(time(NULL) );
	return white + p_moves/2 - black + rand()%11;	//based primarily on white vs black spaces, but possible moves and chance can sway it - and are more important early on
}

// Return true if the current game is finished:
bool Othello::is_game_over()const {
	if (moves_completed() == 60 || pass_count == 2)
		return true;
	return false;
}

// Return true if the given move is legal for the next player:
// it might be a bit convoluted, but it gets the job done... that said, this can probably be done with a for loop
bool Othello::is_legal(const string& move)const {
	if (move == "pass") {
		return can_pass();
	}
	else {
		int col = to_int(toupper(move[0]) ), row = to_int(move[1]);

		if (board[row][col].status() == 0) {
			bool cp, cm;
			int spc, other;

			if (next_mover() == HUMAN) {
				spc = 1;
				other = 2;
			}
			else {
				spc = 2;
				other = 1;
			}

			//direction-finding for flanking
			if (col+1 != 8) {
				cp = true;
				//right
				if (board[row][col+1].status() == other) {
					if (is_flanking(row, col+1, 0, 1, spc) )
						return true;
				}
			}
			if (col-1 != -1) {
				cm = true;
				//left
				if (board[row][col-1].status() == other) {
					if (is_flanking(row, col-1, 0, -1, spc) )
						return true;
				}
			}
			if (row+1 != 8) {
				//bottom
				if (board[row+1][col].status() == other) {
					if (is_flanking(row+1, col, 1, 0, spc) )
						return true;
				}
				//bottom-right
				if (cp == true) {
					if (board[row+1][col+1].status() == other) {
						if (is_flanking(row+1, col+1, 1, 1, spc) )
							return true;
					}
				}
				//bottom-left
				if (cm == true) {
					if (board[row+1][col-1].status() == other) {
						if (is_flanking(row+1, col-1, 1, -1, spc) )
							return true;
					}		
				}				
			}	
			if (row-1 != -1) {
				//top
				if (board[row-1][col].status() == other) {
					if (is_flanking(row-1, col, -1, 0, spc ) )
						return true;
				}
				//top-right	
				if (cp == true) {
					if (board[row-1][col+1].status() == other) {
						if (is_flanking(row-1, col+1, -1, 1, spc ) )
							return true;
					}
				}
				//top-left
				if (cm == true) {
					if (board[row-1][col-1].status() == other) {
						if (is_flanking(row-1, col-1, -1, -1, spc ) )
							return true;
					}
				}	
			}
		}
	}
	return false;
}

main_savitch_14::game::who Othello::winning()const {
	int black = 0, white = 0;
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			if (board[i][j].status() == 1)
				black++;
			else if (board[i][j].status() == 2)
				white++;
		}
	}
	if (black > white)
		return HUMAN;
	else if (black < white)
		return COMPUTER;
	else
		return NEUTRAL;
}

//returns an int that the parameter is referring to, returns 8 if not valid
int Othello::to_int(const char& ch)const {
	if (ch >= 'A' && ch < 'I') {
		if (ch == 'A') return 0;
		else if (ch == 'B') return 1;
		else if (ch == 'C') return 2;
		else if (ch == 'D') return 3;
		else if (ch == 'E') return 4;
		else if (ch == 'F') return 5;
		else if (ch == 'G') return 6;
		else if (ch == 'H') return 7;
 	}
 	else if (ch > '0' && ch < '9') {
 		if (ch == '1') return 0;
 		else if (ch == '2') return 1;
 		else if (ch == '3') return 2;
 		else if (ch == '4') return 3;
 		else if (ch == '5') return 4;
 		else if (ch == '6') return 5;
 		else if (ch == '7') return 6;
 		else if (ch == '8') return 7;
 	}
 	else 
		return 8;
}

//returns true if the move will flank anything in the given direction
bool Othello::is_flanking(const int& row, const int& col, const int& change_x, const int& change_y, const int& ori)const {
	if (row == -1 || row == 8 || col == 8 || col == -1 || board[row][col].status() == 0)
		return false;

	if (board[row][col].status() == ori)
		return true;

	is_flanking(row+change_x, col+change_y, change_x, change_y, ori);
}

//flanks all eligiable pieces from the starting position
void Othello::flank(const int& row, const int& col, const int& change_x, const int& change_y, const int& ori) {
	if (row == -1 || row == 8 || col == 8 || col == -1 || board[row][col].status() == 0 || board[row][col].status() == ori)
		return;

	board[row][col].flip();

	flank(row+change_x, col+change_y, change_x, change_y, ori);
}

//returns true if the current player can pass
bool Othello::can_pass()const {
	string str;
	for (char i = 'A'; i < 'I'; i++) {
		for (char j = '1'; j < '9'; j++) {
			str = i;
			str += j;
			if (is_legal(str) )
				return false;
		}
	}
	return true;
}
