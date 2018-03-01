/*
	Corbin Dotson
	11/17/2016
	CS2401 Project 6: Othello
	A virtual implementation of the Othello board game
*/

#include <iostream>
#include <cstdlib>
#include "othello.h"

using namespace std;

int main() {
	Othello oth;

	oth.play();
	if (oth.winning() == main_savitch_14::game::HUMAN)
		cout << "You win!\n";
	else if (oth.winning() == main_savitch_14::game::COMPUTER)
		cout << "You lose.\n";
	else
		cout << "Stalemate.\n";
	return EXIT_SUCCESS;
}
