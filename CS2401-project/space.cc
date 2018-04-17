/*
	Corbin Dotson
	11/17/2016
	CS2401 Project 6: Othello
	space class implementation
*/

#include "space.h"

using namespace std;

//flips the "chip" to the other color
void space::flip() {
	if (stat == 1)
		stat = 2;
	else if (stat == 2)
		stat = 1;
}

int space::other()const {
	if (stat == 1)
		return 2;
	else if (stat == 2)
		return 1;
}
