/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "human.h"

/* ********************************************************************* 
Function Name: makeTurn (human)
Purpose:  allow human player to make a turn, stop the game, or ask for help
Parameters: none
Return Value: none
Algorithm:
        1) Ask human to place a stone
        2) Check input for stop or quit: round and tournament will be stopped
        3) Check input for help: suggestMove strategy will be used to place the 
        move for the player
        4) Check if move is valid by attempting to place it. If placement returns 
        false, output "Invalid input" and repeat algorithm
Assistance Received: none 
********************************************************************* */
void Human::makeTurn(){
    bool validMove = false;
    do {
        cout << "Enter intersection to place a " << getColorName() << " stone: ";
        string input;
        cin >> input;
        if (input == "stop" || input == "STOP" || input == "quit" || input == "QUIT"){
            m_board.stop();
            return;
        }
        if (input == "help" || input == "HELP"){
            input = suggestMove();
            cout << "Suggested move: " << input << endl;
            cout << "Enter intersection to place a " << getColorName() << " stone: ";
            cin >> input;
        }
        validMove = m_board.makeMove(m_color, input);
        if (!validMove){
            cout << "Invalid input." << endl;
        }
    } while (!validMove);
}
