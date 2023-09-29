/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "computer.h"

/* ********************************************************************* 
Function Name: makeTurn (computer)
Purpose:  simulate the computer playing its turn
Parameters: none
Return Value: none
Algorithm: 
        1) Receive a move using strategy in suggestMove
        2) If move not blank, place it on the board, otherwise 
        make a random move (shouldn't ever get there)
        3) Output where the stone was placed
Assistance Received: none 
********************************************************************* */

void Computer::makeTurn(){
    cout << "Computer turn" << endl;
    string move = suggestMove();
    cout << "The computer placed a " << getColorName() << " stone at ";

    if (!move.empty()){
        m_board.makeMove(m_color, move);

        cout << move << endl;  
    }
    else{
        // Random implementation just in case
        int row = 0, col = 0;
        do {
            row = rand()%MAXROWCOL;
            col = rand()%MAXROWCOL;
        } while (!m_board.makeMove(m_color, row, col));

        cout << m_board.convertRowCol(row, col) << endl;
    }
}