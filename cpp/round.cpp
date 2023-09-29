/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "round.h"

/* ********************************************************************* 
Function Name: play
Purpose:  simulate a round, alternating turns, checking for 
            the game being over or human input to stop or quit
Parameters: 
        a_index - index of current player to make a turn
Return Value: none
Algorithm:
        1) Check if board  is empty. If it is, first move automatically
        placed at the center as per game rules. Current index updated
        2) While game isn't over and human didn't type stop/quit:
            a) Display board during human player's turn
            b) Current player makes turn
            c) Index updated
        3) If stop/quit entered, make exitCall true to escape while loop
        in tournament. Otherwise, declare game over and display board
Assistance Received: none 
********************************************************************* */
void Round::play(int a_index){

    int count = m_board.countNonEmpty();
    if (count == 0){
        m_board.makeMove(m_roster[a_index]->getColor(), "J10");
        cout << "On the first turn, the first player must place a white stone at the center, J10." << endl;
        a_index ^= 1;
    }

    while (!m_board.gameOver() && !m_board.isStop()){
        if (a_index == 0){
            m_board.display();
        }
        m_roster[a_index]->makeTurn();
        a_index ^= 1;
    }
    if (m_board.isStop()){
        m_exitCall = true;
    }
    else{
        cout << "Game over!" << endl << endl;
        m_board.display();
    }
}