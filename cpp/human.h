/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#pragma once 
#include "stdafx.h"
#include "board.h"
#include "player.h"

class Human : public Player {
    public:
        Human(Board &a_board):
            Player(a_board)
            {}
        virtual void makeTurn();
};   