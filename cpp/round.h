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

class Round{
    public:
        Round(Board& a_board, Player** a_roster):
            m_exitCall(false),
            m_board(a_board),
            m_roster(a_roster)
            {}
        void play(int a_index);
        bool exitCall() const {return m_exitCall;};
    private:
        bool m_exitCall;
        Board& m_board;
        Player** m_roster;
};