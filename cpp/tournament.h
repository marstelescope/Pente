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
#include "round.h"

class Tournament{
    public:
        Tournament(Board& a_board, Player** a_roster):
            m_board(a_board),
            m_roster(a_roster),
            m_compScore(0),
            m_humanScore(0)
            {}
        int coinToss();
        void run(int a_index);

    private:
        Board& m_board;
        Player** m_roster;
        int m_compScore;
        int m_humanScore;
        void endTournament();
};