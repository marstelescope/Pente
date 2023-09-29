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

class Player {
    public:
        Player(Board &a_board):
            m_board(a_board),
            m_color(' ')
            {}
            
        virtual void makeTurn() = 0;

        void setColor(char a_color){
            m_color = a_color;
        }

        char getColor() const { return m_color;}

        char getOpponentColor() const { return (m_color == 'W' ? 'B' : 'W'); }

        string getColorName() const { return (m_color == 'W' ? "white" : "black"); }

    protected:
        char m_color;
        Board &m_board;

        // Strategy components
        string suggestMove();
        string findWinning(char a_color);
        string blockWinIn2();
        string maxCapture();
        string captureCheck(char a_color1, char a_color2);
        string find3();
        string find2();
        string find1();
        string findAvailable();
        string findWinning2();
};
     