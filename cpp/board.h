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
const char EMPTY = 'O';
const int MAXROWCOL = 19;

class Board{
    public:
        Board(){
            boardInit(); 
            m_stop = false; 
            };
        bool load(string a_file);     
        void save(string a_file);
        void display();
        void boardInit();
        bool makeMove(char a_color, string a_move);
        bool makeMove(char a_color, int a_row, int a_column);
        void setCompNext(bool a_compNext){m_compNext = a_compNext;}
        bool gameOver();
        int getHumanScore() const {return m_humanScore;}    
        int getCompScore() const {return m_compScore;}  

        string convertRowCol(int row, int col){
            char c = col + 'A';
            return c + to_string(MAXROWCOL-row);
        }

        void stop(){ m_stop = true; }

        bool isStop() const { return m_stop; }

        bool isCompNext() const { return m_compNext; }

        bool isBlackNext() const { return m_blackNext; }

        // Counts number of non-empty spots on the board
        int countNonEmpty(){
            int count = 0;
            for (int i = 0; i < MAXROWCOL; i++){
                for (int j = 0; j < MAXROWCOL; j++){
                    if (m_board[i][j] != EMPTY){
                        count++;
                    }
                }
            }
            m_moveCount = count;
            return count;
        }

        int getMoveCount() const { return m_moveCount; }

        char getCompColor() const {
            if (m_compNext){
                return m_blackNext ? 'B' : 'W';
            }
            else {
                return m_blackNext ? 'W' : 'B';
            }
        }

        char getHumanColor() const {
            if (m_compNext){
                return m_blackNext ? 'W' : 'B';
            }
            else {
                return m_blackNext ? 'B' : 'W';
            }
        }

        char getColorByPos(int a_row, int a_col) const {
            return m_board[a_row][a_col];
        }

        void setColorByPos(int a_row, int a_col, char a_color){
            m_board[a_row][a_col] = a_color;
        }

        void setCompScore(int a_compScore){ m_compScore = a_compScore; }
        void setHumanScore(int a_humanScore){ m_humanScore = a_humanScore; }
        void setBlackNext(bool a_blackNext){ m_blackNext = a_blackNext; }
        vector<string> findPattern(const string& a_pattern);
        int count5inRow(int a_row, int a_column);

        void checkCapture(int a_row, int a_column);  
        void setCheck(bool a_checkOnly){ m_checkOnly = a_checkOnly; }
        int getCompCap() const { return m_compCaptured; }
        int getHumanCap() const {return m_humanCaptured; }
        void setCompCap(int a_compCap){ m_compCaptured = a_compCap; }
        void setHumanCap(int a_humanCap){ m_humanCaptured = a_humanCap; }

    private:
        bool validateInput(string a_input, char &a_column, int &a_row);
        bool helper5Count(int a_row, int a_column, int a_incRow, int a_incCol);
        int helper4Count(char a_color, int a_row, int a_column, int a_incRow, int a_incCol);
        bool overlap(set<pair<int, int>>& a_currentChain);
        void check5inRow(int a_row, int a_column);
        int count4inRow(char a_color);
        vector<string> patternHelper(const string& a_pattern, int a_row, int a_column, int a_incRow, int a_incCol);

        char m_board[MAXROWCOL][MAXROWCOL];
        int m_humanCaptured;
        int m_humanScore;
        int m_compCaptured;
        int m_compScore;
        int m_fiveCount;
        bool m_compNext;
        bool m_blackNext;
        bool m_stop;
        int m_moveCount; 
        bool m_compWon;
        bool m_humanWon;
        bool m_checkOnly;
        vector<vector<pair<int,int>>>m_winning5;
};
