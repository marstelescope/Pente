/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "stdafx.h"
#include "board.h"
#include "player.h"
#include "round.h"
#include "tournament.h"
#include "computer.h"
#include "human.h"   

int main(){
    // Creating board and player objects to start the game
    Board board;
    Player * roster[2]; 
    roster[0] = new Human(board);
    roster[1] = new Computer(board);

    // Index to keep track of turn
    int index = 0;

    // Seed to ensure randomness of coin toss 
    // + random moves by computer, if required
    srand (time(NULL));

    // Start of I/O
    string input;
    cout << "Load existing game? Enter 'Y' or 'N': ";       
    cin >> input;

    string filename;
    bool fileLoaded = false;
    
    // Validate user input
    while (input != "Y" && input != "N" && input != "y" && input != "n"){
        cout << "Please enter 'Y' or 'N': ";
        cin >> input;
    }

    if (input == "y" || input == "Y"){
        cout << "Enter file name: ";
        cin >> filename;

        // File not found or errors in file
        if (!board.load(filename)){
            cout << "Starting new game." << endl;
            board.boardInit();
        }
        // File properly read and loaded, set player colors
        else{
            fileLoaded = true;
            index = (board.isCompNext() ? 1 : 0);               
            roster[index]->setColor(board.isBlackNext() ? 'B' : 'W');
            roster[index^1]->setColor(board.isBlackNext() ? 'W' : 'B');
        }
    }

    // Allow user to name new game file
    if (!fileLoaded){
        cout << "Enter file name ending in .txt to store new game: ";
        cin >> filename;
    }
    cout << endl;

    // Tournament begins/resumes
    Tournament tournament(board, roster);
    tournament.run(index);
    
    // Board is saved into file when user quits/saves/tournament ends
    board.save(filename);

    return 0;
}