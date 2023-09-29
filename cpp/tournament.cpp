/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "tournament.h"

/* ********************************************************************* 
Function Name: coinToss
Purpose: simulate a coin toss, used at the start of a new round when 
            scores are tied
Parameters: none
Return Value: returns index of first player to play
Algorithm: 
        1) Generate a random value to simulate coin toss
        2) Announce winner
Assistance Received: none 
********************************************************************* */
int Tournament::coinToss(){
    string input;
    int index = 0;

    cout << "Coin toss to determine first move. Choose 'heads' or 'tails': ";
    cin >> input;

    // Random value generated, converted immediately to heads or tails
    string winner = (rand()%2 == 0 ? "heads" : "tails");      
    cout << winner << " won!" << endl;

    // Allowing user to input any capitaliztion
    for (auto& x : input) {
        x = tolower(x);
    }
        
    // If guessed incorrectly, computer goes first
    m_board.setCompNext(winner != input);
    if (winner != input){
        // Index of computer player
        cout << "Computer goes first!" << endl;
        index = 1;
    }
    else {
        cout << "Human goes first!" << endl;
    }

    return index;
}

/* ********************************************************************* 
Function Name: run
Purpose:  run the tournament, which is comprised of rounds; update
            tournament score at the end of each round
Parameters: 
        a_index - index either read from file or set to zero and to be determined 
                    if board is empty
Return Value: none
Algorithm:
        1) Get current scores from board and store in tournament scores
        2) Play rounds while human player says yes or until exit call
        3) If user didn't exit before end of round, scores displayed
        4) Tournament scores displayed and winner announed if user 
        said no to playing more rounds and there was no premature exit
Assistance Received: none 
********************************************************************* */
void Tournament::run(int a_index){
    Round round(m_board, m_roster);
    // Get current score from m_board and set it as tournament score
    // m_board score variables set to zero as they will track round score
    m_humanScore = m_board.getHumanScore();
    m_compScore = m_board.getCompScore();
    m_board.setCompScore(0);
    m_board.setHumanScore(0);

    string input;

    // Play rounds while human says yes or until exit call
    do {
        // If board if empty, first player is determined
        if (m_board.countNonEmpty() == 0){
            if (m_compScore == m_humanScore){
                a_index = coinToss();
            }
            else if (m_compScore > m_humanScore) {
                cout << "Computer plays first because computer has highest score." << endl;
                a_index = 1;
            }
            else {
                cout << "Human plays first because human has highest score." << endl;
                a_index = 0;
            }

            // Roster, next player and color updated
            m_roster[a_index]->setColor('W');
            m_roster[a_index^1]->setColor('B');
            m_board.setCompNext(a_index == 1);
            m_board.setBlackNext(false);
        }

        // Round starts
        round.play(a_index);     

        // If user didn't call to exit, round ends in a win/drawn 
        // and scores are updated and displayed
        if (!round.exitCall()){
            m_humanScore += m_board.getHumanScore();  
            m_compScore += m_board.getCompScore();
            cout << "Round score:" << endl;
            cout << "Human: " << m_board.getHumanScore() << endl;
            cout << "Computer: " << m_board.getCompScore() << endl;
            cout << "Winner of this round: ";
            if (m_board.getHumanScore() > m_board.getCompScore()){
                cout << "human player!" << endl;
            }
            else if (m_board.getHumanScore() <<m_board.getCompScore()){
                cout << "computer player!" << endl;
            }

            cout << endl << "Tournament scores:" << endl;
            cout << "Human: " << m_humanScore << endl;
            cout << "Computer: " << m_compScore << endl << endl; 

            m_board.boardInit(); 

            cout << "Play another round? Enter 'Y' or 'N': " << endl; 
            cin >> input;
   
            while (input != "Y" && input != "N" && input != "y" && input != "n"){
                cout << "Please enter 'Y' or 'N': ";
                cin >> input;
            }
            cout << endl;
            if (input == "n" || input == "N"){
                break;
            }
        }
    } while (!round.exitCall());
    
    // If user didn't call to exit prematurely, tournament scores and winner displayed
    if (!round.exitCall()){
        endTournament();
    }

    // Updated to be saved accurately into file
    m_board.setHumanScore(m_humanScore);
    m_board.setCompScore(m_compScore);
}


/* ********************************************************************* 
Function Name: endTournament
Purpose:   display tournament scores at the end of the tournament 
            and announce winner
Parameters: none
Return Value: none
Algorithm: none
Assistance Received: none 
********************************************************************* */
void Tournament::endTournament(){
    cout << "Tournament scores:" << endl;
    cout << "Human: " << m_humanScore << endl;
    cout << "Computer: " << m_compScore << endl << endl; 
    if (m_compScore > m_humanScore){
        cout << "The winner of the tournament is the computer player!" << endl;
    }
    else if (m_compScore < m_humanScore) {
        cout << "The winner of the tournament is the human player!" << endl;
    }
    else {
        cout << "The tournament ends in a draw!" << endl;
    }
}