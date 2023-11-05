/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "player.h"

/* ********************************************************************* 
Function Name: suggestMove
Purpose:  suggest a move for current player (computer always, human when
            help is requested) based on strategy in order of importance
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Check to find highest scoring winning move. If multiple winning
        moves and opponent isn't one move away from winning, winning delayed.
        Otherwise, winning placed.
        2) Check if oppenent can win and block this move. This is second priority
        3) Check if opponent can win in two moves, and block one of them
        4) Check to find highest scoring capture, if any
        5) Check if current player can avoid being captured
        6) If current player has a chain of 3 stones, add a 4th
        7) If current player has a chain of 2 stones, add a 3rd
        8) If current player has one stone, add another near it
        9) Find any available spot 
Assistance Received: none 
********************************************************************* */
string Player::suggestMove(){
    string move;

    cout << "Strategy: ";

    // Find highest scoring winning move. If multiple winning moves and 
    // opponent isn't one move away from winning, winning delayed, next >
    move = findWinning2();
    if (!move.empty()){
        cout << "Found winning move with highest score: " << move << endl;
        return move;
    }

    // Check if oppenent can win and block this move
    move = findWinning(getOpponentColor());
    if (!move.empty()){
        cout << "Block winning move: " << move << endl;
        return move;
    }

    // Check if opponent can win in two moves, and block one of them
    move = blockWinIn2();
    if (!move.empty()){
        cout << "Block winning in two moves: " << move << endl;
        return move;
    }

    //  Check if opponent can be captured, find highest scoring 
    move = maxCapture();
    if (!move.empty()){
        cout << "Capture opponent: " << move << endl;
        return move;
    }

    // Check if current player can avoid being captured
    move = captureCheck(getColor(), getOpponentColor());
    if (!move.empty()){
        cout << "Avoid being captured: " << move << endl;
        return move;
    }

    // Add 4th stone to an existing chain of 3
    move = find3();
    if (!move.empty()){
        cout << "Create a row of 4 stones: " << move << endl;
        return move;
    }

    // Add 3rd stone to an existing chain of 2
    move = find2();
    if (!move.empty()){
        cout << "Create a row of 3 stones: " << move << endl;
        return move;
    }

    // Add a stone near an existing stone
    move = find1();
    if (!move.empty()){
        cout << "Add a stone near existing cluster: " << move << endl;
        return move;
    }

    // If all else fails
    cout << "First available slot." << endl;
    return findAvailable();
}


/* ********************************************************************* 
Function Name: findWinning2
Purpose:  find highest scoring winning move. Delay winning to increase 
            score if there are multiple winning moves and opponent isn't 
            one move away from winning
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Go through every position on the board, scanning for empty
        ones, where a move can be placed
        2) Temporarily set the space to current player's color where empty
        3) Count how many 5 in a rows that placement results in, store
        value in local variable count
        4) If 5 in a row achieved, pushback that position to vector for
        that count of 5s in the map, and incremement countWinPos of winning
        positions available
        5) Reset that board space back to empty
        6) After scanning entire board, if there are 2 or more winning positions
        and opponent can't win in one turn, postpone winning move to increase score
        7) If there is only one winning position or opponent can win next turn,
        return most scoring spot
        8) If there are no winning spots, return empty
Assistance Received: none 
********************************************************************* */
string Player::findWinning2(){    
    int countWinPos = 0;    
    map<int, vector<pair<int,int>>> maxScore;

    // Scan board for places where moves can be placed
    for (int i = 0; i < MAXROWCOL; i++){
        for (int j = 0; j < MAXROWCOL; j++){
            if (m_board.getColorByPos(i,j) == EMPTY){
                // Temporary stone placement
                m_board.setColorByPos(i, j, getColor());

                // Record score that that placement results in if > 0
                int count = m_board.count5inRow(i, j);
                if (count > 0){
                    maxScore[count].push_back({i,j});
                    countWinPos++;
                }

                // Restore board position
                m_board.setColorByPos(i, j, EMPTY);
            }
        }
    }
    // If there are 2 or more winning position and opponent can't win right 
    // away, postpone winning move to increase score
    if (countWinPos > 1 && findWinning(getOpponentColor()) == ""){
        cout << "Postponing winning to increase score." << endl;
    }
    // Else, if there is a winning position, win 
    else if (!maxScore.empty()){
        // Placing move on most scoring spot
        return m_board.convertRowCol(maxScore.rbegin()->second[0].first, maxScore.rbegin()->second[0].second);
    }

    // No winning position
    return "";
}

/* ********************************************************************* 
Function Name: findWinning
Purpose:  find clusters of 4 stones that can become a 5 in a row
Parameters: 
        a_color - color of player
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Go through the different cluster of 4 arrangements and pass 
        them to findPattern
        2) Store the pattern location returned in a local vector patternLoc
        3) If patternLoc has the winning pattern stored, return
        the empty spot where stone should be placed to win the game
        4) If there are no winning spots, return empty
Assistance Received: none 
********************************************************************* */
string Player::findWinning(char a_color){
    vector<string> patternLoc;
    char pat[6];
    pat[5] = 0;

    // Shifting the pattern we are searching for
    // i.e. _CCCC_ could win, C_CCC_, CC_CC_, etc
    for (int i = 0; i < 5; i++){
        for (int j = 0; j < 5; j++){
            pat[j] = a_color;
        }
        pat[i] = EMPTY;

        // Store the pattern
        patternLoc = m_board.findPattern(pat);

        if (!patternLoc.empty()){
            // If the pattern isn't empty, return the empty winning spot 
            return patternLoc[i];
        }
    }

    // No winning position
    return "";
}

/* ********************************************************************* 
Function Name: blockWinIn2
Purpose:  find clusters that could win in two moves and block
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Go through the different cluster arrangements that can win
        in two moves (_*CCC_, _C*CC_, and so on) and pass them to findPattern
        2) Store the pattern location returned in a local vector patternLoc
        3) If patternLoc has the winning pattern stored, return
        the empty spot where stone should be placed to block win in 2
        4) If there are no winning spots, return empty
Assistance Received: none 
********************************************************************* */
string Player::blockWinIn2(){
    vector<string> patternLoc;
    char pat[7];
    pat[6] = 0;
    pat[0] = EMPTY;
    pat[5] = EMPTY;

    // Shifting the pattern we are searching for
    // i.e. __CCC_ could win in 2, _C_CC_, etc
    for (int i = 1; i < 5; i++){
        for (int j = 1; j < 5; j++){
            pat[j] = getOpponentColor();
        }
        pat[i] = EMPTY;
        patternLoc = m_board.findPattern(pat);

        // Block win in 2 found
        if (!patternLoc.empty()){
            return patternLoc[i];
        }
    }

    // No win in 2 patterns
    return "";
}

/* ********************************************************************* 
Function Name: maxCapture
Purpose:  find most scoring capture, if any available
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Go through every position on the board, scanning for empty
        ones, where a move can be placed
        2) Temporarily set the space to current player's color where empty
        3) Check if captured score changed
        4) Pushback to maxScore[scoreChange] the position 
        5) Restore changed values 
        6) Return the spot that scores the most captures, if any
        7) Otherwise, return empty
Assistance Received: none 
********************************************************************* */
string Player::maxCapture(){    
    int countWinPos = 0;    
    map<int, vector<pair<int,int>>> maxScore;

    // In order for values not to be altered
    int compCaptured = m_board.getCompCap();
    int humanCaptured = m_board.getHumanCap();

    // Scan board for places where moves can be placed
    for (int i = 0; i < MAXROWCOL; i++){
        for (int j = 0; j < MAXROWCOL; j++){
            if (m_board.getColorByPos(i,j) == EMPTY){
                // Temporarily stone placement
                m_board.setColorByPos(i, j, getColor());
                m_board.setCheck(true);
                m_board.checkCapture(i,j);
                
                // If placement resulted in a score change, store score
                if (compCaptured != m_board.getCompCap()){
                    maxScore[m_board.getCompCap() - compCaptured].push_back({i,j});
                    m_board.setCompCap(compCaptured);
                }
                if (humanCaptured != m_board.getHumanCap()){
                    maxScore[m_board.getHumanCap() - humanCaptured].push_back({i,j});
                    m_board.setHumanCap(humanCaptured);
                }

                // Restore values
                m_board.setColorByPos(i, j, EMPTY);
                m_board.setCheck(false);
            }
        }
    }
    if (!maxScore.empty()){
        // Placing move on most scoring spot
        return m_board.convertRowCol(maxScore.rbegin()->second[0].first, maxScore.rbegin()->second[0].second);
    }

    // No winning position
    return "";
}

/* ********************************************************************* 
Function Name: captureCheck
Purpose:  find capture shaped clusters, both to capture and to avoid being 
            captures
Parameters: 
        a_color1 - first color in a position of being captured
        a_color2 - other color in position that could capture
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Set up the patterns _112 and 211_ and check the board
        using findPattern if they are present
        2) If so, return blank spot that captures/avoids being captured
        3) If no such pattern found, return empty string (no move suggested)
Assistance Received: none 
********************************************************************* */
string Player::captureCheck(char a_color1, char a_color2){
    vector<string> patternLoc;
    char pat[5];
    pat[4] = 0;

    // Checking for first pattern _112
    pat[0] = EMPTY;
    pat[1] = pat[2] = a_color1;
    pat[3] = a_color2;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[0];
    }

    // Checking for second pattern 211_
    pat[0] = a_color2;
    pat[3] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[3];
    }

    return "";
}

/* ********************************************************************* 
Function Name: find3
Purpose:  find clusters of 3 stones that can turn to 4
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Set up various clusters of 3 patterns
        2) Check if they can be found on the board
        3) If so, return move that will turn cluster to 4
        4) If no patterns are found, return empty
Assistance Received: none 
********************************************************************* */
string Player::find3(){
    vector<string> patternLoc;
    char pat[6];
    pat[5] = 0;

    // Pattern *CCC_ (star is returning value)
    pat[0] = pat[4] = EMPTY;
    pat[1] = pat[2] = pat[3] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[0];
    }

    // Pattern _*CCC (star is returning value)
    pat[1] = EMPTY;
    pat[4] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }

    // Pattern CCC*_ (star is returning value)
    pat[0] = pat[1] = getColor();
    pat[3] = pat[4] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[3];
    }

    // Pattern C*CC_ (star is returning value)
    pat[1] = EMPTY;
    pat[3] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }

    // Pattern _C*CC (star is returning value)
    pat[0] = pat[2] = EMPTY;
    pat[1] = pat[4] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[2];
    }
    
    // Pattern CC*C_ (star is returning value)
    pat[0] = getColor();
    pat[4] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[2];
    }

    // Pattern _CC*C (star is returning value)
    pat[0] = pat[3] = EMPTY;
    pat[2] = pat[4] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[3];
    }

    return "";
}

/* ********************************************************************* 
Function Name: find2
Purpose:  find clusters of 2 stones that can turn to 3
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Set up various clusters of 2 patterns
        2) Check if they can be found on the board
        3) If so, return move that will turn cluster to 3
        4) If no patterns are found, return empty
Assistance Received: none 
********************************************************************* */
string Player::find2(){
    vector<string> patternLoc;
    char pat[5];
    pat[4] = 0;

    // Pattern *CC_ (star is returning value)
    pat[0] = pat[3] = EMPTY;
    pat[1] = pat[2] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[0];
    }

    // Pattern _*CC (star is returning value)
    pat[1] = EMPTY;
    pat[3] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }

    // Pattern CC*_ (star is returning value)
    pat[0] = pat[1] = getColor();
    pat[3] = pat[2] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[2];
    }

    // Pattern C*C_ (star is returning value)
    pat[1] = EMPTY;
    pat[2] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }

    // Pattern _C*C (star is returning value)
    pat[0] = pat[2] = EMPTY;
    pat[1] = pat[3] = getColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[2];
    }
    
    return "";
}

/* ********************************************************************* 
Function Name: find1
Purpose:  find single stones near which another stone can be placed
            while also avoiding being captured
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) If move count 1, suggested move is near the center stone
        2) If move count is 2, must place stone 3 intersections
        away from center. Two options provided, if one is taken, return other
        3) Find patterns and suggest best move to avoid capture
Assistance Received: none 
********************************************************************* */
string Player::find1(){
    int moveCount = m_board.getMoveCount();

    // If move count 1, suggested move is near the center stone
    if (moveCount == 1){
        return m_board.convertRowCol(10,9);
    }

    // If move count is 2, must place stone 3 intersections away
    // from center. Two options provided, if one is taken, return other
    if (moveCount == 2){
        if (m_board.getColorByPos(6,9) == EMPTY){ 
            return m_board.convertRowCol(6,9);
        }
        else {
            return m_board.convertRowCol(12,9);
        }
    }

    vector<string> patternLoc;
    char pat[5];
    pat[4] = 0;

    // Pattern _C*_ (star is returning valye)
    pat[1] = getColor();
    pat[0] = pat[3] = pat[2] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[2];
    }

    // Pattern _*C_ (star is returning valye)
    pat[2] = getColor();
    pat[1] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }

    // Pattern C*_C (star is returning valye)
    pat[0] = pat[3] = getColor();
    pat[2] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        return patternLoc[1];
    }
    
    // Pattern *_CO (star is returning valye)
    pat[3] = getOpponentColor();
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        // Avoid capture
        return patternLoc[0]; 
    }

    // Pattern OC_* (star is returning valye)
    pat[0] = getOpponentColor();
    pat[1] = getColor();
    pat[2] = pat[3] = EMPTY;
    patternLoc = m_board.findPattern(pat);
    if (!patternLoc.empty()){
        // Avoid capture
        return patternLoc[3]; 
    }

    return "";
}

/* ********************************************************************* 
Function Name: findAvailable
Purpose:  find first available move in the case there are no patterns
Parameters: none
Return Value: string of the move suggested and to be placed
Algorithm:
        1) Traverse entire board
        2) Return first empty spot
Assistance Received: none 
********************************************************************* */
string Player::findAvailable(){
    for (int i = 0; i < MAXROWCOL; i++){
        for (int j = 0; j < MAXROWCOL; j++){
            if (m_board.getColorByPos(i,j) == EMPTY){
                return m_board.convertRowCol(i,j);
            }
        }
    }
    return "";
}
