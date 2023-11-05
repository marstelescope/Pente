/*
************************************************************
* Name:  Mariya Popova                                     *
* Project:  1 Pente C++                                    *
* Class: CMPS 366-01 Organization of Programming Languages *
* Date:  September 27, 2023                                *
************************************************************
*/

#include "board.h"

/* ********************************************************************* 
Function Name: load
Purpose: load all the values from file inputed by user into corresponding
          object variables
Parameters: 
        a_file - filename inputed by user 
Return Value: true if file was loaded properly, false if errors were 
              encountered
Algorithm: 
        1) Read file using fstream top down according to expected syntax
        2) Assign values to object variables or report error when encountered
Assistance Received: none 
********************************************************************* */
bool Board::load(string a_file){
    // Opening file
    fstream fs;
    fs.open (a_file, fstream::in); 
    if (!fs){
        cout << "File not found." << endl;
        return false;
    }

    // Reading file line by line according to expected syntax
    string line;
    getline(fs, line);

    // Board heading not found
    if (line != "Board:"){
        cout << "Board heading expected." << endl;
        return false;
    }

    // Reading board values
    for (int i = 0; i < MAXROWCOL; i++){
        getline(fs, line);

        // Extra or missing characters in board line
        if (line.length() != MAXROWCOL){
            cout << "Board error." << endl;
            return false;
        }

        // Saving board line into double array, character by character
        for (int j = 0; j < MAXROWCOL; j++){
            char c = line[j];

            // Unexpected characters encountered, exit
            if (c != EMPTY && c != 'B' && c != 'W'){ 
                cout << "Invalid board piece." << endl;
                return false;
            }

            // Value from file saved to double array
            m_board[i][j] = c;
        }
    }

    // Empty line
    getline(fs, line); 

    // Checking for human header syntax
    getline(fs, line);
    if (line != "Human:"){
        cout << "Missing information." << endl;
        return false;
    }

    // Identifying captured pairs label 
    getline(fs, line);
    if (line.substr(0, 16) != "Captured pairs: "){
        cout << "Missing information." << endl;
        return false;
    }

    // Saving captured pairs amount into object variable
    m_humanCaptured = stoi(line.substr(16));

    // Checking validity of amount
    if (m_humanCaptured >= 5){
        cout << "Human captures exceed winning criteria." << endl;
        return false;
    }

    // Identifying score label
    getline(fs, line);
    if (line.substr(0, 7) != "Score: "){
        cout << "Missing score information." << endl;
        return false;
    }

    // Saving score amount into object variable
    m_humanScore = stoi(line.substr(7));

    // Empty line
    getline(fs, line);

    // Checking for computer header syntax
    getline(fs, line);
    if (line != "Computer:"){
        cout << "Missing information." << endl;
        return false;
    }
    
    // Identifying captured pairs label 
    getline(fs, line);
    if (line.substr(0, 16) != "Captured pairs: "){
        cout << "Missing information." << endl;
        return false;
    }

    // Checking validity of amount
    m_compCaptured = stoi(line.substr(16));
    if (m_compCaptured >= 5){
        cout << "Computer captures exceed winning criteria." << endl;
        return false;
    }

    // Identifying score label
    getline(fs, line);
    if (line.substr(0, 7) != "Score: "){
        cout << "Missing information." << endl;
        return false;
    }

    // Saving score amount into object variable
    m_compScore = stoi(line.substr(7));

    // Empty line
    getline(fs, line);

    // Identifying next player label
    getline(fs, line);
    if (line.substr(0,13) != "Next Player: "){
        cout << "Missing information" << endl;
        return false;
    }

    // Identifying next player by locating separator - syntax
    int separator = line.find(" - ");
    string player = line.substr(13, separator - 13);
    string color = line.substr(separator + 3);

    // Checking validity 
    if (player != "Human" && player != "Computer"){
        cout << "Player invalid." << endl;
        return false;
    }
    if (color != "White" && color != "Black"){
        cout << "Color invalid." << endl;
        return false;
    }

    // Saving values into object variables
    m_compNext = (player == "Computer");
    m_blackNext = (color == "Black");

    // Closing file
    fs.close();

    // Checking that there are no existing 5 in a rows in file, which
    // exceeds winning critera, in which case a new game must be started
    return (findPattern("BBBBB").empty() && findPattern("WWWWW").empty()); 
}

/* ********************************************************************* 
Function Name: display
Purpose: display board after computer turn for human view and convenience 
Parameters: none
Return Value: none
Algorithm: none
Assistance Received: none 
********************************************************************* */
void Board::display(){
    cout << "Board:" << endl; 

    for (int i = 0; i < MAXROWCOL; i++){
            cout << (MAXROWCOL - i) << " ";
            // For fomatting purposes
            if (i > 9){
                cout << " ";
            }
            // Display board row
            for (int j = 0; j < MAXROWCOL; j++){
                cout << m_board[i][j];
            }
            cout << endl;
        }
    cout << "   ABCDEFGHIJKLMNOPQRS" << endl << endl;
    cout << "Human:" << endl;
    cout << "Captured pairs: " << m_humanCaptured << endl;

    cout << "Computer:" << endl;
    cout << "Captured pairs: " << m_compCaptured << endl;
    cout << endl;
}

/* ********************************************************************* 
Function Name: save
Purpose: save current game state: board, captured pairs, scores, and next
         player and color to a file according to file syntax 
Parameters: 
        a_file - filename inputed by user 
Return Value: none
Algorithm: none
Assistance Received: none 
********************************************************************* */
void Board::save(string a_file){
    fstream fs;
    fs.open(a_file, fstream::out | fstream::trunc);
    fs << "Board:" << endl;
    for (int i = 0; i < MAXROWCOL; i++){
            for (int j = 0; j < MAXROWCOL; j++){
                fs << m_board[i][j];
            }
            fs << endl;
        }
    fs << endl;
    fs << "Human:" << endl;
    fs << "Captured pairs: " << m_humanCaptured << endl;
    fs << "Score: " << m_humanScore << endl << endl;

    fs << "Computer:" << endl;
    fs << "Captured pairs: " << m_compCaptured << endl;
    fs << "Score: " << m_compScore << endl << endl;

    fs << "Next Player: " << (m_compNext? "Computer":"Human") << " - " 
            << (m_blackNext? "Black":"White") << endl;
}

/* ********************************************************************* 
Function Name: boardInit
Purpose: clear or initialize values at the start of a new round
Parameters: none
Return Value: none
Algorithm: none
Assistance Received: none 
********************************************************************* */
void Board::boardInit(){
    m_humanCaptured = 0;
    m_humanScore = 0;
    m_compCaptured = 0;
    m_compScore = 0;
    m_fiveCount = 0;
    m_compNext = 0;
    m_blackNext = 0;
    m_compWon = false;
    m_humanWon = false;
    m_checkOnly = false;
    m_winning5.clear();
    for (int i = 0; i < MAXROWCOL; i++){
            for (int j = 0; j < MAXROWCOL; j++){
                m_board[i][j] = EMPTY;
            }
    }
}

/* ********************************************************************* 
Function Name: validateInput
Purpose: validate input of a move by checking length and character bounds
Parameters: 
        a_input - move inputed by user in string form
        a_column and a_row - passed by reference, changed if move is valid
Return Value: true if input is valid, false if not
Algorithm: none
Assistance Received: none 
********************************************************************* */
bool Board::validateInput(string a_input, char &a_column, int &a_row){

    // Check length of input - shouldn't be less than 2 or more than 3
    if (a_input.length() < 2 || a_input.length() > 3){
        return false;
    }

    // Check first character is between A and S, change a_column if so
    if (tolower(a_input[0]) < 'a' || tolower(a_input[0]) > 's'){
        return false;
    }
    a_column = a_input[0];

    // Check second character to be a number
    if (a_input[1] >= '0' && a_input[1] <= '9'){

        // Convert second and third character to integer, changing a_row
        a_row = stoi(a_input.substr(1,2));

        // Check the conversion is between 1 and 19
        if (a_row > 0 && a_row < (MAXROWCOL+1)){
            return true;
        }
    }
    return false;
}

/* ********************************************************************* 
Function Name: makeMove
Purpose: buffer function to call for validation of move in string form before 
        converting move to row and column form to pass to makeMove with 3 parameters
Parameters: 
        a_color - color of player
        a_move - move inputed
Return Value: true if move is valid, false if not
Algorithm: none
Assistance Received: none 
********************************************************************* */
bool Board::makeMove(char a_color, string a_move){
    char column;
    int row;

    // If input is valid, a_move is converted to column and row form
    if (!validateInput(a_move, column, row)){
        return false;
    }
    int columnIndex = tolower(column) - 'a';
    int rowIndex = MAXROWCOL - row;

    return makeMove(a_color, rowIndex, columnIndex);
}

/* ********************************************************************* 
Function Name: makeMove
Purpose: check that row and column are empty on the board for the move
         to be placed, remind first player of restriction on their second turn
         if it is violated
Parameters: 
        a_color - color of player
        a_row - row of move
        a_column - column of move
Return Value: true if move is valid, false if not
Algorithm: none
Assistance Received: none 
********************************************************************* */
bool Board::makeMove(char a_color, int a_row, int a_column){
    // Check that row and column is empty for stone to be placed
    if (m_board[a_row][a_column] == EMPTY){
        if (m_moveCount == 2){
            if (a_row > 6 && a_row < 12 &&
                a_column > 6 && a_column < 12){
                // Remind restriction 
                cout << "On the second turn, the first player must place another white stone"  
                    << " at least 3 intersections away from the first white stone." << endl;
                return false;
            }
        }

        // Place stone
        m_board[a_row][a_column] = a_color;

        // Check if this placement results in a capture or 5 in a row
        checkCapture(a_row, a_column);
        check5inRow(a_row, a_column);

        // Update next player, next color, and move count
        m_blackNext = !m_blackNext;
        m_compNext = !m_compNext;
        m_moveCount++;

        return true;
    }

    return false;
}

/* ********************************************************************* 
Function Name: checkCapture
Purpose: check if the current stone placement results in a capture and increment 
         capture score for current player if so
Parameters: 
        a_row - row of stone placement
        a_column - column of stone placement
Return Value: none
Algorithm: 
        1) Eight if statements for all directions to check if current placement 
        captures opponent's stones
        2) If captured, and it isn't a checkOnly call from player strategy,
        opponent's stone locations change to empty and player's
        score is incremented
Assistance Received: none 
********************************************************************* */
void Board::checkCapture(int a_row, int a_column){
    // Storing current player's color and opponent's color
    char color = m_board[a_row][a_column];
    char opponentColor = (color == 'W'? 'B' : 'W');

    // Checking up the column
    if (a_row > 2 && m_board[a_row-1][a_column] == opponentColor &&
                     m_board[a_row-2][a_column] == opponentColor &&
                     m_board[a_row-3][a_column] == color){
        if (!m_checkOnly){
            m_board[a_row-1][a_column] = EMPTY;
            m_board[a_row-2][a_column] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Checking down the column
    if (a_row < 16 && m_board[a_row+1][a_column] == opponentColor &&
                     m_board[a_row+2][a_column] == opponentColor &&
                     m_board[a_row+3][a_column] == color){
        if  (!m_checkOnly){
            m_board[a_row+1][a_column] = EMPTY;
            m_board[a_row+2][a_column] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Checking left in row
    if (a_column > 2 && m_board[a_row][a_column-1] == opponentColor &&
                     m_board[a_row][a_column-2] == opponentColor &&
                     m_board[a_row][a_column-3] == color){
        if (!m_checkOnly){
            m_board[a_row][a_column-1] = EMPTY;
            m_board[a_row][a_column-2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Checking right in row
    if (a_column < 16 && m_board[a_row][a_column+1] == opponentColor &&
                     m_board[a_row][a_column+2] == opponentColor &&
                     m_board[a_row][a_column+3] == color){
        if (!m_checkOnly){
            m_board[a_row][a_column+1] = EMPTY;
            m_board[a_row][a_column+2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Moving down right from up left
    if (a_column < 16 && a_row < 16 && m_board[a_row+1][a_column+1] == opponentColor &&
                     m_board[a_row+2][a_column+2] == opponentColor &&
                     m_board[a_row+3][a_column+3] == color){
        if (!m_checkOnly){            
            m_board[a_row+1][a_column+1] = EMPTY;
            m_board[a_row+2][a_column+2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Moving up left from down right
    if (a_column > 2 && a_row > 2 && m_board[a_row-1][a_column-1] == opponentColor &&
                     m_board[a_row-2][a_column-2] == opponentColor &&
                     m_board[a_row-3][a_column-3] == color){
        if (!m_checkOnly){  
            m_board[a_row-1][a_column-1] = EMPTY;
            m_board[a_row-2][a_column-2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Moving down left from up right
    if (a_column > 2 && a_row < 16 && m_board[a_row+1][a_column-1] == opponentColor &&
                     m_board[a_row+2][a_column-2] == opponentColor &&
                     m_board[a_row+3][a_column-3] == color){
        if (!m_checkOnly){  
            m_board[a_row+1][a_column-1] = EMPTY;
            m_board[a_row+2][a_column-2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
    // Moving up right from down left
    if (a_column < 16 && a_row > 2 && m_board[a_row-1][a_column+1] == opponentColor &&
                     m_board[a_row-2][a_column+2] == opponentColor &&
                     m_board[a_row-3][a_column+3] == color){
        if (!m_checkOnly){                
            m_board[a_row-1][a_column+1] = EMPTY;
            m_board[a_row-2][a_column+2] = EMPTY;
        }
        m_compNext ? m_compCaptured++ : m_humanCaptured ++;
    }
}

/* ********************************************************************* 
Function Name: helper5Count
Purpose: checks if stone placement created a chain of 5 or more stones, checking 
         in one orientation at a time (up and down, left and right, UL to DR 
         diagonal, DL to UR diagonal)
Parameters: 
        a_row - row of stone placement
        a_column - column of stone placement
        a_incRow - row increment, direction being checked
        a_incCol - column increment, direction being checked
Return Value: true if 5+ in a row is present, false if not
Algorithm: 
        1) Check in one direction, increasing row by a_incRow and col by a_incCol,
        updating count while player's stone color encountered
        2) Check in the other direction, decreasing row by a_incRow and col by
        a_incCol, updating count while player's stone color encountered
        3) If count is updated, pushback row and col to local winning5
        4) If final count is >=5, pushback winning5 to m_winning5. This aids in 
        computing final score and ensuring 4 in a rows aren't counted from a winning 5 set
Assistance Received: none 
********************************************************************* */
bool Board::helper5Count(int a_row, int a_column, int a_incRow, int a_incCol){
    // Setting count to -1 as it will be incremented twice 
    // in while loops for the starting position
    int count = -1;
    vector<pair<int,int>> winning5;

    char color = m_board[a_row][a_column];
    int row = a_row;
    int col = a_column;

    // Checking stones in one direction
    while (row >= 0 && col >= 0 && col < MAXROWCOL && row < MAXROWCOL && m_board[row][col] == color){
        // Start point excluded here to avoid overlap in the next while loop
        if (!(a_row == row && a_column == col)){   
            winning5.push_back(make_pair(row, col));
        }
        row += a_incRow;
        col += a_incCol;
        count++;
    }

    // Reseting to start point 
    row = a_row;
    col = a_column;

    // Checking stones in the other direction
    while (row >=0 && col >=0 && col <MAXROWCOL && row < MAXROWCOL && m_board[row][col] == color){
        winning5.push_back(make_pair(row, col));
        row -= a_incRow;
        col -= a_incCol;
        count++;
    }

    // To avoid overlap in scoring of 4 in a rows with winning 5 in a rows
    if (count >= 5){
        m_winning5.push_back(winning5);
    }

    // 5 in a row found!
    return (count >= 5);
}

/* ********************************************************************* 
Function Name: count5inRow
Purpose: count how many 5 in a rows the stone placement resulted in
Parameters: 
        a_row - row of stone placement
        a_column - column of stone placement
Return Value: number of 5 in a rows achieved by current player
Algorithm: none
Assistance Received: none 
********************************************************************* */
int Board::count5inRow(int a_row, int a_column){
    int fiveInRow = 0;
    if (helper5Count(a_row, a_column, 0, 1)) {fiveInRow++;}
    if (helper5Count(a_row, a_column, 1, 0)) {fiveInRow++;}
    if (helper5Count(a_row, a_column, 1, 1)) {fiveInRow++;}
    if (helper5Count(a_row, a_column, 1, -1)) {fiveInRow++;}
    return fiveInRow;
}

/* ********************************************************************* 
Function Name: check5inRow
Purpose: check if count5inRow returns a value greater than 0,
          and if so, update m_compWon or m_humanWon based on 
          whose turn it is currently, and store the winner's count
          of 5 in a rows in m_fiveCount
Parameters: 
        a_row - row of stone placement
        a_column - column of stone placement
Return Value: none
Algorithm: none
Assistance Received: none 
********************************************************************* */
void Board::check5inRow(int a_row, int a_column){
    int fiveInRow = count5inRow(a_row, a_column);

    // If 5 in a row encountered, update m_compWon or m_humanWon 
    // based on whose turn it is currently
    if (fiveInRow > 0){
        if (m_compNext){
            m_compWon = true;
        } 
        else {
            m_humanWon = true;
        }
    }
    m_fiveCount = fiveInRow;
}

/* ********************************************************************* 
Function Name: overlap
Purpose: check if the current set of 4 in a row stones overlaps with the
          winning 5 in a row set(s)
Parameters: 
        a_current4 - set of 4 in a row we are checking, passed by reference,
                    not modified
Return Value: true if overlap exists, false otherwise
Algorithm: none
Assistance Received: none 
********************************************************************* */
bool Board::overlap(set<pair<int, int>>& a_current4){
    int count = 0;
    
    // Check each winning5 vector in m_winning5 vector of vectors
    for (auto winning5 : m_winning5){
        // Check each spot in the vector against those in a_current4
        for (auto a : winning5){
            if (a_current4.find(a) != a_current4.end()){
                count++;
            }
            if (count > 1){
                return true;
            }
        }
    }
    return false;
}

/* ********************************************************************* 
Function Name: helper4Count
Purpose: count number of 4 in a rows, checking one orientation at a time 
        (up and down, left and right, UL to DR diagonal, DL to UR diagonal)
Parameters: 
        a_color - color being checked
        a_row - starting row
        a_column - starting column
        a_incRow - row increment, direction being checked
        a_incCol - column increment, direction being checked
Return Value: number of 4 in a rows in given orientation 
Algorithm:    
        1) Start at a_row and a_column and while they are within board bounds,
        tranverse board by changing a_row and a_column by a_incRow and a_incCol
        2) At each row, col position, check if a_color is stored there. If so, insert
        into current4. If not, if count > 0, restore count and clear current4
        3) If count reaches 4, compare current4 to winning set. If there is no overlap
        (current4 isn't a subset of winning 5), then count of 4s increases
        4) Return total number of 4 in a rows
Assistance Received: none 
********************************************************************* */
int Board::helper4Count(char a_color, int a_row, int a_column, int a_incRow, int a_incCol){  
    int count = 0;
    int count4 = 0;
    set<pair<int, int>> current4;

    while (a_row >= 0 && a_column >= 0 && a_column < MAXROWCOL && a_row < MAXROWCOL){
        // Color needed not encountered
        if (m_board[a_row][a_column] != a_color){
            if (count > 0){
                count = 0;
                current4.clear();
            }
        }
        // Potential for 4 in a row 
        else {
            count ++;
            current4.insert(make_pair(a_row, a_column)); 
            if (count == 4){
                // Compare set to winning 5
                if (!overlap(current4)){
                    count4++;
                }
                count = 0;
                current4.clear();
            }
        }
        a_row += a_incRow;
        a_column += a_incCol;
    }

    // Returns how many counts of 4 in given row, column or diagonal
    return count4;
}

/* ********************************************************************* 
Function Name: count4row
Purpose:  count total number of 4 in a rows for given color/player
Parameters: 
        a_color - color being checked
Return Value: total number of 4 in a rows
Algorithm:    
        1) Call helper4count for each orientation, add result to local count
        2) Return local count
Assistance Received: none 
********************************************************************* */
int Board::count4inRow(char a_color){
    int count = 0;
    for (int i = 0; i < MAXROWCOL; i++){ 
        // Moving right
        count += helper4Count(a_color, i, 0, 0, 1); 
        // Moving down
        count += helper4Count(a_color, 0, i, 1, 0);

        // Moving diag upper right to down left (upper half)
        if (i != 18){
            count += helper4Count(a_color, 0, i, 1, -1); 
        }
        // Moving diag upper right to down left (lower half)
        count += helper4Count(a_color, i, 18, 1, -1); 
        
        // Moving diag upper left to down right (upper half)
        if (i != 0){
            count += helper4Count(a_color, 0, i, 1, 1); 
        }
        // Moving diag upper left to down right (lower half)
        count += helper4Count(a_color, i, 0, 1, 1); 
    }
    return count;
}

/* ********************************************************************* 
Function Name: findPattern
Purpose:  find the pattern passed and return vector of pattern's location
Parameters: 
        a_pattern - pattern being checked, passed by reference, not altered
Return Value: vector stores location of the pattern
Algorithm:    
        1) Call patternHelper for each orientation on the board, one at a time
        2) Store what it returns in local vector pattern
        3) If pattern is not empty, return pattern
        4) Repeat until pattern found or when orientations exhausted, return
        empty pattern
Assistance Received: none 
********************************************************************* */
vector<string> Board::findPattern(const string& a_pattern){
    vector<string> pattern;
    for (int i = 0; i < MAXROWCOL; i++){ 
        // Checking right
        pattern = patternHelper(a_pattern, i, 0, 0, 1);
        if (!pattern.empty()){ return pattern; }
        // Checking down
        pattern = patternHelper(a_pattern, 0, i, 1, 0);
        if (!pattern.empty()){ return pattern; }

        // Checking diag upper right to down left (upper half)
        if (i != 18){
            pattern = patternHelper(a_pattern, 0, i, 1, -1);
            if (!pattern.empty()){ return pattern; }
        }
        // Checking diag upper right to down left (lower half)
        pattern = patternHelper(a_pattern, i, 18, 1, -1); 
        if (!pattern.empty()){ return pattern; }
        
        // Checking diag upper left to down right (upper half)
        if (i != 0){
            pattern = patternHelper(a_pattern, 0, i, 1, 1); 
            if (!pattern.empty()){ return pattern; }
        }
        // Checking diag upper left to down right (lower half)
        pattern = patternHelper(a_pattern, i, 0, 1, 1);
        if (!pattern.empty()){ return pattern; }
    }
    return pattern;
}

/* ********************************************************************* 
Function Name: patternHelper
Purpose: helper function to find a_pattern passed, one board orientation
            at a time.
Parameters: 
        a_pattern - pattern being checked, passed by reference, not altered
        a_row - starting row
        a_column - starting column
        a_incRow - row increment, direction being checked
        a_incCol - column increment, direction being checked
Return Value: vector location of the pattern 
Algorithm:    
        1) Start at a_row and a_column and while they are within board bounds,
        tranverse board by changing a_row and a_column by a_incRow and a_incCol
        2) At each row col position, check if position color corresponds
        with what is expected in the pattern
        3) If it does, pushback to local pattern the current position
        4) Otherwise, reset count, clear vector, and use restart positions,
        which will be incremented +1 from where the current pattern started.
        This is necessary to not skip over any position that reset the pattern 
        but are part of the next pattern right away
        5) Pattern locations are returned if pattern is found, or empty vector
        is returned
Assistance Received: none 
********************************************************************* */
vector<string> Board::patternHelper(const string& a_pattern, int a_row, 
                                        int a_column, int a_incRow, int a_incCol){
     
    vector<string> pattern;                                       
    int row = a_row;
    int col = a_column;
    int count = 0;
    int restartRow = 0;
    int restartCol = 0;

    // Tranverse board within bounds
    while (row >= 0 && col >= 0 && col < MAXROWCOL && row < MAXROWCOL){
        // Check if intersection color corresponds with what is expected in the pattern
        if (m_board[row][col] != a_pattern[count]){
            if (count > 0){
                count = 0;
                // Restarting from current pattern beginning (later incremented)
                row = restartRow;
                col = restartCol;
                pattern.clear();
            }
        }
        // Corresponds to pattern
        else {
            if (count == 0){
                restartRow = row;
                restartCol = col;
            }
            count++;
            pattern.push_back(convertRowCol(row, col));

            // Pattern found
            if (count == a_pattern.length()){
                return pattern;
            }
        }

        // Increment to next position
        row += a_incRow;
        col += a_incCol;
    }

    // Pattern not found
    pattern.clear();
    return pattern;
}

/* ********************************************************************* 
Function Name: gameOver
Purpose: keep track of when game comes to end: full board, 5 in a row,
           or 5 or more captures
Parameters: none
Return Value: true if game is over, false otherwise
Algorithm:    
        1) Check if board is full, then gameOver is true
        2) Check if m_compWon is true, then announce computer as the winner,
        and add to score its m_fiveCount * 5 (5 points per 5)
        3) Step 2 repeated for human player
        4) If neither computer nor human won and gameOver is true, board 
        must be full, it's a draw
        5) Check computer and human captures exceeding or equaling 5,
        announce winner
        6) If game is over, add up total scores for the round, return true
Assistance Received: none 
********************************************************************* */
bool Board::gameOver(){
    cout << endl;
    bool gameOver = (countNonEmpty() == MAXROWCOL*MAXROWCOL);
    cout << endl;

    // Determine who won
    if (m_compWon){
        cout << "Computer won by 5 in a row!" << endl;
        m_compScore += m_fiveCount * 5;
        gameOver = true;
    }
    else if (m_humanWon){
        cout << "Human won by 5 in a row!" << endl;
        m_humanScore += m_fiveCount * 5;
        gameOver = true;
    }
    else if (gameOver){
        cout << "It's a draw." << endl;
        gameOver = true;
    }
    if (m_compCaptured >= 5) {
        cout << "Computer won by 5 captures!" << endl;
        gameOver = true;
    }
    if (m_humanCaptured >= 5){
        cout << "Human won by 5 captures!" << endl;
        gameOver = true;
    }

    // Add up total scores for round
    if (gameOver){
        m_humanScore += m_humanCaptured;
        m_compScore += m_compCaptured;
        m_humanScore += count4inRow(getHumanColor());
        m_compScore += count4inRow(getCompColor());
    }

    return gameOver;
}
