% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: board_init
% Purpose: initialize board in list of lists form
% Parameters: 
%        count - number of rows (call with 19 for Pente)
%        board - out variable, initialized board
% Algorithm: add to list of lists, row after row
% Assistance Received: none 
% ***********************************************************************
board_init(0, []).
board_init(N, [ ["o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o", "o"] | Rest]) :-
    N > 0,
    NewN is N - 1,
    board_init(NewN, Rest).

% ********************************************************************* 
% Function Name: play
% Purpose: carry out the play of a turn
% Parameters: 
%        board - current board state 
%        human - human capture count and round score
%        comp - computer capture count and round score
%        nextplayer - whose turn it is currently and their stone color
%        outcome - out variable, list of board, human, comp, nextplayer, and exit case
% Algorithm:       
%       1. Check if board is full. Declare a draw and return list values.
%       2. Current player identified, appropriate function to make a move called.
% Assistance Received: none 
% ***********************************************************************
play(Board, Human, Comp, [Player, Color], Outcome) :-
    board_print(Board),
    capture_print(Human, Comp),

    ( get_move_count(Board, 0, 0, 0, 361) ->
        Outcome = [Board, Human, Comp, [Player, Color], 'over'],
        format("Game over! Full board. It's a draw!~n")
    ;
        (Player = "human" ->
            human_move(Board, Human, Comp, [Player, Color], Outcome)
        ;
            computer_move(Board, Human, Comp, [Player, Color], Outcome)
        )
    ).

% ********************************************************************* 
% Function Name: board_print
% Purpose: display board with labels
% Parameters: 
%        board - current board state 
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
board_print([]) :-
    format("    A B C D E F G H I J K L M N O P Q R S~n").
board_print([First | Rest]) :-
    length([First | Rest], Length),
    (   Length < 10 
    ->  format("~w  ", [Length])
    ; 
        format("~w ", [Length])
    ),
    format("~w~n", [First]),
    board_print(Rest).

% ********************************************************************* 
% Function Name: capture_print
% Purpose: display capture amount for human and computer
% Parameters: 
%        human - list of human captures and score
%        comp - list of computer captures and score
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
capture_print([FirstH | _],[FirstC | _]):-
    format("Human captures: ~w~n", [FirstH]),
    format("Computer captures: ~w~n", [FirstC]).

% ********************************************************************* 
% Function Name: get_move_count
% Purpose: get number of moves that have been placed on the board
% Parameters: 
%        board - current board state 
%        row - row being checked
%        column - column being checked
%        count - move count
%        finalcount - out variable, final move count when board fully traversed
% Algorithm: traverse board; if space isnt empty, increase count.
% Assistance Received: none 
% ***********************************************************************
get_move_count(_, 19, _, Count, Count).
get_move_count(Board, Row, 19, Count, FinalCount) :-
    NextRow is Row + 1,
    get_move_count(Board, NextRow, 0, Count, FinalCount).
get_move_count(Board, Row, Column, Count, FinalCount) :-
    get_element_by_pos(Board, [Row, Column], Element),
    (   Element \= "o"   % arrow needed here
    ->  CountUp is Count + 1,
        NextColumn is Column + 1,
        get_move_count(Board, Row, NextColumn, CountUp, FinalCount)
    ;   
        NextColumn is Column + 1,
        get_move_count(Board, Row, NextColumn, Count, FinalCount)
    ).

% ********************************************************************* 
% Function Name: get_element_by_pos
% Purpose: get element that is in a board positions: "o", "b", "w"
% Parameters: 
%        board - current board state 
%        position - list of row and column
%        element - out variable, the stone color in that position
% Algorithm: none
% Assistance Received: https://www.swi-prolog.org/pldoc/man?predicate=nth0/3
% ***********************************************************************
get_element_by_pos(Board, [Row, Column], Element) :-
    nth0(Row, Board, PositionRow),
    nth0(Column, PositionRow, Element).

% ********************************************************************* 
% Function Name: human_move
% Purpose: prompt user for move until it is valid, then process it
% Parameters: 
%        board - current board state 
%        human - current human stats
%        comp - current computer stats
%        nextplayer - next player and stone color
%        outcome - out variable, list of board, human, comp, nextplayer, and exit case
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
human_move(Board, Human, Comp, [Player, Color], Outcome) :-
    make_move(Board, Color, _, Result),
    (   Result = [] 
    ->  Outcome = [Board, Human, Comp, [Player, Color], 'quit']
    ;
        Result = "invalid" 
    ->  human_move(Board, Human, Comp, [Player, Color], Outcome) 
    ;
        process_move(Result, Human, Comp, [Player, Color], Outcome)
    ).

% ********************************************************************* 
% Function Name: computer_move
% Purpose: simulate computer move, get move from strategy, place it, and process it
% Parameters: 
%        board - current board state 
%        human - current human stats
%        comp - current computer stats
%        nextplayer - next player and stone color
%        outcome - out variable, list of board, human, comp, nextplayer, and exit case
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
computer_move(Board, Human, Comp, [Player, Color], Outcome) :-
    format("~nComputer turn~n"),
    sub_string(Color, 0, 1, _, Clr),
    strategy(Board, Clr, [Row, Column]),
    row_col_to_move_name(Row, Column, Name),
    format("The computer placed a ~w stone on ~w~n", [Color, Name]),
    place_move(Board, Color, [Row, Column], MoveResult),
    process_move(MoveResult, Human, Comp, [Player, Color], Outcome).

% ********************************************************************* 
% Function Name: make_move
% Purpose: prompt player to make a move, check if input returns to be valid,
%          check if board space is empty, place move if so
% Parameters: 
%        board - current board state 
%        color - color of current player
%        move - move inputed by user within function
%        outcome - out variable, empty if user quits, invalid if move invalid, 
%                                uninstantiated otherwise (proceed to place move with outcome)
% Algorithm:       
%       1. Prompt user to make a move.
%       2. Call to validate the make_move_input
% Assistance Received: none 
% ***********************************************************************
make_move(Board, Color, Move, Outcome) :-
    format("Enter intersection to place "),
    format("~w", Color),
    format(" stone: ~n"),
    read_line_to_string(user_input, Move),
    make_move_input(Board, Color, Move, Outcome).

% ********************************************************************* 
% Function Name: make_move_input
% Purpose: validate make move input and act accordingly
% Parameters: 
%        board - current board state 
%        color - color of current player
%        move - move inputed by user
%        outcome - out variable, empty if user quits, invalid if move invalid, 
%                                uninstantiated otherwise (proceed to place move with outcome)
% Algorithm:       
%       1. Check if quit or help was entered, respond to request accordingly.
%       2. Call move_to_row_col, which will report errors when encountered. If 
%          errors encountered, row and col will be -1, not within bounds, and outcome   
%          will be invalid.
%       3. Continue to make_move_constraints if move input is valid.
% Assistance Received: none 
% ***********************************************************************
make_move_input(_, _, Move, []) :-
    string_upper(Move, "QUIT"),
    writeln('Exiting...').

make_move_input(Board, Color, Move, "invalid") :-
    string_upper(Move, "HELP"),
    sub_string(Color, 0, 1, _, Clr),
    strategy(Board, Clr, _).

make_move_input(Board, Color, Move, Outcome) :-
    move_to_row_col(Move, [Row, Column]),
    within_bounds(Row, Column),
    make_move_constraints(Board, Color, Row, Column, Outcome).
make_move_input(_, _, _, "invalid").

% ********************************************************************* 
% Function Name: move_to_row_col
% Purpose: checks syntax of passed move, returns row and column if valid
% Parameters: 
%        move - move to be converted to row and column format
%        result - out variable, row and column if syntax correct, [-1, -1] otherwise
% Algorithm:       
%       1. Check first character to be between A and S.
%       2. Check the second and third values (if any) to be a number 1 - 19.
%       3. If values valid, convert move to row and column and return them in a list.
% Assistance Received: none 
% ***********************************************************************
move_to_row_col(Move, Result) :-
    string_upper(Move, UppercaseMove),
    sub_string(UppercaseMove, 0, 1, _, FirstChar),
    get_column(FirstChar, Column),
    get_row(Move, Row),
    valid_row_col(Row, Column, Val),
    (   Val = 1 
    ->  R is 19 - Row,
        C is Column,
        Result = [R, C]
    ;
        Result = [-1, -1]
    ).

% ********************************************************************* 
% Function Name: within_bounds
% Purpose: check that row and col are within board bounds
% Parameters: 
%        row - row being checked
%        column - column being checked
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
within_bounds(Row, Column) :-
    Row >= 0,
    Column >= 0,
    Row < 19,
    Column < 19.

% ********************************************************************* 
% Function Name: get_column
% Purpose: validate column entered and return its index
% Parameters: 
%        firstchar - firstchar of move input
%        column - out variable, column index if valid, -1 otherwise
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_column(FirstChar, Column):-
    string_chars(FirstChar, [Letter | _]),
    char_code(Letter, Code),
    valid_first_char(Code, Column).

% ********************************************************************* 
% Function Name: valid_first_char
% Purpose: convert column char to index if valid
% Parameters: 
%        code - first char code
%        result - out variable, row index if valid, -1 otherwise
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
valid_first_char(Code, Result) :-
    Code >= 65,
    Code =< 83,
    Result is Code - 65.

valid_first_char(Code, -1) :-
    (Code < 65 ; Code > 83),
    format("First character must be between A and S.~n").

% ********************************************************************* 
% Function Name: get_row
% Purpose: validate row entered and return its index
% Parameters: 
%        move - move input
%        row - out variable, row index if valid, -1 otherwise
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_row(Move, Row):-
    get_number(Move, Number),
    valid_number(Number, Row).

% ********************************************************************* 
% Function Name: get_number
% Purpose: extract number from move input based on move length, ensure number is
%           comprised of digit values
% Parameters: 
%        move - move input
%        number - out variable, extracted number based on move length
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_number(Move, Number):-
    string_length(Move, 2),
    (   sub_string(Move, 1, 1, _, NumChar),
        char_type(NumChar, digit) 
    ->  number_string(Number, NumChar)
    ; 
        format("Number expected after one character.~n"),
        Number = -1
    ).
get_number(Move, Number):-
    string_length(Move, 3),
    (   sub_string(Move, 1, 1, _, DigitOne),
        sub_string(Move, 2, 1, _, DigitTwo),
        char_type(DigitOne, digit),
        char_type(DigitTwo, digit) 
    ->  sub_string(Move, 1, 2, _, NumChar),
        number_string(Number, NumChar)
    ;
        format("Number expected after one character.~n"),
        Number = -1
    ).
get_number(Move, -1):-
    string_length(Move, X),
    (X < 2 ; X > 3).

% ********************************************************************* 
% Function Name: valid_number
% Purpose: convert column char to index if valid
% Parameters: 
%        number - number extracted from move input
%        result - out variable, row index if valid, -1 otherwise
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
valid_number(Number, ValidNumber) :-
    Number > 0,
    Number < 20,
    ValidNumber = Number.

valid_number(Number, -1) :-
    (Number < 1; Number > 19),
    format("Number expected between 1 and 19.~n").

valid_row_col(Row, Col, 1):-
    Row \= -1,
    Col \= -1.

% ********************************************************************* 
% Function Name: make_move_constraints
% Purpose: enforce contraints based on move count or if board space is taken
% Parameters: 
%        board - current board state
%        color - color of stone being placed
%        row - row where move is to be placed 
%        column - column where move is to be placed 
%        outcome - out variable, empty if user quits, invalid if move invalid, 
%                                uninstantiated otherwise (proceed to place move with outcome)
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
make_move_constraints(Board, _, Row, Column, "invalid") :-
    get_move_count(Board, 0, 0, 0, 0),
    row_col_to_move_name(Row, Column, Move),
    \+ string_upper(Move, "J10"),
    writeln('First move must be J10.').

make_move_constraints(Board, _, Row, Column, "invalid") :-
    get_move_count(Board, 0, 0, 0, 2),
    Row > 6, Row < 12, Column > 6, Column < 12,
    writeln('On the first player\'s second turn, move must be at least 3 intersections away from the center stone.').

make_move_constraints(Board, Color, Row, Column, Outcome) :-
    get_element_by_pos(Board, [Row, Column], "o"),
    place_move(Board, Color, [Row, Column], Outcome).

make_move_constraints(_, _, _, _, "invalid") :-
    writeln('Board space taken.').

% ********************************************************************* 
% Function Name: row_col_to_move_name
% Purpose: return move name for row and column 
% Parameters: 
%        row - row being converted
%        column - column being converted
%        move - out variable, move name for that row and column
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
row_col_to_move_name(Row, Column, Move) :-
    CharCode is Column + 65,
    char_code(Char, CharCode),
    RowNumber is 19 - Row,
    atom_string(Char, CharString),
    number_string(RowNumber, RowString),
    atomic_list_concat([CharString, RowString], Move).

% ********************************************************************* 
% Function Name: get_next_player
% Purpose: gets next player and stone color in list form
% Parameters: 
%        currentplayer - list of player and stone color
%        nextplayer - out variable, list of next player and stone color
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_next_player(["human", "white"], ["computer", "black"]).
get_next_player(["human", "black"], ["computer", "white"]).
get_next_player(["computer", "white"], ["human", "black"]).
get_next_player(["computer", "black"], ["human", "white"]).

% ********************************************************************* 
% Function Name: place_move
% Purpose: place stone, return updated board and any increments to score/captures
% Parameters: 
%        board - board state before move placed
%        color - color being placed
%        position - where it is being placed
%        result - out variable, board after move is placed, with score inc and 
%                               cap inc the move resulted in, if any!
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
place_move(Board, Color, [Row, Column], Result) :-
    sub_string(Color, 0, 1, _, Clr),
    board_after_move(Board, Row, Column, Clr, NewBoard),
    check_capture(NewBoard, Clr, Row, Column, 0, [AfterCapBoard, CapCount]),
    check_5_in_a_row(NewBoard, Row, Column, Clr, Score5),
   (   (Score5 > 0; CapCount > 0) ->   
        Result = [AfterCapBoard, Score5, CapCount] 
    ;
        Result = AfterCapBoard
    ).

% ********************************************************************* 
% Function Name: board_after_move
% Purpose: update board after a move is placed
% Parameters: 
%        board - current board state 
%        row - row of move placement
%        column - column of move placement
%        color - stone color being placed
%        updatedboard - out variable, board after move
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
board_after_move([Row|Rest], 0, Col, Color, [UpdatedRow|Rest]) :-
    row_after_move(Row, Col, Color, UpdatedRow).
board_after_move([First|Rest], Row, Col, Color, [First|UpdatedRest]) :-
    Row > 0,
    NextRow is Row - 1,
    board_after_move(Rest, NextRow, Col, Color, UpdatedRest).

% ********************************************************************* 
% Function Name: row_after_move
% Purpose: update row after a move is placed
% Parameters: 
%        row - row of move placement
%        column - column of move placement
%        color - stone color being placed
%        updatedrow - out variable, row after move
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
row_after_move([_|Rest], 0, Color, [Color|Rest]).
row_after_move([First|Rest], Col, Color, [First|UpdatedRest]) :-
    Col > 0,
    NextCol is Col - 1,
    row_after_move(Rest, NextCol, Color, UpdatedRest).

% ********************************************************************* 
% Function Name: check_capture
% Purpose: check if a move resulted in captures, update board and count score increase
% Parameters: 
%        board - board state (being updated in function if there are captures)
%        color - color of stone placed
%        row - row of move that is being checked if it resulted in captures
%        column - column of move that is being checked if it resulted in captures
%        score - capture score for this move, being updated
%        result - out variable, list of board after all captures and total score increase
% Algorithm: 
%         1. Check each direction, while within board bounds, if capture pattern exists.
%         2. If so, remove captured stones, and pass updated board and score back to 
%            function until all directions exhausted.
%         3. When no more captures are found, return updated board and score.
% Assistance Received: none 
% ***********************************************************************
check_capture(Board, Color, Row, Column, Score, Result):-
    Row > 2, 
    get_opponent_color(Color, OpponentColor),
    RowMin1 is Row - 1,
    RowMin2 is Row - 2,
    RowMin3 is Row - 3,
    get_element_by_pos(Board, [RowMin1, Column], OpponentColor),
    get_element_by_pos(Board, [RowMin2, Column], OpponentColor),
    get_element_by_pos(Board, [RowMin3, Column], Color),
    remove_captures(Board, RowMin1, Column, RemoveCap1),
    remove_captures(RemoveCap1, RowMin2, Column, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Row < 16, 
    get_opponent_color(Color, OpponentColor),
    RowPlus1 is Row + 1,
    RowPlus2 is Row + 2,
    RowPlus3 is Row + 3,
    get_element_by_pos(Board, [RowPlus1, Column], OpponentColor),
    get_element_by_pos(Board, [RowPlus2, Column], OpponentColor),
    get_element_by_pos(Board, [RowPlus3, Column], Color),
    remove_captures(Board, RowPlus1, Column, RemoveCap1),
    remove_captures(RemoveCap1, RowPlus2, Column, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Column > 2, 
    get_opponent_color(Color, OpponentColor),
    ColMin1 is Column - 1,
    ColMin2 is Column - 2,
    ColMin3 is Column - 3,
    get_element_by_pos(Board, [Row, ColMin1], OpponentColor),
    get_element_by_pos(Board, [Row, ColMin2], OpponentColor),
    get_element_by_pos(Board, [Row, ColMin3], Color),
    remove_captures(Board, Row, ColMin1, RemoveCap1),
    remove_captures(RemoveCap1, Row, ColMin2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Column < 16, 
    get_opponent_color(Color, OpponentColor),
    ColPlus1 is Column + 1,
    ColPlus2 is Column + 2,
    ColPlus3 is Column + 3,
    get_element_by_pos(Board, [Row, ColPlus1], OpponentColor),
    get_element_by_pos(Board, [Row, ColPlus2], OpponentColor),
    get_element_by_pos(Board, [Row, ColPlus3], Color),
    remove_captures(Board, Row, ColPlus1, RemoveCap1),
    remove_captures(RemoveCap1, Row, ColPlus2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Row > 2, 
    Column > 2,
    get_opponent_color(Color, OpponentColor),
    ColMin1 is Column - 1,
    ColMin2 is Column - 2,
    ColMin3 is Column - 3,
    RowMin1 is Row - 1,
    RowMin2 is Row - 2,
    RowMin3 is Row - 3, 
    get_element_by_pos(Board, [RowMin1, ColMin1], OpponentColor),
    get_element_by_pos(Board, [RowMin2, ColMin2], OpponentColor),
    get_element_by_pos(Board, [RowMin3, ColMin3], Color),
    remove_captures(Board, RowMin2, ColMin1, RemoveCap1),
    remove_captures(RemoveCap1, RowMin2, ColMin2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Row < 16, 
    Column < 16, 
    get_opponent_color(Color, OpponentColor),
    ColPlus1 is Column + 1,
    ColPlus2 is Column + 2,
    ColPlus3 is Column + 3,
    RowPlus1 is Row + 1,
    RowPlus2 is Row + 2,
    RowPlus3 is Row + 3,
    get_element_by_pos(Board, [RowPlus1, ColPlus1], OpponentColor),
    get_element_by_pos(Board, [RowPlus2, ColPlus2], OpponentColor),
    get_element_by_pos(Board, [RowPlus3, ColPlus3], Color),
    remove_captures(Board, RowPlus1, ColPlus1, RemoveCap1),
    remove_captures(RemoveCap1, RowPlus2, ColPlus2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Row < 16,  
    Column > 2, 
    get_opponent_color(Color, OpponentColor),
    ColMin1 is Column - 1,
    ColMin2 is Column - 2,
    ColMin3 is Column - 3,
    RowPlus1 is Row + 1,
    RowPlus2 is Row + 2,
    RowPlus3 is Row + 3,
    get_element_by_pos(Board, [RowPlus1, ColMin1], OpponentColor),
    get_element_by_pos(Board, [RowPlus2, ColMin2], OpponentColor),
    get_element_by_pos(Board, [RowPlus3, ColMin3], Color),
    remove_captures(Board, RowPlus2, ColMin1, RemoveCap1),
    remove_captures(RemoveCap1, RowPlus2, ColMin2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).

check_capture(Board, Color, Row, Column, Score, Result):-
    Row > 2, 
    Column < 16,
    get_opponent_color(Color, OpponentColor),
    ColPlus1 is Column + 1,
    ColPlus2 is Column + 2,
    ColPlus3 is Column + 3,
    RowMin1 is Row - 1,
    RowMin2 is Row - 2,
    RowMin3 is Row - 3, 
    get_element_by_pos(Board, [RowMin1, ColPlus1], OpponentColor),
    get_element_by_pos(Board, [RowMin2, ColPlus2], OpponentColor),
    get_element_by_pos(Board, [RowMin3, ColPlus3], Color),
    remove_captures(Board, RowMin1, ColPlus1, RemoveCap1),
    remove_captures(RemoveCap1, RowMin2, ColPlus2, RemoveCap2),
    NewScore is Score + 1,
    check_capture(RemoveCap2, Color, Row, Column, NewScore, Result).
check_capture(Board, _, _, _, Score, [Board, Score]).

% ********************************************************************* 
% Function Name: remove_captures
% Purpose: removes stone in specified position
% Parameters: 
%        board - board state
%        row - row of stone being removed
%        column - column of stone being removed
%        updatedboard - out variable, board after stone removal
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
remove_captures(Board, Row, Column, UpdatedBoard) :-
    board_after_move(Board, Row, Column, "o", UpdatedBoard).

% ********************************************************************* 
% Function Name: get_opponent_color
% Purpose: get opponent color
% Parameters: 
%        color - current players color
%        opponentcolor - out variable, opponents color
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_opponent_color("w", "b").
get_opponent_color("b", "w").

% ********************************************************************* 
% Function Name: check_5_in_a_row
% Purpose: check if more resulted in 5 in a row, return score
% Parameters: 
%        board - board state 
%        color - color of stone placed
%        row - row of move that is being checked if it resulted in 5 in a row
%        column - column of move that is being checked if it resulted in 5 in a row
%        score - out variable, score for 5 in a rows
% Algorithm: 
%         1. Call helper_5_in_a_row in all directions, check how many consecutive stones there are.
%         2. Add them into score if score >= 5 (checked by get_score_for_5).
% Assistance Received: none 
% ***********************************************************************
check_5_in_a_row(Board, Row, Column, Color, Score) :-                        
    helper_5_in_a_row(Board, Color, Row, Column, 1, 0, SubScore1a),
    helper_5_in_a_row(Board, Color, Row, Column, -1, 0, SubScore1b),
    helper_5_in_a_row(Board, Color, Row, Column, 0, 1, SubScore2a),
    helper_5_in_a_row(Board, Color, Row, Column, 0, -1, SubScore2b),
    helper_5_in_a_row(Board, Color, Row, Column, 1, 1, SubScore3a),
    helper_5_in_a_row(Board, Color, Row, Column, -1, -1, SubScore3b),
    helper_5_in_a_row(Board, Color, Row, Column, 1, -1, SubScore4a),
    helper_5_in_a_row(Board, Color, Row, Column, -1, 1, SubScore4b),
    Score1 is SubScore1a + SubScore1b - 1,
    Score2 is SubScore2a + SubScore2b - 1,
    Score3 is SubScore3a + SubScore3b - 1,
    Score4 is SubScore4a + SubScore4b - 1,
    get_score_for_5(Score1, S1),
    get_score_for_5(Score2, S2),
    get_score_for_5(Score3, S3),
    get_score_for_5(Score4, S4),
    Score is S1 + S2 + S3 + S4.

% ********************************************************************* 
% Function Name: helper_5_in_a_row
% Purpose: count stones recursively in a direction while color matches
% Parameters: 
%        board - board state 
%        color - color of stone placed
%        row - row being checked
%        column - column being check
%        incrow - row increment, direction being checked
%        inccolumn - column increment, direction being checked
%        count - out variable, count of matching color stones in given direction
% Return Value: count of matching color stones in given direction
% Algorithm: 
%         1. Check if row and column match expected color. If so, increment count,
%            and check next row col positions.
%         2. Otherwise, return count that matched in that direction.
% Assistance Received: none 
% ***********************************************************************
helper_5_in_a_row(Board, Color, Row, Column, IncRow, IncColumn, Count) :-
    within_bounds(Row, Column),
    get_element_by_pos(Board, [Row, Column], Element),
    Element = Color,
    NewRow is Row + IncRow,
    NewColumn is Column + IncColumn,
    helper_5_in_a_row(Board, Color, NewRow, NewColumn, IncRow, IncColumn, NewCount),
    Count is NewCount + 1.
helper_5_in_a_row(_, _, _, _, _, _,  0).

% ********************************************************************* 
% Function Name: get_score_for_5
% Purpose: returns score of 5 if there are at least 5 consecutive stones, 0 otherwise
% Parameters: 
%        score - number of consecutive stones
%        result - score of 0 or 5
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
get_score_for_5(Score, 5) :-
    Score >= 5.

get_score_for_5(Score, 0) :-
    Score < 5.

% ********************************************************************* 
% Function Name: process_move
% Purpose: process the move after it has been placed
% Parameters: 
%        moveresult - definitely has board after move, might also have score inc and cap inc
%        human - list of human captures and score
%        comp - list of computer captures and score
%        outcome - out variable, list containing final board, human and comp information,
%                                next player and exit case
% Algorithm: 
%       1. If move result has all three elements, update the current players score and captures.
%       2. Check if these increments resulted in a game over condition.
%       3. If so, instantiate the outcome. Otherwise, call play with updated player values.
%       4. If move result only has board, call play with next player only.
% Assistance Received: none 
% ***********************************************************************
process_move(MoveResult, Human, Comp, CurrentPlayer, Outcome) :-
    get_next_player(CurrentPlayer, NextPlayer),
    % Move resulted in score increase or capture
    (length(MoveResult, 3) ->
        MoveResult = [UpdatedBoard, Score5, CapCount],
        CurrentPlayer = [Player, _],
        (Player = "human" ->
            inc_score(Human, Score5, NewH),
            inc_cap(NewH, CapCount, NewHuman),
            NewHuman = [Caps, Scr],
            ( (Scr >= 5 ; Caps >= 5) ->
                board_print(UpdatedBoard),
                capture_print(NewHuman, Comp),
                format("Game over! Human won! ~n"),
                Outcome = [UpdatedBoard, NewHuman, Comp, NextPlayer, 'over']
            ;
                play(UpdatedBoard, NewHuman, Comp, NextPlayer, Outcome)
            )
        ;
            inc_score(Comp, Score5, NewC),
            inc_cap(NewC, CapCount, NewComp),
            NewComp = [Caps, Scr],
            ( (Scr >= 5 ; Caps >= 5) ->
                board_print(UpdatedBoard),
                capture_print(Human, NewComp),
                format("Game over! Computer won! ~n"),
                Outcome = [UpdatedBoard, Human, NewComp, NextPlayer, 'over']
            ;
                play(UpdatedBoard, Human, NewComp, NextPlayer, Outcome)
            )
        )
    ; 
        play(MoveResult, Human, Comp, NextPlayer, Outcome)
    ).

% ********************************************************************* 
% Function Name: inc_score
% Purpose: updates score in list for player
% Parameters: 
%        player - list of captures and score
%        scoreinc - score increase increase
%        updatedplayer - out variable, updated list of captures and score
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
inc_score([Captures, CurrentScore], ScoreInc, [Captures, NewScore]) :-
    NewScore is CurrentScore + ScoreInc.

% ********************************************************************* 
% Function Name: inc_captures
% Purpose: updates capture count in list for player
% Parameters: 
%        player - list of captures and score
%        capinc - capture count increase
%        updatedplayer - out variable, updated list of captures and score
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
inc_cap([Captures, Score], CapInc, [NewCaptures, Score]) :-
    NewCaptures is Captures + CapInc.


% ********************************************************************* 
% Function Name: count_4_in_a_row
% Purpose: Count total number of 4 in a rows for given color/player
% Parameters: 
%        board - board state 
%        color - color being checked
%        scoresum - total number of 4 in a rows
%        loopcount - to check orientations recursively
%        finalscore - out variable, total number of 4 in a rows for given color
% Algorithm: 
%         1. Call helper_4_count for each direction, update score as received.
%         2. When board checked completely, copy scoresum into finalscore.
% ********************************************************************* 
count_4_in_a_row(_, _, FinalScore, 19, FinalScore).
count_4_in_a_row(Board, Color, Scoresum, Loopcount, FinalScore) :-
    Loopcount < 19, % safety measure
    helper_4_count(Board, Color, Loopcount, 0, 0, 1, 0, Dir1),
    helper_4_count(Board, Color, 0, Loopcount, 1, 0, 0, Dir2),
    helper_4_count(Board, Color, Loopcount, 18, 1, -1, 0, Dir4),
    helper_4_count(Board, Color, Loopcount, 0, 1, 1, 0, Dir6),
    Score is Dir1 + Dir2 + Dir4 + Dir6,
    % Special cases to not double count central diagonals
    (Loopcount = 18 ->
        helper_4_count(Board, Color, 0, Loopcount, 1, 1, 0, Dir5),
        Newscore is Scoresum + Score + Dir5
    ; Loopcount = 0 ->
        helper_4_count(Board, Color, 0, Loopcount, 1, -1, 0, Dir3),
        Newscore is Scoresum + Score + Dir3
    ; 
        helper_4_count(Board, Color, 0, Loopcount, 1, -1, 0, Dir3),
        helper_4_count(Board, Color, 0, Loopcount, 1, 1, 0, Dir5),
        Newscore is Scoresum + Score + Dir3 + Dir5
    ),
    NextLoop is Loopcount + 1,
    count_4_in_a_row(Board, Color, Newscore, NextLoop, FinalScore).

% ********************************************************************* 
% Function Name: helper_4_count
% Purpose: count stones recursively in a direction while color matches,
%          if count is 4 when no match is found, increment score
% Parameters: 
%        board - board state 
%        color - color being checked
%        row - row being checked
%        column - column being check
%        incrow - row increment, direction being checked
%        inccolumn - column increment, direction being checked
%        currentcount - number of stones in a row
%        score - out variable, number of 4 in a rows in given direction
% Algorithm: 
%         1. While within board bounds, check if row and column match color.
%         2. If so, increase count and update row and column by passed increments.
%         3. When match no longer occurs or board bounds exceeded, check if count is 4.
%            If so, increment score.
% ********************************************************************* 
helper_4_count(Board, Color, Row, Column, Incrow, Inccolumn, Currentcount, Score) :-
    within_bounds(Row, Column),
    (   get_element_by_pos(Board, [Row, Column], Element),
        Element = Color ->
        % Match! Increase count
        NewRow is Row + Incrow,
        NewColumn is Column + Inccolumn,
        NewCount is Currentcount + 1,
        helper_4_count(Board, Color, NewRow, NewColumn, Incrow, Inccolumn, NewCount, NewScore),
        Score is NewScore
    ; 
        % Match no longer occurs, increase score if 4 in a row
        (Currentcount = 4 ->
            NewRow is Row + Incrow,
            NewColumn is Column + Inccolumn,
            helper_4_count(Board, Color, NewRow, NewColumn, Incrow, Inccolumn, 0, NewScore),
            Score is NewScore + 1
        ; 
            % Reset currentcount
            NewRow is Row + Incrow,
            NewColumn is Column + Inccolumn,
            helper_4_count(Board, Color, NewRow, NewColumn, Incrow, Inccolumn, 0, NewScore),
            Score is NewScore
        )
    ).
% Board bounds exceeded, increase score if 4 in a row
helper_4_count(_, _, _, _, _, _, 4, 1).
helper_4_count(_, _, _, _, _, _, _, 0).
