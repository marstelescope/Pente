% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: open_from_file
% Purpose: read board, score, and next player data from file
% Parameters: 
%        contents - list of each string line from file
%        board - out variable, board from file
%        human - out variable, human stats from file
%        comp - out variable, computer stats from file
%        nextplayer - out variable, next player information from file
% Algorithm: follow expected file format 
% Assistance Received: https://www.swi-prolog.org/pldoc/man?predicate=split_string/4
% ***********************************************************************
open_from_file(Contents, Board, Human, Comp, NextPlayer) :-
    board_from_file(Contents, Board, 3, 22),
    nth0(25, Contents, HumanLine),
    split_string(HumanLine, ", ", "", HumanL),
    delete(HumanL, "", [HumanCap, HumanScore]),
    atom_number(HumanCap, HumanCapNum),
    atom_number(HumanScore, HumanScoreNum), 
    Human = [HumanCapNum, HumanScoreNum],
    nth0(28, Contents, CompLine),
    split_string(CompLine, ", ", "", CompL),
    delete(CompL, "", [CompCap, CompScore]),
    atom_number(CompCap, CompCapNum),
    atom_number(CompScore, CompScoreNum), 
    Comp = [CompCapNum, CompScoreNum],
    nth0(31, Contents, NextPlayerLine),
    split_string(NextPlayerLine, ", ", "", NextPlayerL),
    delete(NextPlayerL, "", NextPlayer).

% ********************************************************************* 
% Function Name: board_from_file
% Purpose: read board from file
% Parameters: 
%        contents - list of each string line from file
%        board - out variable, board in proper list form
%        currentline - current line from contents being read
%        endline - stop condition for recursion
% Algorithm: none
% Assistance Received: https://stackoverflow.com/questions/6094500/prolog-remove-multiple-elements-from-a-list#:~:text=To%20remove%20multiple%20elements%2C%20we,Xs%2C%20Y%2C%20Zs).
% ***********************************************************************
board_from_file(_, [], EndLine, EndLine). 
board_from_file(Contents, [Line | Rest], CurrentLine, EndLine) :-
    nth0(CurrentLine, Contents, TempLine),
    split_string(TempLine, ", ", "", TL),
    subtract(TL, ["]",",", "["], TList),
    delete(TList, "", Line),
    NextLine is CurrentLine + 1,
    board_from_file(Contents, Rest, NextLine, EndLine).

% ********************************************************************* 
% Function Name: save_game
% Purpose: save game state to file
% Parameters: 
%        board - board at current game state
%        human - human captures and score at current game state
%        comp - computer captures and score at current game state
%        np - next player information
%        filename - filename to save to
% Algorithm: none
% Assistance Received: none
% ********************************************************************* 
save_game(Board, [Human1 , Human2], [Comp1 , Comp2], [NP1 , NP2], Filename) :-
    open(Filename, write, Stream), 
    write(Stream, '[\n'),
    write(Stream, '   ; Board:\n'),
    write(Stream, '   [\n'),
    board_to_file(Stream, Board),
    write(Stream, '    ],\n'),
    write(Stream, '\n    ; human\n'),
    format(Stream, "    ~w, ~w,~n", [Human1, Human2]),
    write(Stream, '\n    ; computer\n'),
    format(Stream, "    ~w, ~w,~n", [Comp1, Comp2]),
    write(Stream, '\n    ; next player\n'), 
    format(Stream, "    ~w, ~w~n", [NP1, NP2]),
    write(Stream, '].'),
    close(Stream).

% ********************************************************************* 
% Function Name: board_to_file
% Purpose: save board to file
% Parameters: 
%        stream - file stream
%        board - board to save to file
% Algorithm: none
% Assistance Received: none
% ********************************************************************* 
board_to_file(_, []).
board_to_file(Stream, [Row|Rest]) :-
    write(Stream, '     [ '),
    row_to_file(Stream, Row),
    (   Rest = [] 
    ->  write(Stream, ' ]\n') 
    ; 
        write(Stream, ' ],\n') 
    ),
    board_to_file(Stream, Rest).

% ********************************************************************* 
% Function Name: row_to_file
% Purpose: save board to file
% Parameters: 
%        stream - file stream
%        row - board row being written to file
% Algorithm: none
% Assistance Received: none
% ********************************************************************* 
row_to_file(_, []).
row_to_file(Stream, [Row]) :-
    write(Stream, Row).
row_to_file(Stream, [CurrentPosition | Rest]) :-
    write(Stream, CurrentPosition),
    write(Stream, ', '),
    row_to_file(Stream, Rest).