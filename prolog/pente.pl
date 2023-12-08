% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: load_files
% Purpose: load all files needed for game
% Parameters: none
% Algorithm: none
% Assistance Received: none
% ********************************************************************* 
load_files :-
    consult('file.pl'),
    consult('board.pl'),
    consult('round.pl'),
    consult('player.pl'),
    consult('tournament.pl').

% ********************************************************************* 
% Function Name: pente
% Purpose: carry out the pente game!
% Parameters: none
% Algorithm: none
% Assistance Received: https://www.swi-prolog.org/pldoc/man?predicate=read_line_to_string/2
% ********************************************************************* 
pente :-
    load_files,
    format("Welcome to Pente ~n"),
    format("Load existing game? Enter 'Y' or 'N': ~n"),
    read_line_to_string(user_input, S1),
    string_lower(S1, S2),
    start(S2).

% ********************************************************************* 
% Function Name: start
% Purpose: load all files needed for game
% Parameters: 
%      userinput - what the user wrote when prompted from pente
% Algorithm: 
%      1. "y" - If file exists, load game state details from file 
%      2. "y" - If file doenst exist, initialize new game
%      3. "n" - Initialize new game
%      4. invalid input - prompt and call start again
% Assistance Received: none
% ********************************************************************* 
start("y") :-
    get_filename(Filename),
    (   exists_file(Filename)
    ->  read_file_to_string(Filename, FileContent, []),
        split_string(FileContent, "\n", "", L),
        open_from_file(L, Board, Human, Comp, NextPlayer),
        tournament(Board, Human, Comp, NextPlayer, Filename)
    ;   
        format("File does not exist, starting a new game. ~n"),
        board_init(19, Board),
        tournament(Board, [0, 0], [0, 0], ["human", "white"], Filename)
    ).

start("n") :-
    load_files,
    format("Starting new game. "),
    get_filename(Filename),
    board_init(19, Board),
    tournament(Board, [0, 0], [0, 0], ["human", "white"], Filename).

start(_) :-
    format("Invalid input. Enter 'Y' or 'N': ~n"),
    read_line_to_string(user_input, S1),
    string_lower(S1, S2),
    start(S2).

% ********************************************************************* 
% Function Name: get_filename
% Purpose: get a valid filename so program doesnt crash when saving
% Parameters: 
%      filename - out variable, filename
% Algorithm: none
% Assistance Received: none
% ********************************************************************* 
get_filename(Filename) :-
    format("Enter filename: ~n"),
    read_line_to_string(user_input, Input),
    (   (Input = "" ; Input = "\n")
    ->  get_filename(Filename)
    ;   
        Filename = Input
    ).