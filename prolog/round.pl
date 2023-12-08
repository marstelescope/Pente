% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: coin_toss
% Purpose: simulate a coin toss by generating a random state and assigning
%          it heads or tails, compare to user guess
% Parameters: 
%        nextplayer - out variable, list of next player and their stone color
% Algorithm: none
% Assistance: https://stackoverflow.com/questions/2261238/random-items-in-prolog
% ********************************************************************* 
coin_toss(NextPlayer) :-
    % Simulate a coin toss by generating a random state and assigning it heads or tails
    random_member(a(Result), [a("heads"), a("tails")]),

    format("Choose heads or tails: "),
    read_line_to_string(user_input, Inp),
    string_lower(Inp, Input),
    (   (Input = "heads" ; Input = "tails") ->  
        format("Coin landed on: ~a~n", [Result]),
        (   (Result = Input) ->
            format("Human plays first!~n"),
            NextPlayer = ["human", "white"] 
        ;   
            format("Computer plays first!~n"),
            NextPlayer = ["computer", "white"]
        )
    ;   
        format("Invalid input.~n"),
        coin_toss(NextPlayer)
    ).

% ********************************************************************* 
% Function Name: round
% Purpose: carry out a round
% Parameters: 
%        board - board state passed from file or initialized 
%        human - list of human captures and tournament score 
%        comp - list of computer captures and tournament score 
%        nextplayer - list of next player and their stone color
%        outcome - out variable, list of game state parameters upon quit or game over 
% Algorithm:
%       1. If board is in an initial state, decide who plays first
%           based on score. If scores are equal, call coin toss.
%        2. Start/resume play of the game.
% Assistance Received: none 
% ********************************************************************* 
round(Board, [_, HumanScore], [_, CompScore], _, Outcome):-
    get_move_count(Board, 0, 0, 0, 0),
    CompScore = HumanScore,
    format("Coin toss to determine first player...~n"),
    coin_toss(FirstPlayer),
    play(Board, [0, 0], [0, 0], FirstPlayer, Outcome).

round(Board, [_, HumanScore], [_, CompScore], _, Outcome):-
    get_move_count(Board, 0, 0, 0, 0),
    CompScore > HumanScore,
    format("Computer goes first because of higher score!~n"),
    play(Board, [0, 0], [0, 0], ["computer", "white"], Outcome).

round(Board, [_, HumanScore], [_, CompScore], _, Outcome):-
    get_move_count(Board, 0, 0, 0, 0),
    CompScore < HumanScore,
    format("Human goes first because of higher score!~n"),
    play(Board, [0, 0], [0, 0], ["human", "white"], Outcome).

round(Board, [HumanCap, _], [CompCap, _], NextPlayer, Outcome):-
    get_move_count(Board, 0, 0, 0, Count),
    Count > 0,
    format("Resuming game~n"),
    play(Board, [HumanCap, 0], [CompCap, 0], NextPlayer, Outcome).