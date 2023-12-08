% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: tournament
% Purpose: carry out the tournament, which is comprised of rounds; update
%          tournament score at the end of each round
% Parameters: 
%      board - board state passed from file or initialized 
%      human - list of human captures and tournament score 
%      comp - list of computer captures and tournament score 
%      nextplayer - list of next player and their stone color 
%      filename - file to save to if the user decides to end the game 
% Algorithm:
%      1. Store tournament scores before round changes value to track round score.
%      2. Store the current state of the round (values returned by round) to quit or continue 
%         the game accordingly.
%      3. If the user didnt quit before the end of the round, round scores displayed. 
%         If the user quit, save the current state to a file.
%      4. Tournament scores displayed, and the winner announced if the user 
%         says no to playing another round. Stats saved to a file.
%      5. The tournament is called again with updated tournament score values if
%         the user agrees to play another round from the prompt.
% Assistance Received: none 
% ********************************************************************* 
tournament(Board, [HCap, HScore], [CCap, CScore], NextPlayer, Filename):-
    % Store scores before round changes value to keep track of round score
    HumanTourScore = HScore,
    CompTourScore = CScore,
    round(Board, [HCap, HScore], [CCap, CScore], NextPlayer, [FinalBoard, [HRoundCap, HRoundScore], [CRoundCap, CRoundScore], RoundNextPlayer, ExitCase]),
    % Act according to exit case
    board_init(19, FreshBoard),
    (   ExitCase = 'quit' 
    ->  save_game(FinalBoard, [HRoundCap, HumanTourScore], [CRoundCap, CompTourScore], RoundNextPlayer, Filename),
        format("Game saved to file.~n")
    ;
        scores_after_round(FinalBoard, RoundNextPlayer, [HRoundCap, HRoundScore], [CRoundCap, CRoundScore], HumanTourScore, CompTourScore, TournamentScores),
        TournamentScores = [HumanScore, CompScore],
        format("Play another round? Enter 'Y' or 'N': ~n"),
        read_line_to_string(user_input, UserInput),
        string_upper(UserInput, Input),
        another_round_prompt(Input, FreshBoard, [0, HumanScore], [0, CompScore], NextPlayer, Filename)
    ).

% ********************************************************************* 
% Function Name: scores_after_round
% Purpose: compute round and tournament scores and display them
% Parameters: 
%      board - board after the round ends
%      nextplayer - list of the next player and their stone color 
%      human - list of human captures and round score 
%      comp - list of computer captures and round score 
%      humantourscore - humans tournament score before this round 
%      comptourscore - computers tournament score before this round 
%      tournamentscores - out variable, list of human and computer score after this round
% Algorithm:
%      1. Compute round scores by adding capture count, any 5 in a row scores, and counting
%         4 in a rows for each player.
%      2. Compute tournament scores by adding to the current tournament scores the round scores.
%      3. Print both, return a list of tournament scores to the tournament function.
% Assistance Received: none 
% ********************************************************************* 
scores_after_round(Board, NextPlayer, [HumanC, HumanS], [CompC, CompS], HumanTourScore, CompTourScore, TournamentScores) :-
    % Check the next player color to add 4 in a row counts to the correct players
    (NextPlayer = ["human", "white"] ; NextPlayer = ["computer", "black"]),
    count_4_in_a_row(Board, "b", 0, 0, Comp4),
    CompRoundScore is CompC + CompS + Comp4,
    count_4_in_a_row(Board, "w", 0, 0, Human4),
    HumanRoundScore is HumanC + HumanS + Human4,
    CompTournament is CompTourScore + CompRoundScore,
    HumanTournament is HumanTourScore + HumanRoundScore,
    format("Round scores:~n"),
    print_scores(HumanRoundScore, CompRoundScore),
    format("Tournament scores:~n"),
    print_scores(HumanTournament, CompTournament),
    TournamentScores = [HumanTournament, CompTournament].

scores_after_round(Board, NextPlayer, [HumanC, HumanS], [CompC, CompS], HumanTourScore, CompTourScore, TournamentScores) :-
    % Check the next player color to add 4 in a row counts to the correct players
    (NextPlayer = ["computer", "white"] ; NextPlayer = ["human", "black"]),
    count_4_in_a_row(Board, "w", 0, 0, Comp4),
    CompRoundScore is CompC + CompS + Comp4,
    count_4_in_a_row(Board, "b", 0, 0, Human4),
    HumanRoundScore is HumanC + HumanS + Human4,
    CompTournament is CompTourScore + CompRoundScore,
    HumanTournament is HumanTourScore + HumanRoundScore,
    format("Round scores:~n"),
    print_scores(HumanRoundScore, CompRoundScore),
    format("Tournament scores:~n"),
    print_scores(HumanTournament, CompTournament),
    TournamentScores = [HumanTournament, CompTournament].

% ********************************************************************* 
% Function Name: print_scores
% Purpose: score printing function
% Parameters: 
%      humanscore - score passed for the human 
%      compscore - score passed for the computer 
% Algorithm: none
% Assistance Received: none 
% ********************************************************************* 
print_scores(HumanScore, CompScore) :-
    format("Human: ~w~n", [HumanScore]),
    format("Computer: ~w~n", [CompScore]).

% ********************************************************************* 
% Function Name: another_round_prompt
% Purpose: get valid input from the user 
% Parameters: 
%      userinput - to decide which rule to follow
%      board - new board to save to file or start next rounf
%      human - list of human captures and tournament score 
%      comp - list of computer captures and tournament score 
%      nextplayer - list of the next player and their stone color 
%      filename - filename to save game to if no more rounds
% Algorithm: 
%      1. "y" - call tournament to start next round
%      2. "n" - display winner of tournament and save game to file
%      4. invalid input - prompt and call another_round_prompt again
% Assistance Received: none 
% ********************************************************************* 
another_round_prompt("N", FreshBoard, [0, HumanScore], [0, CompScore], NextPlayer, Filename):-
    writeln("Tournament scores:"),
    print_scores(HumanScore, CompScore),
    (   HumanScore > CompScore
    ->  format("Winner of the tournament: Human player!~n")
    ; 
        CompScore > HumanScore 
    ->  format("Winner of the tournament: Computer player!~n")
    ; 
        format("Winner of the tournament: It's a draw!~n")
    ),
    save_game(FreshBoard, [0, HumanScore], [0, CompScore], NextPlayer, Filename).
another_round_prompt("Y", FreshBoard, [0, HumanScore], [0, CompScore], _, Filename):-
    tournament(FreshBoard, [0, HumanScore], [0, CompScore], ["", ""], Filename).
another_round_prompt(_, FreshBoard, [0, HumanScore], [0, CompScore], NextPlayer, Filename):-
    format("Enter 'Y' or 'N': ~n"),
    read_line_to_string(user_input, UserInput),
    string_upper(UserInput, Input),
    another_round_prompt(Input, FreshBoard, [0, HumanScore], [0, CompScore], NextPlayer, Filename).
