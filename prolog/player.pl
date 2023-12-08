% *************************************************************
% * Name:  Mariya Popova                                      *
% * Project:  4 Pente Prolog                                  *
% * Class:  CMPS 366-01 Organization of Programming Languages *
% * Date:  December 5, 2023                                   *
% *************************************************************

% ********************************************************************* 
% Function Name: strategy
% Purpose: suggest a move based on outlined strategy design 
% Parameters: 
%        board - current board state 
%        color - color of player calling the strategy
%        move - out variable, move recommended by the strategy
% Algorithm:       
%       1. Check if there is a winning 5 in a row move. Return highest scoring one.
%       2. Check if the oppenent can win with 5 in a row and block this move. 
%       3. Check if the opponent can win in two moves and block one of them.
%       4. Check if a capture can be made.  Return highest scoring one.
%       5. Check if current player can avoid being captured.
%       6. If current player has a chain of 3 stones, add a 4th.
%       7. If current player has a chain of 2 stones, add a 3rd.
%       8. If its the third move on the board, enforce 3 intersections from center
%       9. If current player has one stone, add another near it.
%       10. If opponent has one stone, add another near it.
%       11. If it is the first move, suggest only J10.
%       12. Random move (in case of emergency).
% Assistance Received: none 
% ***********************************************************************
strategy(Board, Color, Move):-
    format("Strategy: "),
    get_opponent_color(Color, OpponentColor),
    find_winning_max(Board, Color, 0, 0, [], WinningMax),
    find_winning_max(Board, OpponentColor, 0, 0, [], OpponentWinningMax),
    block_win_in_2(Board, OpponentColor, OpponentWinInTwo),
    find_max_capture(Board, Color, 0, 0, [], CaptureMax),
    find_max_capture(Board, OpponentColor, 0, 0, [], AvoidCapture),
    find3(Board, Color, Cluster3),
    find2(Board, Color, Cluster2),
    find1(Board, Color, Cluster1),
    find1(Board, OpponentColor, NearOpponent),
    random_move(Board, RandomMove),
    (   % Executed if there is a winning move
        \+ WinningMax = [],
        WinningMax = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Found winning move with highest score: ~w~n", Name),
        Move = WinningMax
    ;   
        % Executed if the opponent has a winning move
        OpponentWinningMax = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Block opponent's winning move: ~w~n", Name),
        Move = OpponentWinningMax
    ;
        % Executed if the opponent has a winning move two moves away 
        \+ OpponentWinInTwo = [],
        OpponentWinInTwo = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Block winning in 2 moves: ~w~n", Name),
        Move = OpponentWinInTwo
    ;
        % Executed if opponent can be captured   
        \+ CaptureMax = [],
        CaptureMax = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Capture opponent: ~w~n", Name),
        Move = CaptureMax 
    ;
        % Executed if opponent can be captured   
        \+ AvoidCapture = [],
        AvoidCapture = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Avoid being captured: ~w~n", Name),
        Move = AvoidCapture 
    ;
        % Executed if cluster of 3 can be turned to 4
        \+ Cluster3 = [],
        Cluster3 = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Create a row of 4 stones: ~w~n", Name),
        Move = Cluster3
    ;
        % Executed if cluster of 2 can be turned to 3
        \+ Cluster2 = [],
        Cluster2 = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Create a row of 3 stones: ~w~n", Name),
        Move = Cluster2
    ;
        % Executed on the first players second move
        get_move_count(Board, 0, 0, 0, 2),
        format("First player's second move must be 3 intersections away from center: "),
        (   get_element_by_pos(Board, [6,9], "o"),
            format("J13~n"),
            Move = [6,9]
        ;
            get_element_by_pos(Board, [12,9], "o"),
            format("J7~n"),
            Move = [12,9]
        )
    ;
        % Executed if cluster of 1 can be turned to 2
        \+ Cluster1 = [],
        Cluster1 = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Add stone near existing stone: ~w~n", Name),
        Move = Cluster1
    ;
        % Executed if player has no stones on board but opponent does
        \+ NearOpponent = [],
        NearOpponent = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Add stone near opponent's stone: ~w~n", Name),
        Move = NearOpponent
    ;
        % Executed if it is the first move
        get_move_count(Board, 0, 0, 0, 0),
        format("First move must be J10~n"),
        Move = [9, 9]
    ;
        % Executed if something went wrong
        RandomMove = [Row, Col],
        row_col_to_move_name(Row, Col, Name),
        format("Random move: ~w~n", Name),
        Move = [Row, Col]
    ).

% ********************************************************************* 
% Function Name: find_winning_max
% Purpose: find winning 5 in a row positions, return the one with maximum 5 in a row score
% Parameters: 
%        board - current board state 
%        color - color being checked
%        row - current row being checked
%        col - current column being checked
%        positions - list of positions storing score, row, and col of winning positions
%        pos - out variable, final max scoring position, empty list if no match
% Algorithm:       
%       1. Traverse the board for empty spaces.
%       2. On each empty space, place a stone, and check if it results in 5 in a row.
%       3. If score is > 0, store the score it yields and the position, and pass to
%          recursive function call until end of board.
%       4. At end of board, returns empty list if there are no positions. Otherwise,
%          call max_position to return the highest scoring row and column.
% Assistance Received: none 
% ***********************************************************************
find_winning_max(_, _, 19, _, Positions, Pos):-
    length(Positions, Length),
    Length > 0,
    max_position(Positions, [0, 0, 0], Pos).
find_winning_max(_, _, 19, _, [], []). 
find_winning_max(Board, Color, Row, 19, Positions, Pos):-
    NextRow is Row + 1,
    find_winning_max(Board, Color, NextRow, 0, Positions, Pos).
find_winning_max(Board, Color, Row, Col, Positions, Pos) :-
    NextCol is Col + 1,
    get_element_by_pos(Board, [Row, Col], "o"),
    board_after_move(Board, Row, Col, Color, AfterMove),
    check_5_in_a_row(AfterMove, Row, Col, Color, Score),
    (
        Score > 0,
        find_winning_max(Board, Color, Row, NextCol, [[Score, Row, Col] | Positions], Pos)
    ;
        find_winning_max(Board, Color, Row, NextCol, Positions, Pos)
    ).
find_winning_max(Board, Color, Row, Col, Positions, Pos):-
    \+ get_element_by_pos(Board, [Row, Col], "o"),
    NextCol is Col + 1,
    find_winning_max(Board, Color, Row, NextCol, Positions, Pos).

% ********************************************************************* 
% Function Name: max_position
% Purpose: find position in list of positions with highest score
% Parameters: 
%        positions - list of position lists holding score, row, and columm
%        max - current position with maximum score value
%        pos - out variable, once positions are fully traversed, copy max into it
% Return Value: row and column with highest score
% Algorithm:       
%       1. If positions is empty, it has been traversed. Return max row and col.
%       2. Check if first element in positions has higher score than current max,
%           replace max in recursive function call if so.
%       3. Call max-position with remaining positions to be checked.
% Assistance Received: none 
% ***********************************************************************
max_position([], [_, Row, Col], [Row, Col]).
max_position([[Score, Row, Col] | Positions], [MaxScore | _], Pos) :-
    Score > MaxScore,
    max_position(Positions, [Score, Row, Col], Pos).
max_position([_ | Positions], Max, Pos) :-
    max_position(Positions, Max, Pos).

% ********************************************************************* 
% Function Name: block_win_in_2
% Purpose: block a win two moves away
% Parameters: 
%        board - current board state 
%        color - color being checked
%        match - out variable, suggested move, empty list if no match
% Algorithm:       
%       1. Check the board for 4 different patterns 2 moves away from winning with 
%          the help of findPattern.
%       2. If at least one pattern is found, the appropriate blocking position is returned.
%       3. If none of the patterns are found, empty list returned.
% Assistance Received: none 
% ***********************************************************************
block_win_in_2(Board, Color, Match):-
    find_pattern(Board, ["o", Color, Color, Color, "o", "o"], 0, Pattern1),
    find_pattern(Board, ["o", Color, Color, "o", Color, "o"], 0, Pattern2),
    find_pattern(Board, ["o", Color, "o", Color, Color, "o"], 0, Pattern3),
    find_pattern(Board, ["o", "o", Color, Color, Color, "o"], 0, Pattern4),

    (   \+ Pattern1 = [],
        Pattern1 = [_, _, _, _, Match | _]
    ;
        \+ Pattern2 = [],
        Pattern2 = [_, _, _, Match | _]
    ;
        \+ Pattern3 = [],
        Pattern3 = [_, _, Match | _]
    ;
        \+ Pattern4 = [],
        Pattern4 = [_, Match | _]
    ;
        Match = []
    ).

% ********************************************************************* 
% Function Name: find_pattern
% Purpose: find passed pattern and return its board positions
% Parameters: 
%        board - current board state 
%        pattern - pattern being searched for
%        loopcount - serves as loop count to iterate through board
%        matchpositions - out variable, positions of the pattern or empty list if no match.
% Return Value: positions of the pattern or empty list if no match.
% Algorithm:       
%       1. Call pattern_helper for each orientation on the board.
%       2. Store the returns in local direction labels.
%       3. If one of the returns has positions stored, return them.
%       4. Call find_pattern with increased loopcount until a set of positions
%          is found or entire board has been traversed.
%       5. No match: return empty list.
% Assistance Received: none 
% ***********************************************************************
find_pattern(_, _, 19, []).
find_pattern(Board, Pattern, Loopcount, MatchPositions) :-
    Loopcount < 19, % safety measure
    pattern_helper(Board, Pattern, Loopcount, 0, 0, 1, [], 0, Dir1),
    pattern_helper(Board, Pattern, 0, Loopcount, 1, 0, [], 0, Dir2),
    pattern_helper(Board, Pattern, Loopcount, 18, 1, -1, [], 0, Dir4),
    pattern_helper(Board, Pattern, Loopcount, 0, 1, 1, [], 0, Dir6),
    % Special cases to not double count central diagonals
    (   Loopcount = 18 ->
        pattern_helper(Board, Pattern, 0, Loopcount, 1, 1, [], 0, Dir5),
        Dir3 = []
    ; 
        Loopcount = 0 ->
        pattern_helper(Board, Pattern, 0, Loopcount, 1, -1, [], 0, Dir3),
        Dir5 = []
    ; 
        pattern_helper(Board, Pattern, 0, Loopcount, 1, -1, [], 0, Dir3),
        pattern_helper(Board, Pattern, 0, Loopcount, 1, 1, [], 0, Dir5)
    ),
    AllDirs = [Dir1, Dir2, Dir3, Dir4, Dir5, Dir6],
    remove_empty_lists(AllDirs, Found),
    length(Found, NumFound),
    (   NumFound > 0 ->
        Found = [First | _],
        reverse_list(First, [], MatchPositions)
    ;
        NextLoop is Loopcount + 1,
        find_pattern(Board, Pattern, NextLoop, MatchPositions)
    ).

% ********************************************************************* 
% Function Name: remove_empty_lists
% Purpose: remove empty lists from a list of lists
% Parameters: 
%        inputlist - list of lists
%        outputlist - out variable, inputlist not containing any empty lists
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
empty_list([]).
remove_empty_lists(InputList, OutputList) :-
    exclude(empty_list, InputList, OutputList).

% ********************************************************************* 
% Function Name: reverse_list
% Purpose: reverses a list 
% Parameters: 
%        [first | rest] - input list
%        reversed - reversed list so far
%        reversedlist - out variable, input list in reverse order
% Algorithm: none
% Assistance Received: none 
% ***********************************************************************
reverse_list([], ReversedList, ReversedList).
reverse_list([First|Rest], Reversed, ReversedList) :- 
    reverse_list(Rest, [First|Reversed], ReversedList).

% ********************************************************************* 
% Function Name: pattern_helper
% Purpose: find passed pattern and return its board positions
% Parameters: 
%        board - current board state 
%        pattern - pattern being searched for
%        row - row being checked
%        col - column being check
%        incrow - row increment, direction being checked
%        inccol - column increment, direction being checked
%        positions - list of positions of the pattern so far
%        count - keep track of which pattern element is needed next 
%                based on number of positions found already
%        matchedpositions - out variable, final positions once full pattern found
% Algorithm:       
%        1. While within board bounds, tranverse board by changing row and 
%        col by incrow and inccol.
%        2. At each row col position, check if color corresponds to what 
%        is expected in the pattern.
%        3. If it does, add to positions list and increment count.
%        4. Otherwise, call pattern_helper with empty positions list and 0 count,
%        restarting at the first position in positions, if any, incremented by 
%        the increments. This is necessary to not skip over any position that 
%        reset the pattern but are part of the next pattern right away.
%        5. pattern_helper called recursively until count is equal to the pattern
%           length or end of row/col direction being traversed.
% Assistance Received: none 
% ***********************************************************************
% Full pattern found
pattern_helper(_, Pattern, _, _, _, _, Positions, Count, Positions) :-
    length(Pattern, Count). 
% Position matches what is expected
pattern_helper(Board, Pattern, Row, Col, IncRow, IncCol, Positions, Count, MatchedPositions) :-
    % length(Pattern, Count),
    within_bounds(Row, Col),
    get_element_by_pos(Board, [Row, Col], Element),
    nth0(Count, Pattern, Element),
    NextRow is Row + IncRow,
    NextCol is Col + IncCol, 
    NewCount is Count + 1,
    pattern_helper(Board, Pattern, NextRow, NextCol, IncRow, IncCol, [ [Row, Col] | Positions], NewCount, MatchedPositions).
% No match, next position
pattern_helper(Board, Pattern, Row, Col, IncRow, IncCol, Positions, _, MatchedPositions) :-
    length(Positions, 0),
    within_bounds(Row, Col),
    NextRow is Row + IncRow,
    NextCol is Col + IncCol,
    pattern_helper(Board, Pattern, NextRow, NextCol, IncRow, IncCol, [], 0, MatchedPositions).
% Restart at position after the first in the current pattern positions
pattern_helper(Board, Pattern, Row, Col, IncRow, IncCol, Positions, _, MatchedPositions) :-
    length(Positions, Length),
    within_bounds(Row, Col),
    Length > 0,
    Positions = [[FirstMatchedRow, FirstMatchedColumn] | _],
    NextRow is FirstMatchedRow + IncRow,
    NextCol is FirstMatchedColumn + IncCol,
    pattern_helper(Board, Pattern, NextRow, NextCol, IncRow, IncCol, [], 0, MatchedPositions).
% No match found at all
pattern_helper(_, _, Row, Col, _, _, _, _, []) :-
    \+ within_bounds(Row, Col).

% ********************************************************************* 
% Function Name: find_max_capture
% Purpose: find position on board that results in the maximum number of captures
% Parameters: 
%        board - current board state 
%        color - color being placed to check
%        row - current row being checked
%        col - current column being checked
%        positions - list of positions storing score, row, and col of capturing positions
%        pos - out variable, final max scoring position, empty list if no match
% Algorithm:       
%       1. Traverse the board for empty spaces.
%       2. On each empty space, place a stone, and check if it results in a capture.
%       3. If score is > 0, store the score it yields and the position, and pass to
%          recursive function call until end of board.
%       4. At end of board, returns empty list if there are no positions. Otherwise,
%          call max_position to return the highest scoring row and column.
% Assistance Received: none 
% ***********************************************************************
% Board fully traversed, return max positions
find_max_capture(_, _, 19, _, Positions, Pos):-
    length(Positions, Length),
    Length > 0,
    max_position(Positions, [0, 0, 0], Pos).
% Board fully traversed, no positions, return empty list
find_max_capture(_, _, 19, _, [], []). 
% Next row
find_max_capture(Board, Color, Row, 19, Positions, Pos):-
    NextRow is Row + 1,
    find_max_capture(Board, Color, NextRow, 0, Positions, Pos).
find_max_capture(Board, Color, Row, Col, Positions, Pos):-
    NextCol is Col + 1,
    get_element_by_pos(Board, [Row, Col], "o"), 
    check_capture(Board, Color, Row, Col, 0, [_, Score]),
    (   Score > 0,
        find_max_capture(Board, Color, Row, NextCol, [[Score, Row, Col] | Positions], Pos)
    ;
        find_max_capture(Board, Color, Row, NextCol, Positions, Pos)
    ).
find_max_capture(Board, Color, Row, Col, Positions, Pos):-
    \+ get_element_by_pos(Board, [Row, Col], "o"),
    NextCol is Col + 1,
    find_max_capture(Board, Color, Row, NextCol, Positions, Pos).

% ********************************************************************* 
% Function Name: find3
% Purpose: find clusters of 3 stones that can turn to 4, and later to 5
% Parameters: 
%        board - current board state 
%        color - color being checked
%        match - out variable, position recommended, empty list if none
% Algorithm:       
%       1. Set up various clusters of 3 patterns.
%       2. Check if they can be found on the board.
%       3. If so, return move that will turn cluster to 4.
%       4. If no patterns are found, return empty list.
% Assistance Received: none 
% ***********************************************************************
find3(Board, Color, Match):-
    find_pattern(Board, ["o", Color, Color, Color, "o"], 0, Pattern1),
    find_pattern(Board, ["o", "o", Color, Color, Color], 0, Pattern2),
    find_pattern(Board, [Color, Color, Color, "o", "o"], 0, Pattern3),
    find_pattern(Board, ["o", Color, "o", Color, Color], 0, Pattern4),
    find_pattern(Board, [Color, "o", Color, Color, "o"], 0, Pattern5),
    find_pattern(Board, [Color, Color, "o", Color, "o"], 0, Pattern6),
    find_pattern(Board, ["o", Color, Color, "o", Color], 0, Pattern7),

    (   \+ Pattern1 = [],
        Pattern1 = [Match | _]
    ;
        \+ Pattern2 = [],
        Pattern2 = [_, Match | _]
    ;
        \+ Pattern3 = [],
        Pattern3 = [_, _, _, Match | _]
    ;
        \+ Pattern4 = [],
        Pattern4 = [_, _, Match | _]
    ;
        \+ Pattern5 = [],
        Pattern5 = [_, Match | _]
    ;
        \+ Pattern6 = [],
        Pattern6 = [_, _, Match | _]
    ;
        \+ Pattern7 = [],
        Pattern7 = [_, _, _, Match |_ ]
    ;
        Match = []
    ).

% ********************************************************************* 
% Function Name: find2
% Purpose: find clusters of 2 stones that can turn to 3
% Parameters: 
%        board - current board state 
%        color - color being checked 
%        match - out variable, position recommended, empty list if none
% Return Value: row and column of suggested position
% Algorithm:       
%       1. Set up various clusters of 2 patterns.
%       2. Check if they can be found on the board.
%       3. If so, return move that will turn cluster to 3.
%       4. If no patterns are found, return empty list.
% Assistance Received: none 
% ***********************************************************************
find2(Board, Color, Match):-
    find_pattern(Board, ["o", Color, Color, "o"], 0, Pattern1),
    find_pattern(Board, ["o", "o", Color, Color], 0, Pattern2),
    find_pattern(Board, [Color, Color, "o", "o"], 0, Pattern3),
    find_pattern(Board, ["o", Color, "o", Color], 0, Pattern4),
    find_pattern(Board, [Color, "o", Color, "o"], 0, Pattern5),

    (   \+ Pattern1 = [],
        Pattern1 = [Match | _]
    ;
        \+ Pattern2 = [],
        Pattern2 = [_, Match | _]
    ;
        \+ Pattern3 = [],
        Pattern3 = [_, _, Match | _]
    ;
        \+ Pattern4 = [],
        Pattern4 = [_, _, Match | _]
    ;
        \+ Pattern5 = [],
        Pattern5 = [_, Match | _]
    ;
        Match = []
    ).

% ********************************************************************* 
% Function Name: find1
% Purpose: find single stones near which another stone can be placed
%            while also avoiding being captured
% Parameters: 
%        board - current board state 
%        color - color being checked
%        match - out variable, position recommended, empty list if none
% Return Value: row and column of suggested position
% Algorithm:       
%       1. If move count is 2, must place stone 3 intersections
%        away from center. Two options provided, if one is taken, return other.
%       2. Find patterns and suggest best move while avoiding being captured.
%           (i.e. OC*_ placing C stone at * can result in capture from opponent,
%                 so it is instead placed here OC_*)
% Assistance Received: none 
% ***********************************************************************
find1(Board, Color, Match):-
    get_opponent_color(Color, OpponentColor),
    find_pattern(Board, [OpponentColor, Color, "o", "o"], 0, Pattern1),
    find_pattern(Board, ["o", "o", Color, "o"], 0, Pattern2),
    find_pattern(Board, ["o", Color, "o", "o"], 0, Pattern3),
    find_pattern(Board, ["o", "o", Color, OpponentColor], 0, Pattern4),

    (   \+ Pattern1 = [],
        Pattern1 = [_, _, _, Match]
    ;
        \+ Pattern2 = [],
        Pattern2 = [_, Match | _]
    ;
        \+ Pattern3 = [],
        Pattern3 = [_, _, Match | _]
    ;
        \+ Pattern4 = [],
        Pattern4 = [Match | _]
    ;
        Match = []
    ).

% ***********************************************************************
% Function Name: random_move
% Purpose: generate a random move
% Parameters: 
%        board - current board state 
%        move - out variable, random move returned
% Algorithm:       
%       1. Generates a random row and column, checks if it is empty on board,
%           returns move if so.
%       2. If position not empty on board, call random_move and try again.
% NOTE: This is added in the strategy so the computer can keep playing
%       if something malfunctions. However, there are enough strategy
%       methods that this should never be called. Purely a safety.
% Assistance Received: none 
% ***********************************************************************
random_move(Board, Move):-
    random_between(0, 18, RandomRow),
    random_between(0, 18, RandomCol),
    (   get_element_by_pos(Board, [RandomRow, RandomCol], "o") ->
        Move = [RandomRow, RandomCol]
    ; 
        random_move(Board, Move)
    ).