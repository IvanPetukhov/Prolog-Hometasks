get_size(7, 7).

% Считает смещение.
get_offset(X, Y, Offset) :-
   get_size(W, _),
   Offset is Y * W + X.

% Делит список в позиции N.
split(0, [_ | T], [], T).
split(N, [X | T], [X | T1], Y) :-
   N1 is N - 1,
   split(N1, T, T1, Y).

% "Вынимает" значение.
get_value(X, Y, Maze, Value) :-
   get_offset(X, Y, Offset),
   nth0(Offset, Maze, Value).

% Устанавливает значение.
set_value(X, Y, Value, In, Out) :-
   get_offset(X, Y, Offset),
   split(Offset, In, Before, After),
   append(Before, [Value], Temp),
   append(Temp, After, Out).

% Выводит лебиринт.
show_block(wall)  :- write('[]').
show_block(space) :- write('  ').
show_maze(0, _) :- true.
show_maze(X, [H | T])  :-
   show_block(H),
   get_size(W, _),
   X1 is X - 1,
   ( X1 mod W =:= 0 -> nl, show_maze(X1, T); show_maze(X1, T) ).
show_maze(Maze) :-
   get_size(W, H),
   T is W * H,
   show_maze(T, Maze).

% Инициализация лабиринта.
init_maze(0, []).
init_maze(X, Maze) :-
   T is X - 1,
   init_maze(T, S),
   Maze = [wall|S].
init_maze(Maze) :-
   get_size(W, H),
   T is W * H,
   init_maze(T, Temp),
   set_value(1, 1, space, Temp, Maze).

% Создание в конкретных координатах.
carve_maze_coord(X, Y, Dir, Count, In, Out) :-
   ( Dir =:= 0 -> DX is  1, DY is  0;
     Dir =:= 1 -> DX is  0, DY is  1;
     Dir =:= 2 -> DX is -1, DY is  0;
     Dir =:= 3 -> DX is  0, DY is -1 ),
   X1 is X  + DX, Y1 is Y  + DY,
   X2 is X1 + DX, Y2 is Y1 + DY,
   get_size(W, H),
   ( X2 > 0, Y2 > 0, X2 < W, Y2 < H,
     get_value(X1, Y1, In, wall),
     get_value(X2, Y2, In, wall) ->
      set_value(X1, Y1, space, In, M1),
      set_value(X2, Y2, space, M1, M2),
      Dir1 is random(3),
      carve_maze_coord(X2, Y2, Dir1, 0, M2, Out)
   ;
      ( Count < 3 ->
         Count1 is Count + 1,
         Dir1 is (Dir + 1) mod 4,
         carve_maze_coord(X, Y, Dir1, Count1, In, Out)
      ;
         Out = In
      )
   ).

% Создание.
carve_maze(X, Y, In, Out) :-
   Dir is random(3),
   get_size(W, H),
   ( X < W, Y < H ->
      carve_maze_coord(X, Y, Dir, 0, In, Temp),
      X1 is X + 2,
      carve_maze(X1, Y, Temp, Out)
   ;
      ( X =:= W, Y < H ->
         carve_maze_coord(X, Y, Dir, 0, In, Temp),
         Y1 is Y + 2,
         carve_maze(1, Y1, Temp, Out)
      ;
         set_value(1, 0, space, In, M1),
         set_value(W - 2, H - 1, space, M1, Out)
      )
   ).
carve_maze(In, Out) :- carve_maze(1, 1, In, Out).

maze :-
   init_maze(EmptyMaze),
   !,
   carve_maze(EmptyMaze, Maze),
   show_maze(Maze).

size_check(W, H, X, Y) :-
   (W mod 2 =:= 0 -> X is W + 1; X is W),
   (H mod 2 =:= 0 -> Y is H + 1; Y is H).

create_maze(W, H) :-
   size_check(W, H, X, Y),
   assert(get_size(X, Y)),
   maze.

main :-
   new(Dialog, dialog('Maze')),
   send_list(Dialog, append,
	     [
		new(N1, int_item(width, low := 5, high := 20, default := 5)),
		new(N2, int_item(height, low := 5, high := 20, default := 5)),
		button(enter, message(@prolog, create_maze, N1?selection, N2?selection)),
		button(cancel, message(Dialog, destroy))
	     ]),
   send(Dialog, default_button, enter),
   send(Dialog, open).


