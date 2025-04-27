% 
% @author: 2211605
%

%main program


reduce_prog([Var,_,Body]) :-
		create_env(Var,env([[]],false),Env),
		reduce_stmt(config(Body,Env),_).
											
%check if X has been declared in the first list of the environment
has_declared(X,[id(X,_,_,Value)|_],id(X,_,_,Value)) :- !.
has_declared(X,[_|L],R) :-  has_declared(X,L,R).
has_declared(_, [], _) :- fail.
%check if X has been declared in the enviroment
check_declared(X, env([], _), _) :- throw(undeclare_identifier(X)).
check_declared(X, env([L|_],_), Y) :- has_declared(X,L,Y), !.
check_declared(X, env([_|L],_),Y) :- check_declared(X, env(L,_), Y).

%create a symbol table from the list of variable or constant declarations
create_env([],L,L).
create_env([var(X,Y)|_],env([_],_),_):- is_builtin(X,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|_],env([L1|_],_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|L],env([L1|L2],T),L3):- create_env(L,env([[id(X,var,Y,undefined)|L1]|L2],T),L3).

create_env([const(X,Y)|_],env([_],_),_):- is_builtin(X,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|_],env([L1|_],_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|L],env([L1|L2],T),L3):- create_env(L,env([[id(X,const,Y,undefined)|L1]|L2],T),L3).


% Helper: merge_env_after_block(+BlockEnv, +OuterEnv, -MergedEnv)
% BlockEnv = env([BlockScope|OuterScopes], Status)
% OuterEnv = env([OuterScope|OuterScopes], Status)
% MergedEnv = env([NewOuterScope|OuterScopes], Status)
merge_env_after_block(env([_BlockScope|OuterScopes], Status), env([OuterScope|OuterScopes], Status), env([NewOuterScope|OuterScopes], Status)) :-
    % Only update variables that exist in OuterScope
    update_outer_scope(OuterScope, OuterScopes, NewOuterScope).

% update_outer_scope(+OldScope, +Scopes, -NewScope)
% For each variable in OldScope, get its value from the corresponding scope in [OldScope|Scopes]
update_outer_scope([], _, []).
update_outer_scope([id(Name, Kind, Type, _)|T], Scopes, [id(Name, Kind, Type, Value)|T2]) :-
    get_var_value(Name, [Scopes], Value),
    update_outer_scope(T, Scopes, T2).

% get_var_value(+Name, +Scopes, -Value)
get_var_value(Name, [Scope|_], Value) :-
    member(id(Name, _, _, Value), Scope), !.
get_var_value(Name, [_|Rest], Value) :-
    get_var_value(Name, Rest, Value).
get_var_value(_, [], undefined).


boolean(true).
boolean(false).

% check type
check_type(Value, integer) :- integer(Value), !.
check_type(Value, float) :- float(Value), !.
check_type(Value, string) :- string(Value), !.
check_type(Value, boolean) :- boolean(Value), !.
check_type(_, _) :- fail.
% update env
% Case 1: Variable found in the current scope
update_env(Var, Value, env([Scope|Rest], Status), env([NewScope|Rest], Status)) :-
    has_declared(Var, Scope, id(Var, Kind, Type, _)), !, % Check if Var exists
    check_type(Value, Type), % Verify type compatibility
    replace_var(Scope, id(Var, Kind, Type, Value), NewScope). % Update value in scope

% Case 2: Variable not in current scope, search outer scopes
update_env(Var, Value, env([Scope|Rest], Status), env([Scope|NewRest], Status)) :-
    update_env(Var, Value, env(Rest, Status), env(NewRest, Status)).

% Case 3: Variable not found in any scope
update_env(Var, _, env([], _), _) :-
    throw(undeclare_identifier(Var)).

% replace_var(Scope, Var, Value, NewScope)
% Replaces the variable's value in the scope.
replace_var([id(Var, var, _, _)|T], id(Var, var, Type, Value), [id(Var, var, Type, Value)|T]) :- !.
replace_var([H|T], id(Var, var, Type, Value), [H|NewT]) :- replace_var(T, id(Var, var, Type, Value), NewT).


reduce(config(sub(E1), Env), config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		R is -V1.	
reduce(config(add(E1,E2),Env),config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		reduce_all(config(E2,Env),config(V2,Env)),
		R is V1+V2.
reduce(config(sub(E1,E2),Env),config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		reduce_all(config(E2,Env),config(V2,Env)),
		R is V1-V2.
reduce(config(times(E1,E2),Env),config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		reduce_all(config(E2,Env),config(V2,Env)),
		R is V1*V2.
% reduce for rdiv(E1, E2)
reduce(config(rdiv(E1, E2), Env), config(R, Env)) :- 

    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    
    % Kiểm tra kiểu của V1, V2
    ( check_type(V1, float);   check_type(V1, integer) ),
    ( check_type(V2, float);   check_type(V2, integer) ),
    % Xác định kiểu kết quả
    (   ( check_type(V1, float); check_type(V2, float) )
     ->  R is float(V1) / V2  % Kết quả là float nếu có ít nhất một float
    ; R is V1 // V2        % Kết quả là int nếu cả hai là int
    ).
% reduce(config(rdiv(_, E2), Env), _) :- 
%     reduce_all(config(E2, Env), config(V2, Env)),
%     V2 = 0, !,
%     throw(division_by_zero).
reduce(config(rdiv(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(rdiv(E1, E2))).

% reduce for idiv(E1, E2)
reduce(config(idiv(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    % Kiểm tra kiểu của V1, V2
    check_type(V1, integer),
    check_type(V2, integer),
    % Tính kết quả
    R is V1 // V2.  % Phép chia lấy nguyên

% reduce(config(idiv(_, E2), Env), _) :- 
%     reduce_all(config(E2, Env), config(V2, Env)),
%     V2 = 0, !,
%     throw(division_by_zero).
reduce(config(idiv(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ check_type(V1, integer)
    ;   \+ check_type(V2, integer)
    ),
    throw(type_mismatch(idiv(E1, E2))).

% reduce for imod(E1, E2)
reduce(config(imod(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    % Kiểm tra kiểu của V1, V2
    check_type(V1, integer),
    check_type(V2, integer),
    % Tính kết quả
    R is V1 mod V2.  % Phép chia lấy dư
% reduce(config(imod(_, E2), Env), _) :- 
%     reduce_all(config(E2, Env), config(V2, Env)),
%     V2 = 0, !,
%     throw(division_by_zero).
reduce(config(imod(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ check_type(V1, integer)
    ;   \+ check_type(V2, integer)
    ),
    throw(type_mismatch(imod(E1, E2))).
% reduce for bnot(E1)
reduce(config(bnot(E1), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    check_type(V1, boolean),
    (V1 = true -> R = false; R = true),!.
reduce(config(bnot(E1), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(bnot(E1))).

% reduce for band(E1, E2) with short-circuit evaluation
reduce(config(band(E1, _), Env), config(false, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    check_type(V1, boolean),
    V1 = false, !.  % Nếu E1 = false, không đánh giá E2
reduce(config(band(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    check_type(V1, boolean),
    V1 = true,
    reduce_all(config(E2, Env), config(V2, Env)),
    check_type(V2, boolean),
    (   V2 = true -> R = true ; R = false ).
reduce(config(band(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(band(E1, E2))).

% reduce for bor(E1, E2) with short-circuit evaluation
reduce(config(bor(E1, _), Env), config(true, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    check_type(V1, boolean),
    V1 = true, !.  % Nếu E1 = true, không đánh giá E2
reduce(config(bor(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    check_type(V1, boolean),
    V1 = false,
    reduce_all(config(E2, Env), config(V2, Env)),
    check_type(V2, boolean),
    (   V2 = true -> R = true ; R = false ).
reduce(config(bor(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(bor(E1, E2))).

% reduce for greater(E1, E2)
reduce(config(greater(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 > V2 -> R = true ; R = false ).
reduce(config(greater(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(greater(E1, E2))).

% reduce for less(E1, E2)
reduce(config(less(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 < V2 -> R = true ; R = false ).
reduce(config(less(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(less(E1, E2))).

% reduce for ge(E1, E2) (greater or equal)
reduce(config(ge(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 >= V2 -> R = true ; R = false ).
reduce(config(ge(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(ge(E1, E2))).

% reduce for le(E1, E2) (less or equal)
reduce(config(le(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 =< V2 -> R = true ; R = false ).
reduce(config(le(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(le(E1, E2))).

% reduce for ne(E1, E2) (not equal)
reduce(config(ne(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    % Kiểm tra cả hai cùng kiểu
    (   ( check_type(V1, integer), check_type(V2, integer) )
    ;   ( check_type(V1, boolean), check_type(V2, boolean) )
    ),
    (   V1 \= V2 -> R = true ; R = false ).
reduce(config(ne(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    \+ (   ( check_type(V1, integer), check_type(V2, integer) )
       ;   ( check_type(V1, boolean), check_type(V2, boolean) )
       ),
    throw(type_mismatch(ne(E1, E2))).

% reduce for eql(E1, E2) (equal)
reduce(config(eql(E1, E2), Env), config(R, Env)) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    % Kiểm tra cả hai cùng kiểu
    (   ( check_type(V1, integer), check_type(V2, integer) )
    ;   ( check_type(V1, boolean), check_type(V2, boolean) )
    ),
    (   V1 = V2 -> R = true ; R = false ).
reduce(config(eql(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    \+ (   ( check_type(V1, integer), check_type(V2, integer) )
       ;   ( check_type(V1, boolean), check_type(V2, boolean) )
       ),
    throw(type_mismatch(eql(E1, E2))).


reduce_all(config(V, Env), config(V, Env)) :- 
    boolean(V), !. % Giá trị boolean
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    check_declared(V, Env, id(V, const, _, Value)),
    ( Value \= undefined -> true
        ; throw(undefined_variable(V))
        ), !. % Giá trị hằng
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    check_declared(V, Env, id(V, var, _, Value)),
    ( Value \= undefined -> true
        ; throw(undefined_variable(V))
        ), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    integer(V), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    float(V), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    string(V), !.
reduce_all(config(E, Env), config(E2, Env)) :-
    reduce(config(E, Env), config(E1, Env)), !,
    reduce_all(config(E1, Env), config(E2, Env)).
reduce_all(config(V, _), _) :-
	atom(V),
	throw(undeclare_identifier(V)).

reduce_stmt(config([], Env), config(_,Env)) :- true.
reduce_stmt(config([call(writeInt,[X])|Rest], Env), config(call(writeInt,[X]),Env2)) :-
	reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, integer); throw(type_mismatch(call(writeInt,[X])))),!,
	write(V),
	reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(writeIntLn,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, integer); throw(type_mismatch(call(writeIntLn,[X])))),!,
    writeln(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(readInt,V)|Rest], Env),config(_,Env2)) :-
    read(V),
    (check_type(V, integer); throw(type_mismatch(call(readInt,V)))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), config(_,Env2)).
reduce_stmt(config([call(readReal,[])|Rest], Env),config(_,Env2)) :-
    read(V),
    (float(V);throw(type_mismatch(call(readReal,[])))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), config(_,Env2)).
reduce_stmt(config([call(writeReal,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    write(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(writeRealLn,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    writeln(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(writeBool,[X])|Rest], Env),config(_,Env2)) :-    
    reduce_all(config(X,Env),config(V,Env)),
    write(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(writeBoolLn,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    writeln(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(readBool,V)|Rest], Env),config(_,Env2)) :-
    read(V),
    (check_type(V, boolean); throw(type_mismatch(call(readBool,V)))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), config(_,Env2)).
reduce_stmt(config([call(writeStrLn,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, string); throw(type_mismatch(call(writeStrLn,V)))),!,
    writeln(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).
reduce_stmt(config([call(writeStr,[X])|Rest], Env),config(_,Env2)) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, string); throw(type_mismatch(call(writeStr,V)))),!,
    write(V),
    reduce_stmt(config(Rest, Env), config(_,Env2)).

reduce_stmt(config([assign(I, E1)|Rest], Env),config(assign(I,E1),Env2)) :-
	reduce_all(config(E1, Env), config(Rhs, Env)),
	update_env(I, Rhs, Env, NewEnv),
	reduce_stmt(config(Rest, NewEnv), config(_, Env2)).

reduce_stmt(config([block(L1, L2)|Rest], Env), config(block(L1,L2), Env2)) :-
    (   L1 = [] ->
        % No new scope, just execute in current env and propagate all changes
        reduce_stmt(config(L2, Env), config(_, BlockEnv1)),
        reduce_stmt(config(Rest, BlockEnv1), config(_, Env2))
    ;   % New scope created
        create_env(L1, Env, BlockEnv),
        reduce_stmt(config(L2, BlockEnv), config(_, BlockEnv1)),
        merge_env_after_block(BlockEnv1, Env, MergedEnv),
        reduce_stmt(config(Rest, MergedEnv), config(_, Env2))
    ).

reduce_stmt(config([if(E, S1, S2)|Rest], Env),config(if(E, S1, S2),Env2)) :-
    reduce_all(config(E, Env), config(V1, Env)),
    (check_type(V1, boolean); throw(type_mismatch(if(E, S1, S2)))),!,
    (V1 = true -> reduce_stmt(config([S1], Env), config(_,NewEnv)); reduce_stmt(config([S2], Env), config(_,NewEnv))),
    reduce_stmt(config(Rest, NewEnv), config(_,Env2)).

reduce_stmt(config([if(E, S1)|Rest], Env),config(if(E, S1),Env2)) :-
    reduce_all(config(E, Env), config(V1, Env)),
    (check_type(V1, boolean); throw(type_mismatch(if(E, S1)))),!,
    (V1 = true -> reduce_stmt(config([S1], Env), config(_,NewEnv)); true),
    reduce_stmt(config(Rest, NewEnv), config(_,Env2)).

% reduce_stmt for while(E, S) with break/continue handling
reduce_stmt(config([while(E, S)|Rest], Env), config(while(E,S),Env2)) :- 
    Env = env(Scopes, PrevStatus),
    EnvInLoop = env(Scopes, true),  % Đánh dấu đang trong vòng lặp
    reduce_all(config(E, EnvInLoop), config(V, EnvInLoop)),
    (check_type(V, boolean); throw(type_mismatch(while(E, S)))),!,
    (   V = true
    ->  reduce_stmt(config([S], EnvInLoop), Result),
        (   Result = config(break(null), Env1)
        ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
        ;   Result = config(continue(null), Env1)
        ->  reduce_stmt(config([while(E, S)], Env1), config(while(E,S), NewEnv))  % Bỏ qua phần còn lại, tiếp tục vòng lặp
        ;   Result = config(_, Env1)
        ->  reduce_stmt(config([while(E, S)], Env1), config(while(E,S), NewEnv))
        )
    ;   V = false,
        NewEnv = EnvInLoop
    ),
    NewEnv = env(Scopes2, _),
    EnvAfterLoop = env(Scopes2, PrevStatus),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% reduce_stmt for do(L, E) with break/continue handling
reduce_stmt(config([do(L, E)|Rest], Env), config(do, Env2)) :- 
    Env = env(Scopes, PrevStatus),
    EnvInLoop = env(Scopes, true),  % Đánh dấu đang trong vòng lặp
    reduce_stmt(config(L, EnvInLoop), Result),
    (   Result = config(break, Env1)
    ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
    ;   Result = config(continue, Env1)
    ->  reduce_all(config(E, Env1), config(V, Env1)),
        (check_type(V, boolean); throw(type_mismatch(do(L, E)))),!,
        (   V = true
        ->  reduce_stmt(config([do(L, E)], Env1), config(_, NewEnv))
        ;   V = false,
            NewEnv = Env1
        )
    ;   Result = config(_, Env1)
    ->  reduce_all(config(E, Env1), config(V, Env1)),
        (check_type(V, boolean); throw(type_mismatch(do(L, E)))),!,
        (   V = true
        ->  reduce_stmt(config([do(L, E)], Env1), config(_, NewEnv))
        ;   V = false,
            NewEnv = Env1
        )
    ),
    NewEnv = env(Scopes2, _),
    EnvAfterLoop = env(Scopes2, PrevStatus),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% reduce_stmt for loop(E, S) with break/continue handling
reduce_stmt(config([loop(E, S)|Rest], Env), config(_,Env2)) :- 
    Env = env(Scopes, PrevStatus),
    EnvInLoop = env(Scopes, true),  % Đánh dấu đang trong vòng lặp
    reduce_all(config(E, EnvInLoop), config(N, EnvInLoop)),
    (check_type(E, integer); throw(type_mismatch(loop(E,S)))),!,
    N >= 0,  % Đảm bảo số lần lặp không âm
    reduce_loop_with_ctrl(N, S, EnvInLoop, NewEnv),
    NewEnv = env(Scopes2, _),
    EnvAfterLoop = env(Scopes2, PrevStatus),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% Handle break/continue with exception if not in loop
reduce_stmt(config([break(null)|_], env(_, false)), _) :-
    throw(break_not_in_loop(null)).
reduce_stmt(config([continue(null)|_], env(_, false)), _) :-
    throw(continue_not_in_loop(null)).

reduce_stmt(config([break(null)|_], env(Scopes, true)), config(break(null), env(Scopes, true))) :- !.
reduce_stmt(config([continue(null)|_], env(Scopes, true)), config(continue(null), env(Scopes, true))) :- !.
% reduce_stmt(config([call(I, L)|Rest], Env),_) :-
%     reduce_all(config(L, Env), config(V1, Env)),
%     check_declared(I, Env, id(I, func, _, _)),
%     reduce_stmt(config(Rest, Env), _).


% Helper predicate for loop with break/continue
reduce_loop_with_ctrl(0, _, Env, Env) :- !.
reduce_loop_with_ctrl(N, S, Env, NewEnv) :-
    N > 0,
    reduce_stmt(config([S], Env), Result),
    (   Result = config(break, Env1)
    ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
    ;   Result = config(continue, Env1)
    ->  N1 is N - 1,
        reduce_loop_with_ctrl(N1, S, Env1, NewEnv)  % Bỏ qua phần còn lại, tiếp tục vòng lặp
    ;   Result = config(_, Env1)
    ->  N1 is N - 1,
        reduce_loop_with_ctrl(N1, S, Env1, NewEnv)
    ).


