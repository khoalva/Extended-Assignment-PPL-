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
    check_declared(V, Env, id(V, const, _, Value)), !. % Giá trị hằng
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    check_declared(V, Env, id(V, var, _, Value)), !.
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

reduce_stmt(config([call(writeInt,[X])|Rest], Env),_) :-
	reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, integer); throw(type_mismatch(call(writeInt,[X])))),!,
	write(V),
	reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(writeIntLn,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, integer); throw(type_mismatch(call(writeIntLn,[X])))),!,
    writeln(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(readInt,V)|Rest], Env),_) :-
    read(V),
    (check_type(V, integer); throw(type_mismatch(call(readInt,V)))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), _).
reduce_stmt(config([call(readReal,[])|Rest], Env),_) :-
    read(V),
    (float(V);throw(type_mismatch(call(readReal,[])))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), _).
reduce_stmt(config([call(writeReal,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    write(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(writeRealLn,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    writeln(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(writeBool,[X])|Rest], Env),_) :-    
    reduce_all(config(X,Env),config(V,Env)),
    write(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(writeBoolLn,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    writeln(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(readBool,V)|Rest], Env),_) :-
    read(V),
    (check_type(V, boolean); throw(type_mismatch(call(readBool,V)))),!,
    update_env(V, V, Env, NewEnv),
    reduce_stmt(config(Rest, NewEnv), _).
reduce_stmt(config([call(writeStrLn,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, string); throw(type_mismatch(call(writeStrLn,V)))),!,
    writeln(V),
    reduce_stmt(config(Rest, Env), _).
reduce_stmt(config([call(writeStr,[X])|Rest], Env),_) :-
    reduce_all(config(X,Env),config(V,Env)),
    (check_type(V, string); throw(type_mismatch(call(writeStr,V)))),!,
    write(V),
    reduce_stmt(config(Rest, Env), _).

reduce_stmt(config([assign(I, E1)|Rest], Env),_) :-
	reduce_all(config(E1, Env), config(Rhs, Env)),
	update_env(I, Rhs, Env, NewEnv),
	reduce_stmt(config(Rest, NewEnv), _).



