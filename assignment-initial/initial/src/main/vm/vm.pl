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
check_type(Value, integer) :- 
	integer(Value), !.
check_type(Value, boolean) :-
	boolean(Value), !.
check_type(Value, float) :-
	float(Value), !.
check_type(Value, string) :-
	string(Value), !.
check_type(Value, _) :-
	throw(type_mismatch(Value)).

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
reduce(config(rdiv(E1,E2),Env),config(R,Env)) :-  
		reduce_all(config(E1,Env),config(V1,Env)),
		reduce_all(config(E2,Env),config(V2,Env)),
		R is V1/V2.

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
reduce_all(config(E, Env), config(E2, Env)) :-
    reduce(config(E, Env), config(E1, Env)), !,
    reduce_all(config(E1, Env), config(E2, Env)).
reduce_all(config(V, _), _) :-
	atom(V),
	throw(undeclare_identifier(V)).

reduce_stmt(config([call(writeInt,[X])|Rest], Env),_) :-
	reduce_all(config(X,Env),config(V,Env)),
	write(V),
	reduce_stmt(config(Rest, Env), _).

reduce_stmt(config([assign(I, E1)|Rest], Env),_) :-
	reduce_all(config(E1, Env), config(Rhs, Env)),
	update_env(I, Rhs, Env, NewEnv),
	reduce_stmt(config(Rest, NewEnv), _).

