%main program


reduce_prog([Var,_,Body]) :-
		create_env(Var,env([[]],false),Env),
		reduce_stmt(config(Body,Env),_).
											
%check if X has been declared in the first list of the environment
has_declared(X,[id(X,Y,Z)|_],id(X,Y,Z)):- !.
has_declared(X,[_|L],R) :-  has_declared(X,L,R).

%create a symbol table from the list of variable or constant declarations
create_env([],L,L).
create_env([var(X,Y)|_],env([_],_),_):- is_builtin(X,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|_],env([L1|_],_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|L],env([L1|L2],T),L3):- create_env(L,env([[id(X,var,Y)|L1]|L2],T),L3).

create_env([const(X,Y)|_],env([_],_),_):- is_builtin(X,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|_],env([L1|_],_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|L],env([L1|L2],T),L3):- create_env(L,env([[id(X,const,Y)|L1]|L2],T),L3).

%update env

% Case 1: Variable found in the current scope
update_env(Var, Value, env([Scope|Rest], Status), env([NewScope|Rest], Status)) :-
    has_declared(Var, Scope, id(Var, var, Type)), !, % Check if Var exists
    check_type(Value, Type), % Verify type compatibility
    replace_var(Scope, Var, Value, NewScope). % Update value in scope

% Case 2: Variable not in current scope, search outer scopes
update_env(Var, Value, env([Scope|Rest], Status), env([Scope|NewRest], Status)) :-
    update_env(Var, Value, env(Rest, Status), env(NewRest, Status)).

% Case 3: Variable not found in any scope
update_env(Var, _, env([], _), _) :-
    throw(undeclare_identifier(Var)).

% replace_var(Scope, Var, Value, NewScope)
% Replaces the variable's value in the scope.
replace_var([id(Var, var, _)|T], Var, Value, [id(Var, var, Value)|T]) :- !.
replace_var([H|T], Var, Value, [H|NewT]) :- replace_var(T, Var, Value, NewT).


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
    member(V, [true, false]), !. % Giá trị boolean
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    has_declared(V, Env, id(V, const, Value)), !. % Giá trị hằng
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    has_declared(V, Env, id(V, var, Value)), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    integer(V), !.
reduce_all(config(E, Env), config(E2, Env)) :-
    reduce(config(E, Env), config(E1, Env)), !,
    reduce_all(config(E1, Env), config(E2, Env)).

reduce_stmt(config([call(writeInt,[X])],_),_) :- 
	reduce_all(config(X,Env),config(V,Env)),
	write(V).

reduce_stmt(config([assign(I, E1)], NewEnv),_) :-  
	reduce_all(config(E1, Env), config(Rhs, Env)),
	update_env(I, Rhs, Env, NewEnv).

