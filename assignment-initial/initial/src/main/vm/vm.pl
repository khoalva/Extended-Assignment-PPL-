


% 
% @author: 2211605
%

%main program

%env(Scopes, Status, Funcs)
reduce_prog([Var,Func,Body]) :-
		create_env(Var,env([[]],false,[]),Env),
        create_env(Func, Env, Env2),
		reduce_stmt(config(Body,Env2),_).
											
%check if X has been declared in the first list of the environment
has_declared(_, [], _) :- fail.
has_declared(X,[id(X,Y,Z,Value)|_],id(X,Y,Z,Value)) :- !.
has_declared(X,[_|L],R) :-  has_declared(X,L,R).

%check if X has been declared in the environment
check_declared(X, env([], _, _), _) :- throw(undeclare_identifier(X)).
check_declared(X, env([L|_], _, _), Y) :- has_declared(X,L,Y), !.
check_declared(X, env([_|L], _, _), Y) :- check_declared(X, env(L, _, _), Y).

%create a symbol table from the list of variable or constant declarations
create_env([],L,L).
create_env([var(X,Y)|_],env([_],_,_),_):- is_builtin(X,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|_],env([L1|_],_,_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(var(X,Y))).
create_env([var(X,Y)|L],env([L1|L2],T,F),L3):- create_env(L,env([[id(X,var,Y,undefined)|L1]|L2],T,F),L3).

create_env([const(X,Y)|_],env([_],_,_),_):- is_builtin(X,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|_],env([L1|_],_,_),_):-has_declared(X,L1,_),!,throw(redeclare_identifier(const(X,Y))).
create_env([const(X,Y)|L],env([L1|L2],T,F),L3):- create_env(L,env([[id(X,const,Y,undefined)|L1]|L2],T,F),L3).

% Handle parameter declarations
create_env([par(X,Y)|_], env([_], _, _), _) :- 
    is_builtin(X,_), !, 
    throw(redeclare_identifier(par(X,Y))).
create_env([par(X,Y)|_], env([L1|_], _, _), _) :- 
    has_declared(X,L1,_), !, 
    throw(redeclare_identifier(par(X,Y))).
create_env([par(X,Y)|L], env([L1|L2], T, F), L3) :- 
    create_env(L, env([[id(X,par,Y,undefined)|L1]|L2], T, F), L3).

% Update create_env to handle function declarations with parameter and body validation
create_env([func(Name, Params, RetType, Body)|Rest], env(Scopes, Status, Funcs), env(Scopes, Status, NewFuncs)) :-
    % Check function name not already declared
    (member(func(Name, _, _, _), Funcs); member(proc(Name, _, _), Funcs)) ->
        throw(redeclare_function(Name))
    ;

        % Create temp environment with parameters as innermost scope, preserving globals
        create_env(Params, env([[]|Scopes],Status,Funcs), _),
        % Validate function body (should add return type check)
        % validate_function_body(Body, RetType, ParamEnv),
        % Add function to environment and continue
        create_env(Rest, env(Scopes, Status, [func(Name, Params, RetType, Body)|Funcs]), env(Scopes, Status, NewFuncs)).

% Update create_env to handle procedure declarations with parameter and body validation
create_env([proc(Name, Params, Body)|Rest], env(Scopes, Status, Funcs), env(Scopes, Status, NewFuncs)) :-
    % Check procedure name not already declared
    (member(func(Name, _, _, _), Funcs); member(proc(Name, _, _), Funcs)) ->
        throw(redeclare_procedure(Name))
    ;
        % Create temp environment with parameters as innermost scope, preserving globals
        create_env(Params, env([[]|Scopes],Status,Funcs), _),
        % Validate procedure body
        % validate_procedure_body(Body, ParamEnv),
        % Add procedure to environment and continue
        create_env(Rest, env(Scopes, Status, [proc(Name, Params, Body)|Funcs]), env(Scopes, Status, NewFuncs)).

% Helper: merge_env_after_block(+BlockEnv, +OuterEnv, -MergedEnv)
% BlockEnv = env([BlockScope|OuterScopes], Status)
% OuterEnv = env([OuterScope|OuterScopes], Status)
% MergedEnv = env([NewOuterScope|OuterScopes], Status)
merge_env_after_block(env([_BlockScope|OuterScopes], Status, Funcs), 
                     env([OuterScope|OuterScopes], Status, Funcs), 
                     env([NewOuterScope|OuterScopes], Status, Funcs)) :-
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


% determine_status(+Result, +Default, -Status)
% Result: config(X, _)
% Default: atom representing the current statement (e.g., if, block, while, ...)
% Status: break, continue, or Default


determine_status(break(null), _, break(null)) :- !.
determine_status(continue(null), _, continue(null)) :- !.
determine_status(end, _, end) :- !.
determine_status(_, Default, Default).

% handle_control(+Status1, +Rest, +Env1, -Status, -Env2)
% If Status1 is break/continue, propagate it. Otherwise, continue reducing Rest.
handle_control(break(null), _, Env1, _, config(break(null), Env1)) :- !.
handle_control(continue(null), _, Env1, _, config(continue(null), Env1)) :- !.
handle_control(_, Rest, Env1, Kind, config(Status, Env2)) :-
    (Rest = [] -> Status = Kind, Env2 = Env1;
    reduce_stmt(config(Rest, Env1), config(Status1, Env2)),
    determine_status(Status1, Kind, Status)
    ).
boolean(true).
boolean(false).

% check type
check_type(Value, integer) :- integer(Value), !.
check_type(Value, float) :- float(Value), !.
check_type(Value, string) :- string(Value), !.
check_type(Value, boolean) :- boolean(Value), !.
check_type(undefined, _) :- true, !.
check_type(_, _) :- fail.
% update env
% Case 1: Variable found in the current scope
update_env(Var, Value, env([Scope|Rest], Status, Funcs), env([NewScope|Rest], Status, Funcs)) :-
    has_declared(Var, Scope, id(Var, Kind, Type, _)),!, % Check if Var exists
    check_type(Value, Type), % Verify type compatibility
    replace_var(Scope, id(Var, Kind, Type, Value), NewScope). % Update value in scope

% Case 2: Variable not in current scope, search outer scopes
update_env(Var, Value, env([Scope|Rest], Status, Funcs), env([Scope|NewRest], Status, Funcs)) :-
    update_env(Var, Value, env(Rest, Status, Funcs), env(NewRest, Status, Funcs)).

% Case 3: Variable not found in any scope
update_env(Var, _, env([], _, _), _) :-
    throw(undeclare_identifier(Var)).

% replace_var(Scope, Var, Value, NewScope)
% Replaces the variable's value in the scope.
replace_var([id(Var, Kind, _, _)|T], id(Var, Kind, Type, Value), [id(Var, Kind, Type, Value)|T]) :- !.
replace_var([H|T], id(Var, Kind, Type, Value), [H|NewT]) :- replace_var(T, id(Var, Kind, Type, Value), NewT).


reduce(config(sub(E1), Env), config(R,NewEnv)) :-  
		reduce_all(config(E1,Env),config(V1,NewEnv)),
        check_type(V1, T1),
        (   (T1 \= integer, T1 \= float) ->
        throw(type_mismatch(sub(E1)))
        ;   true
        ),
		R is -V1.
	
reduce(config(add(E1,E2),Env),config(R,NewEnv)) :-
    reduce_all(config(E1,Env),config(V1,Env1)),
    reduce_all(config(E2,Env1),config(V2,NewEnv)),
    check_type(V1, T1),
    check_type(V2, T2),
    % Kiểm tra cả hai toán hạng phải là integer hoặc float
    (   (T1 \= integer, T1 \= float, T2 \= integer, T2 \= float) ->
        throw(type_mismatch(add(E1,E2)))
    ;   true
    ),

    % Kiểm tra kiểu không khớp (cho phép integer và float kết hợp)
    (   T1 \= T2,
        (T1 = integer, T2 = float ; T1 = float, T2 = integer) -> true
    ;   T1 \= T2 -> throw(type_mismatch(add(E1,E2)))
    ;   true
    ),

    % Tính R và quyết định kiểu của R
    (   (T1 = float ; T2 = float) ->
        R is float(V1 + V2)  % Trả về float nếu có ít nhất một float
    ;   R is V1 + V2        % Trả về integer nếu cả hai là integer
    ).


reduce(config(sub(E1,E2),Env),config(R,NewEnv)) :-
    reduce_all(config(E1,Env),config(V1,Env1)),
    reduce_all(config(E2,Env1),config(V2,NewEnv)),
    check_type(V1, T1),
    check_type(V2, T2),
    % Kiểm tra cả hai toán hạng phải là integer hoặc float
    (   (T1 \= integer, T1 \= float, T2 \= integer, T2 \= float) ->
        throw(type_mismatch(sub(E1,E2)))
    ;   true
    ),
    % Kiểm tra kiểu không khớp (cho phép integer và float kết hợp)
    (   T1 \= T2,
        (T1 = integer, T2 = float ; T1 = float, T2 = integer) -> true
    ;   T1 \= T2 -> throw(type_mismatch(sub(E1,E2)))
    ;   true
    ),
    % Tính R và quyết định kiểu của R
    (   (T1 = float ; T2 = float) ->
        R is float(V1 - V2)  % Trả về float nếu có ít nhất một float
    ;   R is V1 - V2        % Trả về integer nếu cả hai là integer
    ).
reduce(config(times(E1,E2),Env),config(R,NewEnv)) :-
    reduce_all(config(E1,Env),config(V1,Env1)),
    reduce_all(config(E2,Env1),config(V2,NewEnv)),
    check_type(V1, T1),
    check_type(V2, T2),
    % Kiểm tra cả hai toán hạng phải là integer hoặc float
    (   (T1 \= integer, T1 \= float, T2 \= integer, T2 \= float) ->
        throw(type_mismatch(times(E1,E2)))
    ;   true
    ),
    % Kiểm tra kiểu không khớp (cho phép integer và float kết hợp)
    (   T1 \= T2,
        (T1 = integer, T2 = float ; T1 = float, T2 = integer) -> true
    ;   T1 \= T2 -> throw(type_mismatch(times(E1,E2)))
    ;   true
    ),
    % Tính R và quyết định kiểu của R
    (   (T1 = float ; T2 = float) ->
        R is float(V1 * V2)  % Trả về float nếu có ít nhất một float
    ;   R is V1 * V2        % Trả về integer nếu cả hai là integer
    ).
% reduce for rdiv(E1, E2)
reduce(config(rdiv(E1, E2), Env), config(R, NewEnv)) :- 

    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    
    % Kiểm tra kiểu của V1, V2
    check_type(V1, T1),
    check_type(V2, T2),
    % Kiểm tra cả hai toán hạng phải là integer hoặc float
    (   (T1 \= integer, T1 \= float, T2 \= integer, T2 \= float) ->
        throw(type_mismatch(rdiv(E1,E2)))
    ;   true
    ),
    % Kiểm tra kiểu không khớp (cho phép integer và float kết hợp)
    (   T1 \= T2,
        (T1 = integer, T2 = float ; T1 = float, T2 = integer) -> true
    ;   T1 \= T2 -> throw(type_mismatch(rdiv(E1,E2)))
    ;   true
    ),
    % Tính R và quyết định kiểu của R
    (   (T1 = float ; T2 = float) ->
        R is float(V1 / V2)  % Trả về float nếu có ít nhất một float
    ;   R is V1 / V2        % Trả về integer nếu cả hai là integer
    ).


% reduce for idiv(E1, E2)
reduce(config(idiv(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    % Kiểm tra kiểu của V1, V2
    check_type(V1, T1),
    check_type(V2, T2),

    % Kiểm tra cả hai toán hạng phải là integer
    (   (T1 \= integer; T2 \= integer) ->
        throw(type_mismatch(idiv(E1,E2)))
    ;   true
    ),
    % Tính kết quả
    R is V1 // V2.  % Phép chia lấy nguyên


% reduce for imod(E1, E2)
reduce(config(imod(E1, E2), Env), config(R, NewEnv)) :- 

    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    % Kiểm tra E2 \= 0
    V2 \= 0, !,
    % Kiểm tra kiểu của V1, V2
    check_type(V1, T1),
    check_type(V2, T2),

    % Kiểm tra cả hai toán hạng phải là integer
    (   (T1 \= integer; T2 \= integer) ->
        throw(type_mismatch(imod(E1,E2)))
    ;   true
    ),
    % Tính kết quả
    R is V1 mod V2.  % Phép chia lấy dư

% reduce for bnot(E1)
reduce(config(bnot(E1), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, NewEnv)),
    check_type(V1, boolean),
    (V1 = true -> R = false; R = true),!.
reduce(config(bnot(E1), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, _)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(bnot(E1))).
reduce(config(band(E1, _), Env), config(false, NewEnv)) :- 

    reduce_all(config(E1, Env), config(V1, TempEnv)),

    check_type(V1, boolean),
    V1 = false,
    NewEnv = TempEnv, !.

reduce(config(band(E1, E2), Env), config(R, NewEnv)) :- 

    reduce_all(config(E1, Env), config(V1, Env1)),

    check_type(V1, boolean),
    V1 = true,
    reduce_all(config(E2, Env1), config(V2, TempEnv)),

    check_type(V2, boolean),
    (   V2 = true -> R = true ; R = false ),
    NewEnv = TempEnv.

reduce(config(band(E1, E2), Env), _) :- 

    reduce_all(config(E1, Env), config(V1, _)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(band(E1, E2))).

% reduce for bor(E1, E2) with short-circuit evaluation
reduce(config(bor(E1, _), Env), config(true, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, NewEnv)),
    check_type(V1, boolean),
    V1 = true, !.  % Nếu E1 = true, không đánh giá E2
reduce(config(bor(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    check_type(V1, boolean),
    V1 = false,
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    check_type(V2, boolean),
    (   V2 = true -> R = true ; R = false ).
reduce(config(bor(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, _)),
    \+ check_type(V1, boolean),
    throw(type_mismatch(bor(E1, E2))).

% reduce for greater(E1, E2)
reduce(config(greater(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 > V2 -> R = true ; R = false ).
reduce(config(greater(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(greater(E1, E2))).

% reduce for less(E1, E2)
reduce(config(less(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 < V2 -> R = true ; R = false ).
reduce(config(less(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(less(E1, E2))).

% reduce for ge(E1, E2) (greater or equal)
reduce(config(ge(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 >= V2 -> R = true ; R = false ).
reduce(config(ge(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(ge(E1, E2))).

% reduce for le(E1, E2) (less or equal)
reduce(config(le(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    (   check_type(V1, float); check_type(V1, integer) ),
    (   check_type(V2, float); check_type(V2, integer) ),
    (   V1 =< V2 -> R = true ; R = false ).
reduce(config(le(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    (   \+ (check_type(V1, float); check_type(V1, integer))
    ;   \+ (check_type(V2, float); check_type(V2, integer))
    ),
    throw(type_mismatch(le(E1, E2))).

% reduce for ne(E1, E2) (not equal)
reduce(config(ne(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    % Kiểm tra cả hai cùng kiểu
    (   ( check_type(V1, integer), check_type(V2, integer) )
    ;   ( check_type(V1, boolean), check_type(V2, boolean) )
    ),
    (   V1 \= V2 -> R = true ; R = false ).
reduce(config(ne(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    \+ (   ( check_type(V1, integer), check_type(V2, integer) )
       ;   ( check_type(V1, boolean), check_type(V2, boolean) )
       ),
    throw(type_mismatch(ne(E1, E2))).

% reduce for eql(E1, E2) (equal)
reduce(config(eql(E1, E2), Env), config(R, NewEnv)) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, NewEnv)),
    % Kiểm tra cả hai cùng kiểu
    (   ( check_type(V1, integer), check_type(V2, integer) )
    ;   ( check_type(V1, boolean), check_type(V2, boolean) )
    ),
    (   V1 = V2 -> R = true ; R = false ).
reduce(config(eql(E1, E2), Env), _) :- 
    reduce_all(config(E1, Env), config(V1, Env1)),
    reduce_all(config(E2, Env1), config(V2, _)),
    \+ (   ( check_type(V1, integer), check_type(V2, integer) )
       ;   ( check_type(V1, boolean), check_type(V2, boolean) )
       ),
    throw(type_mismatch(eql(E1, E2))).

% Function call expression - evaluate function and get return value
reduce(config(call(Name, Args), Env), config(ReturnValue, NewEnv)) :-

    Env = env(Scopes, Status, Funcs),
    % Check if function exists and handle appropriately
    (member(func(Name, Params, RetType, Body), Funcs) -> 
        % User-defined function
        
        handle_user_function(Name, Args, Params, RetType, Body, env(Scopes, Status, Funcs), ReturnValue,NewEnv)

    ;
        % Try built-in function
        (is_builtin(Name, func) -> 
            handle_builtin_function(Name, Args, env(Scopes, Status, Funcs), ReturnValue,NewEnv)
        ;
            % Neither user-defined nor built-in
            throw(undeclare_function(Name))
        )
    ).

% Helper to check argument types against parameter types
check_argument_types([], [], [], _, _).
check_argument_types([ArgValue|ArgValues], [par(_, Type)|Params], [_|OrigArgs], FuncName, AllArgs) :-
    % Check if argument value matches parameter type
    (check_type(ArgValue, Type) -> 
        true
    ;
        throw(type_mismatch(call(FuncName, AllArgs)))
    ),
    check_argument_types(ArgValues, Params, OrigArgs, FuncName, AllArgs).


% Handle user-defined function execution
handle_user_function(Name, Args, Params, RetType, Body, Env, ReturnValue,ResultEnv) :-
    % Validate argument count
    length(Args, ArgCount),
    length(Params, ParamCount),
    (ArgCount =:= ParamCount -> true ; throw(wrong_number_of_argument(call(Name, Args)))),

    % In handle_user_function:
    evaluate_args_with_originals(Args, Env, ArgValues, OrigArgs,NewEnv),

    % Pass the original Args list to check_argument_types
    check_argument_types(ArgValues, Params, OrigArgs, Name, Args),
    % Create new environment with parameters as innermost scope
    create_function_env(ArgValues, Params, NewEnv, env(NewScopes, NewStatus, NewFuncs)),
    
    NewScopes = [L1|L2],
    FuncEnv = env([[id(Name, var, RetType, undefined)|L1]|L2], NewStatus, NewFuncs), 

    % Execute function body
    reduce_stmt(config(Body, FuncEnv), config(_, TempEnv)),

    % Extract the return value - function name is used as the return variable
    check_declared(Name, TempEnv, id(Name, _, RetType, ReturnValue)),

    % Verify return value matches declared return type
    check_type(ReturnValue, RetType),
    
    TempEnv = env([_|T2], TemStatus, TemFuncs),

    ResultEnv = env(T2, TemStatus, TemFuncs)
    .


% Handle built-in function execution
handle_builtin_function(Name, Args, Env, ReturnValue,NewEnv) :-
    
    % Evaluate arguments while preserving originals for error messages
    evaluate_args_with_originals(Args, Env, ArgValues, OrigArgs,NewEnv),
    % Call appropriate built-in function
    execute_builtin_func(Name, ArgValues, OrigArgs, ReturnValue).

% Execute built-in functions
execute_builtin_func(readInt, [], _, Value) :- 
    read(Value), 
    (integer(Value); throw(type_mismatch(call(readInt,[])))), !.
    
execute_builtin_func(readReal, [], _, Value) :- 
    read(Value), 
    (float(Value); throw(type_mismatch(call(readReal,[])))), !.
    
execute_builtin_func(readBool, [], _, Value) :- 
    read(Value), 
    (boolean(Value); throw(type_mismatch(call(readBool,[])))), !.


reduce_all(config(V, Env), config(V, Env)) :- 
    boolean(V), !. % Giá trị boolean
reduce_all(config(V, Env), config(V, Env)) :- 
    integer(V), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    float(V), !.
reduce_all(config(V, Env), config(V, Env)) :- 
    string(V), !.
reduce_all(config(V, Env), config(V, Env)) :-
    V = undefined, !.
reduce_all(config(E, Env), config(E2, Env2)) :-
    reduce(config(E, Env), config(E1, Env1)), !,
    reduce_all(config(E1, Env1), config(E2, Env2)).
reduce_all(config(V, Env), config(Value, Env)) :- 
    atom(V), 
    check_declared(V, Env, id(V, _, _, Value)), !. % Giá trị hằng
reduce_all(config(V, _), _) :-
	atom(V),
	throw(undeclare_identifier(V)).

reduce_stmt(config([], Env), config(end, Env)) :- !.

reduce_stmt(config([assign(I, E1)|Rest], Env), config(Status, Env2)) :- 

    check_declared(I, Env, id(I, Kind, Type, _)),
    (Kind = const -> 
        throw(cannot_assign(assign(I, E1)))
    ;
        reduce_all(config(E1, Env), config(Rhs, Env1)),
        (Rhs = undefined -> 
            throw(invalid_expression(E1))
        ;
            (check_type(Rhs, Type) -> 
                true
            ;   
                throw(type_mismatch(assign(I, E1)))
            ),
            update_env(I, Rhs, Env1, NewEnv),

            reduce_stmt(config(Rest, NewEnv), config(TempStatus, TempEnv)),

            determine_status(TempStatus, assign(I, E1), Status),
            Env2 = TempEnv
        )
    ).

reduce_stmt(config([block(L1, L2)|Rest], Env), config(ReturnStatus, Env2)) :-
    (   L1 = [] ->
        % No new scope, just execute in current env and propagate all changes
        reduce_stmt(config(L2, Env), config(NewStatus, BlockEnv1)),
        determine_status(NewStatus, block(L1,L2), Status1),
        handle_control(Status1, Rest, BlockEnv1, block(L1,L2), config(ReturnStatus, Env2))
    ;   % New scope created
        Env = env(Scopes, Status, Funcs),
        create_env(L1, env([[]|Scopes], Status, Funcs), BlockEnv),
        reduce_stmt(config(L2, BlockEnv), config(NewStatus, BlockEnv1)),
        merge_env_after_block(BlockEnv1, env([[]|Scopes], Status, Funcs), MergedEnv),
        determine_status(NewStatus, block(L1,L2), Status1),
        handle_control(Status1, Rest, MergedEnv, block(L1,L2), config(ReturnStatus, Env2))
    ).

reduce_stmt(config([if(E, S1, S2)|Rest], Env),config(Status,Env2)) :-


    reduce_all(config(E, Env), config(V1, Env1)),

    (check_type(V1, boolean); throw(type_mismatch(if(E, S1, S2)))),

    % Only put the cut after we've verified the conditional value
    ((V1 = true -> reduce_stmt(config([S1], Env1), config(NewStatus,NewEnv))
              ; V1 = false -> reduce_stmt(config([S2], Env1), config(NewStatus,NewEnv))
              ; throw(type_mismatch(if(E, S1, S2))))),!,
    determine_status(NewStatus, if(E,S1,S2), Status1),
    handle_control(Status1, Rest, NewEnv, if(E,S1,S2), config(Status, Env2)).

reduce_stmt(config([if(E, S1)|Rest], Env), config(Status, Env2)) :-
    reduce_all(config(E, Env), config(V1, Env1)),
    (check_type(V1, boolean); throw(type_mismatch(if(E, S1)))),!,
    (V1 = true -> reduce_stmt(config([S1], Env1), config(NewStatus, NewEnv)), 
                  determine_status(NewStatus, if(E,S1), Status1)
                  ; Status1 = if(E,S1), NewEnv = Env1),
    
    handle_control(Status1, Rest, NewEnv, if(E,S1), config(Status, Env2)).

% reduce_stmt for while(E, S) with break/continue handling
reduce_stmt(config([while(E, S)|Rest], Env), config(while(E,S),Env2)) :- 
    Env = env(Scopes, PrevStatus, Funcs),
    EnvInLoop = env(Scopes, true, Funcs),  % Đánh dấu đang trong vòng lặp
    reduce_all(config(E, EnvInLoop), config(V, Env3)),
    (check_type(V, boolean); throw(type_mismatch(while(E, S)))),!,
    (   V = true
    ->  reduce_stmt(config([S], Env3), Result),
        (   Result = config(break(null), Env1)
        ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
        ;   Result = config(continue(null), Env1)
        ->  reduce_stmt(config([while(E, S)], Env1), config(while(E,S), NewEnv))  % Bỏ qua phần còn lại, tiếp tục vòng lặp
        ;   Result = config(_, Env1)
        ->  reduce_stmt(config([while(E, S)], Env1), config(while(E,S), NewEnv))
        )
    ;   V = false,
        NewEnv = Env3
    ),
    NewEnv = env(Scopes2, _, Funcs2),
    EnvAfterLoop = env(Scopes2, PrevStatus, Funcs2),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% reduce_stmt for do(L, E) with break/continue handling
reduce_stmt(config([do(L, E)|Rest], Env), config(do, Env2)) :- 
    Env = env(Scopes, PrevStatus, Funcs),
    EnvInLoop = env(Scopes, true, Funcs),  % Đánh dấu đang trong vòng lặp
    reduce_stmt(config(L, EnvInLoop), Result),
    (   Result = config(break(null), Env1)
    ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
    ;   Result = config(continue(null), Env1)
    ->  reduce_all(config(E, Env1), config(V, Env3)),
        (check_type(V, boolean); throw(type_mismatch(do(L, E)))),!,
        (   V = true
        ->  reduce_stmt(config([do(L, E)], Env3), config(_, NewEnv))
        ;   V = false,
            NewEnv = Env3
        )
    ;   Result = config(_, Env3)
    ->  reduce_all(config(E, Env1), config(V, Env3)),
        (check_type(V, boolean); throw(type_mismatch(do(L, E)))),!,
        (   V = true
        ->  reduce_stmt(config([do(L, E)], Env1), config(_, NewEnv))
        ;   V = false,
            NewEnv = Env3
        )
    ),
    NewEnv = env(Scopes2, _, Funcs2),
    EnvAfterLoop = env(Scopes2, PrevStatus, Funcs2),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% reduce_stmt for loop(E, S) with break/continue handling
reduce_stmt(config([loop(E, S)|Rest], Env), config(loop(E,S),Env2)) :- 
    Env = env(Scopes, PrevStatus, Funcs),
    EnvInLoop = env(Scopes, true, Funcs),  % Đánh dấu đang trong vòng lặp
    reduce_all(config(E, EnvInLoop), config(N, EnvInLoop2)),
    (check_type(N, integer); throw(type_mismatch(loop(E,S)))),!,
    N >= 0,  % Đảm bảo số lần lặp không âm
    reduce_loop_with_ctrl(N, S, EnvInLoop2, NewEnv),
    NewEnv = env(Scopes2, _, Funcs2),
    EnvAfterLoop = env(Scopes2, PrevStatus, Funcs2),
    reduce_stmt(config(Rest, EnvAfterLoop), config(_,Env2)).


% Handle break/continue with exception if not in loop
reduce_stmt(config([break(null)|_], env(_, false,_)), _) :-
    throw(break_not_in_loop(break(null))).
reduce_stmt(config([continue(null)|_], env(_, false,_)), _) :-
    throw(continue_not_in_loop(continue(null))).

reduce_stmt(config([break(null)|_], env(Scopes, true, Funcs)), config(break(null), env(Scopes, true, Funcs))) :- !.
reduce_stmt(config([continue(null)|_], env(Scopes, true, Funcs)), config(continue(null), env(Scopes, true, Funcs))) :- !.
% Procedure call statement - split into built-in and user-defined procedures
reduce_stmt(config([call(Name, Args)|Rest], env(Scopes, Status, Funcs)), config(OutStatus, Env2)) :-
    % Case 1: Built-in procedure
    (is_builtin(Name, proc) -> 
        % Evaluate arguments in current environment
        evaluate_args_with_originals(Args, env(Scopes, Status, Funcs), ArgValues, OrigArgs,Env1),
        % Execute built-in procedure with both values and original expressions
        execute_builtin_proc(Name, ArgValues, OrigArgs, Env1, NewEnv),
        % Continue with rest
        reduce_stmt(config(Rest, NewEnv), config(NewStatus, Env2)),
        determine_status(NewStatus, call(Name, Args), OutStatus)
    % Case 2: User-defined procedure
    ; member(proc(Name, Params, Body), Funcs) ->
        % Validate argument count
        length(Args, ArgCount),
        length(Params, ParamCount),

        (ArgCount =:= ParamCount -> true ; throw(wrong_number_of_argument(call(Name, Args)))),
        
        % Evaluate arguments in current environment
        evaluate_args_with_originals(Args, env(Scopes, Status, Funcs), ArgValues, _,Env1),
        
        % Create new environment with parameters as innermost scope
        % Preserves function list but creates new parameter scope
        create_function_env(ArgValues, Params, Env1, ProcEnv),
 
        % Execute procedure body
        reduce_stmt(config(Body, ProcEnv), config(_, NewEnv)),
  
        % Continue with rest (procedures don't return values)
        reduce_stmt(config(Rest, NewEnv), config(NewStatus, Env2)),
        determine_status(NewStatus, call(Name, Args), OutStatus)
    % Neither built-in nor user-defined
    ; throw(undeclare_procedure(Name))
    ).

% create_function_env(+ArgValues, +Params, +Env, -ProcEnv)
% Creates environment with parameters in innermost scope, preserving outer scopes
create_function_env(ArgValues, Params, env(Scopes, _, Funcs), env([ParamScope|Scopes], false, Funcs)) :-
    % Create parameter scope by binding parameter names to argument values
    create_param_bindings(ArgValues, Params, ParamScope).

% Helper to create parameter bindings
create_param_bindings([], [], []).
create_param_bindings([Value|Values], [par(Name, Type)|Params], [id(Name, var, Type, Value)|Bindings]) :-
    create_param_bindings(Values, Params, Bindings).

% Helper to evaluate args while keeping originals
evaluate_args_with_originals([], Env, [], [],Env).
evaluate_args_with_originals([Arg|Args], Env, [Value|Values], [Arg|Origs],NewEnv) :-
    reduce_all(config(Arg, Env), config(Value, Env1)),
    evaluate_args_with_originals(Args, Env1, Values, Origs,NewEnv).

% Updated builtin execution
execute_builtin_proc(Name, ArgValues, OrigArgs, Env, NewEnv) :-
    execute_proc(Name, ArgValues, OrigArgs, Env, NewEnv).

% Individual procedure implementations with originals
execute_proc(writeInt, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, integer); throw(type_mismatch(call(writeInt,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    write(Value).

execute_proc(writeIntLn, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, integer); throw(type_mismatch(call(writeIntLn,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    writeln(Value).

% These procedures need the original expressions for error messages
execute_proc(writeReal, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, float); throw(type_mismatch(call(writeReal,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    write(Value).

execute_proc(writeRealLn, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, float); throw(type_mismatch(call(writeRealLn,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    writeln(Value).

execute_proc(writeBool, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, boolean); throw(type_mismatch(call(writeBool,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    write(Value).

execute_proc(writeBoolLn, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, boolean); throw(type_mismatch(call(writeBoolLn,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    writeln(Value).

execute_proc(writeStr, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, string); throw(type_mismatch(call(writeStr,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    write(Value).

execute_proc(writeStrLn, [Value], [OrigExpr], Env, Env) :-
    (check_type(Value, string); throw(type_mismatch(call(writeStrLn,[OrigExpr])))),!,
    (Value = undefined -> throw(invalid_expression(OrigExpr)); true),
    writeln(Value).
% Individual procedure implementations with originals
execute_proc(writeLn, _, _, Env, Env) :-
    nl,!.

% Helper predicate for loop with break/continue
reduce_loop_with_ctrl(0, _, Env, Env) :- !.
reduce_loop_with_ctrl(N, S, Env, NewEnv) :-
    N > 0,
    reduce_stmt(config([S], Env), Result),
    (   Result = config(break(null), Env1)
    ->  NewEnv = Env1  % Thoát vòng lặp khi gặp break
    ;   Result = config(continue(null), Env1)
    ->  N1 is N - 1,
        reduce_loop_with_ctrl(N1, S, Env1, NewEnv)  % Bỏ qua phần còn lại, tiếp tục vòng lặp
    ;   Result = config(_, Env1)
    ->  N1 is N - 1,
        reduce_loop_with_ctrl(N1, S, Env1, NewEnv)
    ).


