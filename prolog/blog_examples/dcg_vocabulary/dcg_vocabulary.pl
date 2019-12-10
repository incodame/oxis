:- use_module(library(dcg/basics)).

%% Vocabulary

% a generic list_of for dcgs
list_of(_,    [])     --> [].
list_of(Pred, [L|Ls]) --> [L], {call(Pred, L, _, _)}, list_of(Pred, Ls).

% a non empty list
list_min1(Pred, [L])    --> [L], {call(Pred, L, _, _)}.
list_min1(Pred, [L|Ls]) --> [L], {call(Pred, L, _, _)}, list_min1(Pred, Ls).
 
% a non empty list with separator
list_sep(Pred, _,   [L])        --> [L], {call(Pred, L, _, _)}.
list_sep(Pred, Sep, [L,Sep|Ls]) --> [L], {call(Pred, L, _, _)}, [Sep], list_sep(Pred, Sep, Ls).

% an optional object
maybe(_,      [])  --> [].
maybe(Pred,   [L]) --> [L], {call(Pred, L, _, _)}.

%% Sample use in grammars

hat(H) --> {H='hat'}.
hut(H) --> {H='hut'}.
chapeau(H) --> (
    hat(H)
  | hut(H)
  ).

chapeaux(Cs) --> list_of(chapeau, Cs).
chap_min(Cs) --> list_min1(chapeau, Cs).
chap_sep(Cs) --> list_sep(chapeau, ',', Cs).
chap_opt(C)  --> maybe(chapeau, C).

%% Tests

test_list_of_1(Cs) :-
    phrase((chapeaux(Cs)), [hat]).
%Cs = [hat] ;

test_list_of_2(Cs) :-
    phrase((chapeaux(Cs)), [hat, hut]).
%Cs = [hat, hut] ;

test_list_of_3(Cs) :-
    phrase((chapeaux(Cs)), [hat, hot]).
%false.

test_list_of_4(Cs) :-
    phrase((chapeaux(Cs)), []).
%Cs = [] ;

test_list_min1_1(Cs) :-
    phrase((chap_min(Cs)), [hat]).
%Cs = [hat] ;
test_list_min1_2(Cs) :-
    phrase((chap_min(Cs)), [hat, hut]).
%Cs = [hat, hut] ;

test_list_min1_3(Cs) :-
    phrase((chap_min(Cs)), [hat, hot]).
%false.

test_list_min1_4(Cs) :-
    phrase((chap_min(Cs)), []).
%false.

test_list_sep_1(Cs) :-
    phrase((chap_sep(Cs)), [hat]).
%Cs = [hat] ;

test_list_sep_2(Cs) :-
    phrase((chap_sep(Cs)), [hat, ',', hut]).
%Cs = [hat, ',', hut] ;

test_list_sep_3(Cs) :-
    phrase((chap_sep(Cs)), [hat, ',', hot]).
%false.

test_list_sep_4(Cs) :-
    phrase((chap_sep(Cs)), [hat, hot]).
%false.

test_list_sep_5(Cs) :-
    phrase((chap_sep(Cs)), []).
%false.

test_maybe_1(C) :-
    phrase((chap_opt(C)), [hat]).
%Cs = [hat] ;

test_maybe_2(C) :-
    phrase((chap_opt(C)), [hat, hut]).
%false.

test_maybe_3(C) :-
    phrase((chap_opt(C)), [hat, hot]).
%false.

test_maybe_4(C) :-
    phrase((chap_opt(C)), []).
%C = [] ;
