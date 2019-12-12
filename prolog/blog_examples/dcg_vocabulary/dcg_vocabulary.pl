%% Vocabulary
list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

%% generic rules, for grammars using atoms

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

%% generic rules, for grammars using "strings"

conc_of(_,    []) --> [].
conc_of(Pred, [List|Lists]) --> list(List), {call(Pred, List, _, _)}, conc_of(Lists).

conc_min1(Pred, [List])       --> list(List), {call(Pred, List, _, _)}.
conc_min1(Pred, [List|Lists]) --> list(List), {call(Pred, List, _, _)}, conc_min1(Pred, Lists).

conc_sep(Pred, _,       [List])       --> list(List), {call(Pred, List, _, _)}.
conc_sep(Pred, SepPred, [List|Lists]) -->
    list(List), {call(Pred, List, _, _)},
    list(Sep),  {call(SepPred, Sep, _, _)},
    conc_sep(Pred, SepPred, Lists).

maybe_str(_,      [])  --> [].
maybe_str(Pred,   [List]) --> list(List), {call(Pred, List, _, _)}.

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

bird1(B)  --> "cuckoo", { string_codes("cuckoo", B) }.
bird2(B)  --> "sparrow", { string_codes("sparrow", B) }.
birdie(B) --> ( bird1(B) | bird2(B) ).
wspace(W) --> {W=32}.
separ(C)  --> (
    list_min1(wspace, C)
  | ",",    {string_codes(",", C)}
  ).
birdies(Bs) --> conc_sep(birdie, separ, Bs).
bird_opt(B) --> maybe_str(birdie, B).

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


% tests for grammars using "strings"

test_conc_sep_1(Bird) :-
    Data="cuckoo",
    string_codes(Data, L),
    phrase((birdies([B])), L),
    string_codes(Bird, B).
%Bird = "cuckoo" ;

test_conc_sep_2(Bird1, Bird2) :-
    Data="cuckoo,sparrow",
    string_codes(Data, L),
    phrase((birdies([B1,B2])), L),
    string_codes(Bird1, B1),
    string_codes(Bird2, B2).
%Bird1 = "cuckoo", Bird2 = "sparrow" ;

test_conc_sep_3(Bird1, Bird2) :-
    Data="cuckoo  sparrow",
    string_codes(Data, L),
    phrase((birdies([B1,B2])), L),
    string_codes(Bird1, B1),
    string_codes(Bird2, B2).
%Bird1 = "cuckoo", Bird2 = "sparrow" ;


test_maybe_str_1(Bird) :-
    Data="sparrow",
    string_codes(Data, L),
    phrase((bird_opt([B])), L),
    string_codes(Bird, B).
%Bird = "sparrow" ;

test_maybe_str_2(Bird) :-
    Data="squirrel",
    string_codes(Data, L),
    phrase((bird_opt([B])), L),
    string_codes(Bird, B).
%false.

test_maybe_str_3(Bird) :-
    Data="",
    string_codes(Data, L),
    phrase((bird_opt(B)), L),
    string_codes(Bird, B).
%Bird = ""
