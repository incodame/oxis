%% Grammar

% a non empty list
list_min1(Pred, [L])    --> [L], {call(Pred, L, _, _)}.
list_min1(Pred, [L|Ls]) --> [L], {call(Pred, L, _, _)}, list_min1(Pred, Ls).

% an optional object
maybe(_,      [])  --> [].
maybe(Pred,   [L]) --> [L], {call(Pred, L, _, _)}.

dot(C) --> {C=46}.

digit(D) --> {D=48;D=49;D=50;D=51;D=52;D=53;D=54;D=55;D=56;D=57}.

wspace(W) --> {W=32;W=9}.

separ(C)  --> list_min1(wspace, C).

clock_tag  --> "CLOCK:".

yyyy_mm_dd(Date) -->
    list_min1(digit, YyyyL), "-",
    list_min1(digit, MmL), "-",
    list_min1(digit, DdL),
    { length(YyyyL, 4),  length(MmL, 2), length(DdL, 2),
      number_codes(Yyyy, YyyyL), number_codes(Mm, MmL), number_codes(Dd, DdL),
      Date = Yyyy-Mm-Dd }.

day_of_week --> ("Mo" ; "Di" ; "Mi" ; "Do" ; "Fr" ; "Sa"; "So").

time_hh24_mi(Time) -->
    list_min1(digit, Hh24L), ":",
    list_min1(digit, MiL),
    { length(Hh24L, 2), length(MiL, 2),
      number_codes(Hh24, Hh24L), number_codes(Mi, MiL),
      Time = Hh24:Mi:0 }.

timestamp([Date, Time]) -->
    "[",
    yyyy_mm_dd(Date), separ(_),
    day_of_week, maybe(dot, _), separ(_),
    time_hh24_mi(Time),
    "]".
 
duration(Duration) -->
    list_min1(digit, HoL), ":",
    list_min1(digit, MiL),
    { length(MiL, 2),
      number_codes(Ho, HoL), number_codes(Mi, MiL),
      Duration = Ho:Mi }.

clock(Start, End, Elapsed) -->
    separ(_),
    clock_tag,
    separ(_),
    timestamp(Start),
    "--",
    timestamp(End),
    separ(_),
    "=>",
    separ(_),
    duration(Elapsed).

%% Tests

test_parse_clock(Start, End, Elapsed) :-
    Data = '   CLOCK: [2020-02-05 Mi. 16:06]--[2020-02-05 Mi. 17:26] =>  1:20',
    atom_codes(Data, L),
    phrase((clock(Start, End, Elapsed)), L).

test :-
    Data = 'Mi',
    atom_codes(Data, L),
    phrase(day_of_week, L).
