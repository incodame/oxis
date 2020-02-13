%
% Clocking Tool
% Text processor for emacs org. mode files
% February 2020 
% v0.2
%
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(julian)).
:- use_module(library(list_util)).
:- use_module(library(regex)).
:- use_module(library(yall)).
 
om_notes_directory('/path/to/org_mode_notes').

daily_notes_file(Day, OmNotesFilePath) :-
    om_notes_directory(OmNotesDir),
    directory_files(OmNotesDir, Files),
    member(OmNotesFile, Files),
    OmNotesFile =~ "^\\d{8}.org$",
    atom_concat(Day, '.org', OmNotesFile),
    format(atom(OmNotesFilePath), '~w/~w', [OmNotesDir, OmNotesFile]).

has_clock(Line) :-
    string_codes(Line, L),
    phrase((clock(_Start, _End, _Elapsed)), L).

% Example use: ?- clocks('20200206', Clocks), take(2, Clocks, Res).
clocks(Day, ClockLines) :-
    daily_notes_file(Day, OmNotesFilePath),
    lines(file(OmNotesFilePath), Lines),
    lazy_include(has_clock, Lines, ClockLines).

clock_duration(ClockLine, Duration) :-
    string_codes(ClockLine, L),
    phrase((clock(_Start, _End, Duration)), L).

% Example use: ?- duration_add(1:30, 0:32, R).
duration_add(Dur1, Dur2, Dur) :-
    Dur1 = Ho1:Mi1, Dur2 = Ho2:Mi2,
    DurInSec #= Ho1*3600+Mi1*60+Ho2*3600+Mi2*60,
    DurH #= DurInSec div 3600, DurRest #= DurInSec mod 3600,
    DurMi #= DurRest div 60,
    Dur = DurH:DurMi.
 
duration_minus(Dur1, Dur2, Dur) :-
    Dur1 = Ho1:Mi1, Dur2 = Ho2:Mi2,
    DurInSec #= Ho1*3600+Mi1*60-Ho2*3600-Mi2*60,
    DurH #= DurInSec div 3600, DurRest #= DurInSec mod 3600,
    DurMi #= DurRest div 60,
    Dur = DurH:DurMi.
 
duration_over(Dur, Threshold) :-
    Dur = Ho1:Mi1, Threshold = Ho2:Mi2,
    DurInSec #= Ho1*3600+Mi1,
    ThrInSec #= Ho2*3600+Mi2,
    DurInSec #>= ThrInSec.

daily_agg(Day, Dur) :-
    clocks(Day, ClockLines),
    maplist(clock_duration, ClockLines, CDurations),
    foldl([A,B,C]>>duration_add(A,B,C), CDurations, 0:0, Dur).

monthly_over(Year, Month, Threshold, Day, DayAgg) :-
    month_day(Year, Month, D),
    format(atom(Day), '~`0t~d~4+~`0t~d~2+~`0t~d~2+', [Year, Month, D]),
    daily_agg(Day, DayAgg),
    duration_over(DayAgg, Threshold).

% Example use: Checking overtime for one year
%?- findall(Over, (yearly_over(2016, 08:30, M, Day, Agg), 
%       duration_over(12:0, Agg), duration_minus(Agg, 08:30, Over)), L), 
%       foldl([A,B,C]>>duration_add(A,B,C), L, 0:0, Sum).
yearly_over(Year, Threshold, Month, Day, DayAgg) :-
    year_day(Year, Month, D),
    format(atom(Day), '~`0t~d~4+~`0t~d~2+~`0t~d~2+', [Year, Month, D]),
    daily_agg(Day, DayAgg),
    duration_over(DayAgg, Threshold).

month_day(Year, Month, D) :-
    D in 1..31, form_time([Year-Month-D, weekday], Dt), date(Dt).

year_day(Year, M, D) :-
    M in 1..12, D in 1..31, form_time([Year-M-D, weekday], Dt), date(Dt).

%% Grammar

% a non empty list
list_min1(Pred, [L])    --> [L], {call(Pred, L, _, _)}.
list_min1(Pred, [L|Ls]) --> [L], {call(Pred, L, _, _)}, list_min1(Pred, Ls).

% an optional object
maybe(_,      [])  --> [].
maybe(Pred,   [L]) --> [L], {call(Pred, L, _, _)}.

dot(C) --> {C=46}.

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
 
day_of_week --> ( "Mo"
                | "Di"
                | "Mi"
                | "Do"
                | "Fr"
                | "Sa"
                | "So" ).
 
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
    Data = "   CLOCK: [2020-02-05 Mi. 16:06]--[2020-02-05 Mi. 17:26] =>  1:20",
    string_codes(Data, L),
    phrase((clock(Start, End, Elapsed)), L).
 
test_check_clock(Result) :-
    test_parse_clock(Start, End, Ho:Mi),
    form_time(Start, StT),
    form_time(End, EndT),
    compare_time(<,StT,EndT),
    delta_time(StT, s(DeltaSec), EndT),
    ClockDelta #= Ho*3600+Mi*60,
    (ClockDelta = DeltaSec ->
        Result = 'Ok'
    ;
         Result = 'Nok'
    ).

