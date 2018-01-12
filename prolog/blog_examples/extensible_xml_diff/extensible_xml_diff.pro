:- module(extensible_xml_diff, [
              diff/2,
              foundbird1/2,
              foundbird2/2,
              load_trees/2,
              navig_xpath/4,
              navig_xpath1/4,
              navig_xpath2/5,
              navig_xpath3/6,
              remember_foundbird1/4,
              remember_foundbird2/4,
              set_tree_versions/2
          ]).
:- use_module(library(xpath)).
:- use_module(library(tabling)).
:- use_module(library(lists)).
:- table load_trees/2.
:- dynamic foundbird1/2.
:- dynamic foundbird2/2.
:- dynamic navigpath1/4.
:- dynamic navigpath2/4.

blog_examples_dir(BlogExamples) :-
    BlogExamples = 'D:/Prolog/src/Blog'.

load_trees(XmlRoot1, XmlRoot2) :-
    blog_examples_dir(BlogExamples),
    tree_versions(Initial, Modified),
    format(atom(File1), '~w/orange_tree_~w.xml', [ BlogExamples, Initial]),
    load_xml(File1, XmlRoot1, _),
    format(atom(File2), '~w/orange_tree_~w.xml', [ BlogExamples, Modified]),
    load_xml(File2, XmlRoot2, _).

tree_versions('initial', 'modified').
% use this in tests
set_tree_versions(Initial, Modified) :-
    retractall(tree_versions(_, _)),
    assertz(tree_versions(Initial, Modified)).

%
% generic diff, extensible via:
%  - 2 node matchers (generators)
%

% internal "messages" (interface)
ask_object_match(SE1, SE2) :-
    shift(ask_object_match(SE1, SE2)).

yield_object_match(SE1, SE2) :-
    shift(yield_object_match(SE1, SE2)).

yield_subnode_match(SE1, SE2, B1, B2) :-
    shift(yield_subnode_match(SE1, SE2, B1, B2)).

yield_no_subnode_match(SE1, SE2, B) :-
    shift(yield_no_subnode_match(SE1, SE2, B)).

% generic diff implementation
diff(ObjectMatcher, SubNodeMatcher) :-
    reset(SubNodeMatcher,Term1,SubNodeMatcher1),
    ( SubNodeMatcher1 == 0 ->
      true
    ; Term1 = ask_object_match(SE1, SE2) ->
      reset(ObjectMatcher,Term2,ObjectMatcher1),
      ( Term2 == 0 ->
          SE1 = eof,
          SE2 = eof,
          % unfinished goal from SubNodeMatcher
          call(SubNodeMatcher1)
      ; Term2 = yield_object_match(SE1, SE2) ->
          % SE1 and SE2 now unify with ask_object_match(SE1,SE2), recursion with unfinished goals
          diff(ObjectMatcher1,SubNodeMatcher1)
      )
    ; Term1 = yield_subnode_match(SE1, SE2, B1, B2) ->
      (
          % matching subnode different
          B1 \= B2 -> writeln([SE1, SE2, B1, B2])
      )
    ; Term1 = yield_no_subnode_match(SE1, SE2, B) ->
          % matching subnode absent
          writeln([SE1, SE2, B])
    ).

%
% object matchers
%
on(Xp1, AtU1) :-
    load_trees(Root1, Root2),
    ( xpath(Root1, Xp1, SE1), xpath(SE1, /self(AtU1), U1),
      ((xpath(Root2, Xp1, SE2), xpath(SE2, /self(AtU1), U1)) ->
           yield_object_match(SE1, SE2)
      ;
           format(string(Message), '~w: ~w', [ "not found", U1]), writeln(Message),
           yield_object_match(SE1, eof)
      )
    ; xpath(Root2, Xp1, SE2), xpath(SE2, /self(AtU1), U1),
      not((xpath(Root1, Xp1, SE1), xpath(SE1, /self(AtU1), U1))),
      yield_object_match(eof, SE2)
    ).

on(Xp1, AtU1, Xp2, AtU2) :-
    load_trees(Root1, Root2),
    ( xpath(Root1, Xp1, E1), xpath(E1, /self(AtU1), U1), xpath(E1, Xp2, SE1), xpath(SE1, /self(AtU2), U2),
      ((xpath(Root2, Xp1, E2), xpath(E2, /self(AtU1), U1), xpath(E2, Xp2, SE2), xpath(SE2, /self(AtU2), U2)) ->
            yield_object_match(SE1, SE2)
        ;
            format(string(Message), '~w: ~w // ~w', [ "not found", U1, U2]), writeln(Message),
            yield_object_match(SE1, eof)
      )
    ; xpath(Root2, Xp1, E2), xpath(E2, /self(AtU1), U1), xpath(E2, Xp2, SE2), xpath(SE2, /self(AtU2), U2),
      not((xpath(Root1, Xp1, E1), xpath(E1, /self(AtU1), U1), xpath(E1, Xp2, SE1), xpath(SE1, /self(AtU2), U2))),
      yield_object_match(eof, SE2)
    ).

%
% subnode matchers
%
from(Xp2, At3) :-
    ask_object_match(SE1, SE2),
    (
      retractall(foundbird1(_,_)),
      retractall(foundbird2(_,_)),
      % reference to stack overflow
      ignore((remember_foundbird1(SE1, Xp2, At3, /self(At3)), false)),
      ignore((remember_foundbird2(SE2, Xp2, At3, /self(At3)), false)),
      (
          foundbird1(RpathId, B1),
          foundbird2(RpathId, B2),
          yield_subnode_match(SE1, SE2, B1, B2)
      ;
          foundbird1(RpathId, B), not(foundbird2(RpathId, _)),
          yield_no_subnode_match(SE1, SE2, B)
      ;
          foundbird2(RpathId, B), not(foundbird1(RpathId, _)),
          yield_no_subnode_match(SE1, SE2, B)
      )
    ).

from(Xp2) :-
    ask_object_match(SE1, SE2),
    (
        retractall(foundbird1(_,_)),
        retractall(foundbird2(_,_)),
        % reference to stack overflow
        ignore((remember_foundbird1(SE1, Xp2, @self, /self), false)),
        ignore((remember_foundbird2(SE2, Xp2, @self, /self), false)),
        (
            foundbird1(RpathId, B1),
            foundbird2(RpathId, B2),
            yield_subnode_match(SE1, SE2, B1, B2)
        ;
        foundbird1(RpathId, B), not(foundbird2(RpathId, _)),
        yield_no_subnode_match(SE1, SE2, B)
        ;
        foundbird2(RpathId, B), not(foundbird1(RpathId, _)),
        yield_no_subnode_match(SE1, SE2, B)
        )
    ).

%
% navig_xpath
%

%
% version 1
%
navig_xpath1([], _, Z-Z, _) :- !.

navig_xpath1([H|T], Path, A-C, Goal) :-
    navig_xpath1(H, Path, A-B, Goal),
    navig_xpath1(T, Path, B-C, Goal), !.

% sub node..
% matched, including leaf node
navig_xpath1(element(E, Attrs, _), Path, [[E|Path]|Z]-Z, element(E, Attrs, _)) :- !.

% goto sub elements
navig_xpath1(element(E, _, Selms), Path, A-B, Goal) :-
    findall(Y, (member(Y, Selms), not(atom(Y))), L),
    navig_xpath1(L, [E|Path], A-B, Goal), !.
% anything else..
navig_xpath1(_, _, Z-Z, _).

%
% version 2
%
navig_xpath2([], _, _, Z-Z, _) :- !.

navig_xpath2([H|T], [I|J], Path, A-C, Goal) :-
    navig_xpath2(H, I, Path, A-B, Goal),
    navig_xpath2(T, J, Path, B-C, Goal), !.

% sub node..
% matched, including leaf node
navig_xpath2(element(E, Attrs, _), I, Path, [[Estr|Path]|Z]-Z, element(E, Attrs, _)) :-
    format(atom(Estr), '~w(~w)', [E, I]), !.

% goto sub elements
navig_xpath2(element(E, _, Selms), I, Path, A-B, Goal) :-
    findall(Y, (member(Y, Selms), not(atom(Y))), L),
    indexl2(L, [], IndexList),
    format(atom(Estr), '~w(~w)', [E, I]),
    navig_xpath2(L, IndexList, [Estr|Path], A-B, Goal), !.
% anything else..
navig_xpath2(_, _, _, Z-Z, _).

%
% version 3
%
navig_xpath3([], _, _, Z-Z, _, _) :- !.

navig_xpath3([H|T], [I|J], Path, A-C, Goal, GoalAttr) :-
    navig_xpath3(H, I, Path, A-B, Goal, GoalAttr),
    navig_xpath3(T, J, Path, B-C, Goal, GoalAttr), !.

% sub node..
% matched, including leaf node
navig_xpath3(element(E, Attrs, _), I, Path, [[Estr|Path]|Z]-Z, element(E, SpecAttrs, _), GoalAttr) :-
    (GoalAttr = 'self' ->
         GoalValue = SpecAttrs
    ;
         member(GoalAttr=GoalValue, Attrs)
    ),
    sublist(SpecAttrs, Attrs),
    (GoalAttr = 'self' ->
         format(atom(Estr), '~w(~w)', [E, I])
    ;
         format(atom(Estr), '~w(~w, @~w=~w)', [E, I, GoalAttr, GoalValue])
    ), !.

% goto sub elements
navig_xpath3(element(E, _, Selms), I, Path, A-B, element(G, Attrs, S), GoalAttr) :-
    findall(Y, (member(Y, Selms), not(atom(Y))), L),
    indexl3(L, G, GoalAttr, [], IndexList),
    format(atom(Estr), '~w(~w)', [E, I]),
    navig_xpath3(L, IndexList, [Estr|Path], A-B, element(G, Attrs, S), GoalAttr), !.
% anything else..
navig_xpath3(_, _, _, Z-Z, _, _).

sublist(SubL, L) :-
    list_to_set(L, LSet),
    list_to_set(SubL, SubSet),
    subset(SubSet, LSet).

indexl([], [], _).
indexl([_|T], [N|U], N) :-
    N1 is N+1,
    indexl(T, U, N1), !.

indexl2([], _, []).
indexl2([element(E, _, _)|T], L, [N|U]) :-
    (memberchk(found(E, M), L) ->
         N is M+1
    ;
         N is 1
    ),
    indexl2(T, [found(E,N)|L], U), !.

indexl3([], _, _, _, []).
indexl3([element(E, Attrs, _)|T], GoalElem, GoalAttr, L, [N|U]) :-
    (GoalElem = E,
     memberchk(GoalAttr=GoalValue, Attrs),
     memberchk(found(E,GoalAttr,GoalValue,M), L) ->
         N is M+1
    ;

     memberchk(found(E, M), L) ->
         N is M+1
    ;
         N is 1
    ),
    (GoalElem = E,
     memberchk(GoalAttr=GoalValue, Attrs) ->
         indexl3(T, GoalElem, GoalAttr, [found(E,GoalAttr,GoalValue,N)|L], U)
    ;
         indexl3(T, GoalElem, GoalAttr, [found(E,N)|L], U)
    ), !.

% navig_xpath
navig_xpath(FromNode, ToNode, IdAttr, XpathList) :-
    navig_xpath3(FromNode, 1, [], L-[], ToNode, IdAttr),
    maplist(format_xpath, L, XpathList).

format_xpath(Path, PathAtom) :-
    reverse(Path, Rpath),
    atomic_list_concat(Rpath, '/', PathAtom).

% Map based tree diff solution
remember_foundbird1(SE1, Xp2, @Attr3, Xp3) :-
    xpath(SE1, Xp2, Bird),
    xpath(Bird, Xp3, BirdID),
    birdspec(Bird, Attr3, BirdSpec),
    (navigpath1(SE1, BirdID, PathList, CurrentIndex) ->
         NewIndex is CurrentIndex + 1,
         retractall(navigpath1(SE1, BirdID, PathList, _)),
         assertz(navigpath1(SE1, BirdID, PathList, NewIndex))
    ;
         navig_xpath(SE1, BirdSpec, Attr3, PathList),
         NewIndex is 1,
         assertz(navigpath1(SE1, BirdID, PathList, NewIndex))
    ),
    nth1(NewIndex, PathList, RpathId),
    assertz(foundbird1(RpathId, Bird)),
    writeln(['navigpath1', RpathId]).

remember_foundbird2(SE2, Xp2, @Attr3, Xp3) :-
    xpath(SE2, Xp2, Bird),
    xpath(Bird, Xp3, BirdID),
    birdspec(Bird, Attr3, BirdSpec),
    (navigpath2(SE2, BirdID, PathList, CurrentIndex) ->
         NewIndex is CurrentIndex + 1,
         retractall(navigpath2(SE2, BirdID, PathList, _)),
         assertz(navigpath2(SE2, BirdID, PathList, NewIndex))
    ;
         navig_xpath(SE2, BirdSpec, Attr3, PathList),
         NewIndex is 1,
         assertz(navigpath2(SE2, BirdID, PathList, NewIndex))
    ),
    nth1(NewIndex, PathList, RpathId),
    assertz(foundbird2(RpathId, Bird)),
    writeln(['navigpath2', RpathId]).

birdspec(element(E, AttrList, _), Attr, BirdSearchSpec) :-
    (Attr = 'self' ->
         BirdSearchSpec = element(E, AttrList, [])
    ;
         member(Attr=SearchVal, AttrList),
         BirdSearchSpec = element(E, [Attr=SearchVal], [])
    ).
