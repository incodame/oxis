:- use_module(library(xpath)).
:- use_module(library(tabling)).
:- table load_trees/2.
:- dynamic foundbird1/2.
:- dynamic foundbird2/2.

blog_examples_dir(BlogExamples) :-
    BlogExamples = 'D:/Prolog/src/Blog'.

load_trees(XmlRoot1, XmlRoot2) :-
    blog_examples_dir(BlogExamples),
    format(atom(File1), '~w/~w', [ BlogExamples, 'orange_tree.xml']),
    load_xml(File1, XmlRoot1, _),
    format(atom(File2), '~w/~w', [ BlogExamples, 'orange_tree_modified.xml']),
    load_xml(File2, XmlRoot2, _).

% LCS based tree diff solution
lcs_birds([B|L1], [B|L2], [B|Lcs]) :- !,
    lcs_birds(L1, L2, Lcs).
lcs_birds([B1|L1], [B2|L2], Lcs) :-
    lcs_birds(L1, [B2|L2], Lcs1),
    lcs_birds([B1|L1], L2, Lcs2),
    longest(Lcs1, Lcs2, Lcs), !.
lcs_birds(_,_,[]).
longest(L1,L2,Longest) :-
    length(L1, Length1),
    length(L2, Length2),
    ((Length1 > Length2) -> Longest = L1 ; Longest = L2).

tree_d2_lcs(Xp1, XpU1, Xp2, SE1, SE2, B) :-
    load_trees(Root1, Root2),
    (
        xpath(Root1, Xp1, SE1), xpath(SE1, XpU1, U1),
        (
            (xpath(Root2, Xp1, SE2), xpath(SE2, XpU1, U1)) ->
             writeln(U1),
             findall(Bird, xpath(SE1, Xp2, Bird), L1),
             findall(Bird, xpath(SE2, Xp2, Bird), L2),
             lcs_birds(L1, L2, Lcs),
             (member(B, L1) ; member(B, L2)),
             not(member(B, Lcs))
          ;
             format(string(Message), '~w: ~w', [ "not found", U1]), writeln(Message)
        )
    ;   xpath(Root2, Xp1, SE2), xpath(SE2, XpU1, U1), xpath(SE2, Xp2, B),
        not((xpath(Root1, Xp1, SE1), xpath(SE1, XpU1, U1)))
    ).

% Map based tree diff solution
remember_foundbird1(SE1, Xp2, Xp3) :-
    xpath(SE1, Xp2, Bird),
    xpath(Bird, Xp3, BirdID),
    assertz(foundbird1(BirdID, Bird)).

remember_foundbird2(SE2, Xp2, Xp3) :-
    xpath(SE2, Xp2, Bird),
    xpath(Bird, Xp3, BirdID),
    assertz(foundbird2(BirdID, Bird)).

tree_d2_map(Xp1, XpU1, Xp2, Xp3, SE1, SE2, B) :-
    load_trees(Root1, Root2),
    (
        xpath(Root1, Xp1, SE1), xpath(SE1, XpU1, U1),
        (
            (xpath(Root2, Xp1, SE2), xpath(SE2, XpU1, U1)) ->
             writeln(U1),
             retractall(foundbird1(_,_)),
             retractall(foundbird2(_,_)),
             % reference to stack overflow
             ignore((remember_foundbird1(SE1, Xp2, Xp3), false)),
             ignore((remember_foundbird2(SE2, Xp2, Xp3), false)),
             (
                 foundbird1(Bid, B),
                 foundbird2(Bid, B2),
                 B \= B2
               ;
                 foundbird1(Bid, B), not(foundbird2(Bid, _))
               ;
                 foundbird2(Bid, B), not(foundbird1(Bid, _))
             )
          ;
             format(string(Message), '~w: ~w', [ "not found", U1]), writeln(Message)
        )
    ;   xpath(Root2, Xp1, SE2), xpath(SE2, XpU1, U1), xpath(SE2, Xp2, B),
        not((xpath(Root1, Xp1, SE1), xpath(SE1, XpU1, U1)))
    ).

% diff expressions
diff(on(Xp1,AtU1),from(Xp2,At3)) :-
    tree_d2_map(Xp1, /self(AtU1), Xp2, /self(At3), N1, N2, B),
    writeln([N1, N2, B]).

