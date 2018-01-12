:- use_module(library(xpath)).
:- use_module(library(plunit)).
:- begin_tests(extensible_xml_diff).
:- working_directory(_, 'd:/prolog/src/Blog').
:- use_module(extensible_xml_diff).

%'relative navigation path to first huhu' :-
test(remember_first_huhu, []) :-
    load_trees(R1, _),
    retractall(foundbird1(_,_)),
    retractall(navigpath1(_,_,_,_)),
    xpath(R1, //'NEST'(@type='owl-nest'), N),
    ignore((remember_foundbird1(N, //bird, @name, /self(@name)), false)),
    foundbird1('NEST(1)/bird(1, @name=huhu)', element(bird, Attrs, _)),
    member(name='huhu', Attrs),
    member(age='3', Attrs).

%test of from(//bird)
test(remember_all_huhu_1, []) :-
    load_trees(_, R2),
    retractall(foundbird2(_,_)),
    retractall(navigpath2(_,_,_,_)),
    xpath(R2, //'NEST'(@type='owl-nest'), N),
    ignore((remember_foundbird2(N, //bird, @self, /self), false)),
    foundbird2('NEST(1)/bird(2)', element(bird, Attrs1st, _)),
    member(name='huhu', Attrs1st),
    member(age='4', Attrs1st),
    foundbird2('NEST(1)/bird(4)', element(bird, Attrs2st, _)),
    member(name='huhu', Attrs2st),
    member(age='36', Attrs2st).

%test of from(//bird, @name)
test(remember_all_huhu_2, []) :-
    load_trees(_, R2),
    retractall(foundbird2(_,_)),
    retractall(navigpath2(_,_,_,_)),
    xpath(R2, //'NEST'(@type='owl-nest'), N),
    ignore((remember_foundbird2(N, //bird, @name, /self(@name)), false)),
    foundbird2('NEST(1)/bird(1, @name=huhu)', element(bird, Attrs1st, _)),
    member(name='huhu', Attrs1st),
    member(age='4', Attrs1st),
    foundbird2('NEST(1)/bird(2, @name=huhu)', element(bird, Attrs2, _)),
    member(name='huhu', Attrs2),
    member(age='36', Attrs2).

:- end_tests(extensible_xml_diff).

%?- run_tests.
% PL-Unit: extensible_xml_diff [navigpath1,NEST(1)/bird(1, @name=owli)]
%[navigpath1,NEST(1)/bird(1, @name=huhu)]
%[navigpath1,NEST(1)/bird(1, @name=cucko)]
% Break level 1
%[1]  ?- foundbird1(Bid, Bird).
%Correct to: "extensible_xml_diff:foundbird1(Bid,Bird)"? yes
%            Bid = 'NEST(1)/bird(1, @name=owli)',
%Bird = element(bird, [name=owli, px_order='2.1', type=owl, age='2'], []) ;
%Bid = 'NEST(1)/bird(1, @name=huhu)',
%Bird = element(bird, [name=huhu, px_order='2.1', type=owl, age='3'], []) ;
%Bid = 'NEST(1)/bird(1, @name=cucko)',
%Bird = element(bird, [name=cucko, px_order='2.1', type=cuckoo, age='6'], ['\n            %        ', element(weight, [], ['400']), '\n                  ']).
