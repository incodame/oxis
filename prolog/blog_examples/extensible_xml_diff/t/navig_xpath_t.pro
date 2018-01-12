:- use_module(library(xpath)).
:- use_module(library(plunit)).
:- begin_tests(extensible_xml_diff).
:- working_directory(_, 'd:/prolog/src/Blog').
:- use_module(extensible_xml_diff).

%'navigation path to parent huhu' :-
test(navig_xpath1_to_parent_huhu, []) :-
    load_trees(R1, _),
    xpath(R1, //'NEST'(@type='owl-nest'), N),
    navig_xpath1(N, [], L-[], element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], [])),
    writeln(L),
    L = [[bird, 'NEST']].

test(navig_xpath2_to_parent_huhu, []) :-
    load_trees(R1, _),
    xpath(R1, //'NEST'(@type='owl-nest'), N),
    navig_xpath2(N, 1, [], L-[], element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], [])),
    writeln(L),
    L = [['bird(4)', 'NEST(1)']].

test(navig_xpath3_to_parent_huhu, []) :-
    load_trees(R1, _),
    xpath(R1, //'NEST'(@type='owl-nest'), N),
    navig_xpath3(N, 1, [], L-[], element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], []), 'name'),
    writeln(L),
    L = [['bird(2, @name=huhu)', 'NEST(1)']].

test(navig_xpath_to_parent_huhu_1, []) :-
    load_trees(R1, _),
    xpath(R1, //'NEST'(@type='owl-nest'), N),
    navig_xpath(N, element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], []), 'name', XpathList),
    writeln(XpathList),
    XpathList = ['NEST(1)/bird(2, @name=huhu)'].

test(navig_xpath_to_parent_huhu_2, []) :-
    load_trees(_, R2),
    xpath(R2, //'NEST'(@type='owl-nest'), N),
    navig_xpath(N, element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], []), 'name', XpathList),
    writeln(XpathList),
    XpathList = ['NEST(1)/bird(2, @name=huhu)'].

test(navig_xpath_to_parent_huhu_2_self_attr, []) :-
    load_trees(_, R2),
    xpath(R2, //'NEST'(@type='owl-nest'), N),
    navig_xpath(N, element(bird, [name=huhu, px_order='2.1', type=owl, age='36'], []), 'self', XpathList),
    writeln(XpathList),
    XpathList = ['NEST(1)/bird(4)'].

test(navig_xpath_to_all_huhu, []) :-
    load_trees(_, R2),
    xpath(R2, //'NEST'(@type='owl-nest'), N),
    navig_xpath(N, element(bird, [name=huhu, type=owl], []), 'name', XpathList),
    writeln(XpathList),
    XpathList = ['NEST(1)/bird(1, @name=huhu)', 'NEST(1)/bird(2, @name=huhu)'].

:- end_tests(extensible_xml_diff).
