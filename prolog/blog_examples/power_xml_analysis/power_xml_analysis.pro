:- use_module(library(xpath)).
:- use_module(library(tabling)).
:- table load_tree/1.

blog_examples_dir(BlogExamples) :-
    BlogExamples = 'D:/Prolog/src/Blog'.

load_tree(XmlRoot) :-
    blog_examples_dir(BlogExamples),
    format(atom(File), '~w/~w', [ BlogExamples, 'orange_tree.xml']),
    writeln('Loading Xml tree.'),
    load_xml(File, XmlRoot, _).

% extracting text
get_gps_position_1(Val) :-
    load_tree(XmlRoot),
    xpath(XmlRoot, //gps-position(text), Val).

get_gps_position_2(Val) :-
    load_tree(XmlRoot),
    xpath(XmlRoot, //'gps-position'(text), Val).

% extracting nodes
get_birds(Bird) :-
    load_tree(XmlRoot),
    xpath(XmlRoot, //bird, Bird).

get_nests(Nest, Branch) :-
    load_tree(XmlRoot),
    xpath(XmlRoot, //branch, Branch),
    xpath(Branch,  //'NEST', Nest).

% using unification with intermediate nodes
get_cuckoos(Bird) :-
    get_birds(Bird),
    xpath(Bird, /self(@type), 'cuckoo').

% using unification for bottom up search, with 100% reuse of previous predicates ?
nests_with_cuckoos(Branch, Nest) :-
    get_nests(Nest, Branch),
    get_cuckoos(Cuckoo),
    xpath(Nest, //bird, Cuckoo).

% using tabling to avoid loading twice the same Xml file
% header change
