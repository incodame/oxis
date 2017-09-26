:- use_module(library(xpath)).
load_trees(XmlRoot1, XmlRoot2) :-
    load_xml('./xml_source_1.xml', XmlRoot1, _),
    load_xml('./xml_source_2.xml', XmlRoot2, _).

find_differences(Reference, Root1, Root2) :-
    xpath(Root1, //'Child'(@name=Name), Node),
    not(xpath(Root2, //'Child'(@name=Name), Node)),
    writeln([Reference, Name, Node]).

diff :-
    load_trees(Root1, Root2),
    (find_differences('1', Root1, Root2) ; find_differences('2', Root2, Root1)).
     
