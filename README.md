# oxis
Emacs toolkit to sOrt Xml from InSide out


A practical toolkit to work from inside Xml content


problem
-------
Line-Oriented Xml diff is not usable, when working with common formats


example
-------
In Maven pom.xml, it makes sense to sort records by group-id then artifact-id, from within the dependencies section

In Informatica mapping export xml, it's suitable to sort elements by NAME attribute


Some goals of this toolkit
--------------------------
To provide the necessary Emacs Lisp functions for
- on the fly re-sorting of sub Xml sections, based on the kind of Xml file being edited
- provide interactively several Xml tree sorting options to the user
- interacting with the Emacs ediff functions
- work within the constraints of Xsd schemas or DTDs

The functions developed should be sufficiently generic to enable new developers to
easily integrate support for new Xml formats.
