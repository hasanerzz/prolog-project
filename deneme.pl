:- dynamic male/1, female/1, parent/2, married/2, birth_year/2, death_year/2.

% Define initial male and female members
male('Murat Aslan').
female('Sedanur Aslan').

% Define initial marriage relationships
married('Murat Aslan', 'Sedanur Aslan').

% Adding a new person
add_person(Name, male) :-
    \+ male(Name),
    assertz(male(Name)).

add_person(Name, female) :-
    \+ female(Name),
    assertz(female(Name)).

% Updating a person's gender
update_gender(Name, male) :-
    retractall(female(Name)),
    (male(Name) -> true ; assertz(male(Name))).

update_gender(Name, female) :-
    retractall(male(Name)),
    (female(Name) -> true ; assertz(female(Name))).

% Printing the family tree
print_family_tree :-
    findall((Parent, Child), parent(Parent, Child), ParentChildPairs),
    writeln('Parent-Child Relationships:'),
    forall(member((Parent, Child), ParentChildPairs), writeln(Parent-Child)),
    findall((Spouse1, Spouse2), married(Spouse1, Spouse2), MarriedPairs),
    writeln('Married Couples:'),
    forall(member((Spouse1, Spouse2), MarriedPairs), writeln(Spouse1-Spouse2)).
% Calculating age
current_year(2024).  % This can be dynamically set based on the current year
calculate_age(Name, Age) :-
    birth_year(Name, BirthYear),
    (   death_year(Name, DeathYear) -> Age is DeathYear - BirthYear
    ;   current_year(CurrentYear), Age is CurrentYear�-�BirthYear).

% Printing information about a person
print_person_info(Name) :-
    (male(Name) -> Gender = 'Male' ; female(Name) -> Gender = 'Female' ; Gender = 'Unknown'),
    format('Name: ~w, Gender: ~w~n', [Name, Gender]),
    findall(Child, parent(Name, Child), Children),
    format('Children: ~w~n', [Children]),
    findall(Parent, parent(Parent, Name), Parents),
    format('Parents: ~w~n', [Parents]),
    (married(Name, Spouse) -> format('Spouse: ~w~n', [Spouse]) ; true),
    (calculate_age(Name, Age) -> format('Age: ~w~n', [Age]) ; writeln('Age:�Unknown')).

% Finding relationships
find_relationship(X, Y, 'karde�') :- sibling(X, Y).
find_relationship(X, Y, 'parent') :- parent(X, Y).
find_relationship(X, Y, 'child') :- parent(Y, X).
find_relationship(X, Y, 'grandparent') :- grandparent(X, Y).
find_relationship(X, Y, 'grandchild') :- grandparent(Y, X).
find_relationship(X, Y, 'amca') :- uncle(X, Y).
find_relationship(X, Y, 'dayi') :- dayi(X, Y).
find_relationship(X, Y, 'teyze') :- aunt(X, Y).
find_relationship(X, Y, 'hala') :- hala(X,Y).
find_relationship(X, Y, 'cousin') :- cousin(X, Y).
find_relationship(X, Y, 'spouse') :- married(X, Y).
find_relationship(X, Y, 'baldiz') :- baldiz(X,Y).
find_relationship(X, Y, 'kayinbirader') :- kayinbirader(X,Y).
find_relationship(X, Y, 'elti') :- elti(X,Y).
find_relationship(X, Y, 'bacanak') :- bacanak(X,Y).


% Additional relationships
father(F, C) :- male(F), parent(F, C).
mother(M, C) :- female(M), parent(M, C).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
grandfather(GF, GC) :- male(GF), grandparent(GF, GC).
grandmother(GM, GC) :- female(GM), grandparent(GM, GC).
uncle(U, N) :- male(U), sibling(U, P),male(P), parent(P, N).
dayi(D,Y) :-  male(D), sibling(D, P), female(P), parent(P,Y).
aunt(A, N) :- female(A), sibling(A, P), female(P),parent(P, N).
hala(H, Y) :- female(H), sibling(H,P), male(P), parent(P,Y).
cousin(X, Y) :- parent(P1, X), parent(P2, Y), sibling(P1, P2).
baldiz(S,H) :- male(H),married(H,W), female(S), sibling(S,W).
kayinbirader(B,H) :- male(B), male(H), married(H,W), sibling(W,B).
elti(S,W) :- female(S), female(W), married(H,W), married(S,X),
    sibling(X,H).
bacanak(S,W) :- male(S), male(W), married(H,W), married(S,X),
    sibling(X,H).
eniste(E, H):- male(E), male(H), sibling(H,S), female(S), married(E,S).


% Adding a marriage
add_marriage(Spouse1, Spouse2) :-
    assertz(married(Spouse1, Spouse2)),
    assertz(married(Spouse2, Spouse1)).

% Menu
menu :-
    repeat,
    writeln('1-) Ask relation'),
    writeln('2-) Add/Update person'),
    writeln('3-) Get information of any person'),
    writeln('4-) Print the family tree'),
    writeln('5-) Add marriage'),
    writeln('6-) Terminate the program'),
    read(Choice),
    (   Choice == 1 -> ask_relation, fail
    ;   Choice == 2 -> add_update_person, fail
    ;   Choice == 3 -> get_person_info, fail
    ;   Choice == 4 -> print_family_tree, fail
    ;   Choice == 5 -> add_marriage_prompt, fail
    ;   Choice == 6 -> writeln('Terminating program.'), !, fail
    ;   writeln('Invalid choice, please try again.'), fail
    ).

% Asking for relationship
ask_relation :-
    writeln('Enter the first person:'),
    read(Person1),
    writeln('Enter the second person:'),
    read(Person2),
    (   find_relationship(Person1, Person2, Relationship) ->
        format('The relationship between ~w and ~w is: ~w~n', [Person1, Person2, Relationship])
    ;   writeln('No relationship found.')
    ).

% Adding or updating a person
add_update_person :-
    writeln('Please type the father\'s name:'),
    read(FatherName),
    writeln('Please type the mother\'s name:'),
    read(MotherName),
    writeln('Please type the child\'s name:'),
    read(ChildName),
    writeln('Please type the child\'s gender (male/female):'),
    read(Gender),
    writeln('Please type the child\'s birth year:'),
    read(BirthYear),
    writeln('Please type the child\'s death year (if deceased, else type "none"):'),
    read(DeathYearInput),
    parse_death_year(DeathYearInput, DeathYear),
    assert_person(FatherName, male),
    assert_person(MotherName, female),
    assert_person(ChildName, Gender),
    assert_parent(FatherName, ChildName),
    assert_parent(MotherName, ChildName),
    assert_birth_year(ChildName, BirthYear),
    assert_death_year(ChildName, DeathYear),
    writeln('Person added/updated successfully.'),
    print_family_tree.

parse_death_year('none', none) :- !.
parse_death_year(Year, Year).

assert_person(Name, Gender) :-
    (   Gender == male ->
        assertz(male(Name))
    ;   Gender == female ->
        assertz(female(Name))
    ).


assert_parent(ParentName, ChildName) :-
    assertz(parent(ParentName, ChildName)).

assert_birth_year(Name, Year) :-
    assertz(birth_year(Name, Year)).

assert_death_year(Name, none) :-
    !.
assert_death_year(Name, Year) :-
    assertz(death_year(Name, Year)).


% Getting information about a person
get_person_info :-
    writeln('Enter the name of the person:'),
    read(Name),
    print_person_info(Name).


% Adding a marriage prompt
add_marriage_prompt :-
    writeln('Enter the name of the first spouse:'),
    read(Spouse1),
    writeln('Enter the name of the second spouse:'),
    read(Spouse2),
    ask_relation(Spouse1, Sposue2, Relation1).
    ask_relation(Spouse2, Spouse1, Relation2).
    (prohibited_relationship(Spouse1, Spouse2) ->
        write('INVALID MARRIAGE : ~w - ~w', [Relation1, Relation2)
    ; underage_marriage(Spouse1, Spouse2) ->
        writeln('Under 18 age marriage.')
    ; add_marriage(Spouse1, Spouse2),
      writeln('Marriage added successfully.')
����).

underage_marriage(Spouse1, Spouse2) :-
    (calculate_age(Spouse1, Age1), Age1 < 18);
    (calculate_age(Spouse2, Age2), Age2 < 18).

prohibited_relationship(Spouse1, Spouse2) :-
    uncle(Spouse1, Spouse2).
prohibited_relationship(Spouse1, Spouse2) :-
    aunt(Spouse1, Spouse2).
prohibited_relationship(Spouse1, Spouse2) :-
    parent(Spouse1, Spouse2).
prohibited_relationship(Spouse1, Spouse2) :-
    uncle(Spouse2, Spouse1).
prohibited_relationship(Spouse1, Spouse2) :-
    aunt(Spouse2, Spouse1).
prohibited_relationship(Spouse1, Spouse2) :-
    parent(Spouse2, Spouse1).
prohibited_relationship(Spouse1, Spouse2) :-
    dayi(Spouse1, Spouse2).
prohibited_relationship(Spouse1, Spouse2) :-
    dayi(Spouse2, Spouse1).
prohibited_relationship(Spouse1, Spouse2) :-
    hala(Spouse1, Spouse2).
prohibited_relationship(Spouse1, Spouse2) :-
    hala(Spouse2, Spouse1).





% Start the program
:- initialization(menu).
















