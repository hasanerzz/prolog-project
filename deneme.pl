:- dynamic male/1, female/1, parent/2, married/2, birth_year/2, death_year/2.

% Define initial male and female members
male('m').
female('s').


% Define initial marriage relationships
married('m', 's').

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
    writeln('---LEVEL 0---'),
    print_individual_with_spouse('m'),
    find_children(['m', 's'], Level1Members),
    print_next_levels(Level1Members, 1).

find_children(Parents, Children) :-
    findall(Child, (member(Parent, Parents), parent(Parent, Child)), ChildrenList),
    list_to_set(ChildrenList, Children).

print_next_levels([], _).
print_next_levels(Individuals, Level) :-
    Individuals \= [],
    format('---LEVEL ~w---~n', [Level]),
    print_individuals_with_spouses(Individuals),
    find_children(Individuals, NextLevelIndividuals),
    NextLevel is Level + 1,
    print_next_levels(NextLevelIndividuals, NextLevel).

print_individuals_with_spouses([]).
print_individuals_with_spouses([Individual | Rest]) :-
    print_individual_with_spouse(Individual),
    print_individuals_with_spouses(Rest).

print_individual_with_spouse(Individual) :-
    (married(Individual, Spouse) ->
        format('~w-~w~n', [Individual, Spouse]);
        format('~w~n', [Individual])).


%Calculating age
current_year(2024).  % This can be dynamically set based on the current year
calculate_age(Name, Age) :-
    birth_year(Name, BirthYear),
    (   death_year(Name, DeathYear) -> Age is DeathYear - BirthYear
    ;   current_year(CurrentYear), Age is CurrentYear - BirthYear).


print_person_info(Name) :-
    (male(Name) -> Gender = 'Male' ; female(Name) -> Gender = 'Female' ; Gender = 'Unknown'),
    format('Name: ~w, Gender: ~w~n', [Name, Gender]),
    findall(Child, parent(Name, Child), Children),
    (Children == [] -> ChildrenList = 'None' ; ChildrenList = Children),
    format('Children: ~w~n', [ChildrenList]),
    findall(Parent, parent(Parent, Name), Parents),
    (Parents == [] -> ParentsList = 'None' ; ParentsList = Parents),
    format('Parents: ~w~n', [ParentsList]),
    (married(Name, Spouse) -> format('Spouse: ~w~n', [Spouse]) ; format('Spouse: None~n')),
    (   death_year(Name, DeathYear) ->
        (   DeathYear \= none ->
            (   birth_year(Name, BirthYear) ->
                Age is DeathYear - BirthYear,
                format('Status: Dead~n'),
                format('Birth Year: ~w~n', [BirthYear]),
                format('Death Year: ~w~n', [DeathYear]),
                format('Age at Death: ~w years~n', [Age])
            ;   format('Status: Dead~n'),
                format('Death Year: ~w~n', [DeathYear]),
                writeln('Birth Year: Unknown, unable to calculate age')
            )
        ;   format('Status: Dead~n'),
            writeln('Death Year: Unknown'),
            (   birth_year(Name, BirthYear) ->
                writeln('Death Year is unknown, unable to calculate age')
            ;   writeln('Birth Year: Unknown')
            )
        )
    ;   (   birth_year(Name, BirthYear) ->
            current_year(CurrentYear),
            Age is CurrentYear - BirthYear,
            format('Status: Alive~n'),
            format('Birth Year: ~w~n', [BirthYear]),
            format('Age: ~w years~n', [Age])
        ;   format('Status: Alive~n'),
            writeln('Birth Year: Unknown')
        )
    ).







% Finding relationships
find_relationship(X, Y, 'Kardes') :- sibling(X, Y).
find_relationship(X, Y, 'Parent') :- parent(X, Y).
find_relationship(X, Y, 'Cocuk') :- parent(Y, X).
find_relationship(X, Y, 'Grandparent') :- grandparent(X, Y).
find_relationship(X, Y, 'Torun') :- grandparent(Y, X).
find_relationship(X, Y, 'Amca') :- uncle(X, Y).
find_relationship(X, Y, 'Dayi') :- dayi(X, Y).
find_relationship(X, Y, 'Teyze') :- aunt(X, Y).
find_relationship(X, Y, 'hala') :- hala(X,Y).
find_relationship(X, Y, 'Kuze') :- cousin(X, Y).
find_relationship(X, Y, 'Es') :- married(X, Y).
find_relationship(X, Y, 'Baldiz') :- baldiz(X,Y).
find_relationship(X, Y, 'Kayinbirader') :- kayinbirader(X,Y).
find_relationship(X, Y, 'Elti') :- elti(X,Y).
find_relationship(X, Y, 'Bacanak') :- bacanak(X,Y).
find_relationship(X, Y, 'Ogul') :- ogul(X, Y).
find_relationship(X, Y, 'Kiz') :- kiz(X, Y).
find_relationship(X, Y, 'Erkek Kardes') :-erkekKardes(X,Y).
find_relationship(X, Y, 'Kiz Kardes') :- kizKardes(X, Y).
find_relationship(X, Y, 'Abi') :- abi(X, Y).
find_relationship(X, Y, 'Abla') :- abla(X, Y).
find_relationship(X, Y, 'Gelin') :- gelin(X, Y).
find_relationship(X, Y, 'Damat') :- damat(X, Y).
find_relationship(X, Y, 'Kayinpeder') :- kayinpeder(X, Y).
find_relationship(X, Y, 'Kayinvalide') :- kayinvalide(X, Y).



% Additional relationships
father(F, C) :- male(F), parent(F, C).
mother(M, C) :- female(M), parent(M, C).
ogul(O, P) :- male(O), parent(P,O).
kiz(K, P) :- female(K), parent(P, K).
gelin(G, K) :- female(G), ogul(O, K), married(G, O).
damat(G, K) :- male(G), kiz(O, K), married(G, O).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
erkekKardes(K,B) :- male(K),sibling(K,B),(birth_year(K) < birth_year(B)).
kizKardes(K,B) :- female(K),sibling(K,B),(birth_year(K) < birth_year(B)).
abi(K,B) :- male(B), sibling(K,B), (birth_year(K) < birth_year(B)).
abla(K,B) :- female(B), sibling(K,B), (birth_year(K) < birth_year(B)).
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
kayinpeder(K, E) :- gelin(E, K); damat(E, K), male(K).
kayinvalide(K, E) :- gelin(E, K); damat(E, K), female(K).



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
    writeln('1. Add Person'),
    writeln('2. Update Person'),
    writeln('0. Cancel'),
    read(Response),
    (Response == 1 -> add_person_prompt
    ; Response == 2 -> update_person
    ; writeln('Invalid option, returning to menu.')
    ).
% Check if the parent is alive at the time of the child's birth
is_alive(Parent, ChildBirthYear) :-
    (death_year(Parent, DeathYear) -> ChildBirthYear =< DeathYear ; true).

% Validate parenthood
valid_parenthood(Parent, ChildBirthYear) :-
    is_married(Parent),
    is_of_age(Parent, ChildBirthYear),
    is_alive(Parent, ChildBirthYear).

% Assert parenthood only if valid
assert_valid_parent(Parent, Child, ChildBirthYear) :-
    valid_parenthood(Parent, ChildBirthYear) ->
        assertz(parent(Parent, Child));
        writeln('Invalid parenthood: Check marriage status, age, and life status of the parent.').

add_person_prompt :-
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
    (DeathYear \= none -> assert_death_year(ChildName, DeathYear) ; true),  % Check if death year is provided
    writeln('Person added successfully.'),
    print_family_tree.


update_person :-
    writeln('Please type the name of the person to update:'),
    read(Name),
    (   (male(Name) ; female(Name)) ->
        writeln('What do you want to update? (birth/death/cancel)'),
        read(UpdateType),
        (UpdateType == birth -> update_birth_year(Name)
        ; UpdateType == death -> update_death_year(Name)
        ; UpdateType == cancel -> writeln('Update canceled.')
        ; writeln('Invalid option, returning to menu.')
        )
    ;   writeln('Person not found, returning to menu.')
    ).

update_birth_year(Name) :-
    writeln('Please type the new birth year:'),
    read(NewBirthYear),
    retractall(birth_year(Name, _)),
    assertz(birth_year(Name, NewBirthYear)),
    writeln('Birth year updated successfully.'),
    print_person_info(Name).


update_death_year(Name) :-
    writeln('Please type the new death year (if deceased, else type "none"):'),
    read(DeathYearInput),
    parse_death_year(DeathYearInput, NewDeathYear),
    retractall(death_year(Name, _)),
    (NewDeathYear \= none -> assertz(death_year(Name, NewDeathYear)) ; true),
    writeln('Death year updated successfully.'),
    print_person_info(Name).





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
    \+ prohibited_relationship(Spouse1, Spouse2),
    \+ underage_marriage(Spouse1, Spouse2),
    add_marriage(Spouse1, Spouse2).


underage_marriage(Spouse1, Spouse2) :-
    (calculate_age(Spouse1, Age1), Age1 < 18);
    (calculate_age(Spouse2, Age2), Age2 < 18).


prohibited_relationship(Spouse1, Spouse2) :-
    uncle(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    aunt(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : TEYZE - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    father(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : BABA - COCUK').
prohibited_relationship(Spouse1, Spouse2) :-
    mother(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : ANNE - COCUK').
prohibited_relationship(Spouse1, Spouse2) :-
    uncle(Spouse2, Spouse1),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    aunt(Spouse2, Spouse1),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    father(Spouse2, Spouse1),
    writeln('INVALID MARRIAGE : BABA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    mother(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : ANNE - COCUK').
prohibited_relationship(Spouse1, Spouse2) :-
    dayi(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    dayi(Spouse2, Spouse1),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    hala(Spouse1, Spouse2),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').
prohibited_relationship(Spouse1, Spouse2) :-
    hala(Spouse2, Spouse1),
    writeln('INVALID MARRIAGE : AMCA - YEGEN').




% Start the program
:- initialization(menu).














