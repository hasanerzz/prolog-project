% Define dynamic predicates for family members and marriages
:- dynamic person/9.
:- dynamic marriage/2.

% Define example base family members
person(id1, male, 'John', 'Doe', date(1950, 1, 1), date(2020, 12, 31), '', '', []).
person(id2, female, 'Jane', 'Doe', date(1952, 2, 2), date(2018, 11, 30), '', '', [id3, id4]).
person(id3, male, 'Jim', 'Doe', date(1970, 3, 3), date(2010, 10, 10), id1, id2, []).
person(id4, female, 'Emma', 'Doe', date(1980, 4, 4), _, id1, id2, []).

% Add a new person to the family tree
add_person(ID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother) :-
    \+ person(ID, _, _, _, _, _, _, _, _),  % Ensure the person does not already exist
    assertz(person(ID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, [])),
    (Father \= '' -> add_child(Father, ID) ; true),
    (Mother \= '' -> add_child(Mother, ID) ; true).

% Update birth or death date of a person
update_person(ID, BirthDate, DeathDate) :-
    retract(person(ID, Gender, Name, Surname, _, _, Father, Mother, Children)),
    assertz(person(ID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, Children)).

% Add a child to a parent's children list
add_child(ParentID, ChildID) :-
    person(ParentID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, Children),
    retract(person(ParentID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, Children)),
    assertz(person(ParentID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, [ChildID | Children])).

% Check if a person is alive
is_alive(ID) :-
    person(ID, _, _, _, _, DeathDate, _, _, _),
    var(DeathDate).  % DeathDate is unbound if the person is alive

% Calculate the age of a person
calculate_age(ID, Age) :-
    person(ID, _, _, _, date(BY, BM, BD), DeathDate, _, _, _),
    get_time(CurrentTime),
    stamp_date_time(CurrentTime, DateTime, 'UTC'),
    date_time_value(year, DateTime, CY),
    date_time_value(month, DateTime, CM),
    date_time_value(day, DateTime, CD),
    (   var(DeathDate)  % If the person is alive
    ->  Age is CY - BY - (CM < BM ; CM == BM, CD < BD)
    ;   DeathDate = date(DY, DM, DD),
        Age is DY - BY - (DM < BM ; DM == BM, DD < BD)).

% Calculate the level of a person in the family tree
calculate_level(ID, Level) :-
    calculate_level(ID, 0, Level).

calculate_level(ID, Acc, Level) :-
    person(ID, _, _, _, _, _, Father, _, _),
    (   Father == '' -> Level is Acc
    ;   NewAcc is Acc + 1,
        calculate_level(Father, NewAcc, Level)).

% Find relationships between people
relationship(Child, Parent, 'Baba') :-
    person(Child, _, _, _, _, _, Parent, _, _),
    person(Parent, male, _, _, _, _, _, _, _).

relationship(Child, Parent, 'Anne') :-
    person(Child, _, _, _, _, _, _, Parent, _),
    person(Parent, female, _, _, _, _, _, _, _).

relationship(Person1, Person2, 'Erkek Kardeş') :-
    person(Person1, male, _, _, _, _, Father, Mother, _),
    person(Person2, _, _, _, _, _, Father, Mother, _),
    Person1 \= Person2.

relationship(Person1, Person2, 'Kız Kardeş') :-
    person(Person1, female, _, _, _, _, Father, Mother, _),
    person(Person2, _, _, _, _, _, Father, Mother, _),
    Person1 \= Person2.

relationship(Person1, Person2, 'Abla') :-
    person(Person1, female, _, _, date(Y1, M1, D1), _, Father, Mother, _),
    person(Person2, _, _, _, date(Y2, M2, D2), _, Father, Mother, _),
    Person1 \= Person2,
    (Y1 < Y2 ; Y1 == Y2, M1 < M2 ; Y1 == Y2, M1 == M2, D1 < D2).

relationship(Person1, Person2, 'Abi') :-
    person(Person1, male, _, _, date(Y1, M1, D1), _, Father, Mother, _),
    person(Person2, _, _, _, date(Y2, M2, D2), _, Father, Mother, _),
    Person1 \= Person2,
    (Y1 < Y2 ; Y1 == Y2, M1 < M2 ; Y1 == Y2, M1 == M2, D1 < D2).

relationship(Person1, Person2, 'Amca') :-
    person(Person1, _, _, _, _, _, Grandfather, _, _),
    person(Person2, male, _, _, _, _, Grandfather, _, _),
    Person1 \= Person2,
    person(Person1, _, _, _, _, _, Father, _, _),
    Father \= Person2.

relationship(Person1, Person2, 'Hala') :-
    person(Person1, _, _, _, _, _, Grandfather, _, _),
    person(Person2, female, _, _, _, _, Grandfather, _, _),
    Person1 \= Person2,
    person(Person1, _, _, _, _, _, Father, _, _),
    Father \= Person2.

relationship(Person1, Person2, 'Dayı') :-
    person(Person1, _, _, _, _, _, _, Grandmother, _),
    person(Person2, male, _, _, _, _, _, Grandmother, _),
    Person1 \= Person2,
    person(Person1, _, _, _, _, _, _, Mother, _),
    Mother \= Person2.

relationship(Person1, Person2, 'Teyze') :-
    person(Person1, _, _, _, _, _, _, Grandmother, _),
    person(Person2, female, _, _, _, _, _, Grandmother, _),
    Person1 \= Person2,
    person(Person1, _, _, _, _, _, _, Mother, _),
    Mother \= Person2.

relationship(Person1, Person2, 'Kuzen') :-
    person(Person1, _, _, _, _, _, Grandfather, _, _),
    person(Person2, _, _, _, _, _, Grandfather, _, _),
    Person1 \= Person2,
    person(Person1, _, _, _, _, _, Father, _, _),
    Father \= Person2,
    person(Person2, _, _, _, _, _, _, _, Children2),
    member(Person1, Children2).

relationship(Person1, Person2, 'Yeğen') :-
    person(Person1, _, _, _, _, _, _, Parent, _),
    person(Person2, _, _, _, _, _, _, Parent, _),
    Person1 \= Person2,
    (   person(Person2, male, _, _, _, _, _, _, _) -> true
    ;   person(Person2, female, _, _, _, _, _, _, _)).

relationship(Person1, Person2, 'Enişte') :-
    marriage(Person2, Person3),
    relationship(Person1, Person3, 'Kız Kardeş').

relationship(Person1, Person2, 'Yenge') :-
    marriage(Person2, Person3),
    relationship(Person1, Person3, 'Erkek Kardeş').

relationship(Person1, Person2, 'Kayınvalide') :-
    marriage(Person2, Person3),
    relationship(Person3, Person1, 'Anne').

relationship(Person1, Person2, 'Kayınpeder') :-
    marriage(Person2, Person3),
    relationship(Person3, Person1, 'Baba').

relationship(Person1, Person2, 'Gelin') :-
    marriage(Person2, Person3),
    person(Person2, female, _, _, _, _, _, _, _),
    relationship(Person1, Person3, 'Baba').

relationship(Person1, Person2, 'Damat') :-
    marriage(Person2, Person3),
    person(Person2, male, _, _, _, _, _, _, _),
    relationship(Person1, Person3, 'Baba').

relationship(Person1, Person2, 'Bacanak') :-
    marriage(Person1, Person3),
    marriage(Person2, Person4),
    relationship(Person3, Person4, 'Erkek Kardeş').

relationship(Person1, Person2, 'Baldız') :-
    marriage(Person1, Person3),
    relationship(Person2, Person3, 'Kız Kardeş').

relationship(Person1, Person2, 'Görümce') :-
    marriage(Person1, Person3),
    relationship(Person2, Person3, 'Erkek Kardeş').

relationship(Person1, Person2, 'Kayınbirader') :-
    marriage(Person1, Person3),
    relationship(Person2, Person3, 'Erkek Kardeş').

% Marriage validity checks
valid_marriage(Person1, Person2) :-
    person(Person1, _, _, _, date(Y1, M1, D1), _, _, _, _),
    person(Person2, _, _, _, date(Y2, M2, D2), _, _, _, _),
    get_time(CurrentTime),
    stamp_date_time(CurrentTime, DateTime, 'UTC'),
    date_time_value(year, DateTime, CY),
    date_time_value(month, DateTime, CM),
    date_time_value(day, DateTime, CD),
    Age1 is CY - Y1 - (CM < M1 ; CM == M1, CD < D1),
    Age2 is CY - Y2 - (CM < M2 ; CM == M2, CD < D2),
    Age1 >= 18,
    Age2 >= 18,
    \+ relationship(Person1, Person2, _).

marry(Person1, Person2) :-
    valid_marriage(Person1, Person2),
    assertz(marriage(Person1, Person2)).

% Print family tree
print_family_tree :-
    findall(ID, person(ID, _, _, _, _, _, _, _, _), People),
    print_people(People).

print_people([]).
print_people([ID|Rest]) :-
    person(ID, Gender, Name, Surname, BirthDate, DeathDate, Father, Mother, Children),
    format('~w ~w ~w, born on ~w, died on ~w~n', [Gender, Name, Surname, BirthDate, DeathDate]),
    format('Father: ~w, Mother: ~w~n', [Father, Mother]),
    format('Children: ~w~n', [Children]),
    print_people(Rest).

% Translate relationship to Turkish
translate_relationship(Relation, Turkish) :-
    ( Relation = 'father' -> Turkish = 'Baba'
    ; Relation = 'mother' -> Turkish = 'Anne'
    ; Relation = 'brother' -> Turkish = 'Erkek Kardeş'
    ; Relation = 'sister' -> Turkish = 'Kız Kardeş'
    ; Relation = 'uncle' -> Turkish = 'Amca'
    ; Relation = 'aunt' -> Turkish = 'Hala'
    ; Relation = 'cousin' -> Turkish = 'Kuzen'
    ; Relation = 'nephew' -> Turkish = 'Yeğen'
    ; Relation = 'niece' -> Turkish = 'Yeğen'
    ; Relation = 'husband' -> Turkish = 'Koca'
    ; Relation = 'wife' -> Turkish = 'Karısı'
    ; Relation = 'son' -> Turkish = 'Oğul'
    ; Relation = 'daughter' -> Turkish = 'Kız'
    ; Relation = 'father-in-law' -> Turkish = 'Kayınpeder'
    ; Relation = 'mother-in-law' -> Turkish = 'Kayınvalide'
    ; Relation = 'brother-in-law' -> Turkish = 'Kayınbirader'
    ; Relation = 'sister-in-law' -> Turkish = 'Baldız'
    ; Relation = 'grandfather' -> Turkish = 'Dede'
    ; Relation = 'grandmother' -> Turkish = 'Büyükanne'
    ; Turkish = Relation).

% Example usage
:- add_person(id5, male, 'Mark', 'Doe', date(2005, 5, 5), _, id3, id4).
:- update_person(id1, date(1950, 1, 1), date(2021, 1, 1)).
:- is_alive(id4).
:- calculate_age(id1, Age), writeln(Age).
:- relationship(id3, id4, Rel), writeln(Rel).
:- print_family_tree.
