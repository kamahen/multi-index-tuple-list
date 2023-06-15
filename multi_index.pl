% -*- mode: Prolog; coding: utf-8 -*-

%% Create a list of tuples and efficiently lookup, insert, delete
%% items.

:- module(multi_index, [xrb_visit/2, rb_visit_ge/3]).

:- encoding(utf8).

:- use_module(library(ordsets)).
:- use_module(library(rbtrees)).

% https://swi-prolog.discourse.group/t/facts-vs-local-knowledge/6636/9

data_index(node(From,_,_),  Index, Key), ground(From)    => Index = 1, Key=From.
data_index(node(_,Edge,To), Index, Key), ground(Edge-To) => Index = 2, Key=Edge-To.
data_index(node(_,_,To),    Index, Key), ground(To)      => Index = 3, Key=To.
data_index(node(_,Edge,To), Index, Key), ground(Edge)    => Index = 2, Key=Edge-To.
data_index(node(_,_,_),     Index, _)                    => Index = 0.

data([node(n1, road, n2),
      node(n1, train, n3),
      node(n2, road, n3),
      node(n3, road, n4)]).


user:portray(Term) :-
    % in the following, format/2 is used because
    % print_message(warning, E) gives an infinite recursion.
    E = error(ErrorTerm, _Context), % avoid trapping abort, timeout, etc.
    % _Context = context(index:index_portray/1,_)
    catch(index_portray(Term),
          E,
          format('portrayEXCEPTION/~q:~q', [ErrorTerm, Term])).

%! index_portray is semidet.
%  TODO: change all to use index_portray_unify (see 1st clause).

index_portray(Rbtree) :-
    is_rbtree(Rbtree),
    !,
    rb_visit(Rbtree, Pairs),
    format('~p', [rbtree:Pairs]).

xrb_visit(t(_,T),Lf) :-
    phrase(visit(T),Lf).

visit(black('',_,_,_)) --> !, [].
visit(red(L,K,V,R)) --> !,
    visit(L),
    [K-V],
    visit(R).
visit(black(L,K,V,R)) --> !,
    visit(L),
    [K-V],
    visit(R).


rb_visit_ge(t(_,T),Lf, Key0) :-
    phrase(visit_ge(T, Key0), Lf).

visit_ge(black('',_,_,_), _Key0) --> !, [].
visit_ge(red(L,K,V,R), Key0) --> !,
    (   { K @>= Key0 }
    ->  visit_ge(L, Key0),
        [K-V]
    ;   []
    ),
    visit_ge(R, Key0).
visit_ge(black(L,K,V,R), Key0) --> !,
    (   { K @>= Key0 }
    ->  visit_ge(L, Key0),
        [K-V]
    ;   []
    ),
    visit_ge(R, Key0).
