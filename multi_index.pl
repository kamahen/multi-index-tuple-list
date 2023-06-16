% -*- mode: Prolog; coding: utf-8 -*-

%% Create a list of tuples and efficiently lookup, insert, delete
%% items.

:- module(multi_index, [xrb_visit/2,
                        rb_visit_ge/3,
                        rb_lookup_ge/4,
                        rb_in_ge/4]).

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

% New predicate rb_in_ge(+Key, -KeyFound, -Val, Tree).

rb_in_ge(KeyGe, Key, Val, t(_,Tree)) =>
    enum_ge(Key, Val, Tree, KeyGe).

enum_ge(Key, Val, black(L,K,V,R), KeyGe) =>
    L \= '',
    enum_cases_ge(Key, Val, L, K, V, R, KeyGe).
enum_ge(Key, Val, red(L,K,V,R), KeyGe) =>
    enum_cases_ge(Key, Val, L, K, V, R, KeyGe).
enum_ge(_Key, _Val, _Tree, _KeyGe) => fail.

enum_cases_ge(Key, Val, L, NodeKey, _, _, KeyGe) :-
    Key @=< NodeKey,
    enum_ge(Key, Val, L, KeyGe).
enum_cases_ge(Key, Val, _, Key, Val, _, KeyGe) :-
    Key @>= KeyGe.
enum_cases_ge(Key, Val, _, _, _, R, KeyGe) :-
    enum_ge(Key, Val, R, KeyGe).


% Rewrite rb_visit/2 using DCGs:

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

% New predicate: rb_visit_ge/3.

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

% New predicate: rb_lookup_ge/4

% There's a more efficient way of doing this deterministically, but
% the code is more complex (see earlier versions of this module, which
% have the detemrinistic code, although it would need some
% modifications to be the best algorithm).

rb_lookup_ge(Key, KeyFound, Val, T) :-
    once(rb_in_ge(Key, KeyFound, Val, T)).

:- if(false).

tree_node_left(  red(L,_,_,_), L).
tree_node_left(black(L,_,_,_), L).

tree_node_key(  red(_,K,_,_), K).
tree_node_key(black(_,K,_,_), K).

tree_node_value(  red(_,_,V,_), V).
tree_node_value(black(_,_,V,_), V).

tree_node_right(  red(_,_,_,R), R).
tree_node_right(black(_,_,_,R), R).

tree_node_kv(  red(_,K,V,_), K,V).
tree_node_kv(black(_,K,V,_), K,V).

:- endif.

