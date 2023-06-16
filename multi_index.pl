% -*- mode: Prolog; coding: utf-8 -*-

%% Create a list of tuples and efficiently lookup, insert, delete
%% items.

:- module(multi_index, [xrb_visit/2,
                        rb_visit_ge/3,
                        rb_lookup_ge/4]).

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

rb_lookup_ge(_Key, _KeyFound, _Val, t(black('', A, B, ''), black('', A, B, ''))) => fail.
rb_lookup_ge(Key, KeyFound, Val, T) =>
    % TODO: avoid calling rb_max/3 by adding a flag as to whether a
    %       left subtree has been visited and adjusting lookup_ge_/8
    %       accordingly (see also the code for rb_max/3).
    rb_max(T, KA,VA),
    Key @=< KA,
    T = t(_,Tree),
    lookup_ge(Key, KeyFound, Val, Tree, KA,VA).

% lookup_ge(+Key, -KeyFound, -Val, +Tree, +KBest, +VBest)
%   KBest-VBest are the best key-value found so far

lookup_ge(_Key, KeyFound, Val, black('',_,_,''), Kbest,Vbest) => KeyFound=Kbest, Val=Vbest.
lookup_ge(Key, KeyFound, Val, Tree, Kbest,Vbest) =>
    tree_node_key(Tree,KA),
    compare(Cmp,KA,Key),
    lookup_ge_(Cmp,Key, KA, KeyFound,Val,Tree, Kbest,Vbest).

lookup_ge_(>, K, KA, KeyFound, V, Tree, Kbest,Vbest) :-
    tree_node_left(Tree,NTree),
    min_key(KA, Tree, Kbest,Vbest, Kbest2,Vbest2), 
    lookup_ge(K, KeyFound, V, NTree, Kbest2,Vbest2).
lookup_ge_(<, K, _KA, KeyFound, V, Tree, Kbest,Vbest) :-
    tree_node_right(Tree,NTree),
    lookup_ge(K, KeyFound, V, NTree, Kbest,Vbest).
lookup_ge_(=, _K, KA, KeyFound, V, Tree, _Kbest,_Vbest) :-
    KeyFound = KA,
    tree_node_value(Tree,V).

min_key(K, T, Kbest,_,     K2,V2), K @< Kbest => K2 = K, tree_node_value(T, V2).
min_key(_, _, Kbest,Vbest, K2,V2) => K2 = Kbest, V2 = Vbest.

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

