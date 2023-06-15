% -*- mode: Prolog; coding: utf-8 -*-

:- module(test_multi_index,
	  [ test_multi_index/0
	  ]).

:- encoding(utf8).

:- use_module(library(plunit)).
:- use_module(multi_index).

test_multi_index :-
    run_tests([ basic
               ]).

:- begin_tests(basic).

test(next, NextKV == d-4) :-
    list_to_rbtree([a-1, b-2, d-4], Tree),
    rb_next(Tree, b, NextKV).
test(next, fail) :-
    list_to_rbtree([a-1, b-2, d-4], Tree),
    rb_next(Tree, d, _NextKV).
test(next, fail) :-
    list_to_rbtree([a-1, b-2, d-4], Tree),
    rb_next(Tree, c, _NextKV).
test(visit_ge, KVs == [d-4]) :-
    list_to_rbtree([a-1, b-2, d-4], Tree),
    rb_visit_ge(Tree, KVs, c).
test(visit_ge, KVs == [d-4]) :-
    list_to_rbtree([a-1, b-2, d-4], Tree),
    rb_visit_ge(Tree, KVs, d).

rb_next(Tree, Key, NextKey-NextValue) :-
    rb_next(Tree, Key, NextKey, NextValue).

:- end_tests(basic).
