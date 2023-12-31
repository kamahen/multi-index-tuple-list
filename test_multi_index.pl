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

test(lookup_ge, KV == b-2) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_lookup_ge(a, KV, T).
test(lookup_ge, KV == b-2) :-
    list_to_rbtree([b-2,c-3,d-4,e-5], T), rb_lookup_ge(b, KV, T).
test(lookup_ge, KV == c-3) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_lookup_ge(b2, KV, T).
test(lookup_ge, KV == c-3) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_lookup_ge(c, KV, T).
test(lookup_ge, KV == d-4) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_lookup_ge(c2, KV, T).
test(lookup_ge, KV == e-5) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_lookup_ge(d2, KV, T).
test(lookup_ge, KV == f-6) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_lookup_ge(e5, KV, T).
test(lookup_ge, KV == f-6) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_lookup_ge(f, KV, T).
test(lookup_ge, fail) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_lookup_ge(d2, _KV, T).

test(in_ge, all(KV == [b-2,c-3,d-4,e-5,f-6])) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_in_ge(a, KV, T).
test(in_ge, all(KV == [b-2,c-3,d-4,e-5])) :-
    list_to_rbtree([b-2,c-3,d-4,e-5], T), rb_in_ge(b, KV, T).
test(in_ge, all(KV == [c-3,d-4])) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_in_ge(b2, KV, T).
test(in_ge, all(KV == [c-3,d-4])) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_in_ge(c, KV, T).
test(in_ge, all(KV == [d-4])) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_in_ge(c2, KV, T).
test(in_ge, all(KV == [e-5,f-6])) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_in_ge(d2, KV, T).
test(in_ge, all(KV == [f-6])) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_in_ge(e5, KV, T).
test(in_ge, all(KV == [f-6])) :-
    list_to_rbtree([b-2,c-3,d-4,e-5,f-6], T), rb_in_ge(f, KV, T).
test(in_ge, fail) :-
    list_to_rbtree([b-2,c-3,d-4], T), rb_in_ge(d2, _KV, T).

rb_next(Tree, Key, NextKey-NextValue) :-
    rb_next(Tree, Key, NextKey, NextValue).

rb_lookup_ge(Key, KeyFound-ValueFound, Tree) :-
    rb_lookup_ge(Key, KeyFound, ValueFound, Tree).

rb_in_ge(Key, KeyFound-ValueFound, Tree) :-
    rb_in_ge(Key, KeyFound, ValueFound, Tree).

:- end_tests(basic).
