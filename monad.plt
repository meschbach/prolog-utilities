/*
 *
 *   Copyright 2013 Mark Eschbach
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */
:- begin_tests( monad ).
:- use_module( monad ).
:- use_module( iterator ).
:- use_module( testing ).

backtracking_alternate( Monad ) :-
	monad( Monad-_, Value ),
	Value = 5,
	!,fail
	.
backtracking_alternate( Monad ) :-
	monad( Monad-_, Value ),
	Value = 4
	.
backtracking_process( M0 ) :-
	monad( M0-M1, 1 ),
	monad( M1-M2, 2 ),
	monad( M2-M3, 3 ),
	backtracking_alternate( M3 )
	.

test('monad: does not call source on backtracking') :-
	range(1,5,Iterator),
	monad( backtracing_test_monad, Iterator, Monad ), !,
	backtracking_process( Monad )
	.

test('monad/3: binds output') :-
	range(1,5,Iterator),
	monad( monad_3_bound, Iterator, Monad ), !,
	ground( Monad )
	.
test('monad: provides first value from the iterator' ) :-
	range(1,5,Iterator),
	monad( monad_3_bound, Iterator, Monad ), !,
	monad( Monad-_, Value ),
	Value should_equal 1
	.

test('monad: provides first three values' ) :-
	range(1,5,Iterator),
	monad( monad_3_bound, Iterator, M0 ), !,
	monad( M0-M1, Value0 ),
	monad( M1-M2, Value1 ),
	monad( M2-_, Value2 ),
	[Value0, Value1, Value2] should_equal [1,2,3]
	.

:- dynamic bad_io_predicate/3.
test('monad: consistent with non deterministic input' ) :-
	retractall( bad_io_preidcate/1 ),
	asserta( bad_io_predicate(1) ),
	iterator( Iterator, bad_io_predicate_iterate, bad_io_predicate_end, -1 ),
	monad( monad_redo_with_nondet, Iterator, M0 ), !,
	monad( M0-M1, Value0 ),
	monad( M1-_, Value1 ),
	monad( M1-_, Value2 ),
	[Value0, Value1, Value2] should_equal [1,2,2]
	.

bad_io_predicate_iterate( _Current, Value, Value ) :-
	bad_io_predicate( Value ),
	retractall( bad_io_predicate ),
	Future is Value + 1,
	asserta( bad_io_predicate(Future) ),
	!
	.
bad_io_predicate_end( 5 ).

test('monad_iterator: provides a consistent iterator' ) :-
	range( 1, 5, Iterator ),
	monad_iterator( Iterator, monad_iterator_backtracking_test, M0 ),!,
	it_next( M0-M1, Value0 ),
	it_next( M1-M2, Value1 ),
	it_next( M1-M2, Value2 ),
	[Value0, Value1, Value2] should_equal [1,2,2]
	.

monad_iterator_alt( State ) :- it_next( State-_, 3), !.
monad_iterator_alt( State ) :- it_next( State-_, 2), !.

test('monad_iterator: alternate test') :-
	retractall( bad_io_preidcate/1 ),
	asserta( bad_io_predicate(1) ),
	iterator( Iterator, bad_io_predicate_iterate, bad_io_predicate_end, -1 ),!,
	it_next( Iterator-M1, 1 ),
	monad_iterator_alt( M1 )
	.
:- end_tests( monad ).
