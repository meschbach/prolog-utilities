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
:- begin_tests( iterator ).
:- use_module( iterator ).

test_source_capture( Value, Value, [], Value ).
test_source_end( Value, Value ).

test_source_list_end( [] ).
test_source_list( [], [], [] ).
test_source_list( [H|T], T, H ). 

test('iterator/4: Succeeds') :-
	iterator( _Source, test_source_read, test_source_end, start)
	.
test('it_next/3: Provides state value to predicate') :-
	iterator( S0, test_source_capture(start), test_source_list_end, start),
	it_next( S0, _, _Value)
	.
test('it_next/3: Chains state' ) :-
	iterator( S0, test_source_list, test_source_list_end, [test, values]),
	it_next( S0, S1, test),
	it_next( S1, _S2, values)
	.

test('it_end/1: calls end predicate') :-
	iterator( Source, fail, test_source_end(Value), []), 
	it_end( Source ),
	Value = []
	.

test('it_next/3: fails at the end', fail) :-
	iterator( S0, test_source_list, test_source_list_end, []),
	it_next( S0, _, _ )
	.

test('range_iterator/3: Fails if Start > End', fail ) :-
	range( 10, 5, _Iterator)
	.
test('range_iterator/3: Initial Value provide start') :-
	range( 2006, 2010, Iterator ),
	it_next( Iterator-_I1, 2006 )
	.
test('range_iterator/3: Ends at specified value') :-
	range( 2006, 2007, Iterator ),
	it_next( Iterator-I0, 2006 ),
	it_end( I0 )
	.

:- end_tests( iterator ).
