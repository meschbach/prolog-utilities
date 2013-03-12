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
:- begin_tests( dynamic_list ).
:- use_module( dynamic_list ).
:- use_module( testing ).
:- use_module( iterator ).

test('dynamic_list_template/2: Provides an unbound list') :-
	range(1, 10, Iterator),
	dynamic_list_template( Iterator, List ),
	var(List)
	.
test('dynamic_list_template/2: Unifies with expected value') :-
	range(1, 10, Iterator),
	dynamic_list_template( Iterator, List ),
	List = [H|_],
	H should_unify_with 1
	.
test('dynamic_list_template/2: Unifies with entire list') :-
	range(2, 8, Iterator),
	dynamic_list_template( Iterator, List ),
	List should_unify_with [2,3,4,5,6,7]
	.
test('dynamic_list_template/2: Given list, provides an iterator', fail) :-
	dynamic_list_template( Iterator, [test, from, list] ),
	it_next( Iterator-I0, test ),
	it_next( I0-I1, from ),
	it_next( I1-I2, list ),
	it_end( I2 )
	.

:- end_tests( dynamic_list).

