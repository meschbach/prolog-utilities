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

test('range/3: head is start of the range') :-
	range(1,100,List),
	List = [H|_],
	H should_equal 1
	.

test('range/3: Generate entire range') :-
	range(1,3, List),
	List should_equal [1,2,3]
	.

test('range/3: straight list unification') :-
	range(5, 10, List),
	List should_unify_with [5,6,7,8,9,10]
	.

:- end_tests( dynamic_list).

