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
:- begin_tests( testing ).
:- use_module( testing ).

test('should_equal/2: empty list is equal to any empty list') :-
	with_output_to(chars(_), should_equal([],[])).

test('should_equal/2: Fails with two unbound variables', fail) :-
	with_output_to(chars(_), should_equal( _, _ )).

test('should_equal/2: two lists with unbound elemnts fail', fail) :-
	with_output_to(chars(_), should_equal( [_A,_B], [_C,_D]))
	.
test('should_equal/2: with left unbound, right bound should fail', fail ):-
	with_output_to(chars(_), should_equal( _A, test_value))
	.

test('should_unify_with/2: with left unbound, right bound should succeed' ):-
	with_output_to(chars(_), should_unify_with( _, 12 ))
	.
test('should_unify_with/2: with left unbound, right bound list' ):-
	with_output_to(chars(_), should_unify_with( _, [12,13,14] ))
	.

:- end_tests( testing ).

