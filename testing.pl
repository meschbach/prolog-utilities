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
:- module( testing, [
		should_equal/2,
		should_unify_with/2,
		op( 100, xfy, should_equal ),
		op( 100, xfy, should_unify_with )
	]).
:- use_module( output ).
/** <module> testing Provides assertions with failure details. 

I ran into the problem of receiving test failures without details on the
resulting data of the failure.  The predicates provided by this module will
print the expected and actual values upon failure.

@author Mark Eschbach
@license apache2
*/

%% should_equal( +, + ) is det.
%
% Ensures the bound variables refer to the same value.
%
should_equal( A, B ) :-
	not( ground(A) ),
	!,
	write_failure(['Expected ',A,' to be bound and equal to ',B]),
	fail.

should_equal( Value, Value ) :- ground(Value), !.
should_equal( ListA, ListB ) :-
	is_list( ListA ),
	is_list( ListB ),
	(
		elements_should_equal( ListA, ListB )
	;
		!,
		write(' Expected '),
		write_list( ListA ),
		write(' should equal '),
		write_list( ListB ),
		fail
	).
should_equal( A, B ) :-
	write_failure([' Expected ', A,' to equal ', B]),
	!, fail
	.
write_failure( [] ).
write_failure( [H|T] ) :- write( H ), write_failure(T).

%% should_unify_with( ?, ? )
%
% Ensures the two given variables may be unified
%
should_unify_with( A, B ) :- A = B, !.
should_unify_with( A, B ) :-
	write(' Expected '), write(A), write(' got '), write( B ),
	fail
	.

%
% Ensure the elments of a given list are equal
%
elements_should_equal( [], [] ) :- !.
elements_should_equal( [], _ ) :- !, fail.
elements_should_equal( _, [] ) :- !, fail.
elements_should_equal( [H1|T1], [H2|T2] ) :-
	H1 should_equal H2,
	elements_should_equal( T1, T2 )
	.

