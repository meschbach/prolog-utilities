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
:- begin_tests( pure_input ).
:- use_module( iterator ).
:- use_module( pure_input ).
:- use_module( dynamic_list ).
:- use_module( testing ).

test('pure_input:character_iterator/2: values tests',[
			setup( zebra_file( Stream ) ),
			cleanup( close( Stream ) )
		]) :-
	pure_input:character_iterator( Stream, Iterator ),
	dynamic_list_template( Iterator, List ),
	!,
	List should_unify_with [z,e,b,r,a,'\n']
	.

backtrack_test( State ) :- it_next(State-_Next, Value), Value = o.
backtrack_test( State ) :- it_next(State-_Next, Value), Value = e.

test('pure_input:character_iterator/2: Backtrack test',[
			setup( zebra_file( Stream ) ),
			cleanup( close( Stream ) )
		]) :-
	pure_input:character_iterator( Stream, Iterator ),
	!,
	it_next( Iterator-I0, z ),
	backtrack_test( I0 )
	.

test('pure_input:byte_iterator/2: recieves expected values', [
		setup( binary_zebra_file( Stream ) ),
		cleanup( close( Stream ) )
	]) :-
		atom_codes( z, [ZCode] ),
		byte_iterator( Stream, Iterator ),!,
		it_next( Iterator-_, Byte ), 
		Byte should_unify_with ZCode 
	.

byte_backtracking_template( State, ExpectedAtom ) :-
	atom_codes( ExpectedAtom, [Expected] ),!,
	it_next( State-_, Value ),
	Value = Expected
	.
byte_backtracking( State ) :-
	byte_backtracking_template( State, a )
	.
byte_backtracking( State ) :-
	byte_backtracking_template( State, z )
	.

test('pure_input:byte_iterator/2: backtracking works', [
		setup( binary_zebra_file( Stream ) ),
		cleanup( close( Stream ) )
	]) :-
		byte_iterator( Stream, Iterator ),!,
		byte_backtracking( Iterator )
	.

/***************************************
 * Bucket Iterator  
 **************************************/
test('bucket_iterator: gives empty iterator with empty list' ) :-
	bucket_iterator( [], Iterator ),!,
	it_end( Iterator )
	.
test('bucket_iterator: provides given length of bucket' ) :-
	Example = [0,1,2,3,4,5,6,7,8,9,10,11],
	bytes:network_uint32( 12, Header ),
	append( Header, Example, Bytes ),
	bucket_iterator( Bytes, Iterator ),!,
	it_next( Iterator-_, Result ), !,
	Result should_unify_with Example
	.
:- end_tests( pure_input ).

