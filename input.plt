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
:- begin_tests( input ).
:- use_module( input ).
:- use_module( iterator ).
:- use_module( testing ).
:- use_module( dynamic_list ).

test('source_as_characters/2: Builds an iterator', [
		setup( alphabet_file( Stream ) ),
		cleanup( close( Stream ) )
	]) :-
		stream_as_characters( Stream, Iterator ),
		is_iterator( Iterator )
	.
test('source_as_characters/2: Grounds the iterator', [
		setup( alphabet_file( Stream ) ),
		cleanup( close( Stream ) )
	]) :-
		stream_as_characters( Stream, Iterator ),
		ground(Iterator)
	.
test('source_as_characters/2 with alphabet: provides first character',[
		setup( alphabet_file( Stream ) ),
		cleanup( close( Stream ) )
	]) :-
		stream_as_characters( Stream, Iterator ),
		it_next( Iterator, _, Character ),
		a should_unify_with Character
	.

test('source_as_characters/2 with zebra: Provides first character', [
			setup( zebra_file( Stream ) ),
			cleanup( close( Stream ) )
		]) :-
	stream_as_characters( Stream, Iterator ),
	it_next( Iterator, _, Character ),
	z should_unify_with Character
	.

test('source_as_characters/2 with zebra: Provides entire file',[
			setup( zebra_file( Stream ) ),
			cleanup( close( Stream ) )
		]) :-
	stream_as_characters( Stream, Iterator ),
	dynamic_list_template( Iterator, List ),
	!,
	[z,e,b,r,a,'\n'] should_unify_with List
	.

:- end_tests( input ).

