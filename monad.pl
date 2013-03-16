/*
 *
 *   copyright 2013 mark eschbach
 *
 *   licensed under the apache license, version 2.0 (the "license");
 *   you may not use this file except in compliance with the license.
 *   you may obtain a copy of the license at
 *
 *       http://www.apache.org/licenses/license-2.0
 *
 *   unless required by applicable law or agreed to in writing, software
 *   distributed under the license is distributed on an "as is" basis,
 *   without warranties or conditions of any kind, either express or implied.
 *   see the license for the specific language governing permissions and
 *   limitations under the license.
 */
:- module( monad, [
		monad/3,
		monad/2,
		monad_iterator/3
	]).
/** <module> monad An implementation of the monad construct in Prolog 

@author Mark Eschbach
@license apache2
 */
:- use_module(iterator).
:- dynamic monad_state( _Name, _Index, _NextState, _Value ).

monad( Name, Source, Monad ) :-
	Monad = monad( Source, 0, Name ) 
	.
abolish_monad( monad( _Source, _Index, Name ) ) :-
	retractall( monad_state( Name, _, _, _ ) )
	.

monad( monad( _Source, Index, Name )-monad( NextSource, NextIndex, Name ), Value ) :-
	monad_state( Name, Index, NextSource, Value ),!,
	NextIndex is Index +1
	.
monad( monad( Source, Index, Name )-monad( NextSource, NextIndex, Name ), Value ) :-
	NextIndex is Index+1,
	it_next( Source, NextSource, Value ),!,
	asserta( monad_state( Name, Index, NextSource, Value ) )
	.

monad_iterator( Iterator, Name, MonadicIterator ) :-
	monad( Name, Iterator, Monad ),
	iterator( MonadicIterator, monad_next, monad_done, Monad )
	.
abolish_iterator( Iterator ) :-
	iterator:context( Iterator, Monad ),
	abolish_monad( Monad )
	.

monad_done( monad( Source, Index, Name ) ) :-
	it_end( Source ),
	not( monad_state( Name, Index, _, _ ) )
	.
monad_next( Previous, Next, Value ) :-
	monad( Previous-Next, Value )
	.

