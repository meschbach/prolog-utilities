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
:- module( iterator, [
	iterator/4,
	it_next/2,
	it_next/3,
	it_end/1,
	is_iterator/1,
	state/2,
	range/3
	]).

/** <module> input Utilities for logical input

@author Mark Eschbach
@license apache2
*/

:- meta_predicate iterator( ?, 3, 2, ? ).
%% iterator( ?Source, :Read, :End, ?State ) is det.
iterator( Source, ReadModule:ReadOp, EndModule:EndOp, State ) :-
	Source = iterator( ReadModule:ReadOp, EndModule:EndOp, State )
	.

it_next( S0-S1, Value ) :- it_next( S0, S1, Value ). 
it_next( Source0, _Source1, _Value ) :- it_end(Source0), !, fail.
it_next( Source0, Source1, Value ) :-
	iterator( Source0, ReadMod:ReadOp, EndMod:End, State ),
	iterator( Source1, ReadMod:ReadOp, EndMod:End, Next),
	call( ReadMod:ReadOp, State, Next, Value )
	.

it_end( S0-S0 ) :- it_end( S0 ).
it_end( Source ) :-
	iterator( Source, _:_, Module:End, State ),
	call( Module:End, State )
	.

is_iterator( Iterator ) :- Iterator = iterator(_:_,_:_,_).
state( Iterator, Context ) :- Iterator = iterator( _:_, _:_, Context ).

range( Start, End, Iterator ) :-
	End >= Start,
	iterator(Iterator, range_next, range_end( End ), Start)
	.

range_end( End, End ).
range_next( Previous, Next, Previous ) :- Next is Previous + 1.

