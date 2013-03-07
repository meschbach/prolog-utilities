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
:- module( dynamic_list, [
		range/3, 
		dynamic_list_template/4
	]).
/** <module> dynamic_list A template to construct lists on the fly 

This module provides a template predicate, dynamic_list_template/4, to generate lists one element at a time.  This prevents a Prolog application from needing to load an entire data stream, or generate an entire list in memory.

An example implementation for creating a list of numbers within an array
is included as an example.

@author Mark Eschbach
@license apache2
*/

%% range_has_ended( ?, ?)
%
% Boundry check for the top of the list
%
range_has_ended( End, range( Current ) ) :- End < Current.

%% range_iterate( ?PreviousState, ?NextState, ?Value)
%
% If we have not reached the top of the list, we use this predicate to
% generate the next solution
%
range_iterate( 
	range( Current ),
	range( Next ),
	Current
	) :- 
	Next is Current + 1
	.

%% range( +Start, +End, ?List )
%
% Creates a new list containing all the elements within the range between
% Start and End.  Start must be less than or equal to end.
%
range( Start, End, List ) :-
	dynamic_list_template(
		range_has_ended(End),
		range_iterate,
		range( Start ),
		List
		).

%% dynamic_list_template( +HasEnded, +Next, ?InitalState, ?List )
%
% Delcares a new dynamically growing list.
%
% @param HasEnded msut be a predicate accepting the current state as the last parameter.  If this predicate succeeds, then the list will end.
% @param Next must be a predicate accepting the previous state, the next state, and the resulting value from the transition.
% @param InitialState is the seed state passed as the 'current' or 'previous' state to get the state machine rolling.
% @param List is a list containg the elements to be generated.  Elements are generated according to the contract of binding a variable for freeze/2.
%
dynamic_list_template( HasEnded, Next, InitialState, List ) :-
  dynamic_list_iterate( 
  	List, 
  	dynamic_list_state( HasEnded, Next ),
  	InitialState
  )
	.
%
% Attmepts to terminate the given list, if our HasEnded predicate succeeds.
%
dynamic_list_iterate( List, dynamic_list_state(HasEnded, _Next), PreviousState ) :-
	call(HasEnded, PreviousState),!,
	List = [].
%
% Transitions the given iterator to the next state, capturing binding the next
% value within the list.
%
dynamic_list_iterate( [H|T], Iterator, PreviousState ) :-
  Iterator = dynamic_list_state(_IsLast, Next),
	call(Next, PreviousState, NextState, H),
	freeze( T, dynamic_list_iterate( T, Iterator, NextState ))
	.

