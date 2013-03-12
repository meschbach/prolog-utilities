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
		dynamic_list_template/2
	]).
:- use_module( iterator ).
/** <module> dynamic_list A template to construct lists on the fly 

Provides a template for converting an iterator into a list.  This is useful for sources
such as streams, which could incrementally provide values.  In the caes of an incremental stream,
Prolog would dynamically fill in values as needed.

@author Mark Eschbach
@license apache2
*/

%% dynamic_list_template( ?Iterator, ?List )
%
% Delcares a list to be fed values dynamically from an array.
%
% @param Iterator is the backing geneartor for the list
% @param List is the list to generate value in
%
dynamic_list_template( Iterator, List ) :-
	ground(Iterator),
	freeze( List,
		dynamic_list_iterate(
			List,
			Iterator
		)
	)
	.

%
% Checks to ensure we have not yet reached the end of our list
%
dynamic_list_iterate( [], Iterator ) :- it_end( Iterator ).
%
% Transitions the given iterator to the next state, capturing binding the next
% value within the list
%
dynamic_list_iterate( [H|T], Iterator ) :-
	it_next( Iterator, Next, H ),
	freeze( T, dynamic_list_iterate( T, Next ))
  .

