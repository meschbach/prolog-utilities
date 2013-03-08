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
:- module( service_template, [
	repl_service/4,
	repl_transition/2
	]).

/** <module> service_template Predicates for creating and managing services 

This module provides templates for REPL based services.

@author Mark Eschbach
@license apache2
*/

:- meta_predicate repl_service( 2, 2, 2, + ).

repl_service( Read, Process, Output, Start-End ) :-
	repl_loop( Read, _, Process, Output, _, Start-End)
	.	

repl_loop( _Read, _ReadState, _Strategy, _Output, _OutputState, End-End ) :- !.
repl_loop(  Read, ReadI-ReadO, Strategy,  Output,  OutI-OutO,   Start-End ) :-
	call( Read, ReadI-ReadM, InputRecord),!,
	call(Strategy, InputRecord-OutputRecord, Start-Next),!,
	call(Output, OutI-OutM, OutputRecord),!,
	repl_loop( Read, ReadM-ReadO, Strategy, Output, OutM-OutO, Next-End )
	.

repl_transition( Start-End, Start-End).

