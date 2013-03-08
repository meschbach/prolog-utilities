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
:- begin_tests( service_template ).
:- use_module( service_template ).
:- use_module( testing ).

test_read_strategy( _, test ).
test_write_strategy( _, _ ).

exit_strategy( _, start-end ). 

test('repl_service: exit strategy') :-
	repl_service( test_read_strategy, exit_strategy, test_write_strategy, start-end )
	.

state_transition_test( _, State ) :-
	repl_transition( State, Count-NextCount ),
	NextCount is Count + 1.

test('repl_service: passes state from start to end') :-
	repl_service( test_read_strategy, state_transition_test, test_write_strategy, 1-10 )
	.

read_record_op( Input-[], State ) :-
	Input = 10,
	repl_transition( State, _-end ).
read_record_op( Record-Record, _State ).
read_record_input( [H|T]-T, H ). 
	
test('repl_service: reads record') :-
	repl_service( read_record_input, read_record_op, test_write_strategy, start-end )
	.

write_record_op( _I-write_test, _-end ).
write_record_output( _, write_test ). 
test('repl_service: writes output record') :-
	repl_service( test_read_strategy, write_record_op, write_record_output, start-end )
	.

:- end_tests( service_template).

