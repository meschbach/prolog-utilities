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
:- use_module( testing ).
:- use_module( iterator ).
:- use_module( dynamic_list ).
:- use_module( pure_input ).
:- use_module( service_template ).
:- use_module( input ).
:- use_module( monad ).

%
% Runs all tests
%
do_tests :-
	load_test_files([]),
	run_tests,
	halt
	.
