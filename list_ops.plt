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
:- begin_tests( list_ops ).
:- use_module( list_ops ).

base_list( [1,2,3,5,8,13,21] ).

test('sublist/4: Grounded whole and count provides grounded part and end' ) :-
	base_list( WholeList ),
	sublist( WholeList, 3, Part, End),!,
	ground( Part ),
	ground( End )
	.

test('sublist/4: only provides a given subset') :-
	base_list( WholeList ),
	sublist( WholeList, 3, [1,2,3], [5,8,13,21] )
	.

:- end_tests( list_ops ).
