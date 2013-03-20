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
:- module( list_ops, [
		sublist/4
	]).
/** <module>  sublist Provides common operations on lists

@author Mark Eschbach
@license apache2
 */
sublist( WholeList, SplitPoint, Part, End ) :-
	append( Part, End, WholeList ),
	length( Part, SplitPoint )
	.

