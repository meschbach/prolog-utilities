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
:- module( bytes, [
		byte/1,
		bytes/1,
	  network_uint32/3
	]).
/** <module> bytes A module for manipulating lists bytes 

@author Mark Eschbach
@license apache2
 */
:- use_module( list_ops ).

byte( Value ) :- -1 < Value, Value < 256.
bytes( [] ).
bytes( [H|T] ) :- byte(H), bytes(T).

network_uint32( Uint32,
		Input,
		Remainder 
	) :-
		sublist( Input, 4, Bytes, Remainder ),
		bytes( Bytes ),
		Bytes = [Byte0, Byte1, Byte2, Byte3],
		Uint32 is 
			( Byte0 << 24
			\/ Byte1 << 16
			\/ Byte2 << 8
			\/ Byte3
			)
	.
