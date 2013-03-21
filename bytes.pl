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
		byte_domain/1,
		bytes/1,
		network_uint32/2,
	  network_uint32/3
	]).
/** <module> bytes A module for manipulating lists bytes 

@author Mark Eschbach
@license apache2
 */
:- use_module( list_ops ).

%% byte( -Value ) is det.
%
% Determines if the given integer is within the bounds of a
% byte (8-bit octet).
%
byte( Value ) :- ground(Value), -1 < Value, Value < 256.
byte_domain( Value ) :- freeze( Value, byte(Value) ). 

bytes( [] ).
bytes( [H|T] ) :- byte_domain(H), bytes(T).

%% network_uint32( +Uint32, -Bytes ) is det.
%
% Converts between Uint32 and network byte order
% of Bytes.
%
network_uint32( Uint32, Bytes ) :-
	Bytes = [Byte0, Byte1, Byte2, Byte3],
	bytes( Bytes ),
	(
		ground( Bytes ),
		Uint32 is
			( Byte0 << 24
			\/ Byte1 << 16
			\/ Byte2 << 8
			\/ Byte3
			)
	;
		ground( Uint32 ),
		Byte0 is (Uint32 >> 24) /\ 0xff,
		Byte1 is (Uint32 >> 16) /\ 0xff,
		Byte2 is (Uint32 >> 8) /\ 0xff,
		Byte3 is Uint32 /\ 0xff
	)
	.
network_uint32( Uint32,
		Input,
		Remainder 
	) :-
		append( Bytes, Remainder, Input ),
		network_uint32( Uint32, Bytes )
	.

