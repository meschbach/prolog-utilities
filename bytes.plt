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
:- begin_tests( bytes ).
:- use_module( bytes ).
:- use_module( testing ).

/***************************************
 * Byte domains
 **************************************/
test('byte/1: with good value') :-
	byte( 128 )
	.
test('byte/1: with minimum value') :-
	byte( 0 )
	.
test('byte/1: with maximum value') :-
	byte(255)
	.
test('byte/1: beyond maximum', fail) :- byte(256).
test('byte/1: below minimum', fail) :- byte(-1).

/***************************************
 * Byte Domains
 **************************************/
test('byte_domain/1: with unbound -> succeeds when within domain') :-
	byte_domain(A),
	A = 127 
	.

/***************************************
 * Verify a given list contains a
 * series of bytes
 **************************************/
test('bytes/1: All valid') :-
	bytes([128,0,255]).
test('bytes/1: All invalid', fail) :-
	bytes([-1,1024, 721]).
test('bytes/1: mixed of valid and invalid', fail) :-
	bytes([127, 15, 12, 18, 256]).

/* domain functionality */
test('bytes/1: with unbound values') :-
	bytes([X,Y,Z,128]),
	X = 8,
	Y = 20,
	Z = 6
	.

/***************************************
 * Converting a series of a bytes within
 * a list into an integer
 **************************************/
test('network_uint32/2: bytes 0x01 to integer works') :-
	network_uint32( Result, [0,0,0,1]),!,
	Result should_unify_with 1
	.
test('network_uint32/2: bytes 0xf0 << 24 to integer works') :-
	Expected is 0xf0 << 24,
	network_uint32( Result, [0xf0, 0, 0, 0] ),!,
	Result should_unify_with Expected
	.
test('network_uint32/3: bytes 0xf0 << 24 to integer leaves remaining bytes') :-
	network_uint32( _Result, [0xf0, 0, 0, 0, 100, 200], Remainder),!,
	Remainder should_unify_with [100, 200]
	.

/***************************************
 * Converting an integer into a network
 * uint32 bytes
 **************************************/
test('network_uint32/3: Convert 0 into a series of bytes') :-
	network_uint32( 0, Value ),!,
	Value should_unify_with [0, 0, 0, 0]
	.
test('network_uint32/3: Convert 256 into a series of bytes') :-
	network_uint32( 256, Value ),!,
	Value should_unify_with [0, 0, 1, 0]
	.

:- end_tests( bytes ).
