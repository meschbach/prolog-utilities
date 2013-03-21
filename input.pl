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
:- module( input, [
		stream_as_characters/2,
		stream_as_bytes/2
	]).
/** <module> input Common operations for managing input

@author Mark Eschbach
@license apache2
 */
:- use_module( iterator ).

stream_as_characters( Stream, Iterator ) :-
	State = character_stream( Stream ),
	iterator( Iterator, read_character_stream, end_of_character_stream, State)
	.
read_character_stream( 
		character_stream( Stream ),
		character_stream( Stream ),
		Character ) :-
	get_char( Stream, Character )
	.
end_of_character_stream(
		character_stream( Stream ) 
	) :-
	at_end_of_stream( Stream ).

stream_as_bytes( Stream, Iterator ) :-
	State = byte_stream( Stream ),
	iterator( Iterator, read_byte_stream, end_of_byte_stream, State )
	.
read_byte_stream(
		byte_stream( Stream ),
		byte_stream( Stream ),
		Byte
	) :-
		get_byte( Stream, Byte )
	.
end_of_byte_stream( byte_stream( Stream ) ) :- at_end_of_stream( Stream ).

