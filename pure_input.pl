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
:- module( pure_input, [
		character_iterator/2,
		byte_iterator/2,
		bucket_iterator/2
	]).
/** <module> pure_input Pure logical input streams 

@author Mark Eschbach
@license apache2
 */
:- use_module( input ).
:- use_module( monad ).

%% bucket_iterator( +Stream, -Iterator ) is det.
%
% Creates an iterator which provides a series of
% bytes.
%
character_iterator( Stream, Iterator ) :-
	stream_as_characters( Stream, Imperative ),
	monad_iterator( Imperative, Stream, Iterator )
	.

%% bucket_iterator( +Stream, -Iterator ) is det.
%
% Creates an iterator which provides a series of
% bytes.
%
byte_iterator( Stream, Iterator ) :-
	stream_as_bytes( Stream, Imperative ),
	monad_iterator( Imperative, Stream, Iterator)
	.

%% bucket_input( -List, +Iterator ) is det.
%
% Establishes -Iterator as a series of lists
% containing the bytes within each bucket.  A
% bucket is defined as the length as declared
% by an unsigned 32-bit integer in network
% byte order at the beging beging of a 
% bucket.  The lenght is excluding the
% declaring bucket length.
%
bucket_iterator( List, Iterator ) :-
	iterator( Iterator, bucket_next, bucket_end, List ) 
	.
bucket_next( Current, Next, Bucket ) :-
	network_uint32( Length, Current, StartOfBucket ),
	sublist( StartOfBucket, Length, Bucket, Next )
	.
bucket_end( [] ).

