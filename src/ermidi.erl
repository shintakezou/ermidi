-module(ermidi).
-export([parse/1]).

parse(S) ->
    case parse_header(S) of
	{ok, R} ->
	    try
		parse_chunk(R)
	    catch
		_:T ->
		    erlang:display(T),
		    erlang:display(erlang:get_stacktrace())
	    end;
	{fail, _} -> 
	    io:fwrite("Not a Standard MIDI File~n")
    end.


%% ---------------

-define(MIDI_HEADER, "MThd").
-define(MIDI_HEADER_LENGTH, 6).
-define(MIDI_TRACK,  "MTrk").

-define(EV_NOTEOFF, 16#8).
-define(EV_NOTEON, 16#9).
-define(EV_POLYKEYPRESSURE, 16#A).
-define(EV_CC, 16#B).
-define(EV_PROGRAMCHANGE, 16#C).
-define(EV_CHANNELPRESSURE, 16#D).
-define(EV_PITCHBEND, 16#E).
-define(EV_SYSEX, 16#F).

-define(EV_META, 16#FF).

-define(DRUM_CHANNEL, 9). %% GM

%% ---------------

midi_format_to_string(0) ->
    "single multi-channel track";
midi_format_to_string(1) ->
    "simultaneous tracks";
midi_format_to_string(2) ->
    "sequentially independent single-track patterns";
midi_format_to_string(_) ->
    "unknown / not standard".

%% -------

midi_format_check(0, 1) ->
    "";
midi_format_check(0, L) when L > 1 ->
    " (wrong: expected 1)";
midi_format_check(_, _) ->
    "".

%% -----------

midi_interpret_div(<<0:1, TicksPerQNote:15>>) ->
    integer_to_list(TicksPerQNote) ++ " delta ticks per quarter note";
midi_interpret_div(<<1:1, FramesPerSec:7/signed, Resolution:8>>) ->
    integer_to_list(abs(FramesPerSec)) ++ " frames/s, " ++
	integer_to_list(Resolution) ++ " units/frame";
midi_interpret_div(_) ->
    "???".


%% ----

print_header(Format, TracksNo, Division) ->
    io:fwrite("MIDI Header~n" ++
		  "  Format: ~B (~s)~n" ++
		  "  Tracks: ~B~s~n" ++
		  "  Division: ~s~n~n", [
					 Format, midi_format_to_string(Format),
					 TracksNo, midi_format_check(Format, TracksNo),
					 midi_interpret_div(<<Division:16>>)
					]).

print_unknown_chunk(Chunk, Len) ->
    Pieces = <<Chunk:32>>,
    io:fwrite("Chunk: ~.16B (~p) (skipping)~n" ++
		  "  Length: ~B~n", [
				     Chunk, Pieces,
				     Len
				    ]).


print_track_start(Len) ->
    io:fwrite("MIDI Track~n  Length: ~B~nBegin~n",
	      [Len]).

print_track_end() ->
    io:fwrite("End.~n~n").


%% ----------

interpret_key(S, 0) when S > 0 ->
    lists:nth(S, [
		  "G", "D", "A", "E", "B", "F#", "C#"
		 ]) ++ " major";
interpret_key(S, 0) when S < 0 ->
    lists:nth(abs(S), [
		       "F", "Bb", "Eb", "Ab", "Db", "Gb", "Cb"
		      ]) ++ " major";
interpret_key(S, 1) when S > 0 ->
    lists:nth(S, [
		  "E", "B", "F#", "C#", "G#", "D#", "A#"
		 ]) ++ " minor";
interpret_key(S, 1) when S < 0 ->
    lists:nth(abs(S), [
		       "D", "G", "C", "F", "Bb", "Eb", "Ab"
		      ]) ++ " minor";
interpret_key(0, 0) -> "C major";
interpret_key(0, 1) -> "A minor".


%% -------------			   
% ["C/B#", "C#/Db", "D", "D#/Eb", 
%  "E/Fb", "F/E#", "F#/Gb", "G",
%  "G#/Ab", "A", "A#/Bb", "B/Cb"]
% It could pick # or b according to the key signature (provided it is correct); 
% but then I need to keep track of the time signature, and to keep a state, ...
% Ruled out this, anyway, using only # (or only b) makes it clearer;
% Besides, 10+1 can be a rhythm channel too, and 9+1 (which defaults to rhythm)
% could be a melody channel... I ignore all these things.
gm_note(Ch, Note) when Ch /= ?DRUM_CHANNEL ->
    lists:nth((Note rem 12)+1, ["C", "C#", "D", "D#", 
				"E", "F", "F#", "G",
				"G#", "A", "A#", "B"]) ++
	integer_to_list((Note div 12) - 1);
gm_note(_, _) ->
    "drum".


%% -------

gm_instrument(Ch, Program) when Ch /= ?DRUM_CHANNEL ->
    "an instrument";
gm_instrument(_, _) ->
    "a drumset".

%% -------		     

control_change_map(Cc) when Cc < 32 ->
    case Cc of
	0 -> "bank select";
	1 -> "modulation wheel";
	2 -> "breath control";
	4 -> "foot control";
	5 -> "portamento";
	6 -> "data entry";
	7 -> "channel volume";
	8 -> "balance";
	10 -> "pan";
	11 -> "expression";
	12 -> "effect control 1";
	13 -> "effect control 2";
	16 -> "general purpose controller 1";
	17 -> "general purpose controller 2";
	18 -> "general purpose controller 3";
	19 -> "general purpose controller 4";
	_ -> "undefined"	     
    end;

control_change_map(Cc) when Cc < 64 ->
    "LSB " ++ control_change_map(Cc-32);

control_change_map(Cc) ->
    case Cc of
	64 -> "damper pedal on/off";
	65 -> "portamento on/off";
	66 -> "sostenuto on/off";
	67 -> "soft pedal on/off";
	68 -> "legato footswitch on/off";
	69 -> "hold 2 on/off";
	70 -> "sound variation";
	71 -> "timbre/harmonic";
	72 -> "release time";
	73 -> "attack time";
	74 -> "brightness";
	75 -> "decay time";
	76 -> "vibrato rate";
	77 -> "vibrato depth";
	78 -> "vibrato delay";
	80 -> "general purpose controller 5";
	81 -> "general purpose controller 6";
	82 -> "general purpose controller 7";
	83 -> "general purpose controller 8";
	84 -> "portamento control";
	88 -> "high resolution velocity prefix";
	91 -> "effects 1 depth (reverb send level)";
	92 -> "effects 2 depth (tremolo depth)";
	93 -> "effects 3 depth (chorus send level)";
	94 -> "effects 4 depth (celeste depth)";
	95 -> "effects 5 depth (phaser depth)";
	96 -> "data increment";
	97 -> "data decrement";
	98 -> "NRPN LSB";
	99 -> "NRPN MSB";
	100 -> "RPN LSB";  %%
	101 -> "RPN MSB";  %%
	120 -> "all sound off";
	121 -> "reset all controller";
	122 -> "local control on/off";
	123 -> "all notes off";
	124 -> "omni mode off";
	125 -> "omni mode on";
	126 -> "mono mode on";
	127 -> "poly mode on";
	_ -> "undefined"
    end.


frame_rate(R) ->
    lists:nth(R+1, ["24", "25", "30 drop frame", "30 non-drop frame"]).


%% ---------------

chunk_skip(Bin, Offset) ->
    <<_:Offset/binary, Next/binary>> = Bin,
    Next.

%% ---------------

parse_header(<<?MIDI_HEADER, 
	      ?MIDI_HEADER_LENGTH:32,
	      Format:16,
	      TracksNo:16,
	      Division:16,
	      Rest/binary>>) ->
    print_header(Format, TracksNo, Division),
    {ok, Rest};
parse_header(_) -> {fail, []}.

%% ----------------

var_len(<<0:1, Delta:7, Rest/binary>>) ->
    {ok, Delta, Rest};
var_len(<<1:1, Delta1:7, 0:1, Delta2:7, Rest/binary>>) ->
    {ok, (Delta1 bsl 7) bor Delta2, Rest};
var_len(<<1:1, Delta1:7, 1:1, Delta2:7, 0:1, Delta3:7, Rest/binary>>) ->
    {ok, (Delta1 bsl 14) bor (Delta2 bsl 7) bor Delta3, Rest};
var_len(<<1:1, Delta1:7, 1:1, Delta2:7, 1:1, Delta3:7, 0:1, Delta4:7, Rest/binary>>) ->
    {ok, (Delta1 bsl 21) bor (Delta2 bsl 14) bor (Delta3 bsl 7) bor Delta4, Rest};
var_len(_) ->
    {fail, [], []}.


%% ----------------

read_event(<<Meta:8, Type:8, Data/binary>>, _) when Meta == ?EV_META ->
    {ok, Len, Rest} = var_len(Data),
    case Type of
	0 -> % sequence number 
	    <<SeqNo:16, _/binary>> = Rest,
	    io:fwrite("sequence number ~B~n", [SeqNo]);
	1 -> % text event
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("text: ~s~n", [binary_to_list(Text)]);
	2 -> % copyright notice
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("copyright: ~s~n", [binary_to_list(Text)]);
	3 -> % seq/track name
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("sequence/track name: ~s~n", [binary_to_list(Text)]);
	4 -> % instrument name
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("instrument: ~s~n", [binary_to_list(Text)]);
	5 -> % lyrics
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("lyrics: ~s~n", [binary_to_list(Text)]);
	6 -> % marker
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("marker: ~s~n", [binary_to_list(Text)]);
	7 -> % cue point
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("cue point: ~s~n", [binary_to_list(Text)]);
	8 -> % program name
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("program name: ~s~n", [binary_to_list(Text)]);
	9 -> % device name
	    <<Text:Len/binary, _/binary>> = Rest,
	    io:fwrite("device name: ~s~n", [binary_to_list(Text)]);
	16#20 -> % midi channel prefix
	    <<Prefix:8, _/binary>> = Rest,
	    io:fwrite("channel prefix: ~B~n", [Prefix+1]);
	16#21 -> % midi port
	    <<Port:8, _/binary>> = Rest,
	    io:fwrite("MIDI port: ~B~n", [Port]);
	16#2F -> % end of track
	    io:fwrite("end of track~n");
	16#51 -> % set tempo
	    <<Tempo:24, _/binary>> = Rest,
	    io:fwrite("set tempo: ~B us/MIDI quarter note~n", [Tempo]);
	16#54 -> % SMPTE Offset
	    <<_:1, Rr:2, Hr:5, Mn, Se, Fr, Ff, _/binary>> = Rest,
	    io:fwrite("SMPTE offset ~B:~B:~B (~s fps), ~B frame, ~B fractional frames~n",
		      [Hr, Mn, Se, frame_rate(Rr), Fr, Ff]);
	16#58 -> % time signature
	    <<Nn, Dd, Cc, Bb, _/binary>> = Rest,
	    io:fwrite("time ~B/~B, ~B clocks/click, ~B notated 32nd/MIDI quarter note~n",
		      [Nn, 1 bsl Dd, Cc, Bb]);
	16#59 -> % key signature
	    <<Sf/signed, Mi, _/binary>> = Rest,
	    io:fwrite("key ~s~n", [interpret_key(Sf, Mi)]);
	16#7F -> % sequencer specific
	    io:fwrite("sequencer specific meta-event~n");
	% non standard, "supported by MidiKit and QMidi"
	16#4B -> % M-Live tag
	    ReducedLen = Len - 1,
	    <<Tag:8, Text:ReducedLen/binary, _/binary>> = Rest,
	    io:fwrite("M-Live ~s: ~s~n", [case Tag of
					      1 -> "genre";
					      2 -> "artist";
					      3 -> "composer";
					      4 -> "duration";
					      5 -> "bpm";
					      _ -> "?"
					  end, Text]);
	_ -> % other
	    io:fwrite("meta-event ~B, len ~B~n", [Type, Len])
    end,
    if Len > 0 ->
	    Skipped = chunk_skip(Rest, Len),
	    {ok, Skipped, {no_status, <<>>}};
       true ->
	    {ok, Rest, {no_status, <<>>}}
    end;

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_NOTEON ->
    <<Note:8, Vel:8, Rest/binary>> = Data,
    io:fwrite("ch. ~B, NoteOn ~B (~s), vel. ~B~n", [Ch+1, Note, gm_note(Ch, Note), Vel]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_NOTEOFF ->
    <<Note:8, Vel:8, Rest/binary>> = Data,
    io:fwrite("ch. ~B, NoteOff ~B (~s), vel. ~B~n", [Ch+1, Note, gm_note(Ch, Note), Vel]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_CC ->
    <<Cc, Val, Rest/binary>> = Data,
    io:fwrite("ch. ~B CC ~s ~B~n", [Ch+1, control_change_map(Cc), Val]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_PROGRAMCHANGE ->
    <<Val, Rest/binary>> = Data,
    io:fwrite("ch. ~B program change ~B (~s)~n", [Ch+1, Val+1, gm_instrument(Ch, Val)]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_PITCHBEND ->
    <<L, M, Rest/binary>> = Data,
    io:fwrite("ch. ~B pitch bend ~B~n", [Ch+1, (((M band 16#7F) bsl 7) bor (L band 16#7F)) - 16#2000]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_POLYKEYPRESSURE ->
    <<K, V, Rest/binary>> = Data,
    io:fwrite("ch. ~B aftertouch on note ~B, value ~B~n", [Ch+1, K, V]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Ch:4, Data/binary>>, _) when Code == ?EV_CHANNELPRESSURE ->
    <<V, Rest/binary>> = Data,
    io:fwrite("ch. ~B channel pressure ~B~n", [Ch+1, V]),
    {ok, Rest, {status, <<Code:4, Ch:4>>}};

read_event(<<Code:4, Msg:4, Data/binary>>, _) when Code == ?EV_SYSEX ->
    case Msg of
	0 ->
	    {ok, Len, Rest} = var_len(Data),
	    <<SysMsg:Len/binary, AfterSysEx/binary>> = Rest,
	    io:fwrite("sysex (~B): ~p~n", [Len, SysMsg]),
	    {ok, AfterSysEx, {no_status, <<>>}};
	1 -> 
	    <<_:1, N:3, D:4, Rest/binary>> = Data,
	    io:fwrite("MIDI time code quarter frame: type ~B, values ~B~n",
		      [N, D]),
	    {ok, Rest, {no_status, <<>>}};
	2 ->
	    <<_:1, L:7, _:1, M:7, Rest/binary>> = Data,
	    io:fwrite("song position pointer LSB ~B MSB ~B (~B)~n",
		      [L, M, (M bsl 7) bor L]),
	    {ok, Rest, {no_status, <<>>}};
	3 ->
	    <<_:1, S:7, Rest/binary>> = Data,
	    io:fwrite("song select ~B~n", [S]),
	    {ok, Rest, {no_status, <<>>}};
	6 ->
	    io:fwrite("tune request~n"),
	    {ok, Data, {no_status, <<>>}};
	7 ->
	    io:fwrite("eox~n"),
	    {ok, Data, {no_status, <<>>}};
	% system real time message...
	8 ->
	    io:fwrite("timing clock~n"),
	    {ok, Data, {no_status, <<>>}};
	10 ->
	    io:fwrite("start~n"),
	    {ok, Data, {no_status, <<>>}};
	11 ->
	    io:fwrite("continue~n"),
	    {ok, Data, {no_status, <<>>}};
	12 ->
	    io:fwrite("stop~n"),
	    {ok, Data, {no_status, <<>>}};
	14 ->
	    io:fwrite("active sensing~n"),
	    {ok, Data, {no_status, <<>>}};
	%% this can't be used into a SMF, because is "mapped" to meta-events
	15 -> %% FF -> meta-event
	    io:fwrite("system reset~n"),
	    {ok, Data, {no_status, <<>>}};
	_ ->
	    io:fwrite("undefined~n"),
	    {ok, Data, {no_status, <<>>}}
    end;

read_event(<<V:8, Rest/binary>>, {status, <<S:8>>}) when V < 128 ->
    read_event(<<S, V, Rest/binary>>, {status, <<S>>});

read_event(_, _) ->
    io:fwrite("unknown event~n"),
    {fail, <<>>, {no_status, <<>>}}.


%% ----------------

parse_events(TrackEvents, RunningStatus) when byte_size(TrackEvents) > 0 ->
    {R, D, Ev} = var_len(TrackEvents),
    case R of
	ok -> 
	    io:fwrite("  ~12B ", [D]),
	    {O, NextTick, LastStatus} = read_event(Ev, RunningStatus),
	    case O of
		ok ->
		    parse_events(NextTick, LastStatus);
		_ -> {fail, []}
	    end;
	_ ->
	    {fail, []}
    end;

parse_events(<<>>, _) ->
    {ok, []};

parse_events(_, _) ->
    io:fwrite("what?~n"),
    {ok, []}.





%% ----------
parse_chunk(<<?MIDI_TRACK, Len:32, Rest/binary>>) ->
    <<Events:Len/binary, NextChunk/binary>> = Rest,
    print_track_start(Len),
    case parse_events(Events, {no_status, <<>>}) of
	fail -> io:fwrite("  crippled events~n");
	_   -> ok
    end,
    print_track_end(),
    parse_chunk(NextChunk);

parse_chunk(<<Chunk:32, ChunkLen:32, Rest/binary>>) ->
    print_unknown_chunk(Chunk, ChunkLen),
    NextChunk = chunk_skip(Rest, ChunkLen),
    parse_chunk(NextChunk);

parse_chunk(S) when byte_size(S) > 0 ->
    io:fwrite("corrupted MIDI file?~n"),
    {fail, []};

parse_chunk(<<>>) -> {ok, []}.



