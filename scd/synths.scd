// SynthDefs for use with Tidal/SuperDirt

// https://github.com/v7b1/mi-UGens
"SynthDef 'rings' ".post;
// MutableInstruments Rings under internal excitation
(SynthDef(\rings,  {
  	|out, sustain=1, freq=440, struct=0.25, bright=0.5, damp=0.7 pos=0.25, model=0|
  var tone = MiRings.ar(
    pit: freq.cpsmidi, 
    struct: struct,
    bright: bright,
    damp: damp,
    pos: pos,
    model: model
  );
  var env = Env.new(levels: [0, 1, 0], times: [0.025, 1], curve: [-5, -5]);
  // Line indexes the envelope
  var line = Line.ar(0, 1, sustain, doneAction: Done.freeSelf);
  var gain = IEnvGen.ar(env, line);
  var outAudio = tone * gain;
  OffsetOut.ar(out, outAudio);
}).add);
"✓".postln;

"SynthDef 'plaits' ".post;
(
SynthDef(\plaits, {|out, sustain=1, pan, accelerate, freq, harm=0.5, timbre=0.5, morph=0.5, engine=0|
    var env = EnvGen.ar(Env.linen(0.01, 0.98, 0.01, 1,-3), timeScale:sustain, doneAction:2);
	var sound = MiPlaits.ar(pitch: freq.cpsmidi, engine: engine, harm: harm, timbre: timbre, morph: morph);
	Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;
);
"✓".postln;
