"SynthDef 'test'...".post;
SynthDef(\test,  {
  	|out, sustain=1, freq=440, note=60|
  var tone = SinOsc.ar(freq);
  OffsetOut(out, [tone, tone]);
}).add;
"✓".postln;
