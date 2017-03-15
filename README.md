Attentional Blink AND Dual-stream experiment implemented with [Psychopy](https://github.com/psychopy/psychopy)
============================
Licensing: MIT license, like CC-BY for code which means do whatever you want with it, with an attribution to the author.

The basis of this experiment was Alex's [attentional blink repository](https://github.com/alexholcombe/attentional-blink).

This was created for Eva's honours thesis, so that she could run her first experiment, with one block of attentional blink trials and one block of dual-stream trials.

The version initially used did not report in the single-target condition which side the single target was presented on. That was fixed for Eva's second experiment (in which noise was added, and only dual-stream task was used).

For Eva's experiments, people just typed the two letters in, to make it similar to the AB task, meaning that they always respond to the left stream first.

The addition of letterLineupResponse.py means that, for the dual-stream (simultaneous targets) task, we can elicit each response first, in half of trials.

** NOTES **

* Increase presentation rate

* Increase streams

*Compare skew empirically

Originally I thought whichStreamEachCue would always start with zero. Dumb!
line 646

What proportion of trials should be single-trial?

Doesn't fully support yet cuing more than 2 streams and then having single post-cue.

Add different mouse clicks for confidence and low confidence.

Add eyetracking code.
line 919
Angle binning MAKE SURE STREAM POS COUNTERBALANCED IN NSTREAMS CONDITION
 and confidence feedback