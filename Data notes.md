* ID code protocol
	* initials + participant number
		* i.e. CL1
* There's a bug that leads to incorrect accuracy and relative position data, see [this issue](https://github.com/alexholcombe/nStream/issues/1). This isn't the case for pilot data, but affects everything collected from 3/5/17 
	* The first participant was affected by the bug. Their data are useless. The bug was resolved afterwards
* I cancelled the third participant's data collection (4/5/17) because the eyetracker failed to register her eye. 
* IK4 and LT5 have data in two (temporally contiguous) blocks: IK42 and LT52 are the second block. They did the same number of trials as the other participants. The eyetracker crashed in the middle of IK4's first session, around trial 160, so I ran another block of 160 trials. I forgot to change the nTrialPerCondition settings back for LT5. 