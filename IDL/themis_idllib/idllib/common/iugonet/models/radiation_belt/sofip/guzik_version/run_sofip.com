$!
$! To run a particular radiation model the appropriate common block must be 
$! uncommented in the SOFIP.FOR source and compiled.  Next the BLOCK DATA
$! routine for the radiation model is compiled and linked with the SOFIP
$! object.  Note that only electrons OR protons can be run in a single pass.
$! In addition, electrons need a inner model (AEI7LO, AEI7HI) and an outer
$! model (AE5MIN, AE6MAX).  This example is setup for the AP8MAC proton model.
$!
$! NOTE: The sample input deck (SAMPLE_INPUT.INP) does not necessarily 
$! correspond to the actual parameter used to general the sample orbit
$! (SAMPLE_ORBIT.DAT).  See the SOFIP document for details on how to run
$! the program
$!
$ FOR SOFIP
$ FOR AP8MAC
$ LINK SOFIP,AP8MAC
$ DEFINE FOR005 SAMPLE_INPUT.INP
$ DEFINE FOR009 SAMPLE_ORBIT.DAT
$ DEFINE FOR006 SAMPLE_ORBIT_6.OUT
$ DEFINE FOR007 SAMPLE_ORBIT_7.OUT
$ RUN SOFIP
$ EXIT
