The following routines were modified or enhanced in IDL 5.4.
They may or may not affect cdfx.pro, but the routine certainly uses them.


- Analysis routine: Roberts
- Spawn. Called by DeviceClose, IDLMakeCDF, Pickdir and Getdirs
- Xdisplayfile. Called by Pickdir and cdfx
- Xfont
- Xinteranimate
- Xloadct
- Xvaredit

RCJ 09/00


Status log:
CDFx is currently known to work on IDL versions 5.3-6.1.  
We never operationally ran CDFx w/ IDL 6.2 on 32-bit platforms - too many issues.


CDFx is known to work w/ IDL6.3 64 bit on linux, MAC and PC.
Some rumors about CDF and IDL6.3 64 bit NOT working correctly on a SUN - 
I believe CDF, IDL6.3 in 32 bit mode, works fine.


