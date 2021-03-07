
MSISE-1990 Model 

Author:	A. E. Hedin, NASA/GSFC, Code 914, Greenbelt, Maryland 20771

Parameter: Neutral densities and temperature from ground to thermosphere

Brief Description: 

The MSISE model describes the neutral temperature and densities in Earth's
atmosphere from ground to thermospheric heights. Below 72.5 km the model is
primarily based on the MAP Handbook (Labitzke et al., 1985) tabulation of zonal
average temperature and pressure by Barnett and Corney, which was also used for
the CIRA-86. Below 20 km these data were supplemented with averages from the
National Meteorological Center (NMC). In addition, pitot tube, falling sphere,
and grenade sounder rocket measurements from 1947 to 1972 were taken into
consideration. Above 72.5 km MSISE-90 is essentially a revised MSIS-86 model
taking into account data derived from space shuttle flights and newer incoherent
scatter results. For someone interested only in the thermosphere (above 120 km),
the author recommends the MSIS-86 model. MSISE is also not the model of
preference for specialized tropospheric work. It is rather for studies that
reach across several atmospheric boundaries.
24/10/01 Corrected version as provided by M. Picone and A. Hedin (Corrected
error in GHP6 when using METER6(.TRUE.).

CORRECTION: 
July 15, 2003 - New version that fixes an error related to units used, that resulted
	in an incorrect magnitude of the scale height variable that was used to compute 
	values of pressure levels. Specifically, using both MKS units and a grid of 
	pressure levels (rather than an altitude grid), the previous version of MSISE-90 
	(Subroutine GHP6) produced incorrect pressure values.  

Availability: 
(1) FORTRAN source code from this directory:
msise90_driver.for  Example driver program; includes the MSISE90 subroutines 
                    (compilation: f77 -o msis msise90.for)
msise90_out.txt     Output from driver program 
msise90_sub.for     MSISE90 Fortran subroutines 
                  
(2) Model parameters can be computed and plotted online at 
http://nssdc.gsfc.nasa.gov/space/model/models/msis.html

(3) Homepage at http://nssdc.gsfc.nasa.gov/space/model/atmos/msise.html

NSSDC ID #MN-61F

References:

K. Labitzke, J. J. Barnett, and B. Edwards (eds.), Handbook MAP 16, SCOSTEP,
University of Illinois, Urbana, 1985.

A. E. Hedin, Extension of the MSIS Thermospheric Model into the Middle and
Lower Atmosphere, J. Geophys. Res. 96, 1159, 1991. 
