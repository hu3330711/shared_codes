=========================================================================
National Space Science Data Center                         5 May 1997 
=========================================================================

NAME                THE EQUATORIAL-NEUTRAL SHEET SOFTWARE
                

SCIENTIFIC CONTACT      Ronglan XU and Lei LI
                        Center for Space Sci. and Applied Res., 
                        Chinese Academy of Science, 
                        PO Box 8701, Beijing 100080, China        
                        Internet: XURL@SUN20.CSSAR.AV.CN,   XURL@SUN.IHEP.AC.CN
                          (April 15, 1997)



NSSDC CONTACT           N. Papitashvili, NASA/GSFC/NSSDC, code 633/STX, 
                        Greenbelt, Maryland 20771, U.S.A., 
                        DECNET:  NCF::NATASHA
                        INTERNET: natasha@nssdca.gsfc.nasa.gov
                        
CONTENT:

  Name                     Comments
--------------------------------------------------------------------------
AEN.FOR       Finds the AEN (Analytical Equatorial Neutral) 
              sheet in the  magnetopause in different time and position
 
DEN.FOR       Finds the DEN (Displaced Equatorial Neutral) sheet inside 
              the magnetopause in different tine and positions 

SEN.FOR       Finds the SEN (Standard Equatorial Neutral) sheet inside 
              the magnetopause in different time and position
        
AAREADME.DOC              This file
--------------------------------------------------------------------------

BRIEF DESCRIPTION:


The average configuration of the geomagnetotail has been deduced from numerous
spacecrafts measurements. These lobes are separated by the neutral sheet. The
position of the neutral sheet varies with time, and can only be definitely
known at the instant a spacecraft traverses the reversal region of the magnetic
field. Numerous studies (Fairfield 1980 and references there in) have shown
that the annual and diurnal changes of the tilt angle between the earth-sun
line and the subsolar point induce significant movements of the neutral sheet
away from the solar magnetospheric XY plane.

According to the previous neutral sheet models, there remains a varying
discontinuous region between neutral sheet and the equatorial plane of the
tilted geomagnetic dipole field. A Standard Equatorial-Neutral sheet (SEN)
model (Xu, 1992), a Displaced Equatorial-Neutral sheet (DEN) model (Xu, 1991)
and an Analytical Equatorial-Neutral sheet (AEN) model (Xu et al., 1993,
Zhu and Xu, 1994 and Wang and Xu, 1994) inside the magnetopause (Sibeck et al.,
1991) are developed. 

In these three Equatorial-Neutral Sheet models the tilted equatorial plane 
joints smoothly to the neutral sheet, so that they can be used in the whole
magnetosphere regions, including in the near tail region. The SEN and DEN
models contain of 3 different complicate equations. The AEN model is expressed 
by one simple equation and remain approximately the general characteristic of
the DEN Model. Based on the AEN model, a normal curve coordinate system is set
up, in which the AEN sheet becomes one of the coordinate surface. This
coordinate system will change with the Earth's tilt angle,and thus can almost
offset the titled effect on the pattern of the geomagnetic field on the
Magnetotail (Zhu and XU, 1994).  

                        THE SOFTWARE

This packege  contains the FORTRAN 77 source codes (*.FOR) of SEN, DEN and AEN.
They compute the position of SEN sheet along Zsm, at a given point Xsm and 
Ysm inside the magnetopause at different dates and times, 

The input and output of these software are:

The Input are: DATE, TIME, XSM, YSM, ZSM. Where DATE and TIME are expressed in 
the following 2 different format, and XSM, YSM, ZSM is the position in GSM
Coordinate System.
   (1) When Date is Year and DOY (Day Of Year), and Time is Second of Day (SD).
For example: Year=1995, DOY=34, SD=35000, then Date is 95034 and SD is 35000
  (2) When Date is Year/Month/Day, and Time is Hour/Minute/Second. For example:
February 3, 1995, 2 Hour 5 minute and 10 second, then Date is 950203, and Time
is 020520.

The output are ZSEN (ZDEN or ZAEN) and IE. Where ZSEN (ZDEN or ZAEN) is the
position of the neutral sheet along Zsm axis, at XSM and YSM.  IE is the
parameter to show whether the observed point in in or outside the magnetopause

The software contain the following subroutines
 (1) SDOY(DATE,TIME,YR,DOY,HR,TMI,SC)
   Find Year, Day of year, Month, Day of month, Hour, Minute, Second from 
   Date(year/month/day) and Hour/Minute/Second 
 (2) STIME(YD,SD,YR,DOY,TMO,DAY,HR,TMI,SC)
   Find Year,Day of Year, Month), Day of month, Hour, Minute and Second from 
   Year and Day of Year and Second of Day
 (3) STIL(DOY,HR,TMI,TIL)
   Find the tilt angle of the geomagnetic axis for different Date and Time
 (4) SMPF(XSM,RMP,IM)
    Find the radius of the cross section of the magnetopause
 (5) SSEN(SDEN or SAEN)(TILA,XSM,YSM,ZSEN,RMP,IE)
   Find the position of SEN sheet along ZSM axis for different Rate and Time
For SDEN we have two other subroutines:
 (6) SD1(TIL,H,H1,XSM,D)
  Find the displaced parameter D of the DEN model, so that it provides 
   approximately equal cross-sectional areas above and below the neutral sheet
 (7) SFA4(AA,BB,CC,DD,X)
   Find the root of equation: X**4+AA*X**3+BB*X**2+CC*X+DD=0 in SD1

The detail information and instructions of these software can be find in the
commends of these software. If you have questions and commends please fill free
to contact with us

                           REFERENCES

Fairfield, D. H., A statistical determination of the shape and position of
the geomagnetic neutral sheet, J. Geophys. Res., 85, 775, 1980.

Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
magnetopause shape, location, and motion, J. Geophys. Res., 96, 5489, 1991

Wang, Z.-D. and R.-L. Xu, Neutral Sheet Observed on ISEE Satellite,
Geophysical Research Letter, 21, (19)2087, 1994

Xu, R.-L., A Displaced Equatorial Neutral Sheet Surface Observed on 
ISEE-2 Satellite, J. Atmospheric and Terrestrial Phys., 58, 1085, 1991

Xu, R.-L., Dynamics of the Neutral Sheet in the Magnetotail during 
Substorm, Advances in solar-terrestrial science of China, ed. by W.-R. Hu et 
al., China Science Press, 1992

Xu, R.-L., Z.-D. Wang, M. Zhu and Q.-G. Zong, Particle Precipitation from
the Magnetotail during Substorm, Adv. Space Research, 13, (4)269, 1992

Zhu, M. and R.-L. Xu, A continuous neutral sheet model and a normal 
curved coordinate system in the magnetotail, Chinese J. Space Science, 14, 
(4)269, 1994 (in Chinese)

Li and Xu, GRL,27,855 (March,2000)
-------------------------------------------------------------------------------------