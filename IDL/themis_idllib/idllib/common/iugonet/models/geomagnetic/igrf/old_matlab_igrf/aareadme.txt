=========================================================================
National Space Science Data Center                         09 July 1998
=========================================================================

NAME                  Magnetic Field Model


SCIENTIFIC CONTACT    Carlos Roithmayr
                      NASA Langley Research Center
                      Spacecraft and Sensors Branch (CBC)
                      757 864 6778
                      c.m.roithmayr@larc.nasa.gov

NSSDC CONTACT         N. Papitashvili, NASA/GSFC/NSSDC, code 612.4,
                      Greenbelt, Maryland 20771, U.S.A.,
                      INTERNET: natasha@mail630.gsfc.nasa.gov

CONTENT:

  Name                     Comments
--------------------------------------------------------------------------
  bfield.m           Compute magnetic field exerted at a point P.

  igrf_example.m     Example MATLAB script to calculate values of the
                     geomagnetic field at 12 points spaced equally on a
                     circle inclined 51.6 deg to Earth's equator, and 400
                     km above Earth's surface.  The field is calculated
                     with IGRF coefficients up to degree and order 10, for
                     the year 1995.00.  Comments include results to use in
                     checking installation of the software.

   IGRF95.m          MATLAB routine to load Schmidt-normalized coefficients
                     retrieved from igrf95.dat
                     (ftp://nssdc.gsfc.nasa.gov/pub/models/geomagnetic/igrf/)


   recursion.m       Recursive calculations of derived Legendre polynomials
                     and other quantities needed for gravitational and
                     magnetic fields.

   schmidt.m         Compute coefficients that relate Schmidt functions to
                     associated Legendre functions.
-------------------------------------------------------------------------

Main program inputs and outputs:

     repe    (km)      Position vector from Earth's center, E*, to a
                       point, P, expressed in a basis fixed in the
                       Earth (ECF): 1 and 2 lie in equatorial plane
                       with 1 in the plane containing the prime meridian,
                       3 in the direction of the north pole.

     nmax              Maximum degree of contributing spherical harmonics

     mmax              Maximum order of contributing spherical harmonics

     G, H     Tesla    Schmidt-normalized Gauss coefficients

     R_mean   km       Mean radius for International Geomagnetic
                       Reference Field (6371.2 km)

     bepe     Tesla    Magnetic field at a point, P, expressed in ECF
                       basis

--------------------------------------------------------------------

Brief description of model:

This package can be used in MATLAB/SIMULINK simulations of spacecraft
motion when a planetary magnetic field model is required.  For example,
it may be necessary to evaluate the torque produced by interaction
of the spacecraft's magnetic moment with the field, or by attitude control
devices such as magnetic dampers and magnetic torquers.  A field model
would also be required in a simulation involving magnetometers or similar
attitude sensors, which operate by measuring the field.

The model is a collection of MATLAB functions that calculate a magnetic
field vector when the field is regarded as a gradient of a potential
function that, in turn, can be described as an infinite series of spherical
harmonics.  Computation of the field vector is performed speedily by means
of recursion formulae, and the calculations do not suffer from the shortcoming
of a singularity when evaluated at points that lie on the polar axis.

Although the MATLAB routine IGRF95.m includes Gauss coefficients for the
1995 International Geomagnetic Reference Field, the user is free to
substitute coefficients from other IGRF model years, or use coefficients
that result from interpolation.  One may even use Gauss coefficients
associated with other celestial bodies or magnets.

Results from these MATLAB routines (reported in the comments of
igrf_example.m) have been compared to those from modified FORTRAN code
(bilcal.for and shellig.for) obtained originally from
ftp://nssdc.gsfc.nasa.gov/pub/models/geomagnetic/igrf/ at Goddard's
National Space Science Data Center, and results are identical to 14 decimal
places.  In contrast to the FORTRAN code, the MATLAB routines do not compute
L-value, magnetic inclination and declination angles, and dipole moment, nor
do they provide profiles of the field as a function of altitude, latitude,
longitude, and year.  The production of such profiles can be accomplished
rather easily with MATLAB instructions that make repeated calls to
routines recursion.m and bfield.m; in fact, igrf_example.m produces a profile
taken along a circle.


References:

     1. Haymes, R. C., Introduction to Space Science, Wiley, New
        York, 1971.

     2. Lundberg, J. B., and Schutz, B. E., "Recursion Formulas of
        Legendre Functions for Use with Nonsingular Geopotential Models",
        Journal of Guidance, Control, and Dynamics, Vol. 11, Jan--Feb 1988,
        pp. 32--38.

     3. Mueller, A. C., "A Fast Recursive Algorithm for Calculating
        the Forces Due to the Geopotential", NASA JSC Internal Note
        No. 75-FM-42, June 9, 1975.

     4. Roithmayr, C., "Contributions of Spherical Harmonics to
        Magnetic and Gravitational Fields", EG2-96-02, NASA Johnson
        Space Center, Jan. 23, 1996.

         Ref. 4 is available as a PDF document for veiwing with Adobe
         Acrobat Reader (freeware)

=================================================================

