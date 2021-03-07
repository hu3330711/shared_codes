%+=====================================================================+
%
%     Programmers:  Carlos Roithmayr          			Feb 1997
%
%		    NASA Langley Research Center
%		    Spacecraft and Sensors Branch (CBC)
%		    757 864 6778
%		    c.m.roithmayr@larc.nasa.gov
%
%+---------------------------------------------------------------------+
%
%     Purpose:
%
%     Calculate values of the geomagnetic field at 12 points spaced
%     equally on a circle inclined 51.6 deg to Earth's equator, and
%     400 km above Earth's surface.  The field is calculated with
%     IGRF coefficients up to degree and order 10, for the year 1995.00.
%
%     The results reported below list position vector "repe" (km)
%     from Earth's center E* to a point P, expressed in a basis fixed
%     in the Earth: unit vectors e1 and e2 lie in the equatorial plane
%     with e1 in the plane containing the prime meridian, and e3 in the
%     direction of the north pole.
%     The magnetic field vector, "bepe" (Tesla), is also projected into
%     the e1-e2-e3 basis.
%+---------------------------------------------------------------------+
% repe_array =
%
%   1.0e+03 *
%
%   6.77813900000000                  0                  0
%   5.87004056438205   2.10511299713392   2.65599159357255
%   3.38906950000000   3.64616266670955   4.60031238454349
%                  0   4.21022599426785   5.31198318714511
%  -3.38906950000000   3.64616266670955   4.60031238454349
%  -5.87004056438205   2.10511299713392   2.65599159357256
%  -6.77813900000000                  0                  0
%  -5.87004056438205  -2.10511299713392  -2.65599159357255
%  -3.38906950000000  -3.64616266670955  -4.60031238454349
%                  0  -4.21022599426785  -5.31198318714511
%   3.38906950000000  -3.64616266670955  -4.60031238454350
%   5.87004056438205  -2.10511299713392  -2.65599159357256
%   6.77813900000000                  0                  0
%+---------------------------------------------------------------------+
%  bepe =
%
%   1.0e-04 *
%
%   0.10577006862061  -0.03280311340912   0.22622896453283
%  -0.23689849118264  -0.08381212188929   0.19012647353034
%  -0.28097149868399  -0.27932841208010  -0.09478661247316
%  -0.00993087024324  -0.40696658723490  -0.25516638287475
%   0.30515405726843  -0.28406934448847  -0.08473891260811
%   0.22227445395075  -0.09720240368498   0.17861064660414
%  -0.02802489095986  -0.05094783955262   0.27966635300835
%  -0.26857111543808  -0.16322087400994   0.13741460188272
%  -0.18857636914770  -0.32093878639814  -0.08330783529524
%   0.07456603136381  -0.29449414094440  -0.09390302297815
%   0.13261087563580  -0.17589319924929  -0.00159363090568
%   0.17318903393474  -0.12129151029501   0.05449768902703
%   0.10577006862061  -0.03280311340912   0.22622896453283
%
%+=====================================================================+

global R_mean

R_mean = 6371.2;              % Mean radius for International Geomagnetic
                              % Reference Field (6371.2 km)

[G,H] = IGRF95;               % IGRF coefficients for 1995

nmax = 10;                    % max degree of geopotential
mmax = 10;                    % max order  of geopotential

Kschmidt = schmidt(nmax,mmax);

R_E = 6378.139;               % radius of Earth, km
R_km = R_E + 400.000;         % radius of circular orbit, km

i_rad=51.6*pi/180;            % inclination of orbit plane
  Si=sin(i_rad);
  Ci=cos(i_rad);
arg_lat=0:pi/6:2*pi;          % values of argument of latitude

for k = 1:13

St=sin(arg_lat(k));
Ct=cos(arg_lat(k));

% direction cosine matrix from E to LVLH,
% assuming longitude of ascending node = 0

D(1,1)=Ct;     D(1,2)=-St;     D(1,3)=0;
D(2,1)=Ci*St;  D(2,2)=Ci*Ct;   D(2,3)=-Si;
D(3,1)=Si*St;  D(3,2)=Si*Ct;   D(3,3)=Ci;

% position vector from E* to P, E-basis

repe = R_km*D(:,1)';
repe_array(k,:) = repe;

[A,ctilde,stilde] = recursion(repe,nmax,mmax);
bepe(k,:) = bfield(repe,nmax,mmax,Kschmidt,A,ctilde,stilde,G,H);

end

format long
repe_array
bepe

