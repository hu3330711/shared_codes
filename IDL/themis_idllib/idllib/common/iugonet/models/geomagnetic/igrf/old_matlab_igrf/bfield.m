function bepe = bfield(repe,nmax,mmax,K,A,ctilde,stilde,G,H)
global R_mean

%+=====================================================================+
%
%     Programmers:  Carlos Roithmayr                           Feb 1997
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
%     Compute magnetic field exerted at a point P.
%
%+---------------------------------------------------------------------+
%
%     Argument definitions:
%
%     repe    (km)      Position vector from Earth's center, E*, to a
%                       point, P, expressed in a basis fixed in the
%                       Earth (ECF): 1 and 2 lie in equatorial plane
%                       with 1 in the plane containing the prime
%                       meridian, in the direction of the north pole.
%
%     nmax              Maximum degree of contributing spherical harmonics
%
%     mmax              Maximum order of contributing spherical harmonics
%
%     K		            coefficients that relate Schmidt functions to
%						associated Legendre functions.
%
%     A                 Derived Legendre polynomials
%
%     ctilde            See pp. 4--9 of Ref. [1]
%
%     stilde            See pp. 4--9 of Ref. [1]
%
%     G, H     Tesla    Schmidt-normalized Gauss coefficients
%
%     R_mean   km       Mean radius for International Geomagnetic
%                       Reference Field (6371.2 km)
%
%     bepe     Tesla    Magnetic field at a point, P, expressed in ECF
%                       basis
%
%+---------------------------------------------------------------------+
%
%     References:
%
%     1. Mueller, A. C., "A Fast Recursive Algorithm for Calculating
%        the Forces Due to the Geopotential", NASA JSC Internal Note
%        No. 75-FM-42, June 9, 1975.
%
%     2. Roithmayr, C., "Contributions of Spherical Harmonics to
%        Magnetic and Gravitational Fields", EG2-96-02, NASA Johnson
%        Space Center, Jan. 23, 1996.
%
%+---------------------------------------------------------------------+
%
%     Conversion factors:
%
%       1 Tesla = 1 Weber/(meter-meter) = 1 Newton/(Ampere-meter)
%               = 1e+4 Gauss  =  1e+9 gamma
%
%+=====================================================================+

% The number 1 is added to degree and order since MATLAB can't have an array
% index of 0.

e1=[1 0 0];
e2=[0 1 0];
e3=[0 0 1];

rmag = sqrt(repe*repe');
rhat = repe/rmag;

u = rhat(3);                    % sin of latitude

bepe = [0 0 0];

% Seed for recursion formulae

scalar = R_mean*R_mean/(rmag*rmag);

for n = 1:nmax

% Recursion formula
  scalar = scalar*R_mean/rmag;

  i=n+1;

  for m = 0:n

    j=m+1;

    if m <= mmax
      ttilde(i,j) = G(i,j)*ctilde(j) + H(i,j)*stilde(j);

%     ECF 3 component {Eq. (2), Ref. [2]}
      b3(i,j) =  -ttilde(i,j)*A(i,j+1);

%     rhat component {Eq. (2), Ref. [2]}
      br(i,j) = ttilde(i,j)*(u*A(i,j+1) + (n+m+1)*A(i,j));

%     Contribution of zonal harmonic of degree n to magnetic
%     field.  {Eq. (2), Ref. [2]}

      bepe = bepe + scalar*K(i,j)*(b3(i,j)*e3 + br(i,j)*rhat);
    end

    if ((m > 0) & (m <= mmax))

%     ECF 1 component {Eq. (2), Ref. [2]}
      b1(i,j) = -m*A(i,j)*(G(i,j)*ctilde(j-1) + H(i,j)*stilde(j-1));

%     ECF 2 component {Eq. (2), Ref. [2]}
      b2(i,j) = -m*A(i,j)*(H(i,j)*ctilde(j-1) - G(i,j)*stilde(j-1));

%     Contribution of tesseral harmonic of degree n and order m to
%     magnetic field.  {Eq. (2), Ref. [2]}
      bepe = bepe + scalar*K(i,j)*(b1(i,j)*e1 + b2(i,j)*e2);
    end

  end
end

