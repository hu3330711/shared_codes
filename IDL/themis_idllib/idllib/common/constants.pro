pro constants
common share_constants,qe,qp,me,mp,c,pi,k_B,R_E,mu0,eps0,OMEGA_E,G,M_earth,GM,e_me
  qe      =double(-1.602176462*1e-19)
  qp      =double(1.602176462*1e-19)
  me      =double(9.10938188*1e-31)
  mp      =double(1.67262158*1e-27)
  c       =double(2.99792458*1e8) 
  pi      =double(3.14159265358979323846)
  k_B     =double(1.3806503*1e-23)
  R_E     =double(6378.13*1e3)
  mu0     =double(1.2566370614*1e-6) 
  eps0    =double(8.854187817*1e-12)
  OMEGA_E =double(7.292115E-05)
  G       =double(6.673E-11)
  M_earth =double(5.974E+24)
  GM      =double(3.9860044000E+14)
  e_me    =double(1.7588201736E+11)
print,"constants set"
end
