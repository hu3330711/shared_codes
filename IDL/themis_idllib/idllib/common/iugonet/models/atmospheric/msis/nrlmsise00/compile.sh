ifort -c nrlmsise00_sub.for
ifort -c nrlmsise00_driver_iugonet.f90
ifort nrlmsise00_sub.o nrlmsise00_driver_iugonet.o -o nrlmsise00_driver_iugonet.out
cp nrlmsise00_driver_iugonet.out CC.out
