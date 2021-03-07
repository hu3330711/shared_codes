ifort -c cira.for
ifort -c igrf.for
ifort -c iridreg.for
ifort -c iriflip.for
ifort -c irifun.for
ifort -c irisub.for
ifort -c iritec.for
ifort -c iritest.for
ifort cira.o	igrf.o	iridreg.o  iriflip.o  irifun.o	irisub.o  iritec.o  iritest.o -o iritest
