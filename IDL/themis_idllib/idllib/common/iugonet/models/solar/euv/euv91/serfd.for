C  SERFD.FOR -------------------------------------------- Feb 92
C
C *******************************************************************
C
C       interactive driver for SERF2 EUV model program EUV91
C       calls subroutines rddata, getind, and flux
C
C *******************************************************************
C
        integer*4       yyddd,i,j,iselect,io,istatus
        real*4          c(12,52),indices(10,3750),S(4),w1(39),w2(39)
        real*4          date,Eflux(39),Pflux(39),wave(39)
        real*4          aunit(5),unit,outa(39)
        character*10    fname
        character*11    tab1(5),ttab1
        character*12    tab2(5),ttab2
        common  /coeff/ c,indices,date,wave,w1,w2,S,Eflux,Pflux
        data            fname(1:1),fname(7:10) /'f','.dat'/
        data            aunit /1.,1.e-4,1.,1.e+3,1.6022E-12/
        data       tab1 /'Photon flux','Photon flux','Energy flux',
     1                     'Energy flux','Energy flux'/
        data       tab2 /'  cm-2 s-1  ','   m-2 s-1  ',
     1             'erg cm-2 s-1','  J m-2 s-1 ',' eV cm-2 s-1'/

        write (*,*) ' Reading the coefficient and indices tables'
                call rddata
        idbeg=int(indices(1,1))
        idend=int(indices(6,3750))
1234    write (6,7789) idbeg,idend
7789    format('  Enter YYDDD date between',I6,' -',I6,'  > ')
          read  (*,'(I5)') yyddd
        if((yyddd.lt.idbeg).or.(yyddd.gt.idend)) then
                write(6,6677)
6677            format(1x,'Unvalid entry, please try again'/)
                goto 1234
                endif
        date = real(yyddd)

        write (*,*) ' EUV/Dec/91: Calculating the flux'
          date = real(yyddd)
          call getind
          call flux
1235    write (*,*) ' Output options:  enter selection number'
        write (*,*) '  1 = photon flux [cm-2 s-1]       2 = [m-2 s-1]'
        write (*,*) '  3 = energy flux [erg cm-2 s-1]   4 = [J m-2 s-1]'
        write (*,*) '  5 = energy flux [eV cm-2 s-1]'
        write (*,*) '>'
         read (*,*) iselect
                unit=1./aunit(iselect)
                ttab1=tab1(iselect)
                ttab2=tab2(iselect)
        do 2331 i=1,39
          if(iselect.lt.3) then
            outa(i) = Pflux(i) * unit
          else
            outa(i) = Eflux(i) * unit
          endif
2331     continue
        io=6
        write (io,1000) ttab1,ttab1,ttab2,ttab2
1000    format(4x,'Wavelength',4x,a11,7x,'Wavelength',4x,a11/
     1         4x,' Angstrom ',3x,a12,7x,' Angstrom ',3x,a12)
        do 1122 j=1,19
1122      write(io,5566) w1(j),w2(j),outa(j),w1(j+20),w2(j+20),
     1         outa(j+20)
5566      format(1x,2(F7.2,1x),2x,E9.3,5x,2(F7.2,1x),2x,E9.3)
          write(io,2001) w1(20),w2(20),outa(20),yyddd
2001      format(1x,2(F7.2,1x),2x,E9.3,3x,'<<< EUV91 >>>',
     1             4x,'Date:',i5)
        if(io.eq.3) then
          close(unit=3)
          io=6
          endif
1236   write(*,*) 'Change - 1   Store - 2    New date - 3    Exit - 4 >'
        read(*,*) istatus
        if(istatus.eq.1) goto 1235
        if(istatus.eq.2) then
          write(fname,1041) yyddd+i
1041      format('f',i5,'.dat')
          open(unit=3,file=fname,status='unknown',form='formatted')
          io=3
          write(6,1001) fname
1001      format(' Storing last output table in file ',a10)
          goto 1236
          endif
        if(istatus.eq.3) goto 1234
        end
