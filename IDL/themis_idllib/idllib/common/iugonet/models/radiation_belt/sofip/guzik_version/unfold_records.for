C
      character inline*6600, otline*132
      character infile*80, otfile*80
      integer*4 linf,lotf
      integer*4 lenin,irec,orec
      integer*4 reclen,linlen,getint
      logical*1 trans_logical,okin,okot,more_lines,okrc,okln
C
      type *,' '
      type *,'*** Read a file which has multiple lines concatenated'
      type *,'*** into long records (up to 6600 characters), split'
      type *,'*** these records into lines (up to 132 characters)'
      type *,'*** and output to a new file.'
      type *,' ' 
      lfnin = 10
      lfnot = 12
      okin = .false.
      okot = .false.
      if (trans_logical('INPUT_FILE',infile,linf)) then
        okin = .true.
        open(lfnin,file=infile(1:linf),type='old',err=1100)
        type *,'Opened ',infile(1:linf),' as the input.'
      else
        type *,'ERROR: Logical INPUT_FILE must be defined'
        okin = .false.
      endif
      if (trans_logical('OUTPUT_FILE',otfile,lotf)) then
        okot = .true.
        open(lfnot,file=otfile(1:lotf),type='new',err=1200,
     *       carriagecontrol='list')
        type *,'Opened ',otfile(1:lotf),' as the output.'
      else
        type *,'ERROR: Logical OUTPUT_FILE must be defined'
        okot = .false.
      endif
      if (.not.okin .or. .not.okot) goto 1000
C
      okrc = .false.
      okln = .false.
      reclen = getint('Enter input file record length')
      linlen = getint('Enter output file line length')
      if (reclen .gt. 6600) then
        okrc = .false.
        type *,'ERROR: Input record length must be =< 6600'
      else
        okrc = .true.
      endif
      if (linlen .gt. 132) then
        okln = .false.
        type *,'ERROR: Output line length must be =< 132'
      else
        okln = .true.
      endif
      if (.not.okrc .or. .not.okln) goto 9000
C
      type *,' '
      irec = 0
      orec = 0
      more_lines = .true.
      do while (more_lines)
        call blankit(inline,reclen)
        read(lfnin,'(q,a<lenin>)',end=100,err=2100) 
     *                                          lenin,inline(1:lenin)
        irec = irec + 1
        nlines = lenin/linlen
        nremain= mod(lenin,linlen)
        if (nremain .ne. 0) nlines = nlines + 1
        type *,' '
        do il=1,nlines
          ibeg = (il - 1)*linlen + 1
          iend = ibeg + linlen - 1
          if (iend .gt. lenin) iend = lenin
          call blankit(otline,linlen)
          ilen = iend - ibeg + 1
          otline(1:ilen) = inline(ibeg:iend)
          write(lfnot,'(a<ilen>)',err=2200) otline(1:ilen)
          orec = orec + 1
          write(*,10) irec,orec
   10     format('+','Splitting input ',I5,' to output ',I6)
        enddo
      enddo
C
  100 close(unit=lfnin)
      close(unit=lfnot)
      goto 9000
C
 1000 type *,'ERROR: One or more control logicals not defined'
      goto 9000
C
 1100 type *,'ERROR: With opening ',infile(1:linf),' as input'
      goto 9000
C
 1200 type *,'ERROR: With opening ',otfile(1:lotf),' as output'
      goto 9000
C
 2100 type *,'ERROR: With reading input file ',infile(1:linf)
      goto 9000
C
 2200 type *,'ERROR: With writing output file ',otfile(1:lotf)
      goto 9000
C
 9000 continue
      end
C*******************************************************************************

      LOGICAL FUNCTION TRANS_LOGICAL(LOGSTR,TRNSTR,TRNLEN)
C
C      Check to see that the logical LOGSTR has been assigned
C
C      11/26/90 * v1.0 - Initial version by TGG
C
      PARAMETER SS$_NORMAL = '00000001'X
      PARAMETER LNM$_STRING = '00000002'X	!  Translation string
C
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 CODE
            INTEGER*4 BUFADR
            INTEGER*4 RETLENADR
          END MAP
          MAP
            INTEGER*4 END_LIST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ LNMLST(2)
      CHARACTER*(*) LOGSTR,TRNSTR
      CHARACTER*255 LOGTRN
      INTEGER*4     LOGLEN,TRNLEN
      INTEGER*4 SYS$TRNLNM
C
      LTRNSTR = LEN(TRNSTR)
      CALL BLANKIT(TRNSTR,LTRNSTR)
      TRNLEN = 0
      LNMLST(1).BUFLEN    = 255
      LNMLST(1).CODE      = LNM$_STRING
      LNMLST(1).BUFADR    = %LOC(LOGTRN)
      LNMLST(1).RETLENADR = %LOC(LOGLEN)
      LNMLST(2).END_LIST = 0
      IOSTAT = SYS$TRNLNM(,'LNM$PROCESS_TABLE',LOGSTR,,LNMLST)
      IF (IOSTAT .EQ. SS$_NORMAL) THEN
        TRANS_LOGICAL = .TRUE.
        TRNLEN = LOGLEN
        IF (TRNLEN .GT. LTRNSTR) TRNLEN = LTRNSTR
        TRNSTR(1:TRNLEN) = LOGTRN(1:TRNLEN)
      ELSE
        TRANS_LOGICAL = .FALSE.
      ENDIF
C
      RETURN
      END

C*******************************************************************************

      INTEGER FUNCTION GETINT(PROMPT)
C
C      Writes the PROMPT string to the screen and waits for the user to
C      enter an integer value.  This integer is returned as the function
C      value
C
C      12/19/87 * v1.0 - Initial version by TGG
C
      CHARACTER*(*) PROMPT
      CHARACTER     ERSLIN*3,ESC*1
      DATA ESC/'1B'X/,ERSLIN/' [K'/
C
      ERSLIN(1:1) = ESC
      WRITE(*,'(''$'',A,'': '',A)') PROMPT,ERSLIN
      READ(*,*) INTVAL
      GETINT = INTVAL
      RETURN
      END

C*******************************************************************************

      SUBROUTINE BLANKIT(STR,LSTR)
C
C      Set the given string STR of length LSTR to blanks
C
      CHARACTER*(*) STR
      JSTR = LEN(STR)
      IC = 1
      DO WHILE ((IC.LE.LSTR).AND.(IC.LE.JSTR))
        STR(IC:IC) = ' '
        IC = IC + 1
      ENDDO
      RETURN
      END

