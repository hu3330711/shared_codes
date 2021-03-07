      CHARACTER CARD*80,KEY*8,INPFILE*50,OUTFILE*50
      INTEGER*4 IEK,IEC,IERR,LLINE,LINP,LOUT
      INTEGER*4 FINDEND,GETSTR
      LOGICAL*1 MORE
C
      LFNIN=2
      LFNOT=3
      LINP=GETSTR('Enter filename of input ',INPFILE,50)
      OPEN(UNIT=LFNIN,NAME=INPFILE,TYPE='OLD')
      LOUT=GETSTR('Enter filename of output',OUTFILE,50)
      OPEN(UNIT=LFNOT,NAME=OUTFILE,TYPE='NEW',FORM='FORMATTED',
     *            CARRIAGECONTROL='LIST',RECORDTYPE='VARIABLE')
C
      MORE = .TRUE.
      DO WHILE (MORE)
        READ(LFNIN,'(Q,A<IEC>)',END=10) IEC,CARD(1:IEC)
        IF (IEC .GT. 72) IEC=72
        LLINE = FINDEND(CARD(1:IEC))
        WRITE(LFNOT,'(A<LLINE>)') CARD(1:LLINE)
      ENDDO
C
   10 CLOSE(UNIT=LFNIN)
      CLOSE(UNIT=LFNOT)
      END
C*******************************************************************************

      INTEGER FUNCTION GETSTR(PROMPT,STRING,MAXSTR)
C
C      Writes the PROMPT string to the screen and waits for the user to
C      enter the string STRING.  The length of the user entered string
C      is not checked, but FORTRAN will truncate it to the length defined
C      in the calling program.  The length of the input string is returned
C      as the function value.
C
C      11/16/87 * v1.0 - Initial version by TGG
C
      CHARACTER*(*) PROMPT
      CHARACTER*(*) STRING
      CHARACTER     TMPSTR*80,ERSLIN*3,ESC*1
      DATA ESC/'1B'X/,ERSLIN/' [K'/
C
      ERSLIN(1:1) = ESC
      WRITE(*,'(''$'',A,'': '',A)') PROMPT,ERSLIN
      READ(*,'(Q,A)') LEN,TMPSTR(1:LEN)
      IF (LEN .GT. MAXSTR) LEN = MAXSTR
      STRING(1:LEN) = TMPSTR(1:LEN)
      GETSTR = LEN
      RETURN
      END

C*******************************************************************************

      INTEGER FUNCTION FINDEND(STR)    
C
C      Finds the last character of a string.
C
      CHARACTER*(*) STR
      CHARACTER CH*1,NUL*1
C
      NUL = CHAR(0)
      FINDEND = LEN(STR)      
      CH = STR(FINDEND:FINDEND)
C      
      DO WHILE (((CH .EQ. ' ').OR.(CH.EQ.NUL)) .AND. (FINDEND .GT. 0))
        FINDEND = FINDEND - 1
        CH = STR(FINDEND:FINDEND) !Back a space
      ENDDO
      RETURN
      END

