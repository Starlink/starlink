      PROGRAM UNPACK
C----------------------------------------------------------------------
C Convert packed (binary) representation of GRFONT into unpacked
C (ASCII) representation suitable for editing. The input file is
C read from PLT$FONT as in PGPLOT; the output file is GRFONT.TXT.
C 
C This program uses the PGPLOT internal routines GRSY00 and
C GRSYXD and must therefore be linked with the non-shareable library.
C
C T. J. Pearson  1987 May 6
C----------------------------------------------------------------------
      INTEGER        XYGRID(300)
      LOGICAL        UNUSED
      INTEGER        I, N, LENGTH
C-----------------------------------------------------------------------
      OPEN (UNIT=1, FILE='grfont.txt', STATUS='NEW',
     1      CARRIAGECONTROL='LIST')
      CALL GRSY00
      DO 30 N=1,4000
          CALL GRSYXD(N,XYGRID,UNUSED)
          IF (.NOT.UNUSED) THEN
c             DO 10 I=1,300
              do 10 i=2,300,2
                  IF(XYGRID(I).EQ.-64) THEN
                      IF (XYGRID(I+1).EQ.-64) THEN
                          LENGTH = I+1
                          GOTO 20
                      END IF
                  END IF
   10         CONTINUE
              STOP 'Unfortunate error'
   20         WRITE (1,'(7(2X,2I4))') N, LENGTH, (XYGRID(I), I=1,5)
              WRITE (1,'(7(2X,2I4))') (XYGRID(I),I=6,LENGTH)
          END IF
   30 CONTINUE
C-----------------------------------------------------------------------
      END
