      SUBROUTINE CONTDEF(NPARAMS,PARAMS,CONT_ST,CONT_EN,NZONES,OUT_LU)
C+
C
C Subroutine:
C
C     C O N T D E F
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), CONT_ST (>), CONT_EN (>), NZONES (>)
C OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C This subroutine lets the user define some continuum bins (either
C by including them on the command line or by defining them interactively
C using the cursor.)
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
C
C The continuum bin ranges
C
      REAL CONT_ST(*)
      REAL CONT_EN(*)
      INTEGER NZONES
C
C Misc.
C
      INTEGER I
      REAL MARK_1,MARK_2,Y
      CHARACTER*1 CH
C
C There must be an even number of parameters for this command
C
      IF (NPARAMS.GT.0) THEN
       IF ( REAL(NPARAMS/2).EQ.(REAL(NPARAMS)/2.) ) THEN
        NZONES = 1
        DO I = 1,NPARAMS,2
         CONT_ST(NZONES) = PARAMS(I)
         CONT_EN(NZONES) = PARAMS(I+1)
         NZONES = NZONES+1
        ENDDO
         NZONES = NZONES-1
         ELSE
         CALL WR_ERROR('Odd number of parameters',OUT_LU)
         GOTO 666
       ENDIF
      ELSE
C
C If no parameters are given on the command line then it is aussmed
C that the user wishes to use the cursor to define the bins.
C
10     FORMAT(1X,'Start: ',F7.1)
20     FORMAT(1X,'End: ',F7.1)
C
C
       MARK_1 = 0.
       MARK_2 = 1.E-33
       NZONES = 0
C
C A bin with start.lt.end will finish the bin definition
C
       DO WHILE(MARK_2.GT.MARK_1)
        NZONES = NZONES+1
        MARK_1 = MARK_2
        WRITE(OUT_LU,*) 'Zone number',NZONES
        CALL PGCURSE(MARK_1,Y,CH)
        WRITE(OUT_LU,10) MARK_1
        CONT_ST(NZONES) = MARK_1
        MARK_2 = MARK_1
        CALL PGCURSE(MARK_2,Y,CH)
        WRITE(OUT_LU,20) MARK_2
        CONT_EN(NZONES) = MARK_2
       ENDDO
       NZONES = NZONES-1
      ENDIF
666   CONTINUE
      END
