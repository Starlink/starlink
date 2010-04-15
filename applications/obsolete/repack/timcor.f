*+TIMCOR - Apply the corrections
      SUBROUTINE TIMCOR(EXP,BINSIZ,ATC,NEXP,DATA,QUAL,VAR,NI,STATUS)
      IMPLICIT NONE
      INCLUDE  'EXPOS_DEF.INC'
* Input
      RECORD	/EXPOS_DEF/ EXP
      REAL	BINSIZ
      INTEGER	NI
      INTEGER   NEXP(NI)
      REAL      ATC(NI)

* Output:
      INTEGER   QUAL(NI)
      REAL      DATA(NI)
      REAL	VAR(NI)
      INTEGER   STATUS

* M. Denby Sep 88
* P McGale May 95 - UNIX mods
*-

*    Local variables :
      INTEGER	IT
      REAL	CTS
      REAL	TOLER

      IF (STATUS .NE. 0) RETURN

      TOLER = 1. ! MIN(10.,BINSIZ/10.)

*   Loop over the data
      DO IT = 1, NI
	IF (ATC(IT) .LT. TOLER) THEN
	  DATA(IT) = 0.
          VAR(IT)  = 0.
          QUAL(IT) = 1
        ELSE
	  CTS = DATA(IT)
	  DATA(IT) = CTS/ATC(IT)
	  QUAL(IT) = 0
	  IF (CTS .EQ. 0.) THEN
	    VAR(IT) = 1./ATC(IT)**2
	  ELSE
	    VAR(IT) = CTS/ATC(IT)**2
	  ENDIF
        ENDIF
      ENDDO

      END
