C+
      SUBROUTINE ARC_ARPARM (PARMS)
C
C     A R P A R M
C
C     Allows the user to modify the parameter array values used to
C     control the automatic line search.
C
C     Parameters -  ("!" modified)
C
C     (!) PARMS   (Real array PARMS(2)) The autofit parameters.  For
C                 details, see the ARAUTO listing.  PARMS(1) is
C                 CHFACT, PARMS(2) is SIGFACT.
C
C                                              KS / AAO 30th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      REAL PARMS(2)
C
C     Local variables
C
      INTEGER STATUS
      REAL VALUE
C
C     Explain the function of the parameters.
C
      CALL FIG_HELP('arcparm',STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to open explanatory text file',STATUS)
      END IF
      CALL PAR_CNPAR('CHFACT')
      CALL PAR_RDVAL('CHFACT',1.,100.,PARMS(1),' ',VALUE)
      PARMS(1)=VALUE
      CALL PAR_CNPAR('SIGFACT')
      CALL PAR_RDVAL('SIGFACT',1.,100.,PARMS(2),' ',VALUE)
      PARMS(2)=VALUE
C
      RETURN
      END
