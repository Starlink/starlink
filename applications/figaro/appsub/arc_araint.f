C+
      SUBROUTINE ARC_ARAINT (PARMS)
C
C     A R A I N T
C
C     Initialises the parameter array used to control the automatic
C     line search.
C
C     Parameters -  ("<" output)
C
C     (<) PARMS   (Real array PARMS(2)) The autofit parameters.  For
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
C     Initial values
C
      REAL CHFACT, SIGFACT
      PARAMETER (CHFACT=3.0, SIGFACT=3.0)
C
C     Set both CHFACT and SIGFACT
C
      PARMS(1)=CHFACT
      PARMS(2)=SIGFACT
C
      RETURN
      END
