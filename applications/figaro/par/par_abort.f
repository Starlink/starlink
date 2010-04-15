C+
C                           P A R _ A B O R T
C
C  Routine name:
C     PAR_ABORT
C
C  Function:
C     Indicates if any parameter request has been aborted by the user.
C
C  Description:
C     This routine adds support for user-requested aborts in response
C     to a parameter prompt.  If the user replies with an abort request
C     to any prompt, the parameter routine in question will return a
C     default value and set an abort flag.  This abort flag will prevent
C     any further parameter prompts, and can be tested by this routine.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     ABORT = PAR_ABORT ()
C
C  Parameters:   None.
C
C  Returns:
C
C     (<) ABORT   (Logical,function value) True if the parameter system
C                 abort flag is set, false if not.
C
C  Prior requirements:
C     PAR_INIT must have been called to initialise the parameter system.
C
C  Common variable details:
C     (>) ABORT    (Logical) The parameter system abort flag.
C
C  History:
C     5th Sept 1988   Original version.  KS / AAO.
C     12th Aug 1992   New include file.  HME/ UoE, Starlink.
C-
      LOGICAL FUNCTION PAR_ABORT ()
C
      IMPLICIT NONE
C
      INCLUDE 'PARBLK'
C
      PAR_ABORT=ABORT
C
      END
