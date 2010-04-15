C+
C                            G K D _ I N I T
C
C  Routine name:
C     GKD_INIT
C
C  Function:
C     Dummy subroutine.
C
C  Description:
C     This routine used to attempt to clear the alphanumeric text
C     screen, if any, of the terminal being used for interactive
C     graphics. That routine is discontinued, this routine returns
C     without action.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL GKD_INIT (DEVICE)
C
C  Parameters:   (">" input,"!" modified, "W" workspace, "<" output)
C
C     (>) DEVICE         (Fixed string,descr) Ignored.
C
C  History:
C     10th March 1988 Original version.  KS / AAO.
C     18th Aug   1992 Discontinue the routine.  HME / UoE, Starlink.
C-
      SUBROUTINE GKD_INIT (DEVICE)
C
      IMPLICIT NONE
C
      CHARACTER*(*) DEVICE
C
      END
