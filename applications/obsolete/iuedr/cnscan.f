      SUBROUTINE CNSCAN

*+
*
*   Name:
*      SUBROUTINE CNSCAN
*
*   Description:
*      This routine cancels the current Scan contents.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          21-SEP-88     IUEDR Vn. 2.0
*
*   Method:
*      Propagate cancellation to all lower levels via structured calls.
*
*-

*   Implicit:
      IMPLICIT NONE

*   CMSCAN:
      INCLUDE 'CMSCAN'

      NOSCAN = .TRUE.

      END
