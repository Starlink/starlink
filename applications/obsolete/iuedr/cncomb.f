      SUBROUTINE CNCOMB

*+
*
*   Name:
*      	SUBROUTINE CNCOMB
*
*   Description:
*      This cancels the current CMCOMB contents.
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

*   CMCOMB:
      INCLUDE 'CMCOMB'

      NOCOMB = .TRUE.

      END
