      SUBROUTINE CNDSN

*+
*
*   Name:
*      SUBROUTINE CNDSN
*
*   Description:
*      This subroutine cancels the entire dataset and contents.
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

*   CMFILE:
      INCLUDE 'CMFILE'

      NODSN = .TRUE.

      CALL STR_TERM( 0, 81, DSNAME )

      CACHAN = .FALSE.
      DACHAN = .FALSE.
      SPCHAN = .FALSE.
      DQCHAN = .FALSE.
      MECHAN = .FALSE.

      CALL CNDATA

      END
