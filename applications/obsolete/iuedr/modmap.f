      SUBROUTINE MODMAP

*+
*
*   Name:
*      SUBROUTINE MODMAP
*
*   Description:
*     If there is a mapped spectrum, mark it as needing to be written
*     to disk to retain changes.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   CMFILE:
      INCLUDE 'CMFILE'

*   CMCOMB:
      INCLUDE 'CMCOMB'

      IF (.NOT.NOCOMB) MECHAN = .TRUE.

      END
