      SUBROUTINE MODUED

*+
*
*   Name:
*      SUBROUTINE MODUED
*
*   Description:
*      If there is a current dataset, mark its image part as requiring
*      update.
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

      IF (.NOT.NODSN) DACHAN = .TRUE.

      END
