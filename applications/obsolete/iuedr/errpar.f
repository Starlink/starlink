      SUBROUTINE ERRPAR( STR )
*+
*   Name:
*      SUBROUTINE ERRPAR
*
*   Description:
*      Add parameter name to current error message.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      01-SEP-81
*         AT4 version.
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Converion to SGP/16 style.
*
*   Method:
*      For now just direct the text to STDERR.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB       ! arbitrary string length
      PARAMETER (ARB = 100)

*   Import:
      BYTE STR(ARB)     ! parameter name to be written

      CALL ERRSTR( STR )

      END

*+
*      Martin Clayton     20-JUL-94     IUEDR Vn. 3.1-2
*
*  "Parameter cancel error"  message generation
*
*-
      SUBROUTINE PCANER( STR, STATUS )
      IMPLICIT NONE

      INTEGER ARB
      PARAMETER (ARB = 100)

      BYTE STR(ARB)          ! The parameter name
      INTEGER STATUS

      CALL ERRSTR( STR )
      CALL ERROUT( ': parameter cancel error\\', STATUS )

      END
