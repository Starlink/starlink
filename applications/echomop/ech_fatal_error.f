      LOGICAL FUNCTION ECH_FATAL_ERROR( ERROR_CODE )
*+
*  Name:
*     ECH_FATAL_ERROR

*  Description:
*     This function return TRUE if the error code passed to it is to
*     potentially cause an abort. This function is used to allow any
*     ECH_ routines called to exit immediately, thus facilitating return
*     of control to the user-interface level where appropriate action
*     can be taken depending upon context (eg BATCH,INTERACTIVE).

*  Arguments:
*     ERROR_CODE = INTEGER (Given)

*  Created:
*     12 Nov 1989 DJM / UCL

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      INTEGER ERROR_CODE
*.
      IF ( ERROR_CODE .EQ. PAR__ABORT ) THEN
         ECH_FATAL_ERROR = .TRUE.
         ERROR_CODE = ECH__ABORT_INIT

      ELSE IF ( ERROR_CODE .LE. ECH__NEED_RDCOBJ .AND.
     :          ERROR_CODE .GE. ECH__NOCR_ACCESS ) THEN
         ECH_FATAL_ERROR = .TRUE.

      ELSE
         ECH_FATAL_ERROR = .FALSE.
      END IF

      END
