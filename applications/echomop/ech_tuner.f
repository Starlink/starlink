      SUBROUTINE ECH_TUNER
*+
*  Name:
*     ECHOMOP - ECH_TUNER

*  Purpose:
*     Interactive tuning of hidden parameters.

*  Description:
*     This routine provides a semi-helpful method of examining and setting
*     all of the tuning (TUNE_) parameters available for all ECH tasks.
*
*     This routine supports the interactive setup/inquiry of all the tuning
*     parameters for all the ECHOMOP tasks.

*  Invocation:
*     CALL ECH_TUNER

*  Arguments:
*     None.

*  Method:
*     Proccessing done by module ECH_ACTIVE_TUNE

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'

*  Status:
      INTEGER STATUS

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

      CALL ECH_SET_CONTEXT( 'TUNING' , 'Global' )
      CALL ECH_INITIALISE( STATUS )
      CALL ECH_ACTIVE_TUNE( ' ' ,  ' ' , STATUS )
      CALL ECH_CLOSEDOWN( STATUS )

      END
