      SUBROUTINE ERR_FLBEL( STATUS )
*+
*  Name:
*     ERR_FLBEL

*  Purpose:
*     Deliver an ASCII BEL and flush the current error context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_FLBEL( STATUS )

*  Description:
*     An ASCII BEL character is delivered to the user and then all
*     pending error messages in the current error context are delivered
*     to the user using a call to ERR_FLUSH. On successful completion, 
*     the error context is annulled and the status argument reset to 
*     SAI__OK; if an error occurs during output of the error messages, the 
*     error context is not annulled and the status argument is returned 
*     set to ERR__OPTER.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return if the 
*        error message output is successful; if not, it is set to 
*        ERR__OPTER.

*  Algorithm:
*     -  Call ERR1_PRERR to deliver the BEL character.
*     -  Call ERR_FLUSH to flush the current error context.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1993 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'ERR_SYS'                 ! ERR_ private constants

*  Global Variables:
      INCLUDE 'ERR_CMN'                 ! ERR_ global variables

*  Status:
      INTEGER STATUS

*.

*  Set the ERR_ bell flag.
      ERRBEL = ( .NOT. ERR__NOBEL )

*  Flush the current error context.
      CALL ERR_FLUSH( STATUS )

      END
