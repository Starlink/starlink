      SUBROUTINE ERR1_PRERR( TEXT, STATUS )
*+
*  Name:
*     ERR1_PRERR

*  Purpose:
*     Deliver the text of an error message to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR1_PRERR( TEXT, STATUS )

*  Description:
*     The given error message text is delivered to the user.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Notes:
*     -  This is the stand-alone version of ERR1_PRERR.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1991 (PCTR):
*        Original version.
*     8-AUG-1991 (PCTR):
*        Modified for EMS Vn. 1.3
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'ERR_ERR'                 ! ERR_ error codes
      INCLUDE 'ERR_PAR'                 ! ERR_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      INTEGER IOSTAT                    ! Fortran IOSTAT status
      INTEGER LENG                      ! String length

*.

*  Get the length of the error message.
      LENG = MIN( CHR_LEN( TEXT ), ERR__SZMSG )

*  Attempt to deliver the message via the user interface.
      IF ( LENG .GT. 0 ) THEN
         WRITE( *, '( A )', IOSTAT = IOSTAT ) TEXT
      ELSE
         WRITE( *, '( 1X )', IOSTAT = IOSTAT )
      END IF

*  Check the Fortran I/O status and set the returned status if
*  neccessary.
      IF ( IOSTAT .NE. 0 ) STATUS = ERR__OPTER

      END
