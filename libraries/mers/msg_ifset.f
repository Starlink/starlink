      SUBROUTINE MSG_IFSET( FILTER, STATUS )
*+
*  Name:
*     MSG_IFSET

*  Purpose:
*     Set the filter level for conditional message output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_IFSET( FILTER, STATUS )

*  Description:
*     The value of the message filtering level is set using the given
*     filtering value. If no such level exists, then an error is 
*     reported and the status returned set to MSG__IFINV: the current 
*     filtering level remains unchanged.

*  Arguments:
*     FILTER = INTEGER (Given)
*        The filtering level.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'MSG_ERR'          ! MSG_ error codes

*  Global Variables:
      INCLUDE 'MSG_CMN'          ! MSG_ output filter level

*  Arguments Given:
      INTEGER FILTER

*  Status:
      INTEGER STATUS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the given filter value is acceptable.
      IF ( FILTER .LT. MSG__QUIET .OR. FILTER .GT. MSG__VERB ) THEN

*     The given value for message filtering is outside the allowed
*     range: set status and report an error message.
         CALL EMS_MARK
         STATUS = MSG__INVIF
         CALL EMS_SETI( 'FILTER', FILTER )
         CALL EMS_REP( 'MSG_IFSET_INVIF',
     :   'MSG_IFSET: Invalid message filtering value: ^FILTER', 
     :   STATUS )
         CALL EMS_RLSE
      ELSE

*     Assign the message filtering level.
         MSGINF = FILTER
      END IF

      END
