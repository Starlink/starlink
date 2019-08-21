      SUBROUTINE NDF_CREPL( PARAM, PLACE, STATUS )
*+
*  Name:
*     NDF_CREPL

*  Purpose:
*     Create a new NDF placeholder via the ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CREPL( PARAM, PLACE, STATUS )

*  Description:
*     The routine creates a new NDF placeholder via the ADAM parameter
*     system, associates it with a parameter, and returns an identifier
*     for it.  A placeholder is used to identify a position in the
*     underlying data system (HDS) and may be passed to other routines
*     (e.g. NDF_NEW) to indicate where a newly created NDF should be
*     positioned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     PLACE = INTEGER (Returned)
*        NDF placeholder identifying the nominated position in the
*        data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new NDF, where they are
*     effectively exchanged for an NDF identifier.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOPL will be returned for the PLACE argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOPL
*     constant is defined in the include file NDF_PAR.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-NOV-2010 (DSB):
*        original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZPAR ) NAME ! Placeholder name string
      INTEGER IPAR               ! Parameter table index
      INTEGER IPCB               ! Index to placeholder entry in the PCB
      INTEGER TSTAT              ! Temporary status variable

*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that annulling error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Find the parameter index in the parameter tables.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )

*  Loop until a valid NDF structure has been created or a
*  non-recoverable error occurs.
 1    CONTINUE                ! Start of 'DO WHILE' loop

*  Obtain the placeholder location via the parameter.
      CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a placeholder entry for the object in the PCB.
         CALL NDF1_PLFOR( DAT__ROOT, NAME, IPCB, STATUS )

*  If this failed, then the user must be re-prompted. Report contextual
*  information and flush any error messages.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'NDF_CREPL_CTX',
     : 'NDF_CREPL: Unable to create a new NDF placeholder via the ' //
     : '''%^PARAM'' parameter.',
     :                       STATUS )
            CALL ERR_FLUSH( STATUS )

*  Cancel the parameter association, annulling any further error
*  messages this may generate.
            CALL SUBPAR_CANCL( IPAR, STATUS )
            CALL ERR_ANNUL( STATUS )

*  Return to re-prompt.
            GO TO 1
         END IF
      END IF

*  Export the required placeholder.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  If an error occurred, then annul the PCB entry.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANNPL( .TRUE., IPCB,
     :                                               STATUS )
      END IF

*  If an error occurred, then classify it...

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREPL_ABT',
     :   'Aborted creation of a new NDF placeholder via the ' //
     :   '''%^PARAM'' parameter.', STATUS )

*  If an "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREPL_NULL',
     :   'Null NDF placeholder specified for the ''%^PARAM'' ' //
     :   'parameter.', STATUS )

*  For other errors, add context information and call the error tracing
*  routine.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREPL_ERR',
     :   'NDF_CREPL: Error creating an NDF placeholder via the ' //
     :   '''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_CREPL', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
