      SUBROUTINE NDF_EXIST( PARAM, MODE, INDF, STATUS )
*+
*  Name:
*     NDF_EXIST

*  Purpose:
*     See if an existing NDF is associated with an ADAM parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_EXIST( PARAM, MODE, INDF, STATUS )

*  Description:
*     The routine determines if an existing (and accessible) NDF is
*     associated with an ADAM parameter. If it is, then an identifier
*     is returned for it. If not, then the routine returns with an
*     identifier value of NDF__NOID; this then allows the NDF structure
*     to be created (e.g. using NDF_CREAT) if required.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of NDF access required: 'READ', 'UPDATE' or 'WRITE'.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If 'WRITE' access is specified, then all the NDF's components
*     will be reset to an undefined state ready to receive new values.
*     If 'UPDATE' access is specified, the NDF's components will retain
*     their values, which may then be modified.
*     -  The behaviour of this routine is the same as NDF_ASSOC, except
*     that in the event of the NDF structure not existing (or being
*     inaccessible), control is returned to the application with an
*     identifier value of NDF__NOID, rather than re-prompting the user.
*     -  Note that unlike the DAT_EXIST routine, on which it is
*     modelled, this routine does not set a STATUS value if the data
*     structure does not exist.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.  The
*     NDF__NOID constant is defined in the include file NDF_PAR.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1989 (RFWS):
*        Original version.
*     18-OCT-1989 (RFWS):
*        Minor changes to comments and error message.
*     29-JAN-1990 (RFWS):
*        Changed handling of re-prompts to avoid looping under
*        parameter system error conditions. Changed error messages to
*        include the parameter name using escape characters.
*     20-MAR-1990 (RFWS):
*        Implemented checking of the access mode value for validity and
*        resetting of the NDF's component values if write access is
*        requested.
*     21-MAR-1990 (RFWS):
*        Re-structured status checking.
*     25-SEP-1991 (RFWS):
*        Added user-access to NDF sections (re-implementaton based on
*        the new NDF_ASSOC routine).
*     17-OCT-1991 (RFWS):
*        Disable unwanted access modes if read access is requested but
*        write access is available.
*     4-DEC-1991 (RFWS):
*        Added a contextual error report before re-prompting after an
*        error.
*     12-AUG-1993 (RFWS):
*        Modified to call the new routine NDF1_NFIND for accessing NDF
*        data structures.
*     12-AUG-1993 (RFWS):
*        Removed resetting of access control flags if READ access is
*        requested, as this should be handled correctly by NDF1_NFIND
*        and lower level routines.
*     28-SEP-1993 (RFWS):
*        Restored resetting of access control flags - it is still
*        needed if the same NDF is accessed twice, first for UPDATE and
*        then for READ access.
*     13-OCT-1993 (RFWS):
*        Changed to allow conversion of foreign format files on input.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL )(Read)
*           Data object container file name.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF_MXACB ) = LOGICAL (Write)
*           Access control flags.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_ABSNT         ! Test for absent NDF or component

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) JUNK ! Junk locator value
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to NDF structure
      CHARACTER * ( DAT__SZLOC ) LOC0 ! Locator to HDS container file
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode string
      CHARACTER * ( NDF__SZPAR ) NAME ! NDF name string
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IACC               ! Loop counter for access modes
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IPAR               ! Parameter table index
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL HASLOC             ! Is locator associated with parameter?

*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that flushing error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Find the parameter index in the parameter tables and see whether the
*  parameter already has a locator associated with it.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETLOC( IPAR, HASLOC, JUNK, STATUS )

*  Check the access mode string for validity.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )

*  If the access mode string is valid, then check the access mode
*  against that given in the interface file. Report an error if it
*  conflicts.
C  (Removed because it requires access to the SUBPAR_ common blocks at
C  present.)
C      IF ( STATUS .EQ. SAI__OK ) THEN
C         IF ( ( VMODE .NE. 'READ' ) .AND.
C     :        ( .NOT. PARWRITE( IPAR ) ) ) THEN
C            STATUS = NDF__ACDEN
C            CALL MSG_SETC( 'MODE', MODE )
C            CALL MSG_SETC( 'PARAM', PARAM )
C            CALL ERR_REP( 'NDF_EXIST_ACON',
C     :                    '^MODE access to the ''%^PARAM'' ' //
C     :                    'parameter conflicts with the access ' //
C     :                    'mode specified in the ADAM interface ' //
C     :                    'file (possible programming error).',
C     :                    STATUS )
C         END IF
C      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop until a valid NDF structure has been obtained, or a
*  non-recoverable error occurs, or we determine that the NDF does not
*  exist.
 1       CONTINUE                ! Start of "DO WHILE" loop

*  Obtain the NDF name via the parameter and attempt to open the data
*  object.
         CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
         IACB = 0
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL NDF1_OPFOR( DAT__ROOT, NAME, VMODE, IACB, STATUS )

*  If this failed for any reason except that the NDF doesn't exist,
*  then the user must be re-prompted. Report contextual information and
*  flush any error messages.
            IF ( ( STATUS .NE. SAI__OK ) .AND.
     :           ( .NOT. NDF1_ABSNT( STATUS ) ) ) THEN
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL ERR_REP( 'NDF_EXIST_CTX',
     :            'NDF_EXIST: Unable to associate an NDF structure' //
     :            'with the ''%^PARAM'' parameter.', STATUS )
               CALL ERR_FLUSH( STATUS )

*  Cancel the parameter association, annulling any further error
*  messages this may generate.
               CALL SUBPAR_CANCL( IPAR, STATUS )
               CALL ERR_ANNUL( STATUS )

*  Return to re-prompt.
               GO TO 1
            END IF
         END IF
      END IF

*  After importing a valid data structure, check if the parameter
*  system already has locators for it. If not, then save locators for
*  the NDF object and the container file in the parameter system and
*  link the object locator with the parameter name.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. HASLOC ) CALL NDF1_PTLOC( PARAM, IPAR, VMODE, IACB,
     :                                        STATUS )

*  If read access was requested, then disable all unwanted access modes.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( VMODE .EQ. 'READ' ) THEN
               DO 2 IACC = 1, NDF__MXACC
                  ACB_ACC( IACC, IACB ) = .FALSE.
 2             CONTINUE

*  If write access was requested, then reset any pre-existing NDF
*  component values.
            ELSE IF ( VMODE .EQ. 'WRITE' ) THEN
               CALL NDF1_RST( IACB, '*', STATUS )
            END IF
         END IF

*  Export an NDF_ identifier.
         CALL NDF1_EXPID( IACB, INDF, STATUS )

*  If an error occurred, then annul any ACB entry which may have been
*  acquired.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL NDF1_ANL( IACB, STATUS )

*  Cancel the parameter association, without generating any further
*  error messages.
            TSTAT = SAI__OK
            CALL ERR_MARK
            CALL SUBPAR_CANCL( IPAR, TSTAT )
            CALL ERR_ANNUL( TSTAT )
            CALL ERR_RLSE
         END IF
      END IF

*  If an error occurred, then classify it...

*  If the NDF does not exist, then this condition is simply indicated
*  by a returned INDF value of NDF__NOID, so annul the error.
      IF ( NDF1_ABSNT( STATUS ) ) THEN
         CALL ERR_ANNUL( STATUS )

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_EXIST_ABT',
     :   'Aborted attempt to associate an existing NDF structure ' //
     :   'with the ''%^PARAM'' parameter.', STATUS )

*  If a "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_EXIST_NULL',
     :   'Null NDF structure specified for the ''%^PARAM'' ' //
     :   'parameter.', STATUS )

*  For other errors, add context information and call the error tracing
*  routine.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_EXIST_ERR',
     :   'NDF_EXIST: Error determining if an existing NDF structure' //
     :   'is associated with the ''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_EXIST', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
