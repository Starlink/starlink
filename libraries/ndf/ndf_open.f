      SUBROUTINE NDF_OPEN( LOC, NAME, MODE, STAT, INDF, PLACE, STATUS )
*+
*  Name:
*     NDF_OPEN

*  Purpose:
*     Open an existing or new NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_OPEN( LOC, NAME, MODE, STAT, INDF, PLACE, STATUS )

*  Description:
*     The routine finds an existing NDF data structure and returns an
*     identifier for it, or creates a placeholder for a new NDF.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the enclosing HDS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the HDS structure component.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of NDF access required: 'READ', 'UPDATE' or 'WRITE'.
*     STAT = CHARACTER * ( * ) (Given)
*        The state of the NDF, specifying whether it is known to exist
*        or not: 'NEW', 'OLD', or 'UNKNOWN'.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     PLACE = INTEGER (Returned)
*        NDF placeholder identifying the nominated position in the
*        data system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the STAT argument is set to 'NEW', then this routine will
*     return a placeholder for a new NDF. If STAT is set to 'OLD', it
*     will search for an existing NDF. If STAT is set to 'UNKNOWN', it
*     will first search for an existing NDF but will return a
*     placeholder for a new NDF if an existing one cannot be found.
*     -  If this routine succeeds, then a valid value will be returned
*     for INDF if the NDF already existed, or for PLACE if it did not
*     exist. The unused return argument will be set to the appropriate
*     null value (NDF__NOID or NDF__NOPL respectively).
*     -  If 'WRITE' access is specified for an existing NDF, then all
*     the NDF's components will be reset to an undefined state ready to
*     receive new values.  If 'UPDATE' access is specified, the NDF's
*     components will retain their values, which may then be modified.
*     -  An error will result if the STAT argument is set to 'OLD' but
*     no existing NDF could be found. An error will also result if a
*     placeholder for a new NDF is to be returned but 'READ' access was
*     requested.
*     -  The value given for the NAME argument may be an HDS path name,
*     consisting of several fields separated by '.', so that an NDF can
*     be opened in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator LOC.  Array subscripts may
*     also be used in this component name.  Thus a string such as
*     'MYSTRUC.ZONE(2).IMAGE' could be used as a valid NAME value.
*     -  An NDF can be opened within an explicitly named container file
*     by supplying the symbolic value DAT__ROOT for the LOC argument
*     and giving a full HDS object name (including a container file
*     specification) for the NAME argument.
*     -  If a blank value is given for the NAME argument, then the
*     NDF will be the object identified directly by the locator LOC.
*     -  If a placeholder is to be returned and the new NDF is to be a
*     top-level object, then a new container file will be created.
*     Otherwise, the container file and all structures lying above the
*     new NDF should already exist.
*     -  If the LOC and NAME arguments identify a pre-existing object
*     which is not a top-level object, then this may be used as the
*     basis for the new NDF. An object which is to be used in this way
*     must be an empty scalar structure with an HDS type of 'NDF'.
*     -  The locator supplied as input to this routine may later be
*     annulled without affecting the behaviour of the NDF_ system.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOPL will be returned for the PLACE argument, and a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same values will also be
*     returned if the routine should fail for any reason.
*     -  The NDF__NOPL and NDF__NOID constants are defined in the
*     include file NDF_PAR. The DAT__ROOT constant is defined in the
*     include file DAT_PAR (see SUN/92).

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
*     DSB: David Berry (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1993 (DSB):
*        Original version.
*     10-AUG-1993 (RFWS):
*        Revised the prologue.
*     12-AUG-1993 (RFWS):
*        Re-structured to export NDF identifiers explicitly rather than
*        depending on an internal routine to do this.
*     12-AUG-1993 (RFWS):
*        Handle ACB access control flags correctly and reset NDF
*        components if WRITE access is specified.
*     2-NOV-1993 (RFWS):
*        Updated to support foreign format files.
*     4-NOV-1993 (RFWS):
*        Updated to return placeholders which may refer to foreign
*        format files.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF_MXACB ) = LOGICAL (Read and Write)
*           NDF access control flags.

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) STAT

*  Arguments Returned:
      INTEGER INDF
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_ABSNT         ! Test for absent NDF or component

*  Local Variables:
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode string
      CHARACTER * ( NDF__SZSTA ) VSTAT ! Validated NDF state string
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IPCB               ! Index to new NDF entry in the PCB
      LOGICAL CANWRT             ! NDF is writeable?

*.

*  Set inital values for INDF and PLACE.
      INDF = NDF__NOID
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the access mode and state strings for validity.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )
      CALL NDF1_VSTAT( STAT, VSTAT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the ACB index.
         IACB = 0

*  OLD.
*  ===
*  First deal with existing NDFs. Find and import the data structure and
*  export an NDF identifier for it.
         IF ( VSTAT .EQ. 'OLD' ) THEN
            CALL NDF1_OPFOR( LOC, NAME, VMODE, IACB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL NDF1_EXPID( IACB, INDF, STATUS )

*  If an error occurred, then annul the ACB entry.
               IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANL( IACB, STATUS )
            END IF

*  NEW.
*  ===
*  Now deal with new NDFs. If READ access is not required, then obtain
*  a PCB entry for the new NDF and export a placeholder for it.
         ELSE IF( VSTAT .EQ. 'NEW' ) THEN
            IF( VMODE .NE. 'READ' ) THEN
               CALL NDF1_PLFOR( LOC, NAME, IPCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  If an error occurred, then annul the PCB entry.
                  IF ( STATUS .NE. SAI__OK )
     :               CALL NDF1_ANNPL( .TRUE., IPCB, STATUS )
               END IF

*  Report an error if READ access was requested.
            ELSE
               STATUS = NDF__MODIN
               CALL ERR_REP( 'NDF_OPEN_RD1',
     :                       'READ access is not permitted when ' //
     :                       'accessing a new NDF (possible ' //
     :                       'programming error).', STATUS )
            END IF

*  UNKNOWN.
*  =======
*  Now deal with NDFs of unknown state. Defer error reporting and
*  attempt to access the NDF assuming it already exists. If this
*  succeeds, then export an identifier for it (if an error occurs,
*  annul the ACB entry).
         ELSE
            CALL ERR_MARK
            CALL NDF1_OPFOR( LOC, NAME, VMODE, IACB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL NDF1_EXPID( IACB, INDF, STATUS )
               IF ( STATUS .NE. SAI__OK ) CALL NDF1_ANL( IACB, STATUS )

*  If the NDF could not be accessed because it doesn't exist, then
*  annul the error.
            ELSE IF ( NDF1_ABSNT( STATUS ) ) THEN
               CALL ERR_ANNUL( STATUS )

*  If READ access is not required, then obtain a PCB entry for a new
*  NDF and export a placeholder for it.
               IF( VMODE .NE. 'READ' ) THEN
                  CALL NDF1_PLFOR( LOC, NAME, IPCB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL NDF1_EXPPL( IPCB, PLACE, STATUS )

*  If an error occurred, then annul the PCB entry.
                     IF ( STATUS .NE. SAI__OK )
     :                  CALL NDF1_ANNPL( .TRUE., IPCB, STATUS )
                  END IF

*  Report an error if READ access was requested and the NDF didn't
*  exist.
               ELSE
                  STATUS = NDF__MODIN
                  CALL ERR_REP( 'NDF_OPEN_RD2',
     :                          'READ access is not permitted when ' //
     :                          'accessing a new NDF (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF

*  Release the error stack.
            CALL ERR_RLSE
         END IF
      END IF

*  If no error has occurred and an existing NDF has been accessed, then
*  check if READ access was requested.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( IACB .NE. 0 ) ) THEN
         IF ( VMODE .EQ. 'READ' ) THEN

*  If so, then disable all unwanted modes of access. This must be done
*  because it is possible to request READ access to an object for which
*  a writeable locator has been supplied. It is also possible to access
*  the same object twice - first for UPDATE and then for READ access.
            DO 1 IACC = 1, NDF__MXACC
               ACB_ACC( IACC, IACB ) = .FALSE.
 1          CONTINUE

*  If any other access mode was requested, then examine all the ACB
*  access control flags to ensure that the object is writeable.
         ELSE
            CANWRT = .TRUE.
            DO 2 IACC = 1, NDF__MXACC
               CANWRT = CANWRT .AND. ACB_ACC( IACC, IACB )
 2          CONTINUE

*  If it is not writeable, then a read-only HDS locator has been
*  supplied, so report an error.
            IF ( .NOT. CANWRT ) THEN
               STATUS = NDF__ACDEN
               CALL MSG_SETC( 'MODE', VMODE )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_OPEN_ACC',
     :                       '^MODE access to the NDF structure ' //
     :                       '^NDF is not available via the ' //
     :                       'specified HDS locator (possible ' //
     :                       'programming error).', STATUS )

*  Annul the ACB entry.
               CALL NDF1_ANL( IACB, STATUS )

*  If WRITE access was requested to an existing NDF, then reset any
*  pre-existing NDF component values.
            ELSE IF ( VMODE .EQ. 'WRITE' ) THEN
               CALL NDF1_RST( IACB, '*', STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then reset the returned identifier and
*  placeholder, report context information and call the error tracing
*  routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         INDF = NDF__NOID
         PLACE = NDF__NOPL
         CALL ERR_REP( 'NDF_OPEN_ERR',
     :   'NDF_OPEN: Error opening an NDF data structure.', STATUS )
         CALL NDF1_TRACE( 'NDF_OPEN', STATUS )
      END IF

      END
