      SUBROUTINE DESTA3( QNAME, INDF, XNAME, XTYPE, LOCS, OK, STATUS )
*+
*  Name:
*     DESTA3

*  Purpose:
*     Store quality name in output NDF for flagging un-used samples.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DESTA3( QNAME, INDF, XNAME, XTYPE, LOCS, OK, STATUS )

*  Description:
*     If no quality name definitions exist in the output NDF, new
*     quality name structures are created within the named extension
*     (the extension is created if it does not already exist). If the
*     given quality name already exists, a warning is issued and the
*     locators to the quality information are rleased. In this case
*     argument OK is returned false, and no attempt should be made to
*     use the returned value of argument LOCS. If the quality name does
*     not already exist, then a definition of the quality name is
*     created and OK is returned true.

*  Arguments:
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to use to flag samples which are not used in
*        the estimation of detector offsets. If QNAME is blank, OK is
*        returned .FALSE., but no further action is taken.
*     INDF = INTEGER (Given)
*        NDF identifier for the output NDF.
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of NDF extension in which to store quality name defintions
*        if none already exist.
*     XTYPE = CHARACTER * ( * ) (Given)
*        HDS type to be assigned to any created NDF extension.
*     LOCS( 5 ) = CHARACTER * ( * ) (Returned)
*        Locators for quality name definitions needed by IRQ routines.
*     OK = LOGICAL (Returned)
*        .TRUE. if the locators returned by LOCS are valid. Returned
*        .TRUE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ_ error constants.

*  Arguments Given:
      CHARACTER QNAME * ( * )
      INTEGER INDF
      CHARACTER XNAME * ( * )
      CHARACTER XTYPE * ( * )

*  Arguments Returned:
      CHARACTER LOCS( 5 ) * ( * )
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER XLOC*(DAT__SZLOC)! Locator to NDF extension.
      LOGICAL THERE              ! True if object exists.

*.

*  Initialise the returned value of OK.
      OK = .TRUE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied value of QNAME. Return with OK set false if a
*  blank value was supplied.
      IF( QNAME .EQ. ' ' ) THEN
         OK = .FALSE.
         GO TO 999
      END IF

*  Attempt to locate quality name definitions within the output NDF.
      CALL IRQ_FIND( INDF, LOCS, XNAME, STATUS )

*  If no quality name information was found, annul the error.
      IF( STATUS .EQ. IRQ__NOQNI ) THEN
         CALL ERR_ANNUL( STATUS )

*  See if the named extension exists in the NDF.
         CALL NDF_XSTAT( INDF, XNAME, THERE, STATUS )

*  If not, create it, and annul the locator to the extension.
         IF( .NOT. THERE ) THEN
            CALL NDF_XNEW( INDF, XNAME, XTYPE, 0, 0, XLOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
         END IF

*  Create structures for storing quality name definitions in the output
*  NDF.
         CALL IRQ_NEW( INDF, XNAME, LOCS, STATUS )

      END IF

*  Attempt to find the quality name given by QNAME.
      CALL IRQ_CHKQN( LOCS, QNAME, THERE, STATUS )

*  If it already exists, warn the user that un-used samples will not be
*  flagged. Release the locators, and flag that no quality values are to
*  be assigned to this output NDF.
      IF( THERE ) THEN
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
         CALL MSG_SETC( 'QN', QNAME )
         CALL MSG_OUTIF( MSG__QUIET, 'DESTA3_MSG1',
     :                 'WARNING: Quality name ^QN is already in use. '//
     :                 'Cannot flag un-used samples in output NDF.',
     :                 STATUS )
         CALL IRQ_RLSE( LOCS, STATUS )      
         IF( STATUS .EQ. SAI__OK ) OK = .FALSE.

*  If the quality name does not already exist, add a definition.
      ELSE
         CALL IRQ_ADDQN( LOCS, QNAME, .FALSE., 'DESTCRDD: Samples '//
     :                   'excluded from offset estimation', STATUS )
      END IF

 999  CONTINUE

      END
