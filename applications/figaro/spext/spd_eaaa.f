      SUBROUTINE SPD_EAAA( NDF, ACCESS, XTHERE, XLOC, STATUS )
*+
*  Name:
*     SPD_EAAA

*  Purpose:
*     Locate Specdre Extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAAA( NDF, ACCESS, XTHERE, XLOC, STATUS )

*  Description:
*     This routine looks whether a given main NDF has a Specdre
*     Extension and returns an HDS locator to the Extension. Depending
*     on the access requested, the Extension may be cleared of any
*     existing contents or it may be created if it does not yet exist.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF. The routine looks whether
*        this NDF has an Extension called Specdre.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access requested. This can be
*        -  'READ': The Extension is not created. If it does not already
*           exist then XTHERE will be false and XLOC invalid.
*        -  'WRITE': If the Extension exists, all its components are
*           deleted. This will return XTHERE true and a valid locator.
*        -  'UPDATE': An existing Extension remains unchanged. If no
*           Extension exists, it is created. This will return XTHERE
*           true and a valid locator.
*     XTHERE = LOCIGAL (Returned)
*        True if the Extension was found or created.
*     XLOC = CHARACTER * ( * ) (Returned)
*        The HDS locator to the Extension. DAT__NOLOC is returned if the
*        Extension does not exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set, if the given NDF identfier is
*        invalid or if the given access mode is invalid.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07-MAY-1992 (HME):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension parameters

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      LOGICAL XTHERE
      CHARACTER * ( * ) XLOC

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check given parameters.
      IF ( NDF .EQ. NDF__NOID ) THEN
         XTHERE = .FALSE.
         XLOC = DAT__NOLOC
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVNDF',
     :       'SPD_EAAA: Error: Invalid NDF identifier.', STATUS )
         GO TO 500
      END IF
      IF ( ACCESS .NE. 'READ' .AND. ACCESS .NE. 'WRITE' .AND.
     :     ACCESS .NE. 'UPDATE' ) THEN
         XTHERE = .FALSE.
         XLOC = DAT__NOLOC
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVACC',
     :       'SPD_EAAA: Error: Invalid mode access requested.', STATUS )
         GO TO 500
      END IF

*  Common action for all access modes.
      CALL NDF_XSTAT( NDF, XNAME, XTHERE, STATUS )

*  If read access, look if Extension is there and get its locator.
      IF ( ACCESS .EQ. 'READ' ) THEN
         IF ( XTHERE ) THEN
            CALL NDF_XLOC( NDF, XNAME, ACCESS, XLOC, STATUS )
         ELSE
            XLOC = DAT__NOLOC
         END IF

*  Else if write access, look if Extension is there, delete it, create
*  it.
      ELSE IF ( ACCESS .EQ. 'WRITE' ) THEN
         IF ( XTHERE ) CALL NDF_XDEL( NDF, XNAME, STATUS )
         XTHERE = .FALSE.
         CALL NDF_XNEW(  NDF, XNAME, XTYPE, 0, 0, XLOC, STATUS )
         CALL NDF_XSTAT( NDF, XNAME, XTHERE, STATUS )

*  Else if update access, look if Extension is there, create if
*  necessary.
      ELSE IF ( ACCESS .EQ. 'UPDATE' ) THEN
         IF ( XTHERE ) THEN
            CALL NDF_XLOC(  NDF, XNAME, ACCESS, XLOC, STATUS )
         ELSE
            CALL NDF_XNEW(  NDF, XNAME, XTYPE, 0, 0, XLOC, STATUS )
            CALL NDF_XSTAT( NDF, XNAME, XTHERE, STATUS )
         END IF
      END IF

*  Return.
 500  CONTINUE
      END
