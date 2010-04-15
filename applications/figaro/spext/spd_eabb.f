      SUBROUTINE SPD_EABB( NDF, XLOC, SPAXIS, STATUS )
*+
*  Name:
*     SPD_EABB

*  Purpose:
*     Set number of spectroscopic axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EABB( NDF, XLOC, SPAXIS, STATUS )

*  Description:
*     This routine sets the number of the spectroscopic axis to the
*     given value. If necessary, the structure in the Specdre Extension
*     to hold the value is created. The given value is checked against
*     the dimensionality of the main's base NDF. If the value is
*     acutally changed, the structures SPECVALS, SPECWIDS, COVRS, and
*     RESULTS in the Specdre Extension are deleted, since they are
*     rendered invalid by changing the spectroscopic axis.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator the Specdre Extension. This should be an
*        extension to the main NDF.
*     SPAXIS = INTEGER (Given)
*        The value to be stored in the SPECAXIS structure of the Specdre
*        Extension. This must be greater than or equal to 1 and less
*        than or equal to the number of axes in the main's base NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02-MAR-1992 (HME):
*        Original version (SPAAW).
*     22-JUN-1992 (HME):
*        Make it an SPE-routine. Use main's base NDF dimensionality to
*        check against.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension paramters

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      INTEGER SPAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER OLDVAL             ! Old value of SPECAXIS
      INTEGER BNDF               ! Identifier of main's base NDF
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER DIMS( NDF__MXDIM ) ! Lower bounds of NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the given value against its limits.
      CALL NDF_BASE( NDF, BNDF, STATUS )
      CALL NDF_DIM( BNDF, NDF__MXDIM, DIMS, NDIM, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )
      IF ( SPAXIS .LT. 1 .OR. SPAXIS .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVDAT', 'SPD_EABB: Error: ' //
     :      'Spectroscopic axis would be out of range.', STATUS )
         GO TO 500
      END IF

*  Get the old value, which defaults to 1.
      OLDVAL = 1
      CALL NDF_XGT0I( NDF, XNAME, XCMP1, OLDVAL, STATUS )

*  Need action only if the new value is not the old one.
      IF ( SPAXIS .NE. OLDVAL ) THEN

*     Put the value.
         CALL NDF_XPT0I( SPAXIS, NDF, XNAME, XCMP1, STATUS )

*     Delete SPECVALS, SPECWIDS, COVRS, RESULTS.
         CALL DAT_THERE( XLOC, XCMP6, EXIST, STATUS )
         IF ( EXIST ) CALL DAT_ERASE( XLOC, XCMP6, STATUS )
         CALL DAT_THERE( XLOC, XCMP7, EXIST, STATUS )
         IF ( EXIST ) CALL DAT_ERASE( XLOC, XCMP7, STATUS )
         CALL DAT_THERE( XLOC, XCMP8, EXIST, STATUS )
         IF ( EXIST ) CALL DAT_ERASE( XLOC, XCMP8, STATUS )
         CALL DAT_THERE( XLOC, XCMP9, EXIST, STATUS )
         IF ( EXIST ) CALL DAT_ERASE( XLOC, XCMP9, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
