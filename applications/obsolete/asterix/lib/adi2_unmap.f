      SUBROUTINE ADI2_UNMAP( FID, PSID, APTR, STATUS )
*+
*  Name:
*     ADI2_UNMAP

*  Purpose:
*     Unmap a FITSfile based item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_UNMAP( FID, PSID, APTR, STATUS )

*  Description:
*     Unmaps an item in a FITS file. All mapping is actually dynamic using
*     DYN or a mapping of an ADI data object, so we must write back the
*     data to file on unmapping.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     PSID = INTEGER (given)
*        Private storage item containing mapping details
*     APTR = INTEGER (given)
*        The particular mapping to unmap. Zero means all of them
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
*        Original version.
*     10 Apr 1997 (RB):
*        Alter to reflect changes in STOMAP
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			FID, PSID, APTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*3		FORM			! Object form
      CHARACTER*6		MODE			! Access mode
      CHARACTER*8		TYPE			! Mapping type

      INTEGER			BCOL			! Column number
      INTEGER			FROW			! First row used
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			HDUID			! HDU containing data
      INTEGER			KID			! Keyword identifier
      INTEGER			LUN			! Logical i/o unit
      INTEGER			NELM			! # data elements
      INTEGER			PTR			! Mapped address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the kind of object mapped
      CALL ADI_CGET0C( PSID, 'MapSystem', FORM, STATUS )

*  Extract pointer and mode
      CALL ADI_CGET0C( PSID, 'Mode', MODE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )

*  Switch on the mapping form. Binary column?
      IF ( FORM .EQ. 'BC' ) THEN

*    Write column data back to file
        IF ( MODE .NE. 'READ' ) THEN

*      Get HDU containing column
          CALL ADI_CGETREF( PSID, 'Hdu', HDUID, STATUS )

*      Get logical unit
          CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*      Extract type and position in column which was mapped
          CALL ADI_CGET0I( PSID, 'Column', BCOL, STATUS )
          CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
          CALL ADI_CGET0I( PSID, 'Frow', FROW, STATUS )
          CALL ADI_CGET0I( PSID, 'Nelm', NELM, STATUS )

*      Write back data
          FSTAT = 0
          IF ( TYPE .EQ. 'BYTE' ) THEN
            CALL FTPCLB( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          ELSE IF ( TYPE .EQ. 'WORD' ) THEN
            CALL FTPCLI( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
            CALL FTPCLJ( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          ELSE IF ( TYPE .EQ. 'REAL' ) THEN
            CALL FTPCLE( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
            CALL FTPCLD( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
            CALL FTPCLL( LUN, BCOL, FROW, 1, NELM, %VAL(PTR), FSTAT )

          END IF

        END IF

*    Release the mapped data
        CALL DYN_UNMAP( PTR, STATUS )

*  Mapped image data
      ELSE IF ( FORM .EQ. 'I' ) THEN

*  Mapped keyword, or keyword comment
      ELSE IF ( FORM .EQ. 'K' ) THEN

*    Locate the scalar item
        CALL ADI_CGETREF( PSID, 'Key', KID, STATUS )

*    Map the ADI value of the object
        CALL ADI_UNMAP( KID, PTR, STATUS )

*  Miscellaneous mapped data
      ELSE IF ( FORM .EQ. 'X' ) THEN

*    Release the mapped data
        CALL DYN_UNMAP( PTR, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_UNMAP', STATUS )
      END IF

      END
