      SUBROUTINE ADI2_MAPIMG( HDUID, TYPE, MODE, PSID, PTR, STATUS )
*+
*  Name:
*     ADI2_MAPIMG

*  Purpose:
*     Map image data from primary HDU or IMAGE extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MAPIMG( HDUID, TYPE, MODE, PSID, PTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     TYPE = CHARACTER*(*) (given)
*        Data type for mapping
*     MODE = CHARACTER*(*) (given)
*        Access mode, READ, WRITE or UPDATE
*     PSID = INTEGER (given)
*        ADI identifier of private storage area. Mapping details are
*        written here
*     PTR = INTEGER (returned)
*        The mapped data area
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
*     {enter_new_authors_here}

*  History:
*     26 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER			HDUID, PSID
      CHARACTER*(*)		TYPE, MODE

*  Arguments Returned:
      INTEGER			PTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ADIMS(ADI__MXDIM)	! Actual dimensions
      INTEGER			ANDIM			! Actual dimensionality
      INTEGER			FPIX(ADI__MXDIM)	! 1st pix to extract
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			I			! Loop over dims
      INTEGER			LPIX(ADI__MXDIM)	! Last pix to extract
      INTEGER			LUN			! Logical unit of file
      INTEGER			NELM			! # elements

      LOGICAL			ANYF			! Any diff values?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the logical unit
      CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*  Get dimensions of extension
      CALL ADI2_ISHAPE( HDUID, ADI__MXDIM, ADIMS, ANDIM, STATUS )

*  Allocate memory for mapping
      CALL DYN_MAPT( ANDIM, ADIMS, TYPE, PTR, STATUS )

*  If ok, and dimensionalities match
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Bounds to extract
        DO I = 1, ANDIM
          FPIX(I) = 1
          LPIX(I) = ADIMS(I)
        END DO

*    Number of elements
        CALL ARR_SUMDIM( ANDIM, ADIMS, NELM )

*    Read the data
        FSTAT = 0
        IF ( TYPE .EQ. 'BYTE' ) THEN
          CALL FTGSVB( LUN, 1, ANDIM, ADIMS, FPIX, LPIX, FPIX,
     :                 VAL__BADB, %VAL(PTR), ANYF, FSTAT )

        ELSE IF ( TYPE .EQ. 'WORD' ) THEN
          CALL FTGSVI( LUN, 1, ANDIM, ADIMS, FPIX, LPIX, FPIX,
     :                 VAL__BADW, %VAL(PTR), ANYF, FSTAT )

        ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
          CALL FTGSVJ( LUN, 1, ANDIM, ADIMS, FPIX, LPIX, FPIX,
     :                 VAL__BADI, %VAL(PTR), ANYF, FSTAT )

        ELSE IF ( TYPE .EQ. 'REAL' ) THEN
c          CALL FTGSVE( LUN, 1, ANDIM, ADIMS, FPIX, LPIX, FPIX,
c     :                 VAL__BADR, %VAL(PTR), ANYF, FSTAT )
          CALL FTGPVE( LUN, 1, 1, NELM, VAL__BADR, %VAL(PTR),
     :                 ANYF, FSTAT )

        ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
          CALL FTGSVD( LUN, 1, ANDIM, ADIMS, FPIX, LPIX, FPIX,
     :                 VAL__BADD, %VAL(PTR), ANYF, FSTAT )

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'T', TYPE )
          CALL ERR_REP( 'ADI2_MAPIMG_1', 'Illegal mapping type ^T,'/
     :                  /' type is not supported in FITS', STATUS )
          GOTO 99
        END IF

        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )
        END IF

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_MAPIMG', STATUS )
      END IF

      END
