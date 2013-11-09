      SUBROUTINE BDI2_AXWB( MODID, FILID, PSID, STATUS )
*+
*  Name:
*     BDI2_AXWB

*  Purpose:
*     Write back an axis array to an FITS file using BinDS model

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_AXWB( MODID, FILID, PSID, STATUS )

*  Description:
*     Writes axis arrays to a FITS file cache. These are written to
*     keywords in the primary header. If the axis centres are regular
*     then the conventional CRPIX, CDELT are used. If the
*     widths are uniform then CDELT is used. If things are neither
*     regular nor uniform, or if lower and upper widths are given,
*     then the values are written to individual keywords. Its a mess
*     but it works.

*  Arguments:
*     MODID = INTEGER (given)
*        ADI identifier to top level model object
*     FILID = INTEGER (given)
*        The ADI identifier of the FITS file object
*     PSID = INTEGER (given)
*        ADI identifier to private storage
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
*     This routine coerces to the simple array representations, but there
*     should be some mechanism for handling magic values and writing the
*     appropriate flags.

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
*     9 Aug 1995 (DJA):
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

*  Arguments Given:
      INTEGER                   MODID,FILID,PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8		AXKEY			! Axis value keyword
      CHARACTER*1		AXS			! Axis data type
      CHARACTER*1		CAX			! Axis number character
      CHARACTER*4		CBIN			! Bin centres
      CHARACTER*40		CMNT			! Keyword comment
      CHARACTER*20		ITEM			! Item name
      CHARACTER*8		TYPE			! Data type

      DOUBLE PRECISION		DVAL			! DP axis value

      REAL			BASE, SCALE		! Axis base & scale
      REAL			RVAL			! SP axis value
      REAL			UVALUE			! Uniform value

      INTEGER			IAX			! Axis number
      INTEGER			IBIN			! Loop over axis bins
      INTEGER			NDIG			! Digits used in CBIN
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Model shape
      INTEGER			NELM			! # data elements
      INTEGER			PHDU			! Primary HDU id
      INTEGER			PTR			! Item data address

      LOGICAL			REG			! Values regular?
      LOGICAL			UNIF			! Values the same?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Which item are we writing back
      CALL ADI_NAME( PSID, ITEM, STATUS )

*  Extract information required to free cache object
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', NELM, STATUS )

*  Are we writing the axis data? If so, then we check for regularity
      REG = .FALSE.
      UNIF = .FALSE.
      IF ( ITEM(8:) .EQ. 'Data' ) THEN

*    Only bother to check for real axes
        IF ( TYPE .EQ. 'REAL' ) THEN
          CALL ARR_CHKREG( %VAL(PTR), NELM, REG, BASE, SCALE, STATUS )
        END IF

*  Other items are checked for uniformity
      ELSE

*    Only bother to check for real arrays
        IF ( TYPE .EQ. 'REAL' ) THEN
          CALL ARR_CHKUNIF( %VAL(PTR), NELM, UNIF, UVALUE, STATUS )
        END IF

      END IF

*  What kind of axis stuff is this?
      CAX = ITEM(6:6)
      CALL CHR_CTOI( CAX, IAX, STATUS )
      IF ( ITEM(8:) .EQ. 'Data' ) THEN
        AXS = 'C'
      ELSE
        AXS = ITEM(8:8)
      END IF

*  Values are regular?
      IF ( REG ) THEN
        CALL ADI2_FPKYR( FILID, ' ', 'CRPIX'//CAX, 1.0,
     :                   'Axis '//CAX//' reference pixel', STATUS )
        CALL ADI2_FPKYR( FILID, ' ', 'CRVAL'//CAX, BASE,
     :                   'Axis '//CAX//' value at reference pixel',
     :                   STATUS )
        CALL ADI2_FPKYR( FILID, ' ', 'CDELT'//CAX, SCALE,
     :                   'Axis '//CAX//' increment', STATUS )

*  Values are uniform?
      ELSE IF ( UNIF ) THEN
        IF ( AXS .EQ. 'W' ) THEN
          CALL ADI2_FPKYR( FILID, ' ', 'AX'//CAX//AXS, UVALUE,
     :                     'Axis '//CAX//' bin width', STATUS )
        ELSE IF ( AXS .EQ. 'L' ) THEN
          CALL ADI2_FPKYR( FILID, ' ', 'AX'//CAX//AXS, UVALUE,
     :                     'Axis '//CAX//' lower bin width', STATUS )
        ELSE IF ( AXS .EQ. 'H' ) THEN
          CALL ADI2_FPKYR( FILID, ' ', 'AX'//CAX//AXS, UVALUE,
     :                     'Axis '//CAX//' upper bin width', STATUS )
        END IF

*  Values are different. Write to individual keywords
      ELSE

*    Extract dimensions of model
        CALL BDI_GETSHP( MODID, ADI__MXDIM, DIMS, NDIM, STATUS )

*    Locate the primary HDU
        CALL ADI2_FNDHDU( FILID, ' ', .TRUE., PHDU, STATUS )

*    Write keyword for each element of array
        DO IBIN = 1, MIN( 9999, DIMS(IAX) )

*      Construct the axis key
          WRITE( AXKEY, '(A,I4.4)' ) 'AX'//CAX//AXS, IBIN
          CALL CHR_ITOC( IBIN, CBIN, NDIG )

*      Make up the comment
          IF ( AXS .EQ. 'C' ) THEN
            CMNT = 'Axis '//CAX//', bin centre '//CBIN
          ELSE IF ( AXS .EQ. 'W' ) THEN
            CMNT = 'Axis '//CAX//', bin width '//CBIN
          ELSE IF ( AXS .EQ. 'L' ) THEN
            CMNT = 'Axis '//CAX//', bin lower width '//CBIN
          ELSE IF ( AXS .EQ. 'H' ) THEN
            CMNT = 'Axis '//CAX//', bin upper width '//CBIN
          END IF

*      Extract axis value and write it to the HDU
          IF ( TYPE .EQ. 'REAL' ) THEN
            CALL ARR_ELEM1R( PTR, NELM, IBIN, RVAL, STATUS )
            CALL ADI2_HPKYR( PHDU, AXKEY, RVAL, CMNT, STATUS )
          ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
            CALL ARR_ELEM1D( PTR, NELM, IBIN, DVAL, STATUS )
            CALL ADI2_HPKYD( PHDU, AXKEY, DVAL, CMNT, STATUS )
          END IF

        END DO

*    Release the HDU
        CALL ADI_ERASE( PHDU, STATUS )

      END IF

*  Unmap the data
c      CALL ADI_CGETREF( PSID, 'InvObj', ITID, STATUS )
c      CALL ADI_UNMAP( ITID, PTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'BDI2_AXWB', STATUS )
      END IF

      END
