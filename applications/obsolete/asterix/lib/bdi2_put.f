      SUBROUTINE BDI2_PUT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_PUT

*  Purpose:
*     Write object to ASTERIX style binned FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_PUT( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Jun 1996 (DJA):
*        Original version.
*     11 Feb 1997 (RB):
*        Cope with SpacedData arrays
*     18 May 1997 (RB):
*        Cope with 'Axis_n' multiple put.
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20	     	ITEM			! Item name
      CHARACTER*20	     	TYPE			! Item data type

      INTEGER			BPI			! Bytes per item
      INTEGER			CNDIM,CDIMS(ADI__MXDIM)	! Object dimensions
      INTEGER			IPTR			! Input data to write
      INTEGER			ITID			! Item cache object
      INTEGER			NELM			! Total # elements
      INTEGER			PSID			! Item private store
      INTEGER			PTR			! Item data
      INTEGER			WBPTR			! Write back proc

      CHARACTER*1		CAX, AXIS
      LOGICAL			DIDCRE
      REAL			AXINFO(2), BASE, DELTA, TANG
      INTEGER			IDUM, I

      CHARACTER*11		AXITEM(7)

*  Local data:
      DATA AXITEM		/'_Label', '_Units', '_Normalised', '_Data',
     :                           '_Width', '_LoWidth', '_HiWidth'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_TYPE( ARGS(4), TYPE, STATUS )

*  Special case for coping with spaced data axis array (RB)
      IF ( ITEM(1:5) .EQ. 'Axis_' .AND. ITEM(7:11) .EQ. '_Data' ) THEN
        CALL ADI_GET1R( ARGS(4), 2, AXINFO, IDUM, STATUS )
        BASE = AXINFO(1)
        DELTA = AXINFO(2) - AXINFO(1)
        TANG = 1.0 - ( BASE / DELTA )
        CAX = ITEM(6:6)
        IF ( CAX .EQ. '1' ) THEN
          AXIS = 'X'
        ELSE IF ( CAX .EQ. '2' ) THEN
          AXIS = 'Y'
        ELSE
          AXIS = ' '
        END IF
        CALL ADI2_CFIND( ARGS(2), ' ', '.CRPIX'//CAX, ' ', .TRUE.,
     :                   .FALSE., TYPE, 0, 0, DIDCRE, ITID, STATUS )
        CALL ADI_CNEWV0R( ITID, 'Value', TANG, STATUS )
        CALL ADI_CNEWV0C( ITID, 'Comment', AXIS//
     :                    ' pixel of tangent point', STATUS )
        CALL ADI2_CFIND( ARGS(2), ' ', '.CDELT'//CAX, ' ', .TRUE.,
     :                   .FALSE., TYPE, 0, 0, DIDCRE, ITID, STATUS )
        CALL ADI_CNEWV0R( ITID, 'Value', DELTA, STATUS )
        CALL ADI_CNEWV0C( ITID, 'Comment', AXIS//' degrees per pixel',
     :                    STATUS )

*  Special case for Axis_<n> item, only copy, don't invent (RB)
      ELSE IF (ITEM(1:5) .EQ. 'Axis_' .AND. ITEM(7:).LE.' ' ) THEN
        DO I = 1, 7
          CALL BDI2_CFIND( ARGS(1), ARGS(2), ITEM(1:6)//AXITEM(I),
     :                     .TRUE., .FALSE., ITID, CNDIM, CDIMS,
     :                     STATUS )
          print*
          print*, i, ' ', axitem(i)
          call adi_print(itid, status)
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ADI2_DCOP( ARGS(4), ITID, STATUS )
            CALL ADI_ERASE( ITID, STATUS )
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF
        END DO

*  All the other items as standard...
      ELSE

*  Locate the data item
      CALL BDI2_CFIND( ARGS(1), ARGS(2), ITEM, .TRUE., .FALSE.,
     :                 ITID, CNDIM, CDIMS, STATUS )

*  Located ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Copy the data
        CALL ADI2_DCOP( ARGS(4), ITID, STATUS )

*    Release the item
        CALL ADI_ERASE( ITID, STATUS )

*  Try inventing object
      ELSE

*    Scrub status
        CALL ERR_ANNUL( STATUS )

*    Try to invent the object
        CALL BDI2_INVNT( ARGS(1), ARGS(2), ITEM, TYPE, 'WRITE',
     :                   ITID, CNDIM, CDIMS, WBPTR, STATUS )

*    Successful?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Store the object as a component of the BinDS object
          CALL BDI0_STOINV( ARGS(1), ITEM, ITID, STATUS )

*      Locate the private storage for the item, creating if required
          CALL ADI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*      Map the invented object
          CALL ADI_MAP( ITID, TYPE, 'WRITE', PTR, STATUS )

*      Store mapping details
          CALL ADI2_STOMAP( PSID, ADI__NULLID, 'inv', ITID, PTR,
     :                      CNDIM, CDIMS, 0, 0, WBPTR, TYPE,
     :                      'WRITE', STATUS )

*      Bytes per item
          IF ( TYPE .EQ. 'DOUBLE' ) THEN
            BPI = VAL__NBD
          ELSE IF ( TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
            BPI = VAL__NBW
          ELSE IF ( TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
            BPI = VAL__NBB
          ELSE
            BPI = VAL__NBI
          END IF

*      Copy data
          CALL ADI_MAP( ARGS(4), TYPE, 'READ', IPTR, STATUS )
          CALL ARR_SUMDIM( CNDIM, CDIMS, NELM )
          CALL ARR_COP1B( NELM*BPI, %VAL(IPTR), %VAL(PTR), STATUS )
          CALL ADI_UNMAP( ARGS(4), IPTR, STATUS )

*      Release storage
          CALL ADI0_UNMAP( ARGS(1), ARGS(2), PSID, STATUS )

        END IF

      END IF
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_PUT', STATUS )

      END
