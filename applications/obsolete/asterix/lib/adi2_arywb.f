      sUBROUTINE ADI2_ARYWB( MODID, FILID, PSID, STATUS )
*+
*  Name:
*     ADI2_ARYWB

*  Purpose:
*     Write back an array to an FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ARYWB( MODID, FILID, PSID, STATUS )

*  Description:
*     This routine is called before the association between a bit of memory
*     and an FITS object is destroyed.

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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
*        Original version.
*     15 May 1997 (RB):
*        Actually write some code!
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
      CHARACTER*20		TYPE

      INTEGER			CACHEID			! Cache object
      INTEGER			PTR, PTR2		! Item data address
      INTEGER			NDIM, DIMS(ADI__MXDIM)
      INTEGER			NELM

      LOGICAL			MODIFIED, THERE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract information required to free cache object
      CALL ADI_CGET0I( PSID, 'CacheID', CACHEID, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )

*  Is the object an array?
      CALL ADI_THERE( CACHEID, 'SHAPE', THERE, STATUS )
      IF ( .NOT. THERE ) RETURN

*  Copy the data back into the cache
      CALL ADI_CGET0L( CACHEID, 'Modified', MODIFIED, STATUS )
      IF ( MODIFIED ) THEN

*    If the PSID "Ptr" and the address of the CACHEID "Value" are the same
*    then there is no need to copy the data
        CALL ADI_CGET0C( CACHEID, 'TYPE', TYPE, STATUS )
        CALL ADI_CMAP( CACHEID, 'Value', TYPE, 'READ', PTR2, STATUS )
        IF ( PTR .EQ. PTR2 ) THEN
          GOTO 99
        END IF

*    Find out the new shape and type
        CALL ADI_CGET1I( PSID, 'SHAPE', ADI__MXDIM, DIMS, NDIM,
     :                    STATUS )
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
        CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )

*    Destroy the old value, create a new holder and assign the pointer
        CALL ADI_CERASE( CACHEID, 'Value', STATUS )
        CALL ADI_CNEW( CACHEID, 'Value', TYPE, NDIM, DIMS, STATUS )
        CALL ADI_CMAP( CACHEID, 'Value', TYPE, 'WRITE', PTR2, STATUS )

*    Copy the data (but what if they are different types?)
        IF (TYPE .EQ. 'LOGICAL' ) THEN
          CALL ARR_COP1L( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE IF (TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
          CALL ARR_COP1B( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE IF (TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
          CALL ARR_COP1W( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE IF (TYPE .EQ. 'INTEGER' ) THEN
          CALL ARR_COP1I( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE IF (TYPE .EQ. 'REAL' ) THEN
          CALL ARR_COP1R( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE IF (TYPE .EQ. 'DOUBLE' ) THEN
          CALL ARR_COP1D( NELM, %VAL(PTR), %VAL(PTR2), STATUS )
        ELSE
          print*, '*** Warning (ARYWB): Cannot copy data type ', TYPE
        END IF

      END IF

*  Unmap the data
 99   CALL ADI_CUNMAP( CACHEID, 'Value', PTR2, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_ARYWB', STATUS )
      END IF

      END
