      SUBROUTINE ADI2_DMAP( MODID, CACHEID, TYPE, MODE, ENDIM, EDIMS,
     :                      PSID, PTR, NELM, STATUS )
*+
*  Name:
*     ADI2_DMAP

*  Purpose:
*     Map the specified FITS data item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DMAP( MODID, CACHEID, TYPE, MODE, ENDIM, EDIMS, PSID,
*                     PTR, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     MODID = INTEGER (given)
*        Abstract data model
*     CACHEID = INTEGER (given)
*        FITS data object to map
*     TYPE = CHARACTER*(*) (given)
*        Data type in which to map
*     MODE = CHARACTER*(*) (given)
*        Mapping mode
*     ENDIM = INTEGER (given)
*        Expect dimensionality
*     EDIMS[] = INTEGER (given)
*        Expected dimensions
*     PTR = INTEGER (returned)
*        Mapped data pointer
*     NELM = INTEGER (returned)
*        Number of mapped elements
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
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jun 1996 (DJA):
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
      INTEGER			MODID, CACHEID, ENDIM, EDIMS(*), PSID
      CHARACTER*(*)		TYPE, MODE

*  Arguments Returned:
      INTEGER			PTR, NELM

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC
      EXTERNAL			ADI2_ARYWB

*  Local Variables:
      CHARACTER*8		ATYPE			! Actual data type
      CHARACTER*20		ITEM			! Item name

      INTEGER			ENELM			! Expected # elements
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Actual dimensions
      INTEGER			PARENT			! Parent of cache obj

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Expected number of data elements
      CALL ARR_SUMDIM( ENDIM, EDIMS, ENELM )

*  Get array shape and total number of elements
      CALL ADI_CGET0C( CACHEID, 'TYPE', ATYPE, STATUS )
      CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, DIMS, NDIM,
     :                 STATUS )
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  If number of elements differ we report an error
      IF ( ENELM .NE. NELM ) THEN
        CALL ADI_NAME( PSID, ITEM, STATUS )
        CALL MSG_SETC( 'IT', ITEM )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'The dimensions of item ^IT '/
     :           /'differ from those expected - check the program '/
     :                /'which created this file', STATUS )
        GOTO 99
      END IF

*  FITS mapping always works by creating the data array in the cache
*  object. Map that data if it exists, otherwise read it from the file
      CALL ADI_THERE( CACHEID, 'Value', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CMAP( CACHEID, 'Value', TYPE, MODE, PTR, STATUS )
      ELSE
        CALL ADI2_DCOP_IN( CACHEID, PTR, NELM, STATUS )
      END IF

*  Store mapping details
      CALL ADI2_STOMAP( PSID, CACHEID, 'dyn', 0, PTR, ENDIM, EDIMS, 0,
     :                  0, UTIL_PLOC(ADI2_ARYWB), TYPE, MODE, STATUS )

*  Unless the the mode is read the data is potentially modified
      IF ( MODE(1:1) .NE. 'R' ) THEN
        CALL ADI_CPUT0L( CACHEID, 'Modified', .TRUE., STATUS )
        CALL ADI_CGET0I( CACHEID, 'Parent', PARENT, STATUS )
        CALL ADI_CPUT0L( PARENT, 'Modified', .TRUE., STATUS )
      END IF

*  Always return expected number of elements
      NELM = ENELM

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DMAP', STATUS )
      END IF

      END
