      SUBROUTINE ADI2_DEFIMG( HDUID, TYPE, NDIM, DIMS, WKEY, IMID,
     :                        STATUS )
*+
*  Name:
*     ADI2_DEFIMG

*  Purpose:
*     Define IMAGE extension, optionally writing keywords

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DEFIMG( HDUID, TYPE, NDIM, DIMS, WKEY, STATUS )

*  Description:

*  Arguments:
*     HDUID = INTEGER (given)
*        The FITS object containing the component we're interested in
*     TYPE = CHARACTER*(*) (given)
*        The image extension data type
*     NDIM = INTEGER (given)
*        The dimensionality of the data
*     DIMS[] = INTEGER (given)
*        The dimensions of the data
*     WKEY = LOGICAL (returned)
*        Write keywords?
*     IMID = INTEGER (returned)
*        Image cache object
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
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			HDUID, NDIM, DIMS(*)
      CHARACTER*(*)		TYPE
      LOGICAL			WKEY

*  Arguments Returned:
      INTEGER			IMID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		AXKEY			! Axis size keyword

      INTEGER			BITPIX			! Bits per pixel
      INTEGER			IAX			! Loop over dimensions
      INTEGER			IHDU			! HDU number
      INTEGER			IMID			! Image identifier

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make the cache object
      CALL ADI_THERE( HDUID, 'Image', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL ADI_CNEW0( HDUID, 'Image', 'FITSimgCache', STATUS )
      END IF
      CALL ADI_FIND( HDUID, 'Image', IMID, STATUS )

*  If already there then scrub the existing stuff
      IF ( THERE ) THEN
        CALL ADI_CERASE( IMID, 'TYPE', STATUS )
        CALL ADI_CERASE( IMID, 'SHAPE', STATUS )
        CALL ADI_THERE( IMID, 'Value', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CERASE( IMID, 'Value', STATUS )
        END IF
      END IF

*  Write dimensions and type
      CALL ADI_CPUT0C( IMID, 'TYPE', TYPE, STATUS )
      IF ( NDIM .GT. 0 ) THEN
        CALL ADI_CPUT1I( IMID, 'SHAPE', NDIM, DIMS, STATUS )
      ELSE
        CALL ADI_CPUT0I( IMID, 'SHAPE', 0, STATUS )
      END IF

*  Extract and write HDU number
      CALL ADI_CGET0I( HDUID, 'Number', IHDU, STATUS )
      CALL ADI_CPUT0I( IMID, 'Hdu', IHDU, STATUS )
      CALL ADI_CPUT0I( IMID, 'Parent', HDUID, STATUS )

*  Definitely not a table extension
      CALL ADI_CPUT0L( HDUID, 'IsTable', .FALSE., STATUS )

*  Write keywords?
      IF ( WKEY ) THEN

*    Choose BITPIX based on TYPE
        IF ( TYPE .EQ. 'DOUBLE' ) THEN
          BITPIX = -64
        ELSE IF ( TYPE .EQ. 'REAL' ) THEN
          BITPIX = -32
        ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
          BITPIX = 32
        ELSE IF ( TYPE .EQ. 'WORD' ) THEN
          BITPIX = 16
        ELSE IF ( TYPE .EQ. 'BYTE' ) THEN
          BITPIX = 8
        END IF
        CALL ADI2_HPKYI( HDUID, 'BITPIX', BITPIX, '~', STATUS )

*    Write dimensions keywords
        CALL ADI2_HPKYI( HDUID, 'NAXIS', NDIM, '~', STATUS )
        DO IAX = 1, NDIM
          WRITE( AXKEY, '(A,I1.1)' ) 'NAXIS', IAX
          CALL ADI2_HPKYI( HDUID, AXKEY, DIMS(IAX), '~', STATUS )
        END DO

*    Create data
        CALL ADI_CNEW( IMID, 'Value', TYPE, NDIM, DIMS, STATUS )

      END IF

*  Abort point
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DEFIMG', STATUS )
      END IF

      END
