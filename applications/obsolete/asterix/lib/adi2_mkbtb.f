      SUBROUTINE ADI2_MKBTB( FID, HDU, NROWS, NFLDS, BWIDTH, NAMES,
     :                       TYPES, UNITS, VARIDAT, STATUS )
*+
*  Name:
*     ADI2_MKBTB

*  Purpose:
*     Define the data area of a BINTABLE HDU
*     New version with all ADI2_ calls

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MKBTB( FID, HDU, NROWS, NFLDS, BWIDTH, NAMES, TYPES,
*                       UNITS, VARIDAT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITS file object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU
*     NROWS = INTEGER (given)
*        Number of rows in the table
*     NFLDS = INTEGER (given)
*        Number of fields in the table
*     BWIDTH = INTEGER (given)
*        Width of binary table in bytes
*     NAMES[] = CHARACTER*(*) (given)
*        Names of the table fields
*     TYPES[] = CHARACTER*(*) (given)
*        Types of the table fields
*     UNITS[] = CHARACTER*(*) (given)
*        Units of the table fields
*     VARIDAT = INTEGER (given)
*        Number of variable length data bytes
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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Dec 1996 (RB):
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
      INTEGER			FID			! See above
      CHARACTER*(*)		HDU			!
      INTEGER			NROWS			!
      INTEGER			NFLDS			!
      INTEGER			BWIDTH			!
      CHARACTER*(*)		NAMES(*)		!
      CHARACTER*(*)		TYPES(*)		!
      CHARACTER*(*)		UNITS(*)		!
      INTEGER			VARIDAT			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*40		EXTNAME			! Name of extension
      CHARACTER*7		STR			! Field digit string

      INTEGER			FSTAT			! FITSIO status
      INTEGER			HID			! HDU identifier
      INTEGER			LUN			! Logical unit
      INTEGER			NDIG			! Number of digits
      INTEGER			I
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0

*  Locate the HDU buffer
      CALL ADI2_FNDHDU( FID, HDU, .TRUE., HID, STATUS )

*  Define BINTABLE extension
      CALL ADI_CGET0C( HID, 'Name', EXTNAME, STATUS )
      CALL ADI_CPUT0L( HID, 'IsTable', .TRUE., STATUS )

* Write the table dimension
      CALL  ADI2_PKEY0I( FID, EXTNAME, 'NAXIS', 2,
     :                   '2-dimensional binary table', STATUS )
      CALL  ADI2_PKEY0I( FID, EXTNAME, 'NAXIS1', BWIDTH,
     :                   'width of table in bytes', STATUS )
      CALL  ADI2_PKEY0I( FID, EXTNAME, 'NAXIS2', NROWS,
     :                   'number of rows in table', STATUS )

*  Write the group keywords
      CALL  ADI2_PKEY0I( FID, EXTNAME, 'PCOUNT', 0,
     :                   'size of special data area', STATUS )
      CALL  ADI2_PKEY0I( FID, EXTNAME, 'GCOUNT', 1,
     :                   'one data group (required keyword)', STATUS )

*  Write the field keywords
      CALL ADI2_PKEY0I( FID, EXTNAME, 'TFIELDS', NFLDS,
     :                  'number of fields in each row', STATUS )

      DO I = 1, NFLDS
        CALL CHR_ITOC( I, STR, NDIG )
        IF ( NAMES(I) .GT. ' ' ) THEN
          CALL ADI2_PKEY0C( FID, EXTNAME, 'TTYPE'//STR(:NDIG), NAMES(I),
     :                      'label for field '//STR(:NDIG), STATUS )
        END IF
        CALL ADI2_PKEY0C( FID, EXTNAME, 'TFORM'//STR(:NDIG), TYPES(I),
     :                    'data format of the field:', STATUS )
        IF ( UNITS(I) .GT. ' ' ) THEN
          CALL ADI2_PKEY0C( FID, EXTNAME, 'TUNIT'//STR(:NDIG), UNITS(I),
     :                      'physical unit of the field', STATUS )
        END IF
      END DO

*  Define its size
      FSTAT = 0
      CALL ADI2_GETLUN( FID, LUN, STATUS )
      CALL FTBDEF( LUN, NFLDS, TYPES, VARIDAT, NROWS, FSTAT )
      IF ( FSTAT .NE. 0 ) STATUS = SAI__ERROR

*  Free the buffer
      CALL ADI_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_MKBTB', STATUS )
      END IF

      END
