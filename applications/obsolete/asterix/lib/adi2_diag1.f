      SUBROUTINE ADI2_DIAG1( ID, STATUS )
*+
*  Name:
*     ADI2_DIAG1

*  Purpose:
*     Diagnostic dump of state of ADI FitsFile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DIAG1( ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FitsFile object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Jun 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
c      INCLUDE '{global_constants_file}' ! [global_constants_description]

*  Global Variables:
c      INCLUDE '{global_variables_file}' ! [global_variables_description]
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]

*  Arguments Given:
      INTEGER			ID			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*80		CVAL			!

      INTEGER			HID			! HDU identifier
      INTEGER			IHDU			! Loop over HDUs
      INTEGER			IKEY			! Key index
      INTEGER			IVAL			! HDU name
      INTEGER			KCID			! Key container id
      INTEGER			KID			! Key identifier
      INTEGER			NHDU			! # HDU's in definition
      INTEGER			NKEY			! # keywords in HDU

      LOGICAL			LVAL			! Logical value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of HDU's
      CALL ADI_CGET0I( ID, 'Nhdu', NHDU, STATUS )

*  Get top-level data
      CALL ADI2_GETLUN( ID, IVAL, STATUS )
      CALL MSG_SETI( 'LUN', IVAL )
      CALL MSG_PRNT( '  Logical unit : ^LUN' )
      CALL MSG_SETI( 'NHDU', NHDU )
      CALL MSG_PRNT( '  No of HDUs   : ^NHDU' )

*  Loop over HDUs
      DO IHDU = 1, NHDU

*    Get property defining name of the IHDU'th HDU
        CALL ADI2_LOCIHD( ID, IHDU, HID, STATUS )

*    Locate the HDU
        CALL MSG_PRNT( ' ' )
        CALL ADI_CGET0C( HID, 'Name', CVAL, STATUS )
        CALL MSG_PRNT( '  HDU '//CVAL )
        CALL MSG_PRNT( ' ' )

*    Extension type
        CALL ADI_CGET0C( HID, 'Extension', CVAL, STATUS )
        CALL MSG_PRNT( '    Extension type     : '//CVAL )

*    Get flags
        CALL ADI_CGET0L( HID, 'DefStart', LVAL, STATUS )
        IF ( LVAL ) THEN
          CALL MSG_PRNT( '    Definition started : Y' )
        ELSE
          CALL MSG_PRNT( '    Definition started : N' )
        END IF
        CALL ADI_CGET0L( HID, 'DefEnd', LVAL, STATUS )
        IF ( LVAL ) THEN
          CALL MSG_PRNT( '    Definition ended   : Y' )
        ELSE
          CALL MSG_PRNT( '    Definition ended   : N' )
        END IF
        CALL ADI_CGET0L( HID, 'Created', LVAL, STATUS )
        IF ( LVAL ) THEN
          CALL MSG_PRNT( '    Created on disk    : Y' )
        ELSE
          CALL MSG_PRNT( '    Created on disk    : N' )
        END IF
        CALL MSG_PRNT( ' ' )

*    Locate the keywords structure
        CALL ADI_FIND( HID, 'Keys', KCID, STATUS )

*    How many components?
        CALL ADI_NCMP( KCID, NKEY, STATUS )
        DO IKEY = 1, NKEY
          CALL ADI_INDCMP( KCID, IKEY, KID, STATUS )
          CALL ADI_NAME( KID, CVAL, STATUS )
          CALL MSG_PRNT( '    '//CVAL )
          CALL ADI_ERASE( KID, STATUS )
        END DO

*    Release keywords structure
        CALL ADI_ERASE( KCID, STATUS )

*    Free HDU handle
        CALL ADI_ERASE( HID, STATUS )

      END DO

      CALL MSG_PRNT( ' ' )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_DIAG1', STATUS )

      END
