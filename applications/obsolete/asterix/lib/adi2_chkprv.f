      SUBROUTINE ADI2_CHKPRV( FID, CHDU, WRKEYS, STATUS )
*+
*  Name:
*     ADI2_CHKPRV

*  Purpose:
*     Define data areas of HDUs up the point being worked on

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CHKPRV( FID, CHDU, WRKEYS, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     CHDU = INTEGER (given)
*        The HDU being worked on
*     WRKEYS = LOGICAL (given)
*        Commit keywords to file?
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
*     2 Feb 1995 (DJA):
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
      INTEGER			FID			! FITSfile identifier
      INTEGER			CHDU			! Current HDU
      LOGICAL			WRKEYS			! Write keywords?

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8		HDU			! HDU name
      CHARACTER*20		HDUTYPE			! HDU type
      CHARACTER*6		STR			!

      INTEGER			FSTAT			! FITSIO status
      INTEGER			IHDU			! Loop over HDUs
      INTEGER			LUN			! Logical unit
      INTEGER			NHDU			! HDU count
      INTEGER			NKEY			! Keyword count
      INTEGER			OHID			! Old HDU object

      LOGICAL			CREATED			! HDU already created?
      LOGICAL			DEFBEG			! Definition started?
      LOGICAL			DEFEND			! Definition ended?
      LOGICAL			MOVED			! Moved to HDU yet?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0

*  Extract logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Loop over all previous HDU's, ensuring they have defined data areas
      DO IHDU = 1, NHDU

*    Initialise for this HDU
        FSTAT = 0
        MOVED = .FALSE.

*    Locate an HDU by its consecutive number
        IF ( IHDU .EQ. 1 ) THEN
          HDU = ' '
        ELSE
          WRITE( STR, '(A,I1.1)' ) '.HDU', IHDU
          CALL ADI_CGET0C( FID, STR, HDU, STATUS )
        END IF
        CALL ADI2_LOCHDU( FID, HDU, OHID, STATUS )

*    Is the definition incomplete?
        CALL ADI_CGET0L( OHID, '.DEF_END', DEFEND, STATUS )
        IF ( .NOT. DEFEND ) THEN

*      The definition may be unfinished, or it may not even be started
          CALL ADI_CGET0L( OHID, '.DEF_START', DEFBEG, STATUS )
          IF ( DEFBEG ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'HDU', IHDU )
            CALL ERR_REP( ' ', 'Error creating HDU, previous HDU '/
     :       /'number ^HDU data area length is undefined', STATUS )

          ELSE

*        Has the HDU even been created?
            CALL ADI_CGET0L( OHID, '.CREATED', CREATED, STATUS )

*        Create it if it doesn't exist
            IF ( .NOT. CREATED ) THEN
              IF ( IHDU .GT. 1 ) THEN
                CALL FTCRHD( LUN, FSTAT )
              END IF
              CALL FTMAHD( LUN, IHDU, HDUTYPE, FSTAT )
              MOVED = .TRUE.
              CALL ADI_CPUT0L( OHID, '.CREATED', .TRUE., STATUS )

*          Reserve some keyword space too
              CALL ADI_NCMP( OHID, NKEY, STATUS )
              IF ( NKEY .GT. 0 ) THEN
                CALL FTHDEF( LUN, NKEY, STATUS )
              END IF

            END IF

*        There is no definition, however, so create a default one
            CALL FTPHPR( LUN, .TRUE., 8, 0, 0, 0, 1, .TRUE., FSTAT )

*        Mark as defined
            CALL ADI_CPUT0L( OHID, '.DEF_END', .TRUE., STATUS )

          END IF

        END IF

*    Still ok?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Write keywords?
          IF ( WRKEYS ) THEN

*        Move to HDU unless already there
            IF ( .NOT. MOVED ) THEN
              CALL FTMAHD( LUN, IHDU, HDUTYPE, FSTAT )
            END IF

*        Write the keywords
            CALL ADI2_FCOMIT_HDU( FID, OHID, STATUS )

          END IF

        END IF

*    Release old HDU
        CALL ADI_ERASE( OHID, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_CHKPRV', STATUS )

      END
