      SUBROUTINE SLN1_PUTREC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SLN1_PUTREC

*  Purpose:
*     Write a new selection record to an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN1_PUTREC( NARG, ARGS, OARG, STATUS )

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
*     SLN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sln.html

*  Keywords:
*     package:sln, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Sep 1995 (DJA):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*132		ARDOUT			! ARD text line
      CHARACTER*(DAT__SZLOC)	SELOC			! Sort box selector
      CHARACTER*(DAT__SZLOC)	SLOC			! Sort box
      CHARACTER*20		SNAME			! Selector name
      CHARACTER*(DAT__SZLOC)	SRLOC			! Sort box extension
      CHARACTER*(DAT__SZLOC)	SSLOC			! Sort box cell
      CHARACTER*(DAT__SZLOC)	TCLOC			! Text box cell
      CHARACTER*(DAT__SZLOC)	TLOC			! Text box for ARD
      CHARACTER*20		VARIANT			! Selector variant

      INTEGER			DIM, NDIM		! Sort box dimensions
      INTEGER			GRPID			! GRP identifier
      INTEGER			ITXT			! Loop over ARD text
      INTEGER			ISEL			! Loop over selectors
      INTEGER			NSEL			! # selectors
      INTEGER			SELID			! A single selector
      INTEGER			SID			! Selectors structure
      INTEGER			SIZE			! # text records

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does sort box already exist?
      CALL ADI1_LOCSORT( ARGS(1), .FALSE., SLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get current dimensionality
        CALL DAT_SHAPE( SLOC, 1, DIM, NDIM, STATUS )

*    Old style sort box?
        IF ( NDIM .EQ. 0 ) THEN

*      Extension records exist?
          CALL DAT_THERE( SLOC, 'SRECS', THERE, STATUS )
          IF ( .NOT. THERE ) THEN
            CALL DAT_NEW( SLOC, 'SRECS', 'EXT', 1, 1, STATUS )
          END IF
          CALL DAT_FIND( SLOC, 'SRECS', SRLOC, STATUS )

          DIM = 2
          CALL DAT_CELL( SRLOC, 1, DIM-1, SSLOC, STATUS )
          CALL DAT_ANNUL( SRLOC, STATUS )

        ELSE
          DIM = DIM + 1
          CALL DAT_ALTER( SLOC, 1, DIM, STATUS )
          CALL DAT_CELL( SLOC, 1, DIM, SSLOC, STATUS )

        END IF

*  Otherwise create it
      ELSE
        CALL ERR_ANNUL( STATUS )
        CALL ADI1_LOCSORT( ARGS(1), .TRUE., SLOC, STATUS )
        DIM = 1
        CALL DAT_CELL( SLOC, 1, DIM, SSLOC, STATUS )

      END IF

*  Got a good sort box now?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Write program version
        CALL ADI1_CCA2HC( ARGS(2), 'Version', SSLOC, 'VERSION', STATUS )

*    Locate selectors structure
        CALL ADI_FIND( ARGS(2), 'Selectors', SID, STATUS )

*    Get number of selectors
        CALL ADI_NCMP( SID, NSEL, STATUS )
        DO ISEL = 1, NSEL

*      Locate the selector
          CALL ADI_INDCMP( SID, ISEL, SELID, STATUS )

*      Get its name
          CALL ADI_NAME( SELID, SNAME, STATUS )

*      Create a structure for this selector
          CALL DAT_NEW( SSLOC, SNAME, 'SELECTOR', 0, 0, STATUS )
          CALL DAT_FIND( SSLOC, SNAME, SELOC, STATUS )

*      Write the variant
          CALL ADI_CGET0C( SELID, 'Variant', VARIANT, STATUS )
          CALL ADI1_CCA2HC( SELID, 'Variant', SELOC, 'VARIANT', STATUS )

*     Switch on variant
*       Simple bound pairs
          IF ( VARIANT .EQ. 'RANGE_PAIRS' ) THEN

*        Copy range start and stop values
            CALL ADI1_CCA2HI( SELID, 'START', SELOC, 'START', STATUS )
            CALL ADI1_CCA2HI( SELID, 'STOP', SELOC, 'STOP', STATUS )

*      Area description
          ELSE IF ( VARIANT .EQ. 'AREA_DESCRIPTION' ) THEN

*        Extract group identifier
            CALL ADI_CGET0I( SELID, 'GRPID', GRPID, STATUS )

*        Get number of text records
            CALL GRP_GRPSZ( GRPID, SIZE, STATUS )

*        Create HDS array to hold text
            CALL DAT_NEWC( SELOC, 'TEXT', 80, 1, SIZE, STATUS )
            CALL DAT_FIND( SELOC, 'TEXT', TLOC, STATUS )

*        Write text one line at a time
            DO ITXT = 1, SIZE
              CALL DAT_CELL( TLOC, 1, ITXT, TCLOC, STATUS )
              CALL GRP_GET( GRPID, ITXT, 1, ARDOUT, STATUS )
              CALL DAT_PUT0C( TCLOC, ARDOUT(:CHR_LEN(ARDOUT)), STATUS )
              CALL DAT_ANNUL( TCLOC, STATUS )
            END DO

*        Release text array
            CALL DAT_ANNUL( TLOC, STATUS )

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'V', VARIANT )
            CALL ERR_REP( ' ', 'Unsupported selection record selector'/
     :                    /' variant /^V/', STATUS )
          END IF

*      Release the selector
          CALL DAT_ANNUL( SELOC, STATUS )
          CALL ADI_ERASE( SELID, STATUS )

        END DO

*    Release selectors
        CALL ADI_ERASE( SID, STATUS )

*    Annul the cell
        CALL DAT_ANNUL( SSLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN1_PUTREC', STATUS )

      END
