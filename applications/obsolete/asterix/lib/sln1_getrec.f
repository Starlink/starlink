      SUBROUTINE SLN1_GETREC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SLN1_PUTREC

*  Purpose:
*     Read a selection record from an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN1_GETREC( NARG, ARGS, OARG, STATUS )

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
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*132		ARDIN			! ARD text line
      CHARACTER*80		QNAMS			! Quantities needed
      CHARACTER*(DAT__SZLOC)	SELOC			! Sort box selector
      CHARACTER*(DAT__SZLOC)	SLOC			! Sort box
      CHARACTER*20		SNAME			! Selector name
      CHARACTER*(DAT__SZLOC)	SRLOC			! Sort box extension
      CHARACTER*(DAT__SZLOC)	SSLOC			! Sort box cell
      CHARACTER*(DAT__SZLOC)	TCLOC			! Text box cell
      CHARACTER*(DAT__SZLOC)	TLOC			! Text box for ARD
      CHARACTER*(DAT__SZTYP)	TYPE			! Component type
      CHARACTER*20		VARIANT			! Selector variant

      INTEGER			DIM, NDIM		! Sort box dimensions
      INTEGER			GRPID			! GRP identifier
      INTEGER			ICMP			! Loop over components
      INTEGER			IREC			! Requested record
      INTEGER			ITXT			! Loop over ARD text
      INTEGER			NCMP			! # components
      INTEGER			SELID			! A single selector
      INTEGER			SID			! Selectors structure
      INTEGER			SIZE			! # text records

      LOGICAL			OLD			! Old style SORT?
      LOGICAL			WANTED			! User wants this sel'r
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Quantities to extract
      CALL ADI_GET0C( ARGS(2), QNAMS, STATUS )

*  Which record do we require
      CALL ADI_GET0I( ARGS(3), IREC, STATUS )

*  Does sort box already exist?
      CALL ADI1_LOCSORT( ARGS(1), .FALSE., SLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get current dimensionality
        CALL DAT_SHAPE( SLOC, 1, DIM, NDIM, STATUS )

*    Old style sort box?
        IF ( NDIM .EQ. 0 ) THEN

*      Mark as old
          OLD = .TRUE.

*      Need to look for extension records?
          IF ( IREC .EQ. 1 ) THEN
            CALL DAT_CLONE( SLOC, SSLOC, STATUS )

          ELSE

*         Locate extension records
            CALL DAT_FIND( SLOC, 'SRECS', SRLOC, STATUS )

            CALL DAT_CELL( SRLOC, 1, IREC-1, SSLOC, STATUS )

*         Release extension records
            CALL DAT_ANNUL( SRLOC, STATUS )

          END IF

        ELSE

*      Mark as old
          OLD = .FALSE.

*      Locate the cell
          CALL DAT_CELL( SLOC, 1, IREC, SSLOC, STATUS )

        END IF

*    Create export data structure
        CALL ADI_NEW0( 'SelectionRecord', OARG, STATUS )

*    Locate Selectors structure
        CALL ADI_FIND( OARG, 'Selectors', SID, STATUS )

*    Old style SORT box?
        IF ( OLD ) THEN

*      Didn't have program id
          CALL ADI_CPUT0C( OARG, 'Version', '*unknown*', STATUS )

        ELSE

*      Get creator
          CALL ADI1_CCH2AC( SSLOC, 'VERSION', OARG, 'Version', STATUS )

*      Loop through components, looking for selectors
          CALL DAT_NCOMP( SSLOC, NCMP, STATUS )
          DO ICMP = 1, NCMP

*        Locate this component and check it's the correct type
            CALL DAT_INDEX( SSLOC, ICMP, SELOC, STATUS )
            CALL DAT_TYPE( SELOC, TYPE, STATUS )
            CALL DAT_NAME( SELOC, SNAME, STATUS )
            IF ( TYPE .EQ. 'SELECTOR' ) THEN

*          Does user want this selector
              IF ( QNAMS(1:1) .EQ. '*' ) THEN
                WANTED = .TRUE.
              ELSE
                WANTED = CHR_INSET( QNAMS, SNAME )
              END IF
              IF ( WANTED ) THEN

*            Create new selector structure for export
                CALL ADI_NEW0( 'STRUC', SELID, STATUS )

*            Get the selector variant
                CALL ADI1_CCH2AC( SELOC, 'VARIANT', SELID, 'Variant',
     :                            STATUS )
                CALL ADI_CGET0C( SELID, 'Variant', VARIANT, STATUS )

*            Switch on variant
                IF ( VARIANT .EQ. 'RANGE_PAIRS' ) THEN
                  CALL ADI1_CCH2AT( SELOC, 'START', SELID, 'START',
     :                              STATUS )
                  CALL ADI1_CCH2AT( SELOC, 'STOP', SELID, 'STOP',
     :                              STATUS )

                ELSE IF ( VARIANT .EQ. 'AREA_DESCRIPTION' ) THEN

*              Create new group id
                  CALL GRP_NEW( 'ARD selection', GRPID, STATUS )

*              Locate text array
                  CALL DAT_FIND( SELOC, 'TEXT', TLOC, STATUS )
                  CALL DAT_SHAPE( TLOC, 1, SIZE, NDIM, STATUS )
                  DO ITXT = 1, SIZE
                    CALL DAT_CELL( TLOC, 1, ITXT, TCLOC, STATUS )
                    CALL DAT_GET0C( TCLOC, ARDIN, STATUS )
                    CALL GRP_PUT( GRPID, 1, ARDIN, 0, STATUS )
                    CALL DAT_ANNUL( TCLOC, STATUS )
                  END DO
                  CALL DAT_ANNUL( TLOC, STATUS )

*              Write group id
                  CALL ADI_CPUT0I( SELID, 'GRPID', GRPID, STATUS )

                ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'V', VARIANT )
                  CALL ERR_REP( ' ', 'Unsupported selection record '/
     :                            /' selector variant /^V/', STATUS )
                END IF

*            Write the selector
                CALL ADI_CPUTID( SID, SNAME, SELID, STATUS )

*          End of selector wanted test
              END IF

            END IF
            CALL DAT_ANNUL( SELOC, STATUS )

          END DO

        END IF

*    Release selectors structure
        CALL ADI_ERASE( SID, STATUS )

*    Annul the cell
        CALL DAT_ANNUL( SSLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN1_GETREC', STATUS )

      END
