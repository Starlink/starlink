      SUBROUTINE UDI1_COPANC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     UDI1_COPANC

*  Purpose:
*     Copy ancillary data for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI1_COPANC( NARG, ARGS, OARG, STATUS )

*  Description:
*     Copies ancillary data for HDS files. Recognises the omission of
*     GRAFIX and GROUPING components.

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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ACLOC			! Component of ASTERIX
      CHARACTER*(DAT__SZNAM)	ANAME			! ASTERIX cmp name
      CHARACTER*(DAT__SZLOC)	CLOC			! Component of MORE
      CHARACTER*(DAT__SZLOC)	IMLOC			! Input MORE box
      CHARACTER*(DAT__SZNAM)	NAME			! MORE cmp name
      CHARACTER*(DAT__SZLOC)	OALOC			! Output ASTERIX box
      CHARACTER*40		OMIT			! Omission string
      CHARACTER*(DAT__SZLOC)	OMLOC			! Output MORE box

      INTEGER			IACOMP			! ASTERIX component loop
      INTEGER			ICOMP			! Component loop
      INTEGER			NACOMP			! # ASTERIX components
      INTEGER			NCOMP			! # MORE components
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the omit string
      CALL ADI_GET0C( ARGS(5), OMIT, STATUS )

*  Locate MORE box of input
      CALL ADI1_LOCMORE( ARGS(2), .FALSE., IMLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Create the output MORE box
        CALL ADI1_LOCMORE( ARGS(4), .TRUE., OMLOC, STATUS )

*    Loop over input components
        CALL DAT_NCOMP( IMLOC, NCOMP, STATUS )
        DO ICOMP = 1, NCOMP

*      Index this component
          CALL DAT_INDEX( IMLOC, ICOMP, CLOC, STATUS )
          CALL DAT_NAME( CLOC, NAME, STATUS )

*      Omitting anything?
          IF ( (OMIT .GT. ' ') .AND. (NAME.EQ.'ASTERIX') ) THEN

*        Create o/p ASTERIX box
            CALL ADI1_LOCAST( ARGS(4), .TRUE., OALOC, STATUS )

*        Loop over ASTERIX components
            CALL DAT_NCOMP( CLOC, NACOMP, STATUS )
            DO IACOMP = 1, NACOMP

*          Index the ASTERIX component
              CALL DAT_INDEX( CLOC, IACOMP, ACLOC, STATUS )
              CALL DAT_NAME( ACLOC, ANAME, STATUS )

*          Are we omitting this component
              IF ( .NOT. (
     :             ((ANAME.EQ.'GRAFIX').AND.CHR_INSET(OMIT,'grf')).OR.
     :             ((ANAME.EQ.'GROUPING').AND.CHR_INSET(OMIT,'grp'))
     :                   ) ) THEN
                CALL DAT_COPY( ACLOC, OALOC, ANAME, STATUS )
              END IF

*          Release component
              CALL DAT_ANNUL( ACLOC, STATUS )

            END DO

          ELSE
            CALL DAT_COPY( CLOC, OMLOC, NAME, STATUS )
          END IF

*      Release component
          CALL DAT_ANNUL( CLOC, STATUS )

        END DO

*  Great, nothing to copy!
      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UDI1_COPANC', STATUS )

      END
