      SUBROUTINE EDI_DEFLD( ID, PAR, QUANTS, HOW, STATUS )
*+
*  Name:
*     EDI_DEFLD

*  Purpose:
*     Provides defaults to a parameter by locating lists of a certain type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_DEFLD( ID, PAR, QUANTS, HOW, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     PAR = CHARACTER*(*) (given)
*        Name of environment variable whose default is to be set
*     QUANTS = CHARACTER*(*) (given)
*        Names of quantities for which defaults are to be set
*     HOW = CHARACTER*(*) (given)
*        How default is to be set. Can take values NAME or NUMBER. Case is
*        not significant
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Aug 1995 (DJA):
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
      INTEGER			ID
      CHARACTER*(*)		PAR,QUANTS,HOW

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      INTEGER			MXDEF			! Max # quantities
        PARAMETER		( MXDEF = 5 )

*  Local Variables:
      CHARACTER*80		LNAMS			! Default names
      CHARACTER*20		NAME			! List name

      INTEGER			IQ			! Loop over QUANTS
      INTEGER			LNLEN			! LNAMS used length
      INTEGER			LNUMS(MXDEF)		! List numbers
      INTEGER			NDEF			! Number of defaults
      INTEGER			NUM			! List number

      LOGICAL			BYNAME			! Default by name?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do it by name?
      BYNAME = CHR_SIMLR( HOW, 'NAME' )

*  For each element of QUANTS
      LNLEN = 0
      DO IQ = 1, CHR_LEN(QUANTS)

*    Locate the list
        CALL EDI_QFND( ID, QUANTS(IQ:IQ), NAME, NUM, STATUS )

*    List found?
        IF ( NUM .GT. 0 ) THEN
          NDEF = NDEF + 1

*      Default by name?
          IF ( BYNAME ) THEN
            IF ( NDEF .EQ. 1 ) THEN
              LNAMS = NAME
            ELSE
              LNAMS = LNAMS(:LNLEN) // NAME
            END IF
            LNLEN = LNLEN + CHR_LEN(NAME)

*      By number
          ELSE
            LNUMS(NDEF) = NUM
          END IF

*    Failure to find list aborts defaulting process
        ELSE
          GOTO 99

        END IF

      END DO

*  Set the default
      IF ( BYNAME ) THEN
        CALL USI_DEF0C( PAR, LNAMS(:LNLEN), STATUS )
      ELSE
        CALL USI_DEF1I( PAR, NDEF, LNUMS, STATUS )
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_DEFLD', STATUS )

      END
