      SUBROUTINE USI_ASSOC( PAR, CLASS, ACCESS, ID, STATUS )
*+
*  Name:
*     USI_ASSOC

*  Purpose:
*     Associate an ADI object with an environment parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI_ASSOC( PAR, CLASS, ACCESS, ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        Name of environment parameter to use
*     CLASS = CHARACTER*(*) (given)
*        Class of object to associate
*     ACCESS = CHARACTER*(*) (given)
*        Access mode for association
*     ID = INTEGER (returned)
*        ADI identifier of opened object
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
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*      9 Oct 1995 (DJA):
*        Added support for scalar strings and numerics
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
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		CLASS			! Data class required
      CHARACTER*(*)		ACCESS			! Access mode

*  Arguments Returned:
      INTEGER			ID			! ADI identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FNAME			! Input object
      CHARACTER*3		SSTR			! SCL in characters
      CHARACTER*(DAT__SZLOC)	TLOC			! Temp HDS object

      DOUBLE PRECISION		DVAL			! Scalar value

      INTEGER			EP, PPOS		! Character pointers
      INTEGER			FLEN			! Length of FNAME
      INTEGER			FSTAT			! Fortran i/o status
      INTEGER			NDIG			! Chars used in SSTR
      INTEGER			SCL			! Length of scalar data
      INTEGER			TFID			! Temp ADI object

      LOGICAL			LVAL			! Scalar logical value
      LOGICAL			SCALAR			! Read a scalar?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does parameter name include a representation code?
      PPOS = INDEX( PAR, '%' )
      IF ( PPOS .EQ. 0 ) THEN
        EP = LEN(PAR)
      ELSE
        EP = MAX(1,PPOS - 1)
      END IF

*  Get file name
      CALL USI_GET0C( PAR(:EP), FNAME, STATUS )

*  Open the file
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get length of FNAME
        FLEN = CHR_LEN(FNAME)

*    Has user supplied a string delimited by quotes
        SCALAR = .FALSE.
        IF ( (FNAME(1:1).EQ.FNAME(FLEN:FLEN)) .AND.
     :       ((FNAME(1:1) .EQ. '''') .OR. (FNAME(1:1) .EQ. '"')) ) THEN
          SCL = FLEN - 2
          CALL CHR_ITOC( SCL, SSTR, NDIG )
          CALL DAT_TEMP( '_CHAR'//SSTR(:NDIG), 0, 0, TLOC, STATUS )
          CALL DAT_PUT0C( TLOC, FNAME(2:SCL), STATUS )
          SCALAR = (STATUS.EQ.SAI__OK)

*    One of YES, NO, TRUE or FALSE
        ELSE IF ( INDEX( 'yYnNTtFf', FNAME(1:1)) .GT. 0 ) THEN
          IF ( CHR_INSET( 'TRUE,YES', FNAME(:FLEN)) ) THEN
            SCALAR = .TRUE.
            LVAL = .TRUE.
          ELSE IF ( CHR_INSET( 'FALSE,NO', FNAME(:FLEN)) ) THEN
            SCALAR = .TRUE.
            LVAL = .FALSE.
          END IF
          IF ( SCALAR ) THEN
            CALL DAT_TEMP( '_LOGICAL', 0, 0, TLOC, STATUS )
            CALL DAT_PUT0L( TLOC, LVAL, STATUS )
          END IF

*    Last try is a numeric value
        ELSE

*      Try reading as a number
          READ( FNAME, *, IOSTAT=FSTAT ) DVAL
          IF ( FSTAT .EQ. 0 ) THEN
            CALL DAT_TEMP( '_DOUBLE', 0, 0, TLOC, STATUS )
            CALL DAT_PUT0D( TLOC, DVAL, STATUS )
            SCALAR = (STATUS.EQ.SAI__OK)
          END IF

        END IF

*    Scalar object?
        IF ( SCALAR ) THEN

*      Create HDSfile object]
          CALL ADI1_MKFILE( TLOC, 'READ', TFID, STATUS )

*      Link to requested data class object
          CALL ADI_FLINK( TFID, CLASS, ID, STATUS )

*    No representation supplied
        ELSE IF ( PPOS .EQ. 0 ) THEN
          CALL ADI_FOPEN( FNAME, CLASS, ACCESS, ID, STATUS )

*    If caller specified a representation on the parameter, glue it
*    on to the file name
        ELSE
          CALL ADI_FOPEN( FNAME(:MAX(1,FLEN))//PAR(PPOS:),
     :                                   CLASS, ACCESS, ID, STATUS )
        END IF
      END IF

*  Store in common
      CALL USI0_STOREI( PAR(:EP), ID, 'I', STATUS )

      END
