      SUBROUTINE CON_H2ITY( HDSTYP, INTTYP, STATUS )
*+
*  Name:
*     CON_H2ITY

*  Purpose:
*     Converts Interim character format code to its HDS equivalent.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CON_H2ITY( HDSTYP, INTTYP, STATUS )

*  Description:
*     Converts an HDS data type, e.g. _WORD, to its Interim equivalent
*     character form, e.g. SW.

*  Arguments:
*     HDSTYP = CHARACTER * ( DAT__SZTYP ) (Given)
*        The HDS data type.  It must have one of the following values:
*        "_BYTE", "_WORD", "_REAL", "_INTEGER", "_DOUBLE", "_UBYTE",
*        or "_UWORD" corresponding to signed byte, signed word, real,
*        integer, double precision, unsigned byte, and unsigned word.
*        If it is not one of these, an error report is made and
*        SAI__ERROR status will be set.
*     INTTYP = CHARACTER * 2 (Returned)
*        The Interim data type: SB, SW, SL, DP, R, UB, UW.  If the input
*        data type is invalid, this is a null string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 February 4 (MJC):
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
      CHARACTER * ( * )
     :  HDSTYP

*  Arguments Returned:
      CHARACTER * ( * )
     :  INTTYP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTYP
      PARAMETER( MAXTYP=7 )

*  Local Variables:
      INTEGER
     :  I                        ! Loop counter

      CHARACTER
     :  HTYPES( MAXTYP ) * ( DAT__SZTYP ), ! The table of HDS numeric
                                 ! data types
     :  ITYPES( MAXTYP ) * ( 2 ) ! The table of Interim numeric data
                                 ! types

*  Local Data:
      DATA ITYPES/'SB', 'SW', 'SL', 'R', 'DP', 'UB', 'UW' /
      DATA HTYPES/'_BYTE','_WORD','_INTEGER','_REAL','_DOUBLE',
     :            '_UBYTE','_UWORD'/

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a match is found.  Copy the type string to the output
*  argument. If none is found an error report will be made, otherwise
*  the routine exits.
      INTTYP = ' '
      DO I = 1, MAXTYP
         IF ( HDSTYP .EQ. HTYPES( I ) ) THEN
            INTTYP = ITYPES( I )
            GOTO 999
         END IF
      END DO

*  If an error occurred, then report a contextual message.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'HDSTYP', HDSTYP )
      CALL ERR_REP( 'CON_H2ITY_ERR',
     :  'Invalid HDS data type ^HDSTYP.  Unable to convert it to an '/
     :  /'Interim equivalent.', STATUS )

  999 CONTINUE
      END
