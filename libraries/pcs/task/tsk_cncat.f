      SUBROUTINE TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )
*+
*  Name:
*     TASK_CNCAT

*  Purpose:
*     Concatenate an array of strings into an argument list

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )

*  Description:
*     Given an array of strings, concatenate them with separating single
*     spaces, having removed leading and trailing spaces. This generates
*     an argument list suitable for passing through the ADAM message 
*     system.

*  Arguments:
*     NVALS=INTEGER (given)
*           number of strings in the array
*     STRINGS=CHARACTER*(*) (given)
*           the array of strings
*     VALUE=CHARACTER*(*) (returned)
*           the concatenated string
*     STATUS=INTEGER

*  Algorithm:
*     For each of the NVALS elements of STRINGS, use LIB$SKPC to skip 
*     leading spaces and STR$TRIM to remove trailing spaces, then append 
*     the result onto VALUE with a single trailing space.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1985 (REVAD::BDK):
*        Original
*     01-MAR-1990: fix one-off error in FIRSTSPACE and initialise VALUE
*                  to spaces (REVAD::BDK)
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR_FANDL not STR$SKPC and STR$TRIM
*     24-AUG-1993 (RLVAD::AJC):
*        Remove istat - not used
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'

*  Arguments Given:
      INTEGER NVALS                    ! the number of strings

      CHARACTER*(*) STRINGS(*)         ! the array of strings

*  Arguments Returned:
      CHARACTER*(*) VALUE              ! the concatenated line

*  Status:
      INTEGER STATUS

*    External references :

*  Local Variables:
      INTEGER FIRSTSPACE               ! first free space in VALUE

      INTEGER MAXLEN                   ! LEN(VALUE)

      INTEGER J                        ! loop counter

      INTEGER START                    ! position of start of a string

      INTEGER ENDSTR                   ! position of end of a string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      FIRSTSPACE = 1
      MAXLEN = LEN ( VALUE )
      VALUE = ' '

      J = 0
      DO WHILE ( ( STATUS .EQ. SAI__OK ) .AND. ( J .LT. NVALS ) )

         J = J + 1
         CALL CHR_FANDL( STRINGS(J), START, ENDSTR )
         IF ( FIRSTSPACE + ENDSTR +1 .LE. MAXLEN ) THEN
            VALUE(FIRSTSPACE:) = STRINGS(J)(START:)
            FIRSTSPACE = FIRSTSPACE + ENDSTR + 1
         ELSE
            STATUS = TASK__STRFL
         ENDIF

      ENDDO

      END
