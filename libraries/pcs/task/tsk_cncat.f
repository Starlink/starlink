*+  TASK_CNCAT - concatenate an array of strings into an argument list
      SUBROUTINE TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )
*    Description :
*     Given an array of strings, concatenate them with separating single
*     spaces, having removed leading and trailing spaces. This generates
*     an argument list suitable for passing through the ADAM message 
*     system.
*    Invocation :
*     CALL TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )
*    Parameters :
*     NVALS=INTEGER (given)
*           number of strings in the array
*     STRINGS=CHARACTER*(*) (given)
*           the array of strings
*     VALUE=CHARACTER*(*) (returned)
*           the concatenated string
*     STATUS=INTEGER
*    Method :
*     For each of the NVALS elements of STRINGS, use LIB$SKPC to skip 
*     leading spaces and STR$TRIM to remove trailing spaces, then append 
*     the result onto VALUE with a single trailing space.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.06.1985:  Original (REVAD::BDK)
*     01.03.1990:  fix one-off error in FIRSTSPACE and initialise VALUE 
*                  to spaces (REVAD::BDK)
*      4.10.1992:  use CHR_FANDL not STR$SKPC and STR$TRIM (RLVAD::AJC)
*     24.08.1993:  remove istat - not used (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'

*    Import :
      INTEGER NVALS                    ! the number of strings

      CHARACTER*(*) STRINGS(*)         ! the array of strings

*    Export :
      CHARACTER*(*) VALUE              ! the concatenated line

*    Status :
      INTEGER STATUS

*    External references :

*    Local variables :
      INTEGER FIRSTSPACE               ! first free space in VALUE

      INTEGER MAXLEN                   ! LEN(VALUE)

      INTEGER J                        ! loop counter

      INTEGER START                    ! position of start of a string

      INTEGER ENDSTR                   ! position of end of a string
*-

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
