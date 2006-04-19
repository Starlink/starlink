      SUBROUTINE PARSECON_GPBTOK ( LU, TOKEN, TOKLEN, LINENUM, STATUS )
*+
*  Name:
*     PARSECON_GPBTOK

*  Purpose:
*     Get next token from interface file.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_GPBTOK( [p]... )

*  Description:
*     {routine_description}

*  Get Next Token From Connection File On Given Logical Unit.:
*     Invocation :
*      CALL PARSECON_GPBTOK ( LU; TOKEN, TOKLEN, LINENUM, STATUS )
*     Parameters :
*      LU=INTEGER (given)
*            FORTRAN unit number of interface file.
*      TOKEN=CHARACTER*(*) (returned)
*            next token from interface file.
*      TOKLEN=INTEGER (returned)
*            number of characters in the token.
*      LINENUM=INTEGER (returned)
*            number of line currently being read
*      STATUS=INTEGER
*     Method :
*       Get next token from connection file on given logical unit. This
*       routine calls PARSECON_GETTOK and the only extra task it performs is
*       to discard 'new-record' conditions and count them, allowing
*       track to be kept of the number of the line in the input file
*       which has been reached, which is useful for reporting syntax
*       errors.
*     Deficiencies :

*     Bugs :

*     Authors :
*      W.F.Lupton (RGO)
*     History :
*      18.09.1984:  VAX version (REVAD::BDK)
*      16.10.1990:  Move SAVE instruction for standard (RLVAD::AJC)

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER LU                    ! unit number for read from
                                    ! interface file


*  Arguments Returned:
      CHARACTER*(*) TOKEN           ! token (see gettok for further info)

      INTEGER TOKLEN                ! length of token
                                    ! (if <= 0 indicates error or EOF)

      INTEGER LINENUM               ! number of line being read


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER LINE                  ! internal copy of LINENUM
      SAVE LINE


*  Local Data:
      DATA LINE / 0 /


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Get tokens until not an end-of record condition
*
      TOKLEN = -1

      DO WHILE ( ( TOKLEN .EQ. -1 ) .AND. ( STATUS .EQ. SAI__OK ) )

         CALL PARSECON_GETTOK ( LU, TOKEN, TOKLEN, STATUS )
         IF ( TOKLEN .EQ. -1 ) LINE = LINE + 1

      ENDDO

      LINENUM = LINE

      END
