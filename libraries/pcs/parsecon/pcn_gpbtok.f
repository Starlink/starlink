*+  PARSECON_GPBTOK - Get next token from interface file
      SUBROUTINE PARSECON_GPBTOK ( LU, TOKEN, TOKLEN, LINENUM, STATUS )
*    Description :
*   Get next token from connection file on given logical unit.
*    Invocation :
*     CALL PARSECON_GPBTOK ( LU; TOKEN, TOKLEN, LINENUM, STATUS )
*    Parameters :
*     LU=INTEGER (given)
*           FORTRAN unit number of interface file.
*     TOKEN=CHARACTER*(*) (returned)
*           next token from interface file.
*     TOKLEN=INTEGER (returned)
*           number of characters in the token.
*     LINENUM=INTEGER (returned)
*           number of line currently being read
*     STATUS=INTEGER
*    Method :
*      Get next token from connection file on given logical unit. This
*      routine calls PARSECON_GETTOK and the only extra task it performs is
*      to discard 'new-record' conditions and count them, allowing 
*      track to be kept of the number of the line in the input file 
*      which has been reached, which is useful for reporting syntax 
*      errors.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (RGO)
*    History :
*     18.09.1984:  VAX version (REVAD::BDK)
*     16.10.1990:  Move SAVE instruction for standard (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER LU                    ! unit number for read from 
                                    ! interface file

*    Export :
      CHARACTER*(*) TOKEN           ! token (see gettok for further info)

      INTEGER TOKLEN                ! length of token
                                    ! (if <= 0 indicates error or EOF)

      INTEGER LINENUM               ! number of line being read

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER LINE                  ! internal copy of LINENUM
      SAVE LINE

*    Local Data :
      DATA LINE / 0 /


*-

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
