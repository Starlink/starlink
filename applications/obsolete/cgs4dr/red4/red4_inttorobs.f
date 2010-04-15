*+  RED4_INTTOROBS - Convert integration file name to reduced observation
      SUBROUTINE RED4_INTTOROBS( INT_FILE, ROBS_FILE, STATUS )
*    Description :
*     This routine converts an integration file name of the form
*     Iyymmdd_oooo_iiii or IDIR:Iyymmdd_oooo_iiii into a reduced
*     observation file name of the form RODIR:ROyymmdd_oooo.
*
*     The routine was produced as an attempt to rationalise the
*     conversion between one form of file name and another, so that
*     making a change to the naming convention would not be such a
*     major task in the future, and to allow error reports to be
*     generated if an invalid file name is given. It also enables
*     the complex character manipulations to be documented more
*     extensively.
*    Invocation :
*     CALL RED4_INTTOROBS( INT_FILE, ROBS_FILE, STATUS )
*    Parameters :
*     INT_FILE  = CHARACTER*(*)( READ )
*         The integration file name (Iyymmdd_oooo_iiii or
*         IDIR:Iyymmdd_oooo_iiii)
*     ROBS_FILE = CHARACTER*(*)( WRITE )
*         The reduced observation file name (RODIR:ROyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     23-Jul-1990: Original version.                               (SMB)
*     30-Jul-1990: Documentation change: "yy" allowed to
*                  be "yyyy".                                      (SMB)
*     31-Jul-1990: "yyyy" changed back to "yy" !!!!!!!!            (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                  (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     22-Feb-1993: Conform to error strategy                       (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  INT_FILE              ! Integration file name
*    Export:
      CHARACTER*(*)
     :  ROBS_FILE             ! Reduced integration file name
*    Status :
      INTEGER STATUS
*    External references:
*    External :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS,            ! Position of colon in character string
     :  UNDER_POS             ! Position of underscore in character string
      CHARACTER*80
     :  BUFFER                ! Character buffer
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    See if the file name already has a prefix. Look for a ":" or  "/"
*    in the file name.
      COLON_POS = INDEX( INT_FILE, ':' )
      IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( INT_FILE, '/' )

      CALL RED4_GET_PREFIX ('RO', LPREFIX, STATUS)

*    Copy the integration name to a temporary buffer, removing the
*    prefix if present.
      IF ( COLON_POS .NE. 0 ) THEN

         BUFFER = INT_FILE( COLON_POS+1: )
      ELSE

         BUFFER = INT_FILE
      END IF

*    Check that the first character is "I".
*    (If the first character is not "I", the integration file
*    name given is invalid).
      IF ( BUFFER(1:1) .EQ. 'I' .OR. BUFFER(1:1) .EQ. 'i' ) THEN

*       The integration number "_iiii" now needs to be removed. This
*       is done by locating the SECOND occurrence of a "_".
         UNDER_POS = INDEX( BUFFER, '_' )

         IF ( UNDER_POS .GT. 0 ) THEN

            UNDER_POS = UNDER_POS +
     :        INDEX( BUFFER( UNDER_POS+1: ), '_' )
         END IF

*       Check that two "_"s have been found.
*       If not then the integration file name is invalid.
         IF ( UNDER_POS .GT. 0 ) THEN

*         Return the name of the reduced observation file by
*         prefixing the buffer, ignoring the
*         "I" at the beginning and the "_iiii" at the end.
            ROBS_FILE = LPREFIX(:CHR_LEN(LPREFIX))//'ro' /
     :       / BUFFER( 2: UNDER_POS-1 )
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'INT_FILE', INT_FILE )
            CALL ERR_REP( ' ', 'RED4_INTTOROBS: Invalid '/
     :        /'integration file name ^INT_FILE', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'INT_FILE', INT_FILE )
         CALL ERR_REP( ' ', 'RED4_INTTOROBS: Invalid '/
     :     /'integration file name ^INT_FILE', STATUS )
      END IF

      END
