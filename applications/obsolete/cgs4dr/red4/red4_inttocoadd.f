*+  RED4_INTTOCOADD - Convert integration file name to coadd structure name
      SUBROUTINE RED4_INTTOCOADD( INT_FILE, COADD_NAME, STATUS )
*    Description :
*     This routine converts an integration file name of the form
*     Iyymmdd_oooo_iiii or IDIR:Iyymmdd_oooo_iiii into a coadd structure
*     name of the form I_oooo_iiii.
*
*     The routine was produced as an attempt to rationalise the
*     conversion between one form of file name and another, so that
*     making a change to the naming convention would not be such a
*     major task in the future, and to allow error reports to be
*     generated if an invalid file name is given.
*    Invocation :
*     CALL RED4_INTTOCOADD( INT_FILE, COADD_NAME, STATUS )
*    Parameters :
*     INT_FILE  = CHARACTER*(*)( READ )
*         The integration file name (Iyymmdd_oooo_iiii or
*         IDIR:Iyymmdd_oooo_iiii)
*     COADD_NAME = CHARACTER*(*)( WRITE )
*         The name of the corresponding COADD structure (I_oooo_iiii)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     31-Jul-1990: Original version.                               (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                  (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     19-Feb-1993: Conform to error strategy                       (PND)
*      9-Nov-1994: Attempt to make vaguely portable                (AB)
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
     :  COADD_NAME            ! Name of coadd structure
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS,            ! Position of colon in character string
     :  UNDER_POS             ! Position of underscore in character string
      CHARACTER*80
     :  BUFFER                ! Character buffer
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    look for a prefix in the file name.
      COLON_POS = INDEX( INT_FILE, ':' )
      IF (COLON_POS .EQ. 0) COLON_POS = INDEX( INT_FILE, '/')

*   Copy the integration file name to the character buffer,
*   removing the prefix if found.
      IF ( COLON_POS .GT. 0 ) THEN

         BUFFER = INT_FILE( COLON_POS+1: )
      ELSE

         BUFFER = INT_FILE
      END IF

*   Check that the first character in the name is "I", otherwise
*   an invalid integration name has been given.
      IF ( BUFFER(1:1) .EQ. 'I' .OR. BUFFER(1:1) .EQ. 'i') THEN

*      Remove the "yymmdd" characters lying between this "I"
*      and the "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check that an underscore has been found, otherwise the
*      integration file name is invalid.
         IF ( UNDER_POS .GT. 0 ) THEN

*         The coadd structure name is now the "I" followed by
*         the characters after and including this underscore.
            COADD_NAME = 'I' // BUFFER( UNDER_POS: )
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'INT_FILE', INT_FILE )
            CALL ERR_REP( ' ', 'RED4_INTTOCOADD: Invalid '/
     :        /'integration file name ^INT_FILE', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'INT_FILE', INT_FILE )
         CALL ERR_REP( ' ', 'RED4_INTTOCOADD: Invalid integration '/
     :     /'file name ^INT_FILE', STATUS )
      END IF

      END
