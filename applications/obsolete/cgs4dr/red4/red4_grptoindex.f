*+  RED4_GRPTOINDEX - Convert group file name to index file name
      SUBROUTINE RED4_GRPTOINDEX( GRP_FILE, INDEX_FILE, STATUS )
*    Description :
*     This routine converts a group file name of the form
*     RGyymmdd_gggg or RGDIR:RGyymmdd_gggg into the name of the index file
*     in which that group may be found, of the form
*     CGS4_INDEX:CGS4_yymmdd.INDEX.
*
*     N.B. This may also be used to convert a STANDARD file name
*     STyymmdd_gggg into the name of an index file.
*
*     N.B This may also be used to convert a REDUCED observation
*     filename ROyymmdd_oooo or RODIR:ROyymmdd_ooo into the name of
*     an index file.
*    Invocation :
*     CALL RED4_GRPTOINDEX( GRP_FILE, INDEX_FILE, STATUS )
*    Parameters :
*     GRP_FILE   = CHARACTER*(*)( READ )
*         The group file name (RGyymmdd_gggg or RGDIR:RGyymmdd_gggg)
*         or STANDARD file name (STyymmdd_gggg or STDIR:STyymmdd_gggg)
*     INDEX_FILE = CHARACTER*(*)( WRITE )
*         The index file name (CGS4_INDEX:CGS4_yymmdd.INDEX)
*     STATUS     = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     13-Dec-1990: Original version, copied from RED4_OBSTOINDEX.  (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     27-Jul-1992: Add RO files as this routine is called by
*                  divide_by_std action and one can divide RO
*                  files by standards                              (PND)
*     19-Feb-1993: Conform to error strategy                       (PND)
*      9-Nov-1994: Attempt to make vaguely portable                (AB)
*     17-Nov-1994: Translate the prefix for the index file         (AB)
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
     :  GRP_FILE              ! Group file name
*    Export:
      CHARACTER*(*)
     :  INDEX_FILE            ! Index file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS,            ! Position of colon in character string
     :  UNDER_POS             ! Position of underscore in character string
      CHARACTER*80
     :  BUFFER                ! Character buffer
      CHARACTER*256
     :  TRANSLATION           ! Translation of prefix
      CHARACTER*20
     : LPREFIX                ! Prefix to apply
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    look for a prefix in the file name.
      COLON_POS = INDEX( GRP_FILE, ':' )
      IF (COLON_POS .EQ. 0) COLON_POS = INDEX( GRP_FILE, '/')

*   Copy the group file name to the character buffer,
*   removing the prefix if found.
      IF ( COLON_POS .GT. 0 ) THEN

         BUFFER = GRP_FILE( COLON_POS+1: )
      ELSE

         BUFFER = GRP_FILE
      END IF

*   Check that the first two characters in the name are "RG", "ST", or "RO"
*   otherwise an invalid group name has been given.
      IF ( ( BUFFER(1:2) .EQ. 'RG' .OR. BUFFER(1:2) .EQ. 'rg') .OR.
     :     ( BUFFER(1:2) .EQ. 'ST' .OR. BUFFER(1:2) .EQ. 'st') .OR.
     :     ( BUFFER(1:2) .EQ. 'RO' .OR. BUFFER(1:2) .EQ. 'ro') ) THEN

*      Extract the "yymmdd" characters lying between this "RG" (or "ST")
*      and the "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check that an underscore has been found in the right place,
*      otherwise the group file name is invalid.
         IF ( UNDER_POS .GT. 2 ) THEN

*         Prefix the "yymmdd" characters with the appropriate thing and
*         add '.INDEX' to the end, to make the index file name.
            CALL RED4_GET_PREFIX ('INDEX', LPREFIX, STATUS)

*          translate the prefix
            CALL RED4_TRANSLATE_PREFIX (LPREFIX, TRANSLATION, STATUS)

            INDEX_FILE = TRANSLATION(:CHR_LEN(TRANSLATION))//'cgs4_'
     :        // BUFFER( 3: UNDER_POS-1 ) // '.index'
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'GRP_FILE', GRP_FILE )
            CALL ERR_REP( ' ', 'RED4_GRPTOINDEX: Invalid '/
     :        /'group file name (^GRP_FILE)', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'GRP_FILE', GRP_FILE )
         CALL ERR_REP( ' ', 'RED4_GRPTOINDEX: Invalid group '/
     :     /'file name (^GRP_FILE)', STATUS )
      END IF

      END
