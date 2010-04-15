*+  RED4_OBSTOCOADD - Convert observation file name to coadd structure name
      SUBROUTINE RED4_OBSTOCOADD( OBS_FILE, COADD_NAME, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:Oyymmdd_oooo into a coadd structure
*     name of the form O_oooo.
*    Invocation :
*     CALL RED4_OBSTOCOADD( OBS_FILE, COADD_NAME, STATUS )
*    Parameters :
*     OBS_FILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or
*         ODIR:Oyymmdd_oooo)
*     COADD_NAME = CHARACTER*(*)( WRITE )
*         The name of the corresponding COADD structure (O_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     18-Sep-1990: Original version, copied from RED4_INTTOCOADD.  (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                           (SMB)
*     22-Feb-1993: Conform to error strategy                       (PND)
*     10-Nov-1994: Make vaguely portable                           (AB)
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
     :  OBS_FILE              ! Observation file name
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

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   look for a prefix in the file name.
      COLON_POS = INDEX( OBS_FILE, ':' )
      IF (COLON_POS .EQ. 0) COLON_POS = INDEX( OBS_FILE, '/')

*   Copy the observation file name to the character buffer,
*   removing the prefix if found.
      IF ( COLON_POS .GT. 0 ) THEN

         BUFFER = OBS_FILE( COLON_POS+1: )
      ELSE

         BUFFER = OBS_FILE
      END IF

*   Check that the first character in the name is "O", otherwise
*   an invalid observation name has been given.
      IF ( BUFFER(1:1) .EQ. 'O' .OR. BUFFER(1:1) .EQ. 'o') THEN

*      Remove the "yymmdd" characters lying between this "O"
*      and the "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check that an underscore has been found, otherwise the
*      observation file name is invalid.
         IF ( UNDER_POS .GT. 0 ) THEN

*         The coadd structure name is now the "O" followed by
*         the characters after and including this underscore.

            COADD_NAME = 'O' // BUFFER( UNDER_POS: )
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
            CALL ERR_REP( ' ', 'RED4_OBSTOCOADD: Invalid '/
     :        /'OBSERVATION file name ^OBS_FILE', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
         CALL ERR_REP( ' ', 'RED4_OBSTOCOADD: Invalid observation '/
     :     /'file name ^OBS_FILE', STATUS )
      END IF

      END
