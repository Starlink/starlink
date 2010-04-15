*+  RED4_OBSTOINDEX - Convert observation file name to index file name
      SUBROUTINE RED4_OBSTOINDEX( OBS_FILE, INDEX_FILE, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:Oyymmdd_oooo into the name of the index file
*     in which that observation may be found, of the form
*     CGS4_INDEX:CGS4_yymmdd.INDEX.
*
*     N.B. This routine can also be used to convert a CALIBRATION
*     name, of the form CAyymmdd_oooo into an index file name.
*
*     The routine was produced as an attempt to rationalise the
*     conversion between one form of file name and another, so that
*     making a change to the naming convention would not be such a
*     major task in the future, and to allow error reports to be
*     generated if an invalid file name is given.
*    Invocation :
*     CALL RED4_OBSTOINDEX( OBS_FILE, INDEX_FILE, STATUS )
*    Parameters :
*     OBS_FILE   = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
*         or CALIBRATION file name (CAyymmdd_oooo or RODIR:CAyymmdd_oooo)
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
*     23-Jul-1990: Original version.                       (SMB)
*     23-Jul-1990: INDDIR changed to CGS4_INDEX.           (SMB)
*     30-Jul-1990: Documentation change: "yy" allowed to
*                  be "yyyy".                              (SMB)
*     31-Jul-1990: "yyyy" changed back to "yy" !!!!!!!!    (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.          (SMB)
*     13-Dec-1990: Ability to deal with CALIBRATION file
*                  names added.                            (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                   (SMB)
*     22-Feb-1993: Conform to error strategy               (PND)
*     10-Nov-1994: Make vaguely portable                   (AB)
*     17-Nov-1994: Add translation of prefix               (AB)
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
      CHARACTER*256 TRANSLATION    ! translation of prefix
      CHARACTER*20 LPREFIX     ! Prefix
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

*   Check the first characters in the name.
*   If the first charcater is "O" and observation has been given.
*   If the first two characters are "CA" then a CALIBRATION has been given.
*   Otherwise an invalid observation name has been given.
      IF ( BUFFER(1:1) .EQ. 'O' .OR. BUFFER(1:1) .EQ. 'o') THEN

*      Extract the "yymmdd" characters lying between this "O"
*      and the "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check that an underscore has been found in a sensible place,
*      otherwise the observation file name is invalid.
         IF ( UNDER_POS .GT. 2 ) THEN

*         Prefix the "yymmdd" characters with the appropriate prefix and
*         add '.INDEX' to the end, to make the index file name.
            CALL RED4_GET_PREFIX ('INDEX', LPREFIX, STATUS)
*          translate it because fortran i/o cant handle unix environment
*          variables.
            CALL RED4_TRANSLATE_PREFIX (LPREFIX, TRANSLATION, STATUS)
            INDEX_FILE = TRANSLATION(:CHR_LEN(TRANSLATION))//'cgs4_'
     :       // BUFFER( 2: UNDER_POS-1 ) // '.index'
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
            CALL ERR_REP( ' ', 'RED4_OBSTOINDEX: Invalid '/
     :        /'OBSERVATION file name ^OBS_FILE', STATUS )
         END IF
      ELSE IF ( BUFFER(1:2) .EQ. 'CA' .OR. BUFFER(1:2) .EQ. 'ca') THEN

*      Extract the "yymmdd" characters lying between this "CA"
*      and the "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check that an underscore has been found in a sensible place,
*      otherwise the CALIBRATION file name is invalid.
         IF ( UNDER_POS .GT. 3 ) THEN

*         Prefix the "yymmdd" characters with the appropriate prefix and
*         add '.INDEX' to the end, to make the index file name.
            CALL RED4_GET_PREFIX ('INDEX', LPREFIX, STATUS)
            CALL RED4_TRANSLATE_PREFIX (LPREFIX, TRANSLATION, STATUS)
            INDEX_FILE = TRANSLATION(:CHR_LEN(TRANSLATION))//'cgs4_'
     :        // BUFFER( 3: UNDER_POS-1 ) // '.index'
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
            CALL ERR_REP( ' ', 'RED4_OBSTOINDEX: Invalid '/
     :        /'CALIBRATION file name ^OBS_FILE', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'OBS_FILE', OBS_FILE )
         CALL ERR_REP( ' ', 'RED4_OBSTOINDEX: Invalid observation '/
     :     /'file name ^OBS_FILE', STATUS )
      END IF

      END
