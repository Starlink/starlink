*+  RED4_OBSTOROBS - Convert observation file name to reduced observation
      SUBROUTINE RED4_OBSTOROBS( OBS_FILE, ROBS_FILE, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:Oyymmdd_oooo into a reduced observation file
*     name of the form RODIR:ROyymmdd_oooo.
*
*     The routine was produced as an attempt to rationalise the
*     conversion between one form of file name and another, so that
*     making a change to the naming convention would not be such a
*     major task in the future, and to allow error reports to be
*     generated if an invalid file name is given.
*    Invocation :
*     CALL RED4_OBSTOROBS( OBS_FILE, ROBS_FILE, STATUS )
*    Parameters :
*     OBS_FILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
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
*     23-Jul-1990: Original version.                       (SMB)
*     30-Jul-1990: Documentation change: "yy" allowed to
*                  be "yyyy".                              (SMB)
*     31-Jul-1990: "yyyy" changed back to "yy" !!!!!!!!    (SMB)
*     24-Feb-1993: Conform to error strategy               (PND)
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
     :  OBS_FILE              ! observation file name
*    Export:
      CHARACTER*(*)
     :  ROBS_FILE             ! Reduced observation file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS             ! Position of colon in character string
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    See if the file name already has a prefix. Look for a ":" or  "/"
*    in the file name.
      COLON_POS = INDEX( OBS_FILE, ':' )
      IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( OBS_FILE, '/' )

      CALL RED4_GET_PREFIX ('RO', LPREFIX, STATUS)

      IF ( COLON_POS .EQ. 0 ) THEN

*      There is no prefix, so assume the name is Oyymmdd_oooo.
*      Convert it to the correct form by prefixing it
         ROBS_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'r' // OBS_FILE
      ELSE

*      There is a prefix.
*      Convert it to the correct form by replacing the present
*      prefix and adding an 'R'
         ROBS_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'r' /
     :    / OBS_FILE( COLON_POS+1: )
      END IF

      END
