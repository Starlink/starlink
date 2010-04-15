*+  RED4_OBSTOCAL - Convert observation file name to calibration file name
      SUBROUTINE RED4_OBSTOCAL( OBS_FILE, CAL_FILE, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:ROyymmdd_oooo into a calibration file name
*     of the form CGS4_SEARCH:CAyymmdd_oooo.
*    Invocation :
*     CALL RED4_OBSTOCAL( OBS_FILE, CAL_FILE, STATUS )
*    Parameters :
*     OBS_FILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo, or ODIR:Oyymmdd_oooo)
*     CAL_FILE = CHARACTER*(*)( WRITE )
*         The calibration file name (CGS4_SEARCH:CAyymmdd_oooo)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*      7-Dec-1990: Original version.                          (SMB)
*     11-Dec-1990: Directory changed to CGS4_SEARCH, as the
*                  calibration may have been put into the
*                  current directory.                         (SMB)
*     22-Feb-1993: Conform to error strategy                  (PND)
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
     :  OBS_FILE             ! Observation file name
*    Export:
      CHARACTER*(*)
     :  CAL_FILE             ! Calibration file name
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

      CALL RED4_GET_PREFIX ('CA', LPREFIX, STATUS)

      IF ( COLON_POS .EQ. 0 ) THEN

*      There is no prefix, so assume the name is Oyymmdd_oooo.
*      Convert it to the correct form by removing
*      the 'O' adding the prefix and 'CA'
         CAL_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'ca' // OBS_FILE(2:)
      ELSE

*      There is a prefix.
*      Convert it to the correct form by removing the
*      prefix and adding the new prefix and 'CA'

         CAL_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'ca' /
     :    / OBS_FILE( COLON_POS+2: )
      END IF

      END
