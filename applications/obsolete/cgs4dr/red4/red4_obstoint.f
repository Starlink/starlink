*+  RED4_OBSTOINT - Convert observation file name to integration filename
      SUBROUTINE RED4_OBSTOINT( OBS_FILE, INT_FILE, STATUS )
*    Description :
*     This routine converts an observation file name of the form
*     Oyymmdd_oooo or ODIR:Oyymmdd_oooo into a reduced observation file
*     name of the form IDIR:Iyymmdd_oooo_1.
*    Invocation :
*     CALL RED4_OBSTOINT( OBS_FILE, INT_FILE, STATUS )
*    Parameters :
*     OBS_FILE  = CHARACTER*(*)( READ )
*         The observation file name (Oyymmdd_oooo or ODIR:Oyymmdd_oooo)
*     INT_FILE = CHARACTER*(*)( WRITE )
*         The reduced observation file name (IDIR:Iyymmdd_oooo_1)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P.N.Daly     (JACH::PND)
*    History :
*     17-Dec-1993: Original version.                       (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMERRS'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  OBS_FILE              ! observation file name
*    Export:
      CHARACTER*(*)
     :  INT_FILE              ! Integration file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS,            ! Position of colon in character string
     :  CLEN                  ! String length
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    See if the file name already has a prefix. Look for a ":" or  "/"
*    in the file name.
      COLON_POS = INDEX( OBS_FILE, ':' )
      IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( OBS_FILE, '/' )

      CALL RED4_GET_PREFIX ('I', LPREFIX, STATUS)

*   Get the length of the string
      CLEN = 0
      CLEN = CHR_LEN( OBS_FILE )

*   Create the output filename
      INT_FILE = LPREFIX(:CHR_LEN(LPREFIX))// 'i' /
     : / OBS_FILE(COLON_POS+2:CLEN) // '_1'

      END
