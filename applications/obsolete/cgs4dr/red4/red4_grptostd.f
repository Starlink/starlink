*+  RED4_GRPTOSTD - Convert group or observation file name to standard file name
      SUBROUTINE RED4_GRPTOSTD( GRP_FILE, STD_FILE, STATUS )
*    Description :
*     This routine converts a group file name or reduced observation
*     file name of the form RGDIR:RGyymmdd_gggg or RODIR:ROyymmdd_oooo
*     into a standard file name of the form RGDIR:STyymmdd_gggg or
*     RODIR:STyymmdd_oooo. The routine replaces the first two
*     letters at the beginning or after a colon with "ST", so it will
*     work equally well for converting reduced observation or group
*     file names.
*    Invocation :
*     CALL RED4_GRPTOSTD( GRP_FILE, STD_FILE, STATUS )
*    Parameters :
*     GRP_FILE  = CHARACTER*(*)( READ )
*         The group or reduced observation file name (ROyymmdd_oooo,
*         RODIR:ROyymmdd_oooo, RGyymmdd_gggg or RGDIR:RGyymmdd_gggg).
*     STD_FILE = CHARACTER*(*)( WRITE )
*         The standard file name (STyymmdd_oooo, RODIR:STyymmdd_oooo,
*         STyymmdd_gggg or RGDIR:STyymmdd_gggg)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*      7-Dec-1990: Original version.                       (SMB)
*     19-Feb-1993: Conform to error strategy               (PND)
*      9-Nov-1994: Attempt to make vaguely portable        (AB)
*     13-Jun-1996: Remove suffixes after obs num           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  GRP_FILE             ! Group or observation file name
*    Export:
      CHARACTER*(*)
     :  STD_FILE             ! Standard file name
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS,           ! Position of colon in character string
     :  UNDER_POS1,          ! Position of underscore in character string
     :  UNDER_POS2           ! Position of underscore in character string
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Look for a prefix in the string.
      COLON_POS = INDEX( GRP_FILE, ':' )
      IF (COLON_POS .EQ. 0) COLON_POS = INDEX( GRP_FILE, '/')

*   If there is a prefix in the string, replace the first two
*   characters after it with 'ST'. Otherwise replace the first two
*   characters in the string with 'ST'.
      IF ( COLON_POS .GT. 0 ) THEN

         STD_FILE = GRP_FILE(1:COLON_POS) // 'st' //
     :                GRP_FILE(COLON_POS+3:)
      ELSE

         STD_FILE = 'st' // GRP_FILE(3:)
      END IF

*   We now have a file but might have some _pf _dbs _imspc or other suffixes
      CALL CHR_RMBLK( STD_FILE )
      UNDER_POS1 = INDEX( STD_FILE, '_' )
      UNDER_POS2 = INDEX( STD_FILE(UNDER_POS1+1:LEN(STD_FILE)), '_' )
      IF ( UNDER_POS2 .GT. 0 ) STD_FILE = STD_FILE(1:UNDER_POS1+UNDER_POS2-1)
      CALL CHR_RMBLK( STD_FILE )
      IF (VERBOSE) CALL MSG_OUT( ' ', 'Standard file is '//STD_FILE, STATUS )

      END

