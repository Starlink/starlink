*+  RED4_ROBSTOGRP - Convert reduced observation name to reduced group
      SUBROUTINE RED4_ROBSTOGRP( ROBS_FILE, GRPNUM, GRP_FILE, STATUS )
*    Description :
*     This routine converts a REDUCED OBSERVATION file name of the form
*     ROyymmdd_oooo or RODIR:ROyymmdd_oooo into a reduced group
*     file name of the form RGDIR:RGyymmdd_gggg, where gggg is the group
*     number.
*    Invocation :
*     CALL RED4_ROBSTOGRP( ROBS_FILE, GRPNUM, GRP_FILE, STATUS )
*    Parameters :
*     ROBS_FILE = CHARACTER*(*)( READ )
*         The reduced observation file name (ROyymmdd_oooo or
*         RODIR:ROyymmdd_oooo).
*     GRP_FILE  = CHARACTER*(*)( WRITE )
*         The reduced group file name (RGDIR:RGyymmdd_gggg)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     18-Sep-1990: Original version.                       (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                   (SMB)
*     23-FEB-1993: Conform to error strategy               (PND)
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
     :  ROBS_FILE             ! Reduced observation file name
      INTEGER
     :  GRPNUM                ! Group number
*    Export:
      CHARACTER*(*)
     :  GRP_FILE              ! Reduced group file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  CPOS,                 ! Position in character string.
     :  COLON_POS,            ! Position of colon in character string
     :  UNDER_POS             ! Position of underscore in character string
      CHARACTER*80
     :  BUFFER                ! Character buffer
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    See if the file name already has a prefix. Look for a ":" or  "/"
*    in the file name.
      COLON_POS = INDEX( ROBS_FILE, ':' )
      IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( ROBS_FILE, '/' )

      CALL RED4_GET_PREFIX ('RG', LPREFIX, STATUS)

*   Copy the reduced observation name to a temporary buffer, removing the
*   prefix if present.
      IF ( COLON_POS .NE. 0 ) THEN

         BUFFER = ROBS_FILE( COLON_POS+1: )
      ELSE

         BUFFER = ROBS_FILE
      END IF

*   Check that the first two characters are "RO".
*   (If the first characters are not "RO", the reduced observation
*   file name given is invalid).
      IF ( BUFFER(1:2) .EQ. 'RO' .OR. BUFFER(1:2) .EQ. 'ro') THEN

*      The observation number "_oooo" now needs to be removed. This
*      is done by locating the occurrence of an "_".
         UNDER_POS = INDEX( BUFFER, '_' )

*      Check an "_" has been found.
*      If not then the reduced observation file name is invalid.
         IF ( UNDER_POS .GT. 0 ) THEN

*         Return the name of the reduced group file by
*         prefixing the buffer, ignoring the
*         "RO" at the beginning and the "oooo" at the end,
*         and appending "gggg" obtained from the group number.
            CPOS = 0
            GRP_FILE = ' '
            CALL CHR_PUTC( LPREFIX(:CHR_LEN(LPREFIX))//'rg',
     :       GRP_FILE, CPOS )
            CALL CHR_PUTC( BUFFER( 3: UNDER_POS ), GRP_FILE, CPOS )
            CALL CHR_PUTI( GRPNUM, GRP_FILE, CPOS )
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'ROBS_FILE', ROBS_FILE )
            CALL ERR_REP( ' ', 'RED4_ROBSTOGRP: Invalid REDUCED '/
     :        /'OBSERVATION file name given ^ROBS_FILE', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ROBS_FILE', ROBS_FILE )
         CALL ERR_REP( ' ', 'RED4_ROBSTOGRP: Invalid reduced '/
     :     /'observation file name given ^ROBS_FILE', STATUS )
      END IF

      END

