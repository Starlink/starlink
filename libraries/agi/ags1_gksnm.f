************************************************************************
*+  AGS_1GKSNM - Make up a GKS name string from the wkid and conid

      SUBROUTINE AGS_1GKSNM ( WKID, CONID, GNAME, STRLEN, STATUS )

*    Description :
*     This routine makes up a GKS name from the workstation id and
*     the connection id. The name is made up as 'GKS_<wkid>_<conid>'.
*     The resulting name can be used to open SGS on the particular
*     device.
*
*    Invocation :
*     CALL AGS_1GKSNM( WKID, CONID, GNAME, STRLEN, STATUS )
*
*    Method :
*     Convert the workstation and connection identifiers to character
*     strings.
*     Append these onto the prefix and calculate the string length.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Aug 1988
*     Nov 1989  Changed CHR_LDBLNK to CHR_LDBLK
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :

*     GKS workstation identifier
      INTEGER WKID

*     GKS connection identifier
      INTEGER CONID

*    Export :

*     GKS name for the device
      CHARACTER * ( * ) GNAME

*     Length of name string
      INTEGER STRLEN

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER CHR_LEN, CI2, WI2
      CHARACTER SCONID * 20, SWKID * 20
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Write the WKID and CONID into character strings
         WRITE( SWKID,  '( I10 )' ) WKID
         WRITE( SCONID, '( I10 )' ) CONID

*   Find the start and finish of these strings
         CALL CHR_LDBLK( SWKID )
         WI2 = CHR_LEN( SWKID )
         CALL CHR_LDBLK( SCONID )
         CI2 = CHR_LEN( SCONID )

*   Construct a name as GKS_<wkid>_<conid>
         GNAME = 'GKS_' // SWKID( :WI2 ) //
     :           '_' // SCONID( :CI2 )
         STRLEN = 5 + WI2 + CI2

      ENDIF

      END

