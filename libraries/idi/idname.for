*-----------------------------------------------------------------------
*+  IDNAME - Get a unique device name

      SUBROUTINE IDNAME ( DISPID, DEVNAM, NAMLEN, STATUS )

*    Description :
*     This returns a character string which uniquely defines the
*     graphics device. This consists of the concatenation of the node
*     name and the physical device name.
*
*    Invocation :
*     CALL IDNAME( DISPID, DEVNAM, NAMLEN, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     The node name and the device name are obtained from system
*     service calls.
*
*    Deficiencies :
*     Uses VAX system calls.
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     March 1989
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE '($DVIDEF)'
      INCLUDE '($SYIDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Export :
*     Device name
      CHARACTER * ( * ) DEVNAM

*     Length of name string
      INTEGER NAMLEN

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local variables :
      CHARACTER DNAME * 64, NNAME * 64

      INTEGER DLEN, ISTAT, NLEN, VAL
      INTEGER LIB$GETDVI, LIB$GETSYI
*-

*   Get the device name from the system
      ISTAT = LIB$GETDVI( DVI$_DEVNAM, ACHAN( DISPID ),,
     :                    VAL, DNAME, DLEN )
      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__DEVNM
         GOTO 99
      ENDIF

*   Get the node name from the system
      ISTAT = LIB$GETSYI( SYI$_NODENAME, VAL, NNAME, NLEN,, )
      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__DEVNM
         GOTO 99
      ENDIF

*   Make up the full name from the node and device
*   Leave off the colon on the end of the device name
      DEVNAM = NNAME( 1 : NLEN ) // DNAME( 1 : DLEN - 1 )
      NAMLEN = NLEN + DLEN - 1

  99  CONTINUE

      END

