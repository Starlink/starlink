*-----------------------------------------------------------------------
*+  IKNIBW - Input data in wordss from Ikon buffer

      SUBROUTINE IKNIBW ( DISPID, NUMWOR, WORBUF, STATUS )

*    Description :
*     This reads in a number of words from the Ikon.
*
*    Invocation :
*     CALL IKNIBW( DISPID, NUMWOR, WORBUF, STATUS )
*
*    Method :
*     Read in the words using SYS$QIOW.
*
*    Deficiencies :
*     The event flag number has to have the same value as in IKNOUT
*
*    Bugs :
*     Very non-stndard Fortran - INTEGER * 2
*     Uses VAX system calls - SYS$QIOW, SYS$SYNCH
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     March 1990     Check output QIO has finished
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Import-Export :
*     Import : Number of words to read
*     Export : number of words read
      INTEGER NUMWOR

*    Export :
*     Output word buffer
      INTEGER * 2 WORBUF ( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMBUF)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local Constants :
      INTEGER * 2 FUNC
      PARAMETER ( FUNC = IO$_READVBLK .OR. IO$M_SETFNCT .OR.
     :                   IO$M_TIMED )

      INTEGER FNCT, TIMOUT
      PARAMETER ( FNCT = 1 )
      PARAMETER ( TIMOUT = 10 )

*    Local variables :
      INTEGER ISTAT, PAD
*-

*   If there is a current QIO then check it has completed
      IF ( LQIO ) THEN
         ISTAT = SYS$SYNCH( %VAL( NEVF ), IOSB )
         IF ( ( ISTAT .NE. SS$_NORMAL ) .OR.
     :        ( IOSB( 1 ) .NE. SS$_NORMAL ) ) THEN
            STATUS = IDI__IOERR
            GOTO 99
         ENDIF
         LQIO = .FALSE.
      ENDIF

*   Read the buffer from the given channel
      ISTAT = SYS$QIOW( ,%VAL( ACHAN( DISPID ) ), %VAL( FUNC ), IOSB,,,
     :                  WORBUF, %VAL( NUMWOR * 2 ),
     :                  %VAL( TIMOUT ), %VAL( FNCT ),, )

      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__IOERR
      ENDIF

*   Return the number of bytes read not counting any padding
      IF ( IOSB( 2 ) .LT. NUMWOR * 2 ) THEN
         NUMWOR = IOSB( 2 ) / 2
      ENDIF

  99  CONTINUE

      END

