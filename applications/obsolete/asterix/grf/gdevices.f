*+  GDEVICES - open/close and give information on graphics devices
      SUBROUTINE GDEVICES(STATUS)
*    Description :
*    Parameters :
*
*    Deficiences :
*    Bugs :
*    Authors :
*	(BHVAD::RJV)
*    History :
*       20 Mar 89 : changed to incorporate opening and closing (RJV)
*       28 Jun 89 : now uses GNS to list devices (RJV)
*        4 Dec 89 : OPEN option now allows zoning (RJV)
*       19 Jan 90 : change to listing facility
*        3 May 91 : CURS option added (RJV)
*        2 Sep 92 : GDV added (RJV)
*        4 May 93 : LIST added
*       29 Nov 93 : Removed extra STATUS arguments to MSG_SET calls (DJA)
*        6 Jun 94 : Messages changed slightly (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
      INTEGER NMAX
      PARAMETER (NMAX=10)
      CHARACTER*30 VERSION
      PARAMETER (VERSION='GDEVICES Version 1.7-5')
*    Local variables :
      CHARACTER*20 DEV,OLDDEV,CONTEXT,CNTXLIST(NMAX)
      INTEGER XZONE,YZONE
      INTEGER ICONTXT,NCONTXT
      INTEGER BG,FIRST,LAST
      LOGICAL CURSOR
      LOGICAL ACTIVE
      LOGICAL OPEN,CLOSE,SHOW,ERASE,LIST
*    Functions :
*-
      OPEN=.FALSE.
      CLOSE=.FALSE.
      SHOW=.FALSE.
      ERASE=.FALSE.
      LIST=.FALSE.

      CALL MSG_PRNT(VERSION)

*  get option switches
      CALL USI_GET0L('OPEN',OPEN,STATUS)
      CALL USI_GET0L('SHOW',SHOW,STATUS)
      CALL USI_GET0L('CLOSE',CLOSE,STATUS)
      CALL USI_GET0L('ERASE',ERASE,STATUS)
      CALL USI_GET0L('LIST',LIST,STATUS)

      IF (.NOT.(OPEN.OR.SHOW.OR.CLOSE.OR.ERASE.OR.LIST)) THEN
        LIST=.TRUE.
      ENDIF

*  see if device open
      CALL GDV_STATUS(ACTIVE,STATUS)
      IF (ACTIVE) THEN
        CALL GDV_DEVICE(DEV,STATUS)
        CALL GCB_GETCONTXT(CONTEXT,STATUS)
      ENDIF

      IF (SHOW) THEN
        IF (ACTIVE) THEN
*  show device currently open
          CALL MSG_BLNK()
          CALL MSG_SETC('DEV',DEV)
          CALL MSG_PRNT('Current device is ^DEV')
          CALL MSG_BLNK()
          IF (CONTEXT.EQ.' ') THEN
            CONTEXT='none'
          ENDIF
          CALL MSG_PRNT('Active context is : '//CONTEXT)
          CALL GCB_LSTCONTXT(NMAX,CNTXLIST,NCONTXT,STATUS)
          IF (NCONTXT.GE.1) THEN
            CALL MSG_PRNT('Cached contexts are:')
            DO ICONTXT=1,NCONTXT
              CALL MSG_PRNT('                     '//CNTXLIST(ICONTXT))
            ENDDO
          ENDIF
          CALL MSG_BLNK()
          CALL GDV_COLOURS(BG,FIRST,LAST,STATUS)
          IF (FIRST.EQ.0.AND.LAST.EQ.0) THEN
            CALL MSG_SETI('C1',BG)
            CALL MSG_SETI('C2',1)
            CALL MSG_PRNT('Line  colour index range: ^C1 to ^C2')
            CALL MSG_PRNT('Pixel plotting not available')
          ELSE
            CALL MSG_SETI('C1',BG)
            CALL MSG_SETI('C2',FIRST-1)
            CALL MSG_PRNT('Line  colour index range: ^C1 to ^C2')
            CALL MSG_SETI('C1',FIRST)
            CALL MSG_SETI('C2',LAST)
            CALL MSG_PRNT('Pixel colour index range: ^C1 to ^C2')
          ENDIF
          CALL MSG_BLNK()
          CALL GDV_CURSOR(CURSOR,STATUS)
          IF (CURSOR) THEN
            CALL MSG_PRNT('Cursor available')
          ELSE
            CALL MSG_PRNT('No cursor available')
          ENDIF
        ELSE
          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('** no device currently active **')
        ENDIF
      ENDIF

      IF (LIST) THEN
        CALL MSG_BLNK()
        CALL GDV_LIST(STATUS)
        CALL MSG_BLNK()
      ENDIF

      IF (CLOSE) THEN
        IF (ACTIVE) THEN
          CALL GDV_CLOSE(STATUS)
          CALL MSG_PRNT(' ')
          CALL MSG_SETC('DEV',DEV)
          CALL MSG_PRNT('Device ^DEV now closed')
          ACTIVE=.FALSE.
        ELSE
          CALL MSG_PRNT('AST_ERR: no device open')
        ENDIF
      ENDIF

      IF (OPEN) THEN
*  get device name
        CALL USI_GET0C('DEV',DEV,STATUS)
        IF (ACTIVE.AND.STATUS.EQ.SAI__OK) THEN
*  close existing device
          CALL GDV_DEVICE(OLDDEV,STATUS)
          CALL MSG_PRNT(' ')
          CALL MSG_SETC('DEV',OLDDEV)
          CALL MSG_PRNT('Closing ^DEV...')
          CALL GDV_CLOSE(STATUS)
          ACTIVE=.FALSE.
        ENDIF
*  get hard zones
        CALL USI_GET0I('XZONE',XZONE,STATUS)
        CALL USI_GET0I('YZONE',YZONE,STATUS)
        CALL GDV_OPEN(DEV,XZONE,YZONE,STATUS)
        IF (STATUS.EQ.SAI__OK) THEN
          CALL MSG_SETC('DEV',DEV)
          CALL MSG_PRNT('Device ^DEV now open')
        ENDIF
      ENDIF

      IF (ERASE) THEN
        CALL GDV_CLEAR(STATUS)
      ENDIF

      END
