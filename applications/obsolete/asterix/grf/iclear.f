*+  ICLEAR - clear current plotting zone
      SUBROUTINE ICLEAR(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*       1 Feb 93 : used GDV and PGPIXL (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER BGCOL,FIRST,LAST,NCOL
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICLEAR Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF( .NOT.(I_DISP.OR.I_DISP_1D)) THEN
        CALL MSG_PRNT('AST_ERR: no current display to clear')
      ELSE

        CALL GDV_COLOURS(BGCOL,FIRST,LAST,STATUS)
        NCOL=LAST-FIRST+1
        IF (BGCOL.EQ.0.AND.NCOL.GE.8) THEN
          CALL PGVPORT(0.0,1.0,0.0,1.0)
          CALL PGWINDOW(0.0,1.0,0.0,1.0)
          CALL PGPIXL(BGCOL,1,1,1,1,1,1,0.0,1.0,0.0,1.0)
        ELSE
          CALL GDV_CLEAR(STATUS)
        ENDIF
        I_CLEAR=.TRUE.
        I_DISP=.FALSE.
        I_DISP_1D=.FALSE.

      ENDIF

      END
