*+  IPLOT - display current 1D plot
      SUBROUTINE IPLOT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      20 Jan 93 : V1.7-0 uses GCB etc (RJV)
*       1 Jul 94 : V1.7-1 more intelligent screen clearing (RJV)
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
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IPLOT Version 1.7-1')
*-
      CALL MSG_PRNT(VERSION)


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (I_N_1D.EQ.0) THEN
        CALL MSG_PRNT('AST_ERR: no 1D data to plot')
      ELSE



*  clear screen unless device freshly opened
        CALL GDV_FRESH(FRESH,STATUS)
        IF (.NOT.FRESH) THEN
          CALL GDV_CLEAR(STATUS)
        ENDIF

*  select appropriate GCB and display plot
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_1DGCB(STATUS)

        CALL IMG_PLOT(STATUS)


*  flag current plotting status
        I_DISP_1D=.TRUE.
        I_DISP=.FALSE.
        I_CLEAR=.FALSE.

      ENDIF

      END
