*+  GFX_SETCOLS - set colour table
      SUBROUTINE GFX_SETCOLS(STATUS)

*    Description :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL COL(3,16)
      INTEGER BGCOL,FIRST,LAST
      INTEGER NCOL
      INTEGER I
      LOGICAL OK
      LOGICAL RGB,NEG
*    Local data :
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get colour capability of device
        CALL GDV_COLOURS(BGCOL,FIRST,LAST,STATUS)

*  is it a 3-colour table
        CALL GCB_GETL('COLOUR_RGB',OK,RGB,STATUS)
        IF (.NOT.OK) THEN
          RGB=.FALSE.
        ENDIF

*  negative image?
        CALL GCB_GETL('COLOUR_NEG',OK,NEG,STATUS)
        IF (.NOT.OK) THEN
          NEG=.FALSE.
        ENDIF

*  get colour table or use default
        IF (.NOT.RGB) THEN
          CALL GCB_GETI('COLOUR_N',OK,NCOL,STATUS)
          IF (.NOT.OK) THEN
            CALL GFX_DEFCOLS(COL,STATUS)
          ELSE
            I=1
            OK=.TRUE.
            DO WHILE (OK.AND.I.LE.NCOL)
              CALL GCB_GET1R('COLOUR_RED',I,1,OK,COL(1,I),STATUS)
              CALL GCB_GET1R('COLOUR_GREEN',I,1,OK,COL(2,I),STATUS)
              CALL GCB_GET1R('COLOUR_BLUE',I,1,OK,COL(3,I),STATUS)
              I=I+1
            ENDDO
            IF (.NOT.OK) THEN
              CALL GFX_DEFCOLS(COL,STATUS)
            ENDIF
          ENDIF
        ENDIF

        IF (RGB) THEN
          CALL GFX_SETCOLS_RGB(FIRST,LAST,NEG,STATUS)
        ELSE
          CALL GFX_SETCOLS_TABLE(COL,FIRST,LAST,NEG,STATUS)
        ENDIF

      ENDIF

      END


*+
      SUBROUTINE GFX_SETCOLS_TABLE(COL,FIRST,LAST,NEG,STATUS)

*    Description :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL COL(3,16)
      INTEGER FIRST,LAST
      LOGICAL NEG
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL RED,GREEN,BLUE
      REAL REDHUE,GREENHUE,BLUEHUE
      INTEGER NCOL,NSHADE
      INTEGER CI
      INTEGER J,K
*    Local data :
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  how many useable colours
        NCOL=LAST-FIRST+1

*  less than 8 colours - do nothing
        IF (NCOL.GE.8.AND.NCOL.LT.16) THEN
*  less than 16 colours - use every other colour
          CI=FIRST
          DO J=2,16,2
            RED=COL(1,J)
            GREEN=COL(2,J)
            BLUE=COL(3,J)
            IF (NEG) THEN
              CALL PGSCR(CI,1.0-RED,1.0-GREEN,1.0-BLUE)
            ELSE
              CALL PGSCR(CI,RED,GREEN,BLUE)
            ENDIF
            CI=CI+1
          ENDDO

*  16 colours - one to one
        ELSEIF (NCOL.EQ.16) THEN
          CI=FIRST
          DO J=1,16
            RED=COL(1,J)
            GREEN=COL(2,J)
            BLUE=COL(3,J)
            IF (NEG) THEN
              CALL PGSCR(CI,1.0-RED,1.0-GREEN,1.0-BLUE)
            ELSE
              CALL PGSCR(CI,RED,GREEN,BLUE)
            ENDIF
            CI=CI+1
          ENDDO

*  more than 16 - shade in between
        ELSEIF (NCOL.GT.16) THEN
*  how many shades between main colours
          NSHADE=(NCOL-16)/15

          CI=FIRST
*  set colour table
          DO J=1,15
            RED=COL(1,J)
            REDHUE=(COL(1,J+1)-RED)/REAL(NSHADE+1)
            GREEN=COL(2,J)
            GREENHUE=(COL(2,J+1)-GREEN)/REAL(NSHADE+1)
            BLUE=COL(3,J)
            BLUEHUE=(COL(3,J+1)-BLUE)/REAL(NSHADE+1)
            IF (NEG) THEN
              CALL PGSCR(CI,1.0-RED,1.0-GREEN,1.0-BLUE)
            ELSE
              CALL PGSCR(CI,RED,GREEN,BLUE)
            ENDIF
            DO K=1,NSHADE
              CI=CI+1
              RED=RED+REDHUE
              GREEN=GREEN+GREENHUE
              BLUE=BLUE+BLUEHUE
              IF (NEG) THEN
                CALL PGSCR(CI,1.0-RED,1.0-GREEN,1.0-BLUE)
              ELSE
                CALL PGSCR(CI,RED,GREEN,BLUE)
              ENDIF
            ENDDO
            CI=CI+1
          ENDDO
          RED=COL(1,16)
          GREEN=COL(2,16)
          BLUE=COL(3,16)
          IF (NEG) THEN
            CALL PGSCR(CI,1.0-RED,1.0-GREEN,1.0-BLUE)
          ELSE
            CALL PGSCR(CI,RED,GREEN,BLUE)
          ENDIF

        ENDIF

*  force update
        CALL PGUPDT(2)

      ENDIF
      END


*+
      SUBROUTINE GFX_SETCOLS_RGB(FIRST,LAST,NEG,STATUS)
*    Description :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*      Richard Saxton
*    History :
*     11-Nov-1992
*      6-Jan-1993      Uses the full range of colours in the table
*      3-Feb-1993      Really uses all colours (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import :
      INTEGER FIRST,LAST
      LOGICAL NEG
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL RED,GREEN,BLUE
      REAL INC
      INTEGER J
      INTEGER NCOL
*-
      IF (STATUS.NE.SAI__OK) RETURN

      NCOL=LAST-FIRST+1

*  Doesn't make much sense with less than about 64 colours
      IF (NCOL .GE. 64) THEN
*
*    Calculate the cube root of the number of available colours
         INC = NCOL ** 0.3333333
*
         DO J=FIRST,LAST
*
*    set main colours
            RED=MOD(REAL(J-FIRST),INC) / REAL(INT(INC))
            GREEN=MOD(REAL(J-FIRST),(INC*INC)) / REAL(INT(INC*INC))
            BLUE=MOD((J-FIRST),NCOL) / REAL(NCOL - 1)

            RED = MIN(RED, 1.0)
            GREEN = MIN(GREEN, 1.0)
            BLUE = MIN(BLUE, 1.0)
            IF (NEG) THEN
              CALL PGSCR(J,1.0-RED,1.0-GREEN,1.0-BLUE)
            ELSE
              CALL PGSCR(J,RED,GREEN,BLUE)
            ENDIF
         ENDDO
*
      ELSE
*
         CALL MSG_PRNT('AST_ERR: insufficient colours to '/
     &                   /'implement RGB table')
      ENDIF
*
      END
