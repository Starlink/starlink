*     06 Jan 1994 (rp):
*         make UNIX version by commenting out VMS-specific calls
*
C-----------------------------------------------------------------------

      SUBROUTINE IPUT_SCREEN (LINE,ROW,COLUMN,TYPE)

C  User wrap-up routine for VAX-VMS library routine LIB$PUT_SCREEN

      IMPLICIT  NONE

C  Formal parameters:

      CHARACTER LINE*(*)
      INTEGER*4 ROW, COLUMN
      INTEGER*4 TYPE

C  Include files:

      INCLUDE 'FLAGCOMM'

*     IF (IDEV.LT.10) THEN
*       CALL LIB$PUT_SCREEN (LINE,ROW,COLUMN,TYPE)
*     ELSE
        PRINT *,LINE
*     END IF

      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE ISET_CURSOR (ROW,COLUMN)

C  User wrap-up routine for VAX-VMS library routine LIB$SET_CURSOR

      IMPLICIT  NONE

C  Formal parameters:

      INTEGER*4 ROW, COLUMN

C  Include files:

      INCLUDE 'FLAGCOMM'

*     IF (IDEV.LT.10) CALL LIB$SET_CURSOR (ROW,COLUMN)

      RETURN
      END

C-----------------------------------------------------------------------


      SUBROUTINE IERASE_PAGE (ROW,COLUMN)

C  User wrap-up routine for VAX-VMS library routine LIB$ERASE_PAGE

      IMPLICIT  NONE

C  Formal parameters:

      INTEGER*4 ROW, COLUMN

C  Include files:

      INCLUDE 'FLAGCOMM'

*     IF (IDEV.LT.10) CALL LIB$ERASE_PAGE (ROW,COLUMN)

      RETURN
      END

C-----------------------------------------------------------------------


      SUBROUTINE IERASE_LINE (ROW,COLUMN)

C  User wrap-up routine for VAX-VMS library routine LIB$ERASE_LINE

      IMPLICIT  NONE

C  Formal parameters:

      INTEGER*4 ROW, COLUMN

C  Include files:

      INCLUDE 'FLAGCOMM'

*     IF (IDEV.LT.10) CALL LIB$ERASE_LINE (ROW,COLUMN)

      RETURN
      END

C-----------------------------------------------------------------------
