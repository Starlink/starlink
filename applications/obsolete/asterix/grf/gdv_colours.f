*+  GDV_COLOURS - what colour capability?
      SUBROUTINE GDV_COLOURS(BG,FIRST,LAST,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'GDV_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      INTEGER BG		! 0 = can draw in b/g col  1 = can't
      INTEGER FIRST		! first colour for pixel plots
      INTEGER LAST		! last colour available
*    Status :			! BG to FIRST-1 fixed for line plotting
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      LOGICAL OK
      INTEGER MAXCOL(18),FRST(18),ICOL
      DATA MAXCOL/0,9,20,38,53,68,83,98,113,128,143,158,173,188,
     :                                          203,218,233,248/
      DATA FRST/0,2,5,15*8/
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GDV_STATUS(OK,STATUS)

        IF (OK) THEN

          LAST=G_NCOL
          BG=G_BGCOL
          ICOL=18
          DO WHILE (LAST.LT.MAXCOL(ICOL))
            ICOL=ICOL-1
          ENDDO
          LAST=MAXCOL(ICOL)
          FIRST=FRST(ICOL)

        ELSE
          CALL MSG_PRNT('AST_ERR: no graphics device active')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_COLOURS',STATUS)
        ENDIF

      ENDIF

      END
