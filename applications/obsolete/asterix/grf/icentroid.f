*+  ICENTROID - get centroid of circular region of the plot
      SUBROUTINE ICENTROID(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*        9 JAN 91: V1.3-1 correction to bug that always took
*                  current pos as centre of circle (RJV)
*        1 Jul 93: V1.3-2 GTR used (RJV)
*       23 Sep 94: V1.7-0 (RJV)
*       18 Jan 96: V1.8-0 use IMG_GETCIRC (rjv)
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
      CHARACTER*1 CH
      REAL XC,YC
      REAL XR,YR,RAD
      REAL XCENT,YCENT
      INTEGER I,ITER
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICENTROID Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations are correct
        CALL GTR_RESTORE(STATUS)

        CALL IMG_GETCIRC('X','Y',RAD',XC,YC,RAD,STATUS)

*  plot initial circle
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)

*  get number of iterations
        CALL USI_GET0I('ITER',ITER,STATUS)

        DO I=1,ITER


          CALL ICENTROID_ITER(%VAL(I_DPTR),%VAL(I_QPTR),XC,YC,RAD,
     :                                         XCENT,YCENT,STATUS)

          XC=XCENT
          YC=YCENT

        ENDDO

*  set current pos to centroid
        CALL IMG_SETPOS(XCENT,YCENT,STATUS)

*  output centoid position
        CALL USI_PUT0R('XCENT',XCENT,STATUS)
        CALL USI_PUT0R('YCENT',YCENT,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END




      SUBROUTINE ICENTROID_ITER(ARRAY,QUAL,XC,YC,R,XCENT,YCENT,
     :                                                  STATUS)

*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL ARRAY(I_NX,I_NY)                ! Data array
      BYTE QUAL(I_NX,I_NY)                 ! Quality array
      REAL XC,YC,R
*    Import-Export :
*    Export :
      REAL XCENT,YCENT
*    Status :
      INTEGER STATUS
*    Functions :
      LOGICAL IMG_INCIRC
      BYTE BIT_ANDUB
*    Local variables :
      REAL TOTAL
      REAL XPIX,YPIX
      INTEGER IX,IY
      INTEGER I1,I2,J1,J2
      INTEGER NGOOD
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Initialise:
      TOTAL=0.0
      NGOOD=0
      XPIX=0.0
      YPIX=0.0


* Loop over all pixels within rectangle enclosing circle
      CALL IMG_CIRCTOBOX(XC,YC,R,I1,I2,J1,J2,STATUS)
      DO IY=J1,J2
        DO IX=I1,I2

* check pixel is within circle
          IF (IMG_INCIRC(IX,IY,XC,YC,R)) THEN

*   Test pixel quality
            IF (I_BAD) THEN
              GOOD=(BIT_ANDUB(QUAL(IX,IY),I_MASK).EQ.QUAL__GOOD)
            ELSE
              GOOD=.TRUE.
            ENDIF

            IF (GOOD) THEN

               TOTAL=TOTAL+ARRAY(IX,IY)
               XPIX=XPIX+REAL(IX)*ARRAY(IX,IY)
               YPIX=YPIX+REAL(IY)*ARRAY(IX,IY)

               NGOOD=NGOOD+1

            ENDIF

          ENDIF

        ENDDO

      ENDDO

      IF (NGOOD .GT. 0) THEN
         XPIX=XPIX/TOTAL
         YPIX=YPIX/TOTAL
         CALL IMG_PIXTOWORLD(XPIX,YPIX,XCENT,YCENT,STATUS)
      ENDIF


      END
