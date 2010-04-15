*+  KFH_FILL - Routine to fit a patch to an image.
      SUBROUTINE KFH_FILL(CURX,CURY,BOXHT,BOXWDT,RADIUS,CURTYP,
     : SCRTCH,IMAGE,XDIM,YDIM,IMHI,IMLO,WKID1,WKID15,STATUS)
*    Description :
*     This routine accepts the position and size of a circular
*     or rectangular cursor, and produces a patch to fill the
*     cursor. When the patch has been produced, the result is
*     displayed to the user who can either accept or reject the
*     patch.
*    Invocation :
*     CALL KFH_FILL(CURX,CURY,BOXHT,BOXWDT,RADIUS,CURTYP,SCRTCH,
*      IMAGE,XDIM,YDIM,IMHI,IMLO,WKID1,WKID15,STATUS)
*    Parameters :
*     CURX = REAL
*           The X coordinate of the centre of the cursor.
*     CURY = REAL
*           The Y coordinate of the centre of the cursor.
*     BOXHT = INTEGER
*           The height of the rectangular cursor.
*     BOXWDT = INTEGER
*           The width of the rectangular cursor.
*     RADIUS = INTEGER
*           The radius of the circular cursor.
*     CURTYP = CHAR*1
*           The type of cursor.
*     SCRTCH(XDIM,YDIM) = INTEGER
*           Scratch array containing the scaled image.
*     IMAGE(XDIM,YDIM) = REAL
*           The image array.
*     XDIM = INTEGER
*           The X dimension of the image.
*     YDIM = INTEGER
*           The Y dimension of the image.
*     IMHI = REAL
*           The upper limit used for the image scaling.
*     IMLO = REAL
*           The lower limit used for the image scaling.
*     WKID1 = INTEGER
*           The work station identifier of the ARGs.
*     WKID15 = INTEGER
*           The work station identifier of the overlay planes.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     The region containing the patch is defined. The fit is
*     implemented by taking each pixel in the region and form-
*     the sum and the weighted sum from the pixels in the same
*     row and column. The new value of the pixel is calculated
*     by dividing the sum by the weighted sum. This process is
*     repeated for all the elements in the patch. The whole
*     fit is carried out again depending on the number of
*     iterations that the user requires (using the new values
*     formed by the previous fit, so that successive iterations
*     are improvements on the last).
*     The patch is scaled and displayed on the ARGs for the user
*     to see. If he does not want to retain the patch he can
*     reject it when the original data is replaced, but if he
*     accepts the patch then the calculated data is transferred
*     to the output image.
*    Authors :
*     Based on the old program ADISP.
*     K.F.Hartley (RGVAD::KFH)
*     S.Chan (RGVAD::KFH)
*    History:
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Status :
      INTEGER STATUS
*    Local constants :
      INTEGER MXDIMX
      PARAMETER (MXDIMX=512)
      INTEGER MXDIMY
      PARAMETER (MXDIMY=512)
*    Local variables :
      INTEGER NELEMENTS            ! number of elements mapped by NDF_MAP
      INTEGER XDIM                       ! The X-dimension of the image.
      INTEGER YDIM                       ! The Y-dimension of the image.
      REAL A(512,512)                    ! Array to hold patch data.
      REAL B(512,512)                    ! Array to hold patch data.
      INTEGER BOXHT                      ! The height of the rectangular
*                                        ! cursor.
      INTEGER BOXWDT                     ! The width of the rectangular
*                                        ! cursor.
      INTEGER BUTTON                     ! The button pressed by the user.
      CHARACTER*1 CURTYP                 ! The type of cursor.
      REAL CURX                          ! The X-coordinate of the centre
*                                        ! of the cursor.
      REAL CURY                          ! The Y-coordinate of the centre
*                                        ! of the cursor.
      REAL DIFF                          ! The difference between the old
*                                        ! value of the data element and
*                                        ! the new fitted value.
      INTEGER PLACE                      ! temporary placeholder
      INTEGER DIMS(2)                    ! The dimensions of the patch.
      INTEGER LBND(2)                    ! dimensions lower bounds
      DATA LBND / 1, 1 /

      REAL E(0:512)                      ! The array containing the
*                                        ! weights.
      INTEGER I                          ! General variable.
      INTEGER I1                         ! General variable.
      INTEGER I2                         ! General variable.
      REAL IMAGE(XDIM,YDIM)              ! The raw image data.
      REAL IMHI                          ! The upper limit used for
*                                        ! scaling the image.
      REAL IMLO                          ! The lower limit used for
*                                        ! scaling the image.
      INTEGER ITER                       ! General variable.
      INTEGER J                          ! General variable.
      INTEGER J1                         ! General variable.
      INTEGER J2                         ! General variable.
      REAL LASTMX                        ! The last maximum difference.
      INTEGER M                          ! General variable.
      REAL MAXDIF                        ! The maximum difference.
      INTEGER MAXEXP                     ! The maximum number of weights
*                                        ! required.
      INTEGER NITER                      ! The number of iterations.
      INTEGER P                          ! General variable.
      INTEGER Q                          ! General variable.
      INTEGER RADIUS                     ! The radius of the circular
*                                        ! cursor.
      LOGICAL REPEAT                     ! A flag used to simulate a
*                                        ! REPEAT..UNTIL loop using a
*                                        ! DO..WHILE loop.
      INTEGER RXDIM                      ! X-dimension of the patch.
      INTEGER RYDIM                      ! Y-dimension of the patch.
      INTEGER SCRTCH(XDIM,YDIM)          ! Scratch array holding scaled
*                                        ! image.
      REAL SIZE                          ! Scale size.
      REAL SUM                           ! Sum of the elements on the
*                                        ! same row and column as the
*                                        ! element to be replaced.
      INTEGER  TMPLOC                    ! Locator to the temporary
*                                        ! area.
      INTEGER TX1                        ! General variable.
      INTEGER TX2                        ! General variable.
      INTEGER TY1                        ! General variable.
      INTEGER TY2                        ! General variable.
      INTEGER TMPPNT                     ! Pointer to temporary area.
      REAL WSUM                          ! Weighted sum of the elements
*                                        ! on the same row and column as
*                                        ! the element to be replaced.
      INTEGER WKID1                      ! Work station identifier of
*                                        ! the ARGs.
      INTEGER WKID15                     ! Work station identifier of
*                                        ! the overlay planes.
      REAL XARG                          ! General variable.
      INTEGER XCENTR                     ! Variable used in the fit
*                                        ! for the circular cursor.
      INTEGER XHI                        ! The X-coordinate of the
*                                        ! right hand side of the
*                                        ! region to be patched.
      REAL XLIM                          ! Variable used in circular
*                                        ! fit.
      INTEGER XLIMIT                     ! Variable used in circular
*                                        ! fit.
      INTEGER XLO                        ! The X-coordinate of the
*                                        ! left hand side of the
*                                        ! region to be patched.
      INTEGER XPOS                       ! The X-coordinate of the
*                                        ! centre of the region in the
*                                        ! data array which is to be
*                                        ! patched.
      REAL YARG                          ! Variable used in the circular
*                                        ! fit.
      INTEGER YCENTR                     ! Variable used in the circular
*                                        ! fit.
      INTEGER YHI                        ! The Y-coordinate of the
*                                        ! top of the region to be
*                                        ! patched.
      REAL YLIM                          ! Variable used in the circular
*                                        ! fit.
      INTEGER YLIMIT                     ! Variable used in the circular
*                                        ! fit.
      INTEGER YLO                        ! The Y-coordinate of the bottom
*                                        ! of the region to be patched.
      INTEGER YPOS                       ! The Y-coordinate of the centre
*                                        ! of the region in the data array
*                                        ! which is to be patched.
*-

*
*    If the status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Find the position of the centre of the cursor
*       in the image data.
*

         XPOS = NINT(CURX-255.0+0.5*REAL(XDIM))
         YPOS = NINT(CURY-255.0+0.5*REAL(YDIM))

*
*       If any part of the circular cursor lies outside
*       the image, then warn the user.
*

         IF (CURTYP.EQ.'C') THEN

            TX1 = XPOS+RADIUS
            TX2 = XPOS-RADIUS
            TY1 = YPOS+RADIUS
            TY2 = YPOS-RADIUS

            IF (TX1.GT.(XDIM-1).OR.TX2.LT.2.OR.TY1.GT.(YDIM-1).OR.
     :          TY2.LT.2) THEN

               CALL MSG_OUT('CIRCOUT','PART OF CURSOR OUTSIDE IMAGE. '/
     :           /'CANNOT DO A FIT.',STATUS)
               RETURN

            ENDIF

         ENDIF

*
*       Define the extremities of the region used for
*       the data values for the fit.
*

         IF (CURTYP.EQ.'C') THEN

            XLO = MAX(XPOS-RADIUS,2)
            YLO = MAX(YPOS-RADIUS,2)
            XHI = MIN(XPOS+RADIUS,XDIM-1)
            YHI = MIN(YPOS+RADIUS,YDIM-1)

         ELSE

            XLO = MAX(XPOS-BOXWDT,2)
            YLO = MAX(YPOS-BOXHT,2)
            XHI = MIN(XPOS+BOXWDT,XDIM-1)
            YHI = MIN(YPOS+BOXHT,YDIM-1)

         ENDIF

*
*       If the cursor lies outside the image then warn
*       the user.
*

         IF (XLO.GT.XDIM.OR.XHI.LT.1.OR.YLO.GT.YDIM.OR.
     :       YHI.LT.1) THEN

            CALL MSG_OUT('OUTSIDE','CURSOR IS OUTSIDE THE IMAGE.'/
     :       /' CANNOT DO A FIT.',STATUS)

            RETURN

         ENDIF

*
*       Create a copy of region concerned.
*

         RXDIM = XHI-XLO+1
         RYDIM = YHI-YLO+1

         CALL KFH_RGCOPY(IMAGE,A,XDIM,YDIM,RXDIM+2,RYDIM+2,
     :    XLO,XHI,YLO,YHI)

*
*       Create an empty array to hold the results of the
*       consecutive iterations of the fitting algorithm.
*

         CALL KFH_ARRCRE(RXDIM+2,RYDIM+2,B,A)

*
*       Set the size for iteration.
*

         SIZE = 5
*         SIZE = 0.5*MAX(RXDIM,RYDIM)

*
*       Do the fit.
*

         LASTMX = 0

         CALL PAR_GET0I('NOITER',NITER,STATUS)
         CALL PAR_CANCL('NOITER',STATUS)

         DO ITER = 1,NITER

*
*          Pre-calculate the exponentials required for
*          the fit.
*

            MAXEXP = MAX(RXDIM+2,RYDIM+2)
            E(0) = 1.0
            E(1) = EXP(-1.0/SIZE)
            E(2) = E(1)*E(1)

            DO M = 3,MAXEXP

               E(M) = E(M-1)*E(1)

            END DO

*
*          Calculate the new values of the image.
*

            MAXDIF = 0

            DO J = 2,RYDIM+1

               IF (CURTYP.EQ.'R') THEN

                  DO I = 2,RXDIM+1

                     SUM = 0.0
                     WSUM = 0.0

                     DO I1 = 1,RXDIM+2

                        IF (I1.NE.I) THEN

                           SUM = SUM+E((ABS(I-I1)-1))*A(I1,J)
                           WSUM = WSUM+E((ABS(I-I1)-1))

                        ENDIF

                     END DO

                     DO J1 = 1,RYDIM+2

                        IF (J1.NE.J) THEN

                           SUM = SUM+E((ABS(J-J1)-1))*A(I,J1)
                           WSUM = WSUM+E((ABS(J-J1)-1))

                        ENDIF

                     END DO

                     B(I,J) = SUM/WSUM
                     DIFF = ABS(B(I,J)-A(I,J))

                     IF (DIFF.GE.MAXDIF) THEN

                        MAXDIF = DIFF

                     ENDIF

                  END DO

               ELSEIF (CURTYP.EQ.'C') THEN

                  XCENTR = ((RXDIM+2)-1)/2+1
                  YCENTR = ((RYDIM+2)-1)/2+1

                  XARG = REAL(RADIUS*RADIUS-(YCENTR-J)*(YCENTR-J))
                  XLIM = SQRT(XARG)
                  XLIMIT = INT(XLIM+0.5)

                  DO I = XCENTR-XLIMIT,XCENTR+XLIMIT

                     SUM = 0.0
                     WSUM = 0.0

                     DO I1 = XCENTR-XLIMIT-1,XCENTR+XLIMIT+1

                        IF (I1.NE.I) THEN

                           SUM = SUM+E((ABS(I-I1)-1))*A(I1,J)
                           WSUM = WSUM+E((ABS(I-I1)-1))

                        ENDIF

                     END DO

                     YARG = REAL(RADIUS*RADIUS-(XCENTR-I)*(XCENTR-I))
                     YLIM = SQRT(YARG)
                     YLIMIT = INT(YLIM+0.5)

                     DO J1 = YCENTR-YLIMIT-1,YCENTR+YLIMIT+1

                        IF (J1.NE.J) THEN

                           SUM = SUM+E((ABS(J-J1)-1))*A(I,J1)
                           WSUM = WSUM+E((ABS(J-J1)-1))

                        ENDIF

                     END DO

                     B(I,J) = SUM/WSUM
                     DIFF = ABS(B(I,J)-A(I,J))

                     IF (DIFF.GE.MAXDIF) THEN

                        MAXDIF = DIFF

                     ENDIF

                  END DO

               ENDIF

            END DO

*
*          Transfer iterated values into A.
*

            DO J2 = 2,RYDIM+1

               DO I2 = 2,RXDIM+1

                  A(I2,J2) = B(I2,J2)

               END DO

            END DO

*
*          If the maximum difference is less than 0.25 of
*          the maximum difference so far , reduce the scale
*          size by a factor of 2 and reset the maximum
*          difference.
*

            IF (ITER.EQ.1) THEN

               LASTMX = MAXDIF

            ELSE IF (MAXDIF*4.0.LE.LASTMX) THEN

               SIZE = SIZE*0.5
               LASTMX = MAXDIF

            ENDIF

         END DO

*
*       Create temporary space for the patch.
*

         DIMS(1) = XHI-XLO+1
         DIMS(2) = YHI-YLO+1

         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW('_INTEGER', 2, LBND, DIMS, PLACE, TMPLOC,
     :                STATUS)

*
*       Map in the space.
*
         CALL NDF_MAP( TMPLOC, 'DATA', '_INTEGER', 'WRITE',
     :                  TMPPNT, NELEMENTS, STATUS )

*
*       Map it in the space.
*

         CALL KFH_PSCALE(%VAL(TMPPNT),DIMS(1),DIMS(2),SCRTCH,XDIM,
     :    YDIM,A,DIMS(1)+2,DIMS(2)+2,XLO,YLO,IMLO,IMHI)

*
*       Set the ARGs for the image to be displayed.
*

         CALL GKS_DAWK(WKID15)
         CALL GKS_ACWK(WKID1)

*
*       Display the patch.
*

         CALL GKS_PXA(255.0-0.5*REAL(XDIM)+REAL(XLO),
     :                255.0-0.5*REAL(YDIM)+REAL(YLO),
     :                255.0-0.5*REAL(XDIM)+REAL(XHI),
     :                255.0-0.5*REAL(YDIM)+REAL(YHI),
     :                DIMS(1),DIMS(2),%VAL(TMPPNT))

*
*       Set the ARGs back to the overlay planes.
*

         CALL GKS_DAWK(WKID1)
         CALL GKS_ACWK(WKID15)

*
*       Display the button labels.
*

         CALL KFH_BTTNS('ACCEPT',' ',' ','REJECT','*','*',.FALSE.,
     :    STATUS)

*
*       Get the user's answer.
*

         REPEAT = .TRUE.

         DO WHILE (REPEAT)

*
*          See what button the user has pressed.
*

            CALL KFH_BUTVL(BUTTON,STATUS)

*
*          If the red button is pressed then reject the
*          patch.
*

            IF (BUTTON.EQ.4) THEN

*
*             Replace the patch with the original data.
*

               CALL KFH_REPLC(%VAL(TMPPNT),DIMS(1),DIMS(2),SCRTCH,
     :          XDIM,YDIM,XLO,YLO)

               CALL GKS_DAWK(WKID15)
               CALL GKS_ACWK(WKID1)

               CALL GKS_PXA(255.0-0.5*REAL(XDIM)+REAL(XLO),
     :                      255.0-0.5*REAL(YDIM)+REAL(YLO),
     :                      255.0-0.5*REAL(XDIM)+REAL(XHI),
     :                      255.0-0.5*REAL(YDIM)+REAL(YHI),
     :                      DIMS(1),DIMS(2),%VAL(TMPPNT))

*
*             Reset the device to the overlay planes.
*

               CALL GKS_DAWK(WKID1)
               CALL GKS_ACWK(WKID15)

*
*             Leave the loop.
*

               REPEAT = .FALSE.

*
*          If the green button is pressed , then accept the
*          patch.
*

            ELSE IF(BUTTON.EQ.1) THEN

               DO J = 2,RYDIM+1

                  DO I = 2,RXDIM+1

                     P = XLO-2+I
                     Q = YLO-2+J
                     IMAGE(P,Q) = A(I,J)
                     SCRTCH(XLO-2+I,YDIM-YLO-J+3) =
     :                NINT((A(I,J)-IMLO)/(IMHI-IMLO)*255.0)

                  END DO

               END DO

*
*             Leave the loop.
*

               REPEAT = .FALSE.

            ENDIF

         END DO

*
*       Close temporary data area.
*

         CALL NDF_ANNUL(TMPLOC,STATUS)

      ENDIF

      END
