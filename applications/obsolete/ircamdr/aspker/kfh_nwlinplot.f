
*+  KFH_NWLINPLOT - Plots a slice on the specified graphics device.
      SUBROUTINE KFH_NWLINPLOT(LINE,NPTS,XC1,YC1,XC2,YC2,A,B,STATUS)
*    Description :
*     This routine produces a plot of the slice
*     produced by KFH_NWSLICE.
*    Invocation :
*     CALL KFH_NWSLICE(LINE,NPTS,XC1,YC1,XC2,YC2,A,B,STATUS)
*    Parameters :
*     LINE(0:NPTS-1) = REAL
*           The array which holds the data of the
*           slice.
*     NPTS = INTEGER
*           The number of points in the slice.
*     XC1 = REAL
*           The X-coordinate of the first point of
*           the slice.
*     YC1 = REAL
*           The Y-coordinate of the first point of
*           the slice.
*     XC2 = REAL
*           The X-coordinate of the second point of
*           the slice.
*     YC2 = REAL
*           The Y-coordinate of the second point of
*           the slice.
*     A = REAL
*           The value of the previous X-position of
*           the cursor.
*     B = REAL
*           The value of the previous Y-position of
*           the cursor.
*     STATUS = INTEGER
*           The status value on entering this subroutine.
*    Method :
*     This subroutine first takes the slice data
*     previously determined by KFH_NWSLICE and
*     transforms the data into a form that will
*     produce a plot along the axis of the slice.
*     The transformation is a trigonometric one
*     which determines the world coordinates of
*     each point in the slice. The slice is then
*     plotted using SGS.
*    Authors :
*     S.Chan
*    History :
*     26 September 1983
*    Type Definition :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL A                             ! The X-position of the previous
*                                        ! cursor reading.
      REAL B                             ! The Y-position of the previous
*                                        ! cursor reading.
      INTEGER NPTS                       ! The number of points which
*                                        ! make up the slice.
      INTEGER I                          ! General variable.
      REAL LENGTH                        ! The length of the slice.
      REAL LINE(0:NPTS-1)                ! The array containing the slice
*                                        ! data.
      REAL MAX                           ! Maximum value in LINE.
      INTEGER OLDPEN                     ! Pen number.
      REAL SX                            ! Variable in the transformation.
      REAL SY                            ! Variable in the transformation.
      REAL THETA                         ! The angle between the lower
*                                        ! end of the slice and the
*                                        ! horizontal.
      REAL WC(0:1023,2)                  ! The array holding the world
*                                        ! coordinates of the plot.
      REAL XDIFF                         ! The difference between the
*                                        ! X-coordinates.
      REAL YDIFF                         ! The difference between the
*                                        ! Y-coordinates.
      REAL XC1                           ! The X-coordinate of the first
*                                        ! point of the slice.
      REAL XC2                           ! The X-coordinate of the second
*                                        ! point of the slice.
      REAL YC1                           ! The Y-coordinate of the first
*                                        ! point of the slice.
      REAL YC2                           ! The Y-coordinate of the second
*                                        ! point of the slice.
*-

*
*    If the status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Calculation of angled slice.
*

         XDIFF = XC2-XC1
         YDIFF = YC2-YC1
         LENGTH = SQRT((XDIFF*XDIFF)+(YDIFF*YDIFF))
         THETA = -ATAN2(YDIFF,XDIFF)

*
*       Search for the maximum value in LINE.
*

         MAX = LINE(0)

         DO I = 0,NPTS-1

            IF (LINE(I).GT.MAX) THEN

               MAX = LINE(I)

            ENDIF

         END DO

*
*       Convert interpolated values into world coordinates
*       ready for plotting in SGS.
*

         DO I = 0,NPTS-1

            SX = (REAL(I+1)/REAL(NPTS))*LENGTH
            SY = (LINE(I)/MAX)*(LENGTH/3)
            WC(I,1) = XC1+SX*COS(THETA)+SY*SIN(THETA)
            WC(I,2) = YC1+SY*COS(THETA)-SX*SIN(THETA)

         END DO

*
*       Store old pen.
*

         CALL SGS_IPEN(OLDPEN)

*
*       Clear the workstation.
*

         CALL ARGS_S1('ZDI1',0)
         CALL SGS_LINE(XC1,YC1,A,B)
         CALL SGS_FLUSH

*
*       Draw a line joining the points of the slice.
*

         CALL SGS_SPEN(3)
         CALL SGS_LINE(XC1,YC1,XC2,YC2)
         CALL SGS_FLUSH

*
*       Plot the data.
*

         CALL SGS_SPEN(2)
         CALL SGS_BPOLY(XC1,YC1)

         DO I = 0,NPTS-1

            CALL SGS_APOLY(WC(I,1),WC(I,2))

         END DO

         CALL SGS_APOLY(XC2,YC2)
         CALL SGS_FLUSH

*
*       Reset old pen.
*

         CALL SGS_SPEN(OLDPEN)

      ENDIF

      END
