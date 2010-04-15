      SUBROUTINE CAP_PLYSL (CI, XCOLI, YCOLI, ROWS, CORNER, XCORNR,
     :  YCORNR, NUMSEL, SELIST, STATUS)
*+
*  Name:
*     CAP_PLYSL
*  Purpose:
*     Generate a list of points which lie inside a polygon.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLYSL (CI, XCOLI, YCOLI, ROWS, CORNER, XCORNR, YCORNR;
*       NUMSEL, SELIST; STATUS)
*  Description:
*     Generate a list of points which lie inside a polygon.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the input catalogue from which the selection is
*        to be generated.
*     XCOLI  =  INTEGER (Given)
*        Identifier for the X coordinate column in the input catalogue.
*     YCOLI  =  INTEGER (Given)
*        Identifier for the Y coordinate column in the input catalogue.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the input catalogue.
*     CORNER  =  INTEGER (Given)
*        Number of corners (or vertices) in the polygon catalogue.
*     XCORNR(CORNER)  =  REAL (Given)
*        X coordinates of the polygon corners.
*     YCORNR(CORNER)  =  REAL (Given)
*        Y coordinates of the polygon corners.
*     NUMSEL  =  INTEGER (Returned)
*        Number of objects lying inside the polygon.
*     SELIST(ROWS)  =  INTEGER (Work)
*        List of row numbers for the objects lying inside the polygon.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the corners of the bounding box which just encloses
*     the polygon.
*     Compute the position of a fiducial point outside the polygon.
*     Determine whether the catalogue is sorted on either the X or Y
*     column.
*     If the catalogue is so sorted then
*       Get the range of rows corresponding to the appropriate sides
*       of the bounding box.
*     else
*       Set the range of rows to be all the rows in the catalogue.
*     end if.
*     If ok then
*       If the chosen range includes any rows then
*         For each row
*           Read the XY coordinates for the current row from the input
*           catalogue.
*           If ok and no null values were encountered then
*             Determine whether the point lies inside the polygon.
*             If so then
*               Increment the number of selected rows.
*               Add the row to the list of rows inside the polygon.
*             end if
*           end if
*         end for
*         If no rows were found inside the polygon then
*           Set the status.
*           Report an error: no rows selected.
*         end if
*       else
*         Set the status.
*         Report an error: selected range contains no rows.
*       end if
*     end if
*     If the status is not ok then
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     17/6/96  (ACD): Original version.
*     24/10/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  XCOLI,
     :  YCOLI,
     :  ROWS,
     :  CORNER
      REAL
     :  XCORNR(CORNER),
     :  YCORNR(CORNER)
*  Arguments Returned:
      INTEGER
     :  NUMSEL,
     :  SELIST(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CURCOR, ! Current corner in the polygon.
     :  CROSS,  ! No. of crossings from fiducial point to current data point.
     :  STARTR, ! First row which may be inside the polygon.
     :  STOPR,  ! Last row which may be inside the polygon.
     :  ORDER,  ! Order of the current column.
     :  SCOLI,  ! Identifier to sorted column chosen for range selection.
     :  ROW     ! Current row in the input catalogue.
      LOGICAL
     :  NULFLX, ! Flag; null value read for X coordinate?
     :  NULFLY, !  "  ;  "     "    "    "  Y     "     ?
     :  SORT    ! Flag; is the current column sorted?
      REAL
     :  XBMIN,  ! X minimum coord. of the bounding rectangle.
     :  XBMAX,  ! X maximum   "  . "   "     "         "    .
     :  YBMIN,  ! Y minimum   "  . "   "     "         "    .
     :  YBMAX,  ! Y maximum   "  . "   "     "         "    .
     :  XRANGE, ! X range of the bounding rectangle.
     :  YRANGE, ! Y   "   "   "     "         "    .
     :  RMIN,   ! Bounding box minimum for range selection.
     :  RMAX,   !    "      "  maximum  "    "       "    .
     :  XFID,   ! X coord. of the fiducial point.
     :  YFID,   ! Y   "  . "   "     "       "  .
     :  XP,     ! X coord. of the current data point.
     :  YP      ! Y   "  . "   "     "     "     "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the corners of the bounding box which just encloses the
*       polygon.

         XBMIN = XCORNR(1)
         XBMAX = XCORNR(1)
         YBMIN = YCORNR(1)
         YBMAX = YCORNR(1)

         DO CURCOR = 2, CORNER
            IF (XCORNR(CURCOR) .GT. XBMAX)  XBMAX = XCORNR(CURCOR)
            IF (XCORNR(CURCOR) .LT. XBMIN)  XBMIN = XCORNR(CURCOR)
            IF (YCORNR(CURCOR) .GT. YBMAX)  YBMAX = YCORNR(CURCOR)
            IF (YCORNR(CURCOR) .LT. YBMIN)  YBMIN = YCORNR(CURCOR)
         END DO

C        do curcor = 1, corner
C           print2000, curcor, xcornr(curcor), ycornr(curcor)
C2000       format(1x, 'corner, x, y: ', i5, 1pe15.3, 1pe15.3)
C        end do

*
*       Compute the position of a fiducial point outside the polygon.

         XRANGE = XBMAX - XBMIN
         YRANGE = YBMAX - YBMIN

         XFID = XBMAX + XRANGE
         YFID = YBMAX + YRANGE

C        print1999, xfid, yfid
C1999    format(1x, 'xfid, yfid: ', 1pe12.3, 1pe12.3)



*
*       Determine whether the catalogue is sorted on either the X or Y
*       column and if determine which rows fall within the bounding box.
*       If the catalogue is not sorted on one of the columns then set the
*       range of rows to be examined to the whole catalogue.

         SORT = .FALSE.

         CALL CAT_TIQAI (XCOLI, 'ORDER', ORDER, STATUS)
         IF (ORDER .EQ. CAT__ASCND  .OR.  ORDER .EQ. CAT__DSCND)
     :     THEN
            SORT = .TRUE.

            SCOLI = XCOLI
            RMIN = XBMIN
            RMAX = XBMAX
         ELSE
            CALL CAT_TIQAI (YCOLI, 'ORDER', ORDER, STATUS)
            IF (ORDER .EQ. CAT__ASCND  .OR.
     :          ORDER .EQ. CAT__DSCND) THEN
               SORT = .TRUE.

               SCOLI = YCOLI
               RMIN = YBMIN
               RMAX = YBMAX
            END IF
         END IF

         IF (SORT) THEN
            CALL CAT_SRNGR (CI, SCOLI, RMIN, RMAX, STARTR, STOPR,
     :        STATUS)
         ELSE
            STARTR = 1
            STOPR = ROWS
         END IF

*
*       Check separately that all is ok and that the range includes some
*       rows and proceed if both are ok.

         IF (STATUS .EQ. SAI__OK) THEN
            IF (STARTR .GE. 1  .AND.  STOPR .GE. 1  .AND.
     :          STOPR .GT. STARTR) THEN

*
*             Examine each row determining whether it is
*             inside or outside the polygon.

               NUMSEL = 0

               DO ROW = STARTR, STOPR

*
*                Read the XY coordinates for the current row from
*                the input catalogue.

                  CALL CAT_RGET (CI, ROW, STATUS)

                  CALL CAT_EGT0R (XCOLI, XP, NULFLX, STATUS)
                  CALL CAT_EGT0R (YCOLI, YP, NULFLY, STATUS)

C                 print2001, row, xp, yp
C2001             format(1x, 'row, xp, yp: ', i5, 1pe15.3, 1pe15.3)

*
*                Proceed if the status is ok and null values have
*                not been encountered.

                  IF (STATUS .EQ. SAI__OK  .AND.  .NOT. NULFLX
     :              .AND.  .NOT. NULFLY) THEN

*
*                   Check whether the point lies inside the bounding box.

                     IF (XP .LE. XBMAX  .AND.  XP .GE. XBMIN  .AND.
     :                   YP .LE. YBMAX  .AND.  YP .GE. YBMIN ) THEN

*
*                      Determine whether the point lies inside the
*                      polygon.

C                       print3000, corner, xfid, yfid, xp, yp
C3000                   format(1x, 'inside bounding box' /
C    :                     1x, 'corner: ', i5 /
C    :                     1x, 'xfid, yfid, xp, yp: ',
C    :                     1pe15.3, 1pe15.3, 1pe15.3, 1pe15.3)


                        CALL CAP_CLCNT (CORNER, XCORNR, YCORNR,
     :                    XFID, YFID, XP, YP, CROSS, STATUS)

C                       print3001, cross
C3001                   format(1x, 'cross: ', i5 )

*
*                      If the point lies inside the polygon there must
*                      be an odd number of crossings to the fiducial
*                      point outside the polygon.  If the point is
*                      inside the polygon then increment the count of
*                      such points and add it to the list.

                        IF (STATUS .EQ. SAI__OK) THEN
                           IF (MOD(CROSS, 2) .NE. 0) THEN
                              NUMSEL = NUMSEL + 1
                              SELIST(NUMSEL) = ROW
                           END IF
                        END IF

                     END IF
                  END IF
               END DO

*
*             If no objects were selected then set the status and report
*             an error.

               IF (STATUS .EQ. SAI__OK  .AND.  NUMSEL .LE. 0) THEN
                  STATUS = SAI__ERROR

                  CALL ERR_REP ('CAP_PLYSL_NOOB',
     :              'No objects found inside polygon.', STATUS)
               END IF

            ELSE
               STATUS = SAI__ERROR

               CALL ERR_REP ('CAP_PLYSL_NORN', 'No objects found in '/
     :           /'range corresponding to polygon.', STATUS)
            END IF
         END IF

*
*       Finally, if the status is not ok then set the number of objects to
*       zero and report a general error.

         IF (STATUS .NE. SAI__OK) THEN
            NUMSEL = 0

            CALL ERR_REP ('CAP_PLYSL_ERR', 'CAP_PLYSL: failure '/
     :        /'finding points inside the polygon.', STATUS)
         END IF

      END IF

      END
