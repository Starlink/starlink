*+  GEO_POLYIN- Selects points either inside or outside a polygon.
      SUBROUTINE GEO_POLYIN( NCORNER, XCORNR, YCORNR, WHERE, NUMOBJ,
     :       XCOORD, YCOORD, NUMSEL, NUMREJ, SELECT, LSTAT, STATUS )
*
*    Description :
*
*     The routine is given a list of coordinates of the corners
*     of a polygon and a list of coordinates for a set of points.
*     The points are then selected according to whether they lie
*     inside the polygon or not. The selection is exported via
*     a logical array. This array contains an entry for every
*     object in the input list, if the object was selected the
*     entry is set to .TRUE., otherwise it is .FALSE. Depending
*     on the imported value of 'WHERE' the selected points may
*     be either inside or outside the polygon.
*
*    Parameters :
*
*     NCORNER  =  INTEGER (READ)
*           Number of corners in the polygon.
*     XCORNR(NCORNER)  =  REAL (READ)
*           X ordinate of polygon corner
*     YCORNR(NCORNER)  =  REAL (READ)
*           Y ordinate of polygon corner
*     WHERE  =  CHARACTER*(*) (READ)
*           Determines whether points are to be selected either
*           inside or outside the polygon. The permitted values
*           are 'INSIDE' and 'OUTSIDE'. All other values
*           constitute an error.
*     NUMOBJ  =  INTEGER (READ)
*           Number of objects to be selected from.
*     XCOORD(NUMOBJ)  =  REAL (READ)
*           X coords. of objects for selection.
*     YCOORD(NUMOBJ)  =  REAL (READ)
*           Y coords. of objects for selection.
*     NUMSEL  =  INTEGER (WRITE)
*           Number of points satisfying the selection criterion.
*     NUMREJ  =  INTEGER (WRITE)
*           Number of points not satisfying the selection criterion.
*     SELECT(NUMOBJ)  =  LOGICAL (WRITE)
*           Array containing a selection flag for all the objects.
*           The Ith element of the array = .TRUE. if the element
*           satisfied the selection criteria, otherwise it is
*           .FALSE.
*     LSTAT  =  INTEGER  (STATUS)
*           Local status reporting errors in the routine.
*           It is a return status only.
*     STATUS  =  INTEGER (STATUS)
*           Running status.
*
*    Method :
*
*     Compute a fiducial point either inside or outside the polygon,
*     as required.
*     If the fiducial point has been computed ok then
*       compute the coordinates of the bounding rectangle.
*       for all points in the data array.
*         if the point is inside the bounding rectangle then
*           compute the no. of crossings of the polygon from the
*           fiducial point to the data point.
*           if no. of crossings even (condition satisfied) then
*             set flag = .true.
*           else no. of crossings odd (condition not satisfied) then
*             set flag = .false.
*           end if
*         else the point is outside the bounding rectangle (and hence
*         must be outside the polygon)
*           set the flag as appropriate for the selection being
*           performed.
*         end if
*       end for
*     end if
*
*     Note - the "bounding rectangle" is the smallest rectangle which
*     entirely encloses the polygon defining the selection. If a point
*     is outside the bounding rectangle then it is necessarily outside
*     the polygon.
*
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall.   (ROE::ACD)
*    History :
*     29/2/84:  Original version.                            (ROE::ACD)
*     16/6/86:  Converted from Haggis to SCAR.               (ROE::ACD)
*     18/7/86:  Check on whether points are outside the      (ROE::ACD)
*               bounding rectangle added (to speed it up).
*     12/2/91:  Imported into ASTERIX. ADC specifics removed (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NCORNER,        ! No. of corners in the polygon.
     :  NUMOBJ          ! No. of objects to be tested for selection.
      REAL
     :  XCORNR(NCORNER),! X coords. of the corners of the polygon.
     :  YCORNR(NCORNER),! Y coords. of the corners of the polygon.
     :  XCOORD(NUMOBJ), ! X coords. of the data points.
     :  YCOORD(NUMOBJ)  ! Y   "   . "   "   "     "   .
      CHARACTER
     :  WHERE*(*)       ! Points to be selected inside/outside polygon.
*    Export :
      INTEGER
     :  NUMSEL,         ! No. of points selected.
     :  NUMREJ          ! No. of points rejected.
      LOGICAL
     :  SELECT(NUMOBJ)  ! Selection flag. = .true. if object selected.
*    Status :
      INTEGER
     :  LSTAT,     ! Local status. Output only. 0 for success.
     :  STATUS     ! Running status.
*    Local variables :
      INTEGER
     :  INDEX,  ! No. of the current data point.
     :  CROSS   ! No. of crossings from fiducial point to current data point.
      REAL
     :  XFID,   ! X coord. of the fiducial point.
     :  YFID,   ! Y   "  . "   "     "       "  .
     :  XP,     ! X coord. of the current data point.
     :  YP,     ! Y   "  . "   "     "     "     "  .
     :  XBMIN,  ! X minimum coord. of the bounding rectangle.
     :  XBMAX,  ! X maximum   "  . "   "     "         "    .
     :  YBMIN,  ! Y minimum   "  . "   "     "         "    .
     :  YBMAX   ! Y maximum   "  . "   "     "         "    .
*-

      IF (STATUS .EQ. SAI__OK) THEN

*       Compute the position of a fiducial point either inside or
*       outside the polygon, as required.
         CALL GEO_POLYIN_INSID( NCORNER, XCORNR(1), YCORNR(1),
     :                          WHERE, XFID, YFID, LSTAT, STATUS )
         IF (LSTAT .EQ. 0  .AND.  STATUS .EQ. SAI__OK) THEN

*          Establish the coordinates of the bounding rectangle.
            XBMIN = XCORNR(1)
            XBMAX = XCORNR(1)
            YBMIN = YCORNR(1)
            YBMAX = YCORNR(1)

            DO INDEX = 2, NCORNER
               IF (XCORNR(INDEX) .GT. XBMAX) XBMAX = XCORNR(INDEX)
               IF (XCORNR(INDEX) .LT. XBMIN) XBMIN = XCORNR(INDEX)
               IF (YCORNR(INDEX) .GT. YBMAX) YBMAX = YCORNR(INDEX)
               IF (YCORNR(INDEX) .LT. YBMIN) YBMIN = YCORNR(INDEX)
            END DO

*          Check whether each point is inside the polygon or not.
            NUMSEL = 0
            NUMREJ = 0

            DO INDEX = 1, NUMOBJ
               XP = XCOORD(INDEX)
               YP = YCOORD(INDEX)

*             Check if the point lies inside the bounding rectangle.
               IF (XP .LE. XBMAX  .AND.  XP .GE. XBMIN  .AND.
     :           YP .LE. YBMAX  .AND.  YP .GE. YBMIN ) THEN

*                The point lies inside the bounding rectangle - check
*                the number of times the line from the fiducial point
*                to the data point crosses the polygon.
                  CALL GEO_POLYIN_CLCNT( NCORNER, XCORNR, YCORNR,
     :                        XFID, YFID, XP, YP, CROSS, STATUS )

*                If the no. of crossings is even the point is on the
*                same side of the polygon as the fiducial mark, if it
*                is odd it is on the opposite side. Set the selection
*                flag accordingly.
                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (MOD(CROSS, 2) .EQ. 0) THEN
                        SELECT(INDEX) = .TRUE.
                        NUMSEL = NUMSEL + 1
                     ELSE
                        SELECT(INDEX) = .FALSE.
                        NUMREJ = NUMREJ + 1
                     END IF
                  ELSE
                     SELECT(INDEX) = .FALSE.
                  END IF
               ELSE

*                The point lies outside the bounding rectangle and hence
*                outside the polygon.
                  IF (WHERE .EQ. 'INSIDE') THEN
                     SELECT(INDEX) = .FALSE.
                     NUMREJ = NUMREJ + 1
                  ELSE
                     SELECT(INDEX) = .TRUE.
                     NUMSEL = NUMSEL + 1
                  END IF
               END IF

            END DO

         END IF
      END IF

      END



*+  GEO_POLYIN_INSID - Determines coords. of a point inside or outside a polygon.
      SUBROUTINE GEO_POLYIN_INSID( POINTS, XCORNR, YCORNR, WHERE,
     :                                  XPOS, YPOS, LSTAT, STATUS )
*    Description :
*     Determines coordinates for a point either inside or outside a
*     given polygon. The routine is presented with arrays containing
*     the corners of the polygon and a flag indicating whether a
*     point inside or outside the polygon is to be located. Coordinates
*     for a suitable point are then generated.
*    Parameters :
*     POINTS    = REAL (READ)
*           No. of corners in the given polygon.
*     XCORNR(POINTS) = REAL (READ)
*           X coords. of the corners of the polygon.
*     YCORNR(POINTS) = REAL (READ)
*           Y coords. of the corners of the polygon.
*     WHERE = CHARACTER*(*) (READ)
*           Flag indicating whether a point inside or outside the
*           polygon is to be generated. The permitted values are
*           'INSIDE' and 'OUTSIDE'. All other values consitute an error.
*     XPOS = REAL (WRITE)
*           X coord. of point generated.
*     YPOS = REAL (WRITE)
*           Y coord. of point generated.
*     LSTAT = INTEGER (WRITE)
*           Local status reporting error conditions that have been
*           generated by the routine. Values output are;
*            0  -  Success.
*            1  -  Invalid value for 'WHERE' imported.
*            2  -  Unable to find a point inside the polygon.
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     Compute the numerically largest values for the X and Y coords.
*     of the corners and also find the Centre-of-Gravity of the
*     polygon.
*     Generate a point which must be outside the polygon; take the
*     X and Y extrema and add 10% of the distance from the C-o-G.
*     If a point outside the polygon is required then
*       Use the point outside just computed.
*     else if a point inside the polygon is required then
*       do while (not all corners examined and point inside not found)
*         compute coords. of a point 9/10 of the way from the C-o-G
*         to the current corner of the polygon.
*         compute the number of crossings from this point to the
*         point known to be outside the polygon.
*         If the number of crossings is odd (point is inside polygon)
*         then
*           record coords. of point found.
*           set termination flag
*         end if
*       end do
*       If checked all corners without finding a point inside then
*         set status.
*       end if
*     else
*       The given inside/outside flag was invalid - set status.
*     end if
*    Deficiencies :
*     When attempting to find a point inside a polygon the routine
*     examines all the points 9/10 of the way from the C-o-G of the
*     polygon to each corner and checks whether it is inside or outside
*     the polygon. This procedure will yield a point inside most
*     polygons, but there are some shapes (eg. a very thin doughnut
*     with a bite out of it) where it will fail. If a point inside
*     the polygon is not found then an error flag is set. A possible
*     refinement would be; if a point inside the polygon is not
*     located on the first pass round the corners then asymptotically
*     decrease the distance between the points being checked and the
*     corresponding corner until a point inside is found.
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall     (ROE::ACD)
*    History :
*     27/2/84:  Original version.                             (ROE::ACD)
*     16/6/86:  Converted from Haggis to SCAR.                (ROE::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  POINTS          ! Number of corners of polygon.
      REAL
     :  XCORNR(POINTS), ! X coords. of corners of polygon.
     :  YCORNR(POINTS)  ! Y   "   . "     "    "     "   .
      CHARACTER
     :  WHERE*(*)       ! Point inside/outside polygon flag.
*    Export :
      REAL
     :  XPOS,           ! X coord. of point inside or outside polygon.
     :  YPOS            ! Y   "  . "    "     "    "     "       "   .
*    Status :
      INTEGER
     :  LSTAT,          ! Local status.
     :  STATUS          ! Running status.
*    Local Constants :
      INTEGER
     :  LSTAT1,   ! Invalid value for 'WHERE' imported.
     :  LSTAT2    ! Unable to find a point inside the polygon.
      PARAMETER (
     :  LSTAT1=1,
     :  LSTAT2=2)
*    Local variables :
      REAL
     :  XMAX,   ! Max. value of X coord. amongst all corners of polygon.
     :  YMAX,   !  "     "   "  Y   "  .    "     "     "    "     "   .
     :  XSUM,   ! Sum of X values for all corners of polygon.
     :  YSUM,   !  "  "  Y   "     "   "     "    "     "   .
     :  XMEAN,  ! X coord. of C-o-G of polygon.
     :  YMEAN,  ! Y   "  . "    "   "     "   .
     :  XOUT,   ! X coord. of point outside polygon.
     :  YOUT,   ! Y   "  . "    "      "       "   .
     :  XRUN,   ! X coord. of point 9/10 of way from C-o-G to current corner.
     :  YRUN    ! Y   "  . "    "    "   "   "   "     "   "     "      "   .
      INTEGER
     :  INDEX,  ! Number of polygon corner currently being examined.
     :  CROSS   ! Number of times a line crosses the polygon boundary.
      LOGICAL
     :  FOUND   ! Flag; a point inside polygon has been found?
*-

      IF (STATUS .EQ. 0) THEN
         LSTAT = 0

*       Find the largest X and Y points in the polygon and also compute
*       its centre of gravity.
         XMAX = XCORNR(1)
         YMAX = YCORNR(1)
         XSUM = 0.0E0
         YSUM = 0.0E0

         DO INDEX = 1, POINTS
            IF (XCORNR(INDEX) .GT. XMAX)   XMAX = XCORNR(INDEX)
            IF (YCORNR(INDEX) .GT. YMAX)   YMAX = YCORNR(INDEX)
            XSUM = XSUM + XCORNR(INDEX)
            YSUM = YSUM + YCORNR(INDEX)
         END DO

         XMEAN = XSUM / FLOAT(POINTS)
         YMEAN = YSUM / FLOAT(POINTS)

*       Add 10% of the distance from the C-o-G to each extrema;
*       this generates a point which must be outside the polygon.
         XOUT = XMAX + ((XMAX - XMEAN) * 1.0E-1)
         YOUT = YMAX + ((YMAX - YMEAN) * 1.0E-1)

*       In the case where a point outside the polygon is to be generated
*       use this point.
         IF (WHERE .EQ. 'OUTSIDE') THEN
            XPOS = XOUT
            YPOS = YOUT

*       Case where have to generate a point inside the polygon.
         ELSE IF (WHERE .EQ. 'INSIDE') THEN

*          Examine all the corners of the polygon until a suitable
*          one has been found, or all have been looked at.
            INDEX = 0
            FOUND = .FALSE.

            DO WHILE (.NOT. FOUND  .AND.  INDEX .LT. POINTS)

               INDEX = INDEX + 1

*             Compute coords. of point 9/10 of the way from the C-o-G
*             to the INDEXth point.
               XRUN = XMEAN + ((XCORNR(INDEX) - XMEAN) * 9.0E-1)
               YRUN = YMEAN + ((YCORNR(INDEX) - YMEAN) * 9.0E-1)

*             Compute the number of crossings from the point
*             unambiguously outside the polygon to this current point.
               CALL GEO_POLYIN_CLCNT (POINTS, XCORNR, YCORNR, XOUT,
     :                             YOUT,  XRUN, YRUN, CROSS, STATUS )

*             If the no. of crossings is odd a point inside the polygon
*             has been found: record the coords. and set the termination
*             flag.
               IF (MOD(CROSS, 2) .EQ. 1) THEN
                  XPOS = XRUN
                  YPOS = YRUN
                  FOUND = .TRUE.
               END IF

            END DO

*          Set the status if no point inside the polygon has been found.
            IF (.NOT.FOUND) LSTAT = LSTAT2

         ELSE

*          An invalid value was assigned to "WHERE", ie. neither
*          'INSIDE' nor 'OUTSIDE'.
            LSTAT = LSTAT1

         END IF
      END IF

      END


*+  GEO_POLYIN_CLCNT - No. of times a line segment crosses a polygon boundary.
      SUBROUTINE GEO_POLYIN_CLCNT (N, X, Y, X0, Y0, XP, YP, NCROSS,
     :                                                        STATUS)
*    Description :
*     Takes arrays X and Y of N coordinates which contain the vertices
*     of a polygon and calculates how many times, NCROSS, the line
*     segment from (X0,Y0) to (XP,YP) crosses the region boundary.
*    Parameters :
*     N  =  INTEGER (READ)
*           Number of vertices (corners) of the polygon.
*     X(N)  =  REAL (READ)
*           X coords. of the polygon vertices.
*     Y(N)  =  REAL (READ)
*           Y coords. of the polygon vertices.
*     X0  =  REAL (READ)
*           X coord. of one end of the line segment.
*     Y0  =  REAL (READ)
*           Y coord. of one end of the line segment.
*     XP  =  REAL (READ)
*           X coord. of the other end of the line segment.
*     YP  =  REAL (READ)
*           Y coord. of the other end of the line segment.
*     NCROSS  =  INTEGER (WRITE)
*           Number of times the line segment crosses the polygon
*           boundary.
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     Set the number of crossings = 0
*     for all corneres except the first do
*       if coords. of corner not the same as those of the previous
*       corner then
*         Check if the line segment from this corner to the previous
*         corner intersects the input line segment.
*         If it does intersect then
*           increment the no. of crossings
*         end if
*       end if
*     end do
*     if the coords. of the last corner are not equal to those of the first
*     then
*       check if the line segment joining the last and first corners
*       crosses the input segment
*       if it does cross then
*         increment the no. of crossings.
*       end if
*     end if
*    Bugs :
*     None known.
*    Authors :
*     D.R.K.Brownrigg     (ROE::ASOC1)
*     A C Davenhall       (ROE::ACD)
*    History :
*     16/7/82:  Original version.                           (ROE::ASOC1)
*     28/2/84:  Modified to "SSE" stype.                    (ROE::ACD)
*     16/6/86:  Converted from Haggis to SCAR.              (ROE::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     : N      ! Number of vertices (corners) of polygon input.
      REAL
     : X(N),  ! X coords. of polygon vertices.
     : Y(N),  ! Y   "   . "     "       "    .
     : X0,    ! X coord. of one end of the line segment.
     : Y0,    ! Y   "  . "   "   "  "   "   "      "   .
     : XP,    ! X coord. of the other end of the line segment.
     : YP     ! Y   "  . "   "    "    "  "   "   "      "   .
*    Export :
      INTEGER
     :  NCROSS ! No. of times the line crosses the polygon boundary.
*    Status :
      INTEGER
     :  STATUS ! Running status.
*    Local variables :
      INTEGER
     :  I,     ! Number of the corner currently being examined.
     :  INT    ! Flag showing whether line crosses a given polgon side.
*-

*    INT (returned by GEO_POLYIN_CLINT);
*      = 1  -  The 2 input segments cross.
*      = 0  -  The 2 input segments do not cross.
      IF (STATUS .EQ. SAI__OK) THEN
         NCROSS = 0

         DO I = 2, N
            IF (X(I-1) .NE. X(I)  .OR.  Y(I-1) .NE. Y(I)) THEN
               CALL GEO_POLYIN_CLINT (X(I-1), Y(I-1), X(I), Y(I),
     :                                X0, Y0, XP, YP, INT, STATUS)
               IF (INT .EQ. 1) THEN
                  NCROSS = NCROSS + 1
               END IF
            END IF
         END DO

         IF (X(N) .NE. X(1)  .OR.  Y(N) .NE. Y(1)) THEN
            CALL GEO_POLYIN_CLINT (X(N), Y(N), X(1), Y(1), X0, Y0,
     :                                        XP, YP,  INT, STATUS )
            IF (INT .EQ. 1) THEN
               NCROSS = NCROSS + 1
            END IF
         END IF

      END IF

      END



*+  GEO_POLYIN_CLINT - Do two straight line segments intersect?
      SUBROUTINE GEO_POLYIN_CLINT (XIM1, YIM1, XI, YI, X0, Y0, XP, YP,
     :                                                     INT, STATUS )
*    Description :
*     Do two straight line segments intersect?
*     If the line segment from (XIM1,YIM1) to (XI,YI) intersects the
*     line segment from (X0,Y0) to (XP,YP) then INT=1, otherwise INT=0.
*    Parameters :
*     XIM1  =  REAL (READ)
*           X-coord end 1 of line segment 1.
*     YIM1  =  REAL (READ)
*           Y-coord end 1 of line segment 1.
*     XI  =  REAL (READ)
*           X-coord end 2 of line segment 1.
*     YI  =  REAL (READ)
*           Y-coord end 2 of line segment 1.
*     X0  =  REAL (READ)
*           X-coord end 1 of line segment 2.
*     Y0  =  REAL (READ)
*           Y-coord end 1 of line segment 2.
*     XP  =  REAL (READ)
*           X-coord end 2 of line segment 2.
*     YP  =  REAL (READ)
*           Y-coord end 2 of line segment 2.
*     INT  =  INTEGER (WRITE)
*           1 if segments cross, 0 if not.
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     <description of how the subroutine works>
*    Bugs :
*     None known.
*    Authors :
*     D.R.K. Brownrigg.   (ROE::ASOC1)
*     A C Davenhall.      (ROE::ACD)
*    History :
*     16/7/82:  Original version.                           (ROE::ASOC1)
*     16/7/86:  Converted from Haggis to SCAR and changed   (ROE::ACD)
*               to the full Starlink style.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL
     :  XIM1,   ! X-coord end 1 of line segment 1.
     :  YIM1,   ! Y-coord end 1 of line segment 1.
     :  XI,     ! X-coord end 2 of line segment 1.
     :  YI,     ! Y-coord end 2 of line segment 1.
     :  X0,     ! X-coord end 1 of line segment 2.
     :  Y0,     ! Y-coord end 1 of line segment 2.
     :  XP,     ! X-coord end 2 of line segment 2.
     :  YP      ! Y-coord end 2 of line segment 2.
*    Export :
      INTEGER
     :  INT     ! 1 if segments cross, 0 if not.
*    Status :
      INTEGER
     :  STATUS  ! Running status.
*    Local variables :
      REAL
     :  VAL1,
     :  VAL2,
     :  VAL3,
     :  VAL4
*-

      IF (STATUS .EQ. SAI__OK) THEN

         CALL GEO_POLYIN_CLVAL (XIM1, YIM1, X0, Y0, XP, YP, VAL1,
     :                                                      STATUS)
         CALL GEO_POLYIN_CLVAL (XI, YI, X0, Y0, XP, YP, VAL2, STATUS)
         CALL GEO_POLYIN_CLVAL(X0, Y0, XIM1, YIM1, XI, YI, VAL3,
     :                                                      STATUS)
         CALL GEO_POLYIN_CLVAL(XP, YP, XIM1, YIM1, XI, YI, VAL4,
     :                                                      STATUS)

*       The following condition avoids erroneously computing two
*       crossing points when a line crosses a polygon vertex. That is,
*       when the end of one line segment is crossed by a line, and the
*       same line segment end is an end of another line segment.
         IF ( (VAL1*VAL2 .LT. 0.0E0  .OR.  VAL1 .EQ. 0.0E0)  .AND.
     :     (VAL3*VAL4 .LT. 0.0E0 .OR. VAL3 .EQ. 0.0E0) ) THEN
            INT = 1
         ELSE
            INT = 0
         END IF

      END IF

      END



*+  GEO_POLYIN_CLVAL - On which side of a line does a point lie?
      SUBROUTINE GEO_POLYIN_CLVAL (X, Y, XA, YA, XB, YB, VAL, STATUS)
*    Description :
*     On which side of a given line segment does a given point lie?
*     (y-YA) * (XB-XA)  -  (x-XA) * (YB-YA) = 0 defines the straight
*     line through (XA,YA) and (XB,YB). The expression is evaluated for
*     some given point (X,Y) to decide on which side of the line (X,Y)
*     lies.
*    Parameters :
*     X  =  REAL (READ)
*           X-coord of given point.
*     Y  =  REAL (READ)
*           Y-coord of given point.
*     XA  =  REAL (READ)
*           X-coord point 1 on line.
*     YA  =  REAL (READ)
*           Y-coord point 1 on line.
*     XB  =  REAL (READ)
*           X-coord point 2 on line.
*     YB  =  REAL (READ)
*           Y-coord point 2 on line.
*     VAL  =  REAL (WRITE)
*           Value of line expression at (X,Y).
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     Compute VAL.
*     Note: if the computation fails then the return status us set by
*     ADC_ARCHCK.
*    Bugs :
*     None known.
*    Authors :
*     D.R.K. Brownrigg    (ROE::ASOC1)
*     A C Davenhall.      (ROE::ACD)
*    History :
*     16/7/82:  Original version.                           (ROE::ASOC1)
*     16/6/86:  Converted from Haggis to SCAR and changed   (ROE::ACD)
*               and converted to the Starlink style.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL
     :  X,     ! X-coord of given point.
     :  Y,     ! Y-coord of given point.
     :  XA,    ! X-coord point 1 on line.
     :  YA,    ! Y-coord point 1 on line.
     :  XB,    ! X-coord point 2 on line.
     :  YB     ! Y-coord point 2 on line.
*    Export :
      REAL
     :  VAL    ! Value of line expression at (X,Y).
*    Status :
      INTEGER
     :  STATUS ! Running status.
*    External references :
C      EXTERNAL
C     :  ADC_HANDLE
C      LOGICAL
C     :  ADC_ARCHCK
C*    Local variables :
C      LOGICAL
C     :  ERROR  ! Flag; has an error occurred computing VAL?
*-

*
*    Establish the error handler.
C      CALL LIB$ESTABLISH (ADC_HANDLE)

      IF (STATUS .EQ. SAI__OK) THEN

         VAL = (Y-YA) * (XB-XA)  -  (X-XA) * (YB-YA)
C         ERROR = ADC_ARCHCK (STATUS)

C         IF (ERROR) THEN
C            VAL = 0.0E0
C         END IF

      END IF

      END
