C# IL>=a, OL>=0
      SUBROUTINE GKMCVX (NO,XO,YO,ICLOCK,NC,XC,YC)
*
* (C) COPYRIGHT ICL & SERC  1990
*

*--------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:  KEVP
*
      INCLUDE '../include/check.inc'

*  PURPOSE
*  -------
*     Obtain the convex hull of a polygon
*     The polygon must either be
*                 anticlockwise = interior always on the left
*                 or  clockwise = interior always on the right.
*
*  MAINTENANCE LOG
*  ---------------
*     22/07/88  KEVP  Replaced IFIX with machine independent INT
*     22/07/90  PLP   Commenting brought in line with standard format.
*     10/12/91  KEVP  Initialized IBCY & IABY (C91).
*
*  ARGUMENTS
*  ---------
*     INP   NO      Number of vertices
*     INP   XO,YO   Vertices in raster coords when rounded down
*     INP   ICLOCK  Turning indicator; 1=Anticlockise, -1=Clockwise.
*     OUT   NC      Number of vertices in convex hull
*     OUT   XC,YC   Vetices of the convex hull (raster coords)
*
*     NB:  The arrays XC,YC must be NO long.
*          Only the first NC are valid output.
*          The values of XC,YC must be of constant sign
*
      INTEGER  NO, NC, ICLOCK
      REAL     XO(NO),YO(NO), XC(NO),YC(NO)
*
*  LOCALS
*  ------
*     I        Do loop index
*     IAX,IAY  First point in triple
*     IBX,IBY  Middle point in triple
*     ICX,ICY  Last point in triple
*     NCOKPT   Number of consecutive points kept (used to test convexity)
*     NEXT     Next point for triple
*     NKEPT    Number of points kept so far in present pass
*
*
      INTEGER    I
      INTEGER    IAX,IAY, IBX,IBY, ICX,ICY
      INTEGER    IABX,IABY, IBCX,IBCY, IABC
      INTEGER    NKEPT, NCOKPT, NEXT

*  ALGORITHM
*  ---------
*     This routine works quickest for polygons that are almost convex
*     and is ideal for polygons, that have lost their convexity through
*     rounding to integers.
*
*     Each consecutive triple is tested,
*     to see if it turns the right way (according to ICLOCK)
*     when rounded down to integers.
*
*     If it turns the wrong way the middle point is deleted.
*
*     If any points were deleted the above procedure is repeated.
*     Else, the polygon has become convex and the routine is finished.
*
*     The routine finishes also if the total number of consecutive points
*     not deleted becomes equal to the total number of points remaining.
*
*
*     If ICLOCK is invalid NC is set to -1 and nothing else is done.
*
*------------------------------------------------------------------------

*     Check turning indicator, quit if invalid
      IF(IABS(ICLOCK) .NE. 1)THEN
         NC = -1
         GOTO 99
      ENDIF

*     Transfer polygon to arrays XC,YC to work upon
      DO 10 I=1,NO
          XC(I) = XO(I)
          YC(I) = YO(I)
   10 CONTINUE
      NC = NO

*     Initialise
      NCOKPT = 0

*     -------------
*     Start of loop
*     -------------
*
   20 CONTINUE

*     Set variables for start of pass
      IAX = INT(XC(NC))
      IAY = INT(YC(NC))
      IBX = INT(XC(1))
      IBY = INT(YC(1))
      NKEPT = 0
*
*     Loop over consecutive triples (middle point at I)
      DO 30 I=1,NC
         NEXT = I - NC*(I/NC) + 1
         ICX = INT(XC(NEXT))
         ICY = INT(YC(NEXT))
         IABX = IBX - IAX
         IABY = IBY - IAY
         IBCX = ICX - IBX
         IBCY = ICY - IBY
*     +  The cross product IABC              +
*     +  should have the same sign as ICLOCK +
*     +  otherwise the point is deleted.     +
         IABC = (IABX*IBCY - IABY*IBCY)
         IF(ICLOCK*IABC .GT. 0)THEN
*          Point is kept
           NKEPT = NKEPT+1
           NCOKPT = NCOKPT+1
           IF(NKEPT .LT. I)THEN
              XC(NKEPT) = XC(I)
              YC(NKEPT) = YC(I)
           ENDIF
           IF(NCOKPT .GE. NC)THEN
*             Polygon is proven convex, finish
              GOTO 99
           ENDIF
*          move point to start of triple
           IAX = IBX
           IAY = IBY
         ELSE
*          Point is deleted
           NCOKPT = 0
         ENDIF
*        Move last point to middle of triple
         IBX = ICX
         IBY = ICY

   30 CONTINUE

*     Determine whether another pass is required
      IF(NKEPT .NE. NC)THEN
*        Polygon is still not proven convex
         NC = NKEPT
         GOTO 20
      ENDIF

*     -----------
*     End of loop
*     -----------
*     Here - Polygon is passed as convex
*
   99 CONTINUE
*     or ICLOCK is invalid

      RETURN
      END
