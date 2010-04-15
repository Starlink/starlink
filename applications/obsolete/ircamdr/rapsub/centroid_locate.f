*+  CENTROID_LOCATE - locates the centroid of a star like image feature

      SUBROUTINE CENTROID_LOCATE ( INARRAY, DIMS1, DIMS2, XINIT,
     :                             YINIT, SEARCH, POSITIVE, MAXSHIFT,
     :                             MAXITER, TOLER, INVALID, XFINAL,
     :                             YFINAL, ERROR, STATUS )

*    Description :
*
*     This routine locates the centroid of a star like feature within
*     a defined search area in an image about suggested starting
*     coordinates, and returns the final centroid position.
*
*    Invocation :
*
*     CALL CENTROID_LOCATE( INARRAY, DIMS, XINIT, YINIT, SEARCH, POSITIVE,
*                           MAXSHIFT, MAXITER, TOLER, INVALID, XFINAL, YFINAL,
*                           ERROR, STATUS )
*
*    Method :
*
*     The routine forms marginal profiles within a search square, and
*     then subtracts a background estimate from each profile, before
*     finding the profile centroids. This is repeated for a specified
*     number of iterations, until the requested accuracy is met, or
*     until one of several error conditions is met.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     13-01-1986 :  First Rapi2d implementation - direct copy from
*                :  Rodney Warren-Smith's EDRS routine of same name.
*                :  (REVA::MJM)
*     12-AUG-1994   Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2,       ! dimensions of input image
     :    SEARCH,             ! size of search square to be used
     :    MAXITER             ! maximum number of iterations to be used

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! input image
     :    XINIT,              ! x coord of initial estimate position
     :    YINIT,              ! y   "    "    "        "        "
     :    MAXSHIFT,           ! max shift allowable from initial position
     :    TOLER,              ! accuracy required in centroid position
     :    INVALID             ! value of invalid pixels

      LOGICAL
     :    POSITIVE            ! true if image features are positive

*    Export :

      INTEGER
     :    ERROR               ! set to other than 0 if error occurs

      REAL
     :    XFINAL,             ! final x coord of centroid position
     :    YFINAL              !   "   y   "    "     "        "

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    SEARCHMAX           ! maximum search square size allowed
      PARAMETER( SEARCHMAX  =  51 )

*    Local variables :

      INTEGER
     :    NSAMPLES,           ! number of rows/columns in search square
                              ! actually used by program
     :    HALFSIZE,           ! half the above (truncated NSAMPLES/2)
     :    NUMITER,            ! maximum number of iterations to be used,
                              ! forced to be at least one iteration
     :    CURRITER,           ! current iteration counter
     :    ISTART,             ! x coord of edge of current search box
     :    JSTART,             ! y   "    "   "   "    "       "    "
     :    IPOSN,              ! x coord of current column being added
     :    JPOSN,              ! y   "    "    "       "     "     "
     :    NXAV( SEARCHMAX ),  ! used to count additions to x profile
     :    NYAV( SEARCHMAX ),  !   "   "   "       "      " y    "
     :    NTH,                ! value used to determine background
                              ! level - Nth smallest number taken
     :    I, J, K, L, M       ! counters

      REAL
     :    XLAST,              ! x coord worked out in last iteration
     :    YLAST,              ! y   "      "    "   "   "      "
     :    XBACK,              ! background value used in x direction
     :    YBACK,              !      "       "     "   " y     "
     :    XAV( SEARCHMAX ),   ! used to sum x profile
     :    YAV( SEARCHMAX ),   !   "   "  "  y    "
     :    STACK( SEARCHMAX/4 + 2 ),  ! stack used by background estimator
     :    XPOSN,              ! real x position used in scanning
     :    YPOSN,              !   "  y     "      "   "     "
     :    XNUMER,             ! numerator in x centroid calculation
     :    YNUMER,             !     "      " y     "         "
     :    XDENOM,             ! denominator  x     "         "
     :    YDENOM,             !      "       y     "         "
     :    DUMMY,              ! dummy variable used in centroiding
     :    SHIFT,              ! shift of current posn from initial one
     :    POSCHANGE           !   "    "    "      "    "  previous "

      LOGICAL
     :    FINISHED            ! if true, then return - calc. finished

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by initialising the error variable to 0 - the
*    no-error condition. Also initialise the FINISHED
*    logical to FALSE
      ERROR  =  0
      FINISHED  =  .FALSE.

*    initialise the square centre to be at the initial
*    estimated position
      XFINAL  =  XINIT
      YFINAL  =  YINIT

*    make sure the search square size is between 3 and the maximum
*    allowed size, and initialise the half-box size
      NSAMPLES  =  MIN( MAX( 3, SEARCH ), SEARCHMAX )
      HALFSIZE  =  INT( NSAMPLES / 2.0 )

*    check that at least one iteration is done
      NUMITER   =  MAX( 1, MAXITER )

*    initialise the number of iterations done so far, and start
*    the main loop
      CURRITER  =  0

      DO WHILE ( .NOT. FINISHED )

*       increment the current iteration counter
         CURRITER  =  CURRITER + 1

*       note the starting coords for this iteration
         XLAST  =  XFINAL
         YLAST  =  YFINAL

*       work out the starting edges of the current search square
         ISTART  =  NINT(MIN(MAX(-1.0E6,XFINAL),1.0E6)) - (HALFSIZE+1)
         JSTART  =  NINT(MIN(MAX(-1.0E6,YFINAL),1.0E6)) - (HALFSIZE+1)

*       initialise the arrays used to form the marginal profiles
         DO  K  =  1, NSAMPLES
            XAV( K )  =  0.0
            YAV( K )  =  0.0
            NXAV( K ) =  0
            NYAV( K ) =  0
         END DO

*       scan the current search area, forming the x and y profiles from
*       all valid pixels - scan all rows
         DO  J  =  1, NSAMPLES
            JPOSN  =  JSTART + J

*          check that the current row is within the image
            IF(( JPOSN .GE. 1 ) .AND. ( JPOSN .LE. DIMS2)) THEN

*             now scan all pixels in the current row
               DO  I  =  1, NSAMPLES
                  IPOSN  =  ISTART + I

*                check that the current pixel is in the image
                  IF(( IPOSN .GE. 1 ) .AND.
     :               ( IPOSN .LE. DIMS1)) THEN

*                   include valid pixels in the profiles
                     IF( INARRAY( IPOSN,JPOSN ) .NE. INVALID ) THEN

*                      increment the arrays accordingly
                        XAV( I )  = XAV( I ) + INARRAY( IPOSN,JPOSN )
                        YAV( J )  = YAV( J ) + INARRAY( IPOSN,JPOSN )
                        NXAV( I ) = NXAV( I ) + 1
                        NYAV( J ) = NYAV( J ) + 1

                     END IF

                  END IF

               END DO

            END IF

         END DO

*       now evaluate the profile bins that contain at least one
*       valid pixel - invert results if POSTIVE is false
         DO  L  =  1, NSAMPLES

*          first the x profile -
*          check for nil pixels having been added to the current bin
            IF( NXAV( L ) .EQ. 0 ) THEN

*             in that case, make the current bin very large
               XAV( L )  =  1.0E10

            ELSE

*             normalise the current bin value, checking POSITIVE
               IF( POSITIVE ) THEN
                  XAV( L )  =  XAV( L ) / NXAV( L )
               ELSE
                  XAV( L )  = -XAV( L ) / NXAV( L )
               END IF

            END IF

*          repeat for the y profile -
*          check for nil pixels having been added to the current bin
            IF( NYAV( L ) .EQ. 0 ) THEN

*             in that case, make the current bin very large
               YAV( L )  =  1.0E10

            ELSE

*             normalise the current bin value, checking POSITIVE
               IF( POSITIVE ) THEN
                  YAV( L )  =  YAV( L ) / NYAV( L )
               ELSE
                  YAV( L )  = -YAV( L ) / NYAV( L )
               END IF

            END IF

         END DO

*       now call the subroutine NTHMIN to find the lower quartile
*       point in each profile as a background estimate - first
*       evaluate the number NTH
         NTH  =  MAX( INT( NSAMPLES/4 ), 2 )

*       now get the n smallest numbers in the x profile, and put
*       the n-th smallest into XBACK
         CALL NTHMIN( XAV, NSAMPLES, NTH, STACK, STATUS )
         XBACK  =  STACK( 1 )

*       similarly for the y profile, value placed in YBACK
         CALL NTHMIN( YAV, NSAMPLES, NTH, STACK, STATUS )
         YBACK  =  STACK( 1 )

*       now initialise the sums used for forming the centroids
         XNUMER  =  0.0
         XDENOM  =  0.0
         XPOSN   =  REAL( ISTART )
         YNUMER  =  0.0
         YDENOM  =  0.0
         YPOSN   =  REAL( JSTART )

*       scan the profiles, using all data above the estimated
*       backgrounds to form sums for the centroids
         DO  M  =  1, NSAMPLES

*          increment the current values of the scanning positions
            XPOSN  =  XPOSN + 1.0
            YPOSN  =  YPOSN + 1.0

*          x first - check for there being data in the current bin
            IF( NXAV( M ) .NE. 0 ) THEN
               DUMMY  =  MAX( XAV( M )-XBACK, 0.0 )
               XNUMER =  XNUMER + ( XPOSN*DUMMY )
               XDENOM =  XDENOM + DUMMY
            END IF

*          now y - check for there being data in the current bin
            IF( NYAV( M ) .NE. 0 ) THEN
               DUMMY  =  MAX( YAV( M )-YBACK, 0.0 )
               YNUMER =  YNUMER + ( YPOSN*DUMMY )
               YDENOM =  YDENOM + DUMMY
            END IF

         END DO

*       now form the current centroid positions, checking for either
*       profile not containing data
         IF(( XDENOM .LT. 1.0E-10 ).AND.( YDENOM .LT. 1.0E-10 )) THEN

*          neither profile has any data - set the x,y positions back
*          to the starting values, set the error variable, and return
            XFINAL  =  XINIT
            YFINAL  =  YINIT
            ERROR   =  1
            FINISHED  =  .TRUE.

         ELSE

            IF( XDENOM .LT. 1.0E-10 ) THEN

*             no data in the x profile - set the x position to be the
*             same as the previous iteration, and evaluate the new y
               XFINAL  =  XLAST
               YFINAL  =  YNUMER / YDENOM

            ELSE IF( YDENOM .LT. 1.0E-10 ) THEN

*             similarly, no data in the y profile - set the y position
*             equal to the previous one, and evaluate the new x
               XFINAL  =  XNUMER / XDENOM
               YFINAL  =  YLAST

            ELSE

*             both profiles had data - evaluate the new positions in
*             both x and y
               XFINAL  =  XNUMER / XDENOM
               YFINAL  =  YNUMER / YDENOM

            END IF

*          now work out the shift of the current position from the
*          starting position
            SHIFT  =  SQRT( (XFINAL-XINIT)**2 + (YFINAL-YINIT)**2 )

*          test to see if the maximum allowable shift has been exceeded
            IF( SHIFT .GT. MAXSHIFT ) THEN

*             in that case, reset the x,y position back to the initial
*             values, set the error variable, and return
               XFINAL  =  XINIT
               YFINAL  =  YINIT
               ERROR   =  2
               FINISHED  =  .TRUE.

            ELSE

*             otherwise, evaluate the shift in position since the last
*             iteration
               POSCHANGE  =  SQRT((XFINAL-XLAST)**2+(YFINAL-YLAST)**2)

*             check to see if the routine can return - first check
*             the required accuracy tolerance criterion
               IF( POSCHANGE .LE. TOLER ) THEN

*                the required tolerance has been met - return
                  FINISHED  =  .TRUE.

               ELSE IF( CURRITER .EQ. NUMITER ) THEN

*                the requested number of iterations have been done -
*                return
                  FINISHED  =  .TRUE.

               END IF


*          bottom of IF SHIFT GREATER THAN MAXSHIFT statement
            END IF

*       bottom of IF NO DATA IN PROFILES statement
         END IF

*    bottom of iteration do-loop
      END DO


*    that's it - return

      END
