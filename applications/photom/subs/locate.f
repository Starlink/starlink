*+  LOCATE - locates the centroid of a star-like image feature in a 2-d
*            array

      SUBROUTINE LOCATE ( INARR, DIM1, DIM2, XINIT, YINIT, SEARCH,
     :                    POSTIV, MXSHFT, MAXITE, TOLER, XFINAL, YFINAL,
     :                    STATUS )
*
*    Description :
*
*     This routine locates the centroid of a star-like feature within
*     a defined search area in an image about suggested starting
*     co-ordinates, and returns the final centroid position.
*
*     The routine forms marginal profiles within a search square, and
*     then subtracts a background estimate from each profile, before
*     finding the profile centroids. This is repeated for a specified
*     number of iterations, until the requested accuracy is met, or
*     until one of several error conditions is met.
*
*    Invocation :
*
*     CALL LOCATE( INARR, DIM1, DIM2, XINIT, YINIT, SEARCH, POSTIV,
*    :             MXSHFT, MAXITE, TOLER, XFINAL, YFINAL, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*         The input data array
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XINIT = REAL( READ )
*         The x co-ordinate of the initial estimate position
*     YINIT = REAL( READ )
*         The y co-ordinate of the initial estimate position
*     SEARCH = INTEGER( READ )
*         Size of the search square to be used in pixels
*     POSTIV = LOGICAL( READ )
*         True if image features are positive
*     MXSHFT = REAL( READ )
*         Maximum shift allowable from initial position
*     MAXITE = INTEGER ( READ )
*         Maximum number of iterations to be used
*     TOLER = REAL( READ )
*         Accuracy required in centroid position
*     XFINAL = REAL( WRITE )
*         The final x co-ordinate of the centroid position
*     YFINAL = REAL( WRITE )
*         The final y co-ordinate of the centroid position
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise the finished flag
*     Initialise the square centre to be at the initial estimated
*       position
*     Constrain the search square size to between 3 and the maximum
*       allowed size, and initialise the half-box size
*     Constrain number of iterations to be positive
*     Initialise the number of iterations done so far
*     Do until completed
*        Increment the current iteration counter
*        Note the starting co-ords for this iteration
*        Work out the starting edges of the current search square
*        Initialise the arrays used to form the marginal profiles
*        For all lines
*           Scan the current search area, forming the x and y profiles
*             from all valid pixels
*           If the current line is within the image then
*              For all pixels in the current line
*                 If the current pixel is in the image then
*                    Include valid pixels in the profiles by
*                      incrementing the marginal arrays
*                 Endif
*              Enddo
*           Endif
*        Enddo
*        For all profile bins
*           If number of valid pixels contributing to the current bin
*             of the x marginal array is empty then
*              Make current bin very large
*           Else
*             Normalise the current y bin value with apropriate sign
*               depending on whether data are positive or negative
*               w.r.t. background pixels
*           Endif
*           If number of valid pixels contributing to the current bin
*             of the y marginal array is empty then
*              Make current bin very large
*           Else
*             Normalise the current y bin value with apropriate sign
*               depending on whether data are positive or negative
*               w.r.t. background pixels
*           Endif
*        Enddo
*        Call the subroutine NTHMIN to find the lower quartile
*          point in each profile as a background estimate
*        Get the n smallest numbers in the x profile, and store
*          the n-th smallest as the background
*        Get the n smallest numbers in the y profile, and store
*          the n-th smallest as the background
*        Initialise the sums used for forming the centroids
*        For all profile bins
*          scan the profiles, using all data above the estimated
*           backgrounds to form sums for the centroids
*           Increment the current values of the scanning positions
*           If there are data in the current x bin then
*              Sum weights and moment for x centroid
*           Endif
*           If there are data in the current y bin then
*              Sum weights and moment for y centroid
*           Endif
*           If neither profile has any data - set the x,y positions back
*             to the starting values, report the error and set an error
*             status, and return
*           Else
*              If no data in the x profile - set the x position to be
*                the same as the previous iteration, and evaluate the
*                new y similarly, no data in the y profile - set the y
*                position equal to the previous one, and evaluate the
*                new x
*              Else
*                 Both profiles had data - evaluate the new positions in
*                   both x and y
*              Endif
*              Work out the shift of the current position from the
*                starting position
*              If the maximum allowable shift has been exceeded then
*                 Reset the x,y position back to the initial
*                   values, set an error status and report the error
*              Else
*                 Evaluate the shift in position since the last
*                   iteration
*                 If the shift is less than the accuracy tolerance then
*                   Set finished flag
*                 Else if the number of iterations has been done then
*                    Set finished flag
*                 Endif
*              Endif
*           Endif
*        Endif
*     Enddo
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*     Nick Eaton  Durham University (DUVAD::NE)
*
*    History :
*
*     13-01-1986 : First Rapi2d implementation - direct copy from
*                : Rodney Warren-Smith's EDRS routine of same name.
*                : (REVA::MJM)
*     1986 Aug 14: Completed prologue, partially conformed to Starlink
*                  programming standards (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section to arguments, applied
*                  bad-pixel handling via global constants rather
*                  than by argument (RL.STAR::CUR).
*     1988 Jul 5 : Repeat of 1988 May changes, viz. removed ERROR
*                  argument, improved "Method :" section, and added
*                  error reporting (RL.STAR::CUR).
*     1989 Jul 27: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1990 Jan 19: No centroid found leaves final co-ordinates undefined
*                  rather than equal to the initial co-ordinates
*                  (RL.STAR::CUR).
*     1990 Mar 5 : Output co-ordinates conform to the Starlink standard
*                  instead of being pixel indices as before (RAL::CUR).
*     1990 Aug 1 : Fixed the previous change, which was incorrect
*                  (RAL::CUR).
*     1992 Jan 7 : Check denominators in moment calculation are non-zero
*                  (DUVAD::NE).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! SSE global definitions

      INCLUDE 'PRM_PAR'       ! Magic-value and extreme constants


*    Import :

      INTEGER
     :  DIM1, DIM2,
     :  SEARCH,
     :  MAXITE

      REAL
     :  INARR( DIM1, DIM2 ),
     :  XINIT,
     :  YINIT,
     :  MXSHFT,
     :  TOLER

      LOGICAL
     :  POSTIV

*    Export :

      REAL
     :  XFINAL,
     :  YFINAL

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :  SRCHMX                 ! Maximum search square size allowed
      PARAMETER( SRCHMX  =  51 )

*    Local variables :

      INTEGER
     :  NSAMPL,                ! number of lines/columns in search
                               ! square actually used
     :  HLFSIZ,                ! Half the above (truncated NSAMPL/2)
     :  NUMITE,                ! Maximum number of iterations to be
                               ! used, forced to be at least one
                               ! iteration
     :  CURITE,                ! Current iteration counter
     :  ISTART,                ! x co-ord of edge of current search box
     :  JSTART,                ! y   "    "   "   "    "       "    "
     :  IPOSN,                 ! x co-ord of current column being added
     :  JPOSN,                 ! y   "    "    "       "     "     "
     :  NXAV( SRCHMX ),        ! Used to count additions to x profile
     :  NYAV( SRCHMX ),        !   "   "   "       "      " y    "
     :  NTH,                   ! Value used to determine background
                               ! level - Nth smallest number taken
     :  I, J, K, L, M          ! Counters

      REAL
     :  XLAST,                 ! x co-ord worked out in last iteration
     :  YLAST,                 ! y   "      "    "   "   "      "
     :  XBACK,                 ! Background value used in x direction
     :  YBACK,                 !      "       "     "   " y     "
     :  XAV( SRCHMX ),         ! Used to sum x profile
     :  YAV( SRCHMX ),         !   "   "  "  y    "
     :  STACK( SRCHMX/4 + 2 ),
                               ! Stack used by background estimator
     :  XPOSN,                 ! Real x position used in scanning
     :  YPOSN,                 !   "  y     "      "   "     "
     :  XNUMER,                ! Numerator in x centroid calculation
     :  YNUMER,                !     "      " y     "         "
     :  XDENOM,                ! Denominator  x     "         "
     :  YDENOM,                !      "       y     "         "
     :  DUMMY,                 ! Dummy variable used in centroiding
     :  SHIFT,                 ! Shift of current posn from initial one
     :  POSCHG                 !   "    "    "      "    "  previous "

      LOGICAL                  ! If true:
     :  FINSHD,                ! Calculation finished
     :  XEVAL,                 ! An x position can be evaluated
     :  YEVAL                  ! A y position can be evaluated

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN
*      WRITE(*,*) ' DEBUG --- --- --- RUNNING LOCATE()'
*          WRITE(*,*) ' DEBUG --- --- --- NX     = ', DIM1
*          WRITE(*,*) ' DEBUG --- --- --- NY     = ', DIM2
*          WRITE(*,*) ' DEBUG --- --- --- XINIT  = ', XINIT
*          WRITE(*,*) ' DEBUG --- --- --- YINIT  = ', YINIT
*          WRITE(*,*) ' DEBUG --- --- --- DCEN   = ', SEARCH
*          WRITE(*,*) ' DEBUG --- --- --- POSTVE = ', POSTIV
*          WRITE(*,*) ' DEBUG --- --- --- MXSHFT = ', MXSHFT
*          WRITE(*,*) ' DEBUG --- --- --- MXITER = ', MAXITE

*    Initialise the FINSHD flag

      FINSHD  =  .FALSE.

*    initialise the square centre to be at the initial estimated
*    position

      XFINAL  =  XINIT
      YFINAL  =  YINIT

*    make sure the search square size is between 3 and the maximum
*    allowed size, and initialise the half-box size

      NSAMPL  =  MIN( MAX( 3, SEARCH ), SRCHMX )
      HLFSIZ  =  INT( NSAMPL / 2.0 )

*    check that at least one iteration is done

      NUMITE   =  MAX( 1, MAXITE )

*    initialise the number of iterations done so far, and start
*    the main loop

      CURITE  =  0

      DO WHILE ( .NOT. FINSHD )

*       increment the current iteration counter

         CURITE  =  CURITE + 1

*       note the starting co-ords for this iteration

         XLAST  =  XFINAL
         YLAST  =  YFINAL

*       work out the starting edges of the current search square

         ISTART  =  NINT(MIN( MAX(-1.0E6,XFINAL), 1.0E6) ) - (HLFSIZ+1)
         JSTART  =  NINT(MIN( MAX(-1.0E6,YFINAL), 1.0E6) ) - (HLFSIZ+1)

*       initialise the arrays used to form the marginal profiles

         DO  K  =  1, NSAMPL
            XAV( K )  =  0.0
            YAV( K )  =  0.0
            NXAV( K ) =  0
            NYAV( K ) =  0
         END DO

*       scan the current search area, forming the x and y profiles from
*       all valid pixels - scan all lines

         DO  J  =  1, NSAMPL
            JPOSN  =  JSTART + J

*          check that the current line is within the image

            IF ( ( JPOSN .GE. 1 ) .AND. ( JPOSN .LE. DIM2 ) ) THEN

*             now scan all pixels in the current line

               DO  I  =  1, NSAMPL
                  IPOSN  =  ISTART + I

*                check that the current pixel is in the image

                  IF ( ( IPOSN .GE. 1 ) .AND.
     :                 ( IPOSN .LE. DIM1 ) ) THEN

*                   include valid pixels in the profiles

                     IF ( INARR( IPOSN, JPOSN ) .NE. VAL__BADR ) THEN

*                      increment the arrays accordingly

                        XAV( I )  = XAV( I ) + INARR( IPOSN, JPOSN )
                        YAV( J )  = YAV( J ) + INARR( IPOSN, JPOSN )
                        NXAV( I ) = NXAV( I ) + 1
                        NYAV( J ) = NYAV( J ) + 1

                     END IF
                  END IF

*             End of loop for all pixels in the current line

               END DO

            END IF

         END DO

*       now evaluate the profile bins that contain at least one
*       valid pixel - invert results if POSTIVE is false

         DO  L  =  1, NSAMPL

*          first the x profile -
*          check for nil pixels having been added to the current bin

            IF ( NXAV( L ) .EQ. 0 ) THEN

*             In that case, make the current bin very large so that it
*             is excluded from the quartile.

               XAV( L )  =  VAL__MAXR

            ELSE

*             Normalise the current bin value, checking POSTIV.

               IF ( POSTIV ) THEN
                  XAV( L )  =  XAV( L ) / NXAV( L )
               ELSE
                  XAV( L )  = -XAV( L ) / NXAV( L )
               END IF

            END IF

*          repeat for the y profile -
*          check for nil pixels having been added to the current bin

            IF ( NYAV( L ) .EQ. 0 ) THEN

*             in that case, make the current bin very large so that it
*             is excluded from the quartile.

               YAV( L )  =  VAL__BADR

            ELSE

*             normalise the current bin value, checking POSTIV

               IF ( POSTIV ) THEN
                  YAV( L )  =  YAV( L ) / NYAV( L )
               ELSE
                  YAV( L )  = -YAV( L ) / NYAV( L )
               END IF

            END IF

         END DO

*       now call the subroutine NTHMIN to find the lower quartile
*       point in each profile as a background estimate - first
*       evaluate the number NTH

         NTH  =  MAX( INT( NSAMPL/4 ), 2 )

*       now get the n smallest numbers in the x profile, and put
*       the n-th smallest into XBACK

         CALL NTHMIN( XAV, NSAMPL, NTH, STACK, STATUS )
         XBACK  =  STACK( 1 )

*       similarly for the y profile, value placed in YBACK

         CALL NTHMIN( YAV, NSAMPL, NTH, STACK, STATUS )
         YBACK  =  STACK( 1 )

*       Now initialise the sums used for forming the centroids.

         XNUMER  =  0.0
         XDENOM  =  0.0
         XPOSN   =  REAL( ISTART )
         YNUMER  =  0.0
         YDENOM  =  0.0
         YPOSN   =  REAL( JSTART )

*       Assume for the moment that the x-y position cannot be evaluated.

         XEVAL  = .FALSE.
         YEVAL  = .FALSE.

*       scan the profiles, using all data above the estimated
*       backgrounds to form sums for the centroids

         DO  M  =  1, NSAMPL

*          increment the current values of the scanning positions

            XPOSN  =  XPOSN + 1.0
            YPOSN  =  YPOSN + 1.0

*          x first - check for there being data in the current bin

            IF ( NXAV( M ) .NE. 0 ) THEN
               XEVAL  = .TRUE.
               DUMMY  =  MAX( XAV( M ) - XBACK, 0.0 )
               XNUMER =  XNUMER + ( XPOSN * DUMMY )
               XDENOM =  XDENOM + DUMMY
            END IF

*          now y - check for there being data in the current bin

            IF ( NYAV( M ) .NE. 0 ) THEN
               YEVAL  = .TRUE.
               DUMMY  =  MAX( YAV( M )-YBACK, 0.0 )
               YNUMER =  YNUMER + ( YPOSN * DUMMY )
               YDENOM =  YDENOM + DUMMY
            END IF

         END DO

*       now form the current centroid positions, checking for either
*       profile not containing data

         IF ( ( ( .NOT. XEVAL ) .AND. ( .NOT. YEVAL ) ) ) THEN

*          neither profile has any data - set the x,y positions back
*          to the starting values, set an error status, and return

            XFINAL  =  VAL__BADR
            YFINAL  =  VAL__BADR

            STATUS  =  SAI__ERROR
            CALL ERR_REP( 'ERR_LOCATE_NODATA',
     :        'LOCATE: Both x and y profiles contain insufficient data',
     :        STATUS )
            FINSHD  =  .TRUE.

         ELSE IF ( ( XEVAL .AND. ( XDENOM .LT. VAL__SMLR ) ) .OR.
     :             ( YEVAL .AND. ( YDENOM .LT. VAL__SMLR ) ) ) THEN

*          there was no data above/below the background - set the x,y
*          positions back to the starting values, set an error status,
*          and return

            XFINAL  =  VAL__BADR
            YFINAL  =  VAL__BADR

            STATUS  =  SAI__ERROR
            IF ( POSTIV ) THEN
               CALL MSG_SETC( 'SIGN', 'above' )
            ELSE
               CALL MSG_SETC( 'SIGN', 'below' )
            ENDIF
            CALL ERR_REP( 'ERR_LOCATE_NOOBJ',
     :       'LOCATE: Centroid ^SIGN the background could not be found',
     :        STATUS )
            FINSHD  =  .TRUE.

         ELSE

            IF ( .NOT. XEVAL ) THEN

*             no data in the x profile - set the x position to be the
*             same as the previous iteration, and evaluate the new y.
*             Note the '-0.5's below convert from pixel index into
*             co-ordinate.

               XFINAL  =  XLAST
               YFINAL  =  YNUMER / YDENOM - 0.5

            ELSE IF ( .NOT. YEVAL ) THEN

*             similarly, no data in the y profile - set the y position
*             equal to the previous one, and evaluate the new x

               XFINAL  =  XNUMER / XDENOM - 0.5
               YFINAL  =  YLAST

            ELSE

*             both profiles had data - evaluate the new positions in
*             both x and y

               XFINAL  =  XNUMER / XDENOM - 0.5
               YFINAL  =  YNUMER / YDENOM - 0.5

            END IF

*          now work out the shift of the current position from the
*          starting position

            SHIFT  =  SQRT( (XFINAL-XINIT)**2 + (YFINAL-YINIT)**2 )

*          test to see if the maximum allowable shift has been exceeded

            IF ( SHIFT .GT. MXSHFT ) THEN

*             in that case, reset the x,y position back to the initial
*             values, set an error status, and return

               XFINAL  =  VAL__BADR
               YFINAL  =  VAL__BADR

               CALL MSG_SETR( 'SHIFT', SHIFT )
               STATUS  =  SAI__ERROR
               CALL ERR_REP( 'ERR_LOCATE_NODATA',
     :           'LOCATE: Maximum allowable shift has been exceeded. '/
     :           /'Current shift is ^SHIFT', STATUS )
               FINSHD  =  .TRUE.

            ELSE

*             otherwise, evaluate the shift in position since the last
*             iteration

               POSCHG  =  SQRT( (XFINAL-XLAST)**2 + (YFINAL-YLAST)**2 )

*             check to see if the routine can return - first check
*             the required accuracy tolerance criterion

               IF ( POSCHG .LE. TOLER ) THEN

*                the required tolerance has been met - return

                  FINSHD  =  .TRUE.

               ELSE IF ( CURITE .EQ. NUMITE ) THEN

*                the requested number of iterations have been done -
*                return

                  FINSHD  =  .TRUE.

               END IF


*          bottom of if-shift-greater-than-MXSHFT statement

            END IF

*       bottom of if-no-data-in-profiles statement

         END IF

*    bottom of iteration do-loop

      END DO
*      WRITE(*,*) ' DEBUG --- --- --- STATUS = ', STATUS
*      WRITE(*,*) ' DEBUG --- --- --- Leaving LOCATE()'

*    that's it - return

      END
* $Id$
