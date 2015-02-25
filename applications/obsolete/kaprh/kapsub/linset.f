*+  LINSET - Calculates the X, Y positions of points ready for
*            interpolation.

      SUBROUTINE LINSET( X1, Y1, X2, Y2, MAXPTS, LINDAT, NPTS, STATUS )
*
*    Description :
*
*     This routine calculates the X, Y positions of points at radii
*     of integral pixel spacings from the first point.
*
*    Invocation :
*
*     CALL LINSET( X1, Y1, X2, Y2, MAXPTS, LINDAT, NPTS, STATUS )
*
*    Arguments :
*
*     X1 = REAL( READ )
*           The X co-ordinate of the first point from
*           which the slice will be taken.
*     Y1 = REAL( READ )
*           The Y co-ordinate of the first point from
*           which the slice will be taken.
*     X2 = REAL( READ )
*           The X co-ordinate of the second point to
*           which the slice will be taken.
*     Y2 = REAL( READ )
*           The Y co-ordinate of the second point to
*           which the slice will be taken.
*     MAXPTS = INTEGER( READ )
*           Maximum number of points.
*     LINDAT( MAXPTS, 2 ) = REAL( WRITE )
*           The array which will contain the X, Y
*           positions of points.
*     NPTS = INTEGER( WRITE )
*           The number of points in the array.
*     STATUS = INTEGER( READ, WRITE )
*           Status value on entering this subroutine.
*
*    Method :
*
*     The arc tangent of the difference between the Y co-ordinates
*     divided by the difference between the X co-ordinates is
*     calculated. The sine and cosine of this result is then found.
*     The X, Y positions of points at radii of integral pixel spacings
*     from the first point are then calculated.  The final point is
*     removed if it lies beyond the slice limits.
*
*    Authors :
*     C.D.Pike ( and others )
*     S.Chan
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     23 February 1981
*     26 September 1983
*     1986 Sep 20 : Renamed from KFH_LINSET. Standardised to RAPI2D
*                   style; renamed parameters section to arguments and
*                   added access; removed trailing blanks; relocated
*                   'local' variables to import etc.; added status etc.
*                   and error message when there are too many points;
*                   added an extra argument - MAXPTS to make routine
*                   more general; made arrays run from 1 not element 0
*                   and tidied (RL.STAR::CUR).
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*     1989 Sep 19 : Fixed bug that caused point X1, Y1 to be omitted
*                   from the slice (RL.STAR::CUR).
*     1989 Oct 21 : More careful calculation of the number of points
*                   in the slice (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER MAXPTS

      REAL
     :    X1, X2,
     :    Y1, Y2

*    Export :

      REAL LINDAT( MAXPTS, 2 )

      INTEGER NPTS

*    Status :

      INTEGER STATUS

*    Local variables :

      REAL
     :    A,                    ! difference between the Y co-ordinates
     :    B,                    ! difference between the X co-ordinates
     :    CT,                   ! Cosine of the THETA
     :    EXPR,                 ! The sum of the squares of the
                                ! differences between the X co-ordinates
                                ! and the differences between the Y
                                ! co-ordinates
     :    ST,                   ! Sine of the THETA
     :    SEP,                  ! The square root of EXPR
     :    THETA                 ! The angle calculated from taking
                                ! the arc tan of the differences
                                ! between the X co-ordinates and
                                ! the Y co-ordinates
      INTEGER
     :    I                     ! general variable

      LOGICAL                   ! True if:
     :    POSX,                 ! Slice goes from left to right
     :    POSY                  ! Slice goes from bottom to top

*-

*    If the status is bad,  then return to the calling program.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Find transformation coefficents.

      A = Y2 - Y1
      B = X2 - X1
      THETA = ATAN2( A, B )
      CT = COS( THETA )
      ST = SIN( THETA )

*    Obtain the number of points.  The delta allows for rounding errors.
*    Its value was obtained assuming a maximum array size of 4096 * 4096
*    pixels.

      EXPR = B * B + A * A
      SEP = SQRT( EXPR )
      NPTS = INT( SEP + 5.E-3 ) + 1

*    Slice is too long.

      IF ( NPTS .GT. MAXPTS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NPTS', NPTS )
         CALL MSG_SETI( 'MAXPTS', MAXPTS )
         CALL ERR_REP( 'ERR_LINSET_TMP',
     :     'LINSET: Too many points (^NPTS) to fit into the buffer'/
     :     /'(^MAXPTS)', STATUS )
         GOTO 999
      ENDIF

*    Calculate the X, Y positions of points at radii of
*    integral pixel spacings from the first point.

      DO  I = 1, NPTS
         LINDAT( I, 1 ) = REAL( I - 1 ) * CT + X1
         LINDAT( I, 2 ) = REAL( I - 1 ) * ST + Y1
      END DO

*    Check for the final slice point being beyond the limits.

      POSX = X2 .GT. X1
      POSY = Y2 .GT. Y1
      IF ( ( POSX .AND. LINDAT( NPTS, 1 ) .GT. X2 ) .OR.
     :     ( POSY .AND. LINDAT( NPTS, 2 ) .GT. Y2 ) .OR.
     :     ( .NOT. POSX .AND. LINDAT( NPTS, 1 ) .LT. X2 ) .OR.
     :     ( .NOT. POSY .AND. LINDAT( NPTS, 2 ) .LT. Y2 ) ) THEN
         NPTS = NPTS - 1
      END IF

 999  CONTINUE

      END
