*+  INPOL - Interpolates X, Y pairs of points.

      SUBROUTINE INPOL( LINDAT, MAXPTS, NPTS, DATA, DIM1, DIM2, LINE,
     :                  STATUS )
*
*    Description :
*
*     This routine interpolates the X, Y pairs of points generated
*     by the LINSET subroutine. Each point is interpolated
*     and stored in an array which can then be used for plotting.
*
*    Invocation :
*
*     CALL INPOL( LINDAT, MAXPTS, NPTS, DATA, DIM1, DIM2, LINE, STATUS )
*
*    Arguments :
*
*     LINDAT( MAXPTS, 2 ) = REAL( READ )
*         This array contains the X, Y pairs of points to
*           be interpolated.
*     MAXPTS = INTEGER( READ )
*         Maximum number of points and dimension of LINDAT and LINE.
*     NPTS = INTEGER( READ )
*         The number of X, Y points to be interpolated.
*     DATA( DIM1, DIM2 ) = REAL( READ )
*         The array containing the image data.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     LINE( MAXPTS ) = REAL( WRITE )
*         The array which will contain the interpolated values
*           ready for plotting.
*     STATUS = INTEGER( READ, WRITE )
*         Status value on entering this subroutine.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For each point to be interpolated
*        The four surrounding points in the data or image array are
*        obtained. Find the nearest pixel to the point. If the nearest
*        pixel or all the surrounding pixels are invalid, then set the
*        output slice value to be invalid as well. Otherwise the
*        fractiona-pixel displacements of the interpolation point are
*        also calculated. For each valid surrounding pixel the sum of
*        the weighted pixel values and the sum of weights are
*        incremented. The weights are for a bi-linear interpolation. The
*        output value is the weighted mean.
*     Endfor
*     If the slice includes invalid pixels then replace them by the
*     minimum and valid value in the slice, to form an array which can
*     then be used for plotting.
*
*    Authors :
*
*     C.D.Pike ( and others )
*     S.Chan
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     23 February 1981
*     26 September 1983
*     1986 Sep 22: Renamed from KFH_INTERPOLATE. Standardised to RAPI2D
*                  style; renamed parameters section to arguments and
*                  added access; removed trailing blanks; relocated
*                  'local' variables to import etc.; added status etc.
*                  and error message when there are too many points;
*                  added bad-pixel handling; added an extra argument
*                  viz. MAXPTS to make routine more general; made
*                  arrays run from 1 not element 0 and tidied
*                  (RL.STAR::CUR).
*     1988 Jun 22: Added identification to error reporting and removed
*                  NINT in calculation of LINE (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'          ! global SSE definitions
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*    Import :

      INTEGER
     :    DIM1, DIM2,
     :    MAXPTS,
     :    NPTS

      REAL
     :   DATA( DIM1, DIM2 ),
     :   LINDAT( MAXPTS, 2 )

*     Export :

      REAL LINE( NPTS )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :    I,                    ! general variable
     :    IX1, IX2, IY1, IY2,   ! general variables
     :    NUMINV,               ! number of invalid pixels in slice
     :    XCEN, YCEN            ! position of nearest pixel to each
                                ! point

      REAL
     :    F,                    ! Fractional pixel displacement of
                                ! the interpolation point
     :    G,                    ! Fractional pixel displacement of
                                ! the interpolation point
     :    MINV,                 ! Minimum valid value in the slice
     :    SUM,                  ! Sum of weighted surrounding pixels
     :    VAL1,                 ! Bottom-left point surrounding
                                ! the point to be interpolated
     :    VAL2,                 ! Bottom-right point
     :    VAL3,                 ! Top-left point
     :    VAL4,                 ! Top-right point
     :    WEIGHT,               ! Weight of a pixel for interpolation
     :    WTSUM                 ! Sum of weights

*-
*    If the status is bad,  then return to the calling program

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Check input data

      IF ( NPTS .GT. MAXPTS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NPTS', NPTS )
         CALL MSG_SETI( 'MAXPTS', MAXPTS )
         CALL ERR_REP( 'ERR_INPOL_TMP',
     :     'INPOL: Too many points (^NPTS) to fit into the buffer '/
     :     /'(^MAXPTS)', STATUS )
         GOTO 999
      END IF

      NUMINV = 0

*    LINDAT contains the X, Y pairs of points to be interpolated.

      DO  I = 1, NPTS

         IX1 = INT( LINDAT( I, 1 ) )
         IY1 = INT( LINDAT( I, 2 ) )
         IX2 = IX1 + 1
         IY2 = IY1 + 1
         XCEN = NINT( LINDAT( I, 1 ) )
         YCEN = NINT( LINDAT( I, 2 ) )

*       Get the four surrounding points in the data array.

         VAL1 = DATA( IX1, IY1 )
         VAL2 = DATA( IX2, IY1 )
         VAL3 = DATA( IX1, IY2 )
         VAL4 = DATA( IX2, IY2 )

*       For linear interpolation, output pixel is (temporarily) made
*       invalid if nearest input pixel is invalid, or all four
*       surrounding pixels are invalid. Otherwise continue with
*       interpolation.

         IF ( DATA( XCEN, YCEN ) .EQ. VAL__BADR .OR.
     :      (  VAL1 .EQ. VAL__BADR .AND. VAL2 .EQ. VAL__BADR .AND.
     :         VAL3 .EQ. VAL__BADR .AND. VAL4 .EQ. VAL__BADR ) ) THEN
            LINE ( I ) = VAL__BADR
            NUMINV = NUMINV + 1
         ELSE

*          F & G are the fractional-pixel displacements of the
*          interpolation point.

            F = LINDAT( I, 1 ) - REAL( IX1 )
            G = LINDAT( I, 2 ) - REAL( IY1 )

*          Initialise sums for forming weighted mean

            SUM = 0.0
            WTSUM = 0.0

*          Form weighted mean of adjacent four pixels, checking that
*          each lies within the input image and is not invalid

            IF ( VAL1 .NE. VAL__BADR ) THEN

*             Weight is calculated from the x,y shift from integer pixel
*             locations. Bi-linear interpolation is used. First the
*             bottom-left pixel...

               WEIGHT = ( 1.0 - F ) * ( 1.0 -  G )
               SUM = SUM + VAL1 * WEIGHT
               WTSUM = WTSUM + WEIGHT
            END IF

*          bottom right...

            IF ( VAL2 .NE. VAL__BADR ) THEN
               WEIGHT = F * ( 1.0 - G )
               SUM = SUM + VAL2 * WEIGHT
               WTSUM = WTSUM + WEIGHT
            END IF

*          top left...

            IF ( VAL3 .NE. VAL__BADR ) THEN
               WEIGHT = ( 1.0 - F ) * G
               SUM = SUM + VAL3 * WEIGHT
               WTSUM = WTSUM + WEIGHT
            END IF

*          top right...

            IF ( VAL4 .NE. VAL__BADR ) THEN
               WEIGHT = F * G
               SUM = SUM + VAL4 * WEIGHT
               WTSUM = WTSUM + WEIGHT
            END IF

*          Assign weighted mean to output pixel (WTSUM cannot
*          be zero, since at least 1 input pixel must be valid)

            LINE( I ) = SUM / WTSUM
         END IF

      END DO

*    Now to avoid scaling problems in plotting, if there are invalid
*    values in LINE then the minimum value in LINE is found, and is
*    substituted for each invalid value.

      IF ( NUMINV .GT. 0 ) THEN

         MINV = ABS( VAL__BADR )
         DO  I = 1, NPTS
            IF ( LINE( I ) .NE. VAL__BADR ) MINV = MIN( MINV, LINE(I) )
         END DO

*       On exit , LINE will contain the array of interpolated
*       values ready for plotting.

         DO  I = 1, NPTS
            IF ( LINE( I ) .EQ. VAL__BADR )  LINE( I ) = MINV
         END DO

      END IF

 999  CONTINUE

      END
