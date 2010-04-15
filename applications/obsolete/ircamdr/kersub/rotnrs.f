*+  ROTNRS - rotates an image through an angle not divisible by 90.0

      SUBROUTINE ROTNRS ( INARRAY, IDIMS1, IDIMS2, OUTARRAY,
     :                     ODIMS1, ODIMS2, ANGLE, STATUS )

*    Description :
*
*     This routine takes an input array and rotates it clockwise by
*     an angle between 0.0 and 90.0 degrees into the output array. To
*     do this without leaving gaps in the output array due to real
*     to integer truncation when transforming old to new pixels, the
*     method adopted is to calculate for each pixel in the output array,
*     which pixel in the input array is nearest to the calculated pre-
*     transform position, and using its value. This may lead to the
*     occasional instance where two adjacent pixels in the new array
*     are calculated to have come from the same pixel in the old array.
*     This implies some slight degradation of resolution. Perhaps a
*     better implementation should use bilinear interpolation of the
*     surrounding points.
*
*    Invocation :
*
*     CALL ROTNRS ( INARRAY, IDIMS, OUTARRAY, ODIMS, ANGLE, STATUS )
*
*    Method :
*
*     <description of how the subroutine works>
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Konrad Flamm UoE ( REVA::MJM )
*
*    History :
*
*     14-11-1985 :  First implementation for ROTATE (REVA::MJM)
*     17-July-1994  Converted angles to radians to avoid VAX-specific
*                   trig functions, changed arguments to input DIMS
*                   separately so that routine will still compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMS1,         ! dimensions of input array
     :    IDIMS2,         ! dimensions of input array
     :    ODIMS1,         !      "      " output
     :    ODIMS2          !      "      " output  "

      REAL
     :    INARRAY( IDIMS1, IDIMS2 ),    ! input array
     :    ANGLE               ! rotation angle in degrees

*    Export :

      REAL
     :    OUTARRAY( ODIMS1, ODIMS2 )   ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    IIP,          ! pixel in input array corresponding
     :    IJP,          ! to the current output array pixel
     :    I, J          ! array counters

      REAL
     :    ICX,          ! x coordinate of input array centre
     :    ICY,          ! y     "       "   "     "      "
     :    OCX,          ! x     "       " output  "      "
     :    OCY,          ! y     "       "    "    "      "
     :    RI,           ! real x distance of current output array pixel
                        ! from output array centre
     :    RJ,           ! real y distance of current output array pixel
                        ! from output array centre
     :    RIP,          ! real x distance of corresponding input array
                        ! point from input array centre
     :    RJP,          ! real y distance of corresponding input array
                        ! point from input array centre
     :    CANGLE,       ! cosine of the rotation angle
     :    SANGLE,       ! sine of the rotation angle
     :    DTOR          ! factor fro converting degrees to radians

      PARAMETER ( DTOR = 3.141592 / 180.0 )

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    set up the real coordinates of the two array centres
      ICX  =  REAL( IDIMS1 ) / 2.0
      ICY  =  REAL( IDIMS2 ) / 2.0
      OCX  =  REAL( ODIMS1 ) / 2.0
      OCY  =  REAL( ODIMS2 ) / 2.0

*    set up the sine and cosine of the rotation angle
*    first convert degrees to radians
!      ANGLE = ANGLE * DTOR
*
      CANGLE  =  COS( ANGLE )
      SANGLE  =  SIN( ANGLE )
*	type *, 'cangle, sangle = ', cangle, sangle

*    loop round each row in the output array
      DO  J  =  1, ODIMS2

*       set up real y distance of current row from output array centre
         RJ  =  REAL( J ) - 0.5 - OCY

*       loop round each pixel in current row
         DO  I  =  1, ODIMS1

*          set up real x distance of current point from output array
*          centre
            RI  =  REAL( I ) - 0.5 - OCX

*          get the real coordinates in the input array that the current
*          output array coordinates have been transformed from
            CALL OLDCOORDS( RI, RJ, CANGLE, SANGLE, RIP, RJP, STATUS )

*          convert these real distances into integer pixel coordinates
*          (eg: 56,45) - add the input array centre coordinate first,
*          then another 0.5, and then take the nearest integer
            IIP  =  NINT( RIP + ICX + 0.5 )
            IJP  =  NINT( RJP + ICY + 0.5 )

*          now determine if this calculated point exists in the input
*          array - if so, set output array pixel to that value; if not,
*          set output array pixel to zero
            IF( IIP .GE. 1 .AND. IIP .LE. IDIMS1 .AND.
     :          IJP .GE. 1 .AND. IJP .LE. IDIMS2 ) THEN

               OUTARRAY( I, J )  =  INARRAY( IIP, IJP )

            ELSE

               OUTARRAY( I, J )  =  0.0

            ENDIF

         ENDDO
      ENDDO

*    that's it - return

      END
