      SUBROUTINE SURFLIB_CALC_DUAL_BEAM( CHOP_THR, CHOP_PA, DIMS,
     :     IN_DATA, OUT_DATA, USEVAR, IN_VAR, OUT_VAR, STATUS)
*+
*  Name:
*     SURFLIB_CALC_DUAL_BEAM

*  Purpose:
*     Calculates a dual beam map from a single beam map

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURFLIB_CALC_DUAL_BEAM( CHOP_THR, CHOP_PA, DIMS,
*    :     IN_DATA, OUT_DATA, USEVAR, IN_VAR, OUT_VAR, STATUS)

*  Description:
*     This routine calculates a middle-beam dual-beam map by
*     calculating the difference between the left and right
*     beams for each pixel in the map.
*     
*     All pixels off the edge of the image are assumed to
*     have flux 0 and variance 0.
*
*     Bad pixels are assumed to be equivalent to flux of 0
*     and variance 0.

*  Arguments:
*     CHOP_THR = REAL (Given)
*        Chop throw in pixels
*     CHOP_PA  = REAL (Given)
*        Chop position angle (east of north) in degrees
*     DIMS ( 2 ) = INTEGER (Given)
*        Dimensions of image
*     IN_DATA = REAL (Given)
*        Input single-beam image
*     OUT_DATA = REAL (Returned)
*        Output dual-beam image
*     USEVAR = LOGICAL (Given)
*        Are we processing a variance array?
*     IN_VAR = REAL (Given)
*        Input variance image (if USEVAR=TRUE)
*     OUT_VAR = REAL (Returned)
*        Output variance image (if USEVAR=TRUE)
*     STATUS = INTEGER (Given and Returned)
*        Global status

*  Authors:
*     Tim Jenness (JACH)

*  History:
*     $Log$
*     Revision 1.1  1999/01/12 02:52:01  timj
*     First version
*

*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
 
*  Arguments Given:
      REAL    CHOP_THR
      REAL    CHOP_PA
      INTEGER DIMS(2)
      REAL    IN_DATA( DIMS(1), DIMS(2) )
      REAL    IN_VAR ( DIMS(1), DIMS(2) )
      LOGICAL USEVAR

*  Arguments returned:
      REAL    OUT_DATA( DIMS(1), DIMS(2) )
      REAL    OUT_VAR ( DIMS(1), DIMS(2) )

*  Status:
      INTEGER STATUS
 
*  Local variables:
      INTEGER DX                ! X Chop offset
      INTEGER DY                ! Y Chop offset

      INTEGER LX                ! X position of left beam
      INTEGER LY                ! Y position of left beam
      REAL    L_FLUX            ! Flux in left beam
      REAL    L_VAR             ! variance in left beam

      INTEGER RX                ! X position of right beam
      INTEGER RY                ! Y position of right beam
      REAL    R_FLUX            ! Flux in right beam
      REAL    R_VAR             ! variance in right beam

      INTEGER X                 ! X-pixel position
      INTEGER Y                 ! Y-pixel position

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Calculate the pixel offset for the positive beam
*     (This is constant for a given pa and throw)
*     This has to be to the nearest pixel

      DY = NINT( COSD( CHOP_PA ) * CHOP_THR / 2.0 )
      DX = NINT( SIND( CHOP_PA ) * CHOP_THR / 2.0 )

*     Start looping over X and Y

      DO Y = 1, DIMS(2)
         DO X = 1, DIMS(1)

*     Calculate the pixel numbers for the Left and Right beams
            LX = X - DX
            LY = Y + DY
            RX = X + DX
            RY = Y - DY

*     Calculate the flux at each beam remembering that 
*     bad pixels and pixels outside the bounds have flux 0
*     and variance zero

*     Left flux
            IF (LX .LT. 1 .OR. LX .GT. DIMS(1) .OR.
     :           LY .LT. 1 .OR. LY .GT. DIMS(2) ) THEN
               L_FLUX = 0.0 
               L_VAR  = 0.0
            ELSE
               L_FLUX = IN_DATA(LX,LY)
               IF (USEVAR) L_VAR  = IN_VAR(LX,LY)

*     Convert bad pixels to zero flux/var
               IF (L_FLUX .EQ. VAL__BADR) L_FLUX = 0.0
               IF (L_VAR  .EQ. VAL__BADR) L_VAR  = 0.0

            END IF

*     Right flux

            IF (RX .LT. 1 .OR. RX .GT. DIMS(1) .OR.
     :           RY .LT. 1 .OR. RY .GT. DIMS(2) ) THEN
               R_FLUX = 0.0 
               R_VAR  = 0.0
            ELSE
               R_FLUX = IN_DATA(RX,RY)
               IF (USEVAR) R_VAR  = IN_VAR(RX,RY)

*     Convert bad pixels to zero flux/var
               IF (R_FLUX .EQ. VAL__BADR) R_FLUX = 0.0
               IF (R_VAR  .EQ. VAL__BADR) R_VAR  = 0.0

            END IF

*     The middle beam flux is then simply the difference between
*     the left and the right
               
            OUT_DATA(X,Y) = L_FLUX - R_FLUX
            IF (USEVAR) OUT_VAR(X,Y)  = L_VAR + R_VAR

         END DO
      END DO

      END
