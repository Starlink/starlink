      SUBROUTINE SURFLIB_CALC_CHOPPED_IMAGE( NBEAMS, CHOP_THR, CHOP_PA,
     :     DIM1, DIM2, IN_DATA, OUT_DATA, USEVAR, IN_VAR, OUT_VAR,
     :     STATUS)
*+
*  Name:
*     SURFLIB_CALC_DUAL_BEAM

*  Purpose:
*     Calculates a chopped map from a single beam map

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_CALC_CHOPPED_IMAGE( NBEAMS, CHOP_THR, CHOP_PA,
*    :     DIM1, DIM2, IN_DATA, OUT_DATA, USEVAR, IN_VAR, OUT_VAR,
*    :     STATUS)

*  Description:
*     This routine can be used to added a chopped beam response
*     to a single beam image. When NBEAMS=2 this routine
*     calculates a middle-beam dual-beam map by
*     calculating the difference between the left and right
*     beams for each pixel in the map.
*
*     When NBEAMS=3 a triple beam map is constructed where the central
*     beam is the difference between the central beam and the average of
*     the positions a chop distance away on either side.

*     All pixels off the edge of the image are assumed to
*     have flux 0 and variance 0.
*
*     Bad pixels are assumed to be equivalent to flux of 0
*     and variance 0, although for the triple beam image a bad pixel
*     is retained if it coincide with the middle beam position.

*  Arguments:
*     NBEAMS = INTEGER (Given)
*        Number of beams to add. (2 or 3)
*     CHOP_THR = REAL (Given)
*        Chop throw in pixels
*     CHOP_PA  = REAL (Given)
*        Chop position angle (east of north) in degrees
*     DIM1 = INTEGER (Given)
*        First dimension of image
*     DIM2 = INTEGER (Given)
*        Second dimension of image [can not use DIMS as an array
*        since the linux fortran compiler doesnt like it]
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


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.5  2004/09/01 01:06:58  timj
*     fix uninitialised warnings
*
*     Revision 1.4  1999/08/19 03:37:48  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.3  1999/08/03 19:32:47  timj
*     Add copyright message to header.
*
*     Revision 1.2  1999/07/15 01:45:33  timj
*     Correct check at subroutine entry
*
*     Revision 1.1  1999/07/14 21:52:58  timj
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
      INTEGER DIM1
      INTEGER DIM2
      REAL    IN_DATA( DIM1, DIM2 )
      REAL    IN_VAR ( DIM1, DIM2 )
      INTEGER NBEAMS
      LOGICAL USEVAR

*  Arguments returned:
      REAL    OUT_DATA( DIM1, DIM2 )
      REAL    OUT_VAR ( DIM1, DIM2 )

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL    PI
      PARAMETER (PI = 3.14159265359)

*  Local variables:
      REAL    CFRAC             ! fractional chop distance from middle to left
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

      L_VAR = 0.0
      R_VAR = 0.0

*     Check that NBEAMS is in range
      IF (NBEAMS .NE. 2 .AND. NBEAMS .NE. 3) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('NB', NBEAMS)
         CALL ERR_REP(' ','SURFLIB_CALC_CHOPPED_IMAGE: Number of '//
     :        'beams must be either 2 or 3 (not ^NB)', STATUS)
      END IF


*     For dual beam the middle beam is half a chop from the
*     left and right. For triple-beam the left and right are a
*     chop distance away.

      IF (NBEAMS .EQ. 2) THEN
         CFRAC = 0.5
      ELSE
         CFRAC = 1.0
      END IF

*     Calculate the pixel offset for the positive beam
*     (This is constant for a given pa and throw)
*     This has to be to the nearest pixel

      DY = NINT( COS( CHOP_PA * PI / 180.0) * CHOP_THR * CFRAC )
      DX = NINT( SIN( CHOP_PA * PI / 180.0) * CHOP_THR * CFRAC )

*     Start looping over X and Y

      DO Y = 1, DIM2
         DO X = 1, DIM1

*     Calculate the pixel numbers for the Left and Right beams
            LX = X - DX
            LY = Y + DY
            RX = X + DX
            RY = Y - DY

*     Calculate the flux at each beam remembering that
*     bad pixels and pixels outside the bounds have flux 0
*     and variance zero

*     Left flux
            IF (LX .LT. 1 .OR. LX .GT. DIM1 .OR.
     :           LY .LT. 1 .OR. LY .GT. DIM2 ) THEN
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

            IF (RX .LT. 1 .OR. RX .GT. DIM1 .OR.
     :           RY .LT. 1 .OR. RY .GT. DIM2 ) THEN
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
            IF (NBEAMS .EQ. 2) THEN

               OUT_DATA(X,Y) = L_FLUX - R_FLUX
               IF (USEVAR) OUT_VAR(X,Y)  = L_VAR + R_VAR

            ELSE

*     The triple beam flux is the difference between the middle
*     beam and the average of the two off beams

               IF (IN_DATA(X,Y) .EQ. VAL__BADR) THEN
                  OUT_DATA(X,Y) = VAL__BADR
               ELSE
                  OUT_DATA(X,Y) = IN_DATA(X,Y) - ((L_FLUX + R_FLUX)/2.0)
               END IF

               IF (USEVAR) THEN
                  IF (IN_VAR(X,Y) .EQ. VAL__BADR) THEN
                     OUT_VAR(X,Y) = VAL__BADR
                  ELSE
                     OUT_VAR(X,Y)  = L_VAR + R_VAR + IN_VAR(X,Y)
                  END IF
               END IF

            END IF

         END DO
      END DO

      END
