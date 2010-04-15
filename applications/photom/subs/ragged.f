************************************************************************

      SUBROUTINE RAGGED ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                    USEMSK, NXL, NXH, NYL, NYH, XCEN, YCEN, A2,
     :                    A3, E, THETA, VALUES, NV, SKY, SIGMA, VSKY )

*+
*  Name :
*     RAGGED
*
*  Purpose :
*     This finds the sky value from a choice of three estimators
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL RAGGED ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR, MASK, USEMSK,
*    :              NXL, NXH, NYL, NYH, XCEN, YCEN, A2, A3, E, THETA,
*    :              VALUES, NV, SKY, SIGMA, VSKY )
*
*  Description :
*     This finds the sky value from a choice of three estimators defined
*     by SKYEST. Possible choices are
*     1 - simple mean
*     2 - mean with 2 sigma rejection
*     3 - mode
*     A ragged elliptical annulus is used to define the pixels
*     in the aperture. The pixel is chosen if its centre lies between
*     the inner and outer elliptical radii.
*
*  Arguments :
*     SKYEST = INTEGER (Given)
*        Defines the estimator to be used
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     IMVAR( NX, NY ) = REAL (Given)
*        Array containing image variance
*     USEVAR = LOGICAL (Given)
*        Flag indicating use of variance array
*     MASK( NX, NY ) = REAL (Given)
*        Mask array
*     USEMSK = LOGICAL (Given)
*        Flag indicating use of mask
*     NXL = INTEGER (Given)
*        X value for lower integration limit
*     NXH = INTEGER (Given)
*        X value for upper integration limit
*     NYL = INTEGER (Given)
*        Y value for lower integration limit
*     NYH = INTEGER (Given)
*        Y value for upper integration limit
*     XCEN = REAL (Given)
*        Centre of elliptical cursor in x
*     YCEN = REAL (Given)
*        Centre of elliptical cursor in y
*     A2 = REAL (Given)
*        Outer semi-major axis of elliptical annulus
*     A3 = REAL (Given)
*        Inner semi-major axis of elliptical annulus
*     E = REAL (Given)
*        Ellipticity of annulus
*     THETA = REAL (Given)
*        Orientation of ellipse in degrees from x axis
*     VALUES( * ) = REAL (Given)
*        Work space array for storing sky values
*     NV = INTEGER (Given and Returned)
*        Number of elements in workspace array
*        Number of pixels in modal estimate
*     SKY = REAL (Returned)
*        Value in sky aperture per pixel
*     SIGMA = REAL (Returned)
*        Standard deviation in sky aperture
*     VSKY = REAL (Returned)
*        Variance in sky aperture per pixel
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JAN-1988 (NE):
*        Original version.
*     10-SEP-1989 (NE):
*        Pass the array VALUES as a workspace array
*        to overcome the problem of limited size.
*     10-OCT-1989 (NE):
*        Offer a choice of estimator.
*        Added sky mask.
*     10-JAN-1992 (NE):
*        Include data variance.
*     21-FEB-2008 (PWD):
*        Use SAI__OK for status and replace unneccesary internal
*        writes to copy constant strings.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'


*  Arguments Given :
      INTEGER SKYEST
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      REAL IMVAR( NX, NY )
      LOGICAL USEVAR
      REAL MASK( NX, NY )
      LOGICAL USEMSK
      INTEGER NXL
      INTEGER NXH
      INTEGER NYL
      INTEGER NYH
      REAL XCEN
      REAL YCEN
      REAL A2
      REAL A3
      REAL E
      REAL THETA
      REAL VALUES( * )

*  Arguments Given and Returned :
      INTEGER NV

*  Arguments Returned :
      REAL SKY
      REAL SIGMA
      REAL VSKY

*  Local Variables :
      INTEGER I, J, MAXSKY, NVAR, STATUS

      REAL DX, DY, EFACT, FACTOR, PI, PHI, PSI, RTHETA, RG, R2, R3,
     :     SKEW, SMEAN, SMED

*.

*   Set the status
      STATUS = SAI__OK

*   Check that SKYEST has a valid value
      IF ( ( SKYEST .LT. 1 ) .OR. ( SKYEST .GT. 3 ) ) THEN
         SIGMA = -1.0
         SKY = 0.0
         CALL MSG_OUT( ' ', 'Sky estimator is not valid', STATUS )
         GOTO 99
      ENDIF

*   Initialise the variance
      VSKY = 0.0
      NVAR = 0

*   Set the maximum number of sky values to be equal to the size
*   of the workspace array
      MAXSKY = NV

*   Go through the grid array and find all the pixels that have their
*   centers within the inner and outer radii of the annulus.
*   Also check that the relevant pixel is not bad.
      NV = 0

*   Some set ups. Change orientation to a position angle
      PI = ATAN( 1.0 ) * 4.0
      RTHETA = ( THETA + 90.0 ) * PI / 180.0
      EFACT = ( 1.0 - E**2 )

      DO J = NYL, NYH
         DO I = NXL, NXH

*   Calculate the angle and distance of centre of pixel from ellipse centre
            DX = REAL( I ) - 0.5 - XCEN
            DY = REAL( J ) - 0.5 - YCEN

            IF ( ( DX .EQ. 0.0 ) .AND. ( DY .EQ. 0.0 ) ) THEN
               RG = 0.0
               PHI = 0.0
            ELSE
               RG = SQRT( DX**2 + DY**2 )
               PHI = ATAN2( DY, DX )
            ENDIF

*   Calculate orientation of ellipse to this
            PSI = PHI - RTHETA

*   Calculate inner and outer radii at this angle
            FACTOR = EFACT / ( EFACT * COS( PSI )**2 + SIN( PSI )**2 )
            R2 = SQRT( FACTOR * A2**2 )
            R3 = SQRT( FACTOR * A3**2 )

*   Check that the pixel centre is between the two radii
            IF ( ( RG .GE. R3 ) .AND. ( RG .LE. R2 ) .AND.
     :           ( IMAGE( I, J ) .NE. VAL__BADR ) ) THEN

*   If there are more sky pixels than allowed then at the moment the
*   accumulation of these pixels is just terminated.
*   No error message is given at the moment.
               IF ( NV .LT. MAXSKY ) THEN

*   Check the use of the sky mask
*   Include the pixel if the weighting factor is greater than 1/2
                  IF ( USEMSK ) THEN
                     IF ( MASK( I, J ) .GT. 0.5 ) THEN
                        NV = NV + 1
                        VALUES( NV ) = IMAGE( I, J )

*   Sum the variance for the pixel
                        IF ( USEVAR ) THEN
                           IF ( IMVAR( I, J ) .NE. VAL__BADR ) THEN
                              VSKY = VSKY + IMVAR( I, J )
                              NVAR = NVAR + 1
                           ENDIF
                        ENDIF
                     ENDIF

*   Include the pixel if there is no mask
                  ELSE
                     NV = NV + 1
                     VALUES( NV ) = IMAGE( I, J )

*   Sum the variance for the pixel
                     IF ( USEVAR ) THEN
                        IF ( IMVAR( I, J ) .NE. VAL__BADR ) THEN
                           VSKY = VSKY + IMVAR( I, J )
                           NVAR = NVAR + 1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO

*   If there are not enough pixels in the sky then return with a bad value
      IF ( NV .LT. 1 ) THEN
         SIGMA = -1.0
         SKY = 0.0
         CALL MSG_OUT( ' ', 'Sky region is not valid', STATUS )
         GOTO 99
      ENDIF

*   Calculate the variance per pixel
      IF ( USEVAR ) THEN
         IF ( NVAR .GE. 1 ) THEN
            VSKY = VSKY / REAL( NVAR )
         ELSE
            VSKY = 0.0
         ENDIF
      ENDIF

*   Find the sky value from one of the estimators
      IF ( SKYEST .EQ. 1 ) THEN

*   Use the mean as the estimator
         CALL MEAN1( VALUES, NV, SKY, SIGMA )

      ELSEIF ( SKYEST .EQ. 2 ) THEN

*   Use a clipped mean as the estimator
         CALL MEAN2S( VALUES, NV, SKY, SIGMA )

      ELSEIF ( SKYEST .EQ. 3 ) THEN

*   Use the mode as the estimator
         CALL MMM( VALUES, NV, SMEAN, SMED, SKY, SIGMA, SKEW )

      ENDIF

  99  CONTINUE

      END

