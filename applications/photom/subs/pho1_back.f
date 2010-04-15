************************************************************************

      SUBROUTINE PHO1_BACK ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR, GRID,
     :                       GS, MASK, USEMSK, NXL, NXH, NYL, NYH,
     :                       VALUES, NV, SKY, SIGMA, VSKY )

*+
*  Name :
*     PHO1_BACK
*
*  Purpose :
*     This finds the sky value from a choice of three estimators
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL PHO1_BACK( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR, GRID, GS,
*                     MASK, USEMSK, NXL, NXH, NYL, NYH, VALUES, NV,
*                     SKY, SIGMA, VSKY )
*
*  Description :
*     This finds the sky value from a choice of three estimators defined
*     by SKYEST. Possible choices are
*     1 - simple mean
*     2 - mean with 2 sigma rejection
*     3 - mode
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
*     GRID( GS, GS ) = REAL (Given)
*        Work space array
*     GS = INTEGER (Given)
*        Size of grid array
*     MASK( NX, NY ) = INTEGER (Given)
*        Mask array, same size as image
*     USEMSK = LOGICAL (Given)
*        Flag to indicate use of mask
*     NXL = INTEGER (Given)
*        X value for lower integration limit
*     NXH = INTEGER (Given)
*        X value for upper integration limit
*     NYL = INTEGER (Given)
*        Y value for lower integration limit
*     NYH = INTEGER (Given)
*        Y value for upper integration limit
*     VALUES( * ) = REAL (Given)
*        Work space array for storing sky values
*     NV = INTEGER (Given and Returned)
*        Nnumber of elements in workspace array
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
*     PWD: Peter W. Draper (STARLINK - Durham University)
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
*        Limit size of grid array to a square of size GS.
*        Include data variance.
*     19-APR-1996 (PWD):
*        Changed MASK data type to INTEGER from REAL to correspond with
*        ARD usage. Renamed PHO1_BACK to differentiate from old version.
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Use SAI__OK.
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
      INTEGER GS
      REAL GRID( GS, GS )
      INTEGER MASK( NX, NY )
      LOGICAL USEMSK
      INTEGER NXL
      INTEGER NXH
      INTEGER NYL
      INTEGER NYH
      REAL VALUES( * )

*  Arguments Given and Returned :
      INTEGER NV

*  Arguments Returned :
      REAL SKY
      REAL SIGMA
      REAL VSKY

*  Local Constants :
      REAL MINARE
      PARAMETER ( MINARE = 0.333333 )

*  Local Variables :
      INTEGER I, II, J, JJ, MAXSKY, NVAR, STATUS

      REAL SKEW, SMEAN, SMED

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

*   Go through the grid array and find all the pixels that have more
*   than 1/3 of their area inside the aperture.
*   Also check that the relevant pixel is not bad.
      NV = 0
      DO J = NYL, NYH
         JJ = J - NYL + 1
         DO I = NXL, NXH
            II = I - NXL + 1
            IF ( ( GRID( II, JJ ) .GT. MINARE ) .AND.
     :           ( IMAGE( I, J ) .NE. VAL__BADR ) ) THEN

*   If there are more sky pixels than allowed then at the moment the
*   accumulation of these pixels is just terminated.
*   No error message is given at the moment.
               IF ( NV .LT. MAXSKY ) THEN

*   Check the use of the sky mask
*   Include the pixel if the mask value is 0.
                  IF ( USEMSK ) THEN
                     IF ( MASK( I, J ) .EQ. 0 ) THEN
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

* $Id$
