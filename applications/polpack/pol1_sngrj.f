      SUBROUTINE POL1_SNGRJ( ITER, NAME, ILEVEL, NSIGMA, VAR, EL, 
     :                       DIN, VIN, PHI, T, EPS, DOUT, ALLOK, 
     :                       DCOPY, STATUS )
*+
*  Name:
*     POL1_SNGRJ

*  Purpose:
*     Reject aberrant data values from a single-beam input intensity image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGRJ( ITER, NAME, ILEVEL, NSIGMA, VAR, EL, DIN, VIN, PHI, 
*                      T, EPS, DOUT, ALLOK, DCOPY, STATUS )

*  Description:
*     This routine copies the image supplied in DIN, returning the copy 
*     in DCOPY. The copied data values are compared with the data values 
*     implied by the Stokes vector supplied in DOUT. Any which are very 
*     different are set bad in DCOPY. The largest acceptable residual is 
*     determined by NSIGMA.

*  Arguments:
*     ITER = INTEGER (Given)
*        The index of the current rejection iteration.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     ILEVEL = INTEGER (Given)
*        If 2 or more; the number of pixels rejected from the NDF is
*        reported.
*     NSIGMA = REAL (Given)
*        The rejection threshold for aberrant points, expressed as a
*        multiple of the standard deviation of the supplied data value.
*        If VAR is .TRUE. then the standard deviation used is the square root 
*        of the input variance supplied in VIN. Otherwise, it is the
*        standard deviation of the residuals taken over the entire input
*        image. 
*     VAR = LOGICAL (Given)
*        Are input variances available?
*     EL = INTEGER (Given)
*        The number of pixels in each image.
*     DIN( EL ) = REAL (Given)
*        The input intensity values.
*     VIN( EL ) = REAL (Given)
*        The input variance values. Only accessed if VAR is TRUE.
*     PHI = REAL (Given)
*        The analyser angle for the supplied array. In radians.
*     T = REAL (Given)
*        The analyser transmission factor for the supplied array.
*     EPS = REAL (Given)
*        The analyser efficieny factor for the supplied array.
*     DOUT( EL, 3 ) = REAL (Given)
*        This should hold the I,Q, and U values (in planes 1, 2 and 
*        3) derived by the previous iteration. 
*     ALLOK = LOGICAL (Given and Returned)
*        If this is supplied .TRUE., then it is set to .FALSE. if any
*        pixels are rejected. If it supplied .FALSE., then it is returned
*        unchanged.
*     DCOPY( EL ) = REAL (Returned)
*        Returned holding a copy of DIN in which aberrant points have been
*        set to VAL__BADR. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER ITER
      CHARACTER NAME*(*)
      INTEGER ILEVEL
      REAL NSIGMA
      LOGICAL VAR
      INTEGER EL
      REAL DIN( EL )
      REAL VIN( EL )
      REAL PHI
      REAL T 
      REAL EPS
      REAL DOUT( EL, 3 )

*  Arguments Given and Returned:
      LOGICAL ALLOK

*  Arguments Returned:
      REAL DCOPY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element index
      INTEGER NGOOD              ! No. of good pixels remaining
      INTEGER NREJ               ! No. of pixels rejected this iteration
      INTEGER NRES               ! No. of valid residuals
      REAL EXPECT                ! Expected input data value
      REAL K1, K2, K3            ! Constants
      REAL SRES                  ! Sum of squared residuals
      REAL THRESH                ! Squared residual threshold
      REAL VARFAC                ! Variance factor
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store some constants.
      K1 = 0.5*T
      K2 = EPS*COS( 2*PHI )
      K3 = EPS*SIN( 2*PHI )

*  Derive the input intensity values expected on the basis of the supplied
*  I,Q,U values. Store the squared residual between this and the supplied 
*  input intensity in DCOPY. Also find the sum of the squared residuals.
      SRES = 0.0
      NRES = 0

      DO I = 1, EL

         IF( DOUT( I, 1 ) .NE. VAL__BADR .AND.
     :       DOUT( I, 2 ) .NE. VAL__BADR .AND.
     :       DOUT( I, 3 ) .NE. VAL__BADR .AND.
     :       DIN( I ) .NE. VAL__BADR ) THEN

            EXPECT = K1*( DOUT( I, 1 ) + K2*DOUT( I, 2 ) 
     :                                 + K3*DOUT( I, 3 ) )
            DCOPY( I ) = ( EXPECT - DIN( I ) )**2            
            SRES = SRES + DCOPY( I )
            NRES = NRES + 1

         ELSE
            DCOPY( I ) = VAL__BADR
         END IF

      END DO

*  If no good residuals were found, fill the output array with bad values.
      IF( NRES .EQ. 0 ) THEN

         DO I = 1, EL
            DCOPY( I ) = VAL__BADR
         END DO

*  Otherwise, we can copy the acceptable DIN values to DOUT. 
      ELSE 

*  Initialise the number of rejected pixels.
         NREJ = 0

*  Initialise the number of good pixels.
         NGOOD = 0

*  First handle the case where input variances are available.
         IF( VAR ) THEN

*  Store the variance factor.
            VARFAC = NSIGMA**2

*  Check each pixel. 
            DO I = 1, EL

*  Check the squared residual is valid.
               IF( DCOPY( I ) .NE. VAL__BADR ) THEN
 
*  Check the variance is valid.
                 IF( VIN( I ) .NE. VAL__BADR ) THEN

*  Store bad value if the squared residual is too large. Otherwise, copy
*  the supplied input data value.
                     IF( DCOPY( I ) .GE. VIN( I )*VARFAC ) THEN
                        DCOPY( I ) = VAL__BADR
                        NREJ = NREJ + 1
                     ELSE
                        DCOPY( I ) = DIN( I )
                        NGOOD = NGOOD + 1
                     END IF

*  If the variance is not valid, store a bad output value.
                  ELSE
                     DCOPY( I ) = VAL__BADR 
                  END IF         
   
               END IF
   
            END DO

*  Now handle the case where input variances are not available.
         ELSE

*  Find the maximum acceptable squared residual value.
            THRESH = ( NSIGMA**2 )*( SRES/NRES )

*  Check each pixel.
            DO I = 1, EL

*  Check the squared residual is valid.
               IF( DCOPY( I ) .NE. VAL__BADR ) THEN
 
*  Store bad value if the squared residual is too large. Otherwise, copy
*  the supplied input data value.
                  IF( DCOPY( I ) .GE. THRESH ) THEN
                     DCOPY( I ) = VAL__BADR
                     NREJ = NREJ + 1
                  ELSE
                     DCOPY( I ) = DIN( I )
                     NGOOD = NGOOD + 1
                  END IF

               END IF

            END DO

         END IF

*  Set the returned flag indicating if all input pixels were OK.
         IF( ALLOK .AND. NREJ .GT. 0 ) ALLOK = .FALSE.

*  If required, tell the user how many pixels were rejected from this NDF
*  during this iteration.
         IF( ILEVEL .GT. 1 ) THEN
            CALL MSG_SETI( 'ITER', ITER )
            CALL MSG_SETI( 'NREJ', NREJ )
            CALL MSG_SETI( 'NGOOD', NGOOD )
            CALL MSG_SETC( 'NDF', NAME )

            CALL MSG_OUT( 'POL1_SNGRJ_MSG1', 'Iter: ^ITER  Rejected: '//
     :                    '^NREJ  Remaining: ^NGOOD -- ''^NDF''', 
     :                    STATUS )

*  If required, warn the user if no good pixels remain in this NDF.
         ELSE IF( ILEVEL .GT. 0 .AND. NGOOD .EQ. 0 ) THEN
            CALL MSG_SETC( 'NDF', NAME )
            CALL MSG_SETI( 'ITER', ITER )
            CALL MSG_OUT( 'POL1_SNGRJ_MSG1', 'WARNING: No usable '//
     :                    'pixels remain in ''^NDF'' after ^ITER '//
     :                    'rejection iterations.', STATUS )
         END IF

      END IF

      END
