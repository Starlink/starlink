      SUBROUTINE POL1_SNGCL( EL, IE1, IE2, IE3, MAT11, MAT21, 
     :                       MAT31, MAT22, MAT32, MAT33, CM1, CM2, 
     :                       COUNT, DOUT, VOUT, COUT, STATUS )
*+
*  Name:
*     POL1_SNGCL

*  Purpose:
*     Calculate the Stokes vectors, variances and co-variances for single-beam 
*     data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGCL( EL, IE1, IE2, IE3, MAT11, MAT21, MAT31, MAT22, 
*                      MAT32, MAT33, CM1, CM2, COUNT, DOUT, VOUT, COUT, 
*                      STATUS )

*  Description:
*     This routine calculates the Stokes vectors, variances and co-variances 
*     for a single-beam data set, and writes them into the supplied arrays.
*     The method used is described by Sparks & Axon (PASP ????).

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in each image.
*     IE1( EL ) = REAL (Given)
*        The effective intensity values for the first effective analyser.
*     IE2( EL ) = REAL (Given)
*        The effective intensity values for the second effective analyser.
*     IE3( EL ) = REAL (Given)
*        The effective intensity values for the third effective analyser.
*     MAT11( EL ) = REAL (Given)
*        Column 1, row 1 of the matrix giving the effective intensities.
*     MAT21( EL ) = REAL (Given)
*        Column 2, row 1 of the matrix giving the effective intensities
*        (equals column 1, row 2). 
*     MAT31( EL ) = REAL (Given)
*        Column 3, row 1 of the matrix giving the effective intensities
*        (equals column 1, row 3). 
*     MAT22( EL ) = REAL (Given)
*        Column 2, row 2 of the matrix giving the effective intensities.
*     MAT32( EL ) = REAL (Given)
*        Column 3, row 2 of the matrix giving the effective intensities
*        (equals column 2, row 3). 
*     MAT33( EL ) = REAL (Given)
*        Column 3, row 3 of the matrix giving the effective intensities.
*     CM1( EL ) = REAL (Given)
*        The first term needed to calculate the curvature matrix.
*     CM2( EL ) = REAL (Given)
*        The second term needed to calculate the curvature matrix.
*     COUNT( EL ) = REAL (Given)
*        The number of input images contributing to each output pixel.
*     DOUT( EL, 3 ) = REAL (Returned)
*        The output Stokes vectors. Plane 1 holds I, plane 2 holds Q
*        and plane 3 holds U.
*     VOUT( EL, 3 ) = REAL (Returned)
*        The output variance values. 
*     COUT( EL ) = REAL (Returned)
*        The output QU co-variance values. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1999 (DSB):
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
      INTEGER EL
      REAL IE1( EL )
      REAL IE2( EL )
      REAL IE3( EL )
      REAL MAT11( EL )
      REAL MAT21( EL )
      REAL MAT31( EL )
      REAL MAT22( EL )
      REAL MAT32( EL )
      REAL MAT33( EL )
      REAL CM1( EL )
      REAL CM2( EL )
      REAL COUNT( EL )

*  Arguments Returned:
      REAL DOUT( EL, 3 )
      REAL VOUT( EL, 3 )
      REAL COUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element index
      INTEGER NGOOD              ! No. of good output pixels
      DOUBLE PRECISION C1, C2, C3, C4, C5, C6, C7, C8, C9 ! The C matrix
      DOUBLE PRECISION D1, D2, D3, D4, D5, D6, D7, D8, D9 ! The D matrix
      DOUBLE PRECISION DEN       ! Denominator value
      DOUBLE PRECISION Y1, Y2, Y3! The effective intensities
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of good output pixels.
      NGOOD = 0

*  Loop round every pixel.
      DO I = 1, EL

*  Store commonly used array elements.
         D1 = MAT11( I )
         D2 = MAT21( I )
         D3 = MAT31( I )
         D5 = MAT22( I )
         D6 = MAT32( I )
         D9 = MAT33( I )

*  Evaluate the denominator term.
         DEN = D3*D3*D5 + D6*D6*D1 + D9*D2*D2 - D9*D5*D1 - 2*D2*D3*D6

*  Store bad output values if the denominator is zero, or if less than
*  three input images contributed to this output pixel.
         IF( DEN .EQ. 0.0 .OR. COUNT( I ) .LT. 3.0 ) THEN

            DOUT( I, 1 ) = VAL__BADR
            DOUT( I, 2 ) = VAL__BADR
            DOUT( I, 3 ) = VAL__BADR

            VOUT( I, 1 ) = VAL__BADR
            VOUT( I, 2 ) = VAL__BADR
            VOUT( I, 3 ) = VAL__BADR
            COUT( I ) = VAL__BADR

*  Otherwise, calculate the required values.
         ELSE 
            Y1 = 2.0*IE1( I )
            Y2 = 2.0*IE2( I )
            Y3 = 2.0*IE3( I )

            DOUT( I, 1 ) = ( Y1*( D6*D6 - D9*D5 ) + 
     :                       Y2*( D2*D9 - D6*D3 ) + 
     :                       Y3*( D3*D5 - D6*D2 ) ) / DEN
            DOUT( I, 2 ) = ( Y1*( D9*D2 - D6*D3 ) + 
     :                       Y2*( D3*D3 - D1*D9 ) + 
     :                       Y3*( D6*D1 - D3*D2 ) ) / DEN
            DOUT( I, 3 ) = ( Y1*( D5*D3 - D6*D2 ) + 
     :                       Y2*( D6*D1 - D2*D3 ) + 
     :                       Y3*( D2*D2 - D5*D1 ) ) / DEN

*  Calculate the C matrix. This gives the curvature of chi-squared with
*  respect to (I,Q,U).
            C1 = 0.25*D1
            C2 = 0.25*CM1( I )
            C3 = 0.25*CM2( I )
            C4 = C2
            C5 = 0.25*D5
            C6 = 0.25*D6
            C7 = C3
            C8 = C6
            C9 = 0.25*D9

*  Store the I, Q and U variances, and the the QU co-variance in the output 
*  arrays. Store bad values if the matrix is singular. The variances are
*  the diagonal elements of the inverted curvature matrix, and the QU
*  co-variance is column 3 row 2 of the inverted curvature matrix.
            DEN = C3*C3*C5 + C6*C6*C1 + C9*C2*C2 - C9*C5*C1 - 
     :            2*C2*C3*C6

            IF( DEN .EQ. 0.0 ) THEN
               VOUT( I, 1 ) = VAL__BADR
               VOUT( I, 2 ) = VAL__BADR
               VOUT( I, 3 ) = VAL__BADR
               COUT( I ) = VAL__BADR

            ELSE
               VOUT( I, 1 ) = ( C6*C6 - C9*C5 )/DEN
               VOUT( I, 2 ) = ( C3*C3 - C1*C9 )/DEN
               VOUT( I, 3 ) = ( C2*C2 - C5*C1 )/DEN
               COUT( I ) = ( C6*C1 - C2*C3 )/DEN
            END IF

*  Increment the number of good output pixels.
            NGOOD = NGOOD + 1

         END IF

      END DO

*  Report an error if all output pixels are bad.
      IF( NGOOD .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Output would contain no good values.',
     :                 STATUS )
      END IF

      END
