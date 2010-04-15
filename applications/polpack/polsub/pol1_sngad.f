      SUBROUTINE POL1_SNGAD( EL, DIN, VIN, PHI, T, EPS, IE1, IE2,
     :                       IE3, MAT11, MAT21, MAT31, MAT22, MAT32,
     :                       MAT33, COUNT, STATUS )
*+
*  Name:
*     POL1_SNGAD

*  Purpose:
*     Add a single-beam input intensity image into the running total images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGAD( EL, DIN, VIN, PHI, T, EPS, IE1, IE2, IE3,
*                      MAT11, MAT21, MAT31, MAT22, MAT32, MAT33,
*                      COUNT, STATUS )

*  Description:
*     This routine updates a set of images holding different quanities by
*     adding a contribution to each based on the supplied intensity array.
*     The returned quantities are needed to calculate the Stokes vectors,
*     variances, and co-variances corresponding to the supplied set of
*     intensity images.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in each image.
*     DIN( EL ) = REAL (Given)
*        The input intensity values.
*     VIN( EL ) = REAL (Given)
*        The input variance values.
*     PHI = REAL (Given)
*        The analyser angle for the supplied array. In radians.
*     T = REAL (Given)
*        The analyser transmission factor for the supplied array.
*     EPS = REAL (Given)
*        The analyser efficieny factor for the supplied array.
*     IE1( EL ) = REAL (Given and Returned)
*        The effective intensity values for the first effective analyser.
*     IE2( EL ) = REAL (Given and Returned)
*        The effective intensity values for the second effective analyser.
*     IE3( EL ) = REAL (Given and Returned)
*        The effective intensity values for the third effective analyser.
*     MAT11( EL ) = REAL (Given and Returned)
*        Column 1, row 1 of the matrix giving the effective intensities.
*     MAT21( EL ) = REAL (Given and Returned)
*        Column 2, row 1 of the matrix giving the effective intensities
*        (equals column 1, row 2).
*     MAT31( EL ) = REAL (Given and Returned)
*        Column 3, row 1 of the matrix giving the effective intensities
*        (equals column 1, row 3).
*     MAT22( EL ) = REAL (Given and Returned)
*        Column 2, row 2 of the matrix giving the effective intensities.
*     MAT32( EL ) = REAL (Given and Returned)
*        Column 3, row 2 of the matrix giving the effective intensities
*        (equals column 2, row 3).
*     MAT33( EL ) = REAL (Given and Returned)
*        Column 3, row 3 of the matrix giving the effective intensities.
*     COUNT( EL ) = REAL (Given and Returned)
*        The number of input images contributing to each output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1999 (DSB):
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
      REAL DIN( EL )
      REAL VIN( EL )
      REAL PHI
      REAL T
      REAL EPS

*  Arguments Given and Returned:
      REAL IE1( EL )
      REAL IE2( EL )
      REAL IE3( EL )
      REAL MAT11( EL )
      REAL MAT21( EL )
      REAL MAT31( EL )
      REAL MAT22( EL )
      REAL MAT32( EL )
      REAL MAT33( EL )
      REAL COUNT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element index
      DOUBLE PRECISION COS2, SIN2, RCOS, RSIN, RT, R1, R2, RC, RS ! Constant terms
      REAL VARVAL                ! Variance value to use
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store some constants.
      COS2 = COS( DBLE( 2*PHI ) )
      SIN2 = SIN( DBLE( 2*PHI ) )
      RCOS = DBLE( EPS )*COS2
      RSIN = DBLE( EPS )*SIN2
      RT = DBLE( T )**2

*  Loop round every pixel.
      DO I = 1, EL
         VARVAL = VIN( I )

*  Ignore this pixel if either the input variance or intensity is bad, or if
*  the variance is less than or equal to zero.
         IF( DIN( I ) .NE. VAL__BADR .AND. VARVAL .NE. VAL__BADR .AND.
     :       VARVAL .GT. 0.0 ) THEN

*  Constants...
            R1 = RT/VARVAL
            R2 = DBLE( T )/DBLE( VARVAL )
            RC = R1*RCOS
            RS = R1*RSIN

*  Effective intensities...
            IE1( I ) = IE1( I ) + DIN( I )*R2
            IE2( I ) = IE2( I ) + DIN( I )*R2*RCOS
            IE3( I ) = IE3( I ) + DIN( I )*R2*RSIN

*  Matrix elements... Row 1:
            MAT11( I ) = MAT11( I ) + R1
            MAT21( I ) = MAT21( I ) + RC
            MAT31( I ) = MAT31( I ) + RS

*  Matrix elements... Row 2:
            MAT22( I ) = MAT22( I ) + RC*RCOS
            MAT32( I ) = MAT32( I ) + RS*RCOS

*  Matrix elements... Row 3:
            MAT33( I ) = MAT33( I ) + RS*RSIN

*  Increase the count of input images contributing to this pixel.
            COUNT( I ) = COUNT( I ) + 1.0D0

         END IF

      END DO

      END
