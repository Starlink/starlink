*+  UTIL_INDEX - Converts from element of vectorized array to indices
      SUBROUTINE UTIL_INDEX( NDIMS, DIMS, ELEMENT, INDEX )
*    Description :
*     Given the original array size & shape, this routine converts
*     in incrimental counter thru this array into the index values
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phillip Andrews (BHVAD::PLA)
*     David Allan (BHVAD::DJA)
*    History :
*
*     30 Jun 88 : Original (PLA)
*     10 Jul 92 : Drastically simplified (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                NDIMS               ! Dimensionality of the array
      INTEGER                DIMS( NDIMS )       ! Dimensions of the array
      INTEGER                ELEMENT             ! Element of vectorized array
*
*    Export :
*
      INTEGER                INDEX( NDIMS )      ! Index values in original array
*
*    Local variables :
*
      INTEGER                I                   ! Loop over indices
      INTEGER                NELM                ! Total # of elements
      INTEGER                PLEFT               ! Pixel residual
*-

      IF ( NDIMS .EQ. 1 ) THEN

*       Solution is simple!
         INDEX( 1 ) = ELEMENT

      ELSE

*       Find number of elements
         NELM = 1
         DO I = 1, NDIMS
           NELM = NELM * DIMS(I)
         END DO

*       Loop over all the dimensions, in reverse order.
         PLEFT = ELEMENT
         DO I = NDIMS, 2, -1
           NELM = NELM / DIMS(I)
           INDEX(I) = (PLEFT-1)/NELM + 1
           PLEFT = PLEFT - (INDEX(I)-1)*NELM
         END DO
         INDEX(1) = PLEFT

      END IF

      END
