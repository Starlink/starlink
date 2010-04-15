      SUBROUTINE IRM_HMULT( IN1, IN2, M, N, OUT, STATUS )
*+
*  Name:
*     IRM_HMULT

*  Purpose:
*     Multiply two Hermitian images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_HMULT( IN1, IN2, M, N, OUT, STATUS )

*  Description:
*     Each input Hermitian image represents a COMPLEX image in which
*     certain symetries exist. These symetries allow all the
*     information stored in the COMPLEX image to be compressed into a
*     single REAL image. This routine effectively unpacks the two REAL
*     images given as input into two COMPLEX images, multiplies them
*     together pixel-by-pixel to produce another COMPLEX image, and
*     then packs the COMPLEX image back into a single Hermitian REAL
*     image. In fact it is not necessary to do the actual unpacking and
*     packing of these Hermitian images; this algorithm generates the
*     output Hermitian image directly and thus saves time.
*
*     NB, to make this routine consistent with the routines RLFFT and
*     HMFFT which also handle Hermitian images, the input and output
*     Hermitian images are all defined to be in "transposed" form, in
*     which the rows and columns are reversed from their normal form.
*     Thus the first array index is in what is normally called the
*     "line" direction and the second is in the "pixel" direction.
*
*     Each of the supplied FFTs can be thought of as being derived as
*     follows (see the NAG manual, introduction to chapter C06 for more
*     information on the storage of Hermitian FFTs):
*
*  1) Take the one-dimensional FFT of each row of the original image.
*  2) Stack all these one-dimensional FFTs into a pair of 2D images
*     the same size as the original image. One 2D image ("A") holds the
*     real part and the other ("B") holds the imaginary part. Each row
*     of image A will have symetry such that A(J,row) = A(M-J,row) (J
*     goes from 0 to M-1), while each row of image B will have symetry
*     such that B(J,row) = -B(M-J,row).
*  3) Take the one-dimensional FFT of each column of image A.
*  4) Stack all these one-dimensional FFTs into a pair of 2D images,
*     image AA holds the real part of each FFT, and image BA holds the
*     imaginary part. Each column of AA will have symetry such that
*     AA(column,K) = AA(column,N-K) (K goes from 0 to N-1), each column
*     of BA will have symetry such that BA(column,K) = -BA(column,N-K).
*  4) Take the one-dimensional FFT of each column of image B.
*  5) Stack all these one-dimensional FFTs into a pair of 2D images,
*     image AB holds the real part of each FFT, and image BB holds the
*     imaginary part. Each column of AB will have symetry such that
*     AB(column,K) = AB(column,N-K) (K goes from 0 to N), each column of
*     BB will have symetry such that BB(column,K) = -BB(column,N-K).
*  6) The resulting 4 images all have either positive or negative
*     symetry in both axes. The Hermitian FFT images supplied to this
*     routine are made up of one quadrant from each image. The bottom
*     left quadrant is taken from AA, the bottom right quadrant is taken
*     from AB, the top left quadrant is taken from BA and the top right
*     quadrant is taken from BB.
*
*     The product of two hermitian FFTs is itself Hermitian and so can
*     be described in a similar manner. If the first input FFT
*     corresponds to the four images AA1, AB1, BA1 and BB1, and the
*     second input FFt corresponds to the four images AA2, AB2, BA2,
*     BB2, then the output is described by the four images AA, AB, BA
*     and BB where:
*
*     AA =  AA1*AA2 + BB1*BB2 - BA1*BA2 - AB1*AB2
*     BB =  AA1*BB2 + BB1*AA2 + BA1*AB2 + AB1*BA2
*     BA =  BA1*AA2 - AB1*BB2 + AA1*BA2 - BB1*AB2
*     AB = -BA1*BB2 + AB1*AA2 + AA1*AB2 - BB1*BA1

*  Arguments:
*     IN1( N, M ) = DOUBLE PRECISION (Given)
*        The first input transposed Hermitian image.
*     IN2( N, M ) = DOUBLE PRECISION (Given)
*        The first input transposed Hermitian image.
*     M = INTEGER (Given)
*        No. of pixels per line in the non-transposed image from which
*        the Hermitian images were created.
*     N = INTEGER (Given)
*        No. of lines in the non-transposed image from which the
*        Hermitian images were created.
*     OUT( N, M ) = DOUBLE PRECISION (Returned)
*        The product of the two input images, in transposed Hermitian
*        form.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Change name from HMULT to KPG1_HMULT
*     11-JAN-1993 (DSB):
*        Converted to double precision and Included in IRM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER     M
      INTEGER     N
      DOUBLE PRECISION IN1( N, M )
      DOUBLE PRECISION IN2( N, M )

*  Arguments Returned:
      DOUBLE PRECISION OUT( N, M )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION	AA1      ! AA1 image value.
      DOUBLE PRECISION	AA2      ! AA2 image value.
      INTEGER	AALIN            ! Line number in AA corresponding to
                                 ! current pixel.
      INTEGER	AAPIX            ! Pixel number in AA corresponding to
                                 ! current pixel.
      DOUBLE PRECISION	AB1      ! AB1 image value.
      DOUBLE PRECISION	AB2      ! AB2 image value.
      INTEGER	ABLIN            ! Line number in AB corresponding to
                                 ! current pixel.
      INTEGER	ABPIX            ! Pixel number in AB corresponding to
                                 ! current pixel.
      DOUBLE PRECISION	BA1      ! BA1 image value.
      DOUBLE PRECISION	BA2      ! BA2 image value.
      INTEGER	BALIN            ! Line number in BA corresponding to
                                 ! current pixel.
      INTEGER	BAPIX            ! Pixel number in BA corresponding to
                                 ! current pixel.
      DOUBLE PRECISION	BB1      ! BB1 image value.
      DOUBLE PRECISION	BB2      ! BB2 image value.
      INTEGER	BBLIN            ! Line number in BB corresponding to
                                 ! current pixel.
      INTEGER	BBPIX            ! Pixel number in BB corresponding to
                                 ! current pixel.
      INTEGER	J                ! Offset from bottom left corner along
                                 ! the second dimension of the
                                 ! transposed input frames.
      INTEGER   JLIM             ! Upper limit of J in the bottom left
                                 ! quadrant.
      INTEGER	K                ! Offset from bottom left corner along
                                 ! the first dimension of the
                                 ! transposed input frames.
      INTEGER   KLIM             ! Upper limit of K in the bottom left
                                 ! quadrant.
      LOGICAL   MEVEN            ! True if M is even.
      LOGICAL	NEVEN            ! True if N is even.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the limits of the bottom left quadrant (AA).
      IF( MOD( N, 2 ) .EQ. 0 ) THEN
         NEVEN = .TRUE.
         KLIM = N/2 - 1
      ELSE
         NEVEN = .FALSE.
         KLIM = N/2
      END IF

      IF( MOD( M, 2 ) .EQ. 0 ) THEN
         MEVEN = .TRUE.
         JLIM = M/2 - 1
      ELSE
         MEVEN = .FALSE.
         JLIM = M/2
      END IF

*  Deal with the bottom left pixel, for which K=0 and J=0. All terms
*  containing imaginary components (eg BA, AB, BB ) are zero at the
*  bottom left pixel.
      OUT( 1, 1 ) = IN1( 1, 1 )*IN2( 1, 1 )

*  Now do the J=0 column for which all AB and BB terms are zero (in both
*  input images and in the output image).
      AALIN = 1
      BALIN = 1

      DO K = 1, KLIM

         AAPIX = K + 1
         BAPIX = N - K + 1

         AA1 = IN1( AAPIX, AALIN )
         AA2 = IN2( AAPIX, AALIN )
         BA1 = IN1( BAPIX, BALIN )
         BA2 = IN2( BAPIX, BALIN )

         OUT( AAPIX, AALIN ) = AA1*AA2 - BA1*BA2
         OUT( BAPIX, BALIN ) = BA1*AA2 + AA1*BA2

      END DO

      IF( NEVEN ) OUT( KLIM + 2, 1 ) = IN1( KLIM + 2, 1 )
     :                                *IN2( KLIM + 2, 1 )

*  Now do the K=0 row for which all BA and BB terms are zero (in both
*  input images and in the output image).
      AAPIX = 1
      ABPIX = 1

      DO J = 1, JLIM

         AALIN = J + 1
         ABLIN = M - J + 1

         AA1 = IN1( AAPIX, AALIN )
         AA2 = IN2( AAPIX, AALIN )
         AB1 = IN1( ABPIX, ABLIN )
         AB2 = IN2( ABPIX, ABLIN )

         OUT( AAPIX, AALIN ) = AA1*AA2 - AB1*AB2
         OUT( ABPIX, ABLIN ) = AB1*AA2 + AA1*AB2

      END DO

      IF( MEVEN ) OUT( 1, JLIM + 2 ) = IN1( 1, JLIM + 2 )
     :                                *IN2( 1, JLIM + 2 )


*  Now loop round the rest of the bottom left (AA) quadrant, finding the
*  coordinates of the corresponding pixels in the other quadrants. Note,
*  pixel coordinates start at 1 whereas J and K start at 0.
      DO K = 1, KLIM
         AAPIX = K + 1
         BBPIX = N - K + 1
         ABPIX = AAPIX
         BAPIX = BBPIX

         DO J = 1, JLIM

            AALIN = J + 1
            BBLIN = M - J + 1
            ABLIN = BBLIN
            BALIN = AALIN

*  Save the values in scalar variables to make the calculations faster.
            AA1 = IN1( AAPIX, AALIN )
            AA2 = IN2( AAPIX, AALIN )

            BB1 = IN1( BBPIX, BBLIN )
            BB2 = IN2( BBPIX, BBLIN )

            AB1 = IN1( ABPIX, ABLIN )
            AB2 = IN2( ABPIX, ABLIN )

            BA1 = IN1( BAPIX, BALIN )
            BA2 = IN2( BAPIX, BALIN )

*  Calculate the values of the four quadrants in the output image.
            OUT( AAPIX, AALIN ) =   AA1*AA2 + BB1*BB2
     :                            - BA1*BA2 - AB1*AB2
            OUT( BBPIX, BBLIN ) =   AA1*BB2 + BB1*AA2
     :                            + BA1*AB2 + AB1*BA2
            OUT( ABPIX, ABLIN ) = - BA1*BB2 + AB1*AA2
     :                            + AA1*AB2 - BB1*BA2
            OUT( BAPIX, BALIN ) =   BA1*AA2 - AB1*BB2
     :                            + AA1*BA2 - BB1*AB2

         END DO

*  If M is even there will be an extra column to do.
         IF( MEVEN ) THEN

            AALIN = JLIM + 2
            BALIN = AALIN

            AA1 = IN1( AAPIX, AALIN )
            AA2 = IN2( AAPIX, AALIN )

            BA1 = IN1( BAPIX, BALIN )
            BA2 = IN2( BAPIX, BALIN )

            OUT( AAPIX, AALIN ) =  AA1*AA2 - BA1*BA2
            OUT( BAPIX, BALIN ) =  BA1*AA2 + AA1*BA2

         END IF

      END DO

*  If N is even there will be an extra row to do.
      IF( NEVEN ) THEN
         AAPIX = KLIM + 2
         ABPIX = AAPIX

         DO J = 1, JLIM

            AALIN = J + 1
            BBLIN = M - J + 1
            ABLIN = BBLIN
            BALIN = AALIN

            AA1 = IN1( AAPIX, AALIN )
            AA2 = IN2( AAPIX, AALIN )

            BB1 = 0
            BB2 = 0

            AB1 = IN1( ABPIX, ABLIN )
            AB2 = IN2( ABPIX, ABLIN )

            BA1 = 0
            BA2 = 0

            OUT( AAPIX, AALIN ) =  AA1*AA2 - AB1*AB2
            OUT( ABPIX, ABLIN ) =  AB1*AA2 + AA1*AB2

         END DO

         IF( MEVEN ) THEN

            AALIN = JLIM + 2

            AA1 = IN1( AAPIX, AALIN )
            AA2 = IN2( AAPIX, AALIN )

            OUT( AAPIX, AALIN ) =  AA1*AA2

         END IF

      END IF

*  Finish
      END
