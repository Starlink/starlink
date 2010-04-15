      SUBROUTINE IRM_HMFFT( IN, NPIX, NLIN, WORK, FORWRD, OUT, STATUS )
*+
*  Name:
*     IRM_HMFFT

*  Purpose:
*     Takes the FFT of an Hermitian image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_HMFFT( IN, NPIX, NLIN, WORK, FORWRD, OUT, STATUS )

*  Description:
*     NAG routines are used to produce the FFT of the input Hermitian
*     image. The input must be stored in transposed form (i.e rows and
*     columns swapped). Because the input is Hermitian, the output will
*     be a purely real image ( in normal non-transposed form).
*     Hermitian FFTs are much faster than normal FFTs and require only
*     half the space to store them. See the NAG manual chapter C06 for
*     more information about Hermitian FFTs.

*  Arguments:
*     IN( NLIN, NPIX ) = DOUBLE PRECISION (Given)
*        The input Hermitian image, in transposed form (i.e. rows and
*        columns are swapped).
*     NPIX = INTEGER (Given)
*        No. of pixels per line in the input image.
*     NLIN = INTEGER (Given)
*        No. of lines in the input image.
*     WORK( NLIN, NPIX) = DOUBLE PRECISION (Given)
*        Work space.
*     FORWRD = LOGICAL (Given)
*        True if a forward transformation is required, otherwise an
*        inverse transformation is performed.
*     OUT( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        The FFT of the input. A purely real image in normal
*        (non-tranmsposed) form (i.e rows and columns are not swapped).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Name changed from HMFFT to KPG1_HMFFT
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
      INTEGER  NPIX
      INTEGER  NLIN
      DOUBLE PRECISION IN( NLIN, NPIX )
      DOUBLE PRECISION WORK( NLIN, NPIX )
      LOGICAL  FORWRD

*  Arguments Returned:
      DOUBLE PRECISION OUT( NPIX, NLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IFAIL              ! NAG error status.
      INTEGER LIN                ! Line counter.
      INTEGER PIX                ! Pixel counter.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the input to the working space to avoid the input being
*  overwritten.
      DO PIX = 1, NPIX
         DO LIN = 1, NLIN
            WORK( LIN, PIX ) = IN( LIN, PIX )
         END DO
      END DO

*  Transform each column of the transposed FFT. The conjugate is taken
*  first to get the inverse transformation if required. NB, conjugation
*  is not required on the output since it is purely real. Array OUT is
*  used as working space.
      DO PIX = 1, NPIX

         IFAIL = -1

         IF( .NOT. FORWRD ) THEN
            CALL C06GBF( WORK( 1, PIX ), NLIN, IFAIL )

            IF( IFAIL .EQ. 0 ) THEN
               IFAIL = -1
               CALL C06FBF( WORK( 1, PIX ), NLIN, OUT, IFAIL )
            ENDIF

         ELSE
            CALL C06FBF( WORK( 1, PIX ), NLIN, OUT, IFAIL )
         END IF

         IF( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_HMFFT_ERR1',
     :                 'IRM_HMFFT: Unable to take Fourier transform.',
     :                     STATUS )
            GOTO 999
         END IF

      END DO

*  Transpose the work array and store in the array OUT.
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = WORK( LIN, PIX )
         END DO
      END DO

*  Transform each row of the output array.
      DO LIN = 1, NLIN

         IFAIL = -1

         IF( .NOT. FORWRD ) THEN
            CALL C06GBF( OUT( 1, LIN ), NPIX, IFAIL )

            IF( IFAIL .EQ. 0 ) THEN
               IFAIL = -1
               CALL C06FBF( OUT( 1, LIN ), NPIX, WORK, IFAIL )
            END IF

         ELSE
            CALL C06FBF( OUT( 1, LIN ), NPIX, WORK, IFAIL )
         END IF

         IF( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_HMFFT_ERR2',
     :                 'IRM_HMFFT: Unable to take Fourier transform.',
     :                     STATUS )
            GOTO 999
         END IF

      END DO

*  Finish
  999 CONTINUE

      END
