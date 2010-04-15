      SUBROUTINE IRM_RLFFT( IN, NPIX, NLIN, WORK, FORWRD, OUT, STATUS )
*+
*  Name:
*     IRM_RLFFT

*  Purpose:
*     Takes the FFT of a purely real image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_RLFFT( IN, NPIX, NLIN, WORK, FORWRD, OUT, STATUS )

*  Description:
*     NAG routines are used to produce an Hermitian FFT of the input
*     image stored in transposed form (i.e rows and columns swapped).
*     Hermitian FFTs are much faster than normal FFTs and require only
*     half the space to store them. See the NAG manual chapter C06 for
*     more information about Hermitian FFTs.

*  Arguments:
*     IN( NPIX, NLIN ) = DOUBLE PRECISION (Given)
*        The input image.
*     NPIX = INTEGER (Given)
*        No. of pixels per line in the input image.
*     NLIN = INTEGER (Given)
*        No. of lines in the input image.
*     WORK( NPIX, NLIN ) = DOUBLE PRECISION (Given)
*        Work space.
*     FORWRD = LOGICAL (Given)
*        True if a forward transformation is required, otherwise an
*        inverse transformation is performed.
*     OUT( NLIN, NPIX ) = DOUBLE PRECISION (Returned)
*        The FFT in transposed Hermitian form (i.e rows and columns
*        swapped).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*      4-MAR-1991 (DSB):
*        Name changed from RLFFT to KPG1_RLFFT.
*     11-JAN-1993 (DSB):
*        Converted to double precision and Included in IRM
*     {enter_changes_here}

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
      DOUBLE PRECISION IN( NPIX, NLIN )
      DOUBLE PRECISION WORK( NPIX, NLIN )
      LOGICAL  FORWRD

*  Arguments Returned:
      DOUBLE PRECISION OUT( NLIN, NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IFAIL              ! NAG error status.
      INTEGER LIN                ! Line counter.
      INTEGER PIX                ! Pixel counter.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the input image to a temporary working array to avoid the input
*  image being overwritten
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            WORK( PIX, LIN ) = IN( PIX, LIN )
         END DO
      END DO

*  Transform each row of the work array. The real and imaginary results
*  are stored back in the work array in Hermitian form (see NAG manual).
*  Array OUT is used as work space. If an inverse transformation is
*  required then conjugation is performed on the output only, since
*  input is purely real.
      DO LIN = 1, NLIN

         IFAIL = -1
         CALL C06FAF( WORK( 1, LIN ), NPIX, OUT, IFAIL )

         IF ( IFAIL .EQ. 0 .AND. (.NOT. FORWRD) ) THEN
            IFAIL = -1
            CALL C06GBF( WORK( 1, LIN ), NPIX, IFAIL )
         END IF

         IF( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_RLFFT_ERR1',
     :        'IRM_RLFFT: Unable to take Fourier transform.', STATUS )
            GOTO 999
        END IF

      END DO

*  Transpose the work array and store it in array OUT.
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( LIN, PIX ) = WORK( PIX, LIN )
         END DO
      END DO

*  Transform each row of the transposed image (the same as each column
*  of the untransposed image), again storing the output in Hermitian
*  form.
      DO PIX = 1, NPIX

         IFAIL = -1
         CALL C06FAF( OUT( 1, PIX ), NLIN, WORK, IFAIL )

         IF ( IFAIL .EQ. 0 .AND. (.NOT. FORWRD) ) THEN
            IFAIL = -1
            CALL C06GBF( OUT( 1, PIX ), NLIN, IFAIL )
         END IF

         IF( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_RLFFT_ERR2',
     :        'IRM_RLFFT: Unable to take Fourier transform.', STATUS )
            GOTO 999
         END IF

      END DO

*  Finish

  999 CONTINUE

      END
