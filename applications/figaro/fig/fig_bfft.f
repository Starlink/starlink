      SUBROUTINE FIG_BFFT( NDIM, DIMS, FORWRD, NOIMAG, RDATA, IDATA,
     :   WORK, NWORK, STATUS )
*+
*  Name:
*     FIG_BFFT

*  Purpose:
*     Calculate 1-D or N-D Forier transform (foward or backward).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_BFFT( NDIM, DIMS, FORWRD, NOIMAG, RDATA, IDATA,
*        WORK, NWORK, STATUS )

*  Description:
*     Calculates the Fourier transform of a multi-dimensional complex
*     array. The array may, of course, be one-dimensional and this
*     condition is explicitly tested. Either a forward or a reverse
*     transform may be performed.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in the array.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the array.
*     FORWRD = LOGICAL (Given)
*        True/false to select forward/reverse transform.
*     NOIMAG = LOGICAL (Given)
*        If true, IDATA is not a given array, but only returned: The
*        given imaginary part of the array will be assumed all zero.
*     RDATA( DIMS(1), ... ) = DOUBLE PRECISION (Given and Returned)
*        The real part of the array. The real and imaginary part of the
*        array are given as two separate arrays, not as a single array
*        of Fortran type COMPLEX.
*     IDATA( DIMS(1), ... ) = DOUBLE PRECISION (Given and Returned)
*        The imaginary part of the array. The real and imaginary part of
*        the array are given as two separate arrays, not as a single
*        array of Fortran type COMPLEX.
*
*     WORK( NWORK ) = DOUBLE PRECISION (Returned)
*        Work space.
*
*        WORK(1:2*NELM):
*           Merged real and imaginary data.
*        WORK(2*NELM+1:2*NELM+4*NELM+15):
*           Work space for PDA_DCFFT{BF}.
*
*        WORK(1:6*MAXDIM+15):
*           Work space for PDA_DNFFT{BF}
*
*     NWORK = INTEGER (Given)
*        The size of the work space. This must be at least
*        6 * MAXDIM + 15, where MAXDIM is the highest dimension in DIMS.
*        A check is made that this requirement is fulfilled. The caller
*        should pass in NWORK the actual size of the work space it
*        provides.
*     STATUS = INTEGER (Returned)
*        Returned status code. Zero for OK, a non-zero value indicates
*        an error. Error messages are put out through PAR_WRUSER.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Sep 1986 (ks):
*        Original version.
*     20 Apr 1995 (hme):
*        Switch from NAG to FFTPACK. The changes are significant enough
*        to merit a re-write. There are separate routines for forward
*        and reverse transforms. The data format and work space
*        requirement is different. And the work space must be
*        initialised.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      LOGICAL FORWRD
      LOGICAL NOIMAG
      INTEGER NWORK

*  Arguments Given and Returned:
      DOUBLE PRECISION RDATA( 1 )
      DOUBLE PRECISION IDATA( 1 )

*  Arguments Returned:
      DOUBLE PRECISION WORK( NWORK )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I, J               ! Loop indices
      INTEGER NELM               ! Total array size
      INTEGER MAXDIM             ! Largest dimension
      INTEGER IGNORE             ! Ignored status
      INTEGER IFAIL              ! Status returned by N-D transform

*.

*  Set returned status
      STATUS = 0

*  Work out total array size.
      NELM   = 1
      MAXDIM = 1
      DO 1001 I = 1, NDIM
         NELM   = NELM * DIMS(I)
         MAXDIM = MAX( MAXDIM, DIMS(I) )
 1001 CONTINUE

*  Check work space size.
      IF ( NWORK .LT. 6 * MAXDIM + 15 ) THEN
         CALL PAR_WRUSER( 'Insufficient work space for FFT.', IGNORE )
         GO TO 500
      END IF

*  If IDATA to be assumed given all zero, set it zero.
      IF ( NOIMAG ) THEN
         DO 1002 I = 1, NELM
            IDATA(I) = 0D0
 1002    CONTINUE
      END IF


*  There are four cases to distinguish, any combination of
*  forward/reverse and one/multi-dimensional.

*  One could forget the slightly complex 1-D code and just use the N-D
*  routine all the time.

*  If 1-D forward.
      IF ( NDIM .EQ. 1 .AND. FORWRD ) THEN

*     Merge the separate spatial-domain array parts into single array.
*     Initialise work space with DCFFTI.
*     Transform with DCFFTF.
*     Convert frequency-domain data to NAG format.
         J = 0
         DO 1003 I = 1, NELM
            J = J + 1
            WORK(J) = RDATA(I)
            J = J + 1
            WORK(J) = IDATA(I)
 1003    CONTINUE
         CALL PDA_DCFFTI( NELM,          WORK(2*NELM+1) )
         CALL PDA_DCFFTF( NELM, WORK(1), WORK(2*NELM+1) )
         CALL PDA_DC2NAG( NELM, WORK(1), RDATA, IDATA   )

*  Else if 1-D reverse.
      ELSE IF ( NDIM .EQ. 1 ) THEN

*     Convert frequency-domain data from NAG format.
*     Initialise work space with DCFFTI.
*     Transform with DCFFTB.
*     Separate the spatial-domain array parts from single array.
         CALL PDA_DNAG2C( NELM, RDATA, IDATA, WORK(1)   )
         CALL PDA_DCFFTI( NELM,          WORK(2*NELM+1) )
         CALL PDA_DCFFTB( NELM, WORK(1), WORK(2*NELM+1) )
         J = 0
         DO 1004 I = 1, NELM
            J = J + 1
            RDATA(I) = WORK(J)
            J = J + 1
            IDATA(I) = WORK(J)
 1004    CONTINUE

*  Else if N-D forward.
      ELSE IF ( FORWRD ) THEN
         CALL PDA_DNFFTF( NDIM, DIMS, RDATA, IDATA, WORK, IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            CALL PAR_WRUSER( 'Invalid dimensionality for FFT.', IGNORE )
            GO TO 500
         END IF

*  Else (N-D reverse).
      ELSE
         CALL PDA_DNFFTB( NDIM, DIMS, RDATA, IDATA, WORK, IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            CALL PAR_WRUSER( 'Invalid dimensionality for FFT.', IGNORE )
            GO TO 500
         END IF

      END IF

*  Normal return.
      RETURN

*  Error return.
 500  STATUS = 1

      END
