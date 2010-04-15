      SUBROUTINE SPD_WZPCD( INFO, KMAX, LMAX, XK, WK, XL, WL, OKL,
     :   STATUS )
*+
*  Name:
*     SPD_WZPC{DR}

*  Purpose:
*     Calculate pixel overlap matrix.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPCD( INFO, KMAX, LMAX, XK, WK, XL, WL, OKL, STATUS )

*  Description:
*     This routine calculates the matrix of overlaps between two sets
*     of one-dimensional pixel arrays.
*
*     If the first set of pixels was contiguous and the pixel from the
*     second set is neither the first nor the last in the second set,
*     then the sum (over the first set) of overlaps should equal the
*     width (from the second set). This routine issues a warning if this
*     is not even nearly fulfilled.
*
*     This routine is originally written for use by Specdre's RESAMP
*     application. The theoretical foundations are discussed by
*     Meyerdierks (1992). The overlap matrix is defined as
*
*        O'_kl = min{ x_l + w_l/2; x_k + w_k/2 }
*              - max{ x_l - w_l/2; x_k - w_k/2 }     if > 0
*
*        O'_kl = 0                                   else

*  Arguments:
*     INFO = LOGICAL (Given)
*        False if warning message about contiguousness is to be
*        suppressed.
*     KMAX = INTEGER (Given)
*        The length of the first array of pixel positions and widths.
*        This is also the length of matrix rows, i.e. the first
*        dimension of the matrix.
*     LMAX = INTEGER (Given)
*        The length of the second array of pixel positions and widths.
*        This is also the length of matrix columns, i.e. the second
*        dimension of the matrix.
*     XK( KMAX ) = DOUBLE PRECISION (Given)
*        The first array of positions.
*     WK( KMAX ) = DOUBLE PRECISION (Given)
*        The first array of widths.
*     XL( LMAX ) = DOUBLE PRECISION (Given)
*        The second array of positins.
*     WL( LMAX ) = DOUBLE PRECISION (Given)
*        The second array of widths.
*     OKL( KMAX, LMAX ) = DOUBLE PRECISION (Returned)
*        The matrix of overlaps between pixel in either array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Meyerdierks, H., 1992, Covariance in Resampling and Model Fitting,
*     Starlink, Spectroscopy Special Interest Group

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1992 (hme):
*        Original version.
*     13 Jul 1992 (hme):
*        Better documentation.
*     26 Jan 1995 (hme):
*        Renamed from SPAACx.
*     15 Oct 1997 (acc):
*        Change name RESAMPLE to RESAMP due to clash of names with FIGARO.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL INFO
      INTEGER KMAX
      INTEGER LMAX
      DOUBLE PRECISION XK( KMAX )
      DOUBLE PRECISION WK( KMAX )
      DOUBLE PRECISION XL( LMAX )
      DOUBLE PRECISION WL( LMAX )

*  Arguments Returned:
      DOUBLE PRECISION OKL( KMAX, LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL VAL__ZEROR            ! Zero in appropriate type
      PARAMETER ( VAL__ZEROR = 0. )
      DOUBLE PRECISION VAL__ZEROD ! Zero in appropriate type
      PARAMETER ( VAL__ZEROD = 0D0 )
      REAL EPSP1                 ! Epsilon + 1: Precision for checking
      PARAMETER ( EPSP1 = 1.1 )  ! equality of sum of overlaps and width

*  Local Variables:
      INTEGER K                  ! Index within row
      INTEGER L                  ! Index counting rows
      DOUBLE PRECISION SUMK                ! Sum over the row
      DOUBLE PRECISION LEFTK               ! Lower bound of pixel from first set
      DOUBLE PRECISION RIGHTK              ! Upper bound of pixel from first set
      DOUBLE PRECISION LEFTL               ! Lower bound of pixel from second set
      DOUBLE PRECISION RIGHTL              ! Upper bound of pixel from second set

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through rows (l) (second set of pixels).
      DO 2 L = 1, LMAX

*     Lower and upper bounds of pixel from second set.
         LEFTL  = XL(L) - WL(L) / 2.
         RIGHTL = XL(L) + WL(L) / 2.

*     Initialise the sum over the row.
         SUMK = VAL__ZEROD

*     Loop through elements of the row (k).
         DO 1 K = 1, KMAX

*        Lower and upper bounds of pixel from first set.
            LEFTK  = XK(K) - WK(K) / 2.
            RIGHTK = XK(K) + WK(K) / 2.

*        Overlap.
            OKL(K,L) = MIN( RIGHTL, RIGHTK )
     :               - MAX(  LEFTL,  LEFTK )
            OKL(K,L) = MAX( OKL(K,L), VAL__ZEROD )

*        Update the sum over the row.
            SUMK = SUMK + OKL(K,L)
 1       CONTINUE

*     Check for contiguousness.
         IF (  INFO .AND.
     :         L .NE. 1 .AND. L .NE. LMAX .AND.
     :         SUMK .NE. VAL__ZEROD ) THEN
            IF ( SUMK .GT. EPSP1 * WL(L) ) THEN
               CALL MSG_OUT( 'SPD_WZPC_NCNTG',
     :            'A set of pixels seems to be non-contiguous.',
     :            STATUS )
               CALL MSG_OUT( 'SPD_WZPC_NCNTG',
     :            'This may severely compromise the proper ' //
     :            'intepretation of pixel widths.',
     :            STATUS )
            END IF
         END IF
 2    CONTINUE

*  Return.
      END
