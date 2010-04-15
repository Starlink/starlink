      SUBROUTINE SPD_WZPER( VARUSE, KMAX, LMAX, VK, COEFF,
     :   CLM, STATUS )
*+
*  Name:
*     SPD_WZPE{DR}

*  Purpose:
*     Covariance created in resampling.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPER( VARUSE, KMAX, LMAX, VK, COEFF, CLM, STATUS )

*  Description:
*     This routine calculates the covariance created in a resampling
*     process from the matrix of resample coefficients and the vector of
*     pre-resample variance. This routine is originally written for use
*     by the Specdre RESAMP application. The theoretical foundations
*     are described by Meyerdierks (1992). The operation performed by
*     this routine is (depending on availability of the given vector):
*
*        C_lm = sum_k { O_kl O_km V_k }
*
*        C_lm = sum_k { O_kl O_km }
*
*     The routine does take care of bad values in the given vector.
*     The given coefficient matrix must not contain bad values.
*
*     The returned covariance matrix will have no bad values:
*     For an off-diagonal element, if the sum of coefficient products is
*     zero, then all coefficient products are zero, then there is no
*     input pixel contributing to both output pixels involved in this
*     covariance element, then the covariance should be zero.
*     For a diagonal element, if the sum of coefficient squares is zero,
*     then all coefficients are zero, then there is no input pixel
*     contribution to the output pixel involved, then that output pixel
*     is bad, then its variance value is irrelevant and may be set zero.
*
*     If the input variance (given vector) is not available, then the
*     result matrix will contain the covariance except for a constant
*     factor, the global variance.

*  Arguments:
*     VARUSE = LOGICAL (Given)
*        If false, the given vector VK is not used.
*     KMAX = INTEGER (Given)
*        The length of rows of the given matrix, i.e. the first
*        dimension of the matrix. This is also the length of the given
*        vector, if it is to be used.
*     LMAX = INTEGER (Given)
*        The length of columns and rows in the result matrix. This is
*        also the length of columns of the given matrix, i.e. the second
*        dimension of the matrix.
*     VK( KMAX ) = REAL (Given)
*        The given vector.
*     COEFF( KMAX, LMAX ) = REAL (Given)
*        The given matrix.
*     CLM( LMAX, LMAX ) = REAL (Returned)
*        The result matrix.
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
*        Better documentation. Take care of bad values in VK.
*     26 Jan 1995 (hme):
*        Renamed from SPAAKx.
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
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL VARUSE
      INTEGER KMAX
      INTEGER LMAX
      REAL VK( KMAX )
      REAL COEFF( KMAX, LMAX )

*  Arguments Returned:
      REAL CLM( LMAX, LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER K                  ! Index within row of coefficients
      INTEGER L                  ! Index counting row
      INTEGER M                  ! Index counting within covariance row
      REAL SUMK                ! Sum over a coefficient row
      REAL WEIGHT              ! Addend from a row element
      REAL VALLM               ! Value for returned matrix

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If variance available.
      IF ( VARUSE ) THEN

*     Loop through output matrix elements (l,m).
         DO 3 L = 1, LMAX
            DO 2 M = 1, L

*           Initialise the sums over the row of the given matrix.
               SUMK  = 0.
               VALLM = 0.

*           Add up the sums over the row and the scalar product.
               DO 1 K = 1, KMAX
                  WEIGHT = COEFF(K,L) * COEFF(K,M)
                  SUMK   = SUMK  + WEIGHT
                  IF ( VK(K) .NE. VAL__BADR )
     :               VALLM  = VALLM + WEIGHT * VK(K)
 1             CONTINUE

*           Store result in matrix. Set zero if no weight accumulated.
               IF ( SUMK .EQ. 0. ) THEN
                  CLM(M,L) = SUMK
                  CLM(L,M) = SUMK
               ELSE
                  CLM(M,L) = VALLM
                  CLM(L,M) = VALLM
               END IF
 2          CONTINUE
 3       CONTINUE

*  Else.
      ELSE

*     Loop through output matrix elements (l,m).
         DO 6 L = 1, LMAX
            DO 5 M = 1, L

*           Initialise the sums over the row of the given matrix.
               SUMK = 0.

*           Add up the sums over the row and the scalar product. Take care
*           of bad values in given vector, but not in matrix.
               DO 4 K = 1, KMAX
                  WEIGHT = COEFF(K,L) * COEFF(K,M)
                  SUMK   = SUMK + WEIGHT
 4             CONTINUE

*           Store result in matrix. Set zero if no weight accumulated.
               CLM(M,L) = SUMK
               CLM(L,M) = SUMK
 5          CONTINUE
 6       CONTINUE
      END IF

*  Return.
      END
