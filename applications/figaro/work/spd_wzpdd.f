      SUBROUTINE SPD_WZPDD( VARUSE, KMAX, LMAX, IK, VK,
     :   MAT, IL, STATUS )
*+
*  Name:
*     SPD_WZPD{DR}

*  Purpose:
*     Resample data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPDD( VARUSE, KMAX, LMAX, IK, VK, MAT, IL, STATUS )

*  Description:
*     This routine performs the multiplication of a weight matrix with a
*     data vector.
*
*     Note that the given matrix contains non-normalised weights and the
*     result is supposed to be a weighted mean. That means, the result
*     vector component is not simply the scalar product of the matrix
*     row with the given vector.
*
*     This routine is originaly written for use by the Specdre RESAMP
*     application. The theoretical foundations are described by
*     Meyerdierks (1992). The operation on the matrix and vectors is
*
*                    O'_kl
*        O_kl = -----------------
*                sum_i { O'_il }
*
*        I_l = sum_k { O_kl I_k }
*
*     This routine will alter the matrix from weights to coefficients.
*     Thus matrix elements will be set to zero where the corresponding
*     given data or variance are bad. And the matrix elements will be
*     normalised so that the sum over any row is 1 (or 0).
*
*     The routine takes care of bad values in the data and variance
*     vectors: If a data value or its variance are bad, the data value
*     is ignored and the coefficient set to zero. If the combined effect
*     of bad data or variance and of zero coefficients leads to a result
*     data value being undefined, that value defaults to bad. (Actually
*     a bad resampled data value is equivalent to the row of
*     coefficients being all zero.)
*
*     The weight matrix must not have bad values, and the coefficient
*     matrix will not have bad values either.

*  Arguments:
*     VARUSE = LOGICAL (Given)
*        If false, the variance vector is not used.
*     KMAX = INTEGER (Given)
*        The length of the given vector. This is also the length of
*        matrix rows, i.e. the first dimension of the matrix.
*     LMAX = INTEGER (Given)
*        The length of the result vector. This is also the length of
*        matrix columns, i.e. the second dimension of the matrix.
*     IK( KMAX ) = DOUBLE PRECISION (Given)
*        The given vector of data values.
*     VK( KMAX ) = DOUBLE PRECISION (Given)
*        The given vector of variances. This is only needed to check for
*        bad values. If VARUSE is false, it is not used.
*     MAT( KMAX, LMAX ) = DOUBLE PRECISION (Given and Returned)
*        On entry this is the weight matrix. On exit it is the
*        coefficient matrix.
*     IL( LMAX ) = DOUBLE PRECISION (Returned)
*        The result vector of resampled data values.
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
*        Renamed from SPAADx.
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
      DOUBLE PRECISION IK( KMAX )
      DOUBLE PRECISION VK( KMAX )

*  Arguments Given and Returned:
      DOUBLE PRECISION MAT( KMAX, LMAX )

*  Arguments Returned:
      DOUBLE PRECISION IL( LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER K                  ! Index within row
      INTEGER L                  ! Index counting row
      DOUBLE PRECISION WEIGHT              ! Weight
      DOUBLE PRECISION SUMK                ! Sum of weights
      DOUBLE PRECISION VALK                ! Input data value
      DOUBLE PRECISION VALL                ! Output data value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If input variance must be considered.
      IF ( VARUSE ) THEN

*     Loop through rows (l).
         DO 3 L = 1, LMAX

*        Initialise the sum over the row and the scalar product.
            SUMK = 0.
            VALL = 0.

*        Add up the sum over the row and the scalar product. Take care
*        of bad values in given data or variance, but not in matrix. A
*        bad value in the given vectors results in a zero coefficient in
*        the matrix.
            DO 1 K = 1, KMAX
               VALK   = IK(K)
               WEIGHT = MAT(K,L)
               IF (   VALK .EQ. VAL__BADD .OR.
     :               VK(K) .EQ. VAL__BADD ) THEN
                  MAT(K,L) = 0.
               ELSE
                  SUMK = SUMK + WEIGHT
                  VALL = VALL + WEIGHT * VALK
               END IF
 1          CONTINUE

*        Turn the weighted sum into a weighted mean and normalise the
*        matrix row to contain coefficients rather than weights.
            IF ( SUMK .EQ. 0. ) THEN
               IL(L) = VAL__BADD
            ELSE
               IL(L) = VALL / SUMK
               DO 2 K = 1, KMAX
                  MAT(K,L) = MAT(K,L) / SUMK
 2             CONTINUE
            END IF
 3       CONTINUE

*  Else (must not touch the variance array).
      ELSE

*     Loop through rows (l).
         DO 6 L = 1, LMAX

*        Initialise the sum over the row and the scalar product.
            SUMK = 0.
            VALL = 0.

*        Add up the sum over the row and the scalar product. Take care
*        of bad values in given data, but not in matrix. A bad value in
*        the given vector results in a zero coefficient in the matrix.
            DO 4 K = 1, KMAX
               VALK   = IK(K)
               WEIGHT = MAT(K,L)
               IF ( VALK .EQ. VAL__BADD ) THEN
                  MAT(K,L) = 0.
               ELSE
                  SUMK = SUMK + WEIGHT
                  VALL = VALL + WEIGHT * VALK
               END IF
 4          CONTINUE

*        Turn the weighted sum into a weighted mean and normalise the
*        matrix row to contain coefficients rather than weights.
            IF ( SUMK .EQ. 0. ) THEN
               IL(L) = VAL__BADD
            ELSE
               IL(L) = VALL / SUMK
               DO 5 K = 1, KMAX
                  MAT(K,L) = MAT(K,L) / SUMK
 5             CONTINUE
            END IF
 6       CONTINUE
      END IF

*  Return.
      END
