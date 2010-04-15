      SUBROUTINE SPD_WZPFR( PROPCO, LMAX, INL, CNLM,
     :   VECI, SIL, SILSQ, SCLM, STATUS )
*+
*  Name:
*     SPD_WZPF{DR}

*  Purpose:
*     Add up resampled data and covariance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPFR( PROPCO, LMAX, INL, CNLM,
*        VECI, SIL, SILSQ, SCLM, STATUS )

*  Description:
*     This routine adds a vector, the vector of its squared components
*     and a related matrix into appropriate vectors and matrices holding
*     the sums. The given matrix contains the weights to be used in
*     summing up. This routine is originally written for use by the
*     Specdre RESAMP application. The theoretical foundations are
*     described by Meyerdierks (1992). The operation of this routine is
*
*        N_l = N_l + 1
*
*        S(I_l)   = S(I_l)   + I_nl   / C_nll
*
*        S(I_l^2) = S(I_l^2) + I_nl^2 / C_nll
*
*        S(C_lm)  = S(C_lm)  + C_nlm  / ( C_nll C_nmm )
*
*     Finally an integer vector counts the non-bad values in each
*     component of the given vector. Elements in the given vector or the
*     given matrix may have the bad value. If so, the respective sum is
*     not updated. If the given vector component is bad the
*     corresponding counter is also not updated. There is, however no
*     counter for non-bad addends in the matrix summation.
*
*     If CNLM and SCNLM are not available, PROPCO can be set false and
*     this routine will calculate
*
*        N_l = N_l + 1
*
*        S(I_l)   = S(I_l)   + I_nl
*
*        S(I_l^2) = S(I_l^2) + I_nl^2

*  Arguments:
*     PROPCO = LOGICAL (Given)
*        False for no covariance (or variance) processing. CNLM and SCLM
*        will not be touched if this argument is false.
*     LMAX = INTEGER (Given)
*        The length of the given and returned vectors, also the length
*        of matrix rows and columns.
*     INL( LMAX ) = REAL (Given)
*        The vector to be included in the weighted sum.
*     CNLM( LMAX, LMAX ) = REAL (Given)
*        The given matrix. This contains on the diagonal the inverse
*        weights to be used.
*     VECI( LMAX ) = INTEGER (Given and Returned)
*        The vector of counters. These give for each compnent the number
*        of addends that entered the sum.
*     SIL( LMAX ) = REAL (Given and Returned)
*        The sum vector.
*     SILSQ( LMAX ) = REAL (Given and Returned)
*        The vector of the sums of squared components.
*     SCLM( LMAX, LMAX ) = REAL (Given and Returned)
*        The sum matrix.
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
*     24 Jan 1993 (hme):
*        Replace VARUSE argument with PROPCO. Optionally not propagate
*        covariance.
*     26 Jan 1995 (hme):
*        Renamed from SPAAFx.
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
      LOGICAL PROPCO
      INTEGER LMAX
      REAL INL( LMAX )
      REAL CNLM( LMAX, LMAX )

*  Arguments Given and Returned:
      INTEGER VECI( LMAX )
      REAL SIL( LMAX )
      REAL SILSQ( LMAX )
      REAL SCLM( LMAX, LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER L                  ! Index counting rows
      INTEGER M                  ! Index within rows
      REAL VALINL              ! Data value
      REAL VALCLL              ! Covariance value
      REAL VALCMM              ! Covariance value
      REAL VALCLM              ! Covariance value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If CNLM is available, use its inverted diagonal (1/var) as averaging
*  weight and average it into SCLM as well.
      IF ( PROPCO ) THEN

*     Loop through matrix rows and vector elements (l).
         DO 2 L = 1, LMAX

*        Add up vector.
*        If the given vector component is bad, it obviously does not
*        contribute to the sums. If its weight is bad or 1/0, it cannot
*        be entered either.
            VALINL = INL(L)
            VALCLL = CNLM(L,L)
            IF (  VALINL .NE. VAL__BADR .AND.
     :            VALCLL .NE. VAL__BADR .AND.
     :            VALCLL .NE. 0. ) THEN
               SIL(L)   = SIL(L)  + VALINL / VALCLL
               VECI(L)  = VECI(L) + 1
               SILSQ(L) = SILSQ(L) + VALINL*VALINL / VALCLL
            END IF

*        Loop through elements of row (m).
            DO 1 M = 1, LMAX

*           Add up matrix.
*           There are two weights that must neither be bad nor zero.
*           These are the two diagonal elements that are in the same row
*           and in the same column respectively.
*           For diagonal elements this in effect is the sum of inverse
*           diagonal elements.
               VALCMM = CNLM(M,M)
               VALCLM = CNLM(M,L)
               IF (  VALCLM .NE. VAL__BADR .AND.
     :               VALCLL .NE. VAL__BADR .AND.
     :               VALCMM .NE. VAL__BADR .AND.
     :               VALCLL*VALCMM .NE. 0. ) THEN
                  SCLM(M,L) = SCLM(M,L)
     :                      + VALCLM / VALCLL / VALCMM
               END IF
 1          CONTINUE
 2       CONTINUE

*  Else (CMLM unavailable), just use 1 as weight.
*  (Since VARUSE will be false in this case, we do not check.)
      ELSE

*     Loop through matrix rows and vector elements (l).
         DO 3 L = 1, LMAX

*        Add up vector.
*        If the given vector component is bad, it obviously does not
*        contribute to the sums.
            VALINL = INL(L)
            IF (  VALINL .NE. VAL__BADR ) THEN
               SIL(L)   = SIL(L)  + VALINL
               VECI(L)  = VECI(L) + 1
               SILSQ(L) = SILSQ(L) + VALINL*VALINL
            END IF
 3       CONTINUE
      END IF

*  Return.
      END
