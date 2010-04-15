      SUBROUTINE SPD_WZPJD( INFO, LMAX, VECI, IL,
     :   VEC, MAT, GOODC, GOODV, STATUS )
*+
*  Name:
*     SPD_WZPJ{DR}

*  Purpose:
*     Get final variance an covariance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPJD( INFO, LMAX, VECI, IL, VEC, MAT, GOODC, GOODV,
*        STATUS )

*  Description:
*     This routine derives the final variance and covariance for
*     RESAMP. The final variance is based on the scatter between the
*     different input data sets. Contrary to that the covariance matrix
*     is based on the pixel coupling (overlap) and on the hypothesis of
*     a constant data variance that applies to each pixel in each input
*     data set. The global input variance is calculated by this routine.
*     It is reported, but not returned to the calling routine. It is
*     also used to scale the covariance matrix.
*
*     The theoretical foundations of RESAMP are described by
*     Meyerdierks (1992). This routine is for the case of unknown input
*     variances V_nk. These will have been assumed to be all equal. If
*     they were all equal to 1, V'_nl would have been the post-resample
*     variances in the individual spectra and C'_nlm the covariances.
*     Here the variance of any pixel l is calculated as the variance of
*     the weighted mean I_l of indiviual spectra I_nl:
*
*                  1       sum_n { ( I_nl - I_l )^2 / V'_nl }
*        V_l = ---------  ------------------------------------
*               N_l - 1          sum_n { 1 / V'_nl }
*
*               sum_n { I_nl^2 / V'_nl } - I_l^2 sum_n { 1 / V'_nl }
*            = ------------------------------------------------------
*                        ( N_l - 1 ) sum_n { 1 / V'_nl }
*
*                    1           VEC(l)
*            = ------------- { ----------  - IL(l)^2 }
*               VECI(l) - 1     MAT(l,l)
*
*     This allows for each pixel an estimate of the original global
*     pre-resample variance V_nk = V_0, because V_l = V_0 V'_l and
*     1 / V'_l = sum_n { 1 / V'_nl }. We estimate V_0 as a weighted
*     mean:
*
*        V_0 = sum_l { N_l V_l sum_n { 1 / V'_nl } } / sum_l { N_l }
*
*            = sum_l { VECI(l) VEC(l) MAT(l,l) } / sum_l { VECI(l) }
*
*     This global pre-resample variance is then used to scale the
*     covariance matrix.
*
*                                  C_nlm
*        C_lm = V_l V_m sum_n { ----------- }
*                                V_nl V_nm
*
*                                        C'_nlm
*             = V_0 V'_l V'_m sum_n { ------------- }
*                                      V'_nl V'_nm
*
*                                         C'_nlm
*                         V_0 sum_n { --------------- }
*                                      C'_nll C'_nmm
*             = -----------------------------------------------------
*                             C'_nll                    C'_nmm
*                sum_n { --------------- } sum_n { --------------- }
*                         C'_nll C'_nll             C'_nmm C'_nmm
*
*             = V_0 MAT(L,M) / ( MAT(L,L) MAT(M,M) )
*
*     The operation on the matrix is to divide each off-diagonal
*     element by both related diagonal elements (those in the same
*     column and row respectively). The operation on diagonal elements
*     is to invert them. Since the matrix is operated on in situ, all
*     off-diagonal elements are divided before the diagonal is updated.
*     Furthermore, each matrix element is multiplied by the global input
*     variance.
*
*     An off-diagonal element equal to zero or to the bad value retains
*     its value regardless of the related diagonal elements. If one or
*     both of the related diagonal elements are bad, the off-diagonal
*     element is set to zero (!).
*
*     A diagonal element equal to zero or the bad value is set to the
*     bad value. A diagonal element is also considered bad, if the
*     corresponding element of the counter vector is zero or less.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, the global input variance will be reported.
*     LMAX = INTEGER (Given)
*        The length of vectors, matrix rows and columns.
*     VECI( LMAX ) = INTEGER (Given)
*        For each element of the data value vector, zero indicates
*        a bad element while a value greater than zero indicates a good
*        element.
*     IL( LMAX ) = DOUBLE PRECISION (Given)
*        The vector of data values.
*     VEC( LMAX ) = DOUBLE PRECISION (Given and Returned)
*        On entry, this is the vector of the sums of squared data
*        values. On exit it is the vector of variances.
*     MAT( LMAX, LMAX ) = DOUBLE PRECISION (Given and Returned)
*        On input the weighted sum of matrices. On output the covariance
*        matrix.
*     GOODC = LOGICAL (Returned)
*        True if a sensible covariance matrix is returned.
*     GOODV = LOGICAL (Returned)
*        True if a sensible variance vector is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Meyerdierks, H., 1992, Covariance in Resampling and Model Fitting,
*     Starlink, Spectroscopy Special Interest Group

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     ACC: Anne Charles (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     14-FEB-1992 (HME):
*        Original version.
*     13-JUL-1992 (HME):
*        Better documentation.
*        Correct calculation of V_l (and V_0?).
*     26 Jan 1995 (hme):
*        Renamed from SPAAJx.
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
      LOGICAL INFO
      INTEGER LMAX
      INTEGER VECI( LMAX )
      DOUBLE PRECISION IL( LMAX )

*  Arguments Given and Returned:
      DOUBLE PRECISION VEC( LMAX )
      DOUBLE PRECISION MAT( LMAX, LMAX )

*  Arguments Returned:
      LOGICAL GOODC
      LOGICAL GOODV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL VAL__ZEROR            ! Zero in appropriate type
      PARAMETER ( VAL__ZEROR = 0. )
      DOUBLE PRECISION VAL__ZEROD ! Zero in appropriate type
      PARAMETER ( VAL__ZEROD = 0D0 )

*  Local Variables:
      INTEGER L                  ! Index counting rows
      INTEGER M                  ! Index within rows
      REAL COUNTL                ! Given counter
      DOUBLE PRECISION VALIL               ! Data value
      DOUBLE PRECISION VALVEC              ! Vector component
      DOUBLE PRECISION VALMLL              ! Matrix diagonal element
      DOUBLE PRECISION VALMMM              ! Matrix diagonal element
      DOUBLE PRECISION VALMLM              ! Matrix element
      DOUBLE PRECISION SUML1               ! A sum over L
      DOUBLE PRECISION SUML2               ! Another sum over L
      DOUBLE PRECISION V0                  ! The global input variance

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise sums over L.
      SUML1 = VAL__ZEROD
      SUML2 = VAL__ZEROD
      GOODV = .FALSE.

*  Loop through vector elements (l).
*  This first works out the variance V_l, then it adds that into the
*  statistics on V_0.
      DO 1 L = 1, LMAX
         COUNTL = FLOAT(VECI(L))
         VALIL  = IL(L)
         VALVEC = VEC(L)
         VALMLL = MAT(L,L)

*     If this pixel can make a contribution to variance.
         IF (  COUNTL .GT. 1.5 .AND.
     :         VALIL  .NE. VAL__BADD .AND.
     :         VALVEC .NE. VAL__BADD .AND.
     :         VALMLL .NE. VAL__BADD .AND.
     :         VALMLL .NE. VAL__ZEROD ) THEN

*        Output variance.
*        This must not be negative.
            VEC(L) = ( VALVEC / VALMLL - VALIL * VALIL  )
     :               / ( COUNTL - 1. )
            VEC(L) = MAX( VEC(L), VAL__ZEROD )
            GOODV  = .TRUE.

*        Global input variance: sum of weights and weight times value
*        for average.
            SUML1 = SUML1 + COUNTL
            SUML2 = SUML2 + COUNTL * VEC(L) * VALMLL

*     Else, set variance bad.
         ELSE
            VEC(L) = VAL__BADD
         END IF
 1    CONTINUE

*  Global input variance.
*  If it exists, report it.
*  If it cannot be calculated, there is no point in pondering about the
*  covariance.
      IF ( SUML1 .GT. VAL__ZEROD ) THEN
         GOODC = .TRUE.
         V0    = SUML2 / SUML1
         IF ( INFO ) THEN
            CALL MSG_SETD( 'SPD_WZPJD_T01', V0 )
            CALL MSG_OUT( 'SPD_WZPJD_M01', 'The global variance ' //
     :         'in the input NDFs was ^SPD_WZPJD_T01.',
     :         STATUS )
         END IF
      ELSE IF ( INFO ) THEN
         GOODC = .FALSE.
         CALL MSG_OUT( 'SPD_WZPJD_M02',
     :      'The global variance of input NDFs is ' //
     :      'undefined. Output data will lack ' //
     :      'covariance information although resampling ' //
     :      'may have caused pixel interdependence.',
     :      STATUS )
         GO TO 500
      END IF

*  Loop through off-diagonal elements (l,m<>l).
      DO 3 L = 1, LMAX
         VALMLL = MAT(L,L)
         DO 2 M = 1, LMAX
            IF ( M .NE. L ) THEN
               VALMMM = MAT(M,M)
               VALMLM = MAT(M,L)

*           Zero or bad remains so.
               IF (  VALMLM .EQ. 0. .OR.
     :               VALMLM .EQ. VAL__BADD ) THEN
                  CONTINUE

*           Bad diagonal yields zero off-diagonal.
*           (Bad diagonal may be signalled by zero counter.)
               ELSE IF ( VECI(L) .LE. 0 .OR. VECI(M) .LE. 0 .OR.
     :               VALMMM * VALMLL .EQ. 0 .OR.
     :               VALMMM .EQ. VAL__BADD .OR.
     :               VALMLL .EQ. VAL__BADD ) THEN
                  MAT(M,L) = 0.

*           All's well, have to work.
               ELSE
                  MAT(M,L) = V0 * VALMLM / VALMMM / VALMLL
               END IF
            END IF
 2       CONTINUE
 3    CONTINUE

*  Loop through diagonal (l).
      DO 4 L = 1, LMAX
         VALMLL = MAT(L,L)

*     If diagonal is bad.
         IF (  VECI(L)  .LE. 0 .OR.
     :         VALMLL .EQ. 0 .OR.
     :         VALMLL .EQ. VAL__BADD ) THEN
            MAT(L,L) = VAL__BADD

*     Else
         ELSE
            MAT(L,L) = V0 / VALMLL
         END IF
 4    CONTINUE

*  Return.
 500  CONTINUE
      END
