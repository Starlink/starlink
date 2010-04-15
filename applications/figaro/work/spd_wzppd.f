      SUBROUTINE SPD_WZPPD( INFO, VARUSE, USEEXT, KMAX, LMAX, DIM2,
     :   XK, WK, IK, VK, XL, WL, OKL, CLM, IL, VL, CRSL, STATUS )
*+
*  Name:
*     SPD_WZPP{DR}

*  Purpose:
*     Resample all rows in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPPD( INFO, VARUSE, USEEXT, KMAX, LMAX, DIM2,
*        XK, WK, IK, VK, XL, WL, OKL, CLM, IL, VL, CRSL, STATUS )

*  Description:
*     This routine resamples all rows of an image. It was written for the
*     "cube" mode of RESAMP.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, completion of each row is reported.
*     VARUSE = LOGICAL (Given)
*        Switch for propagating covariance and variance or not.
*     USEEXT = LOGICAL (Given)
*        If true, XK and WK are assumed to be of same shape as IK and VK.
*        If false they are assumed to be of size KMAX by 1.
*     KMAX = INTEGER (Given)
*        The first dimension of the input array, i.e. the length of the
*        input rows.
*     LMAX = INTEGER (Given)
*        The first dimension of the output array, i.e. the length of the
*        output rows.
*     DIM2 = INTEGER (Given)
*        The second dimension of the input and output arrays, i.e. the
*        number of rows to be resampled.
*     XK( KMAX, DIM2 ) = DOUBLE PRECISION (Given)
*        The array of input pixel positions. If USEEXT is false, only the
*        first row is used.
*     WK( KMAX, DIM2 ) = DOUBLE PRECISION (Given)
*        The array of input pixel widths. If USEEXT is false, only the
*        first row is used.
*     IK( KMAX, DIM2 ) = DOUBLE PRECISION (Given)
*        The array of input data.
*     VK( KMAX, DIM2 ) = DOUBLE PRECISION (Given)
*        The array of input variances.
*     XL( LMAX ) = DOUBLE PRECISION (Given)
*        The array of output pixel positions.
*     WL( LMAX ) = DOUBLE PRECISION (Given)
*        The array of output pixel widths.
*     OKL( KMAX, LMAX ) = DOUBLE PRECISION (Given and Returned)
*        Workspace needed for the pixel overlap matrix.
*     CLM( LMAX, LMAX ) = DOUBLE PRECISION (Given and Returned)
*        Workspace needed for the output covariance matrix.
*     IL( LMAX, DIM2 ) = DOUBLE PRECISION (Returned)
*        The array of output data.
*     VL( LMAX, DIM2 ) = DOUBLE PRECISION (Returned)
*        The array of output variances.
*     CRSL( LMAX, DIM2 ) = DOUBLE PRECISION (Returned)
*        The array of output covariance row sums.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Feb 1993 (hme):
*        Original version.
*     26 Jan 1995 (hme):
*        Renamed from SPADAx.
*     15 Oct 1997 (acc):
*        Change name RESAMPLE to RESAMP due to clash of names with FIGARO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL INFO
      LOGICAL VARUSE
      LOGICAL USEEXT
      INTEGER KMAX
      INTEGER LMAX
      INTEGER DIM2
      DOUBLE PRECISION XK( KMAX, DIM2 )     ! May actually be KMAX by 1
      DOUBLE PRECISION WK( KMAX, DIM2 )     ! May actually be KMAX by 1
      DOUBLE PRECISION IK( KMAX, DIM2 )
      DOUBLE PRECISION VK( KMAX, DIM2 )
      DOUBLE PRECISION XL( LMAX )
      DOUBLE PRECISION WL( LMAX )

*  Arguments Given and Returned:
      DOUBLE PRECISION OKL( KMAX, LMAX )
      DOUBLE PRECISION CLM( LMAX, LMAX )

*  Arguments Returned:
      DOUBLE PRECISION IL( LMAX, DIM2 )
      DOUBLE PRECISION VL( LMAX, DIM2 )
      DOUBLE PRECISION CRSL( LMAX, DIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants.
      REAL ZEROR                 ! Itself
      PARAMETER ( ZEROR = 0. )
      DOUBLE PRECISION ZEROD     ! Itself
      PARAMETER ( ZEROD = 0D0 )

*  Local Variables:
      LOGICAL NEEDC              ! Ignored
      INTEGER I, J               ! Loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If VARUSE.
      IF ( VARUSE ) THEN

*     If axis data come from the Extension.
         IF ( USEEXT ) THEN

*        Loop along second dimension.
            DO 2 I = 1, DIM2

*           Calculate the matrix of overlaps between pixels of i-th
*           input and output pixels.
               CALL SPD_WZPCD( INFO, KMAX, LMAX, XK(1,I), WK(1,I),
     :            XL, WL, OKL, STATUS )

*           Resample data values of i-th input.
*           This also turns the overlaps into resampling coefficients. I.e.
*           the matrix will be set zero where an input intensity is bad,
*           and the matrix will be normalised so that the sum along a row
*           is 1 (or 0).
*           Furthermore, an input value is ignored also if its variance is
*           bad. This is also reflected in the returned coefficient matrix.
               CALL SPD_WZPDD( .TRUE., KMAX, LMAX, IK(1,I), VK(1,I),
     :            OKL, IL(1,I), STATUS )

*           Post-resample covariance.
*           We don't care about post-resample variance, because it is
*           contained as diagonal in the covariance.
               CALL SPD_WZPED( .TRUE., KMAX, LMAX,
     :            VK(1,I), OKL, CLM, STATUS )

*           Extract the variances from the diagonal of the covariance.
               DO 1 J = 1, LMAX
                  VL(J,I) = CLM(J,J)
 1             CONTINUE

*           Add up the covariance row sums.
*           The switch NEEDC is ignored and assumed true.
               CALL SPD_WZPKD( LMAX, CLM, CRSL(1,I),
     :            NEEDC, STATUS )

*           Progress report.
               IF ( INFO ) THEN
                  CALL MSG_SETI( 'SPD_WZPPD_T01', I )
                  CALL MSG_OUT( 'SPD_WZPPD_M01',
     :               '^SPD_WZPPD_T01 rows processed so far.', STATUS )
               END IF
 2          CONTINUE

*     Else (axis data come from axis stucture).
         ELSE

*        Loop along second dimension.
            DO 4 I = 1, DIM2

*           Calculate the matrix of overlaps between pixels of i-th
*           input and output pixels.
               CALL SPD_WZPCD( INFO, KMAX, LMAX, XK, WK,
     :            XL, WL, OKL, STATUS )

*           Resample data values of i-th input.
               CALL SPD_WZPDD( .TRUE., KMAX, LMAX, IK(1,I), VK(1,I),
     :            OKL, IL(1,I), STATUS )

*           Post-resample covariance.
               CALL SPD_WZPED( .TRUE., KMAX, LMAX,
     :            VK(1,I), OKL, CLM, STATUS )

*           Extract the variances from the diagonal of the covariance.
               DO 3 J = 1, LMAX
                  VL(J,I) = CLM(J,J)
 3             CONTINUE

*           Add up the covariance row sums.
*           The switch NEEDC is ignored and assumed true.
               CALL SPD_WZPKD( LMAX, CLM, CRSL(1,I),
     :            NEEDC, STATUS )

*           Progress report.
               IF ( INFO ) THEN
                  CALL MSG_SETI( 'SPD_WZPPD_T01', I )
                  CALL MSG_OUT( 'SPD_WZPPD_M01',
     :               '^SPD_WZPPD_T01 rows processed so far.', STATUS )
               END IF
 4          CONTINUE
         END IF

*  Else (VARUSE is false).
      ELSE

*     If axis data come from the Extension.
         IF ( USEEXT ) THEN

*        Loop along second dimension.
            DO 5 I = 1, DIM2

*           Calculate the matrix of overlaps between pixels of i-th
*           input and output pixels.
               CALL SPD_WZPCD( INFO, KMAX, LMAX, XK(1,I), WK(1,I),
     :            XL, WL, OKL, STATUS )

*           Resample data values of i-th input.
               CALL SPD_WZPDD( .FALSE., KMAX, LMAX, IK(1,I), ZEROD,
     :            OKL, IL(1,I), STATUS )

*           Progress report.
               IF ( INFO ) THEN
                  CALL MSG_SETI( 'SPD_WZPPD_T01', I )
                  CALL MSG_OUT( 'SPD_WZPPD_M01',
     :               '^SPD_WZPPD_T01 rows processed so far.', STATUS )
               END IF
 5          CONTINUE

*     Else (axis data come from axis stucture).
         ELSE

*        Loop along second dimension.
            DO 6 I = 1, DIM2

*           Calculate the matrix of overlaps between pixels of i-th
*           input and output pixels.
               CALL SPD_WZPCD( INFO, KMAX, LMAX, XK, WK,
     :            XL, WL, OKL, STATUS )

*           Resample data values of i-th input.
               CALL SPD_WZPDD( .FALSE., KMAX, LMAX, IK(1,I), ZEROD,
     :            OKL, IL(1,I), STATUS )

*           Progress report.
               IF ( INFO ) THEN
                  CALL MSG_SETI( 'SPD_WZPPD_T01', I )
                  CALL MSG_OUT( 'SPD_WZPPD_M01',
     :               '^SPD_WZPPD_T01 rows processed so far.', STATUS )
               END IF
 6          CONTINUE
         END IF
      END IF

*  Return.
      END
