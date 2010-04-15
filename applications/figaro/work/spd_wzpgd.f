      SUBROUTINE SPD_WZPGD( PROPCO, LMAX, VECI, SCLM, IL, STATUS )
*+
*  Name:
*     SPD_WZPG{DR}

*  Purpose:
*     Divide weighted sum vector by sum of weights.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPGD( PROPCO, LMAX, VECI, SCLM, IL, STATUS )

*  Description:
*     This routine divides the components of a vector by the
*     corresponding diagonal elements of a matrix. A vector element is
*     considered bad if it has the bad value or if the corresponding
*     element of the counter vector is zero. A matrix diagonal element
*     is considered bad if it has the bad value or is zero.
*     This routine is originally written for use by the Specdre RESAMP
*     application. The theoretical foundation is described by
*     Meyerdierks (1992). The operation of this routine is
*
*                 sum_n { I_nl / C_nll }           IL(L)
*          I_l = ------------------------     = -----------
*                   sum_n { 1 / C_nll }          SCLM(L,L)

*  Arguments:
*     PROPCO = LOGICAL (Given)
*        If false, SCLM is not touched and IL divided by VECI instead.
*     LMAX = INTEGER (Given)
*        The length of the given vector, also the length of columns and
*        rows of given matrix.
*     VECI( LMAX ) = INTEGER (Given)
*        For each component of the given vector, zero indicates a bad
*        component while a value greater than zero indicates a good
*        value.
*     SCLM( LMAX, LMAX ) = DOUBLE PRECISION (Given)
*        The components of the given vector are divided by the diagonal
*        elements of this matrix.
*     IL( LMAX ) = DOUBLE PRECISION (Given and Returned)
*        On input the weighted sum of vectors. On output the weighted
*        mean of vectors.
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
*     29 Jan 1993 (hme):
*        Introduce PROPCO argument.
*     26 Jan 1995 (hme):
*        Renamed from SPAAGx.
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
      INTEGER VECI( LMAX )
      DOUBLE PRECISION SCLM( LMAX, LMAX )

*  Arguments Given and Returned:
      DOUBLE PRECISION IL( LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER L                  ! Index counting vector components
      DOUBLE PRECISION VALIL               ! Data value
      DOUBLE PRECISION VALCL               ! Matrix element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If SCLM is available (and covariance to be propagated).
      IF ( PROPCO ) THEN

*     Loop through vector components (l).
         DO 1 L = 1, LMAX
            VALIL = IL(L)
            VALCL = SCLM(L,L)

*        If both operands are good.
            IF ( VECI(L) .GT. 0  .AND.
     :            VALCL  .NE. 0. .AND.
     :            VALIL  .NE. VAL__BADD .AND.
     :            VALCL  .NE. VAL__BADD ) THEN
               IL(L) = VALIL / VALCL

*        Else.
            ELSE
               IL(L) = VAL__BADD
            END IF
 1       CONTINUE

*  Else (SCLM must not be touched, divide by VECI instead).
      ELSE

*     Loop through vector components (l).
         DO 2 L = 1, LMAX
            VALIL = IL(L)
            VALCL = FLOAT(VECI(L))

*        If both operands are good.
            IF ( VALCL .GT. 0. .AND. VALIL .NE. VAL__BADD ) THEN
               IL(L) = VALIL / VALCL

*        Else.
            ELSE
               IL(L) = VAL__BADD
            END IF
 2       CONTINUE
      END IF

*  Return.
      END
