      SUBROUTINE SPD_WZPHD( LMAX, VECI, CLM, VL, STATUS )
*+
*  Name:
*     SPD_WZPH{DR}

*  Purpose:
*     Divide weighted sum matrix by sum of weights.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPHD( LMAX, VECI, CLM, VL, STATUS )

*  Description:
*     This routine converts the weighted sum of matrices into the proper
*     average matrix. It also isolates the matrix diagonal into a
*     separate vector.
*
*     The operation on the matrix is to divide each off-diagonal
*     element by both related diagonal elements (those in the same
*     column and row respectively). The operation on diagonal elements
*     is to invert them. Since the matrix is operated on in situ, all
*     off-diagonal elements are divided before the diagonal is updated.
*
*     This routine is originally written for use by the Specdre RESAMP
*     application. The theoretical foundation is described by
*     Meyerdierks (1992). The operation performed is
*
*                                   C_nlm
*        C_lm = V_l V_m sum_n { ------------- }
*                                C_nll C_nmm
*
*                                      C_nlm
*                          sum_n { ------------- }
*                                   C_nll C_nmm
*             = -------------------------------------------------
*                            C_nll                   C_nmm
*                sum_n { ------------- } sum_n { ------------- }
*                         C_nll C_nll             C_nmm C_nmm
*
*             = CLM(L,M) / ( CLM(L,L) CLM(M,M) )
*
*
*        V_l = C_ll
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
*     LMAX = INTEGER (Given)
*        The length of vectors, matrix rows and columns.
*     VECI( LMAX ) = INTEGER (Given)
*        For each diagonal element of the given matrix, zero indicates
*        a bad element while a value greater than zero indicates a good
*        element.
*     CLM( LMAX, LMAX ) = DOUBLE PRECISION (Given and Returned)
*        On input the weighted sum of matrices. On output the weighted
*        average.
*     VL( LMAX ) = DOUBLE PRECISION (Returned)
*        The vector of diagonal elements of the returned matrix CLM.
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
*        Renamed from SPAAHx.
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
      INTEGER LMAX
      INTEGER VECI( LMAX )

*  Arguments Given and Returned:
      DOUBLE PRECISION CLM( LMAX, LMAX )

*  Arguments Returned:
      DOUBLE PRECISION VL( LMAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER L                  ! Index counting rows
      INTEGER M                  ! Index within rows
      DOUBLE PRECISION VALCLL              ! Covariance value
      DOUBLE PRECISION VALCMM              ! Covariance value
      DOUBLE PRECISION VALCLM              ! Covariance value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through off-diagonal elements (l,m<>l).
      DO 2 L = 1, LMAX
         VALCLL = CLM(L,L)
         DO 1 M = 1, LMAX
            IF ( M .NE. L ) THEN
               VALCMM = CLM(M,M)
               VALCLM = CLM(M,L)

*           Zero or bad remains so.
               IF (  VALCLM .EQ. 0. .OR.
     :               VALCLM .EQ. VAL__BADD ) THEN
                  CONTINUE

*           Bad diagonal yields zero off-diagonal.
*           (Bad diagonal may be signalled by zero counter.)
               ELSE IF ( VECI(L) .LE. 0 .OR. VECI(M) .LE. 0 .OR.
     :               VALCLL*VALCMM .EQ. 0 .OR.
     :               VALCMM .EQ. VAL__BADD .OR.
     :               VALCLL .EQ. VAL__BADD ) THEN
                  CLM(M,L) = 0.

*           All's well, have to work.
               ELSE
                  CLM(M,L) = VALCLM / VALCMM / VALCLL
               END IF
            END IF
 1       CONTINUE
 2    CONTINUE

*  Loop through diagonal (l).
      DO 3 L = 1, LMAX
         VALCLL = CLM(L,L)

*     If diagonal is bad.
         IF (  VECI(L)  .LE. 0 .OR.
     :         VALCLL .EQ. 0 .OR.
     :         VALCLL .EQ. VAL__BADD ) THEN
            CLM(L,L) = VAL__BADD

*     Else
         ELSE
            CLM(L,L) = 1. / VALCLL
         END IF

*     Copy into diagonal vector.
         VL(L) = CLM(L,L)
 3    CONTINUE

*  Return.
      END
