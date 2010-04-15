      SUBROUTINE SPD_EBABD( MAXDIM, SPAXIS,
     :   DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7,
     :   SPVALS, SPWIDS, STATUS )
*+
*  Name:
*     SPD_EBAB{DR}

*  Purpose:
*     Derive SPECWIDS from SPECVALS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EBABD( MAXDIM, SPAXIS,
*        DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7,
*        SPVALS, SPWIDS, STATUS )

*  Description:
*     This routine derives default values for the spectroscopic pixel
*     widths from the spectroscopic values, where both are stored in the
*     Specdre Extension. The arrays must have been mapped beforehand and
*     must have the same shape. The width values are derived from the
*     centre values in accordance with SUN/33. Thus each width is half
*     the distance between the left and right neighbouring centre. If
*     the pixel in question is at the edge, then its width is simply the
*     distance to the inside neighbour. Note that the input array must
*     not contain bad values.

*  Arguments:
*     MAXDIM = INTEGER (Given)
*        The number of dimensions that the calling routine works with.
*        This must match the local value LMXDIM = 7.
*     SPAXIS = INTEGER (Given)
*        The number of the spectroscopic axis. It is for this axis that
*        the SPVALS values must be pixel centres and the SPWIDS values
*        will be default width values.
*     DIMi = INTEGER (Given)
*        i = 1 ... 7. These are the seven dimensions - or axis lengths
*        or the SPVALS and SPWIDS arrays.
*     SPVALS( DIM1, DIM2, ..., DIM7 ) = DOUBLE PRECISION (Given)
*        The input data, i.e. the array of pixel centres.
*     SPWIDS( DIM1, DIM2, ..., DIM7 ) = DOUBLE PRECISION (Returned)
*        The output data, i.e. the array of pixel widths.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1992 (hme):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MAXDIM
      INTEGER SPAXIS
      INTEGER DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7
      DOUBLE PRECISION SPVALS( DIM1, DIM2, DIM3,
     :   DIM4, DIM5, DIM6, DIM7 )

*  Arguments Returned:
      DOUBLE PRECISION SPWIDS( DIM1, DIM2, DIM3,
     :   DIM4, DIM5, DIM6, DIM7 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER LMXDIM
      PARAMETER ( LMXDIM = 7 )

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER DN( LMXDIM )       ! Pixel step along spectroscopic axis
      INTEGER N1, N2, N3, N4, N5, N6, N7 ! Loop indices for SPWIDS
      INTEGER K1, K2, K3, K4, K5, K6, K7 ! Leftward pixel in SPVALS
      INTEGER L1, L2, L3, L4, L5, L6, L7 ! Rightward pixel in SPVALS
      DOUBLE PRECISION DELTA               ! Actual base for deriving width

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check MAXDIM.
      IF ( MAXDIM .NE. LMXDIM  ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPABJ_MXDIM', 'SPD_EBABD: Error: ' //
     :      'Incompatible number of supported dimensions.', STATUS )
         GO TO 500
      ENDIF

*  Check that spectroscopic axis is not degenerate, i.e. that width is
*  defined.
      DN(1) = DIM1
      DN(2) = DIM2
      DN(3) = DIM3
      DN(4) = DIM4
      DN(5) = DIM5
      DN(6) = DIM6
      DN(7) = DIM7
      IF ( DN(SPAXIS) .LE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPABJ_UNDEF', 'SPD_EBABD: Error: ' //
     :      'Spectroscopic axis is of length 1 or less.', STATUS )
         GO TO 500
      END IF

*  Set pixel offset to neighbour along spectroscopic axis.
      DO 8 I = 1, LMXDIM
         DN(I) = 0
 8    CONTINUE
      DN(SPAXIS) = 1

*  Do-ladder with seven steps.
*  Ni is the output pixel index along axis i.
*  Ki is the input pixel index of the pixel left along the spectroscopic
*  axis. Ki = Ni for all non-spectroscopic axes. Ki = Ni also if Ni is
*  at the left edge of the spectroscopic axis.
*  Li is the input pixel index of the pixel right along the spectrosc.
*  axis. Li = Ni for all non-spectroscopic axes. Li = Ni also if Ni is
*  at the right edge of the spectroscopic axis.
      DO 7 N7 = 1, DIM7
         K7 = MAX( N7 - DN(7), 1 )
         L7 = MIN( N7 + DN(7), DIM7 )
         DO 6 N6 = 1, DIM6
            K6 = MAX( N6 - DN(6), 1 )
            L6 = MIN( N6 + DN(6), DIM6 )
            DO 5 N5 = 1, DIM5
               K5 = MAX( N5 - DN(5), 1 )
               L5 = MIN( N5 + DN(5), DIM5 )
               DO 4 N4 = 1, DIM4
                  K4 = MAX( N4 - DN(4), 1 )
                  L4 = MIN( N4 + DN(4), DIM4 )
                  DO 3 N3 = 1, DIM3
                     K3 = MAX( N3 - DN(3), 1 )
                     L3 = MIN( N3 + DN(3), DIM3 )
                     DO 2 N2 = 1, DIM2
                        K2 = MAX( N2 - DN(2), 1 )
                        L2 = MIN( N2 + DN(2), DIM2 )
                        DO 1 N1 = 1, DIM1
                           K1 = MAX( N1 - DN(1), 1 )
                           L1 = MIN( N1 + DN(1), DIM1 )

*                       DELTA is the pixel distance between the right
*                       and left pixels on which the width is based.
*                       Usually this is 2, but on the edges of the
*                       spectroscopic axis it is 1. We can take the sum
*                       of differences along all dimensions, because the
*                       difference is 0 along all but one axis.
                           DELTA = L1 + L2 + L3 + L4 + L5 + L6 + L7
     :                           - K1 - K2 - K3 - K4 - K5 - K6 - K7

*                       The width of pixel Ni is in general half the
*                       distance between its two neighbours, in which
*                       case DELTA = 2. If Ni is the first or last pixel
*                       along the spectroscopic axis, then the width is
*                       the distance between it and its existing
*                       neighbour, in which case DELTA = 1. Note that
*                       DELTA is declared DOUBLE PRECISION
*                       rather than INTEGER.
                           SPWIDS(N1,N2,N3,N4,N5,N6,N7)
     :                        = ABS( SPVALS(L1,L2,L3,L4,L5,L6,L7)
     :                             - SPVALS(K1,K2,K3,K4,K5,K6,K7) )
     :                        / DELTA
 1                      CONTINUE
 2                   CONTINUE
 3                CONTINUE
 4             CONTINUE
 5          CONTINUE
 6       CONTINUE
 7    CONTINUE

*  Return.
 500  CONTINUE
      END
