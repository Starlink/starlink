      DOUBLE PRECISION FUNCTION KPS1_GAMLN( X )

*+
*  Name:
*     KPS1_GAMLN

*  Purpose:
*     Evaluates the logarithmic gamma function.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPS1_GAMLN( X )

*  Description:
*     This routine calculates the ln(gamma) function for a positive
*     argument X.  Computation is based on an algorithm outlined in
*     References 1 and 2.  The routine uses rational functions that
*     theoretically approximate LN(gamma) to at least 18 significant
*     decimal digits.  The approximation for X > 12 is from Reference
*     3, while approximations for X < 12.0 are similar to those in
*     Reference 1, but are unpublished.  The accuracy achieved depends
*     on the arithmetic system, the compiler, the intrinsic functions,
*     and proper selection of the machine-dependent constants.
*
*     The computation is believed to be free of underflow and overflow.

*  Arguments:
*     X = DOUBLE PRECISION (Given)
*        The argument of the gamma function to evaluate.

*  Result:
*     The logarithmic gamma function.  If X is not positive, or when an
*     overflow would occur the returned value is standard PRIMDAT bad
*     value.

*  Notes:
*     The limiting range is set for IEEE 754 floating point.  The
*     original code had tabulation of various machine-dependent
*     constants.

*  References:
*     1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for
*        the Natural Logarithm of the Gamma Function,' Math. Comp. 21,
*        1967, pp. 198-203.
*
*     2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/DLGAMA, May,
*        1969.
*
*     3) Hart, Et. Al., Computer Approximations, Wiley and sons, New
*        York, 1968.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WJCLS: W. J. Cody and L. Stoltz (Argonne National Laboratory)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 August 9 (MJC):
*        Original version adapted to Starlink conventinos and style from
*        the public-domain ALGAMA code by Cody & Stoltz,revision
*        1988 June 16.  Restricted to double precision and removed
*        type-conditional comments.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'         ! PRIMDAT public constants

*  Arguments Given:
      DOUBLE PRECISION X

*  Local Constants:
      DOUBLE PRECISION FOUR
      PARAMETER ( FOUR = 4.0D0 )

      DOUBLE PRECISION HALF
      PARAMETER ( HALF = 0.5D0 )

      DOUBLE PRECISION ONE
      PARAMETER ( ONE = 1.0D0 )

      DOUBLE PRECISION PNT68
      PARAMETER ( PNT68 = 0.6796875D0 )

      DOUBLE PRECISION SQRTPI
      PARAMETER ( SQRTPI = 0.9189385332046727417803297D0 )

      DOUBLE PRECISION THRHAL
      PARAMETER ( THRHAL = 1.5D0 )

      DOUBLE PRECISION TWELVE
      PARAMETER ( TWELVE = 12.0D0 )

      DOUBLE PRECISION TWO
      PARAMETER ( TWO = 2.0D0 )

      DOUBLE PRECISION ZERO
      PARAMETER ( ZERO = 0.0D0 )

*  Local Variables:
      DOUBLE PRECISION CORR      ! Correction
      INTEGER I                  ! Do-loop increment variables
      DOUBLE PRECISION RES       ! The result
      DOUBLE PRECISION XDEN      ! Denominator
      DOUBLE PRECISION XM1       ! Argument minus 1
      DOUBLE PRECISION XM2       ! Argument minus 2
      DOUBLE PRECISION XM4       ! Argument minus 4
      DOUBLE PRECISION XNUM      ! Numerator
      DOUBLE PRECISION Y         ! Local copy of argument
      DOUBLE PRECISION YSQ       ! Argument squared


*  Local Data:

*  Machine dependent parameters
*  ----------------------------
      DOUBLE PRECISION XBIG      ! largest argument for which
                                 ! LN(GAMMA(X)) is representable in the
                                 ! machine
      DATA XBIG/2.55D305/        ! IEEE 754 floating point

      DOUBLE PRECISION FRTBIG    ! XBIG**0.25
      DATA FRTBIG/2.25D76/

*  Numerator and denominator coefficients for rational minimax.
*  ============================================================

*  Approximation over (0.5,1.5)
*  ----------------------------
      DOUBLE PRECISION D1, P1( 8 ), Q1( 8 )
      DATA D1/-5.772156649015328605195174D-1/
      DATA P1/4.945235359296727046734888D0,2.018112620856775083915565D2,
     1        2.290838373831346393026739D3,1.131967205903380828685045D4,
     2        2.855724635671635335736389D4,3.848496228443793359990269D4,
     3        2.637748787624195437963534D4,7.225813979700288197698961D3/
      DATA Q1/6.748212550303777196073036D1,1.113332393857199323513008D3,
     1        7.738757056935398733233834D3,2.763987074403340708898585D4,
     2        5.499310206226157329794414D4,6.161122180066002127833352D4,
     3        3.635127591501940507276287D4,8.785536302431013170870835D3/

*  Approximation over (1.5,4.0)
*  ----------------------------
      DOUBLE PRECISION D2, P2( 8 ), Q2( 8 )
      DATA D2/4.227843350984671393993777D-1/
      DATA P2/4.974607845568932035012064D0,5.424138599891070494101986D2,
     1        1.550693864978364947665077D4,1.847932904445632425417223D5,
     2        1.088204769468828767498470D6,3.338152967987029735917223D6,
     3        5.106661678927352456275255D6,3.074109054850539556250927D6/
      DATA Q2/1.830328399370592604055942D2,7.765049321445005871323047D3,
     1        1.331903827966074194402448D5,1.136705821321969608938755D6,
     2        5.267964117437946917577538D6,1.346701454311101692290052D7,
     3        1.782736530353274213975932D7,9.533095591844353613395747D6/

*  Approximation over (4.0,12.0)
*  -----------------------------
      DOUBLE PRECISION D4, P4( 8 ), Q4( 8 )
      DATA D4/1.791759469228055000094023D0/
      DATA P4/1.474502166059939948905062D4,2.426813369486704502836312D6,
     1        1.214755574045093227939592D8,2.663432449630976949898078D9,
     2      2.940378956634553899906876D10,1.702665737765398868392998D11,
     3      4.926125793377430887588120D11,5.606251856223951465078242D11/
      DATA Q4/2.690530175870899333379843D3,6.393885654300092398984238D5,
     2        4.135599930241388052042842D7,1.120872109616147941376570D9,
     3      1.488613728678813811542398D10,1.016803586272438228077304D11,
     4      3.417476345507377132798597D11,4.463158187419713286462081D11/

*  Approximation over (12, INF)
*  ----------------------------
      DOUBLE PRECISION C( 7 )
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     1       -5.952379913043012D-04,7.93650793500350248D-04,
     2       -2.777777777777681622553D-03,
     3        8.333333333333333331554247D-02,5.7083835261D-03/

*.

      Y = X
      IF ( ( Y .GT. ZERO ) .AND. ( Y .LE. XBIG ) ) THEN
         IF ( Y .LE. VAL__EPSD ) THEN
            RES = -LOG( Y )

*  Evalutate for different ranges using different coefficients.
*  ============================================================

*  Evaluate for argument greater than 1 and up to 1.5.
         ELSE IF ( Y .LE. THRHAL ) THEN
            IF ( Y .LT. PNT68 ) THEN
               CORR = -LOG( Y )
               XM1 = Y
            ELSE
               CORR = ZERO
               XM1 = ( Y - HALF ) - HALF
            END IF

            IF ( ( Y .LE. HALF ) .OR. ( Y .GE. PNT68 ) ) THEN
                XDEN = ONE
                XNUM = ZERO
                DO I = 1, 8
                   XNUM = XNUM * XM1 + P1( I )
                   XDEN = XDEN * XM1 + Q1( I )
                END DO

                RES = CORR + ( XM1 * (D1 + XM1 * ( XNUM / XDEN ) ) )
            ELSE
                XM2 = ( Y - HALF ) - HALF
                XDEN = ONE
                XNUM = ZERO
                DO I = 1, 8
                   XNUM = XNUM * XM2 + P2( I )
                   XDEN = XDEN * XM2 + Q2( I )
                END DO
                RES = CORR + XM2 * ( D2 + XM2 * ( XNUM / XDEN ) )
            END IF

*  Evaluate for argument that is above 1.5 and up to 4.0.
         ELSE IF ( Y .LE. FOUR ) THEN
            XM2 = Y - TWO
            XDEN = ONE
            XNUM = ZERO
            DO I = 1, 8
               XNUM = XNUM * XM2 + P2( I )
               XDEN = XDEN * XM2 + Q2( I )
            END DO

            RES = XM2 * ( D2 + XM2 * ( XNUM / XDEN ) )

*  Evaluate for the argument being above 4.0 and up to 12.0.
         ELSE IF ( Y .LE. TWELVE ) THEN
            XM4 = Y - FOUR
            XDEN = -ONE
            XNUM = ZERO
            DO I = 1, 8
               XNUM = XNUM * XM4 + P4( I )
               XDEN = XDEN * XM4 + Q4( I )
            END DO

            RES = D4 + XM4 * ( XNUM / XDEN )
         ELSE

*  Evaluate for the argument being greater than 12.0.
            RES = ZERO
            IF ( Y .LE. FRTBIG ) THEN
               RES = C( 7 )
               YSQ = Y * Y
               DO I = 1, 6
                  RES = RES / YSQ + C( I )
               END DO
            END IF

            RES = RES / Y
            CORR = LOG( Y )
            RES = RES + SQRTPI - HALF * CORR
            RES = RES + Y * ( CORR - ONE )
         END IF
      ELSE

*  Return for bad arguments
         RES = VAL__BADD
      END IF

      KPS1_GAMLN = RES

      END
