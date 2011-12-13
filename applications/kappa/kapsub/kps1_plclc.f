      SUBROUTINE KPS1_PLCLC( NEL, DIN1, DIN2, DIN3, DIN4, VIN1, VIN2,
     :                       VIN3, VIN4, DEBIAS, VAR, MAKEI, MAKEQ,
     :                       MAKEU, MAKEP, MAKET, MAKEIP, MAKEIA,
     :                       MAKEIB, AI, AQ, AU, AP, AT, AIP, AIA, AIB,
     :                       AIV, AQV, AUV, APV, ATV, AIPV, AIAV, AIBV,
     :                       STATUS )
*+
*  Name:
*     KPS1_PLCLC

*  Purpose:
*     Calculates polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCLC( NEL, DIN1, DIN2, DIN3, DIN4, VIN1, VIN2, VIN3,
*                      VIN4, DEBIAS, VAR, MAKEI, MAKEQ, MAKEU, MAKEP,
*                      MAKET, MAKEIP, MAKEIA, MAKEIB, AI, AQ, AU, AP,
*                      AT, AIP, AIA, AIB, AIV, AQV, AUV, APV, ATV,
*                      AIPV, AIAV, AIBV, STATUS )

*  Description:
*     This routine calculates the polarisation parameters and (if
*     requested) variances for each pixel in the four input arrays.
*     A bad value will appear in the output data and variance arrays
*     when any of the four input data values is bad, or if the total
*     intensity in the pixel is not positive.  The output variance
*     values are also undefined when any of the four input variances is
*     bad or negative, or any computed variance is not positive, or
*     the percentage polarisation is not positive.

*  Arguments:
*     NEL = INTEGER (Given)
*        The size of the arrays.
*     DIN1( NEL ) = REAL (Given)
*        The input intensity analysed at 0 degrees.
*     DIN2( NEL ) = REAL (Given)
*        The input intensity analysed at 45 degrees.
*     DIN3( NEL ) = REAL (Given)
*        The input intensity analysed at 90 degrees.
*     DIN4( NEL ) = REAL (Given)
*        The input intensity analysed at 135 degrees.
*     VIN1( NEL ) = REAL (Given)
*        The variance on DIN1.  It is ignored if VAR is .FALSE..
*     VIN2( NEL ) = REAL (Given)
*        The variance on DIN1.  It is ignored if VAR is .FALSE..
*     VIN3( NEL ) = REAL (Given)
*        The variance on DIN1.  It is ignored if VAR is .FALSE..
*     VIN4( NEL ) = REAL (Given)
*        The variance on DIN1.  It is ignored if VAR is .FALSE..
*     DEBIAS = LOGICAL (Given)
*        It is .TRUE. if the effects of biassing caused by the
*        asymmetric distribution of percentage polarisation is to be
*        removed.  The returned variance values are unaffected by this
*        value.
*     VAR = LOGICAL (Given)
*        It is .TRUE. if output variance values are to be returned.
*     MAKEI = LOGICAL (Given)
*        It is .TRUE. if a total intensity output array is required.
*     MAKEQ = LOGICAL (Given)
*        It is .TRUE. if a Stokes Q output array is required.
*     MAKEU = LOGICAL (Given)
*        It is .TRUE. if a Stokes U output array is required.
*     MAKEP = LOGICAL (Given)
*        It is .TRUE. if a percentage polarisation output array is
*        required.
*     MAKET = LOGICAL (Given)
*        It is .TRUE. if a polarisation angle output array is required.
*     MAKEIP = LOGICAL (Given)
*        It is .TRUE. if a polarised intensity output array is required.
*     MAKEIA = LOGICAL (Given)
*        It is .TRUE. if a values of the total intensity derived from
*        from the 0- and 90-degree input images are required.
*     MAKEIB = LOGICAL (Given)
*        It is .TRUE. if a values of the total intensity derived from
*        from the 45- and 135-degree input images are required.
*     AI( NEL ) = REAL (Returned)
*        An array holding total intensity values.
*     AQ( NEL ) = REAL (Returned)
*        An array holding normalised Stokes parameter Q values.
*     AU( NEL ) = REAL (Returned)
*        An array holding normalised Stokes parameter U values.
*     AP( NEL ) = REAL (Returned)
*        An array holding percentage polarisation values.
*     AT( NEL ) = REAL (Returned)
*        An array holding polarisation angles (in degrees).
*     AIP( NEL ) = REAL (Returned)
*        An array holding polarised intensity values.
*     AIA( NEL ) = REAL (Returned)
*        An array holding total intensity values derived from the 0- and
*        90-degree input.
*     AIB( NEL ) = REAL (Returned)
*        An array holding total intensity values derived from the 45-
*        and 135-degree input.
*     AIV( NEL ) = REAL (Returned)
*        An array holding the variance of AI.
*     AQV( NEL ) = REAL (Returned)
*        An array holding the variance of AQ.
*     AUV( NEL ) = REAL (Returned)
*        An array holding the variance of AU.
*     APV( NEL ) = REAL (Returned)
*        An array holding the variance of AP.
*     ATV( NEL ) = REAL (Returned)
*        An array holding the variance of AT.
*     AIPV( NEL ) = REAL (Returned)
*        An array holding the variance of AIP.
*     AIAV( NEL ) = REAL (Returned)
*        An array holding the variance of AIA.
*     AIBV( NEL ) = REAL (Returned)
*        An array holding the variance of AIB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1993 (DSB):
*        Original version.
*     1995 April 10 (MJC):
*        More tolerance wwhen assigning bad values, such that there can
*        be a bad value but good variance, and vice versa.  Corrected
*        typo's.  Used modern-style variable declarations.  Other
*        stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER NEL
      REAL DIN1( NEL )
      REAL DIN2( NEL )
      REAL DIN3( NEL )
      REAL DIN4( NEL )
      REAL VIN1( NEL )
      REAL VIN2( NEL )
      REAL VIN3( NEL )
      REAL VIN4( NEL )
      LOGICAL DEBIAS
      LOGICAL VAR
      LOGICAL MAKEI
      LOGICAL MAKEQ
      LOGICAL MAKEU
      LOGICAL MAKEP
      LOGICAL MAKET
      LOGICAL MAKEIP
      LOGICAL MAKEIA
      LOGICAL MAKEIB

*  Arguments Returned:
      REAL AI( NEL )
      REAL AQ( NEL )
      REAL AU( NEL )
      REAL AP( NEL )
      REAL AT( NEL )
      REAL AIP( NEL )
      REAL AIA( NEL )
      REAL AIB( NEL )
      REAL AIV( NEL )
      REAL AQV( NEL )
      REAL AUV( NEL )
      REAL APV( NEL )
      REAL ATV( NEL )
      REAL AIPV( NEL )
      REAL AIAV( NEL )
      REAL AIBV( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Loop count
      REAL FACT                  ! Commonly needed factor
      REAL I                     ! Total intensity from all inputs
      REAL I0                    ! Input 0 degrees intensity
      REAL I135                  ! Input 135 degrees intensity
      REAL I45                   ! Input 45 degrees intensity
      REAL I90                   ! Input 90 degrees intensity
      REAL IA                    ! Total intensity from 0 and 90 degs
      REAL IB                    ! Total. intensity from 45 and 135 degs
      REAL IP                    ! Polarised intensity
      REAL P                     ! Percentage polarisation
      REAL P2                    ! Fractional polarisation squared
      REAL Q                     ! Normalised Q Stokes parameter
      REAL Q2                    ! Normalised Q Stokes parameter squared
      REAL RTOD                  ! Conversion factor; radians to degrees
      REAL T                     ! Polarisation angle
      REAL U                     ! Normalised U Stokes parameter
      REAL U2                    ! Normalised U Stokes parameter squared
      REAL V0                    ! Variance on 0-degree intensity
      REAL V135                  ! Variance on 135-degree intensity
      REAL V45                   ! Variance on 45-degree intensity
      REAL V90                   ! Variance on 90-degree intensity
      REAL VI                    ! Variance on tot. int. from all inputs
      REAL VIA                   ! Variance on tot. int. from 0 and 90
      REAL VIB                   ! Variance on tot. int. from 45 and 135
      REAL VIP                   ! Variance on polarised intensity
      REAL VP                    ! Variance on percentage polarisation
      REAL VQ                    ! Variance on normalised Q Stokes par.
      REAL VT                    ! Variance on Polarisation angle
      REAL VU                    ! Variance on normalised U Stokes par.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the conversion factor from radians to degrees.
      RTOD = 180.0 / ACOS( -1.0 )

*  Loop round each element of the arrays.
      DO EL = 1, NEL

*  Save the input intensity values.
         I0 = DIN1( EL )
         I45 = DIN2( EL )
         I90 = DIN3( EL )
         I135 = DIN4( EL )

*  If any of the four intensities are bad, store bad results.
         IF ( I0 .EQ. VAL__BADR .OR. I45 .EQ. VAL__BADR .OR.
     :        I90 .EQ. VAL__BADR .OR. I135 .EQ. VAL__BADR ) THEN

            IA = VAL__BADR
            IB = VAL__BADR
            IP = VAL__BADR
            I = VAL__BADR
            Q = VAL__BADR
            U = VAL__BADR
            P = VAL__BADR
            T = VAL__BADR

            IF ( VAR ) THEN
               VIA = VAL__BADR
               VIB = VAL__BADR
               VIP = VAL__BADR
               VI = VAL__BADR
               VQ = VAL__BADR
               VU = VAL__BADR
               VP = VAL__BADR
               VT = VAL__BADR
            END IF

*  Otherwise, calculate the two independant estimates of total intensity
*  from the two pairs of orthogonal polarisation states.
         ELSE
            IA = I0 + I90
            IB = I45 + I135

*  If either of these are zero, then store bad results.
            IF ( ABS( IA ) .LE. VAL__SMLR .OR.
     :           ABS( IB ) .LE. VAL__SMLR ) THEN

               IA = VAL__BADR
               IB = VAL__BADR
               IP = VAL__BADR
               I = VAL__BADR
               Q = VAL__BADR
               U = VAL__BADR
               P = VAL__BADR
               T = VAL__BADR

               IF ( VAR ) THEN
                  VIA = VAL__BADR
                  VIB = VAL__BADR
                  VIP = VAL__BADR
                  VI = VAL__BADR
                  VQ = VAL__BADR
                  VU = VAL__BADR
                  VP = VAL__BADR
                  VT = VAL__BADR
               END IF

*  Otherwise, calculate everything else.
            ELSE

*  Normalised Stokes parameter, Q.
               Q = ( I0 - I90 ) / IA
               Q2 = Q * Q

*  Normalised Stokes parameter U.
               U = ( I45 - I135 ) / IB
               U2 = U * U

*  Total intensity estimate from all four input intensities.
               I = 0.5 * ( IA + IB )

*  Percentage polarisation.
               P2 = Q2 + U2
               P = 100.0 * SQRT( MAX( 0.0, P2 ) )

*  Polarisation angle.
               T = RTOD * 0.5 * ATAN2( U, Q )

*  Polarised intensity.
               IP = 0.01 * P * I

*  Now produced variances if required.
               IF ( VAR ) THEN

*  Save the 4 input variances.
                  V0 = VIN1( EL )
                  V45 = VIN2( EL )
                  V90 = VIN3( EL )
                  V135 = VIN4( EL )

*  Treat negative variances as bad variances.
                  IF ( V0 .LT. 0.0 ) V0 = VAL__BADR
                  IF ( V45 .LT. 0.0 ) V45 = VAL__BADR
                  IF ( V90 .LT. 0.0 ) V90 = VAL__BADR
                  IF ( V135 .LT. 0.0 ) V135 = VAL__BADR

*  If any of the four variances are bad, or if the fractional
*  polarisation is not positive, store bad variances
                  IF ( V0 .EQ. VAL__BADR .OR. V45 .EQ. VAL__BADR .OR.
     :                 V90 .EQ. VAL__BADR .OR. V135 .EQ. VAL__BADR .OR.
     :                 P2 .LE. VAL__SMLR ) THEN

                     VIA = VAL__BADR
                     VIB = VAL__BADR
                     VIP = VAL__BADR
                     VI = VAL__BADR
                     VQ = VAL__BADR
                     VU = VAL__BADR
                     VP = VAL__BADR
                     VT = VAL__BADR

*  Otherwise, calculate all the variances.
                  ELSE

*  Total intensities implied by the two pairs of orthogonal polarisation
*  states.
                     VIA = V0 + V90
                     VIB = V45 + V135

*  Total intensity implied by all four polarisation states.
                     VI = 0.25 * ( VIA + VIB )

*  Normalised Stokes parameter, Q and U.
                     VQ = 4.0 * ( (I90*I90) * V0 + (I0*I0) * V90 ) /
     :                    ( IA**4 )
                     VU = 4.0 * ( (I135*I135) * V45 + (I45*I45) * V135 )
     :                    / ( IB**4 )

*  Percentage polarisation.
                     FACT = 1.0 / P2
                     VP = 10000.0 * FACT *( Q2 * VQ + U2 * VU )

*  Polarisation angle.
                     VT = 0.25 * ( FACT**2 ) * ( Q2 * VU + U2 * VQ )

*  Polarised intensity.
                     VIP = FACT * ( Q2 * VIA + U2 * VIB )

*  If any of the variances are negative store bad results.
                     IF ( VIA .LT. 0.0 .OR. VIB .LT. 0.0 .OR.
     :                    VIP .LT. 0.0 .OR. VI .LT. 0.0 .OR.
     :                    VQ .LT. 0.0. OR. VU .LT. 0.0 .OR.
     :                    VP .LT. 0.0 .OR. VT .LT. 0.0 ) THEN

                        VIA = VAL__BADR
                        VIB = VAL__BADR
                        VIP = VAL__BADR
                        VI = VAL__BADR
                        VQ = VAL__BADR
                        VU = VAL__BADR
                        VP = VAL__BADR
                        VT = VAL__BADR

*  If required, make an estimate of the percentage polarisation and
*  polarised intensity excluding the bias introduced because of the
*  distribution of P being non-symmetric.
                     ELSE
                        IF ( DEBIAS ) THEN
                           P = SQRT( MAX( 0.0, 10000.0 * P2 - VP ) )
                           IP = 0.01 * I * P
                        END IF

                     END IF

                  END IF

               END IF

            END IF

         END IF

*  Store the required values in the output arrays.
         IF ( MAKEI ) AI( EL ) = I
         IF ( MAKEQ ) AQ( EL ) = Q
         IF ( MAKEU ) AU( EL ) = U
         IF ( MAKEP ) AP( EL ) = P
         IF ( MAKET ) AT( EL ) = T
         IF ( MAKEIP ) AIP( EL ) = IP
         IF ( MAKEIA ) AIA( EL ) = IA
         IF ( MAKEIB ) AIB( EL ) = IB

         IF ( VAR ) THEN
            IF ( MAKEI ) AIV( EL ) = VI
            IF ( MAKEQ ) AQV( EL ) = VQ
            IF ( MAKEU ) AUV( EL ) = VU
            IF ( MAKEP ) APV( EL ) = VP
            IF ( MAKET ) ATV( EL ) = VT
            IF ( MAKEIP ) AIPV( EL ) = VIP
            IF ( MAKEIA ) AIAV( EL ) = VIA
            IF ( MAKEIB ) AIBV( EL ) = VIB
         END IF

      END DO

      END
