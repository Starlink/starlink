      SUBROUTINE KPG1_ALIGN( NX, NY, IPIN, IPREF, VIN, VREF,
     :                       IPVIN, IPVREF, FORM, IFAC, RFAC,
     :                       IOFF, ROFF, FITVAL, C, STATUS )
*+
*  Name:
*     KPG1_ALIGN

*  Purpose:
*     Align a pair of 2-dimensional arrays using a least squares fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ALIGN( NX, NY, IPIN, IPREF, VIN, VREF,
*                      IPVIN, IPVREF, FORM, IFAC, RFAC,
*                      IOFF, ROFF, FITVAL, C, STATUS )

*  Description:
*     This routine aligns  a pair of 2-dimensional arrays using a least
*     squares fit. The two arrays must be the same size and are assumed
*     to be connected by an affine transformation, and to be in near
*     alignment.

*  Arguments:
*     NX = INTEGER (Given)
*        The length of the first pixel axis in each array
*     NY = INTEGER (Given)
*        The length of the second pixel axis in each array
*     IPIN = INTEGER (Given)
*        Pointer to a _DOUBLE array with dimensions (NX,NY), holding the
*        Data values associated with the IN array.
*     IPREF = INTEGER (Given)
*        Pointer to a _DOUBLE array with dimensions (NX,NY), holding the
*        Data values associated with the REF array.
*     VIN = LOGICAL (Given)
*        If TRUE, use IPVIN as weights within the minimisation.
*     VREF = LOGICAL (Given)
*        If TRUE, use IPVREF as weights within the minimisation.
*     IPVIN = INTEGER (Given)
*        Pointer to a _DOUBLE array with dimensions (NX,NY), holding the
*        Variances associated with the IN array. Only used if VIN is .TRUE.
*     IPVREF = INTEGER (Given)
*        Pointer to a _DOUBLE array with dimensions (NX,NY), holding the
*        Variances associated with the REF array. Only used if VREF is .TRUE.
*     FORM = INTEGER (Given)
*        The form of the affine transformation to use:
*        - 0: Full unrestricted 6 coefficient fit
*        - 1: Shift, rotation and a common X/Y scale but no shear.
*        - 2: Shift and rotation but no scale or shear.
*        - 3: Shift but not rotation, scale or shear.
*     IFAC = DOUBLE PRECISION (Given)
*        A factor by which the input values should be multipled before
*        being used. Idealy, this should result in them having a standard
*        deviation of unity.
*     RFAC = DOUBLE PRECISION (Given)
*        A factor by which the reference values should be multipled before
*        being used. Idealy, this should result in them having a standard
*        deviation of unity.
*     IOFF = DOUBLE PRECISION (Given)
*        An offset to subtract from the scaled input values before being used.
*        Idealy, this should result in them having a mean of zero.
*     ROFF = DOUBLE PRECISION (Given)
*        An offset to subtract from the scaled reference values before being
*        Used. Idealy, this should result in them having a mean of zero.
*     FITVAL = LOGICAL (Given)
*        If TRUE, then the fit is extended to include the relative scale
*        factor and zero point offset between the pixle values in the two
*        arrays.
*     C = DOUBLE PRECISION( * ) (Returned)
*        The coefficients of the affine transformation:
*
*        - Xin = C1 + C2*Xref + C3*Yref
*        - Yin = C4 + C5*Xref + C6*Yref
*
*        (Xin,Yin) are grid coordinates in IN, and (Xref,Yref) are grid
*        coordinates in REF. If FITVAL is FALSE, this array should have
*        at least 6 elements. Otherwise it should have at least 8
*        elements. The last two elements give the relationship between
*        the pixel values in the two arrays:
*
*        - in = C7*ref + C8
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-FEB-2016 (DSB):
*        Original version.
*     11-MAY-2016 (DSB):
*        Added arguments IFAC, RFAC, IOFF and ROFF.
*     12-MAY-2016 (DSB):
*        Added argument FITVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'MSG_PAR'

*  Global Variables:
      INTEGER IPINC
      INTEGER IPREFC
      INTEGER IPVINC
      INTEGER IPVREFC
      INTEGER NXC
      INTEGER NYC
      INTEGER IENTRY
      LOGICAL DEBUGC
      LOGICAL VINC
      LOGICAL VREFC
      LOGICAL FITVALC
      DOUBLE PRECISION IFACC
      DOUBLE PRECISION RFACC
      DOUBLE PRECISION IOFFC
      DOUBLE PRECISION ROFFC
      COMMON /KPG1_ALIGN_COM1/ IPINC, IPREFC, IPVINC, IPVREFC, NXC, NYC,
     :                        IENTRY
      COMMON /KPG1_ALIGN_COM2/ DEBUGC, VINC, VREFC, FITVALC
      COMMON /KPG1_ALIGN_COM3/ IFACC, RFACC, IOFFC, ROFFC

*  External References:
      EXTERNAL KPG1_ALIGN2

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER IPIN
      INTEGER IPREF
      LOGICAL VIN
      LOGICAL VREF
      INTEGER IPVIN
      INTEGER IPVREF
      INTEGER FORM
      DOUBLE PRECISION F
      DOUBLE PRECISION IFAC
      DOUBLE PRECISION RFAC
      DOUBLE PRECISION IOFF
      DOUBLE PRECISION ROFF
      LOGICAL FITVAL

*  Arguments Returned:
      DOUBLE PRECISION C( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION P( 8 )
      INTEGER FILTER
      INTEGER IPFVEC
      INTEGER INFO
      INTEGER IPW1
      INTEGER IPW2
      INTEGER LWA
      INTEGER M
      INTEGER NP

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of good values in the REF array.
      CALL KPG1_NBADD( NX*NY, %VAL( CNF_PVAL( IPREF ) ), M, STATUS )
      M = NX*NY - M

*  Store things in common, for use within the PDA_LMDIF1 service routine.
      IPINC = IPIN
      VINC = VIN
      IF( VIN ) THEN
         IPVINC = IPVIN
      ELSE
         IPVINC = IPIN
      END IF

      IPREFC = IPREF
      VREFC = VREF
      IF( VREF ) THEN
         IPVREFC = IPVREF
      ELSE
         IPVREFC = IPREF
      END IF

      NXC = NX
      NYC = NY
      IENTRY = 0
      IFACC = IFAC
      RFACC = RFAC
      IOFFC = IOFF
      ROFFC = ROFF
      FITVALC = FITVAL

*  See if debug information is to be created. If so, warn the user about
*  the many NDFs that will be created.
      CALL MSG_IFLEV( FILTER, ' ', STATUS )

      DEBUGC = ( FILTER .EQ. MSG__VERB )
      IF( DEBUGC ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', ' The residuals after each iteration will'//
     :                 ' be dumped to NDFs with names that start with'//
     :                 ' KPG1_ALIGN_FVEC', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  How many free parameters in the fit?
      IF( FORM .EQ. 0 ) THEN
         NP = 6
      ELSE IF( FORM .EQ. 1 ) THEN
         NP = 4
      ELSE IF( FORM .EQ. 2 ) THEN
         NP = 3
      ELSE IF( FORM .EQ. 3 ) THEN
         NP = 2
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'F', FORM )
         CALL ERR_REP( ' ', 'KPG1_ALIGN: Illegal FORM value (^F) '//
     :                 'supplied - programming error.', status )
      END IF

*  Initial guess at solution - a unit transformation (in all FORMs). The
*  minimisation could be made much quicker and better by using FFT phase
*  correlation methods to produce a better first guess. See "Robust image
*  registration using log-polar transform" by George Wolberg and Siavash Zokai
*  (http://www-cs.engr.ccny.cuny.edu/~wolberg/pub/icip00.pdf).
      P( 1 ) = 0.0D0
      P( 2 ) = 0.0D0
      P( 3 ) = 0.0D0
      P( 4 ) = 1.0D0
      P( 5 ) = 0.0D0
      P( 6 ) = 1.0D0

*  If we are including the pixel values themselves in the fit, append two
*  extra parameters to the list of freeparameters - the scale and the
*  offset between pixel values in the two arrays.
      IF( FITVAL ) THEN
         P( NP + 1 ) = 0.0D0
         P( NP + 2 ) = 0.0D0
         NP = NP + 2
      END IF

*  Get workspace.
      LWA = M*NP + 5*NP + M
      CALL PSX_CALLOC( LWA, '_DOUBLE', IPW1, STATUS )
      CALL PSX_CALLOC( M, '_DOUBLE', IPFVEC, STATUS )
      CALL PSX_CALLOC( NP, '_INTEGER', IPW2, STATUS )

*  Check pointer can be used safely.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Do the minimisation.
      CALL PDA_LMDIF1( KPG1_ALIGN2, M, NP, P,
     :                 %VAL( CNF_PVAL( IPFVEC ) ), 1.0D-6, INFO,
     :                 %VAL( CNF_PVAL( IPW2 ) ),
     :                 %VAL( CNF_PVAL( IPW1 ) ), LWA )

*  Report errors.
      IF( INFO .EQ. 0 .OR. INFO .GE. 5 ) THEN
         CALL MSG_SETI( 'I', INFO )
         CALL MSG_OUT( ' ', 'Warning: the alignment could not be '//
     :                 'found accurately (PDA_LMDIF1 returned INFO=^I',
     :                 STATUS )

      ELSE IF( INFO .LT. 0 ) THEN
         STATUS = -INFO
         CALL ERR_REP( ' ', 'KPG1_ALIGN: An error occurred within '//
     :                 'the PDA_LMDIF1 service routine.', STATUS )

      END IF

*  If we are including the pixel values themselves in the fit, calculate
*  and store the returned data value scale and offset.
      IF( FITVAL ) THEN
         F = EXP( P( NP - 1 ) )
         C( 7 ) = F*RFAC/IFAC
         C( 8 ) = ( IOFF - F*ROFF + P( NP ) )/IFAC
         NP = NP - 2
      END IF

*  Convert the minimised parameters into a full 6 coefficient affine
*  transformation.
      CALL KPG1_ALIGN6( NP, P, C, STATUS )

*  Free workspace.
 999  CONTINUE

      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPFVEC, STATUS )

      END



      SUBROUTINE KPG1_ALIGN2( M, NP, P, FVEC, IFLAG )

*  Name:
*     KPG1_ALIGN2

*  Purpose:
*     Evaluates function values for minimisation routine PDA_LMDIF1

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ALIGN2( M, NP, P, FVEC, IFLAG )

*  Arguments:
*     M = INTEGER (Given)
*        The number of good pixels in the reference map.
*     NP = INTEGER (Given)
*        The number of coefficients being optimised.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The coefficients.
*     FVEC( M ) = DOUBLE PRECISION (Returned)
*        The residuals between the reference map and the
*        transformed input map.
*     IFLAG = INTEGER (Returned)
*        Status.

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'

*  Global Variables:
      INTEGER IPINC
      INTEGER IPREFC
      INTEGER IPVINC
      INTEGER IPVREFC
      INTEGER NXC
      INTEGER NYC
      INTEGER IENTRY
      LOGICAL DEBUGC
      LOGICAL VINC
      LOGICAL VREFC
      LOGICAL FITVALC
      DOUBLE PRECISION IFACC
      DOUBLE PRECISION RFACC
      DOUBLE PRECISION IOFFC
      DOUBLE PRECISION ROFFC
      COMMON /KPG1_ALIGN_COM1/ IPINC, IPREFC, IPVINC, IPVREFC, NXC, NYC,
     :                        IENTRY
      COMMON /KPG1_ALIGN_COM2/ DEBUGC, VINC, VREFC, FITVALC
      COMMON /KPG1_ALIGN_COM3/ IFACC, RFACC, IOFFC, ROFFC

*  Arguments Given:
      INTEGER M
      INTEGER NP
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      DOUBLE PRECISION FVEC( M )
      INTEGER IFLAG

*  Local Variables
      CHARACTER NAME*20
      DOUBLE PRECISION C( 6 )
      DOUBLE PRECISION F
      DOUBLE PRECISION FSUM
      DOUBLE PRECISION RFAC
      DOUBLE PRECISION ROFF
      INTEGER I
      INTEGER IAT
      INTEGER NSP
      INTEGER STATUS

*  Initialise the inherited status used in this routine.
      STATUS = SAI__OK

*  Increment the number of times this function has been called.
      IENTRY = IENTRY + 1

*  Adjust the scale and offset for the reference data if we are including
*  them in the fit.
      IF( FITVALC ) THEN
         F = EXP( P( NP - 1 ) )
         RFAC = RFACC*F
         ROFF = ROFFC*F - P( NP )
         NSP = NP - 2
      ELSE
         RFAC = RFACC
         ROFF = ROFFC
         NSP = NP
      END IF

*  Get the co-efficients of the full unrestricted affine transformation.
      CALL KPG1_ALIGN6( NSP, P, C, STATUS )

*  Call a lower-level routine to do the work, passing the work arrays
*  using %VAL so that their contents can be accessed.
      CALL KPG1_ALIGN3( NXC, NYC, %VAL( CNF_PVAL( IPINC ) ),
     :                  %VAL( CNF_PVAL( IPREFC ) ), C,
     :                  VINC, %VAL( CNF_PVAL( IPVINC ) ),
     :                  VREFC, %VAL( CNF_PVAL( IPVREFC ) ),
     :                  IFACC, RFAC, IOFFC, ROFF,
     :                  FVEC, FSUM, STATUS )

*  If required, dump the residuals to an NDF for debugging purposes, and
*  display the current coefficients.
      IF( DEBUGC ) THEN
         NAME = 'KPG1_ALIGN_FVEC'
         IAT = 15
         CALL CHR_PUTI( IENTRY, NAME, IAT )
         CALL KPG1_ALIGN4( NXC, NYC, %VAL( CNF_PVAL( IPREFC ) ), FVEC,
     :                     NAME, STATUS )

         CALL MSG_SETR( 'C', REAL( C( 1 ) ) )
         CALL MSG_SETI( 'N', IENTRY )
         DO I = 2, 6
            CALL MSG_SETC( 'C', ',' )
            CALL MSG_SETR( 'C', REAL( C( I ) ) )
         END DO

         IF( FITVALC ) THEN
            CALL MSG_SETC( 'C', ',' )
            CALL MSG_SETR( 'C', REAL( P( 7 ) ) )
            CALL MSG_SETC( 'C', ',' )
            CALL MSG_SETR( 'C', REAL( P( 8 ) ) )
         END IF

         CALL MSG_SETD( 'S', FSUM )
         CALL MSG_OUT( ' ', 'Entry ^N: Cost=^S Transform=(^C)', STATUS )

      END IF

*  If an error has occurred, tell PDA_LMDIF1 to terminate.
      IF( STATUS .NE. SAI__OK ) IFLAG = -STATUS

      END


      SUBROUTINE KPG1_ALIGN3( NX, NY, IN, REF, C,
     :                        USEVIN, VIN, USEVREF, VREF,
     :                        IFAC, RFAC, IOFF, ROFF, FVEC, FSUM,
     :                        STATUS )

*  Name:
*     KPG1_ALIGN3

*  Purpose:
*     Finds the squared residuals between two images, after
*     transforming one using an affine transformation.

*  Invocation:
*     CALL KPG1_ALIGN3(  NX, NY, IN, REF, C, USEVIN, VIN,
*                        USEVREF, VREF, IFAC, RFAC, IOFF,
*                        ROFF, FVEC, FSUM, STATUS )

*  Description:
*     The IN map is resampled using affine transformation with
*     coefficients given in C. The weighted squared residuals between
*     the reamaple IN map and the REF map is found and returned in FVEC.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of pixels in each row.
*     NY = INTEGER (Given)
*        The number of pixels in each column.
*     IN( NX, NY ) = DOUBLE PRECISION (Given)
*        The IN array.
*     REF( NX, NY ) = DOUBLE PRECISION (Given)
*        The REF array.
*     DATA( M ) = DOUBLE PRECISION (Given)
*        The data values.
*     C( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients of the transformation.
*     USEVIN = LOGICAL (Given)
*        Use the VIN array to weight the IN values?
*     VIN( NX, NY ) = DOUBLE PRECISION (Given)
*        The variances associated with the IN array. Only used if VIN is
*        .TRUE.
*     USEVREF = LOGICAL (Given)
*        Use the VREF array to weight the REF values?
*     VREF( NX, NY ) = DOUBLE PRECISION (Given)
*        The variances associated with the REF array. Only used if VREF is
*        .TRUE.
*     IFAC = DOUBLE PRECISION (Given)
*        A factor by which the input values should be multipled before
*        being used. Idealy, this should result in them having a standard
*        deviation of unity.
*     RFAC = DOUBLE PRECISION (Given)
*        A factor by which the reference values should be multipled before
*        being used. Idealy, this should result in them having a standard
*        deviation of unity.
*     IOFF = DOUBLE PRECISION (Given)
*        An offset to subtract from the scaled input values before being used.
*        Idealy, this should result in them having a mean of zero.
*     ROFF = DOUBLE PRECISION (Given)
*        An offset to subtract from the scaled reference values before being
*        Used. Idealy, this should result in them having a mean of zero.
*     FVEC( * ) = DOUBLE PRECISION (Returned)
*        The residuals at all pixels.
*     FSUM = DOUBLE PRECISION (Returned)
*        The square root of the sum of the squared residuals.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION IN( NX, NY )
      DOUBLE PRECISION REF( NX, NY )
      DOUBLE PRECISION C( 6 )
      LOGICAL USEVIN
      DOUBLE PRECISION VIN( NX, NY )
      LOGICAL USEVREF
      DOUBLE PRECISION VREF( NX, NY )
      DOUBLE PRECISION IFAC
      DOUBLE PRECISION RFAC
      DOUBLE PRECISION IOFF
      DOUBLE PRECISION ROFF

*  Arguments Returned:
      DOUBLE PRECISION FVEC( * )
      DOUBLE PRECISION FSUM

*  Arguments Given and Returned:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION DREF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      DOUBLE PRECISION EX
      DOUBLE PRECISION EY
      DOUBLE PRECISION INTERP
      DOUBLE PRECISION KX
      DOUBLE PRECISION KY
      DOUBLE PRECISION NORM
      DOUBLE PRECISION TOTVAR
      DOUBLE PRECISION V1
      DOUBLE PRECISION V2
      DOUBLE PRECISION V3
      DOUBLE PRECISION V4
      DOUBLE PRECISION VINTERP
      DOUBLE PRECISION WGT
      DOUBLE PRECISION WSUM
      DOUBLE PRECISION XIN
      DOUBLE PRECISION YIN
      INTEGER IFVEC
      INTEGER IREF
      INTEGER IX
      INTEGER IY
      INTEGER JREF
      INTEGER JX
      INTEGER JY
      INTEGER NFVEC

*.

*  Initialise
      FSUM = 0.0D0

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

* Initialise sum of weights.
      WSUM = 0.0D0

* Loop round each pixel centre in REF.
      IFVEC = 0
      DO JREF = 1, NY
         KX = C( 1 ) + C( 3 )*DBLE( JREF )
         KY = C( 4 ) + C( 6 )*DBLE( JREF )

         DO IREF = 1, NX
            DREF = REF( IREF, JREF )
            IF( DREF .NE. VAL__BADD ) THEN
               DREF = DREF * RFAC - ROFF

*  Get the grid coords of the corresponding point in IN.
               XIN = KX + C( 2 )*DBLE( IREF )
               YIN = KY + C( 5 )*DBLE( IREF )

*  Find the grid indices of the pixel which is centred to the lower left of
*  the required interpolation point.
               IX = INT( XIN )
               IY = INT( YIN )

*  Find the grid indices of the pixel which is centred to the upper right of
*  the required interpolation point.
               JX = IX + 1
               JY = IY + 1

*  Check that all four neighbouring pixels are within the image area.
               IF( IX .GE. 1 .AND. JX .LE. NX .AND.
     :             IY .GE. 1 .AND. JY .LE. NY ) THEN

*  Store the four pixel values.
                  V1 = IN( IX, IY )
                  V2 = IN( IX, JY )
                  V3 = IN( JX, IY )
                  V4 = IN( JX, JY )

*  Check that all four pixels have valid values.
                  IF( V1 .NE. VAL__BADD .AND. V2 .NE. VAL__BADD .AND.
     :                V3 .NE. VAL__BADD .AND. V4 .NE. VAL__BADD ) THEN

*  Find the fractions of the lower-left pixel X and Y dimensions which
*  are included in the area covered by a pixel centred on the required
*  interpolation point.
                     DX = IX + 1.0D0 - XIN
                     DY = IY + 1.0D0 - YIN

*  Find the fractions of the upper right pixel X and Y dimensions which
*  are included in the area covered by a pixel centred on the required
*  interpolation point.
                     EX = 1.0D0 - DX
                     EY = 1.0D0 - DY

*  Evaluate the interpolated values by weighting the four surrounding
*  pixel values by the are of overlap which the pixel has with a pixel
*  centred on the required interpolation point.
                     INTERP = V1*DX*DY + V2*DX*EY + V3*EX*DY + V4*EX*EY

*  Scale it.
                     INTERP = INTERP*IFAC - IOFF

*  If we are using the IN variances as weights, find the associated
*  variance.
                     IF( USEVIN ) THEN
                        V1 = VIN( IX, IY )
                        V2 = VIN( IX, JY )
                        V3 = VIN( JX, IY )
                        V4 = VIN( JX, JY )

                        IF( V1 .NE. VAL__BADD .AND. V2 .NE. VAL__BADD
     :                      .AND. V3 .NE. VAL__BADD .AND.
     :                      V4 .NE. VAL__BADD ) THEN
                           VINTERP = V1*(DX*DY)**2 + V2*(DX*EY)**2 +
     :                               V3*(EX*DY)**2 + V4*(EX*EY)**2
                           VINTERP = VINTERP*IFAC*IFAC

                        ELSE
                           INTERP = VAL__BADD
                        END IF
                     END IF

                  ELSE
                     INTERP = VAL__BADD
                  END IF
               ELSE
                  INTERP = VAL__BADD
               END IF

*  Calculate the weight.
               IF( USEVIN .OR. USEVREF ) THEN
                  IF( USEVREF ) THEN
                     TOTVAR = VREF( IREF, JREF )
                  ELSE
                     TOTVAR = 0.0D0
                  END IF

                  IF( USEVIN .AND. TOTVAR .NE. VAL__BADD ) THEN
                     TOTVAR = TOTVAR*RFAC*RFAC + VINTERP
                  END IF

                  IF( TOTVAR .NE. VAL__BADD .AND.
     :                TOTVAR .GT. 0.0D0 ) THEN
                     WGT = 1.0D0/TOTVAR
                  ELSE
                     WGT = 0.0D0
                  END IF

               ELSE
                  WGT = 1.0D0
               END IF

*  Find the weighted residual, store it in the next element of FVEC, and update
*  the sum of the weights. If no interpolated IN value could be found,
*  assume a weight of zero.
               IFVEC = IFVEC + 1
               IF( INTERP .NE. VAL__BADD .AND. WGT .GT. 0.0D0 ) THEN
                  FVEC( IFVEC ) = SQRT( WGT ) * ( INTERP - DREF )
                  WSUM = WSUM + WGT
               ELSE
                  FVEC( IFVEC ) = 0.0D0
               END IF

            END IF
         END DO
      END DO

*  Normalise the residuals by dividing them all by the square root of
*  the sum of the weights.
      IF( WSUM .GT. 0.0 ) THEN
         NORM = 1.0D0/SQRT( WSUM )
         NFVEC = IFVEC
         DO IFVEC = 1, NFVEC
            FVEC( IFVEC ) = FVEC( IFVEC ) * NORM
            FSUM = FSUM + FVEC( IFVEC )**2
         END DO
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_ALIGN: All weights are zero.',
     :                 STATUS )
      END IF

*  Return the square root of the mean of the squared residuals.
      FSUM = SQRT( FSUM/NFVEC )

      END





      SUBROUTINE KPG1_ALIGN4( NX, NY, REF, FVEC, NAME, STATUS )

*  Name:
*     KPG1_ALIGN4

*  Purpose:
*     Stores a 2-D array in a new named NDF.

*  Invocation:
*     CALL KPG1_ALIGN4( NX, NY, REF, FVEC, NAME, STATUS )

*  Description:
*     The FVEC map is stored in a new NDF with a given name. The lower
*     pixel bounds is (1,1)

*  Arguments:
*     NX = INTEGER (Given)
*        The number of pixels in each row.
*     NY = INTEGER (Given)
*        The number of pixels in each column.
*     REF( NX, NY ) = DOUBLE PRECISION (Given)
*        The REF array - this defines which map pixels correspond to
*        which elements in FVEC.
*     FVEC( * ) = DOUBLE PRECISION (Given)
*        The residuals array. Bad values in REF have no corresponding
*        values in FVEC.
*     NAME = CHARACTER*(*) (Given)
*        The name for the new NDF.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION REF( NX, NY )
      DOUBLE PRECISION FVEC( * )
      CHARACTER NAME*(*)

*  Arguments Given and Returned:
      INTEGER STATUS

*  Local Variables:
      INTEGER EL
      INTEGER INDF
      INTEGER IPNT
      INTEGER LBND(2)
      INTEGER PLACE
      INTEGER UBND(2)

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) return

*  Create a placeholder for an NDF with the given name.
      CALL NDF_PLACE( DAT__ROOT, NAME, PLACE, STATUS )

*  Create the NDF.
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = NX
      UBND(2) = NY
      CALL NDF_NEW( '_DOUBLE', 2, LBND, UBND, PLACE, INDF, STATUS )

*  Map its data component.
      CALL NDF_MAP( INDF, 'Data', '_DOUBLE', 'WRITE', IPNT, EL,
     :              STATUS )

*  Copy the supplied data array into the NDF
      CALL KPG1_ALIGN5( NX, NY, REF, FVEC, %VAL( CNF_PVAL( IPNT ) ),
     :                  STATUS )

*  Free the NDF.
      CALL NDF_ANNUL( INDF, STATUS )

      END



      SUBROUTINE KPG1_ALIGN5( NX, NY, REF, FVEC, OUT, STATUS )

*  Name:
*     KPG1_ALIGN5

*  Purpose:
*     Copy FVEC values to a 2D array.

*  Invocation:
*     CALL KPG1_ALIGN5( NX, NY, REF, FVEC, OUT, STATUS )

*  Description:
*     The FVEC values are copied into OUT, storing them in the correct
*     2-dimensional positions, as defined by REF.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of pixels in each row.
*     NY = INTEGER (Given)
*        The number of pixels in each column.
*     REF( NX, NY ) = DOUBLE PRECISION (Given)
*        The REF array - this defines which map pixels correspond to
*        which elements in FVEC.
*     FVEC( * ) = DOUBLE PRECISION (Given)
*        The residuals array. Bad values in REF have no corresponding
*        values in FVEC.
*     OUT( MX, NY ) = DOUBLE PRECISION (Returned)
*        The output array to recieve the copied FVEC values.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION REF( NX, NY )
      DOUBLE PRECISION FVEC( * )

*  Arguments Returned:
      DOUBLE PRECISION OUT( NX, NY )

*  Arguments Given and Returned:
      INTEGER STATUS

*  Local Variables:
      INTEGER IFVEC
      INTEGER I
      INTEGER J

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) return

*  Initialise the index of the next value to read from the 1-dimensional
*  FVEC array.
      IFVEC = 1

*  Loop round all REF pixels.
      DO J = 1, NY
         DO I = 1, NX

*  If the current REF value is bad, store a bad value at the same place
*  in the OUT array.
            IF( REF( I, J ) .EQ. VAL__BADD ) THEN
               OUT( I, J ) = VAL__BADD

*  Otherwise, store the next value read from the FVEC array, and then
*  update the index of the next value to read.
            ELSE
               OUT( I, J ) = FVEC( IFVEC )
               IFVEC = IFVEC + 1
            END IF

         END DO
      END DO

      END







      SUBROUTINE KPG1_ALIGN6( NP, P, C, STATUS )

*  Name:
*     KPG1_ALIGN5

*  Purpose:
*     Copy FVEC values to a 2D array.

*  Invocation:
*     CALL KPG1_ALIGN6( NP, P, C, STATUS )

*  Description:
*     Create a full 6 coefficient affine transformation from the supplied
*     minimisation parameters.

*  Arguments:
*     NP = INTEGER (Given)
*        The number of minimisation parameters.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The supplied minimisation parameter.
*     C( 6 ) = DOUBLE PRECISION (Return)
*        The coeffiients of the unrestricted affine transformation.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NP
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      DOUBLE PRECISION C( 6 )

*  Arguments Given and Returned:
      INTEGER STATUS

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) return

*  Full unrestricted affine transformation.
      IF( NP .EQ. 6 ) THEN
         C( 1 ) = P( 1 )
         C( 2 ) = P( 4 )
         C( 3 ) = P( 3 )
         C( 4 ) = P( 2 )
         C( 5 ) = P( 5 )
         C( 6 ) = P( 6 )

*  Shift, rotation and scale.
      ELSE IF( NP .EQ. 4 ) THEN
         C( 1 ) = P( 1 )
         C( 2 ) = P( 4 )*COS( P( 3 ) )
         C( 3 ) = -P( 3 )*SIN( P( 3 ) )
         C( 4 ) = P( 2 )
         C( 5 ) = P( 4 )*SIN( P( 3 ) )
         C( 6 ) = P( 4 )*COS( P( 3 ) )

*  Shift and rotation.
      ELSE IF( NP .EQ. 3 ) THEN
         C( 1 ) = P( 1 )
         C( 2 ) = COS( P( 3 ) )
         C( 3 ) = -SIN( P( 3 ) )
         C( 4 ) = P( 2 )
         C( 5 ) = SIN( P( 3 ) )
         C( 6 ) = COS( P( 3 ) )

*  Shift only.
      ELSE IF( NP .EQ. 2 ) THEN
         C( 1 ) = P( 1 )
         C( 2 ) = 1.0D0
         C( 3 ) = 0.0D0
         C( 4 ) = P( 2 )
         C( 5 ) = 0.0D0
         C( 6 ) = 1.0D0

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NP )
         CALL ERR_REP( ' ', 'KPG1_ALIGN: Bad number of free '//
     :                 'parameters (^N) - programming error.',
     :                 STATUS )
      END IF

      END
