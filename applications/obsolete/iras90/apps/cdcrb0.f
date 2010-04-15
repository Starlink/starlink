      SUBROUTINE CDCRB0( BSMP, ESMP, BDET, EDET, INDAT, SCALE, REVS,
     :                   INTER, SCNLEN, NCROS, CRSDTX, CRSSMP, CRSFLG,
     :                   OUTDAT, STATUS )
*+
*  Name:
*     CDCRB0

*  Purpose:
*     Extract trace sections from input data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRB0( BSMP, ESMP, BDET, EDET, INDAT, SCALE, REVS,
*                  INTER, SCNLEN, NCROS, CRSDTX, CRSSMP, CRSFLG,
*                  OUTDAT, STATUS )

*  Description:
*     This subroutine is used by CDDAT to extract the specified crossing
*     trace sections (data or variance ) from their CRDD NDF data or
*     variance array to an output data array. If the reversing flag is
*     seting, the traces will be reversed while put into output data
*     array. The traces in the output array are alignned with the first
*     trace.

*  Arguments:
*     BSMP, ESMP = INTEGER (Given)
*        Begin and end sample indices of the input array.
*     BDET, EDET = INTEGER (Given)
*        Begin and end detector indices of the input array.
*     INDAT( BSMP: ESMP, BDET: EDET ) = REAL (Given)
*        Input data array some of whose specified trace sections are
*        to be extracted.
*     SCALE( BDET: EDET ) = REAL
*        Scale used to convert data to the specified units when putting
*        data into output array.
*     REVS = LOGICAL (Given)
*        Reversing flag. If true, the traces extracted from the input
*        data array should be reversed while put into output array.
*     INTER = CHARACTER*( * ) (Given)
*        Specifies the interpolation method used while alignning
*        crossing traces with the first one. It can be either 'LINEAR'
*        or 'NEAREST'.
*     SCNLEN = INTEGER (Given)
*        The length of a crossing trace section to be extracted.
*     NCROS = INTEGER (Given)
*        Number of crossings.
*     CRSDTX( NCROS ) = INTEGER (Given)
*        Indices of each trace section in their original NDF data or
*        variance array.
*     CRSSMP( NCROS ) = REAL (Given)
*        Crossing position in fractional sample number of each crossing.
*        This position of each extracted trace section should be put at
*        the middle of the output trace.
*     CRSFLG( NCROS ) = INTEGER (Given and Returned)
*        On entry, flagging out, with value 3,  those crossing traces
*        which belong to the input data/variance array and hence will
*        be extracted and put into the corresponding row of the output
*        array. On exit, the flag of those extracted crossing are set
*        to 2.
*     OUTDAT( SCNLEN, NCROS ) = REAL (Returned)
*        Output data array contains the extracted trace sections in
*        their corresponding columns.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER BSMP, ESMP, BDET, EDET
      REAL INDAT( BSMP: ESMP, BDET: EDET )
      REAL SCALE( BDET: EDET )
      LOGICAL REVS
      CHARACTER*( * ) INTER
      INTEGER SCNLEN, NCROS
      INTEGER CRSDTX( NCROS )
      REAL CRSSMP( NCROS )
      INTEGER CRSFLG( NCROS )

*  Arguments Returned:
      REAL OUTDAT( SCNLEN, NCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DIST1, DIST2          ! Dist. of interpolated point to its
                                 ! adjacent samples
      INTEGER DTX                ! Index of det. trace to be extracted
      REAL FRSMP                 ! Fraction sample of crossing position
      INTEGER I, J               ! Do loop indices
      INTEGER HLFLEN             ! Half of extracted trace length
      REAL OVAL                  ! Output value
      REAL POS                   ! Posit. of crossing sample of 1st
                                 ! trace referring to other traces
      INTEGER SMP                ! Crossing sample of 1st trace
      INTEGER SMP1, SMP2         ! Samples adjacent to an posit. to be
                                 ! interpolated.
      REAL VAL1, VAL2            ! Trace values at SMP1 and SMP2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the half length of the output section.
      HLFLEN = SCNLEN / 2

*  Go through all crossings.
      DO I = 1, NCROS

*  If this crossing having its flag as 3, it is to be extracted from the
*  input CRDD array.
         IF ( CRSFLG( I ) .EQ. 3 ) THEN

*  Get the detector index of this crossing trace.
            DTX = CRSDTX( I )

*  If this is the first crossing, neither reversing nor alignment is
*  required. extract it from the input array.
            IF ( I .EQ. 1 ) THEN

*  Put the crossing sample at the middle of the output section,
               SMP = INT( CRSSMP( I ) )
               OUTDAT( HLFLEN + 1, I ) = INDAT( SMP, DTX )

*  Extract HLFLEN of samples on both sides of the crossing sample.
*  If it goes outside the input trace, set the output sample as bad.
               DO J = 1, HLFLEN
                  IF ( SMP + J .GT. ESMP ) THEN
                     OUTDAT( HLFLEN + 1 + J, I ) = VAL__BADR
                  ELSE
                     OUTDAT( HLFLEN + 1 + J, I ) = INDAT( SMP + J, DTX )
                  END IF
                  IF ( SMP - J .LT. BSMP ) THEN
                     OUTDAT( HLFLEN + 1 - J, I ) = VAL__BADR
                  ELSE
                     OUTDAT( HLFLEN + 1 - J, I ) = INDAT( SMP - J, DTX )
                  END IF
               END DO

*  Otherwise the alignment and reversing have to be considered.
*  Find the position of the crossing sample of the first crossing with
*  repect to present trace after alignning the crossing positions of
*  these two traces.
            ELSE
               FRSMP = CRSSMP( 1 ) - INT( CRSSMP( 1 ) )
               IF ( .NOT.REVS ) THEN
                  POS = CRSSMP( I ) - FRSMP
               ELSE
                  POS = CRSSMP( I ) + FRSMP
               END IF

*  If 'NEAREST' neighborehood interploating is to be used, put the
*  nearest sample to this position to the middle of the output section.
               IF ( INTER( : 7 ) .EQ. 'NEAREST' ) THEN
                  SMP = NINT( POS )
                  IF ( INDAT( SMP, DTX ) .NE. VAL__BADR ) THEN
                     OUTDAT( HLFLEN + 1, I ) = INDAT( SMP, DTX )
     :                                       * SCALE( DTX )
                  ELSE
                     OUTDAT( HLFLEN + 1, I ) = VAL__BADR
                  END IF

*  Extract samples on both side of this sample.
                  DO J = 1,  HLFLEN

*  Go forward along the input trace.
                     IF ( SMP + J .GT. ESMP .OR.
     :                    INDAT( SMP + J, DTX ) .EQ. VAL__BADR ) THEN
                        OVAL = VAL__BADR
                     ELSE
                        OVAL = INDAT( SMP + J, DTX ) * SCALE( DTX )
                     END IF

*  Write to the output trace forward or backward according to revers
*  requirement.
                     IF ( .NOT.REVS ) THEN
                        OUTDAT( HLFLEN + 1 + J, I ) = OVAL
                     ELSE
                        OUTDAT( HLFLEN + 1 - J, I ) = OVAL
                     END IF

*  Go backward along the input trace.
                     IF ( SMP - J .LT. BSMP .OR.
     :                    INDAT( SMP + J, DTX ) .EQ. VAL__BADR ) THEN
                        OVAL = VAL__BADR
                     ELSE
                        OVAL = INDAT( SMP - J, DTX ) * SCALE( DTX )
                     END IF

*  Write to the output trace backward or forward according to revers
*  requirement.
                     IF ( .NOT.REVS ) THEN
                        OUTDAT( HLFLEN + 1 - J, I ) = OVAL
                     ELSE
                        OUTDAT( HLFLEN + 1 + J, I ) = OVAL
                     END IF
                  END DO

*  If 'LINEAR' interpolation is to be used, get the distances from the
*  interpolated point to is adjacent samples.
               ELSE IF ( INTER( : 6 ) .EQ. 'LINEAR' ) THEN
                  DIST1 = POS - REAL( INT( POS ) )
                  DIST2 = REAL( INT( POS ) + 1 ) - POS

*  Scale the samples to the right units.
                  IF ( INDAT( INT( POS ), DTX ) .NE. VAL__BADR ) THEN
                     VAL1 = INDAT( INT( POS ), DTX ) * SCALE( DTX )
                  ELSE
                     VAL1 = VAL__BADR
                  END IF
                  IF ( INDAT( INT( POS ) + 1, DTX ) .NE.
     :                 VAL__BADR ) THEN
                     VAL2 = INDAT( INT( POS ) + 1, DTX ) * SCALE( DTX )
                  ELSE
                     VAL2 = VAL__BADR
                  END IF

*  Find the value at POS by linear interpolating and put at the middle
*  of the output array.
                  CALL IRM_LINR( DIST1, DIST2, VAL1, VAL2, OVAL,
     :                           STATUS )
                  OUTDAT( HLFLEN + 1, I ) = OVAL

*  Extract samples from both sides of POS.
                  DO J = 1, HLFLEN

*  Go forward along the input trace.
                     SMP1 = INT( POS ) + J
                     SMP2 = SMP1 + 1
                     IF ( SMP1 .GT. ESMP .OR.
     :                    INDAT( SMP1, DTX ) .EQ. VAL__BADR ) THEN
                        VAL1 = VAL__BADR
                     ELSE
                        VAL1 = INDAT( SMP1, DTX ) * SCALE( DTX )
                     END IF
                     IF ( SMP2 .GT. ESMP .OR.
     :                    INDAT( SMP1, DTX ) .EQ. VAL__BADR ) THEN
                        VAL2 = VAL__BADR
                     ELSE
                        VAL2 = INDAT( SMP2, DTX ) * SCALE( DTX )
                     END IF

*  Find the value by interpolation.
                     CALL IRM_LINR( DIST1, DIST2, VAL1, VAL2, OVAL,
     :                              STATUS )

*  Write to the output trace forward or backward according to the
*  reversing requirement.
                     IF ( .NOT.REVS ) THEN
                        OUTDAT( HLFLEN + 1 + J, I ) = OVAL
                     ELSE
                        OUTDAT( HLFLEN + 1 - J, I ) = OVAL
                     END IF

*  Go backward along the input trace.
                     SMP1 = INT( POS ) - J
                     SMP2 = SMP1 + 1
                     IF ( SMP1 .LT. BSMP .OR.
     :                    INDAT( SMP1, DTX ) .EQ. VAL__BADR ) THEN
                        VAL1 = VAL__BADR
                     ELSE
                        VAL1 = INDAT( SMP1, DTX ) * SCALE( DTX )
                     END IF
                     IF ( SMP2 .LT. BSMP .OR.
     :                    INDAT( SMP2, DTX ) .EQ. VAL__BADR ) THEN
                        VAL2 = VAL__BADR
                     ELSE
                        VAL2 = INDAT( SMP2, DTX ) * SCALE( DTX )
                     END IF

*  Find the value by interpolation.
                     CALL IRM_LINR( DIST1, DIST2, VAL1, VAL2, OVAL,
     :                              STATUS )

*  Write to the output trace backward or forward according to the
*  reversing requirement.
                     IF ( .NOT.REVS ) THEN
                        OUTDAT( HLFLEN + 1 - J, I ) = OVAL
                     ELSE
                        OUTDAT( HLFLEN + 1 + J, I ) = OVAL
                     END IF
                  END DO
               END IF
            END IF

*  Set flag show this crossing has been extracted.
            CRSFLG( I ) = 2
         END IF
      END DO

      END
