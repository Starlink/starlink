      SUBROUTINE SPD_WZQAR( VTHER1, VTHER2, NELM1, NELM2,
     :   DIM1, DIM2, COEFF0, COEFF1, TOL,
     :   INDAT, INVAR, OUTDAT, OUTVAR, STATUS )
*+
*  Name:
*     SPD_WZQA{DR}

*  Purpose:
*     Copy from one pair of arrays to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZQA{DR}( VTHER1, VTHER2, NELM1, NELM2, DIM1, DIM2,
*        COEFF0, COEFF1, TOL, INDAT, INVAR, OUTDAT, OUTVAR, STATUS )

*  Description:
*     This routine performs the copy action for FILLCUBE. Given input
*     and output array pairs (data,variance) and transforms between
*     input and output pixels along each axis, this routine will copy
*     non-bad data and the associated variance from input to output.

*  Arguments:
*     VTHER1 = LOGICAL (Given)
*        True if INVAR is available.
*     VTHER2 = LOGICAL (Given)
*        True if OUTVAR is available.
*     NELM1 = INTEGER (Given)
*        Total size (declared 1-D size) of INDAT and INVAR.
*     NELM2 = INTEGER (Given)
*        Total size (declared 1-D size) of OUTDAT and OUTVAR.
*     DIM1( NDF__MXDIM ) = INTEGER (Given)
*        Actual dimensions of the INDAT and INVAR arrays. This routine
*        assumes that NDF__MXDIM equals 7. All dimensions must be given,
*        unused dimensions must be given as of length 1.
*     DIM2( NDF__MXDIM ) = INTEGER (Given)
*        Actual dimensions of the OUTDAT and OUTVAR arrays. This routine
*        assumes that NDF__MXDIM equals 7. All dimensions must be given,
*        unused dimensions must be given as of length 1.
*     COEFF0( NDF__MXDIM ) = REAL (Given)
*        Pixel transform coefficient. Along each axis input pixels
*        transfrom into output pixels according to
*           OUTPIX(I) - 1 = COEFF0(I) + COEFF1(I) * (INPIX(I) - 1)
*        This routine assumes that NDF__MXDIM equals 7. All transforms
*        must be given, unused transforms must be given as offset 0. and
*        slope 1. Note that this array is always of type REAL,
*        regardless of the types of IN/OUTDAT/VAR.
*     COEFF1( NDF__MXDIM ) = REAL (Given)
*        Pixel transform coefficient. Along each axis input pixels
*        transfrom into output pixels according to
*           OUTPIX(I) - 1 = COEFF0(I) + COEFF1(I) * (INPIX(I) - 1)
*        This routine assumes that NDF__MXDIM equals 7. All transforms
*        must be given, unused transforms must be given as offset 0. and
*        slope 1. Note that this array is always of type REAL,
*        regardless of the types of IN/OUTDAT/VAR.
*     TOL = REAL (Given)
*        The pixel match tolerance. The fraction of pixel distance by
*        which the output pixel centre may be missed by the input pixel
*        centre along any axis.
*     INDAT( NELM1 ) = REAL (Given)
*        The input data array.
*     INVAR( NELM1 ) = REAL (Given)
*        The input variance array.
*     OUTDAT( NELM2 ) = REAL (Given and Returned)
*        The output data array.
*     OUTVAR( NELM2 ) = REAL (Given and Returned)
*        The output variance array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine assumes that the formal dimensionality is 7, i.e.
*     that NDF__MXDIM equals 7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 May 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADBR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL VTHER1
      LOGICAL VTHER2
      INTEGER NELM1
      INTEGER NELM2
      INTEGER DIM1( NDF__MXDIM )
      INTEGER DIM2( NDF__MXDIM )
      REAL COEFF0( NDF__MXDIM )
      REAL COEFF1( NDF__MXDIM )
      REAL TOL
      REAL INDAT( NELM1 )
      REAL INVAR( NELM1 )

*  Arguments Given and Returned:
      REAL OUTDAT( NELM2 )
      REAL OUTVAR( NELM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop index
      INTEGER I1, I2, I3, I4, I5, I6, I7 ! IN pixel
      REAL J1, J2, J3, J4, J5, J6, J7 ! OUT pixel

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through input data.
*  There are four IF cases, one for each combination of VTHER1/VTHER2.
*  I1...I7 count input along each axis, I counts through the formally
*  one-dimensional input array.
*  J1...J7 are the corresponding output pixel indices, J counts through
*  the formally one-dimensional output array.
*  ------------------------------------------------------------------

*  If both variances exist.
      IF ( VTHER1 .AND. VTHER2 ) THEN
         DO 1007 I7 = 1, DIM1(7)
          J7 = 1. + COEFF0(7) + COEFF1(7) * ( I7 - 1. )
          IF ( J7 .GT. 0.5 .AND. J7 .LT. DIM2(7) + 0.5 .AND.
     :         ABS( J7 - INT( J7 + 0.5 ) ) .LE. TOL ) THEN
           J7 = INT( J7 + 0.5 )
           DO 1006 I6 = 1, DIM1(6)
            J6 = 1. + COEFF0(6) + COEFF1(6) * ( I6 - 1. )
            IF ( J6 .GT. 0.5 .AND. J6 .LT. DIM2(6) + 0.5 .AND.
     :           ABS( J6 - INT( J6 + 0.5 ) ) .LE. TOL ) THEN
             J6 = INT( J6 + 0.5 )
             DO 1005 I5 = 1, DIM1(5)
              J5 = 1. + COEFF0(5) + COEFF1(5) * ( I5 - 1. )
              IF ( J5 .GT. 0.5 .AND. J5 .LT. DIM2(5) + 0.5 .AND.
     :             ABS( J5 - INT( J5 + 0.5 ) ) .LE. TOL ) THEN
               J5 = INT( J5 + 0.5 )
               DO 1004 I4 = 1, DIM1(4)
                J4 = 1. + COEFF0(4) + COEFF1(4) * ( I4 - 1. )
                IF ( J4 .GT. 0.5 .AND. J4 .LT. DIM2(4) + 0.5 .AND.
     :               ABS( J4 - INT( J4 + 0.5 ) ) .LE. TOL ) THEN
                 J4 = INT( J4 + 0.5 )
                 DO 1003 I3 = 1, DIM1(3)
                  J3 = 1. + COEFF0(3) + COEFF1(3) * ( I3 - 1. )
                  IF ( J3 .GT. 0.5 .AND. J3 .LT. DIM2(3) + 0.5 .AND.
     :                 ABS( J3 - INT( J3 + 0.5 ) ) .LE. TOL ) THEN
                   J3 = INT( J3 + 0.5 )
                   DO 1002 I2 = 1, DIM1(2)
                    J2 = 1. + COEFF0(2) + COEFF1(2) * ( I2 - 1. )
                    IF ( J2 .GT. 0.5 .AND. J2 .LT. DIM2(2) + 0.5 .AND.
     :                   ABS( J2 - INT( J2 + 0.5 ) ) .LE. TOL ) THEN
                     J2 = INT( J2 + 0.5 )
                     DO 1001 I1 = 1, DIM1(1)
                      J1 = 1. + COEFF0(1) + COEFF1(1) * ( I1 - 1. )
                      IF ( J1 .GT. 0.5 .AND. J1 .LT. DIM2(1) + 0.5 .AND.
     :                     ABS( J1 - INT( J1 + 0.5 ) ) .LE. TOL ) THEN
                         J1 = INT( J1 + 0.5 )

*                     Work out I and J.
                         I = I7 - 1
                         I = I * DIM1(6) + I6 - 1
                         I = I * DIM1(5) + I5 - 1
                         I = I * DIM1(4) + I4 - 1
                         I = I * DIM1(3) + I3 - 1
                         I = I * DIM1(2) + I2 - 1
                         I = I * DIM1(1) + I1
                         J = J7 - 1
                         J = J * DIM2(6) + J6 - 1
                         J = J * DIM2(5) + J5 - 1
                         J = J * DIM2(4) + J4 - 1
                         J = J * DIM2(3) + J3 - 1
                         J = J * DIM2(2) + J2 - 1
                         J = J * DIM2(1) + J1

*                     If input data not bad.
                         IF ( INDAT(I) .NE. VAL__BADR ) THEN
                            OUTDAT(J) = INDAT(I)
                            OUTVAR(J) = INVAR(I)
                         END IF
                      END IF
 1001                CONTINUE
                    END IF
 1002              CONTINUE
                  END IF
 1003            CONTINUE
                END IF
 1004          CONTINUE
              END IF
 1005        CONTINUE
            END IF
 1006      CONTINUE
          END IF
 1007    CONTINUE

*  Else if OUT variance exists (but IN variance doesn't).
      ELSE IF ( VTHER2 ) THEN
         DO 1017 I7 = 1, DIM1(7)
          J7 = 1. + COEFF0(7) + COEFF1(7) * ( I7 - 1. )
          IF ( J7 .GT. 0.5 .AND. J7 .LT. DIM2(7) + 0.5 .AND.
     :         ABS( J7 - INT( J7 + 0.5 ) ) .LE. TOL ) THEN
           J7 = INT( J7 + 0.5 )
           DO 1016 I6 = 1, DIM1(6)
            J6 = 1. + COEFF0(6) + COEFF1(6) * ( I6 - 1. )
            IF ( J6 .GT. 0.5 .AND. J6 .LT. DIM2(6) + 0.5 .AND.
     :           ABS( J6 - INT( J6 + 0.5 ) ) .LE. TOL ) THEN
             J6 = INT( J6 + 0.5 )
             DO 1015 I5 = 1, DIM1(5)
              J5 = 1. + COEFF0(5) + COEFF1(5) * ( I5 - 1. )
              IF ( J5 .GT. 0.5 .AND. J5 .LT. DIM2(5) + 0.5 .AND.
     :             ABS( J5 - INT( J5 + 0.5 ) ) .LE. TOL ) THEN
               J5 = INT( J5 + 0.5 )
               DO 1014 I4 = 1, DIM1(4)
                J4 = 1. + COEFF0(4) + COEFF1(4) * ( I4 - 1. )
                IF ( J4 .GT. 0.5 .AND. J4 .LT. DIM2(4) + 0.5 .AND.
     :               ABS( J4 - INT( J4 + 0.5 ) ) .LE. TOL ) THEN
                 J4 = INT( J4 + 0.5 )
                 DO 1013 I3 = 1, DIM1(3)
                  J3 = 1. + COEFF0(3) + COEFF1(3) * ( I3 - 1. )
                  IF ( J3 .GT. 0.5 .AND. J3 .LT. DIM2(3) + 0.5 .AND.
     :                 ABS( J3 - INT( J3 + 0.5 ) ) .LE. TOL ) THEN
                   J3 = INT( J3 + 0.5 )
                   DO 1012 I2 = 1, DIM1(2)
                    J2 = 1. + COEFF0(2) + COEFF1(2) * ( I2 - 1. )
                    IF ( J2 .GT. 0.5 .AND. J2 .LT. DIM2(2) + 0.5 .AND.
     :                   ABS( J2 - INT( J2 + 0.5 ) ) .LE. TOL ) THEN
                     J2 = INT( J2 + 0.5 )
                     DO 1011 I1 = 1, DIM1(1)
                      J1 = 1. + COEFF0(1) + COEFF1(1) * ( I1 - 1. )
                      IF ( J1 .GT. 0.5 .AND. J1 .LT. DIM2(1) + 0.5 .AND.
     :                     ABS( J1 - INT( J1 + 0.5 ) ) .LE. TOL ) THEN
                         J1 = INT( J1 + 0.5 )

*                     Work out I and J.
                         I = I7 - 1
                         I = I * DIM1(6) + I6 - 1
                         I = I * DIM1(5) + I5 - 1
                         I = I * DIM1(4) + I4 - 1
                         I = I * DIM1(3) + I3 - 1
                         I = I * DIM1(2) + I2 - 1
                         I = I * DIM1(1) + I1
                         J = J7 - 1
                         J = J * DIM2(6) + J6 - 1
                         J = J * DIM2(5) + J5 - 1
                         J = J * DIM2(4) + J4 - 1
                         J = J * DIM2(3) + J3 - 1
                         J = J * DIM2(2) + J2 - 1
                         J = J * DIM2(1) + J1

*                     If input data not bad.
                         IF ( INDAT(I) .NE. VAL__BADR ) THEN
                            OUTDAT(J) = INDAT(I)
                            OUTVAR(J) = VAL__BADR
                         END IF
                      END IF
 1011                CONTINUE
                    END IF
 1012              CONTINUE
                  END IF
 1013            CONTINUE
                END IF
 1014          CONTINUE
              END IF
 1015        CONTINUE
            END IF
 1016      CONTINUE
          END IF
 1017    CONTINUE

*  Else (OUT variance does not exist, IN variance is ignored).
      ELSE
         DO 1027 I7 = 1, DIM1(7)
          J7 = 1. + COEFF0(7) + COEFF1(7) * ( I7 - 1. )
          IF ( J7 .GT. 0.5 .AND. J7 .LT. DIM2(7) + 0.5 .AND.
     :         ABS( J7 - INT( J7 + 0.5 ) ) .LE. TOL ) THEN
           J7 = INT( J7 + 0.5 )
           DO 1026 I6 = 1, DIM1(6)
            J6 = 1. + COEFF0(6) + COEFF1(6) * ( I6 - 1. )
            IF ( J6 .GT. 0.5 .AND. J6 .LT. DIM2(6) + 0.5 .AND.
     :           ABS( J6 - INT( J6 + 0.5 ) ) .LE. TOL ) THEN
             J6 = INT( J6 + 0.5 )
             DO 1025 I5 = 1, DIM1(5)
              J5 = 1. + COEFF0(5) + COEFF1(5) * ( I5 - 1. )
              IF ( J5 .GT. 0.5 .AND. J5 .LT. DIM2(5) + 0.5 .AND.
     :             ABS( J5 - INT( J5 + 0.5 ) ) .LE. TOL ) THEN
               J5 = INT( J5 + 0.5 )
               DO 1024 I4 = 1, DIM1(4)
                J4 = 1. + COEFF0(4) + COEFF1(4) * ( I4 - 1. )
                IF ( J4 .GT. 0.5 .AND. J4 .LT. DIM2(4) + 0.5 .AND.
     :               ABS( J4 - INT( J4 + 0.5 ) ) .LE. TOL ) THEN
                 J4 = INT( J4 + 0.5 )
                 DO 1023 I3 = 1, DIM1(3)
                  J3 = 1. + COEFF0(3) + COEFF1(3) * ( I3 - 1. )
                  IF ( J3 .GT. 0.5 .AND. J3 .LT. DIM2(3) + 0.5 .AND.
     :                 ABS( J3 - INT( J3 + 0.5 ) ) .LE. TOL ) THEN
                   J3 = INT( J3 + 0.5 )
                   DO 1022 I2 = 1, DIM1(2)
                    J2 = 1. + COEFF0(2) + COEFF1(2) * ( I2 - 1. )
                    IF ( J2 .GT. 0.5 .AND. J2 .LT. DIM2(2) + 0.5 .AND.
     :                   ABS( J2 - INT( J2 + 0.5 ) ) .LE. TOL ) THEN
                     J2 = INT( J2 + 0.5 )
                     DO 1021 I1 = 1, DIM1(1)
                      J1 = 1. + COEFF0(1) + COEFF1(1) * ( I1 - 1. )
                      IF ( J1 .GT. 0.5 .AND. J1 .LT. DIM2(1) + 0.5 .AND.
     :                     ABS( J1 - INT( J1 + 0.5 ) ) .LE. TOL ) THEN
                         J1 = INT( J1 + 0.5 )

*                     Work out I and J.
                         I = I7 - 1
                         I = I * DIM1(6) + I6 - 1
                         I = I * DIM1(5) + I5 - 1
                         I = I * DIM1(4) + I4 - 1
                         I = I * DIM1(3) + I3 - 1
                         I = I * DIM1(2) + I2 - 1
                         I = I * DIM1(1) + I1
                         J = J7 - 1
                         J = J * DIM2(6) + J6 - 1
                         J = J * DIM2(5) + J5 - 1
                         J = J * DIM2(4) + J4 - 1
                         J = J * DIM2(3) + J3 - 1
                         J = J * DIM2(2) + J2 - 1
                         J = J * DIM2(1) + J1

*                     If input data not bad.
                         IF ( INDAT(I) .NE. VAL__BADR ) THEN
                            OUTDAT(J) = INDAT(I)
                         END IF
                      END IF
 1021                CONTINUE
                    END IF
 1022              CONTINUE
                  END IF
 1023            CONTINUE
                END IF
 1024          CONTINUE
              END IF
 1025        CONTINUE
            END IF
 1026      CONTINUE
          END IF
 1027    CONTINUE
      END IF

*  Return.
      END
