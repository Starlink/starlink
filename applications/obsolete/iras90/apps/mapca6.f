      SUBROUTINE MAPCA6( BAND, ZFWHM, YFWHM, NZSECT, NYSECT, SECWGT, C1,
     :                   STATUS )
*+
*  Name:
*     MAPCA6

*  Purpose:
*     Set up sector weights.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA6( BAND, ZFWHM, YFWHM, NZSECT, NYSECT, SECWGT, C1,
*                  STATUS )

*  Description:
*     This routine returns the coefficients C1 for the transformation
*
*     DZ = C1( 1 ) + C1( 2 )*I + C1( 3 )*J
*     DY = C1( 4 ) + C1( 5 )*I + C1( 6 )*J
*
*     where (I,J) are the cross-scan and in-scan indices of a sector
*     within the array of sector weights, and (DZ,DY) are the cross-scan
*     and in-scan offsets of the sector centre from the centre of a
*     full sized detector, in radians. The weight associated with
*     each sector is returned in array SECWGT, and is either uniform, or
*     Gaussian in both in-scan and cross-scan directions. The weights
*     are normalised so that they have a total sum of 1.0.

*  Arguments:
*     BAND = INTEGER (Given)
*        IRAS waveband (1-4) of the CRDD being mapped.
*     ZFWHM = REAL (Given)
*        The FWHM in the cross-scan direction of the Gaussian weighting
*        function, in radians. If ZFWHM is zero, then the uniform
*        weighting scheme is used.
*     YFWHM = REAL (Given)
*        The FWHM in the in-scan direction of the Gaussian weighting
*        function, in radians. If YFWHM is zero, then the uniform
*        weighting scheme is used.
*     NZSECT = INTEGER (Given)
*        The number of sectors across a full sized detector (i.e. in the
*        cross-scan direction).
*     NYSECT = INTEGER (Given)
*        The number of sectors along a full sized detector (i.e. in the
*        in-scan direction).
*     SECWGT( NZSECT, NYSECT ) = REAL (Returned)
*        The weight associated with each sector.
*     C1( 6 ) = REAL (Returned)
*        The coefficients of the transformation from sector indices to
*        focal plane offsets (in radians).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      INTEGER BAND
      REAL ZFWHM
      REAL YFWHM
      INTEGER NZSECT
      INTEGER NYSECT

*  Arguments Returned:
      REAL SECWGT( NZSECT, NYSECT )
      REAL C1( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    CY                 ! Constant used to evaluate in-scan
                                 ! Gaussian.
      REAL    CZ                 ! Constant used to evaluate cross-scan
                                 ! Gaussian.
      REAL    DY                 ! In-scan offset from detector centre,
                                 ! in radians.
      REAL    DZ                 ! Cross-scan offset from detector
                                 ! centre, in radians.
      INTEGER I                  ! Cross-scan sector index.
      INTEGER J                  ! In-scan sector index.
      REAL    SUM                ! Sum of all sector weight values.
      REAL    WGT                ! Constant sector weight value.
      REAL    YTERM              ! In-scan term in EXP argument.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if either NZSECT or NYSECT are zero.
      IF( NZSECT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MAPCA6_ERR1',
     :           'MAPCA6: Argument NZSECT is zero (programming error).',
     :                 STATUS )
         GO TO 999
      END IF

      IF( NYSECT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MAPCA6_ERR2',
     :           'MAPCA6: Argument NYSECT is zero (programming error).',
     :                 STATUS )
         GO TO 999
      END IF

*  Set up the linear transformation coefficients which describe the
*  mapping from sector indices to offsets in focal plane coordinates
*  (Z,Y) from the centre of a full size detector. The resulting offsets
*  are in radians.
      C1( 1 ) = -0.5*IRA__AM2R*REAL( I90__DZ( BAND ) )
     :          *( 1.0 + 1.0/REAL( NZSECT ) )
      C1( 2 ) = IRA__AM2R*REAL( I90__DZ( BAND ) )/REAL( NZSECT )
      C1( 3 ) = 0.0

      C1( 4 ) = -0.5*IRA__AM2R*REAL( I90__DY( BAND ) )
     :          *( 1.0 + 1.0/REAL( NYSECT ) )
      C1( 5 ) = 0.0
      C1( 6 ) = IRA__AM2R*REAL( I90__DY( BAND ) )/REAL( NYSECT )

*  First deal with the Gaussian weighting scheme.
      IF( ZFWHM*YFWHM .NE. 0.0 ) THEN

*  Calculate the cross-scan and in-scan constants required to evaluate
*  the two Gaussians.
         CZ = -2.77259/( ZFWHM**2 )
         CY = -2.77259/( YFWHM**2 )

*  Initialise the total data sum to zero.
         SUM = 0.0

*  Loop round each row in the array of sector weights.
         DO J = 1, NYSECT

*  Calculate the in-scan offset from the detector centre to this row
*  in radians.
            DY = C1( 4 ) + C1( 6 )*REAL( J )

*  Calculate the in-scan contribution to the exponential argument..
            YTERM = CY*( DY**2 )

*  Loop round each sector within this row.
            DO I = 1, NZSECT

*  Calculate the cross-scan offset from the detector centre to this
*  sector in radians.
               DZ = C1( 1 ) + C1( 2 )*REAL( I )

*  Calculate the total Gaussian.
               SECWGT( I, J ) = EXP( CZ*( DZ**2 ) + YTERM )

*  Increment the total data sum.
               SUM = SUM + SECWGT( I, J )

            END DO

         END DO

*  Now normalise the array of weights so that it has a total data sum of
*  1.0.
         DO J = 1, NYSECT
            DO I = 1, NZSECT
               SECWGT( I, J ) = SECWGT( I, J )/SUM
            END DO
         END DO

*  Now deal with the uniform weighting scheme.
      ELSE

*  Set up the constant weight to be assigned to each sector so that the
*  total data sum will be 1.0.
         WGT = 1.0/REAL( NZSECT*NYSECT )

*  Store this constant value in each sector.
         DO J = 1, NYSECT
            DO I = 1, NZSECT
               SECWGT( I, J ) = WGT
            END DO
         END DO

      END IF

 999  CONTINUE

      END
