      SUBROUTINE KPS1_SKYF3( M, N, NPOS, XC, AA, BB, XX, YY, XO, YO,
     :                       FVECC, STATUS )
*+
*  Name:
*     KPS1_SKYF3

*  Purpose:
*     Find the spatial residuals at a set of positions after
*     transformation using a supplied IRA projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SKYF3( M, N, NPOS, XC, AA, BB, XX, YY, XO, YO, FVECC,
*                      STATUS )

*  Description:
*     The supplied sky co-ordinates are transformed using an IRA
*     projection defined by the argument XC (together with other
*     information passed from KPS1_SKYFT in common block SFT_COM).
*     The resulting image co-ordinates are compared with the supplied
*     image co-ordinates and the residuals in X and Y at each position
*     are returned in FVECC.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals to be calculated.  M should be two
*        times NPOS.
*     N = INTEGER (Given)
*        The number of projection parameters supplied in XC.  The
*        remaining ones are defined by values stored in common.
*     NPOS = INTEGER (Given)
*        The size of the arrays AA, BB, XX, YY, XO and YO.
*     XC( N ) = DOUBLE PRECISION (Given)
*        The projection parameters for which the residuals are required.
*        These projection parameters are stored in the same order as the
*        "P" argument for subroutine IRA_CREAT, except that for each
*        projection parameter value which has been fixed by the user,
*        the remaining elements in XC are shuffled down to occupy the
*        location which otherwise would have been used by the fixed
*        projection parameter.  Fixed parameter values are supplied in
*        common.
*     AA( NPOS ) = DOUBLE PRECISION (Given)
*        The sky longitude values.
*     BB( NPOS ) = DOUBLE PRECISION (Given)
*        The sky latitude values.
*     XX( NPOS ) = DOUBLE PRECISION (Given)
*        The image X values.
*     YY( NPOS ) = DOUBLE PRECISION (Given)
*        The image Y values.
*     XO( NPOS ) = DOUBLE PRECISION (Returned)
*        The image X values formed by transforming AA and BB.
*     YO( NPOS ) = DOUBLE PRECISION (Returned)
*        The image Y values formed by transforming AA and BB.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        The residuals.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     31-OCT-1994 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'IRA_PAR'          ! IRA_ public constants

*  Global Variables:
      INCLUDE 'SFT_COM'          ! Used for communicating with KPS1_SKYFT
*        EPOCHC = DOUBLE PRECISION (Read)
*           The epoch of the observations.
*        ORIENTC = LOGICAL (Read)
*           Was the orientation of the image fixed by the user?
*        PC( 8 ) = DOUBLE PRECISION (Read)
*           The initial guess astrometry parameter values, including any
*           fixed values supplied by the user.
*        PRJECC= CHARACTER * (IRA__SZPRJ ) (Read)
*           The projection in use.
*        PSIZEC = LOGICAL (Read)
*           Were the pixel dimensions fixed by the user?
*        REFIMC = LOGICAL (Read)
*           Were the pixel co-ordinates of the reference position fixed
*           by the user?
*        REFSKC = LOGICAL (Read)
*           Were the sky co-ordinates of the reference position fixed by
*           the user?
*        SCSC = CHARACTER * (  IRA__SZSCS ) (Read)
*           The sky co-ordinate system to use.
*        TILTC = LOGICAL (Read)
*           Was the tilt of the celestial sphere prior to projection
*           fixed by the user?

*  Arguments Given:
      INTEGER M
      INTEGER N
      INTEGER NPOS
      DOUBLE PRECISION XC( N )
      DOUBLE PRECISION AA( NPOS )
      DOUBLE PRECISION BB( NPOS )
      DOUBLE PRECISION XX( NPOS )
      DOUBLE PRECISION YY( NPOS )

*  Arguments Given and Returned:
      DOUBLE PRECISION XO( NPOS )
      DOUBLE PRECISION YO( NPOS )

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Counter
      INTEGER IDA                ! IRA identifier for astrometry info

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the new values for the parameters being optimized into the
*  current parameter array.
      I = 0

      IF ( .NOT. REFSKC ) THEN
         I = I + 2
         PC( 1 ) = XC( I - 1 )
         PC( 2 ) = XC( I )
      END IF

      IF ( .NOT. REFIMC ) THEN
         I = I + 2
         PC( 3 ) = XC( I - 1 )
         PC( 4 ) = XC( I )
      END IF

      IF ( .NOT. PSIZEC ) THEN
         I = I + 2
         PC( 5 ) = XC( I - 1 )
         PC( 6 ) = XC( I )
      END IF

      IF ( .NOT. ORIENTC ) THEN
         I = I + 1
         PC( 7 ) = XC( I )
      END IF

      IF ( .NOT. TILTC ) THEN
         I = I + 1
         PC( 8 ) = XC( I )
      END IF

*  Get an IRA identifier for this astrometry information.
      CALL IRA_CREAT( PRJECC, 8, PC, SCSC, EPOCHC, NDF__NOID, IDA,
     :                STATUS )

*  Transform the sky co-ordinates supplied by the user to image
*  co-ordinates using the current projection parameters.
      CALL IRA_TRANS( NPOS, AA, BB, .FALSE., SCSC, IDA, XO, YO, STATUS )

*  Release the resources used to store the astrometry information within
*  IRA.
      CALL IRA_ANNUL( IDA, STATUS )

*  Form the (linear) residuals between the supplied image co-ordinates
*  and the images co-ordinates implied by the current parameter values.
*  The X residuals are stored in the first half of FVECC and the Y
*  residuals in the second half.
      DO I = 1, NPOS
         IF ( XO( I ) .NE. VAL__BADD .AND. YO( I ) .NE. VAL__BADD ) THEN
            FVECC( I ) = XX( I ) - XO( I )
            FVECC( NPOS + I ) = YY( I ) - YO( I )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_SKYF3_ERR1', 'Supplied positions '/
     :        /'fall outside the domain of the current projection.',
     :        STATUS )
         END IF

      END DO

      END
