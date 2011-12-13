      PROGRAM TRN_TEST
*+
*  Name:
*     TRN_TEST

*  Purpose:
*     Test installation of the TRANSFORM facility.

*  Language:
*     Starlink Fortran

*  Description:
*     This program exercises the TRANSFORM facility to ensure it is
*     correctly installed.  It is not an exhaustive test of the system.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     7-DEC-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Adapted for use with Unix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Status:
      INTEGER STATUS             ! Error status

*  Local Constants:
      INTEGER NDAT               ! Number of data points
      PARAMETER ( NDAT = 100 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCTR1 ! Locator to first transformation
      CHARACTER * ( DAT__SZLOC ) LOCTR2 ! Locator to second transformation
      CHARACTER * ( TRN__SZPRC ) PREC ! Precision specification
      CHARACTER * 40 FOR( 2 )    ! Forward transformation functions
      CHARACTER * 40 INV( 2 )    ! Inverse transformation functions
      INTEGER I                  ! Data point counter
      INTEGER IDT                ! Compiled mapping identifier
      REAL XIN( NDAT )           ! Input X coordinates
      REAL XOUT( NDAT )          ! Output X coordinates
      REAL YIN( NDAT )           ! Input Y coordinates
      REAL YOUT( NDAT )          ! Output Y coordinates

*.

*  Initialise status and start up HDS.
      STATUS = SAI__OK
      CALL HDS_START( STATUS )

*  Create the first transformation, converting from cartesian to polar
*  coordinates.
      FOR( 1 ) = 'r = sqrt( x * x + y * y )'
      FOR( 2 ) = 'theta = atan2d( y, x )'
      INV( 1 ) = 'x = r * sind( theta )'
      INV( 2 ) = 'y = r * cosd( theta )'
      PREC = '_REAL:'
      CALL TRN_NEW( 2, 2, FOR, INV, PREC, 'A comment.', ' ', ' ',
     :              LOCTR1, STATUS )

*  Create the second transformation, converting to log-polar
*  coordinates.
      FOR( 1 ) = 'rlog = log10( r )'
      FOR( 2 ) = 'theta_out = theta_in'
      INV( 1 ) = 'r = 10.0 ** rlog'
      INV( 2 ) = 'theta_in = theta_out'
      PREC = '_REAL:'
      CALL TRN_NEW( 2, 2, FOR, INV, PREC, 'A comment.', ' ', ' ',
     :              LOCTR2, STATUS )

*  Append the second transformation to the first one.
      CALL TRN_APND( LOCTR1, LOCTR2, STATUS )
      CALL DAT_ANNUL( LOCTR2, STATUS )

*  Compile the combined transformation.
      IDT = TRN__NOID
      CALL TRN_COMP( LOCTR1, .TRUE., IDT, STATUS )
      CALL DAT_ANNUL( LOCTR1, STATUS )

*  Set up the input data.
      DO I = 1, NDAT
        XIN( I ) = REAL( I - 1 ) - 0.5 * REAL( NDAT - 1 )
        YIN( I ) = 10.0
      ENDDO

*  Transform the data.
      CALL TRN_TR2R( .FALSE., NDAT, XIN, YIN, IDT, XOUT, YOUT, STATUS )

*  Annul the compiled mapping.
      CALL TRN_ANNUL( IDT, STATUS )

*  Display the results.
      DO 1 I = 1, NDAT
         CALL MSG_SETI( 'I', I )
         CALL MSG_SETR( 'XIN', XIN( I ) )
         CALL MSG_SETR( 'YIN', YIN( I ) )
         CALL MSG_SETR( 'XOUT', XOUT( I ) )
         CALL MSG_SETR( 'YOUT', YOUT( I ) )
         CALL MSG_OUT( 'TRN_TEST_PNT',
     :                 '   Point ^I: (^XIN,^YIN) --> (^XOUT,^YOUT)',
     :                 STATUS )
 1    CONTINUE

*  Close down.
      CALL TRN_CLOSE( STATUS )

*  If there has been no error, then report success.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'TRANSFORM_OK',
     :                 '   TRANSFORM installation test succeeded.',
     :                 STATUS )
         CALL MSG_BLANK( STATUS )

*  Otherwise report failure.
      ELSE
         CALL ERR_REP( 'TRN_TEST_ERR',
     :                 'TRN_TEST: TRANSFORM installation test failed.',
     :                 STATUS )
      END IF

*  Exit routine.
      END
