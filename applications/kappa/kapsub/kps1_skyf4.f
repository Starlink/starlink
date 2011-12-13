      SUBROUTINE KPS1_SKYF4( PROJEC, NP, P, SCS, EPOCH, NPOS, AA, BB,
     :                       XX, YY, RMS, LOGFD, XO, YO, STATUS )
*+
*  Name:
*     KPS1_SKYF4

*  Purpose:
*     Log the final projection parameters and corresponding residuals
*     found by application SETSKY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SKYF4( PROJEC, NP, P, SCS, EPOCH, NPOS, AA, BB, XX, YY,
*                      RMS, LOGFD, XO, YO, STATUS )

*  Description:
*     The supplied parameters are logged to the file identified by FD.
*     The supplied sky co-ordinates are transformed and the residuals
*     with the supplied images co-ordinates are found and logged.  These
*     residuals are logged as the distance (in pixels) from the
*     transformed position to the supplied position.

*  Arguments:
*     PROJEC = CHARACTER * ( * ) (Given)
*        The name of the projection.
*     NP = INTEGER (Given)
*        The size of array P.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The projection parameters.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system.
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the the sky co-ordinates were
*        determined.
*     NPOS = INTEGER (Given)
*        The number of positions.
*     AA( NPOS ) = DOUBLE PRECISION (Given)
*        The sky longitude values.
*     BB( NPOS ) = DOUBLE PRECISION (Given)
*        The sky latitude values.
*     XX( NPOS ) = DOUBLE PRECISION (Given)
*        The image X values.
*     YY( NPOS ) = DOUBLE PRECISION (Given)
*        The image Y values.
*     RMS = REAL (Given)
*        The RMS residual in pixels.
*     LOGFD = INTEGER (Given)
*        The FIO file descriptor for the log file.
*     XO( NPOS ) = DOUBLE PRECISION (Returned)
*        The image X values formed by transforming AA and BB.
*     YO( NPOS ) = DOUBLE PRECISION (Returned)
*        The image Y values formed by transforming AA and BB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     31-OCT-1994 (DSB):
*        Original version.
*     1996 June 13 (MJC):
*        Output the pixel dimensions in arcseconds, where appropriate.
*     3-DEC-1998 (DSB):
*        Fix bug which caused the same position to be displayed on each
*        line of the log.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'IRA_PAR'          ! IRA public constants

*  Arguments Given:
      CHARACTER * ( * ) PROJEC
      INTEGER NP
      DOUBLE PRECISION P( NP )
      CHARACTER * ( * ) SCS
      DOUBLE PRECISION EPOCH
      INTEGER NPOS
      DOUBLE PRECISION AA( NPOS )
      DOUBLE PRECISION BB( NPOS )
      DOUBLE PRECISION XX( NPOS )
      DOUBLE PRECISION YY( NPOS )
      REAL RMS
      INTEGER LOGFD

*  Arguments Returned:
      DOUBLE PRECISION XO( NPOS )
      DOUBLE PRECISION YO( NPOS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( IRA__SZFSC ) ATEXT ! Formatted longitude value
      CHARACTER * ( IRA__SZFSC ) BTEXT ! Formatted latitude value
      CHARACTER * ( 80 ) BUF     ! Output buffer
      INTEGER I                  ! Loop count
      INTEGER IDA                ! IRA astrometry identifier
      INTEGER LBUF               ! Used length of BUF
      CHARACTER* ( IRA__SZFSC ) PATEXT ! Formatted position angle
      DOUBLE PRECISION RESID     ! Residual, in pixels
      CHARACTER* ( IRA__SZFSC ) TTEXT ! Formatted tilt angle

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a title for the parameter values which are to be displayed.
      CALL FIO_WRITE( LOGFD, ' ', STATUS )

      CALL MSG_SETR( 'RMS', RMS )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG1', '  These parameter values '/
     :               /'give an RMS positional error of ^RMS '/
     :               /'pixels ... ', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )

*  Display the parameter values, etc., to be stored in the NDF.
      CALL MSG_SETC( 'PRJ', PROJEC )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG2', '    Projection type         '/
     :               /'             : ^PRJ', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      CALL IRA_DTOC( P( 1 ), P( 2 ), SCS, 0, ATEXT, BTEXT, STATUS )
      CALL MSG_SETC( 'A', ATEXT )
      CALL MSG_SETC( 'B', BTEXT )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG3', '    Sky co-ordinates of '/
     :               /'reference point  : ^A, ^B', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      CALL MSG_SETR( 'X', REAL( P( 3 ) ) )
      CALL MSG_SETR( 'Y', REAL( P( 4 ) ) )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG4', '    Image co-ordinates of '/
     :               /'reference point: (^X,^Y)', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      IF ( REAL( IRA__R2AM * P( 5 ) ) .LT. 1.0 .AND.
     :     REAL( IRA__R2AM * P( 6 ) ) .LT. 1.0 ) THEN
         CALL MSG_SETR( 'DX', REAL( IRA__R2AS * P( 5 ) ) )
         CALL MSG_SETR( 'DY', REAL( IRA__R2AS * P( 6 ) ) )
         CALL MSG_LOAD( 'KPS1_SKYF4_MSG5S', '    Pixel dimensions    '/
     :                  /'                 : (^DX,^DY) arcsecs', BUF,
     :                  LBUF, STATUS )
      ELSE

         CALL MSG_SETR( 'DX', REAL( IRA__R2AM * P( 5 ) ) )
         CALL MSG_SETR( 'DY', REAL( IRA__R2AM * P( 6 ) ) )
         CALL MSG_LOAD( 'KPS1_SKYF4_MSG5', '    Pixel dimensions    '/
     :                  /'                 : (^DX,^DY) arcmins', BUF,
     :                  LBUF, STATUS )
      END IF
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      CALL IRA_DTOC1( P( 7 ), 'GAL', 1, 2, PATEXT, STATUS )
      CALL MSG_SETC( 'PA', PATEXT )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG6', '    Position angle of image '/
     :               /'Y axis       : ^PA', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      CALL IRA_DTOC1( P( 8 ), 'GAL', 1, 2, TTEXT, STATUS )
      CALL MSG_SETC( 'TILT', TTEXT )
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG7', '    Tilt of celestial sphere'/
     :               /'             : ^TILT', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )


      CALL FIO_WRITE( LOGFD, ' ', STATUS )

*  Get an IRA identifier for the supplied astrometry information.
      CALL IRA_CREAT( PROJEC, 8, P, SCS, EPOCH, NDF__NOID, IDA,
     :                STATUS )

*  Transform the sky co-ordinates supplied by the user to image
*  co-ordinates.
      CALL IRA_TRANS( NPOS, AA, BB, .FALSE., SCS, IDA, XO, YO, STATUS )

*  Release the resources used to store the astrometry information within
*  IRA.
      CALL IRA_ANNUL( IDA, STATUS )

*  Display the residuals at each point.
      CALL MSG_LOAD( 'KPS1_SKYF4_MSG8', '  Residuals at each supplied'/
     :               /' sky position: ', BUF, LBUF, STATUS )
      CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )

      DO I = 1, NPOS

         IF ( XO( I ) .NE. VAL__BADD .AND. YO( I ) .NE. VAL__BADD ) THEN
            RESID = SQRT( MAX( 0.0D0, ( XX( I ) - XO( I ) )**2 +
     :                                ( YY( I ) - YO( I ) )**2 ) )

            CALL IRA_DTOC( AA( I ), BB( I ), SCS, 0, ATEXT, BTEXT,
     :                     STATUS )
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_SETR( 'R', REAL( RESID )/RMS )
            CALL MSG_LOAD( 'KPS1_SKYF4_MSG9', '    ^I: (^A, ^B) - ^R '/
     :                     /'sigma', BUF, LBUF, STATUS )

         ELSE
            CALL MSG_SETI( 'I', I )
            CALL MSG_LOAD( 'KPS1_SKYF4_MSG10', '    ^I: <invalid '/
     :                     /'position>', BUF, LBUF, STATUS )

         END IF

         CALL FIO_WRITE( LOGFD, BUF( : LBUF ), STATUS )

      END DO

      CALL FIO_WRITE( LOGFD, ' ', STATUS )

      END
