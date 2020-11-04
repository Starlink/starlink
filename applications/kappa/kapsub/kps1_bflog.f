      SUBROUTINE KPS1_BFLOG( LOGF, FD, PIXEL, MAP, CFRM, NBEAM,
     :                       NCOEF, P, SIGMA, REFOFF, REFLAB, POLAR,
     :                       POLSIG, RMS, DPREC, STATUS )
*+
*  Name:
*     KPS1_BFLOG

*  Purpose:
*     Logs the final fit parameters found by application BEAMFIT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFLOG( LOGF, FD, PIXEL, MAP, CFRM, NCOEF, P,
*                      SIGMA, REFOFF, REFLAB, POLAR, POLSIG, RMS,
*                      DPREC, STATUS )

*  Description:
*     The supplied parameters are reported and logged to the file
*     identified by FD.  The centre co-ordinates and beam widths
*     are reported in the current Frame.  The reported widths are
*     full-width half maxima.

*  Arguments:
*     LOGF = LOGICAL (Given)
*        Whether or not a log file is to be written.
*     FD = INTEGER (Given)
*        The FIO file descriptor for the log file.  It is ignored if
*        LOGF is .FALSE.
*     PIXEL = LOGICAL (Given)
*        If TRUE the beam centre and measures of width are in PIXEL
*        co-ordinates.  If FALSE, reports use the current Frame's
*        co-ordinates using AST formatting.  The P and SIGMA arrays
*        should come supplied with co-ordinates is the resested frame.
*        The PIXEL=.TRUE. option is largely for diagnostic purposes.
*     MAP = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     CFRM = INTEGER (Given)
*        A pointer to the current Frame of the NDF.  This argument is
*        ignored if PIXEL is FALSE.
*     NBEAM = INTEGER (Given)
*        The number of beams fitted.
*     NCOEF = INTEGER (Given)
*        The number of coefficients per beam fit.
*     P( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The fit parameters.  Spatial co-ordinates are measured in
*        the reporting frame when argument PIXEL is .FALSE. (the normal
*        value), or in the PIXEL Frame if PIXEL is .TRUE.  See
*        KPS1_BFCRF to convert from PIXEL to reporting-Frame
*        co-ordinates.
*     SIGMA( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The errors in the fit parameters.  The errors of spatial
*        co-ordinates are measured in the reporting frame when argument
*        PIXEL is .FALSE. (the normal value), or in the PIXEL Frame if
*        PIXEL is .TRUE.
*     REFOFF( 2 ) = DOUBLE PRECISION (Given)
*        The offset of the primary beam with respect to the reference
*        point measured in the current WCS Frame, followed by its error.
*     REFLAB = CHARACTER * (*) (Given)
*        Label used to describe reference position in the output.   At
*        present it should be either "map centre", if that was used in
*        the absence of a reference position stored with the original
*        dataset; or "sky reference position".  If another value is
*        supplied, "reference position" will be used.
*     POLAR( 2, NBEAM ) =  DOUBLE PRECISION (Given)
*        The polar co-ordinates of the beam features with respect to
*        the primary beam measured in the current co-ordinate Frame.
*        The orientation is a position angle in degrees, measured from
*        North through East if the current Frame is a Skyframe, or
*        anticlockwise from the Y axis otherwise.  The POLAR(*,1)
*        values of the primary beam are ignored.
*     POLSIG( 2, NBEAM ) =  DOUBLE PRECISION (Given)
*        The standard-deviation errors associated with the polar
*        co-ordinates supplied in argument POLAR.  The POLSIG(*,1)
*        values of the primary beam are ignored.
*     RMS = DOUBLE PRECISION (Given)
*        The RMS residual in pixels.
*     DPREC = CHARACTER ( * ) (Given)
*        Precision of the reported amplitude and background level.
*        Allowed values are the HDS numeric types: '_INTEGER',
*        '_REAL', or '_DOUBLE'.  Double precision will be used
*        if the supplied value is not one of these.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007, 2010, 2011, 2013 Science & Technology
*     Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 February 25 (MJC):
*        Original version.
*     2007 April 26 (MJC):
*        Added PIXEL argument.
*     2007 April 30 (MJC):
*        Add NBEAM argument so that all beam features are reported.
*     2007 May 10 (MJC):
*        Added DPREC argument.  Convert orientation into reporting
*        Frame.
*     2007 May 21 (MJC):
*        Remove the conversions from PIXEL to the reporting Frame.
*     2007 May 25 (MJC):
*        Widen the output buffer and message wrap for long orientation
*        lines.
*     2007 May 30 (MJC):
*        Add POLAR and POLSIG arguments to report polar co-ordinates of
*        secondary-beam features.
*     2007 June 15 (MJC):
*        Add REFOFF argument and report its values.  Qualify sense of
*        polar co-ordinates in the output now there are two different
*        offsets.  Move RMS report to the end thereby placing the
*        position directly under its the header co-ordinates
*        description.  Create non-blank units for SkyFrame polar offset
*        co-ordinates.
*     2007 June 20 (MJC):
*        Apply MSG tuning regardless of the Frame.  Correct non-SKY
*        SENSE.
*     2007 July 9 (MJC):
*        For a SkyFrame report centre errors in arcseconds instead of
*        sexagesimal.  Added REFLAB argument to indicate which reference
*        point was used.
*     2010 July 5 (MJC):
*        Switched to generalised normal distribution by introducing the
*        shape exponent.
*     2011 May 11 (MJC):
*        Removed no-longer-used argument MAP3.
*     2013 July 16 (MJC):
*        Use circular constraint.
*    {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'BF_PAR'           ! BEAMFIT constants

*  Global Variables:
      INCLUDE 'BF_COM'           ! Used for communicating with PDA
                                 ! routine
*        CIRC = LOGICAL (Read)
*           Circular beam fixed by user?

*  Arguments Given:
      LOGICAL LOGF
      INTEGER FD
      LOGICAL PIXEL
      INTEGER MAP
      INTEGER CFRM
      INTEGER NBEAM
      INTEGER NCOEF
      DOUBLE PRECISION P( NCOEF, NBEAM )
      DOUBLE PRECISION SIGMA( NCOEF, NBEAM )
      DOUBLE PRECISION REFOFF( 2 )
      CHARACTER*(*) REFLAB
      DOUBLE PRECISION POLAR( 2, NBEAM )
      DOUBLE PRECISION POLSIG( 2, NBEAM )
      DOUBLE PRECISION RMS
      CHARACTER*(*) DPREC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION R2D       ! Radians to degrees
      PARAMETER ( R2D = 180.0D0 / PI )

*  Local Variables:
      CHARACTER*9 ATTR           ! Name of an AST attribute
      CHARACTER*100 BUF          ! Output buffer
      LOGICAL EQUNIT             ! Same unit for both sky axes?
      CHARACTER*4 FORMAT         ! Orientation format
      INTEGER FRM2               ! Copied reporting Frame
      DOUBLE PRECISION FWHM      ! Full-width half maximum
      DOUBLE PRECISION FWHME     ! FWHM error
      INTEGER IB                 ! Beam loop count
      LOGICAL ISSKY              ! Is the current Frame a SkyFrame?
      INTEGER LAT                ! Index to latitude axis in SkyFrame
      INTEGER LATTR              ! Used length of ATTR
      INTEGER LBUF               ! Used length of BUF
      INTEGER LON                ! Index to longitude axis in SkyFrame
      INTEGER MAJOR              ! Index to major-FWHM value and error
      INTEGER MINOR              ! Index to minor-FWHM value and error
      INTEGER REPAX              ! Axis used for reporting distances
      DOUBLE PRECISION S2FWHM    ! Standard deviation to FWHM
      DOUBLE PRECISION THETA     ! Orientation in degrees
      CHARACTER*30 UNIT1         ! Units for first sky axis
      CHARACTER*30 UNIT2         ! Units for second sky axis

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  This is really a parameter but we cannot use this expression
*  in a PARAMETER statement.
      S2FWHM = SQRT( 8.D0 * LOG( 2.D0 ) )

*  Determine the format units.  The convention is to use a sky latitude
*  (since we measure small angular distances in arcseconds not seconds
*  of time for equatorial), or the first axis.
      ISSKY =  AST_ISASKYFRAME( CFRM, STATUS )
      IF ( ISSKY ) THEN
         LAT = AST_GETI( CFRM, 'LatAxis', STATUS )
         LON = AST_GETI( CFRM, 'LonAxis', STATUS )
         REPAX = LAT
      ELSE
         REPAX = 1
      END IF

*  Reformat sky distances to milliarcseconds.
*  ==========================================
*  Take a copy of the current Frame so that we can set its attributes
*  without changing the way the beam positions are formatted.
      FRM2 = AST_COPY( CFRM, STATUS )

*  Form the Format attribute name for the latitude axis.
      ATTR = 'FORMAT('
      LATTR = 7
      CALL CHR_PUTI( REPAX, ATTR, LATTR )
      CALL CHR_APPND( ')', ATTR, LATTR )

      IF ( ISSKY .AND. .NOT. PIXEL ) THEN

*  Set the format for the FWHM values so they are displayed with three
*  decimal places.  We are assuming here that non-SkyFrame will be
*  Some form of pixel-based domain, like GRID or PIXEL or OFFSET.
         CALL AST_SETC( FRM2, ATTR( : LATTR ), 's.3', STATUS )
      ELSE
         CALL AST_SETC( FRM2, ATTR( : LATTR ), '%-12.3F', STATUS )
      END IF

*  Now do the screen and log file output.

*  Report each fit in turn.
      DO IB = 1, NBEAM

*  Beam position
*  =============

*  Centre
*  ------

*  Display the pixel co-ordinates...
         IF ( PIXEL ) THEN
            CALL MSG_SETR( 'XP', SNGL( P( 1, IB ) ) )
            CALL MSG_SETR( 'YP', SNGL( P( 2, IB ) ) )
            CALL MSG_SETC( 'UNIT', 'pixels' )

         ELSE

*  Display the current Frame's co-ordinates of the beam centre.
            CALL MSG_SETC( 'XP', AST_FORMAT( CFRM, 1, P( 1, IB ),
     :                                       STATUS ) )
            CALL MSG_SETC( 'YP', AST_FORMAT( CFRM, 2, P( 2, IB ),
     :                                       STATUS ) )

*  Form the Unit attribute name for this axis.
            IF ( .NOT. ISSKY ) THEN
               ATTR = 'UNIT('
               LATTR = 5
               CALL CHR_PUTI( REPAX, ATTR, LATTR )
               CALL CHR_APPND( ')', ATTR, LATTR )

*  Get the Unit value.
               CALL MSG_SETC( 'UNIT', AST_GETC( CFRM, ATTR( : LATTR ),
     :                                          STATUS ) )

*  It would be messy inserting both of the expected sexagesimal formats.
*  Report errors in arcseconds.
            ELSE
               UNIT1 = AST_GETC( CFRM, 'UNIT(1)', STATUS )
               UNIT2 = AST_GETC( CFRM, 'UNIT(2)', STATUS )
               EQUNIT = UNIT1 .EQ. UNIT2
               IF ( EQUNIT ) THEN
                  CALL MSG_SETC( 'UNIT', UNIT1 )
               ELSE
                  CALL MSG_SETC( 'UNIT', '' )
               END IF
               CALL MSG_SETC( 'EUNIT', 'arcsec' )
            END IF
         END IF

         IF ( SIGMA( 1, IB ) .EQ. VAL__BADD ) THEN
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG2', '    Centre      '/
     :                     /'    : (^XP,^YP) ^UNIT', BUF, LBUF, STATUS )

         ELSE
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'XE', SNGL( SIGMA( 1, IB ) ) )
               CALL MSG_SETR( 'YE', SNGL( SIGMA( 2, IB ) ) )
            ELSE

*  Centre errors
*  -------------
               IF ( ISSKY ) THEN
                  CALL MSG_SETC( 'XE',
     :                           AST_FORMAT( FRM2, 2, SIGMA( 1, IB ),
     :                                       STATUS ) )
                  CALL MSG_SETC( 'YE',
     :                           AST_FORMAT( FRM2, 2, SIGMA( 2, IB ),
     :                                       STATUS ) )

               ELSE
                  CALL MSG_SETC( 'XE',
     :                           AST_FORMAT( CFRM, 1, SIGMA( 1, IB ),
     :                                       STATUS ) )
                  CALL MSG_SETC( 'YE',
     :                           AST_FORMAT( CFRM, 2, SIGMA( 2, IB ),
     :                                       STATUS ) )
               END IF
            END IF
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG2E', '    Centre      '/
     :                     /'    : (^XP,^YP) ^UNIT +/- (^XE,^YE) '/
     :                     /'^EUNIT', BUF, LBUF, STATUS )
         END IF
         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Offset (polar radius) of primary beam with to the reference point
*  -----------------------------------------------------------------
         IF ( IB .EQ. 1 ) THEN

*  Display the pixel co-ordinates...
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'OFF', SNGL( REFOFF( 1 ) ) )
               CALL MSG_SETC( 'UNIT', 'pixels' )

            ELSE

*  Display the current Frame's co-ordinates of the beam offset
               CALL MSG_SETC( 'OFF', AST_FORMAT( FRM2, REPAX,
     :                                           REFOFF( 1 ), STATUS ) )

*  Form the Unit attribute name for the selected axis.
               ATTR = 'UNIT('
               LATTR = 5
               CALL CHR_PUTI( REPAX, ATTR, LATTR )
               CALL CHR_APPND( ')', ATTR, LATTR )
               IF ( ISSKY ) THEN

*  Get the Unit value.  We use the arcseconds Format for a SkyFrame.
                  CALL MSG_SETC( 'UNIT', AST_GETC( FRM2,
     :                                             ATTR( : LATTR ),
     :                                             STATUS ) )
               ELSE
                  CALL MSG_SETC( 'UNIT', AST_GETC( CFRM,
     :                                             ATTR( : LATTR ),
     :                                             STATUS ) )
               END IF
            END IF

            IF ( REFLAB( 1:10 ) .NE. 'map centre' .AND.
     :           REFLAB( 1:22 ) .NE. 'sky reference position' ) THEN
               CALL MSG_SETC( 'LABEL', 'reference position' )
            ELSE
               CALL MSG_SETC( 'LABEL', REFLAB )
            END IF

            IF ( REFOFF( 2 ) .EQ. VAL__BADD ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG3', '    Offset      '/
     :                        /'    : ^OFF ^UNIT from ^LABEL',
     :                        BUF, LBUF, STATUS )

            ELSE
               IF ( PIXEL ) THEN
                  CALL MSG_SETR( 'OFFE', SNGL( REFOFF( 2 ) ) )
               ELSE

*  Offset error of primary beam with to the reference point
*  --------------------------------------------------------
                  CALL MSG_SETC( 'OFFE', AST_FORMAT( FRM2, REPAX,
     :                                               REFOFF( 2 ),
     :                                               STATUS ) )
               END IF
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG3E', '    Offset      '/
     :                        /'    : ^OFF +/- ^OFFE ^UNIT from '/
     :                        /'^LABEL', BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Polar radius of secondary beam positions
*  ----------------------------------------

*  Note store the two separately as they have to be loaded as separate
*  lines because of the different units.
         ELSE

*  Display the pixel co-ordinates...
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'RAD', SNGL( POLAR( 1, IB ) ) )
               CALL MSG_SETC( 'UNIT', 'pixels' )

            ELSE

*  Display the current Frame's co-ordinates of the beam separation.
               CALL MSG_SETC( 'RAD', AST_FORMAT( FRM2, REPAX,
     :                                           POLAR( 1, IB ),
     :                                           STATUS ) )

*  Form the Unit attribute name for the selected axis.
               ATTR = 'UNIT('
               LATTR = 5
               CALL CHR_PUTI( REPAX, ATTR, LATTR )
               CALL CHR_APPND( ')', ATTR, LATTR )
               IF ( ISSKY ) THEN

*  Get the Unit value.  We use the arcseconds Format for a SkyFrame.
                  CALL MSG_SETC( 'UNIT', AST_GETC( FRM2,
     :                                             ATTR( : LATTR ),
     :                                             STATUS ) )
               ELSE
                  CALL MSG_SETC( 'UNIT', AST_GETC( CFRM,
     :                                             ATTR( : LATTR ),
     :                                             STATUS ) )
               END IF
            END IF

            IF ( POLSIG( 1, IB ) .EQ. VAL__BADD ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG4', '    Offset      '/
     :                        /'    : ^RAD ^UNIT from primary beam',
     :                       BUF, LBUF, STATUS )

            ELSE
               IF ( PIXEL ) THEN
                  CALL MSG_SETR( 'RADE', SNGL( POLSIG( 1, IB ) ) )
               ELSE

*  Polar radius errors
*  -------------------
                  CALL MSG_SETC( 'RADE', AST_FORMAT( FRM2, REPAX,
     :                                               POLSIG( 1, IB ),
     :                                               STATUS ) )
               END IF
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG4E', '    Offset      '/
     :                        /'    : ^RAD +/- ^RADE ^UNIT from '/
     :                        /'primary beam', BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Position angle of secondary beam positions
*  ------------------------------------------

* Format to left-justify the angle to two decimal places.
            WRITE ( FORMAT, '(''F'',I1,''.2'')' )
     :        MAX( 0, INT( LOG10( POLAR( 2, IB ) + VAL__EPSD ) ) ) + 4
            CALL MSG_FMTR( 'PA', FORMAT, SNGL( POLAR( 2, IB ) ) )

            IF ( PIXEL .OR. .NOT. ISSKY ) THEN
               CALL MSG_SETC( 'SENSE',
     :                        '(measured anticlockwise from Y)' )
            ELSE
               CALL MSG_SETC( 'SENSE',
     :                        '(measured from North through East)' )
            END IF

*  Position-angle errors
*  ---------------------
            IF ( POLSIG( 2, IB ) .EQ. VAL__BADD ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG5', '    Position Angle '/
     :                        /' : ^PA degrees ^SENSE', BUF, LBUF,
     :                        STATUS )

            ELSE

*  Format to left-justify the angle to two decimal places, unless
*  it is a nonsensical value.
               IF ( POLSIG( 2, IB ) .GT. 360.0 ) THEN
                  CALL MSG_SETC( 'PAE', 'undef' )
               ELSE
                  WRITE ( FORMAT, '(''F'',I1,''.2'')' )
     :              MAX( INT( LOG10( POLSIG( 2, IB ) + VAL__EPSD ) ),
     :                   0 ) + 4
                  CALL MSG_FMTR( 'PAE', FORMAT,
     :                           SNGL( POLSIG( 2, IB ) ) )
               END IF
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG5E', '    Position Angle '/
     :                        /' : ^PA +/- ^PAE degrees ^SENSE',
     :                        BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )
         END IF

*  FWHMs
*  =====

*  See which is major?  For simplicity this is determined in the
*  PIXEL domain, however, perhaps it should be done in the current
*  (reporting) Frame but what if the axes have different units?
*  We can review this if it's a problem in practice.
         IF ( P( 3, IB ) .GE. P( 4, IB ) ) THEN
            MAJOR = 3
            MINOR = 4
         ELSE
            MAJOR = 4
            MINOR = 3
         END IF

*  Major
*  -----
*  Create a token for the first-axis FWHM in the chosen co-ordinate
*  Frame.
         FWHM = P( MAJOR, IB ) * S2FWHM
         IF ( PIXEL ) THEN
            CALL MSG_SETR( 'FWHM', SNGL( FWHM ) )
            CALL MSG_SETC( 'UNIT', 'pixels' )

         ELSE
            CALL MSG_SETC( 'FWHM', AST_FORMAT( FRM2, REPAX, FWHM,
     :                                         STATUS ) )

*  Form the Unit attribute name for this axis.
            ATTR = 'UNIT('
            LATTR = 5
            CALL CHR_PUTI( REPAX, ATTR, LATTR )
            CALL CHR_APPND( ')', ATTR, LATTR )

            CALL MSG_SETC( 'UNIT',
     :                     AST_GETC( FRM2, ATTR( : LATTR ), STATUS ) )
         END IF

*  Error in major-axis width
*  -------------------------
*  Bad values for the errors in the FWHM indicate no value exists.
         IF ( SIGMA( MAJOR, IB ) .EQ. VAL__BADD ) THEN

*  So report and log just the FWHM values in the desired Frame.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG6', '    FWHM (major)'/
     :                     /'    : ^FWHM ^UNIT', BUF, LBUF, STATUS )

*  We have errors on the widths.
         ELSE
            FWHME = SIGMA( MAJOR, IB ) * S2FWHM
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'FWHME', SNGL( FWHME ) )

            ELSE
               CALL MSG_SETC( 'FWHME', AST_FORMAT( FRM2, REPAX, FWHME,
     :                                             STATUS ) )
            END IF

            IF ( CIRC ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG6E', '    FWHM        '/
     :                        /'    : ^FWHM +/- ^FWHME ^UNIT',
     :                        BUF, LBUF, STATUS )
            ELSE
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG6E', '    FWHM (major)'/
     :                        /'    : ^FWHM +/- ^FWHME ^UNIT',
     :                        BUF, LBUF, STATUS )
            END IF
         END IF

         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Minor
*  -----
*  Create a token for minor-axis FWHM value in the chosen co-ordinate
*  Frame.
         IF ( .NOT. CIRC ) THEN
            FWHM = P( MINOR, IB ) * S2FWHM
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'FWHM', SNGL( FWHM ) )
            ELSE

               CALL MSG_SETC( 'FWHM', AST_FORMAT( FRM2, REPAX, FWHM,
     :                                            STATUS ) )

*  Get the Unit value.  Note assume spatial domain comprises the
*  first two axes.
               CALL MSG_SETC( 'UNIT', AST_GETC( FRM2, ATTR( : LATTR ),
     :                                          STATUS ) )
            END IF

*  Error in minor-axis width
*  -------------------------
*  Bad values for the errors in the FWHM indicate no value exists.
            IF ( SIGMA( MINOR, IB ) .EQ. VAL__BADD ) THEN

*  So report and log just the FWHM value in the desired Frame.
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG7', '    FWHM (minor)'/
     :                        /'    : ^FWHM ^UNIT', BUF, LBUF, STATUS )

*  We have errors on the widths.
            ELSE
               FWHME = SIGMA( MINOR, IB ) * S2FWHM
               IF ( PIXEL ) THEN
                  CALL MSG_SETR( 'FWHME', SNGL( FWHME ) )

               ELSE
                  CALL MSG_SETC( 'FWHME', AST_FORMAT( FRM2, REPAX,
     :                                                FWHME, STATUS ) )
               END IF

               CALL MSG_LOAD( 'KPS1_BFLOG_MSG7E', '    FWHM (minor)'/
     :                        /'    : ^FWHM +/- ^FWHME ^UNIT',
     :                        BUF, LBUF, STATUS )
            END IF

            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Orientation
*  ===========
            IF  ( PIXEL .OR. .NOT. ISSKY ) THEN
               CALL MSG_SETC( 'SENSE',
     :                        '(measured anticlockwise from Y)' )
            ELSE

*  Reverse these if the axis order is swapped in the SkyFrame.
               CALL MSG_SETC( 'SENSE',
     :                        '(measured from North through East)' )
            END IF

*  The message may generate a line longer than 80 characters especially
*  if there is an uncertainty too.   Now 80 is normlly where a line will
*  wrap.  Tune to allow longer lines.
            CALL MSG_TUNE( 'SZOUT', 100, STATUS )

*  Display the major-axis orientation, formatting to have left-justified
*  to two decimal places, unless it's circular then omit reporting the
*  meaningless orientation.
            THETA = P( 5, IB ) * R2D
            IF ( THETA .NE. 0.0D0 ) THEN
               WRITE ( FORMAT, '(''F'',I1,''.2'')' )
     :              MAX( 0, INT( LOG10( THETA + VAL__EPSD ) ) ) + 4
               CALL MSG_FMTR( 'OR', FORMAT, REAL( THETA ) )
               IF ( SIGMA( 5, IB ) .EQ. VAL__BADD ) THEN
                  CALL MSG_LOAD( 'KPS1_BFLOG_MSG8', '    Orientation  '/
     :                           /'   : ^OR degrees ^SENSE', BUF, LBUF,
     :                           STATUS )
               ELSE
                  CALL MSG_FMTR( 'ORE', 'G9.3', SNGL( SIGMA( 5, IB ) ) )
                  CALL MSG_LOAD( 'KPS1_BFLOG_MSG8E', '    Orientation '/
     :                           /'    : ^OR +/- ^ORE degrees ^SENSE',
     :                           BUF, LBUF, STATUS )
               END IF
               IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
               CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Reset the width.  Is there no inquiry function to obtain previous
*  width?
               IF ( PIXEL .OR. .NOT. ISSKY )
     :           CALL MSG_TUNE( 'SZOUT', 100, STATUS )
            END IF
         END IF

*  Amplitude
*  =========

*  Determine the precision.
         IF ( DPREC .EQ. '_INTEGER' ) THEN
            CALL MSG_SETI( 'AMP', NINT( P( 6, IB ) ) )

         ELSE IF ( DPREC .EQ. '_REAL' ) THEN
            CALL MSG_SETR( 'AMP', REAL( P( 6, IB ) ) )

         ELSE
            CALL MSG_SETD( 'AMP', P( 6, IB ) )
         END IF

         IF ( SIGMA( 6, IB ) .EQ. VAL__BADD ) THEN

*  Display the amplitude.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG9', '    Amplitude   '/
     :                     /'    : ^AMP', BUF, LBUF, STATUS )

         ELSE
            IF ( DPREC .EQ. '_INTEGER' ) THEN
               CALL MSG_SETI( 'AMPE', NINT( SIGMA( 6, IB ) ) )

            ELSE IF ( DPREC .EQ. '_REAL' ) THEN
               CALL MSG_SETR( 'AMPE', REAL( SIGMA( 6, IB ) ) )

            ELSE
               CALL MSG_SETD( 'AMPE', SIGMA( 6, IB ) )
            END IF

*  Display the amplitude and its error.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG9E', '    Amplitude   '/
     :                     /'    : ^AMP +/- ^AMPE',
     :                     BUF, LBUF, STATUS )
         END IF
         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Background level
*  ================

*  The background level is the same for all beam positions.
         IF ( IB .EQ. 1 ) THEN

*  Determine the precision.
            IF ( DPREC .EQ. '_INTEGER' ) THEN
               CALL MSG_SETI( 'BGD', NINT( P( 7, IB ) ) )

            ELSE IF ( DPREC .EQ. '_REAL' ) THEN
               CALL MSG_SETR( 'BGD', REAL( P( 7, IB ) ) )

            ELSE
               CALL MSG_SETD( 'BGD', P( 7, IB ) )
            END IF

*  Display the background level.
            IF ( SIGMA( 7, IB ) .EQ. VAL__BADD ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG10', '    Background  '/
     :                        /'    : ^BGD', BUF, LBUF, STATUS )
            ELSE

               IF ( DPREC .EQ. '_INTEGER' ) THEN
                  CALL MSG_SETI( 'BGDE', NINT( SIGMA( 7, IB ) ) )

               ELSE IF ( DPREC .EQ. '_REAL' ) THEN
                  CALL MSG_SETR( 'BGDE', REAL( SIGMA( 7, IB ) ) )

               ELSE
                  CALL MSG_SETD( 'BGDE', SIGMA( 7, IB ) )
               END IF

*  Display the background level and its error.
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG10E', '    Background  '/
     :                        /'    : ^BGD +/- ^BGDE',
     :                        BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )
         END IF

*  Shape
*  =====
         WRITE ( FORMAT, '(''F'',I1,''.3'')' )
     :     MAX( 0, INT( LOG10( P( 8, IB ) + VAL__EPSD ) ) ) + 5
         CALL MSG_FMTR( 'GAMMA', FORMAT, SNGL( P( 8, IB ) ) )
         IF ( SIGMA( 8, IB ) .EQ. VAL__BADD ) THEN

*  Display the amplitude.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG11', '    Shape exponent '/
     :                     /' : ^GAMMA', BUF, LBUF, STATUS )

         ELSE
            WRITE ( FORMAT, '(''F'',I1,''.3'')' )
     :        MAX( 0, INT( LOG10( P( 8, IB ) + VAL__EPSD ) ) ) + 5
            CALL MSG_FMTR( 'GAMMAE', FORMAT, SNGL( SIGMA( 8, IB ) ) )

*  Display the exponent and its error.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG11E', '    Shape exponent '/
     :                     /' : ^GAMMA +/- ^GAMMAE',
     :                     BUF, LBUF, STATUS )
         END IF
         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

         IF ( LOGF ) CALL FIO_WRITE( FD, ' ', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

      END DO

*  Give a title for the parameter values which are to be displayed.
      CALL MSG_SETD( 'RMS', RMS )
      CALL MSG_LOAD( 'KPS1_BFLOG_MSG1', '    RMS fit error   : ^RMS',
     :               BUF, LBUF, STATUS )
      IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
      CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

      IF ( LOGF ) CALL FIO_WRITE( FD, ' ', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      CALL AST_ANNUL( FRM2, STATUS )

      END
