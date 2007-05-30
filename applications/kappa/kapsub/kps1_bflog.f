      SUBROUTINE KPS1_BFLOG( LOGF, FD, PIXEL, MAP, CFRM, NAXC, NBEAM, 
     :                       NCOEF, P, SIGMA, RMS, DPREC, STATUS )
*+
*  Name:
*     KPS1_BFLOG

*  Purpose:
*     Logs the final fit parameters found by application BEAMFIT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFLOG( LOGF, FD, PIXEL, MAP, CFRM, NAXC, NCOEF, P, 
*                      SIGMA, RMS, DPREC, STATUS )

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
*     NAXC = INTEGER (Given) 
*        The number of axes in CFRM.  This argument is ignored if PIXEL 
*        is FALSE.
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
*     Copyright (C) 2007 Particle Physics and Astronomy Research 
*     Council.  All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 February 25 (MJC):
*        Original version.
*     2007 April 26 (MJC):
*        Added PIXEL argument.
*     2007 May 10 (MJC):
*        Added DPREC argument.  Convert orientation into reporting
*        Frame.
*     2007 May 21 (MJC):
*        Remove the conversions from PIXEL to the reporting Frame.
*     2007 May 25 (MJC):
*        Widen the output buffer and message wrap for long orientation
*        lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants 

*  Arguments Given:
      LOGICAL LOGF
      INTEGER FD
      LOGICAL PIXEL
      INTEGER MAP
      INTEGER CFRM
      INTEGER NAXC
      INTEGER NBEAM
      INTEGER NCOEF
      DOUBLE PRECISION P( NCOEF, NBEAM )
      DOUBLE PRECISION SIGMA( NCOEF, NBEAM )
      DOUBLE PRECISION RMS
      CHARACTER*(*) DPREC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of one position from another

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION R2D       ! Radians to degrees
      PARAMETER ( R2D = 180.0D0 / PI )

*  Local Variables:
      CHARACTER*9 ATTR           ! Name of an AST attribute
      CHARACTER*100 BUF          ! Output buffer
      CHARACTER*4 FORMAT         ! Orientation format
      INTEGER FRM2               ! Copied reporting Frame
      DOUBLE PRECISION FWHM      ! Full-width half maximum
      DOUBLE PRECISION FWHME     ! FWHM error
      INTEGER I                  ! Loop count
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
      CHARACTER*30 UNITS( 2 )    ! Units along widths

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

*  Give a title for the parameter values which are to be displayed.
      CALL MSG_SETD( 'RMS', RMS )
      CALL MSG_LOAD( 'KPS1_BFLOG_MSG1', '    RMS fit error   : ^RMS',
     :               BUF, LBUF, STATUS )
      IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
      CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

      IF ( LOGF ) CALL FIO_WRITE( FD, ' ', STATUS )
      CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )

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
            CALL MSG_SETC( 'YP', AST_FORMAT( CFRM, 1, P( 2, IB ),
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
            ELSE
               CALL MSG_SETC( 'UNIT', ' ' )
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
               CALL MSG_SETC( 'XE',
     :                        AST_FORMAT( CFRM, 1, SIGMA( 1, IB ), 
     :                                    STATUS ) )
               CALL MSG_SETC( 'YE',
     :                        AST_FORMAT( CFRM, 2, SIGMA( 2, IB ),
     :                                    STATUS ) )
            END IF
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG2E', '    Centre      '/
     :                     /'    : (^XP,^YP) +/- (^XE,^YE) ^UNIT',
     :                     BUF, LBUF, STATUS )
         END IF
         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

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
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG3', '    FWHM (major)'/
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

            CALL MSG_LOAD( 'KPS1_BFLOG_MSG3E', '    FWHM (major)'/
     :                     /'    : ^FWHM +/- ^FWHME ^UNIT',
     :                     BUF, LBUF, STATUS )
         END IF

         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Minor
*  -----
*  Create a token for minor-axis FWHM value in the chosen co-ordinate
*  Frame.
         FWHM = P( MINOR, IB ) * S2FWHM
         IF ( PIXEL ) THEN
            CALL MSG_SETR( 'FWHM', SNGL( FWHM ) )
         ELSE

            CALL MSG_SETC( 'FWHM', AST_FORMAT( FRM2, REPAX, FWHM,
     :                                         STATUS ) )

*  Get the Unit value.  Note assume spatial domain comprises the
*  first two axes.
            CALL MSG_SETC( 'UNIT',
     :                     AST_GETC( FRM2, ATTR( : LATTR ), STATUS ) )
         END IF

*  Error in minor-axis width
*  -------------------------
*  Bad values for the errors in the FWHM indicate no value exists.
         IF ( SIGMA( MINOR, IB ) .EQ. VAL__BADD ) THEN

*  So report and log just the FWHM value in the desired Frame.
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG4', '    FWHM (minor)'/
     :                     /'    : ^FWHM ^UNIT', BUF, LBUF, STATUS )

*  We have errors on the widths.
         ELSE
            FWHME = SIGMA( MINOR, IB ) * S2FWHM
            IF ( PIXEL ) THEN
               CALL MSG_SETR( 'FWHME', SNGL( FWHME ) )

            ELSE
               CALL MSG_SETC( 'FWHME', AST_FORMAT( FRM2, REPAX, FWHME,
     :                                             STATUS ) )
            END IF

            CALL MSG_LOAD( 'KPS1_BFLOG_MSG4E', '    FWHM (minor)'/
     :                     /'    : ^FWHM +/- ^FWHME ^UNIT',
     :                     BUF, LBUF, STATUS )
         END IF

         IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )


*  Orientation
*  ===========
         IF  ( PIXEL .OR. .NOT. ISSKY ) THEN
            CALL MSG_SETC( 'SENSE', '(measured from X through Y)' )
         ELSE

*  Reverse these if the axis order is swapped in the SkyFrame.
            CALL MSG_SETC( 'SENSE',
     :                     '(measured from North through East)' )

*  The message may generate a line longer than 80 characters especially
*  if there is an uncertainty too.   Now 80 is normlly where a line will
*  wrap.  Tune to allow longer lines.
            CALL MSG_TUNE( 'SZOUT', 100, STATUS )
         END IF

*  Display the major-axis orientation, formatting to have left-justified
*  to two decimal places, unless it's circular then omit reporting the
*  meaningless orientation.
         THETA = P( 5, IB ) * R2D
         IF ( THETA .NE. 0.0D0 ) THEN
            WRITE ( FORMAT, '(''F'',I1,''.2'')' )  
     :           MAX( 0, INT( LOG10( THETA + VAL__EPSD ) ) ) + 4
            CALL MSG_FMTR( 'OR', FORMAT, REAL( THETA ) )
            IF ( SIGMA( 5, IB ) .EQ. VAL__BADD ) THEN
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG5', '    Orientation    '/
     :                        /' : ^OR degrees ^SENSE', BUF, LBUF,
     :                        STATUS )
            ELSE
               CALL MSG_FMTR( 'ORE', 'G9.3', SNGL( SIGMA( 5, IB ) ) )
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG5E', '    Orientation    '/
     :                        /' : ^OR +/- ^ORE degrees ^SENSE',
     :                        BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )

*  Reset the width.  Is there no inquiry function to obtain previous
*  width?
            IF ( PIXEL .OR. .NOT. ISSKY )
     :        CALL MSG_TUNE( 'SZOUT', 100, STATUS )
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
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG6', '    Amplitude   '/
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
            CALL MSG_LOAD( 'KPS1_BFLOG_MSG6E', '    Amplitude   '/
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
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG7', '    Background  '/
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
               CALL MSG_LOAD( 'KPS1_BFLOG_MSG7E', '    Background  '/
     :                        /'    : ^BGD +/- ^BGDE',
     :                        BUF, LBUF, STATUS )
            END IF
            IF ( LOGF ) CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
            CALL MSG_OUTIF( MSG__NORM, ' ', BUF( : LBUF ), STATUS )
         END IF

         IF ( LOGF ) CALL FIO_WRITE( FD, ' ', STATUS )
         CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
      END DO

      CALL AST_ANNUL( FRM2, STATUS )

      END
