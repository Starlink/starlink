      SUBROUTINE KPS1_BFFIL( INDF, IWCS, MAP1, MAP2, RFRM, VAR, NPOS,
     :                       NAXR, NAXIN, INPOS, LOGF, FDL, FIXCON,
     :                       AMPRAT, SLBND, SUBND, FAREA, FITREG,
     :                       REFPOS, REFLAB, NPAR, FPAR, STATUS )
*+
*  Name:
*     KPS1_BFFIL

*  Purpose:
*     Fits to beam positions specified from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFFIL( INDF, IWCS, MAP1, MAP2, RFRM, VAR, NPOS,
*                      NAXR, NAXIN, INPOS, LOGF, FDL, FIXCON,
*                      AMPRAT, SLBND, SUBND, FAREA, FITREG, REFPOS,
*                      REFLAB, NPAR, FPAR, STATUS )

*  Description:
*     This routine finds the generalised Gaussian fits to a batch of
*     image beam features given initial guesses at their positions with
*     optional constraints of some parameters.  The initial
*     beam positions are obtained from a text file or catalogue.
*     The routine also estimates the errors on the fitted parameters,
*     and presents the results to a log file and the screen.  Amongst
*     the results are the rms of the fit, the offset of the primary beam
*     from a reference point, and the polar co-ordinates of secondary
*     beams from the primary beam's location.
*
*     All the co-ordinates are converted to pixel co-ordinates
*     before any fits are made and reported.  This enables the required
*     AST transformations to be applied to all positions in a single
*     call, minimising the time spent in the mapping routines.  This
*     routine should be used in non-interactive modes of BEAMFIT
*     such as "File" or "Catalogue".

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier of the input NDF.
*     IWCS = INTEGER (Given)
*        The FrameSet associated with the NDF.
*     MAP1 = INTEGER (Given)
*        The AST Mapping from the Frame in which the initial guess
*        positions are supplied, to the PIXEL Frame of the NDF.
*     MAP2 = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     RFRM = INTEGER (Given)
*        A pointer to the reporting Frame (i.e. the Frame in which
*        positions are to be reported).
*     VAR = LOGICAL (Given)
*        If TRUE, use variance to weight the fit.
*     NPOS = INTEGER (Given)
*        The number of beam positions to be found.
*     NAXR = INTEGER (Given)
*        The number of axes in the reporting Frame.
*     NAXIN = INTEGER (Given)
*        The number of axes in the Frame in which the initial guess
*        positions are supplied.
*     INPOS( NPOS, NAXIN ) = DOUBLE PRECISION (Given)
*        The beam positions that are one of four options: initial
*        guesses to be fit, fixed locations, or in the case of secondary
*        beam positions separations in absolute or relative polar
*        co-ordinates.  The meaning depends on the settings of the
*        fourth and sixth elements of argument FIXCON, and argument
*        POLPAR.  All should be in the co-ordinate system defined by
*        MAP1 (albeit transformed for polar co-ordinates).
*     LOGF = LOGICAL (Given)
*        Should the results be written to a log file?
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  It is ignored if LOGF is
*        .FALSE.
*     FIXCON( BF__NCON ) = LOGICAL (Given)
*        Flags whether or not to apply constraints.  The elements are
*        the array relate to the following constraints.
*        1 -- Are the beam `source' amplitudes fixed?
*        2 -- Is the background level fixed?
*        3 -- Is the FWHM of the beam fixed?
*        4 -- Are the beam positions fixed at the supplied co-ordinates?
*        5 -- Are the relative amplitudes fixed?
*        6 -- Are the separations to the secondary beam positions fixed?
*        7 -- Is the shape parameter fixed?
*     AMPRAT( BF__MXPOS - 1 ) = REAL (Given)
*        The ratios of the secondary beam `sources' to the first beam.
*        These ratios contrain the fitting provided FIXCON(5) is .TRUE.
*     SLBND( 2 ) = INTEGER (Given)
*        The lower pixel index bounds of the significant axes of the
*        NDF.
*     SUBND( 2 ) = INTEGER (Given)
*        The upper pixel index bounds of the significant axes of the
*        NDF.
*     FAREA = LOGICAL (Given)
*        If .TRUE. then all pixels in the data array are used.  If
*        .FALSE. the region size is defined by argument FITREG.
*     FITREG( 2 ) = INTEGER (Given)
*        The dimensions of the box to use when estimating the Gaussian
*        fit in pixels.  The box is centred around each beam position.
*        each value must be at least 9.  It is only accessed if FAREA is
*        .FALSE.
*     REFPOS( BF__NDIM ) = DOUBLE PRECISION (Given)
*        The reference position measured in the current WCS Frame.  The
*        offset of the primary beam with respect to this points is
*        calculated, reported, and written to an output parameter.
*     REFLAB = CHARACTER * (*) (Given)
*        Label used to describe reference position in the output.   At
*        present it should be either "map centre", if that was used in
*        the absence of a reference position stored with the original
*        dataset; or "sky reference position".  If another value is
*        supplied, "reference position" will be used.
*     NPAR = INTEGER (Given)
*        The maximum number of fit parameters.
*     FPAR( NPAR ) = DOUBLE PRECISION (Given and Returned)
*        On entry this is the fixed fit parameters.  Any free parameters
*        take the value VAL__BADD.  See KPS1_BFFN for a list of the
*        parameters.
*
*        On exit it contains the coefficients of the fit in the PIXEL
*        Frame.   Any supplied non-bad (i.e. fixed) values are
*        unchanged, other than possible transformation of co-ordinates
*        to pixels; but the other elements are returned holding the best
*        value of the corresponding fit parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*      The BF constants used to define array dimensions are located in
*      the include file BF_PAR.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010, 2011 Science & Technology Facilities Council.
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
*     2007 February 28 (MJC):
*        Original version.
*     2007 April 27 (MJC):
*        Added FIXAMP and FIXRAT arguments, and concurrent fitting of
*        multiple Gaussians,
*     2007 May 11 (MJC):
*        Pass constraint flags as an array to shorten the API.
*     2007 May 14 (MJC):
*        Support fixed separations.
*     2007 May 22 (MJC):
*        Call new KPS1_BFCRF to perform co-ordinate conversions of the
*        results.  Tidy up removing superfluous code, and MXPOS and
*        PIXPOS arguments.
*     2007 May 29 (MJC):
*        Make arguments the same type in MAX and MIN function calls.
*     2007 May 31 (MJC):
*        Use new APIs for calculating and reporting polar co-ordinates
*        of secondary features.
*     2007 June 4 (MJC):
*        Second redesign: no longer are offsets supplied from BEAMFIT
*        itself through the OFFSET argument.  Thus locations read from
*        a file or catalogue can be an initial guess or a fixed value,
*        or in the case of secondary beams be separations from the
*        primary.  Removed unused variables.
*     2007 June 8 (MJC):
*        Moved NPAR and FPAR to the end of the non-STATUS arguments (as
*        FPAR is modified.
*     2007 June 15 (MJC):
*        Added REFPOS argument, propagated through to other routines.
*     2007 June 25 (MJC):
*        Added IWCS FrameSet identifier argument and passed it to
*        KPS1_BFCRF.
*     2007 July 9 (MJC):
*        Added REFLAB argument passed to KPS1_BFLOG.
*     2010 July 5 (MJC):
*        Switched to generalised Gaussian fit by the introduction of
*        the shape exponent.
*     2011 May 11 (MJC):
*        Removed no-longer-used arguments MAP3, GOTID, and ID.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'BF_PAR'           ! BEAMFIT constants

*  Global Variables:
      INCLUDE 'BF_COM'           ! Used for communicating with PDA
                                 ! routine
*        NBEAMS = INTEGER (Write)
*           The number of beam positions to fit simultaneously
*        IPWD = INTEGER (Write)
*           Pointer to work space for data values
*        IPWV = INTEGER (Write)
*           Pointer to work space for variance values
*        USEVAR = LOGICAL (Write)
*           Whether or not to use variance to weight the fit.

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER MAP1
      INTEGER MAP2
      INTEGER RFRM
      LOGICAL VAR
      INTEGER NPOS
      INTEGER NAXIN
      INTEGER NAXR
      DOUBLE PRECISION INPOS( NPOS, NAXIN )
      LOGICAL LOGF
      INTEGER FDL
      LOGICAL FIXCON( BF__NCON )
      REAL AMPRAT( BF__MXPOS - 1 )
      INTEGER SLBND( 2 )
      INTEGER SUBND( 2 )
      LOGICAL FAREA
      INTEGER FITREG( 2 )
      DOUBLE PRECISION REFPOS( BF__NDIM )
      CHARACTER*(*) REFLAB
      INTEGER NPAR

*  Arguments Given and Returned:
      DOUBLE PRECISION FPAR( NPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NUMCO              ! Number of graphics co-ordinates
      PARAMETER ( NUMCO = BF__MXPOS * NDF__MXDIM )

*  Local Variables:
      CHARACTER*( NDF__SZFTP ) DTYPE ! Numeric type for results
      DOUBLE PRECISION DX        ! Increment along first axis
      DOUBLE PRECISION DY        ! Increment along second axis
      INTEGER EL                 ! Number of mapped elements
      INTEGER FLBND( BF__NDIM )  ! Fit region lower bounds
      DOUBLE PRECISION FPOS( 2, BF__NDIM ) ! Position in current Frame
      INTEGER FUBND( BF__NDIM )  ! Fit region upper bounds
      INTEGER HBOX( BF__NDIM )   ! Half-sizes of the region
      INTEGER I                  ! Position index
      CHARACTER*( NDF__SZTYP ) ITYPE ! Data type for processing(dummy)
      INTEGER J                  ! Axis index
      INTEGER NDFS               ! Identifier for NDF section
      DOUBLE PRECISION OFFVAR    ! Offset variance
      LOGICAL OK                 ! Is this position OK?
      DOUBLE PRECISION POFSET( BF__MXPOS, BF__NDIM ) ! Pixel offsets
      DOUBLE PRECISION POLAR( 2, BF__MXPOS ) ! Polar co-ordinates
      DOUBLE PRECISION POLSIG( 2, BF__MXPOS ) ! Polar co-ordinate errors
      DOUBLE PRECISION POS( 2, BF__NDIM ) ! Pixel position
      DOUBLE PRECISION PIXPOS( BF__MXPOS, BF__NDIM ) ! Pixel position
      DOUBLE PRECISION PRIPOS( BF__NDIM )! Primary pos, current Frame
      DOUBLE PRECISION RMS       ! Root mean squared deviation to fit
      DOUBLE PRECISION REFOFF( 2 ) ! Offset & error from reference point
      DOUBLE PRECISION RP( BF__NCOEF, BF__MXPOS ) ! Fit coefficients
                                 ! reporting Frame
      DOUBLE PRECISION RSIGMA( BF__NCOEF, BF__MXPOS ) ! Fit errors,
                                 ! reporting Frame
      DOUBLE PRECISION SIGMA( BF__NCOEF, BF__MXPOS ) ! Fit errors,
                                 ! PIXEL Frame
      LOGICAL VERB               ! Flush errors instead of annulling them?
      INTEGER WAX                ! Index of axis measuring fixed FWHMs

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Store the number of beams in COMMON.
      NBEAMS = NPOS

*  Check the supplied Mappings have the required transformations.
      IF ( .NOT. AST_GETL( MAP1, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFFIL_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )
      END IF

      IF ( .NOT. AST_GETL( MAP2, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFFIL_ERR2','The Mapping required '//
     :                 'to map pixel positions into the Current '//
     :                 'co-ordinate Frame is not defined.', STATUS )

      ELSE IF ( ( FIXCON( 3 ) ) .AND.
     :          .NOT. AST_GETL( MAP2, 'TRANINVERSE', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_BFFIL_MSG3','The Mapping required to '//
     :                 'convert distances expressed in the Current '//
     :                 'co-ordinate Frame into pixels co-ordinates '//
     :                 'is not defined.', STATUS )
         CALL MSG_OUT( 'KPS1_BFFIL_MSG4','Fitting will include '//
     :                 'widths of the beam features.', STATUS )

         FIXCON( 3 ) = .FALSE.
      END IF

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

*  Transform the supplied positions to the PIXEL Frame of the NDF.
      CALL AST_TRANN( MAP1, NPOS, NAXIN, NPOS, INPOS, .TRUE., BF__NDIM,
     :                BF__MXPOS, PIXPOS, STATUS )

*  Map the data and variance arrays from the whole NDF.  Copy the
*  use-variance flag to COMMON.
      USEVAR = VAR
      IF ( FAREA ) THEN
         CALL NDF_MAP( INDF, 'Data', '_DOUBLE', 'READ', IPWD, EL,
     :                 STATUS )

*  Map the variance array.  Otherwise create a valid pointer.
         IF ( VAR ) THEN
            CALL NDF_MAP( INDF, 'Variance', '_DOUBLE', 'READ', IPWV, EL,
     :                    STATUS )
         ELSE
            IPWV = IPWD
         END IF
      END IF

*  Determine the data type to use to report the amplitudes and background
*  levels.
      CALL NDF_MTYPE( '_INTEGER,_REAL,_DOUBLE', INDF, INDF, 'Data',
     :                ITYPE, DTYPE, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Fixed separations.
*  ==================

*  Derive pixel offsets.
      IF ( FIXCON( 6 ) .AND. NPOS .GT. 1 ) THEN
         DO I = 2, NPOS
            DO J = 1, BF__NDIM
               POFSET( I - 1, J ) = PIXPOS( I, J ) - PIXPOS( 1, J )
            END DO
         END DO
      END IF

*  Convert the supplied widths to pixels.
*  ======================================

*  We need to specify which axis to use for the widths.  By convention
*  this is the latitude axis of a SkyFrame or the first axis otherwise.
      IF ( AST_ISASKYFRAME( RFRM, STATUS ) ) THEN
         WAX = AST_GETI( RFRM, 'LATAXIS', STATUS )
      ELSE
         WAX = 1
      END IF

*  First convert the PIXEL co-ordinates of the primary beam centre to
*  the reporting Frame.
      CALL AST_TRANN( MAP2, 1, 2, BF__MXPOS, PIXPOS, .TRUE., BF__NDIM,
     :                2, FPOS, STATUS )

*  The supplied fixed widths are in the current Frame's co-ordinates.
*  For fitting we need these to be in pixels.
      IF ( FPAR( 3 ) .NE. VAL__BADD ) THEN

*  Transform the centre and centre plus width from the PIXEL Frame
*  of the NDF to the reporting Frame.
         FPOS( 2, 1 ) = FPOS( 1, 1 )
         FPOS( 2, 2 ) = FPOS( 1, 2 )
         FPOS( 2, WAX ) = FPOS( 2, WAX ) + FPAR( 3 )
         CALL AST_TRANN( MAP2, 2, 2, 2, FPOS, .FALSE., BF__NDIM, 2, POS,
     :                   STATUS )

*  Assign the width in pixels to all the beam positions.
         FPAR( 3 ) = ABS( POS( 1, WAX ) - POS( 2, WAX ) )
         IF ( NPOS .GT. 1 ) THEN
            DO I = 2, NPOS
               FPAR( 3 + ( I - 1 ) * BF__NCOEF ) = FPAR( 3 )
            END DO
         END IF
      END IF

*  Repeat for the minor-axis width.
      IF ( FPAR( 4 ) .NE. VAL__BADD ) THEN
         FPOS( 2, 1 ) = FPOS( 1, 1 )
         FPOS( 2, 2 ) = FPOS( 1, 2 )
         FPOS( 2, WAX ) = FPOS( 2, WAX ) + FPAR( 4 )
         CALL AST_TRANN( MAP2, 2, 2, 2, FPOS, .FALSE., BF__NDIM, 2, POS,
     :                   STATUS )

*  Assign the width in pixels to all the beam positions.
         FPAR( 4 ) = ABS( POS( 1, WAX ) - POS( 2, WAX ) )
         IF ( NPOS .GT. 1 ) THEN
            DO I = 2, NPOS
               FPAR( 4 + ( I - 1 ) * BF__NCOEF ) = FPAR( 4 )
            END DO
         END IF
      END IF

*  Define the half-box size.
      IF ( FAREA ) THEN
         DO J = 1, BF__NDIM
            HBOX( J ) = ( SUBND( J ) - SLBND( J ) ) / 2
         END DO
      ELSE
         DO J = 1, BF__NDIM
            HBOX( J ) = FITREG( J ) / 2
         END DO
      END IF

*  Find the beam parameters.
*  =========================

*  Check for bad axis values.
      OK = .TRUE.
      DO I = 1, NPOS
         DO J = 1, BF__NDIM
            IF ( PIXPOS( I, J ) .EQ. AST__BAD ) THEN
               OK = .FALSE.
            END IF
         END DO
      END DO

*  If the initial position is good, fit to the beam positions.
      IF ( OK ) THEN

*  Specify the pixel bounds around the beam.
         IF ( FAREA ) THEN
            DO J = 1, BF__NDIM
               FLBND( J ) = SLBND( J )
               FUBND( J ) = SUBND( J )
            END DO
         ELSE
            DO J = 1, BF__NDIM
               FLBND( J ) = MAX( NINT( PIXPOS( 1, J ) + 0.5D0 ) -
     :                           HBOX( J ), SLBND( J ) )
               FUBND( J ) = MIN( NINT( PIXPOS( 1, J ) + 0.5D0 ) +
     :                           HBOX( J ), SUBND( J ) )
            END DO

*  We need an NDF section corresponding to the chosen region.
            CALL NDF_SECT( INDF, BF__NDIM, FLBND, FUBND, NDFS, STATUS )

*  Map the section of the data array around the beam.
            CALL NDF_MAP( NDFS, 'Data', '_DOUBLE', 'READ', IPWD, EL,
     :                    STATUS )

*  Map the variance array.  Otherwise create a valid pointer.
            IF ( VAR ) THEN
               CALL NDF_MAP( NDFS, 'Variance', '_DOUBLE', 'READ',
     :                       IPWV, EL, STATUS )
            ELSE
               IPWV = IPWD
            END IF
         END IF

*  Do the fitting and convert positions to reporting Frame.
*  ========================================================

*  The fitted position is returned in pixel co-ordinates.  Positional
*  coefficients are also in pixels.
         CALL KPS1_BFFT( PIXPOS, FLBND, FUBND, FIXCON, AMPRAT,
     :                   POFSET, NPAR, FPAR, SIGMA, RMS, STATUS )

*  Free resources.
         IF ( FAREA ) THEN
            IF ( VAR ) CALL NDF_UNMAP( INDF, 'Variance', STATUS )
            CALL NDF_UNMAP( INDF, 'Data', STATUS )
         ELSE
            CALL NDF_ANNUL( NDFS, STATUS )
         END IF

*  If a fit could not be found...
         OK = STATUS .EQ. SAI__OK

*  Now report the results.
*  ======================
         IF ( OK ) THEN

*  Convert the pixel coefficients to the reporting Frame, also
*  changing the widths from standard deviations to FWHMs.
            CALL KPS1_BFCRF( MAP2, IWCS, NAXR, NPOS, BF__NCOEF, FPAR,
     :                       SIGMA, RP, RSIGMA, POLAR, POLSIG, STATUS )

*  Find the offset of the primary beam with respect to the reference
*  point, now both locations are konwn in the reporting Frame.
            DO J = 1, BF__NDIM
               PRIPOS( J ) = RP( J, 1 )
            END DO
            REFOFF( 1 ) = AST_DISTANCE( RFRM, REFPOS, PRIPOS, STATUS )

*  Assume that the reference position is exactly known, and the only
*  variance comes from the spatial errors of the fitted primary beam.
            DX = ( REFPOS( 1 ) - PRIPOS( 1 ) ) / REFOFF( 1 )
            DY = ( REFPOS( 2 ) - PRIPOS( 2 ) ) / REFOFF( 1 )
            OFFVAR = DX * DX * RSIGMA( 1, 1 ) * RSIGMA( 1, 1 ) +
     :               DY * DY * RSIGMA( 2, 1 ) * RSIGMA( 2, 1 )

            IF ( OFFVAR .GT. 0.0D0 ) THEN
               REFOFF( 2 ) = SQRT( OFFVAR )
            ELSE
               REFOFF( 2 ) = VAL__BADD
            END IF

*  Log the results and residuals if required.
            CALL KPS1_BFLOG( LOGF, FDL, .FALSE., MAP2, RFRM,
     :                       NPOS, BF__NCOEF, RP, RSIGMA, REFOFF,
     :                       REFLAB, POLAR, POLSIG, RMS, DTYPE, STATUS )

*  Write primary beam's fit to output parameters.
            CALL KPS1_BFOP( RFRM, NAXR, NPAR, RP, RSIGMA,
     :                      NPOS, REFOFF, POLAR, POLSIG, RMS, STATUS )

         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'KPS1_BFFIL_MSG5',
     :                      'No fit to the beam.', STATUS )
         END IF

      END IF

*  A final blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF ( LOGF ) CALL FIO_WRITE( FDL, ' ', STATUS )

*  Tidy up.
*  =======
 999  CONTINUE

      END
