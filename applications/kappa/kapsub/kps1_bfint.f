      SUBROUTINE KPS1_BFINT( INDF, IWCS, IPLOT, MAP1, MAP2, RFRM,
     :                       VAR, NPOS, POLPAR, PARAM, CURSOR, MARK,
     :                       MARKER, NAXR, NAXIN, LOGF, FDL, FIXCON,
     *                       AMPRAT, SLBND, SUBND, FAREA, FITREG,
     :                       REFPOS, REFLAB, PIXSCR, NPAR, FPAR,
     :                       STATUS )
*+
*  Name:
*     KPS1_BFINT

*  Purpose:
*     Fits to beam positions specified interactively.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFINT( INDF, IWCS, IPLOT, MAP1, MAP2, RFRM, VAR,
*                      NPOS, POLPAR, PARAM, CURSOR, MARK, MARKER, NAXR,
*                      NAXIN, LOGF, FDL, FIXCON, AMPRAT, SLBND, SUBND,
*                      FAREA, FITREG, REFPOS, REFLAB, PIXSCR, NPAR,
*                      FPAR, STATUS )

*  Description:
*     This routine finds the generalised Gaussian fits to a batch of
*     image beam features given initial guesses at their positions with
*     optional constraints of some parameters.  The initial beam
*     positions are identified interatively by the user, either with a
*     cursor or through an environment parameter.  These may also be
*     interpreted as fixed positions or specify secondary-beam
*     separations in axis or polar co-ordinates depending on the
*     selected constraints.  The routine also estimates the errors on
*     the fitted parameters, and presents the results to a log file and
*     the screen.  Amongst the results are the rms of the fit, the 
*     offset of the primary beam from a reference point, and the polar
*     co-ordinates of secondary beams from the primary beam's location.
*
*     All the co-ordinates are converted to pixel co-ordinates before
*     any fits are made and reported.  This enables the required AST
*     transformations to be applied to all positions in a single call,
*     minimising the time spent in the mapping routines.

*     This routine should be used in non-interactive modes of BEAMFIT
*     such as "Interface" or "Cursor".

*  Arguments:
*     INDF = INTEGER (Given)
*        The input NDF.
*     IWCS = INTEGER (Given)
*        The FrameSet associated with the NDF.
*     IPLOT = INTEGER (Given)
*        The identfier of the plot obtained from graphics database.
*        It is only accessed in Cursor mode when Mark='ELLIPSE'.
*     MAP1 = INTEGER (Given)
*        The AST Mapping from the Frame in which the initial guess
*        positions are supplied, to the PIXEL Frame of the NDF.
*     MAP2 = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     RFRM = INTEGER (Given)
*        A pointer to the reporting Frame.
*     VAR = LOGICAL (Given)
*        If TRUE, use variance to weight the fit.
*     NPOS = INTEGER (Given)
*        The number of beam positions required.
*     POLPAR = LOGICAL (Given)
*        If .TRUE., the values supplied for parameters beginning with
*        argument PARAM are interpreted as a radial distance followed by
*        a position angle with respecxt to the primary beam's position.
*        If .FALSE., the values are regartded as spatial co-ordinates
*        in the current Frame, RFRM.
*     PARAM = CHARACTER * ( * ) (Given)
*        The root name of the parameter to use when acquiring initial
*        positions in Interface mode.  It is the parameter name for
*        primary-beam position.  The parameter names for secondary
*        beam positions have a numerical suffix equal to the number of
*        beam positions supplied already.  Thus the the first secondary
*        beam is %PARAM//1.  Multiple parameter names allows easier
*        command-line processing.
*     CURSOR = LOGICAL (Given)
*        Get initial positions using the cursor? If not, get them using
*        the parameter given by PARAM.
*     MARK = CHARACTER * ( * ) (Given)
*        What positions are to be marked?  Can be "INITIAL", "FIT",
*        'ELLIPSE' or "NONE".
*     MARKER = INTEGER (Given)
*        The PGPLOT number for the marker type to mark the positions
*        specified by MARK.
*     NAXR = INTEGER (Given)
*        The number of axes in the reporting Frame.
*     NAXIN = INTEGER (Given)
*        The number of axes in the Frame in which the initial guess
*        positions are supplied.
*     LOGF = LOGICAL (Given)
*        Should the results be written to a log file?
*     FDL = INTEGER (Given)
*        The file descriptor for the log file. Ignored if LOGF is
*        .FALSE.
*     FIXCON( BF__NCON ) = LOGICAL (Given & Returned)
*        Flags whether or not to apply constraints.  The elements are
*        the array relate to the following constraints.
*        1 -- Are the beam `source' amplitudes fixed?
*        2 -- Is the background level fixed?
*        3 -- Is the FWHM of the beam fixed?
*        4 -- Are the beam positions fixed at the supplied co-ordinates?
*        5 -- Are the relative amplitudes fixed?
*        6 -- Are the separations to the secondary beam positions fixed?
*        7 -- Is the shape parameter fixed?
*        8 -- Is the beam fixed to be circular?
*        9 -- Is the orientation of the Gaussian fixed?
*        
*        Options 3 and 6 will set to .FALSE. if either is .TRUE. on
*        entry and if MAP2 does not have the inverse transformation.
*     AMPRAT( BF__MXPOS - 1 ) = REAL (Given)
*        The ratios of the secondary beam 'sources' to the first beam.
*        These ratios constrain the fitting provided FIXCON(5) is .TRUE.
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
*        in pixels.  The box is centred around each beam position.  Each
*        value must be at least 9.  It is only accessed if FAREA is
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
*     PIXSCR = DOUBLE PRECISION (Given)
*        Pixel aspect ratio.   Normally set to 1.
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
*     Copyright (C) 2009-2011, 2015 Science and Technology Facilities
*     Council.
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
*     2007 March 1 (MJC):
*        Original version.
*     2007 April 27 (MJC):
*        Added FIXAMP and FIXRAT arguments, and concurrent fitting of
*        multiple Gaussians.
*     2007 May 11 (MJC):
*        Pass constraint flags as an array to shorten the API.
*     2007 May 14 (MJC):
*        Support fixed separations.
*     2007 May 22 (MJC):
*        Generate a sequence of parameter names for the initial values.
*        Call new KPS1_BFCRF to perform co-ordinate conversions of the
*        results.  Tidy up removing superfluous code and the OUT
*        argument.
*     2007 May 29 (MJC):
*        Make arguments the same type in MAX and MIN function calls.
*     2007 May 31 (MJC):
*        Use new APIs for calculating and reporting polar co-ordinates
*        of secondary features.  Added POLPAR argument.
*     2007 June 4 (MJC):
*        Second redesign: no longer are offsets supplied from BEAMFIT
*        itself through the OFFSET argument, and separate primary
*        parameter from the secondary to help with the explanation and
*        allowed values for the respective parameters.  Thus locations
*        defined by cursor or parameter can be an initial guess or a
*        fixed value, or in the case of secondary beams be separations
*        from the primary.  More tidying.
*     2007 June 8 (MJC):
*        Moved NPAR and FPAR to the end of the non-STATUS arguments (as
*        FPAR is modified.
*     2007 June 15 (MJC):
*        Added REFPOS argument, propagated through to other routines.
*     2007 June 18 (MJC):
*        Added IPLOT argument and ellipse plotting.  Moved results
*        graphics to new routine.
*     2007 June 25 (MJC):
*        Passed FrameSet identifier to KPS1_BFCRF.
*     2007 July 9 (MJC):
*        Added REFLAB argument passed to KPS1_BFLOG.
*     2009 January 31 (MJC):
*        Check that the input co-ordinates lie with the array bounds.
*     2010 July 5 (MJC):
*        Switched to generalised Gaussian fit by the introduction of
*        the shape exponent.
*     2011 May 11 (MJC):
*        Removed no-longer-used argument MAP3.
*     2015 July 16 (MJC):
*        Add PIXSCR argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_PAR'          ! Parameter-system constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'NDF_PAR'          ! NDF constants
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
*        PIXAR = DOUBLE PRECISION (Write)
*           Pixel aspect ratio

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER IPLOT
      INTEGER MAP1
      INTEGER MAP2
      INTEGER RFRM
      LOGICAL VAR
      INTEGER NPOS
      LOGICAL POLPAR
      CHARACTER PARAM*( * )
      LOGICAL CURSOR
      CHARACTER MARK*( * )
      INTEGER MARKER
      INTEGER NAXR
      INTEGER NAXIN
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
      DOUBLE PRECISION PIXSCR
      INTEGER NPAR

*  Arguments Given and Returned:
      DOUBLE PRECISION FPAR( NPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER*2 CHR_NTH        ! Ordinal string

*  Local Constants:
      INTEGER NUMCO              ! Number of graphics co-ordinates
      PARAMETER ( NUMCO = BF__MXPOS * NDF__MXDIM )

*  Local Variables:
      CHARACTER*30 AMES(2)       ! Instructions on cursor use
      INTEGER ACT                ! Cursor action index
      DOUBLE PRECISION BC( BF__NDIM ) ! Base co-ordinates
      CHARACTER*( NDF__SZFTP ) DTYPE ! Numeric type for results
      DOUBLE PRECISION DX        ! Increment along first axis
      DOUBLE PRECISION DY        ! Increment along second axis
      INTEGER EL                 ! Number of mapped elements
      INTEGER FLBND( BF__NDIM )  ! Fit region lower bounds
      DOUBLE PRECISION FPOS( 2, BF__NDIM ) ! Position in current Frame
      INTEGER FUBND( BF__NDIM )  ! Fit region upper bounds
      INTEGER HBOX( BF__NDIM )   ! Half-sizes of the region
      INTEGER I                  ! Position index
      INTEGER IMARK              ! Marker to use when marking initial
                                 ! positions
      LOGICAL INFO               ! Display instructions on cursor use?
      DOUBLE PRECISION INPOL( 2 ) ! Pole co-ordinates
      CHARACTER*( NDF__SZTYP ) ITYPE ! Data type for processing(dummy)
      INTEGER J                  ! Axis index
      INTEGER K                  ! Index
      CHARACTER*32 MESS          ! Message in cursor mode
      LOGICAL MORE               ! Process another position?
      INTEGER NBPS               ! Number of positions specified so far
      INTEGER NDFS               ! Identifier for NDF section
      INTEGER NCP                ! Number of characters in parameter
                                 ! name
      INTEGER NP                 ! Number of cursor position supplied
      DOUBLE PRECISION OFFVAR    ! Offset variance
      LOGICAL OK                 ! Is this position OK?
      CHARACTER*( PAR__SZNAM + 1 ) PARNAM ! Parameter name for the
                                 ! current initial beam position
      DOUBLE PRECISION PIXPOS( BF__MXPOS, BF__NDIM ) ! Pixel position
      DOUBLE PRECISION POFSET( BF__MXPOS - 1, BF__NDIM ) ! Pixel offsets
      DOUBLE PRECISION POLAR( 2, BF__MXPOS ) ! Polar co-ordinates
      DOUBLE PRECISION POLSIG( 2, BF__MXPOS ) ! Polar co-ordinate errors
      DOUBLE PRECISION POS( 2, BF__NDIM ) ! Pixel position
      DOUBLE PRECISION PRIPOS( BF__NDIM )! Primary pos, current Frame
      DOUBLE PRECISION REFOFF( 2 ) ! Offset & error from reference point
      DOUBLE PRECISION RMS       ! Root mean squared deviation to fit
      DOUBLE PRECISION RP( BF__NCOEF, BF__MXPOS ) ! Fit coeeficients
                                 ! reporting Frame
      DOUBLE PRECISION RSIGMA( BF__NCOEF, BF__MXPOS ) ! Fit errors,
                                 ! reporting Frame
      DOUBLE PRECISION SIGMA( BF__NCOEF, BF__MXPOS ) ! Fit errors,
                                 ! PIXEL Frame
      LOGICAL SINGLE             ! Process only a single position?
      INTEGER STATE              ! State of the supplied parameter
      INTEGER WAX                ! Index of axis measuring fixed FWHMs
      REAL X1, Y1                ! Co-ords. of upper-right of picture
      REAL X2, Y2                ! Co-ords. of lower-left of picture
      REAL XIN, YIN              ! Co-ords. of centre of picture

*  Local Data:
      DOUBLE PRECISION INCO( NDF__MXDIM ) ! Parameter-supplied position
      DATA INCO / NDF__MXDIM * AST__BAD /
      DOUBLE PRECISION INPOS( BF__MXPOS, NDF__MXDIM ) ! Supplied
      DATA INPOS / NUMCO * AST__BAD /                 ! positions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Store the number of beams in COMMON.
      NBEAMS = NPOS

*  Check the supplied Mappings have the required transformations.
      IF ( .NOT. AST_GETL( MAP1, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFINT_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )

      ELSE IF ( CURSOR .AND. ( MARK( 1 : 1 ) .EQ. 'F' .OR.
     :                         MARK( 1 : 1 ) .EQ. 'E' ) .AND.
     :          .NOT. AST_GETL( MAP1, 'TRANINVERSE', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_BFINT_MSG1','The Mapping required '//
     :                 'to map the beam positions into the '//
     :                 'graphics co-ordinate Frame is not defined.',
     :                 STATUS )
         CALL MSG_OUT( 'KPS1_BFINT_MSG2','Beam positions will '//
     :                 'not be marked!', STATUS )
         MARK = 'None'

      END IF

      IF ( .NOT. AST_GETL( MAP2, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFINT_ERR2','The Mapping required '//
     :                 'to map pixel positions into the Current '//
     :                 'co-ordinate Frame is not defined.', STATUS )

      ELSE IF ( ( FIXCON( 3 ) ) .AND.
     :          .NOT. AST_GETL( MAP2, 'TRANINVERSE', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_BFINT_MSG3','The Mapping required to '//
     :                 'convert distances expressed in the Current '//
     :                 'co-ordinate Frame into pixels co-ordinates '//
     :                 'is not defined.', STATUS )
         CALL MSG_OUT( 'KPS1_BFINT_MSG4','Fitting will include '//
     :                 'widths of the beam features.', STATUS )

         FIXCON( 3 ) = .FALSE.
      END IF

      PIXAR = PIXSCR

*  Obtain the data and its type and ROI.
*  =====================================

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

*  Note if there is only one beam position to obtain.
      SINGLE = NPOS .EQ. 1

*  Cursor-mode preliminaries.
*  ==========================

*  The cursor will be positioned initially at the centre of the DATA
*  picture.  Get the bounds the PGPLOT window, and store the mid
*  position if we are in cursor mode.
      IF ( CURSOR ) THEN
         CALL PGQWIN( X1, X2, Y1, Y2 )
         XIN = 0.5 * ( X1 + X2 )
         YIN = 0.5 * ( Y1 + Y2 )

*  Indicate that information describing the use of the pointer and
*  buttons should be given before getting the first position.
         INFO = .TRUE.

*  Store the actions which can be performed using the mouse.
         AMES( 1 ) = 'Select an image feature'
         AMES( 2 ) = 'Exit'
      END IF

*  Store the input marker to use.
      IMARK = -999
      IF ( MARK( 1 : 1 ) .EQ. 'I' ) IMARK = MARKER

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop to find the beam parameters and display the beam position.
*  ===============================================================

*  Initialise the number of positions obtained so far.
      NBPS = 0

      MORE = .TRUE.
      OK = .TRUE.
      DO WHILE ( MORE .AND. NBPS .LT. NPOS .AND. STATUS .EQ. SAI__OK )

*  Get the initial position...

*  Cursor mode
*  -----------
         IF ( CURSOR ) THEN

            IF ( NBPS .GT. 0 ) THEN
               CALL MSG_SETI( 'BP', NBPS + 1 )
               CALL MSG_SETC( 'ORD', CHR_NTH( NBPS + 1, STATUS ) )
               CALL MSG_OUT( ' ', '  Select secondary beam for '/
     :                       /'^BP^ORD beam position', STATUS )
            END IF

            IF ( NPOS .EQ. 1 ) THEN
               MESS = 'select the beam position'
            ELSE
               MESS = 'select the primary beam position'
            END IF

*  Get a position using the cursor, in PGPLOT world co-ordinates.  This
*  corresponds to the Base (i.e. GRAPHICS) Frame of the Plot
*  (millimetres from the bottom-left corner of the view surface).  The
*  positions that may be selected are restricted to the current picture.
            CALL KPG1_PGCUR( INFO, MESS, 2, AMES,
     :                       ' .', X1, X2, Y1, Y2, 0, XIN, YIN, 1, 0,
     :                       0, 0, IMARK, AST__NULL, XIN, YIN, ACT, NP,
     :                       STATUS )

*  Look out for the abort, i.e. the number of points is zero.
            IF ( NP .EQ. 0 ) THEN
               MORE = .FALSE.

*  If a position was supplied, convert it to double precision.
            ELSE
               INPOS( NBPS + 1, 1 ) = DBLE( XIN )
               INPOS( NBPS + 1, 2 ) = DBLE( YIN )
            END IF

*  Indicate that we do not need to see the cursor instructions again.
            INFO = .FALSE.

*  Interactive mode
*  ----------------

*  Just get a position using the specified parameter.  The name of the
*  parameter is incremented for each beam position to make them
*  distinct for command-line usage.
         ELSE

*  Create separate parameter names for each beam position.
            IF ( NBPS .EQ. 0 ) THEN
               PARNAM = PARAM

            ELSE
               PARNAM = ' '
               NCP = 0
               CALL CHR_APPND( PARAM, PARNAM, NCP )
               CALL CHR_PUTI( NBPS + 1, PARNAM, NCP )
            END IF

*  Was the parameter supplied on the command line?
            CALL LPG_STATE( PARNAM, STATE, STATUS )

*  Polar co-ordinates may only be given for secondary beams as the
*  pole is at the centre of the primary beam.
            IF ( POLPAR .AND. NBPS .NE. 0 ) THEN
               INPOL( 1 ) = INPOS( 1, 1 )
               INPOL( 2 ) = INPOS( 1, 2 )
               CALL KPG1_GTPLR( PARNAM, IWCS, .FALSE., INPOL, 90.0D0,
     :                          INCO, BC, STATUS )

            ELSE
               CALL KPG1_GTPOS( PARNAM, RFRM, .FALSE., INCO, BC,
     :                          STATUS )
            END IF

*  If an abort value was supplied, do not annul the error but indicate that
*  the loop should be left.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Assume spatial axes are first two dimensions.
            INPOS( NBPS + 1, 1 ) = INCO( 1 )
            INPOS( NBPS + 1, 2 ) = INCO( 2 )

         END IF

*  Break out of the loop if no position was obtained.
         IF ( .NOT. MORE ) GO TO 10

*  Increment the number of beam positions obtained.
         NBPS = NBPS + 1

*  Transform the supplied position to the PIXEL Frame of the NDF.
         CALL AST_TRANN( MAP1, 1, NAXIN, BF__MXPOS, INPOS( NBPS, 1 ),
     :                   .TRUE., BF__NDIM, BF__MXPOS,
     :                   PIXPOS( NBPS, 1 ), STATUS )

*  Copy the initial PIXEL position into a single-precision array,
*  checking for bad axis values.
         DO J = 1, BF__NDIM
            IF ( PIXPOS( NBPS, J ) .EQ. AST__BAD .OR.
     :           PIXPOS( NBPS, J ) .LT. SLBND( J ) .OR.
     :           PIXPOS( NBPS, J ) .GT. SUBND( J ) ) THEN
               OK = .FALSE.
            END IF
         END DO

*  Check for defective input co-ordinates.
         IF ( .NOT. OK ) THEN
            CALL MSG_SETD( 'CX', INCO( 1 ) )
            CALL MSG_SETD( 'CY', INCO( 2 ) )

*  Report the error and exit for command-line users.
            IF ( STATE .EQ. PAR__ACTIVE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_BFINT_ERR3', 'The supplied '/
     :                       /'co-ordinates (^CX,^CY) convert to bad '/
     :                       /'PIXEL co-ordinates, or lie outside the '/
     :                       /'array bounds.', STATUS )
               IF ( .NOT. CURSOR ) THEN
                  CALL ERR_REP( 'KPS1_BFINT_ERR3A', 'Check that you '/
     :                           /'are using the correct chosen '/
     :                           /'system.', STATUS )
               END IF
               GO TO 999

            ELSE

*  Give the user another chance at a prompt to supply valid
*  co-ordinates.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_BFINT_MSG5', 'The supplied '/
     :                       /'co-ordinates (^CX,^CY) convert to bad '/
     :                       /'PIXEL co-ordinates, or lie outside the '/
     :                       /'array bounds.', STATUS )
               IF ( .NOT. CURSOR ) THEN
                  CALL ERR_REP( 'KPS1_BFINT_MSG5A', 'Check that you '/
     :                           /'are using the correct chosen '/
     :                           /'system by entering : before you '/
     :                           /'try again.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               END IF

*  Reset allowing the using to have another attempt.
               CALL PAR_CANCL( PARNAM, STATUS )
               NBPS = NBPS - 1
               OK = .TRUE.
            END IF
         END IF
      END DO


*  Fixed separations.
*  ==================

*  Add the fixed offsets to the primary beam position.
      IF ( NPOS .GT. 1 .AND. FIXCON( 6 ) ) THEN

*  Derive the offset beam positions in the PIXEL Frame of the NDF.
         DO I = 1, NPOS - 1
            DO J = 1, BF__NDIM
               POFSET( I, J ) = PIXPOS( I + 1, J ) - PIXPOS( 1, J )
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

*  Map the data and variance of the selected ROI.
*  ==============================================

*  The initial position is good, find the beam position.

*  Specify the pixel bounds around the beam.
       IF ( FAREA ) THEN
         DO J = 1, BF__NDIM
            FLBND( J ) = SLBND( J )
            FUBND( J ) = SUBND( J )
         END DO
      ELSE
         DO J = 1, BF__NDIM
            FLBND( J ) = MAX( NINT( PIXPOS( 1, J ) + 0.5D0 )
     :                        - HBOX( J ), SLBND( J ) )
            FUBND( J ) = MIN( NINT( PIXPOS( 1, J ) + 0.5D0 )
     :                        + HBOX( J ), SUBND( J ) )
         END DO

*  We need an NDF section corresponding to the chosen region.
         CALL NDF_SECT( INDF, BF__NDIM, FLBND, FUBND, NDFS, STATUS )

*  Map the section of the data array around the beam.
         CALL NDF_MAP( NDFS, 'Data', '_DOUBLE', 'READ', IPWD, EL,
     :                 STATUS )

*  Map the variance array.  Otherwise create a valid pointer.
         IF ( VAR ) THEN
            CALL NDF_MAP( NDFS, 'Variance', '_DOUBLE', 'READ',
     :                    IPWV, EL, STATUS )
         ELSE
            IPWV = IPWD
         END IF
      END IF

*  Do the fitting and convert positions to reporting Frame.
*  ========================================================

*  The fitted position is returned in pixel co-ordinates.
      CALL KPS1_BFFT( PIXPOS, FLBND, FUBND, FIXCON, AMPRAT,
     :                POFSET, NPAR, FPAR, SIGMA, RMS, STATUS )

*  Free resources.
      IF ( FAREA ) THEN
         IF ( VAR ) CALL NDF_UNMAP( INDF, 'Variance', STATUS )
         CALL NDF_UNMAP( INDF, 'Data', STATUS )
      ELSE
         CALL NDF_ANNUL( NDFS, STATUS )
      END IF

*  If a fit could not be found...
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If a good fit was found, transform the position of the centre of the
*  beam to the reporting Frame.  The positions are the first and second
*  coefficients for each beam.
         DO I = 1, NPOS
            DO J = 1, BF__NDIM
               K = J + ( I - 1 ) * BF__NCOEF
               IF ( FPAR( K ) .NE. VAL__BADD ) THEN
                  PIXPOS( I, J ) = FPAR( K )
               ELSE
                  PIXPOS( I, J ) = AST__BAD
               END IF
            END DO
         END DO

*  Now report the results.
*  =======================

*  Indicate the location, size, and shape of the beam
*  --------------------------------------------------

*  The selection of what appears depends on MARK and MARKER.
         IF ( CURSOR ) THEN
             CALL KPS1_BFPRE( IPLOT, MAP1, NPOS, MARK, MARKER,
     :                        NPAR, FPAR, STATUS )
         END IF

*  Convert the results to the reporting Frame.
*  -------------------------------------------

*  Convert the pixel coefficients to the reporting Frame, also
*  changing the widths from standard deviations to FWHMs.
         CALL KPS1_BFCRF( MAP2, IWCS, NAXR, NPOS, BF__NCOEF, FPAR,
     :                    SIGMA, RP, RSIGMA, POLAR, POLSIG, STATUS )

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
     :           DY * DY * RSIGMA( 2, 1 ) * RSIGMA( 2, 1 )

         IF ( OFFVAR .GT. 0.0D0 ) THEN
            REFOFF( 2 ) = SQRT( OFFVAR )
         ELSE
            REFOFF( 2 ) = VAL__BADD
         END IF

*  Output
*  ------

*  Log the results and residuals if required.
         CALL KPS1_BFLOG( LOGF, FDL, .FALSE., MAP2, RFRM,
     :                    NPOS, BF__NCOEF, RP, RSIGMA, REFOFF, REFLAB,
     :                    POLAR, POLSIG, RMS, DTYPE, STATUS )

*  Write primary beam's fit to output parameters.
         CALL KPS1_BFOP( RFRM, NAXR, NPAR, RP, RSIGMA,
     :                   NPOS, REFOFF, POLAR, POLSIG, RMS, STATUS )

      ELSE
         CALL MSG_OUTIF( MSG__NORM, 'KPS1_BFINT_MSG5',
     :                   'No fit to the beam.', STATUS )
      END IF

*  Arrive here once all positions have been processed.
 10   CONTINUE

*  A final blank line.
      CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
      IF ( LOGF ) CALL FIO_WRITE( FDL, ' ', STATUS )

*  Tidy up.
*  =======
 999  CONTINUE

      END
