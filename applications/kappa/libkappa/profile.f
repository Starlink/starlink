      SUBROUTINE PROFILE( STATUS )
*+
*  Name:
*     PROFILE

*  Purpose:
*     Creates a 1-dimensional profile through an N-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROFILE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application samples an N-dimensional NDF at a set of
*     positions, producing a one-dimensional output NDF containing the
*     sample values.  Nearest-neighbour interpolation is used.
*
*     The samples can be placed at specified positions within the input
*     NDF, or can be spaced evenly along a poly-line joining a set of
*     vertices (see parameter MODE).  The positions of the samples may
*     be saved in an output positions list (see parameter OUTCAT).

*  Usage:
*     profile in out { start finish [nsamp]
*                    { incat=?
*                    mode

*  ADAM Parameters:
*     CATFRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which positions
*        are to be stored in the output catalogue associated with
*        parameter OUTCAT.  The string supplied for CATFRAME can be one
*        of the following options.
*
*        - A Domain name such as SKY, AXIS, PIXEL, etc.
*
*        - An integer value giving the index of the required Frame.
*
*        - An IRAS90 Sky Co-ordinate System (SCS) values such as
*        EQUAT(J2000) (see SUN/163).
*
*        If a null (!) value is supplied, the positions will be stored
*        in the current SKY Frame. [!]
*     CATEPOCH = DOUBLE PRECISION (Read)
*        The epoch at which the sky positions stored in the output
*        catalogue were determined.  It will only be accessed if an
*        epoch value is needed to qualify the co-ordinate Frame
*        specified by COLFRAME.  If required, it should be given as a
*        decimal years value, with or without decimal places ("1996.8",
*        for example).  Such values are interpreted as a Besselian
*        epoch if less than 1984.0 and as a Julian epoch otherwise.
*     FINISH = LITERAL (Read)
*        The co-ordinates of the last sample in the profile, in the
*        current co-ordinate Frame of the NDF (supplying ":" will
*        display details of the required co-ordinate Frame).  The
*        position should be supplied as a list of formatted axis values
*        separated by spaces.  This parameter is only accessed if
*        parameter MODE is set to "Curve" and a null (!) value is given
*        for INCAT.  If the last (top right) pixel in the NDF has valid
*        co-ordinates in the current co-ordinate Frame of the NDF, then
*        these co-ordinates will be used as the suggested default.
*        Otherwise there will be no suggested default.
*     GEODESIC = LOGICAL (Read)
*        If TRUE then the line segments which form the profile will be
*        geodesic curves within the current co-ordinate Frame of the
*        NDF.  Otherwise, the line segments are simple straight lines.
*        This parameter is only accessed if parameter MODE is set to
*        "Curve".
*
*        As an example, consider a profile consisting of a single line
*        segment which starts at RA=0h DEC=+80d and finishes at RA=12h
*        DEC=+80d.  If GEODESIC is FALSE, the line segment will be a
*        line of constant declination, i.e. the "straight" line from the
*        position (0,80) to the position (12,80), passing through
*        (1,80), (2,80), etc.  If GEODESIC is TRUE, then the line
*        segment will be the curve of shortest distance on the celestial
*        sphere between the start and end.  In this particaular case,
*        this will be a great circle passing through the north celestial
*        pole.  [FALSE]
*     IN = NDF (Read)
*        Input NDF structure containing the data to be profiled.
*     INCAT = FILENAME (Read)
*        A catalogue containing a set of vertices or sample positions
*        defining the required profile.  The file should be in the
*        format of a "positions list" such as produced by applications
*        CURSOR and LISTMAKE.  If a null value (!) is given then
*        parameters START and FINISH will be used to obtain the vertex
*        positions.  If parameter MODE is given the value "Curve", then
*        the parameter INCAT is only accessed if a value is given for
*        it on the command line (otherwise a null value is assumed).
*     MODE = LITERAL (Read)
*        The mode by which the sample positions are selected.  The
*        alternatives are listed below.
*
*        - "Curve" -- The samples are placed evenly along a curve
*        specified by a set of vertices obtained from the user.  The
*        line segments joining these vertices may be linear or geodesic
*        (see parameter GEODESIC).  Multiple vertices may be supplied
*        using a text file (see parameter INCAT).  Alternatively, a
*        single line segment can be specified using parameters START
*        and FINISH.  The number of samples to take along the curve is
*        specified by parameter NSAMP.
*
*        - "Points" -- The positions at which samples should be taken
*        are given explicitly by the user in a text file (see parameter
*        INCAT).  No other sample positions are used.
*
*        ["Curve"]
*     NSAMP = INTEGER (Read)
*        The number of samples required along the length of the profile.
*        The first sample is at the first supplied vertex, and the last
*        sample is at the last supplied vertex.  The sample positions
*        are evenly spaced within the current co-ordinate Frame of the
*        NDF.  If a null value is supplied, a default value is used
*        equal to one more than the length of the profile in pixels.
*        This is only accessed if parameter MODE is given the value
*        "Curve".  [!]
*     OUT = NDF (Write)
*        The output NDF.  This will be one-dimensional with length
*        specified by parameter NSAMP.
*     OUTCAT = FILENAME (Write)
*        An output positions list in which to store the sample
*        positions.  This is the name of a catalogue which can be used
*        to communicate positions to subsequent applications.  It
*        includes information describing the available WCS co-ordinate
*        Frames as well as the positions themselves.  If a null value is
*        supplied, no output positions list is produced.  See also
*        parameter CATFRAME.  [!]
*     START = LITERAL (Read)
*        The co-ordinates of the first sample in the profile, in the
*        current co-ordinate Frame of the NDF (supplying ":" will
*        display details of the required co-ordinate Frame).  The
*        position should be supplied as a list of formatted axis values
*        separated by spaces.  This parameter is only accessed if
*        parameter MODE is set to "Curve" and a null (!) value is given
*        for INCAT.  If the first (bottom left) pixel in the NDF has
*        valid co-ordinates in the current co-ordinate Frame of the NDF,
*        then these co-ordinates will be used as the suggested default.
*        Otherwise there will be no suggested default.

*  Examples:
*     profile my_data prof "0 0" "100 100" 40 outcat=samps
*        Create a one-dimensional NDF called prof, holding a profile of
*        the data values in the input NDF my_data along a profile
*        starting at pixel co-ordinates [0.0,0.0] and ending at pixel
*        co-ordinates [100.0,100.0].  The profile consists of forty
*        samples spread evenly (in the pixel co-ordinate Frame) between
*        these two positions.  This example assumes that the current
*        co-ordinate Frame in the NDF my_data represents pixel
*        co-ordinates.  This can be ensured by issuing the command
*        "wcsframe my_data pixel" before running profile.  A FITS binary
*        catalogue is created called samps.FIT containing the positions
*        of all samples in the profile, together with information
*        describing all the co-ordinate Frames in which the positions of
*        the samples are known.  This file may be examined using
*        application LISTSHOW.
*     profile my_data prof "15:32:47 23:40:08" "15:32:47 23:42"
*        This example is the same as the last one except that it is
*        assumed that the current co-ordinate Frame in the input NDF
*        my_data is an equatorial (RA/DEC) system.  It creates a
*        one-dimensional profile starting at RA=15:32:47 DEC=23:40:08,
*        and ending at the same RA and DEC=23:42:00.  The number of
*        points in the profile is determined by the resolution of the
*        data.
*     profile allsky prof incat=prof_path npoint=200 geodesic
*             outcat=aa.fit
*        This examples creates a profile of the NDF allsky through a set
*        of points given in a FITS binary catalogue called
*        prof_path.FIT.  Such catalogues can be created (for example)
*        using application CURSOR.  Each line segment is a geodesic
*        curve.  The profile is sampled at 200 points.  The samples
*        positions are written to the output positions list aa.fit.
*     profile allsky2 prof2 mode=point incat=aa.fit
*        This examples creates a profile of the NDF allsky2 containing
*        samples at the positions given in the positions list aa.fit.
*        Thus, the profiles created by this example and the previous
*        example will sample the two images allsky and allsky2 at the
*        same positions and so can be compared directly.

*  Notes:
*     -  This application uses the conventions of the CURSA package
*     (SUN/190) for determining the formats of input and output
*     positions list catalogues.  If a file type of .fit is given, then
*     the catalogue is assumed to be a FITS binary table.  If a file
*     type of .txt is given, then the catalogue is assumed to be stored
*     in a text file in "Small Text List" (STL) format.  If no file
*     type is given, then ".fit" is assumed.

*  Related Applications:
*     KAPPA: LINPLOT, CURSOR, LISTMAKE, LISTSHOW; CURSA: XCATVIEW.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, WCS,
*     LABEL, TITLE, and UNITS components of the NDF.
*     -  All non-complex numeric data types can be handled. Only
*     double-precision floating-point data can be processed directly.
*     Other non-complex data types will undergo a type conversion
*     before the profile is produced.

*  Implementation Deficiencies:
*     -  As yet, there is no "Cursor" mode allowing the profile to be
*     defined by positions given using a graphics cursor.
*     -  As yet, there is no facility to allow profile sample values
*     to be obtained by binning the data values is the neighbourhood of
*     the sample positions.

*  Copyright:
*     Copyright (C) 1998-1999, 2001, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2006 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     3-SEP-1999 (DSB):
*        Added NULL argument to KPG1_GTPOS call.
*     13-DEC-2001 (DSB):
*        Added parameters CATFRAME and CATEPOCH.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, corrected punctuation, and
*        wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CVAL*80          ! NDF character component value
      CHARACTER MODE*10          ! Source of profile positions
      CHARACTER NDFNAM*255       ! Full NDF specification
      CHARACTER TITLE*80         ! Title from input positions list
      CHARACTER TYPE*( NDF__SZTYP ) ! NDF data type
      DOUBLE PRECISION CC1( NDF__MXDIM ) ! Current Frame co-ords at
                                 ! start of profile
      DOUBLE PRECISION CC2( NDF__MXDIM ) ! Current Frame co-ords at end
                                 ! of profile
      DOUBLE PRECISION ENDS( 2, NDF__MXDIM ) ! Current Frame co-ords at
                                 ! ends of profile
      DOUBLE PRECISION GC1( NDF__MXDIM ) ! GRID co-ords at start of
                                 ! profile
      DOUBLE PRECISION GC2( NDF__MXDIM ) ! GRID co-ords at end of
                                 ! profile
      INTEGER DIM( NDF__MXDIM )  ! Dimensions of input NDF
      INTEGER EL                 ! Number of values mapped
      INTEGER I                  ! Loop count
      INTEGER IERR               ! Index of first numeric error
      INTEGER INDF1              ! Identifier for input NDF
      INTEGER INDF2              ! Identifier for output NDF
      INTEGER IPDAT              ! Pointer to input NDFs DATA array
      INTEGER IPDOUT             ! Pointer to output NDFs DATA array
      INTEGER IPFIL              ! Pointer to input positions array
      INTEGER IPID               ! Pointer to position identifiers array
      INTEGER IPPDAT             ! Pointer to profile data values
      INTEGER IPPVAR             ! Pointer to profile variance values
      INTEGER IPVAR              ! Pointer to input NDFs VARIANCE array
      INTEGER IPVOUT             ! Pointer to output NDFs VARIANCE array
      INTEGER IPW1               ! Pointer to input positions
      INTEGER IWCS               ! Pointer for input NDF WCS FrameSet
      INTEGER IWCSIN             ! Pointer for input positions list
                                 ! FrameSet
      INTEGER LTTL               ! Length of TITLE
      INTEGER MAP                ! Simplified Mapping from IWCS
      INTEGER MAPIN              ! Simplified Mapping from IWCSIN
      INTEGER NAMLEN             ! Length of NDF name
      INTEGER NAXIN              ! Number of axes in input positions
                                 ! list Base Frame
      INTEGER NCAX               ! Number of axes in input NDF's
                                 ! current Frame
      INTEGER NDIM               ! Number of dimensions in input NDF
      INTEGER NERR               ! No. of numeric errors which occurred
      INTEGER NP                 ! Number of elements in profile
      INTEGER NPOS               ! No. of profile positions given
      INTEGER STATE              ! State of parameter INCAT
      LOGICAL BADD               ! Any bad values in profile data
                                 ! values?
      LOGICAL BADV               ! Any bad values in profile variance
                                 ! values?
      LOGICAL FRIPW1             ! Free pointer IPW1?
      LOGICAL GEO                ! Produce a geodesic profile?
      LOGICAL USEFIL             ! Are profile positions given by IPFIL?
      LOGICAL VAR                ! Are variances available?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indicate we have not yet used pointer IPW1.
      FRIPW1 = .FALSE.

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Access the data to be displayed, and get its main parameters.
*  =============================================================
*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )

*  Get a simplified Mapping from Base to Current Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                                    STATUS ), STATUS )

*  Report an error if either the forward or inverse transformation is
*  not defined.
      IF( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         CALL MSG_SETC( 'DOM', AST_GETC( IWCS, 'DOMAIN', STATUS ) )
         CALL NDF_MSG( 'NDF', INDF1 )

         STATUS = SAI__ERROR
         CALL ERR_REP( 'PROFILE_ERR', 'The transformation from pixel '//
     :                 'co-ordinates to the current co-ordinate Frame'//
     :                 ' in ''^NDF'' (^DOM co-ordinates) is not '//
     :                 'defined.', STATUS )

      ELSE IF( .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         CALL MSG_SETC( 'DOM', AST_GETC( IWCS, 'DOMAIN', STATUS ) )
         CALL NDF_MSG( 'NDF', INDF1 )

         STATUS = SAI__ERROR
         CALL ERR_REP( 'PROFILE_ERR', 'The transformation from the '//
     :                 'current co-ordinate Frame in ''^NDF'' (^DOM '//
     :                 'co-ordinates) to pixel co-ordinates is not '//
     :                 'defined.', STATUS )
      END IF

*  Store the number of axes in the current Frame.
      NCAX = AST_GETI( MAP, 'NOUT', STATUS )

*  Get the dimensions of the NDF.
      CALL NDF_DIM( INDF1, NDF__MXDIM, DIM, NDIM, STATUS )

*  Get the name of the NDF.  This is later stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF1 )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NAMLEN, STATUS )

*  See if the VARIANCE componwent is defined.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain information defining the path of the profile through the NDF.
*  ====================================================================

*  See where the samples are to be placed.
      CALL PAR_CHOIC( 'MODE', 'Curve', 'Curve,Points', .TRUE., MODE,
     :                STATUS )

*  Set a dynamic default for INCAT if required.
      IF( MODE .EQ. 'CURVE' ) CALL PAR_DEF0C( 'INCAT', '!', STATUS )

*  The following messing about is required because there seems to be no
*  way of setting a null dynamic default for a parameter.  If it were
*  possible, it would be better to set a dynamic default of "!" for
*  INCAT when in CURVE mode, and not set any dynamic default when in
*  POINTS mode.  The ifl file could then have vpath=dynamic,global.
*  Instead...

*  See if a value has been assigned to the parameter INCAT on the
*  command line.
      CALL LPG_STATE( 'INCAT', STATE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If a value for INCAT was supplied on the command line, or if we are
*  in POINTS mode, attempt to read positions along the path from a file
*  specified by the user.  Positions are returned in the Base Frame of
*  the FrameSet stored in the positions list.
      IF( STATE .EQ. PAR__ACTIVE .OR. MODE .EQ. 'POINTS' ) THEN
         CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSIN, NPOS, NAXIN, IPW1,
     :                    IPID, TITLE, ' ', STATUS )

*  If succesful, indicate that the IPW1 pointer can be used.
         IF( STATUS .EQ. SAI__OK ) THEN
            FRIPW1 = .TRUE.

*  If a null value was given, annul the error.
         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

      END IF

*  If positions were read from the file...
      IF( FRIPW1 ) THEN

*  Attempt to merge the NDFs FrameSet into the FrameSet read from the
*  positions list. The Current Frame in the merged FrameSet (IWCSIN) is
*  inherited from the NDFs FrameSet.
         CALL KPG1_ASMRG( IWCSIN, IWCS, ' ', .FALSE., 2, STATUS )

*  Allocate memory to hold the positions after being mapped into the
*  Current Frame of the NDFs FrameSet.
         CALL PSX_CALLOC( NPOS*NCAX, '_DOUBLE', IPFIL, STATUS )

*  If succesful, indicate that the IPFIL pointer should be used.
         USEFIL = ( STATUS .EQ. SAI__OK )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the Mapping from the Base Frame in the FrameSet read from the
*  positions list, to the Current Frame in the NDFs FrameSet.
         MAPIN = AST_GETMAPPING( IWCSIN, AST__BASE, AST__CURRENT,
     :                           STATUS )
         MAPIN = AST_SIMPLIFY( MAPIN, STATUS )

*  Map the positions read from the positions list using this Mapping.
        CALL AST_TRANN( MAPIN, NPOS, NAXIN, NPOS,
     :                  %VAL( CNF_PVAL( IPW1 ) ),
     :                  .TRUE., NCAX, NPOS, %VAL( CNF_PVAL( IPFIL ) ),
     :                  STATUS )

*  If no positions were obtained using INCAT, use START and FINISH
*  instead.
      ELSE

*  The suggested default for the start of the profile is the first
*  pixel.  Store the GRID co-ordinates at the centre of the first pixel.
         DO I = 1, NDIM
            GC1( I ) = 1.0D0
         END DO

*  Transform to the current Frame.
         CALL AST_TRANN( MAP, 1, NDIM, 1, GC1, .TRUE., NCAX, 1, CC1,
     :                   STATUS )

*  Get a position for the profile start from the environment, using the
*  above current Frame co-ordinates as the dynamic default.
         CALL KPG1_GTPOS( 'START', IWCS, .TRUE., CC1, GC1, STATUS )

*  The suggested default for the end of the profile is the last pixel.
*  Store the GRID co-ordinates at the centre of the last pixel.
         DO I = 1, NDIM
            GC2( I ) = DBLE( DIM( I ) )
         END DO

*  Transform to the current Frame.
         CALL AST_TRANN( MAP, 1, NDIM, 1, GC2, .TRUE., NCAX, 1, CC2,
     :                   STATUS )

*  Get a position for the profile end from the environment, using the
*  above current Frame co-ordinates as the dynamic default.
         CALL KPG1_GTPOS( 'FINISH', IWCS, .TRUE., CC2, GC2, STATUS )

*  Store the positions.
         NPOS = 2

         DO I = 1, NCAX
            ENDS( 1, I ) = CC1( I )
            ENDS( 2, I ) = CC2( I )
         END DO

*  Indicate that the IPFIL pointer should not be used.
         USEFIL = .FALSE.

      END IF

*  Report an error if fewer than 2 positions were obtained.
      IF( NPOS .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PROFILE_ERR', 'Fewer than 2 positions '//
     :                 'obtained.', STATUS )
         GO TO 999
      END IF

*  See if the profiles should follow geodesic curves or straight lines
*  in the current Frame.
      IF( MODE .EQ. 'CURVE' ) CALL PAR_GET0L( 'GEODESIC', GEO, STATUS )

*  Obtain the 1-D array of data values to be display.
*  ==================================================
*  Map the DATA component of the NDF in double precision.
      CALL NDF_MAP( INDF1, 'DATA', '_DOUBLE', 'READ', IPDAT, EL,
     :              STATUS )

*  If variance values are available, also map the variances.
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VARIANCE', '_DOUBLE', 'READ', IPVAR, EL,
     :                 STATUS )
      ELSE
         VAR = .FALSE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See how many points are required along the profile.  If a null value
*  is given annul the error and store a value of zero.  This will cause
*  a default value roughly equal to the number of pixels along the
*  profile to be used.
      IF( MODE .EQ. 'CURVE' ) THEN
         CALL PAR_GET0I( 'NSAMP', NP, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NP = 0
         ELSE
            NP = MAX( 2, ABS( NP ) )
         END IF
      END IF

*  Get the title from the NDF.
      TITLE = ' '
      CALL NDF_CGET( INDF1, 'TITLE', TITLE, STATUS )
      LTTL = MAX( 1, CHR_LEN( TITLE ) )

*  Create the one-dimensional array of data values to be displayed,
*  with associated variances if possible.  A copy of the NDF's WCS
*  FrameSet is returned in which the Base Frame has been re-mapped to
*  represent the one-dimensional GRID co-ordinate in the
*  one-dimensional array.
      IF( USEFIL ) THEN
         CALL KPS1_PRFMK( MODE, NDIM, DIM, %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ), IWCS, NCAX,
     :                    NPOS, NPOS, %VAL( CNF_PVAL( IPFIL ) ), GEO,
     :                    0, %VAL( CNF_PVAL( IPID ) ), 'OUTCAT',
     :                    TITLE( : LTTL ), NP, IPPDAT, IPPVAR,
     :                    BADD, BADV, STATUS )

      ELSE
         CALL KPS1_PRFMK( MODE, NDIM, DIM, %VAL( CNF_PVAL( IPDAT ) ),
     :                    VAR, %VAL( CNF_PVAL( IPVAR ) ), IWCS, NCAX,
     :                    NPOS, NPOS, ENDS, GEO, 1, 0, 'OUTCAT',
     :                    TITLE( : LTTL ), NP, IPPDAT, IPPVAR,
     :                    BADD, BADV, STATUS )
      END IF

*  Tell the user how many samples were created.
      CALL MSG_SETI( 'NP', NP )
      CALL MSG_OUT( 'PROFILE_MSG_', '  Profile contains ^NP samples.',
     :              STATUS )

*  Save the profile in a new NDF.
*  ==============================

*  Get the data type for the output NDF. If the input is _DOUBLE, use
*  _DOUBLE, otherwise use _REAL.
      CALL NDF_TYPE( INDF1, 'DATA', TYPE, STATUS )
      IF( TYPE .NE. '_DOUBLE' ) TYPE = '_REAL'

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create the NDF.
      CALL LPG_CREAT( 'OUT', TYPE, 1, 1, NP, INDF2, STATUS )

*  Map the DATA and (if required) variance components.
      CALL NDF_MAP( INDF2, 'DATA', '_DOUBLE', 'WRITE', IPDOUT, EL,
     :              STATUS )
      IF( VAR ) CALL NDF_MAP( INDF2, 'VARIANCE', '_DOUBLE', 'WRITE',
     :                        IPVOUT, EL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the DATA and (if required) variance components into the output.
      CALL VEC_DTOD( BADD, NP, %VAL( CNF_PVAL( IPPDAT ) ),
     :               %VAL( CNF_PVAL( IPDOUT ) ), IERR,
     :                  NERR, STATUS )
      IF( VAR ) THEN
         CALL VEC_DTOD( BADV, NP, %VAL( CNF_PVAL( IPPVAR ) ),
     :                  %VAL( CNF_PVAL( IPVOUT ) ),
     :                  IERR, NERR, STATUS )
      END IF

*  Store the FrameSet with the output NDF.
      CALL NDF_PTWCS( IWCS, INDF2, STATUS )

*  Copy the TITLE, LABEL and UNITS components from the input NDF.
      IF( TITLE .NE. ' ' ) CALL NDF_CPUT( TITLE( : LTTL ), INDF2,
     :                                    'TITLE', STATUS )

      CVAL = ' '
      CALL NDF_CGET( INDF1, 'LABEL', CVAL, STATUS )
      IF( CVAL .NE. ' ' ) CALL NDF_CPUT( CVAL( : CHR_LEN( CVAL ) ),
     :                                   INDF2, 'LABEL', STATUS )

      CVAL = ' '
      CALL NDF_CGET( INDF1, 'UNITS', CVAL, STATUS )
      IF( CVAL .NE. ' ' ) CALL NDF_CPUT( CVAL( : CHR_LEN( CVAL ) ),
     :                                   INDF2, 'UNITS', STATUS )

*  Tidy up.
*  ========

 999  CONTINUE

*  Release the work space holding the profile data.
      CALL PSX_FREE( IPPDAT, STATUS )
      IF( VAR ) CALL PSX_FREE( IPPVAR, STATUS )
      IF( FRIPW1 ) CALL PSX_FREE( IPW1, STATUS )

      IF( USEFIL) THEN
         CALL PSX_FREE( IPFIL, STATUS )
         CALL PSX_FREE( IPID, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROFILE_ERR', 'PROFILE: Failed to create a '//
     :                 'one-dimensional profile through a data set.',
     :                 STATUS )
      END IF

      END
