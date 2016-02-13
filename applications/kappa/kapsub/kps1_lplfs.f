      SUBROUTINE KPS1_LPLFS( INDF, IWCS, DIST, IAXIS, DIM, XMAP, YMAP,
     :                       MCOMP, DUNIT, NOINV, FSET, STATUS )
*+
*  Name:
*     KPS1_LPLFS

*  Purpose:
*     Create a FrameSet describing Frames required by LINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LPLFS( INDF, IWCS, DIST, IAXIS, DIM, XMAP, YMAP, MCOMP,
*                      DUNIT, NOINV, FSET, STATUS )

*  Description:
*     This routine is supplied with FrameSet describing a 1-d data array as
*     read from an NDF DATA array for instance. The Base Frame should be
*     1-d GRID co-ordinate within the data array. The Current Frame can
*     have any number of axes. The axis within the Current Frame which is
*     to be displayed on the horizontal axis is supplied in IAXIS. If DIST
*     is TRUE, the horizontal axis is annotated with distance from the profile
*     starting point (measured along the profile), and the axis given by
*     IAXIS is used only to determine the format for the distance values.
*
*     A FrameSet is returned containing 3 Frames. Each Frame has 2 axes:
*
*     Frame 1: Corresponds to "what we've got"
*        Axis 1 - GRID co-ordinate in the supplied 1-d data array (i.e.
*                 an array component in the supplied NDF).
*        Axis 2 - Supplied data value.
*
*     Frame 2: This is the Frame which is to be mapped linearly or
*              logarithmically (depending on the LogPlot attribute of the
*              Plot) onto the graphics screen, and is given the Domain
*              AGI_WORLD (strictly AGI_WORLD should always be linear, but
*              no one uses AGI_WORLD these days...)
*        Axis 1 - The axis specified using XMAP.
*        Axis 2 - The axis specified using YMAP.
*
*     Frame 3: Corresponds to "what we want to see" (i.e. the Frame describing
*              the quantities which are to be annotated on the displayed axes).
*              This is a CmpFrame which is given Domain DATAPLOT.
*        Axis 1 - The quantity to be plotted on the horizontal axis of the
*                 graph (i.e. distance from the starting point of the profile
*                 if DIST is .TRUE., or the supplied Current Frame axis
*                 otherwise).
*        Axis 2 - The raw data value, or logged data value as required.
*
*     Frame 2 is the Base Frame on exit, and Frame 3 is the Current Frame.

*  Arguments:
*     INDF = INTEGER (Given)
*        An idenfifier for the NDF being displayed.
*     IWCS = INTEGER (Given)
*        A pointer to the FrameSet associated with the array being plotted.
*     DIST = LOGICAL (Given)
*        Should the graph be annotated with offset from the starting
*        position along the profile path, instead of axis value?
*     IAXIS = INTEGER (Given)
*        The index of the Current Frame axis which is to be used to
*        annotated the horizontal axis (if DIST is .FALSE.) or to format
*        distance values (if DIST is .TRUE.).
*     DIM = INTEGER (Given)
*        The number of pixels in the 1-d array being plotted.
*     XMAP = CHARACTER * ( * ) (Given and Returned)
*        Specifies what to use for axis 1 of Frame 2 in the returned
*        FrameSet. This controls how the values specified by IAXIS/DIST
*        are mapped onto the screen. The options are:
*
*        - "PIXEL" -- Use NDF grid index. This means that pixel index within
*        the input NDF will increase linearly across the screen.
*
*        - "DISTANCE" -- Use a 1-D Frame representing distance along the
*        curve. This means that distance along the curve will increase
*        linearly across the screen.
*
*        - "LINEAR" -- Use the axis specified by IAXIS/DIST and set the
*        LogPlot attribute for this axis to zero. This means that the value
*        used to annotate the axis increases linearly across the screen.
*
*        - "LOG" -- Use the axis specified by IAXIS/DIST and set the
*        LogPlot attribute for this axis non-zero. This means that the
*        logarithm (base 10) of the value used to annotate the axis
*        increases linearly across the screen.
*
*        - "DEFAULT" -- Determine an appropriate default. This default
*        value is returned. One of "LINEAR" or "LOG" is chosen as the
*        default.
*
*     YMAP = CHARACTER * ( * ) (Given)
*        Specifies what to use for axis 2 of Frame 2 in the returned
*        FrameSet. This controls how the data values are mapped onto the
*        screen. The options are:
*
*        - "LINEAR" -- Axis 2 represents data values, and the LogPlot
*        attribute for the axis is set zero, so that data values are
*        mapped linearly onto the screen.
*
*        - "LOG" -- Axis 2 represents data values, and the LogPlot
*        attribute for the axis is set non-zero, so that data values are
*        mapped logarithmically onto the screen.
*
*        - "VALUELOG" -- Axis 2 represents log (base 10) of the data
*        values, and the LogPlot attribute for the axis is set zero, so that
*        the log of the data values are mapped linearly onto the screen.
*
*     MCOMP = CHARACTER * ( * ) (Given)
*        NDF component being displayed.
*     DUNIT = CHARACTER * ( * ) (Given)
*        Data units in the array being plotted. Ignored if blank.
*     NONV = LOGICAL (Returned)
*        Returned .TRUE. if any of the inter-Frame Mappings do not have
*        inverse transformations.
*     FSET = INTEGER (Returned)
*        A pointer to the returned FrameSet. Returned equal to AST__NULL
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998, 2003-2004 Central Laboratory of the Research
*                                   Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     9-DEC-1998 (DSB):
*        Modified Y axis label to include NDF label component.
*     7-JAN-2003 (DSB):
*        Modified DATAPLOT Frame to be a CmpFrame rather than a Frame
*        formed using AST_PICKAXES. this means that the behaviour of
*        SpecFrame axes is retained in the DATAPLOT Frame.
*     25-FEB-2003 (DSB):
*        Extended length of TEXT variable to avoid spurious errors in Y
*        axis label.
*     7-FEB-2004 (DSB):
*        Replaced YLOG parameter by XMAP and YMAP parameters.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     10-DEC-2004 (DSB):
*        Use a SpecFluxFrame instead of a CmpFrame for the "what we want"
*        Frame if the data units correspond to a known flux system and the
*        X axis is described by a SpecFrame.
*     2-MAR-2005 (DSB):
*        Correct logic for checkign for AST__BADUN errors.
*     15-APR-2005 (PWD):
*        Parameterize use of backslashes to improve portability.
*     18-MAY-2007 (DSB):
*        Normalise log and lin RMS values before comparing them, by
*        converting them from an increment in data value (log or lin),
*        to an increment in pixels.
*     2-JUL-2009 (DSB):
*        When choosing between log and lin axes, prefer linear if there is
*        not much difference (since this allows points to be plotted at
*        negative axis values).
*     12-OCT-2009 (DSB):
*        Use the NDF component name (rather than the NDF label value) as
*        the Y axis Symbol attribute rather than the NDF label. This is
*        because the Symbol attribute is used as a column name and so
*        needs to be short and include no spaces.
*     15-OCT-2009 (DSB):
*        Extract code that sets axis attribute to describe an NDF array
*        component into a new routine (KPG1_SAXAT).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      LOGICAL DIST
      INTEGER IAXIS
      INTEGER DIM
      CHARACTER XMAP*(*)
      CHARACTER YMAP*(*)
      CHARACTER DUNIT*(*)
      CHARACTER MCOMP*(*)

*  Arguments Returned:
      LOGICAL NOINV
      INTEGER FSET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER ATTR*20          ! Attribute name
      CHARACTER TEXT*100         ! General text string
      CHARACTER UNIT*100         ! Unit attribute value
      DOUBLE PRECISION C         ! Offset of fit
      DOUBLE PRECISION M         ! Gradient of fit
      DOUBLE PRECISION POS( 2 )  ! Start and end of samples in GRID Frame
      DOUBLE PRECISION RMSLIN    ! RMS residual of fit (linear mapping)
      DOUBLE PRECISION RMSLOG    ! RMS residual of fit (log mapping)
      INTEGER AXES( 2 )          ! Axes to pick from an existing Frame
      INTEGER CFRM               ! Current Frame in supplied FrameSet
      INTEGER FR1                ! Frame 1 in compound frame
      INTEGER FR2                ! Frame 2 in compound frame
      INTEGER FRM0               ! Pointer to distance Frame
      INTEGER FRMI               ! Pointer to axis value Frame
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in a string
      INTEGER ICURR              ! Index of current Frame in IWCS
      INTEGER INPRM(2)           ! Axis permutation array
      INTEGER IPD                ! Pointer to array of returned axis 1 values
      INTEGER IPG                ! Pointer to array of GRID values
      INTEGER IPW                ! Pointer to array of Current Frame values
      INTEGER LUT0               ! Pointer to distance LutMap
      INTEGER LUTI               ! Pointer to axis value LutMap
      INTEGER MAP1               ! Map pointer
      INTEGER MAP2               ! Map pointer
      INTEGER MAPX               ! X axis 1-D Mapping
      INTEGER MAPY               ! Y axis 1-D Mapping
      INTEGER NAX                ! No. of axes in supplied Current Frame
      INTEGER NERR               ! Number of numerical errors
      INTEGER NERRV              ! Number of numerical variance errors
      INTEGER SMAP               ! Base->Current Mapping in supplied FrameSet
      INTEGER TMAP               ! Unused Mapping
      INTEGER UNIFRM             ! Uniform Frame in returned FrameSet
      INTEGER WWGOT              ! Base Frame in returned FrameSet
      INTEGER WWWANT             ! Current Frame in returned FrameSet
      LOGICAL BAD                ! Any bad values found?
      LOGICAL SAMEUN             ! Do all current frame axes have same units?
*.

*  Initialise.
      FSET = AST__NULL
      NOINV = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a simplified Mapping from Base to Current Frame in the supplied
*  FrameSet.
      SMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                     AST__CURRENT, STATUS ),
     :                     STATUS )

*  Save a pointer to the Current Frame in the supplied FrameSet.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  See how many axes the Current Frame has.
      NAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Set a flag indicating if the units on all axes are the same.
      SAMEUN = .TRUE.
      UNIT = ' '
      DO I = 1, NAX

*  All SkyAxes are assumed to have units of "rad". This is a SkyAxis
*  if it has an AsTime attribute. Watch for errors caused by the axis not
*  having an AsTime attribute.
         IF( STATUS .EQ. SAI__OK ) THEN
            ATTR = 'ASTIME('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            TEXT = AST_GETC( CFRM, ATTR( :IAT ), STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               TEXT = 'RAD'

            ELSE
               CALL ERR_ANNUL( STATUS )

               ATTR = 'UNIT('
               IAT = 5
               CALL CHR_PUTI( I, ATTR, IAT )
               CALL CHR_APPND( ')', ATTR, IAT )
               TEXT = AST_GETC( CFRM, ATTR( :IAT ), STATUS )
            END IF
         END IF

         IF( I .EQ. 1 ) THEN
            UNIT = TEXT
         ELSE IF( TEXT .NE. UNIT ) THEN
            SAMEUN = .FALSE.
         END IF

      END DO

*  Now create the Mapping from the "what we've got" Frame to the uniform
*  Frame. This is a parallel CmpMap. The Mapping for axis 1 depends on
*  the value of parameter XMAP. If XMAP is "PIXEL" the Mapping is a UnitMap.
*  If it is "DISTANCE", it is a LutMap giving distance from the starting
*  point for any GRID position. If it is "LINEAR" or "LOG", it is a LutMap
*  giving the value of axis IAXIS for any GRID position. The Mapping for axis
*  2 depends on the value of parameter YMAP. If YMAP is "LINEAR" or "LOG",
*  it is a UnitMap. If YMAP is "VALUELOG", it is an IntraMap implementing a
*  LOG10 function. An IntraMap is used because AST as yet has no LOG10
*  Mapping.
*  =======================================================================

*  Allocate memory to hold an array of DIM 1-d GRID values.
      CALL PSX_CALLOC( DIM, '_DOUBLE', IPG, STATUS )

*  Allocate memory to hold the corresponding n-d Current Frame values.
      CALL PSX_CALLOC( DIM*NAX, '_DOUBLE', IPW, STATUS )

*  Allocate memory to hold the corresponding distance or selected axis values.
      CALL PSX_CALLOC( DIM, '_DOUBLE', IPD, STATUS )

*  Abort if an error occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find DIM positions evenly spaced in the GRID Frame along the profile.
*  The positions are at the centre of each grid cell. This results in the
*  IPG array holding 1.0, 2.0, 3.0, ... NDIM.
      POS( 1 ) = 1.0D0
      POS( 2 ) = DBLE( DIM )
      CALL KPG1_ASSMP( AST__NULL, 2, 1, 2, POS, .FALSE., DIM,
     :                 1.0D0, %VAL( CNF_PVAL( IPG ) ), STATUS )

*  Transform these GRID positions into the Current Frame in the supplied
*  FrameSet.
      CALL AST_TRANN( SMAP, DIM, 1, DIM, %VAL( CNF_PVAL( IPG ) ),
     :                .TRUE., NAX,
     :                DIM, %VAL( CNF_PVAL( IPW ) ), STATUS )

*  Indicate we have found no bad axis values yet.
      BAD = .FALSE.

*  If necessary, we create a LutMap which maps grid indices to distance
*  along the curve.
      IF( DIST .OR. XMAP .EQ. 'DISTANCE' ) THEN

*  Find the distance from the first GRID position to each subsequent GRID
*  position, measured along the profile. If the axes of the current Frame do
*  not all have the same Unit, normalise the distances to a maximum value of
*  1.0.
         CALL KPG1_ASDSV( CFRM, DIM, NAX, %VAL( CNF_PVAL( IPW ) ),
     :                    .NOT. SAMEUN,
     :                    %VAL( CNF_PVAL( IPD ) ), BAD, STATUS )

*  Create the LutMap.
         LUT0 = AST_LUTMAP( DIM, %VAL( CNF_PVAL( IPD ) ),
     :                      1.0D0, 1.0D0, ' ',
     :                      STATUS )

*  If we need to find a default value for XMAP, and the axis is being annotated
*  with distance, see if a log or lin mapping would produce a more even
*  spread of values.
         IF( XMAP .EQ. 'DEFAULT' .AND. DIST ) THEN

*  First find the RMS deviation of a stright line fitted to the axis value
            CALL KPG1_FIT1D( 1, DIM, %VAL( CNF_PVAL( IPD ) ),
     :                       %VAL( CNF_PVAL( IPG ) ), M, C,
     :                       RMSLIN, STATUS )

*  Convert the RMS value into a distance in pixels
            IF( M .NE. 0.0 ) RMSLIN = ABS( RMSLIN/M )

*  Now take the log base 10 of the axis values and find the new RMS
*  deviation.
            CALL KPG1_LOGAD( .TRUE., DIM, %VAL( CNF_PVAL( IPD ) ),
     :                       .FALSE.,
     :                        %VAL( CNF_PVAL( IPD ) ), 10.0D0,
     :                       %VAL( CNF_PVAL( IPD ) ),
     :                        %VAL( CNF_PVAL( IPD ) ),
     :                       NERR, NERRV, STATUS )
            IF( NERR .LT. 0.5*DIM ) THEN
               CALL KPG1_FIT1D( 1, DIM, %VAL( CNF_PVAL( IPD ) ),
     :                          %VAL( CNF_PVAL( IPG ) ), M, C,
     :                          RMSLOG, STATUS )

*  Convert the RMS value into a distance in pixels
               IF( M .NE. 0.0 ) RMSLOG = ABS( RMSLOG/M )

*  Choose the mapping which gives the smallest rms (i.e. spreads the
*  pixels out most evenly on the screen). If they both give small rms
*  (for instance if the dynamic range of the axis is very small) we
*  favour linear axes since log axes loose the negative part of the
*  axis.
               IF( RMSLOG .LT. RMSLIN - DIM*0.01 ) THEN
                  XMAP = 'LOG'
               ELSE
                  XMAP = 'LINEAR'
               END IF
            ELSE
               XMAP = 'LINEAR'
            END IF

         END IF

      ELSE
         LUT0 = AST__NULL
      END IF

*  If necessary, we create a LutMap which maps grid indices to value on
*  the selected axis.
      IF( .NOT. DIST ) THEN

*  Normalise the axis values using AST_NORM and copy the values on the
*  required axis to a new 1-d array. Note if there are any bad values in this
*  array.
         CALL KPS1_LPLNM( CFRM, IAXIS, DIM, NAX,
     :                    %VAL( CNF_PVAL( IPW ) ),
     :                    %VAL( CNF_PVAL( IPD ) ), BAD, STATUS )

*  Create the LutMap.
         LUTI = AST_LUTMAP( DIM, %VAL( CNF_PVAL( IPD ) ),
     :                      1.0D0, 1.0D0, ' ',
     :                      STATUS )


*  If we need to find a default value for XMAP, see if a log or lin mapping
*  would produce a more even spread of values.
         IF( XMAP .EQ. 'DEFAULT' ) THEN

*  First find the RMS deviation of a stright line fitted to the axis value
            CALL KPG1_FIT1D( 1, DIM, %VAL( CNF_PVAL( IPD ) ),
     :                       %VAL( CNF_PVAL( IPG ) ), M, C,
     :                       RMSLIN, STATUS )

*  Convert the RMS into an equivalent number of pixels.
            IF( M .NE. 0.0 ) RMSLIN = ABS( RMSLIN/M )

*  Now take the log base 10 of the axis values and find the new RMS
*  deviation.
            CALL KPG1_LOGAD( .TRUE., DIM, %VAL( CNF_PVAL( IPD ) ),
     :                       .FALSE.,
     :                        %VAL( CNF_PVAL( IPD ) ), 10.0D0,
     :                       %VAL( CNF_PVAL( IPD ) ),
     :                        %VAL( CNF_PVAL( IPD ) ),
     :                       NERR, NERRV, STATUS )
            IF( NERR .LT. 0.5*DIM ) THEN
               CALL KPG1_FIT1D( 1, DIM, %VAL( CNF_PVAL( IPD ) ),
     :                          %VAL( CNF_PVAL( IPG ) ), M, C,
     :                          RMSLOG, STATUS )

*  Convert the RMS into an equivalent number of pixels.
               IF( M .NE. 0.0 ) RMSLOG = ABS( RMSLOG/M )

*  Choose the mapping which gives the smallest rms (i.e. spreads the
*  pixels out most evenly on the screen). If they both give small rms
*  (for instance if the dynamic range of the axis is very small) we
*  favour linear axes since log axes loose the negative part of the
*  axis.
               IF( RMSLOG .LT. RMSLIN - DIM*0.01 ) THEN
                  XMAP = 'LOG'
               ELSE
                  XMAP = 'LINEAR'
               END IF
            ELSE
               XMAP = 'LINEAR'
            END IF

         END IF

      ELSE
         LUTI = AST__NULL
      END IF

*  Report an error if any bad values were found.
      IF( BAD .AND. STATUS .EQ. SAI__OK ) THEN
         IF( IAXIS .NE. 0 ) THEN
            ATTR = 'LABEL('
            IAT = 7
            CALL CHR_PUTI( IAXIS, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            TEXT = AST_GETC( CFRM, ATTR( : IAT ), STATUS )
            CALL KPG1_PGESC( TEXT, STATUS )
            CALL MSG_SETC( 'LBL', TEXT )
            CALL MSG_SETC( 'LBL', ' values' )
         ELSE
            CALL MSG_SETC( 'LBL', ' positions' )
         END IF

         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'KPS1_LPLFS_ERR', 'Some points along the '//
     :                 'profile have undefined ^LBL within '//
     :                 'the current co-ordinate Frame of ''^NDF''.',
     :                 STATUS )
      END IF

*  If pixel indices are to be mapped linearly onto the horizontal screen
*  axis, use a UnitMap for the x axis map. */
      IF( XMAP .EQ. 'PIXEL' ) THEN
         MAPX = AST_UNITMAP( 1, ' ', STATUS )

*  If distance is to be mapped linearly onto the horizontal screen
*  axis, or if the horizontal axis is being annotated with distance,
*  use the LUT0 LutMap created above. */
      ELSE IF( LUT0 .NE. AST__NULL ) THEN
         MAPX = AST_CLONE( LUT0, STATUS )

*  If selected axis value is to be mapped linearly onto the horizontal screen
*  axis, use the LUTI LutMap created above. */
      ELSE
         MAPX = AST_CLONE( LUTI, STATUS )

      END IF

*  Set the NOINV flag if the inverse transformation is not defined.
      NOINV = ( .NOT. AST_GETL( MAPX, 'TranInverse', STATUS ) )

*  Now do axis 2 (the vertical axis). If the Y axis is to display data as
*  supplied, just use a UnitMap.
      IF( YMAP .NE. 'VALUELOG' ) THEN
         MAPY = AST_UNITMAP( 1, ' ', STATUS )

*  If the Y axis is to display log base 10 of the data, create s suitable
*  Mapping.
      ELSE

*  At the moment AST does not have a Log Mapping, so we need to implement
*  a private log mapping by using an IntraMap. Ensure KAPPA intramaps are
*  registered.
         CALL KPG1_ASREG( STATUS )

*  Create a "Log10" Mapping.
         MAPY = AST_INTRAMAP( 'Log10', 1, 1, ' ', STATUS )

      END IF

*  Combine the X and the Y Mappings in parallel.
      MAP1 = AST_CMPMAP( MAPX, MAPY, .FALSE., ' ', STATUS )

*  Now create the Mapping from the "what we've got" Frame to the "what we
*  want" Frame. This is also a parallel CmpMap.
*  =======================================================================

*  Axis 1. This is a LutMap which gives either the distance along the
*  curve (LUT0) or axis value (LUT1).
      IF( DIST ) THEN
         MAPX = AST_CLONE( LUT0, STATUS )
      ELSE
         MAPX = AST_CLONE( LUTI, STATUS )
      END IF

*  Set the NOINV flag if the inverse transformation is not defined.
      NOINV = ( NOINV .OR.
     :          .NOT. AST_GETL( MAPX, 'TranInverse', STATUS ) )

*  Create a CmpMap giving the required "what we've got" -> "what we want"
*  Mapping (the axis 2 Mapping is the same as for the "what we've got" ->
*  "uniform" Mapping).
      MAP2 = AST_CMPMAP( MAPX, MAPY, .FALSE., ' ', STATUS )

*  Create the required Frames
*  ==========================

*  "What we've got":
*  -----------------
*  Axis 1 is copied from the GRID axis of the Base Frame of the supplied
*  FrameSet. Axis 2 is a default Axis.
      AXES( 1 ) = 1
      AXES( 2 ) = 0
      WWGOT = AST_PICKAXES( AST_GETFRAME( IWCS, AST__BASE, STATUS ),
     :                      2, AXES, TMAP, STATUS )

*  Set the label, symbol and units for the data axis (axis 2).
      CALL KPG1_SAXAT( INDF, MCOMP, 2, .FALSE., WWGOT, STATUS )

*  Clear the Domain and Title values which will have been inherited from the
*  supplied FrameSet.
      CALL AST_CLEAR( WWGOT, 'DOMAIN', STATUS )
      CALL AST_CLEAR( WWGOT, 'TITLE', STATUS )

*  "What we want":
*  ---------------

*  Extract a copy of the specified axis from the Current Frame.
      AXES( 1 ) = IAXIS
      FRMI = AST_PICKAXES( CFRM, 1, AXES, TMAP, STATUS )

*  When a SkyAxis is extracted from a SkyFrame, its Format and Digits
*  attributes are set, even if they were not set in the SkyFrame. This means
*  that Plot does not remove trailing zeros from the formatted axis values.
*  To avoid this, explicitly clear the Format and Digits attributes for the
*  extracted axis unless values have been set for them in the original
*  Current Frame.
      ATTR = 'FORMAT('
      IAT = 7
      CALL CHR_PUTI( IAXIS, ATTR, IAT )
      CALL CHR_APPND( ')', ATTR, IAT )

      IF( .NOT. AST_TEST( CFRM, ATTR( : IAT ), STATUS ) ) THEN
         CALL AST_CLEAR( FRMI, 'FORMAT(1)', STATUS )
      END IF

      ATTR( : 6 ) = 'DIGITS'
      IF( .NOT. AST_TEST( CFRM, ATTR( : IAT ), STATUS ) ) THEN
         CALL AST_CLEAR( FRMI, 'DIGITS(1)', STATUS )
      END IF

*  If necessary, create a Frame to represent distance along the curve.
      IF( DIST .OR. XMAP .EQ. 'DISTANCE' ) THEN

*  If the units are the same on all axes of CFRM, we use the axis
*  specified by IAXIS. Otherwise we create a new 1D Frame.
         IF( SAMEUN ) THEN
            FRM0 = AST_COPY( FRMI, STATUS )
            CALL AST_SETC( FRM0, 'LABEL(1)', 'Offset', STATUS )

         ELSE
            FRM0 = AST_FRAME( 1, ' ', STATUS )
            CALL AST_SETC( FRM0, 'LABEL(1)', 'Normalised offset',
     :                        STATUS )
         END IF

*  Set an appropriate Symbol attribute.
         CALL AST_SETC( FRM0, 'SYMBOL(1)', 'OFFSET ', STATUS )

      ELSE
         FRM0 = AST__NULL
      END IF

*  "What we want" is represented by a 2-D CmpFrame. If the X axis is not
*  being annotated with distance along the curve, the first axis is copied
*  from the specified axis in the Current Frame of the supplied FrameSet. If
*  the X axis is being annotated with distance along the curve, the first
*  axis is still copied from the specified axis in the Current Frame but only
*  if all axes have the same units - otherwise a new default Axis is used.
      IF( DIST ) THEN
         FR1 = AST_CLONE( FRM0, STATUS )
      ELSE
         FR1 = AST_CLONE( FRMI, STATUS )
      END IF

*  The second (data) axis will be a FluxFrame is possible. Otherwise it
*  will be a default 1-D Axis. We can use a FluxFrame if the units of the
*  NDF data array can be used to describe any of the flux systems
*  supported by the AST FluxFrame class. To test this create a new
*  FluxFrame and set its units to the supplied data units. Note, the call
*  to AST_SETC may generate an AST__BADUN error, so check the STATUS
*  before invoking  AST_SETC.
      FR2 = AST_FLUXFRAME( AST__BAD, AST__NULL, ' ', STATUS )

      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL AST_SETC( FR2, 'Unit(1)', DUNIT, STATUS )

*  Get the default System value from the FluxFrame. This will depend on
*  the units. If the units do not correspond to any of the supported flux
*  systems, then an error (AST__BADUN) will be reported. Check for this
*  error and annul it if it occurs, create a default simple Frame to
*  use instead of the FluxFrame, and combine it with the X axis Frame
*  into a CmpFrame.
      TEXT = AST_GETC( FR2, 'System', STATUS )
      IF( STATUS .EQ. AST__BADUN ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL AST_ANNUL( FR2, STATUS )
         FR2 = AST_FRAME( 1, ' ', STATUS )
         WWWANT = AST_CMPFRAME( FR1, FR2, ' ', STATUS )

*  If the data units can be used with one of the flux systems supported by
*  the AST FluxFrame class...
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Fix the System value explicit to the value determined by the supplied
*  data units, and then clear the units (they are re-instated below).
         CALL AST_SETC( FR2, 'System', AST_GETC( FR2, 'System',
     :                                           STATUS ), STATUS )
         CALL AST_CLEAR( FR2, 'Unit(1)', STATUS )

*  If the X axis Frame is a SpecFrame, create a SpecFluxFrame rather than
*  a CmpFrame to describe the combination of flux and spectral position.
*  This has the advantage that it supports automatic scaling of the Y
*  axis into other flux systems.
         IF( AST_ISASPECFRAME( FR1, STATUS ) ) THEN
            WWWANT = AST_SPECFLUXFRAME( FR1, FR2, ' ', STATUS )

*  If the X axis Frame is not a SpecFrame, create a CmpFrame combining
*  the axes.
         ELSE
            WWWANT = AST_CMPFRAME( FR1, FR2, ' ', STATUS )
         END IF

      END IF

      CALL AST_ANNUL( FR1, STATUS )
      CALL AST_ANNUL( FR2, STATUS )

*  Set the label, symbol and units for the data axis (axis 2).
      CALL KPG1_SAXAT( INDF, MCOMP, 2, ( YMAP .EQ. 'VALUELOG' ),
     :                 WWWANT, STATUS )

*  Set the Domain of the "what we want" Frame to DATAPLOT (a special
*  Domain used to indicate a 2D Frame with one dependant axis and one
*  independant axis).
      CALL AST_SETC( WWWANT, 'DOMAIN', 'DATAPLOT', STATUS )

*  "Uniform":
*  ----------
*  A 2D CmpFrame. Frame 1 (axis 1) is either "grid", "distance"
*  or axis value, depending on XMAP.
      IF( XMAP .EQ. 'PIXEL' ) THEN
         FR1 = AST_GETFRAME( IWCS, AST__BASE, STATUS )

      ELSE IF( XMAP.EQ. 'DISTANCE' ) THEN
         FR1 = AST_CLONE( FRM0, STATUS )

      ELSE
         FR1 = AST_CLONE( FRMI, STATUS )

      END IF

*  Frame 2 (axis 2) is a copy of the second axis of the What we want"
*  frame.
      AXES( 1 ) = 2
      FR2 = AST_PICKAXES( WWWANT, 1, AXES, TMAP, STATUS )

*  Combine them.
      UNIFRM = AST_CMPFRAME( FR1, FR2, ' ', STATUS )
      CALL AST_ANNUL( FR1, STATUS )
      CALL AST_ANNUL( FR2, STATUS )

*  Set the new Domain ("AGI_WORLD").
      CALL AST_SETC( UNIFRM, 'DOMAIN', 'AGI_WORLD', STATUS )

*  Now create the returned FrameSet.
*  =================================

*  Create a FrameSet holding the "what we've got" Frame. This is Frame 1
*  and is initially both the Base and Current Frame.
      FSET = AST_FRAMESET( WWGOT, ' ', STATUS )

*  Add the "uniform" Frame. This is Frame 2, and becomes the new Current
*  Frame.
      CALL AST_ADDFRAME( FSET, AST__BASE, MAP1, UNIFRM, STATUS )

*  Add the "what we want" Frame. This is Frame 3 and becomes the new
*  Current Frame.
      CALL AST_ADDFRAME( FSET, AST__BASE, MAP2, WWWANT, STATUS )

*  Add the supplied FrameSet into the returned FrameSet, connecting the
*  Base (GRID) Frame to the "what we've got" Frame using a PermMap which
*  maps axis 1 of the "w.w.got" Frame (GRID) onto the GRID Frame of the
*  supplied FrameSet. Temporarily make the GRID Frame current in IWCS since
*  AST_ADDFRAME uses the current Frame.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
      CALL AST_SETI( IWCS, 'CURRENT', AST_GETI( IWCS, 'BASE', STATUS ),
     :               STATUS )

      INPRM( 1 ) = 1
      INPRM( 2 ) = 0
      CALL AST_ADDFRAME( FSET, AST__BASE, AST_PERMMAP( 2, INPRM, 1, 1,
     :                                                 0.0D0, ' ',
     :                                                 STATUS ),
     :                   IWCS, STATUS )

      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Set the Base Frame to correspond to the "uniform" Frame.
      CALL AST_SETI( FSET, 'BASE', 2, STATUS )

*  Set the Current Frame to correspond to the "what we want" Frame.
      CALL AST_SETI( FSET, 'CURRENT', 3, STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Free the workspace.
      CALL PSX_FREE( IPG, STATUS )
      CALL PSX_FREE( IPW, STATUS )
      CALL PSX_FREE( IPD, STATUS )

*  Export the returned FrameSet pointer.
      CALL AST_EXPORT( FSET, STATUS )

*  If an error has occurred, annul the returned FrameSet pointer.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( FSET, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
