      SUBROUTINE WCSSLIDE( STATUS )
*+
*  Name:
*     WCSSLIDE

*  Purpose:
*     Applies a translational correction to the WCS in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSSLIDE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies the WCS information in an NDF so that
*     the WCS position of a given pixel is moved by specified amount
*     along each WCS axis. The shifts to use are specified either by
*     an absolute offset vector given by the ABS parameter or by the
*     difference between a fiducial point and a standard object given
*     by the FID and OBJ parameters respectively. In each case the
*     co-ordinates are specified in the NDF's current WCS co-ordinate
*     Frame.

*  Usage:
*     wcsslide ndf abs

*  ADAM Parameters:
*     ABS( ) = _DOUBLE (Read)
*        Absolute shift for each WCS axis.  The number of values
*        supplied must match the number of WCS axes in the NDF.  It is
*        only used if STYPE="Absolute".  Offsets for celestial longitude
*        and latitude axes should be specified in arcseconds.  Offsets
*        for all other types of axes should be given directly in the
*        units of the axis.
*     FID = LITERAL (Read)
*        A comma-separated list of formatted axis values giving the
*        position of the fiducial point in WCS co-ordinates.  The number
*        of values supplied must match the number of WCS axes in the
*        NDF.  It is only used if STYPE="Relative".
*     NDF = NDF (Update)
*        The NDF to be translated.
*     OBJ = LITERAL (Read)
*        A comma-separated list of formatted axis values giving the
*        position of the standard object in WCS co-ordinates.  The
*        number of values supplied must match the number of WCS axes in
*        the NDF.  It is only used if STYPE="Relative".
*     STYPE = LITERAL (Read)
*        The sort of shift to be used.  The choice is "Relative" or
*        "Absolute".  ["Absolute"]

*  Examples:
*     wcsslide m31 [32,23]
*        The (RA,Dec) axes in the NDF m31 are shifted by 32 arcseconds
*        in right ascension and 23 arcseconds in declination.
*     wcsslide speca stype=rel fid=211.2 obj=211.7
*        The spectral axis in the NDF speca (which measures frequency in
*        GHz), is shifted by 0.5 GHz (i.e. 211.7--211.2).
*     wcsslide speca stype=abs abs=0.5
*        This does just the same as the previous example.

*  Notes:
*     -  The correction is affected by translating pixel co-ordinates by
*     a constant amount before projection them into WCS co-ordinates.
*     Therefore, whilst the translation will be constant across the
*     array in pixel co-ordinates, it may vary in WCS co-ordinates
*     depending on the nature of the pixel->WCS transformation.  The
*     size of the translation in pixel co-ordinates is chosen in order
*     to produce the required shift in WCS co-ordinates at the OBJ
*     position (if STYPE is "Relative"), or at the array centre (if
*     STYPE is "Absolute").

*  Related Applications:
*     KAPPA: SLIDE.

*  Implementation Status:
*     -  There can be an arbitrary number of NDF dimensions.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     16-APR-2008 (DSB):
*        Original Version.
*     25-FEB-2010 (DSB):
*        Fix nasty bug in calculation of shifts when using ABS parameter.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST definitions and declarations
      INCLUDE 'NDF_PAR'          ! NDF system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! Attribute name
      CHARACTER DOM*50           ! Domain name for axis
      CHARACTER STYPE*16         ! Type of shift to be supplied
      DOUBLE PRECISION FID( NDF__MXDIM ) ! WCS coords of fiducial point
      DOUBLE PRECISION GFID( NDF__MXDIM )! GRID coords of fiducial point
      DOUBLE PRECISION GOBJ( NDF__MXDIM )! GRID coords of standard object
      DOUBLE PRECISION OBJ( NDF__MXDIM ) ! WCS coords of standard object
      DOUBLE PRECISION P1( NDF__MXDIM )! Start of geodesic offset
      DOUBLE PRECISION P2( NDF__MXDIM )! Target for geodesic offset
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Translation vector
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions in pixels
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Used length of string
      INTEGER INDF               ! Identifier for input NDF
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER J                  ! Loop variable
      INTEGER MAP0               ! Mapping that shifts pixel coords
      INTEGER MAP1               ! Original GRID->WCS Mapping
      INTEGER MAP2               ! New GRID->WCS Mapping
      INTEGER NDIM               ! Number of dimensions in NDF
      INTEGER NPIX               ! Number of pixel axes in NDF
      INTEGER NWCS               ! Number of WCS axes in NDF
      INTEGER WCSFRM             ! WCS Frame
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Open the input NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Get its WCS FrameSet.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  See how many PIXEL and WCS axes there are.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  See if we want a relative or absolute shift.
      CALL PAR_CHOIC( 'STYPE', 'ABSOLUTE', 'ABSOLUTE,RELATIVE', .TRUE.,
     :                STYPE, STATUS )

*  Get the absolute shift directly as the value of the ABS parameter.
      IF ( STYPE .EQ. 'ABSOLUTE' ) THEN
         CALL PAR_EXACD( 'ABS', NWCS, SHIFT, STATUS )

*  If any of the current Frame axes are SKY axes, then the supplied
*  offset values are interpreted as arc-seconds. Convert them to
*  radians.
         DO I = 1, NWCS
            ATTR = 'Domain('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            DOM = AST_GETC( IWCS, ATTR, STATUS )
            IF( DOM .EQ. 'SKY' ) THEN
               SHIFT( I ) = AST__DD2R*SHIFT( I )/3600.0D0
            END IF
         END DO

*  Get the dimensions of the NDF.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Get the GRID coords at the centre of the array and convert into WCS
*  coords. This becomes the FID point.
         DO I = 1, NPIX
            GFID( I ) = 0.5*( DIM( I ) + 1 )
         END DO
         CALL AST_TRANN( IWCS, 1, NPIX, 1, GFID, .TRUE., NWCS, 1, FID,
     :                   STATUS )

*  P1 is the start of the geodesic and P2 is the end of the geodesic.
*  Initialise them both to the FID point obtained above.
         DO I = 1, NWCS
            P1( I ) = FID( I )
            P2( I ) = FID( I )
         END DO

*  Offset away from this WCS position by the required amount on each
*  axis.
         DO I = 1, NWCS

*  Move the geodesic end-point along the I'th axis by a small amount.
            P2( I ) = 1.001*P1( I )
            IF( P2( I ) .EQ. P1( I ) ) P2( I ) = 1.0

*  Move away from P1 towards P2, by the distance given by SHIFT(I). The
*  resulting position is stored in OBJ.
            CALL AST_OFFSET( IWCS, P1, P2, SHIFT( I ), OBJ, STATUS )

*  The end of this offset becomes the start of the next offset.
            DO J = 1, NWCS
               P1( J ) = OBJ( J )
               P2( J ) = OBJ( J )
            END DO

         END DO

*  Convert the final OBJ position into GRID coords.
         CALL AST_TRANN( IWCS, 1, NWCS, 1, OBJ, .FALSE., NPIX, 1, GOBJ,
     :                   STATUS )

*  Alternatively, get the shift as the difference between the FID and
*  OBJ parameters.
      ELSE

*  Get the co-ordinates of the fiducial point.
         CALL KPG1_GTPOS( 'FID', IWCS, .FALSE., FID, GFID, STATUS )

*  Get the co-ordinates of the standard object.
         CALL KPG1_GTPOS( 'OBJ', IWCS, .FALSE., OBJ, GOBJ, STATUS )

      END IF

*  Form the shift for each pixel axis.
      DO I = 1, NPIX
         IF( GFID( I ) .NE. AST__BAD .AND.
     :       GOBJ( I ) .NE. AST__BAD ) THEN

            SHIFT( I ) = GFID( I ) - GOBJ( I )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unable to calculate the pixel '//
     :                    'co-ordinates of the specified points.',
     :                    status )
         END IF
      END DO

*  Construct a Mapping that shifts pixel coords by the required amount.
      MAP1 = AST_SHIFTMAP( NPIX, SHIFT, ' ', STATUS )

*  Get the GRID->WCS Mapping.
      MAP0 = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Combine the two Mappings.
      MAP2 = AST_CMPMAP( MAP1, MAP0, .TRUE., ' ', STATUS )

*  Get a pointer ot the WCS Frame, and then remove it from the FrameSet.
      WCSFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      CALL AST_REMOVEFRAME( IWCS, AST__CURRENT, STATUS )

*  Add it back in again, using the new Mapping to connect it to the base
*  (GRID) Frame.
      CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, WCSFRM, STATUS )

*  Store the modified FrameSet in the NDF.
      CALL NDF_ PTWCS( IWCS, INDF, STATUS )

*  Exit the NDF context.
      CALL NDF_END( STATUS )

*  Exit the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSSLIDE_ERR1', 'WCSSLIDE: Unable to shift '//
     :                 'the WCS in an NDF.', STATUS )
      END IF

      END
