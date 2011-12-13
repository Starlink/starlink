      SUBROUTINE KPS1_ELPR5( INDF1, INDF2, PA, RATIO, NSTEP, RMIN, RMAX,
     :                       XC, YC, STATUS )
*+
*  Name:
*     KPS1_ELPR5

*  Purpose:
*     Propagates WCS for ELPROF (radial mode).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ELPR5( INDF1, INDF2, PA, RATIO, NSTEP, RMIN, RMAX,
*                      XC, YC, STATUS )

*  Description:
*     This function adds WCS infor to INDF2 based on that in INDF1.
*     It assumes that INDF2 contains a 1-D profile in which each pixel
*     corredponds to an elliptcal annulus in INDF1.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The NDF identifier for the input 2D image.
*     INDF2 = INTEGER (Given)
*        The NDF identifier for the 1D output profile.
*     PA = REAL (Given)
*        The angle between the major axis of the ellipses and the first
*        axis, given in radians measured anti-clockwise.
*     RATIO = REAL (Given)
*        The ratio of the minor- to major-axis lengths.  In the range
*        zero to one.
*     NSTEP = INTEGER (Given)
*        The number of annuli required.
*     RMIN = REAL (Given)
*        The radius (measured on the major axis) of the inner edge of
*        the inner-most annulus.
*     RMAX = REAL (Given)
*        The radius (measured on the major axis) of the outer edge of
*        the outer-most annulus.
*     XC = REAL (Given)
*        The x pixel co-ordinate of the ellipse centre.
*     YC = REAL (Given)
*        The y pixel co-ordinate of the ellipse centre.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-2004 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      REAL PA
      REAL RATIO
      INTEGER NSTEP
      REAL RMIN
      REAL RMAX
      REAL XC
      REAL YC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FOREXP*200       ! Forward MathMap expression
      CHARACTER INVEXP( 2 )*200  ! Inverse MathMap expressions
      DOUBLE PRECISION GX        ! GRID X value at ellipse centre
      DOUBLE PRECISION GY        ! GRID Y value at ellipse centre
      DOUBLE PRECISION INA       ! Radius at start of profile
      DOUBLE PRECISION INB       ! Radius at end of profile
      DOUBLE PRECISION MATRIX( 4 )! Rotation matrix from (gx,gy) to (u,v)
      DOUBLE PRECISION OUTA      ! GRID coord at start of profile
      DOUBLE PRECISION OUTB      ! GRID coord at end of profile
      DOUBLE PRECISION PX        ! PIXEL X value at ellipse centre
      DOUBLE PRECISION PY        ! PIXEL Y value at ellipse centre
      DOUBLE PRECISION SHIFT( 2 )! Shifts from (gx,gy)
      INTEGER IAT                ! Used length of string
      INTEGER IAXIS              ! Index of AXIS Frame in IWCS1
      INTEGER ICURR              ! Index of original Current Frame in IWCS1
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS1
      INTEGER IWCS1              ! 2D WCS FrameSet from INDF1
      INTEGER IWCS2              ! 1D WCS FrameSet from INDF2
      INTEGER MAP1               ! A Mapping
      INTEGER MAP2               ! A Mapping
      INTEGER MAP3               ! A Mapping
      INTEGER MAP4               ! A Mapping
      INTEGER MAP5               ! A Mapping
      INTEGER MAP6               ! A Mapping
      INTEGER MAP7               ! A Mapping
      INTEGER NFRM               ! No. of Frames in IWCS1
      LOGICAL THERE              ! Is the AXIS structure in a defined state?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the WCS FrameSet from INDF1.
      CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )

*  Find the indices of the PIXEL and AXIS Frame in this FrameSet.
      CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX, STATUS )
      CALL KPG1_ASFFR( IWCS1, 'AXIS', IAXIS, STATUS )

*  Get the Mapping from the 2D PIXEL Frame to the 2D GRID (Base) Frame.
      MAP1 = AST_GETMAPPING( IWCS1, IPIX, AST__BASE, STATUS )

*  Use this Mapping to convert the supplied PIXEL centre into GRID coords.
      PX = DBLE( XC )
      PY = DBLE( YC )
      CALL AST_TRAN2( MAP1, 1, PX, PY, .TRUE., GX, GY, STATUS )
      CALL AST_ANNUL( MAP1, STATUS )

*  We construct a Mapping from the 2D GRID coordinate Frame in INDF1 to the
*  1D GRID coordinate Frame in INDF2. This is a series CmpMap made up
*  from several component Mappings. First use a ShiftMap to convert 2D GRID
*  coords to offsets from (GX,GY).
      SHIFT( 1 ) = -GX
      SHIFT( 2 ) = -GY
      MAP1 = AST_SHIFTMAP( 2, SHIFT, ' ', STATUS )

*  Next create a MatrixMap which rotates the offsets created by the above
*  ShiftMap so that the first axis is parallel to the ellipse major axis.
*  We call the rotated offsets (U,V).
      MATRIX( 1 ) = COS( PA )
      MATRIX( 2 ) = SIN( PA )
      MATRIX( 3 ) = -MATRIX( 2 )
      MATRIX( 4 ) = MATRIX( 1 )
      MAP2 = AST_MATRIXMAP( 2, 2, 0, MATRIX, ' ', STATUS )

*  The next Mapping goes from (U,V) to "R", the distance from (XC,YC)
*  in pixels along the major axis, at which an ellipse through (U,V)
*  cuts the major axis. We implement this using a MathMap. The inverse
*  transformation results in U=R, and V=0, that is,  the (U,V) coords of
*  a point a distance R out along the major axis.
      FOREXP = ' '
      IAT = 0
      CALL CHR_APPND( 'R=SQRT( U**2 +(V/(', FOREXP, IAT )
      CALL CHR_PUTR( RATIO, FOREXP, IAT )
      CALL CHR_APPND( '))**2)', FOREXP, IAT )

      INVEXP( 1 ) = 'U=R'
      INVEXP( 2 ) = 'V=0.0'

      MAP3 = AST_MATHMAP( 2, 1, 1, FOREXP, 2, INVEXP,
     :                    'SIMPFI=1,SIMPIF=1', STATUS )

*  The final Map is a WinMap which maps R values onto GRID coords in the
*  1D profile NDF.
      INA = RMIN
      INB = RMAX
      OUTA = 0.5
      OUTB = NSTEP + 0.5
      MAP4 = AST_WINMAP( 1, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Combine these Mapping in series.
      MAP5 = AST_CMPMAP( MAP1, MAP2, .TRUE., ' ', STATUS )
      MAP6 = AST_CMPMAP( MAP3, MAP4, .TRUE., ' ', STATUS )
      MAP7 = AST_CMPMAP( MAP5, MAP6, .TRUE., ' ', STATUS )

*  Note the index of the current Frame of the image WCS FrameSet.
      ICURR = AST_GETI( IWCS1, 'Current', STATUS )

*  Rename the PIXEL Frame in the input WCS FrameSet as ELPROF_PIXEL,
*  and the AXIS Frame (if defined) as ELPROF_AXIS
      CALL AST_SETI( IWCS1, 'Current', IPIX, STATUS )
      CALL AST_SETC( IWCS1, 'Domain', 'ELPROF_PIXEL', STATUS )

      CALL NDF_STATE( INDF1, 'Axis', THERE, STATUS )
      IF( THERE ) THEN
         CALL AST_SETI( IWCS1, 'Current', IAXIS, STATUS )
         CALL AST_SETC( IWCS1, 'Domain', 'ELPROF_AXIS', STATUS )
      END IF

*  Get the WCS FrameSet from INDF2 (the 1D profile), and note how many
*  Frames there are in it.
      CALL KPG1_GTWCS( INDF2, IWCS2, STATUS )
      NFRM = AST_GETI( IWCS2, 'NFrame', STATUS )

*  Add the FrameSet from the image NDF into the FrameSet from the profile
*  NDF, using the Mapping found above to connect the 2D GRID Frame (the
*  Base Frame in IWCS1) to the 1D Grid Frame (the Base Frame in IWCS2).
      CALL AST_INVERT( MAP7, STATUS )
      CALL AST_SETI( IWCS1, 'Current',
     :               AST_GETI( IWCS1, 'Base', STATUS ), STATUS )
      CALL AST_ADDFRAME( IWCS2, AST__BASE, MAP7, IWCS1, STATUS )

*  Make sure the original current Frame in IWCS1 is the current Frame in
*  the merged FrameSet.
      CALL AST_SETI( IWCS2, 'Current', ICURR + NFRM, STATUS )

*  Store the resulting FrameSet back in INDF2.
      CALL NDF_PTWCS( IWCS2, INDF2, STATUS )

*  End the AST context.
      CALL AST_BEGIN( STATUS )

      END
