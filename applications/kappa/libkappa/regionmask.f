      SUBROUTINE REGIONMASK( STATUS )
*+
*  Name:
*     REGIONMASK

*  Purpose:
*     Applies a mask to a region of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL REGIONMASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine masks out a region of an NDF by setting pixels to
*     the bad value, or to a specified constant value. The region to
*     be masked is specified by a file (see Parameter REGION) that
*     should contain a description of the region in a form readable
*     by the Starlink AST library (see SUN/211 or SUN/210). Such formats
*     include AST's own native format and other formats that can be
*     converted automatically to an AST Region (e.g. IVOA MOC and STC-S
*     regions).  AST Regions can be created, for instance, using the
*     Starlink ATOOLS package (a high-level interface to the facilities
*     of the AST library).

*  Usage:
*     regionmask in region out

*  ADAM Parameters:
*     CONST = LITERAL (Given)
*        The constant numerical value to assign to the region, or the
*        string "Bad".  ["Bad"]
*     IN = NDF (Read)
*        The name of the input NDF.
*     INSIDE = _LOGICAL (Read)
*        If a TRUE value is supplied, the constant value is assigned
*        to the inside of the region. Otherwise, it is assigned to the
*        outside.  [TRUE]
*     OUT = NDF (Write)
*        The name of the output NDF.
*     REGION = FILENAME (Read)
*        The name of the file containing a description of the Region.
*        This can be a text file holding a dump of an AST Region (any
*        sub-class of Region may be supplied - e.g. Box, Polygon, CmpRegion,
*        Prism, etc.), or any file that can be converted automatically to
*        an AST Region (for instance an IVOA MOC in text or FITS format,
*        an IVOA STC-S region). An NDF may also be supplied, in which case
*        the rectangular boundary of the NDF is used as the Region. If the
*        axes spanned by the Region are not the same as those of the current
*        WCS Frame in the input NDF, an attempt will be made to create an
*        equivalent new Region that does match the current WCS Frame. An
*        error will be reported if this is not possible.

*  Examples:
*     regionmask a1060 galaxies.txt a1060_sky
*        This copies input NDF a1060 to the output NDF a1060_sky,
*        setting pixels bad if they are contained within the Region
*        specified in text file "galaxies.txt".

*  Related Applications:
*     KAPPA: ARDMASK;
*     ATOOLS: ASTBOX, ASTCMPREGION, ASTELLIPSE, ASTINTERVAL, ASTPOLYGON.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     30-OCT-2008 (DSB):
*        Original version.
*     29-SEP-2009 (DSB):
*        Modified to allow Region and WCS Frame to have different axes.
*     30-SEP-2009 (DSB):
*        Correct half-pixel shift.
*     14-MAY-2019 (DSB):
*        Update prologue for new ATL facilities (i.e. read MOCs).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL AST_ISAREGION

*  Local Variables:
      CHARACTER CONTXT*40        ! Text version of constant value
      DOUBLE PRECISION CONST     ! The constant to assign
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! The shift for each axis
      INTEGER FSET               ! Region -> PIXEL FrameSet
      INTEGER I                  ! Lop count
      INTEGER INDF1              ! Identifier for the source NDF
      INTEGER INDF2              ! Identifier for the output NDF
      INTEGER IPIX               ! Index of PIXEL Frame within IWCS
      INTEGER IREG               ! AST Region
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER MAP                ! Region->pixel Mapping
      INTEGER NAX                ! Number of pixel axes
      INTEGER NEWREG             ! Region matching WCS Frame
      LOGICAL BAD                ! Assign bad values to the region?
      LOGICAL INSIDE             ! Assign value to inside of region?
      LOGICAL VAR                ! Does NDF VARIANCE component exist?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get an AST Region.
      CALL KPG1_GTOBJ( 'REGION', 'Region', AST_ISAREGION, IREG, STATUS )

*  Get the WCS FrameSet from the NDF.
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )

*  We now try to get a region in which the axes are the same in number and
*  type (but not necessarily order - AST_CONVERT, called later, will take
*  account of any difference in axis order) as those spanned by the current
*  WCS Frame.
      CALL ATL_MATCHREGION( IREG, IWCS, NEWREG, STATUS )

*  We need a Mapping from the co-ordinate system represented by the
*  Region to the pixel co-ordinate system of the NDF. The AST_COVNERT
*  routine converts between current Frames, so we need to make the PIXEL
*  Frame the current Frame in the NDFs WCS FrameSet.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
      CALL AST_SETI( IWCS, 'Current', IPIX, STATUS )
      FSET = AST_CONVERT( NEWREG, IWCS, ' ', STATUS )

*  Report an error and abort if the conversion was not defined.
      IF( FSET .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL NDF_MSG( 'N', INDF1 )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Cannot convert between the co-ordinate '//
     :                 'system of the supplied Region and the current'//
     :                 ' WCS co-ordinate system of the NDF ''^N''.',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the Mapping from this FrameSet and modify it to take account of
*  the half pixel shift required by KPS1_RMASK.
      NAX = AST_GETI( FSET, 'Naxes', STATUS )

      DO I = 1, NAX
         SHIFT( I ) = 0.5D0
      END DO

      MAP = AST_CMPMAP( AST_GETMAPPING( FSET, AST__BASE, AST__CURRENT,
     :                                  STATUS ),
     :                  AST_SHIFTMAP( NAX, SHIFT, ' ', STATUS ),
     :                  .TRUE., ' ', STATUS )

*  Create the output NDF as a copy of the input NDF.
      CALL LPG_PROP( INDF1, 'Data,Variance,Quality,Axis,Units,WCS',
     :               'OUT', INDF2, STATUS )

*  Get the string representing the constant value to assign.
      CALL PAR_MIX0D( 'CONST', 'Bad', VAL__MIND, VAL__MAXD, 'Bad',
     :                 .FALSE., CONTXT, STATUS )

*  Get the appropriate numerical value from the string.
      IF( CONTXT .EQ. 'BAD' ) THEN
         CONST = VAL__BADD
      ELSE
         CALL CHR_CTOD( CONTXT, CONST, STATUS )
      END IF

*  See if the value is to be assigned to the inside or the outside of
*  the region.
      CALL PAR_GET0L( 'INSIDE', INSIDE, STATUS )

*  First mask the Data array.
      CALL KPS1_RMASK( INDF2, 'Data', NEWREG, MAP, INSIDE, CONST,
     :                 STATUS )

*  If the Variance array is defined, also mask the Variance array.
      CALL NDF_STATE( INDF2, 'Variance', VAR, STATUS )
      IF( VAR ) THEN
         CALL KPS1_RMASK( INDF2, 'Variance', NEWREG, MAP, INSIDE,
     :                    CONST, STATUS )
      END IF

*  Arrive here if an error occurrs.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'REGIONMASK_ERR', 'REGIONMASK: Failed to mask '//
     :                 'an NDF using an AST Region.', STATUS )
      END IF

      END
