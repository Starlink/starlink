      SUBROUTINE KPG1_LIKE( INDF1, INDF2, LIKWCS, INDF3, STATUS )
*+
*  Name:
*     KPG1_LIKE

*  Purpose:
*     Creates a section from an NDF that matches the area of another
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LIKE( INDF1, INDF2, LIKWCS, INDF3, STATUS )

*  Description:
*     This routine returns an identifier for a section of a supplied
*     NDF (INDF1) that covers the same area as a second template NDF
*     (INDF2). The area matched can be either the pixel area, or the WCS
*     area.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The first NDF, from which a section is required.
*     INDF2 = INTEGER (Given)
*        The second NDF, which defines teh required area.
*     LIKWCS = LOGICAL (Given)
*        If .TRUE., then the section selected from INDF1 matches the WCS
*        bounding box of INDF2. If .FALSE., then the section selected
*        from INDF1 matches the pixel index bounding box of INDF2.
*     INDF3 = INTEGER (Returned)
*        The required section of INDF1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-2009 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      LOGICAL LIKWCS

*  Arguments Given:
      INTEGER INDF3

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      DOUBLE PRECISION DLBND1     ! Input NDF lower bounds
      DOUBLE PRECISION DLBND2( NDF__MXDIM ) ! Input NDF lower bounds
      DOUBLE PRECISION DUBND1     ! Input NDF upper bounds
      DOUBLE PRECISION DUBND2( NDF__MXDIM ) ! Input NDF upper bounds
      DOUBLE PRECISION XL( NDF__MXDIM ) ! Point on lower bound
      DOUBLE PRECISION XU( NDF__MXDIM ) ! Point on upper bound
      INTEGER FS                  ! Template->input FrameSet
      INTEGER I                   ! Axis index
      INTEGER IWCS1               ! WCS FrameSet from input
      INTEGER IWCS2               ! WCS FrameSet from template
      INTEGER LBND1( NDF__MXDIM ) ! Template NDF lower bounds
      INTEGER LBND2( NDF__MXDIM ) ! Input NDF lower bounds
      INTEGER MAP                 ! Template->input Mapping
      INTEGER NDIM1               ! Number of template dimensions
      INTEGER NDIM2               ! Number of input dimensions
      INTEGER UBND1( NDF__MXDIM ) ! Template NDF upper bounds
      INTEGER UBND2( NDF__MXDIM ) ! Input NDF upper bounds
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Get the pixel index bounds of the template.
      CALL NDF_BOUND( INDF2, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )

*  If WCS bounds are being used, we need to transform these pixel index
*  bounds into the PIXEL frame of INDF1, aligning in some suitable common
*  WCS frame.
      IF( LIKWCS ) THEN

*  Begin an AST context.
         CALL AST_BEGIN( STATUS )

* Get the pixel index bounds of the template.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1,
     :                   STATUS )

*  Get the WCS FrameSets from the two input NDFs.
         CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )
         CALL KPG1_GTWCS( INDF2, IWCS2, STATUS )

*  Align them in some suitable WCS co-ordinate Frame.
         CALL AST_INVERT( IWCS1, STATUS )
         CALL AST_INVERT( IWCS2, STATUS )
         FS = AST_CONVERT( IWCS2, IWCS1, ' ', STATUS )

* Get the Mapping from GRID coords in INDF2 to GRID coords in INDF1.
         IF( FS .NE. AST__NULL ) THEN
            MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )

*  Get the floating point bounds of the INDF2 pixel box, and convert to
*  the GRID Frame
            DO I = 1, NDIM2
               DLBND2( I ) = 0.5D0
               DUBND2( I ) = DBLE( UBND2( I ) - LBND2( I ) ) + 1.5D0
            END DO

*  Loop over all pixel axes.
            DO I = 1, NDIM1

*  Get the corresponding GRID bounds within INDF1.
               CALL AST_MAPBOX( FS, DLBND2, DUBND2, .TRUE., I,
     :                          DLBND1, DUBND1, XL, XU, STATUS )

*  Convert to pixel indices
               UBND1( I ) = LBND1( I ) + NINT( DUBND1 ) - 2
               LBND1( I ) = LBND1( I ) + NINT( DLBND1 ) - 1
            END DO

*  Report an error if the two supplied NDFs could not be aligned.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF1', INDF1 )
            CALL NDF_MSG( 'NDF2', INDF2 )
            CALL ERR_REP( ' ', 'Unable to find a common WCS frame in '//
     :                    'which to align ^NDF1 and ^NDF2.', STATUS )
         END IF

*  End the AST context.
         CALL AST_END( STATUS )

*  If the pixel index bounds are being used, copy them.
      ELSE
         NDIM1 = NDIM2
         DO I = 1, NDIM1
            LBND1( I ) = LBND2( I )
            UBND1( I ) = UBND2( I )
         END DO
      END IF

*  Cur the required section from INDF1.
      CALL NDF_SECT( INDF1, NDIM1, LBND1, UBND1, INDF3, STATUS )

      END

