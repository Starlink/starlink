      SUBROUTINE SETBOUND( STATUS )
*+
*  Name:
*     SETBOUND

*  Purpose:
*     Sets new bounds for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETBOUND( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application sets new pixel-index bounds for an NDF, either
*     trimming it to remove unwanted pixels, or padding it with bad
*     pixels to achieve the required shape.  The number of dimensions
*     may also be altered.  The NDF is accessed in update mode and
*     modified in situ, preserving existing pixel values which lie
*     within the new bounds.

*  Usage:
*     setbound ndf

*  ADAM Parameters:
*     LIKE = NDF (Read)
*        This parameter may be used to specify an NDF which is to be
*        used as a shape template.  If such a template is supplied, then
*        its bounds will be used to determine the new shape required
*        for the NDF specified via the NDF parameter.  By default no
*        template will be used and the new shape will be determined
*        by means of a section specification applied to the NDF being
*        modified (see the "Examples"). [!]
*     NDF = NDF (Read and Write)
*        The NDF whose bounds are to be modified.  In normal use, an NDF
*        section will be specified for this parameter (see the
*        "Examples") and the routine will use the bounds of this section
*        to determine the new bounds required for the base NDF from
*        which the section is drawn.  The base NDF is then accessed in
*        update mode and its bounds are modified in situ to make them
*        equal to the bounds of the section specified.  If a section is
*        not specified, then the NDF's shape will only be modified if a
*        shape template is supplied via the LIKE parameter.

*  Examples:
*     setbound datafile(1:512,1:512)
*        Sets the pixel-index bounds of the NDF called datafile to be
*        (1:512,1:512), either by trimming off unwanted pixels or by
*        padding out with bad pixels, as necessary.
*     setbound alpha(:7,56:)
*        Modifies the NDF called alpha so that its first dimension has
*        an upper bound of 7 and its second dimension has a lower bound
*        of 56.  The lower bound of the first dimension and the upper
*        bound of the second dimension remain unchanged.
*     setbound ndf=kg74b(,5500.0~100.0)
*        Sets new bounds for the NDF called kg74b.  The bounds of the
*        first dimension are left unchanged, but those of the second
*        dimension are changed so that this dimension has an extent of
*        100.0 centred on 5500.0, using the physical units in which
*        this second dimension is calibrated.
*     setbound newspec like=oldspec
*        Changes the bounds of the NDF newspec so that they are equal
*        to the bounds of the NDF called oldspec.
*     setbound xflux(:2048) like=xflux
*        Extracts the section extending from the lower bound of the
*        1-dimensional NDF called xflux up to pixel 2048, and then
*        modifies the bounds of this section to be equal to the
*        original bounds of xflux, replacing xflux with this new NDF.
*        This leaves the final shape unchanged, but sets all pixels
*        from 2049 onwards to be equal to the bad-pixel value.
*     setbound whole(5:10,5:10) like=whole(0:15,0:15)
*        Extracts the section (5:10,5:10) from the base NDF called
*        whole and then sets its bounds to be equal to those of the
*        section whole(0:15,0:15), replacing whole with this new NDF.
*        The effect is to select a 6-pixel-square region from the
*        original NDF and then to pad it with a 5-pixel-wide border of
*        bad pixels.

*  Notes:
*     This routine modifies the NDF in situ and will not release unused
*     file space if the size of the NDF is reduced.  If recovery of
*     unused file space is required, then the related application
*     NDFCOPY should be used.  This will copy the selected region of an
*     NDF to a new data structure from which any unused space will be
*     eliminated.

*  Related Applications:
*     KAPPA: NDFCOPY, SETORIGIN; Figaro: ISUBSET.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-MAR-1991 (RFWS):
*        Original version.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel-index bounds of section
      INTEGER LBNDL( NDF__MXDIM ) ! Lower bounds of shape template
      INTEGER NDF1               ! NDF (section) ID supplied
      INTEGER NDF1B              ! ID for base NDF to be modified
      INTEGER NDF2               ! ID for shape template NDF
      INTEGER NDIM               ! Number of section dimensions
      INTEGER NDIML              ! Number of shape template dimensions
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel-index bounds of section
      INTEGER UBNDL( NDF__MXDIM ) ! Upper bounds of shape template
      LOGICAL LIKE               ! Shape template supplied?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF to be modified and obtain its
*  bounds (normally a section will be supplied).
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF1, STATUS )
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Obtain an identifier for the base NDF associated with the section.
      CALL NDF_BASE( NDF1, NDF1B, STATUS )

*  Defer error reporting and attempt to obtain another NDF via the LIKE
*  parameter to act as a template.
      LIKE = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', NDF2, STATUS )

*  If a null template was given, then simply annul the error.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Otherwise, obtain the bounds of the template.
         ELSE
            LIKE = .TRUE.
            CALL NDF_BOUND( NDF2, NDF__MXDIM, LBNDL, UBNDL, NDIML,
     :                      STATUS )
         END IF

*  End the error context.
         CALL ERR_RLSE
      END IF

*  Set the NDF bounds to be equal to those of the section which was
*  supplied (note that if a base NDF was given, then this will have no
*  effect).
      CALL NDF_SBND( NDIM, LBND, UBND, NDF1B, STATUS )

*  If a shape template was given, further modify the NDF's shape to
*  match it.
      IF ( LIKE ) CALL NDF_SBND( NDIML, LBNDL, UBNDL, NDF1B, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETBOUND_ERR',
     :   'SETBOUND: Error setting new pixel bounds for an NDF.',
     :   STATUS )
      END IF

      END
