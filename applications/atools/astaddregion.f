      SUBROUTINE ASTADDREGION( STATUS )
*+
*  Name:
*     ASTADDREGION

*  Purpose:
*     Add a Region into a Moc.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTADDREGION( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies a Moc by combining it with a supplied
*     Region. The Region must be defined within a SkyFrame, or within
*     a CmpFrame that contains a SkyFrame. The Region will be converted
*     to ICRS before being combined with the Moc. The way in which they
*     are combined is determined by the CMODE parameter.
*
*     Note, since Moc is a subclass of Region this method can be used to
*     add a Moc into another Moc. In such cases, the data is transferred
*     from one Moc to another directly. For other classes of Region an
*     adaptive algorithm is used to find the HEALPix cells that are inside
*     the Region. An initial grid, corresponding to the HEALPix cells at
*     the order given by the Moc's "MinOrder" attribute, is placed over the
*     bounding box of the supplied Region. Each of these cells is tested
*     at 9 positions (corners, edge-centres and cell-centre). If all 9
*     positions are inside the supplied Region, then the whole cell is
*     assumed to be inside the Region. If no positions are inside the
*     supplied Region, then the whole cell is assumed to be outside the
*     Region. If there is a mix of inside and outside positions, the cell
*     is divided into four sub-cells at HEALPix order "MinOrder+1", and the
*     same test is applied to each sub-cell in turn. When the HEALPix order
*     reaches the value of the Moc's "MaxOrder" attribute, each cell is
*     tested only at the cell centre, and is assumed to be inside the
*     Region if the cell centre is in the Region.
*
*     This process means that contiguous "islands" or "holes" in the
*     supplied region may be missed if they are smaller than the cell size
*     associated with HEALPix order "MinOrder".

*  Usage:
*     astaddregion this cmode region result

*  ADAM Parameters:
*     CMODE = LITERAL (Read)
*        Indicates how the Moc and select pixels are to be combined. Any
*        of the following values may be supplied:
*        - "AND": The modified Moc is the intersection of the original
*        Moc and the selected pixels.
*        - "OR": The modified Moc is the union of the original Moc and
*        the selected pixels.
*        - "XOR": The modified Moc is the exclusive disjunction of the
*        original Moc and the selected pixels.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. Can be "AST", "XML",
*        "STCS", or any FitsChan encoding such as FITS-WCS. Only used
*        if the output object is written to a text file. An error is
*        reported if the output object cannot be written using the
*        requested format. ["AST"]
*     REGION = LITERAL (Read)
*        The Region to add to THIS.
*     RESULT = LITERAL (Read)
*        A file to receive the modified Moc. If it ends with ".fit", or
*        ".fits" (case insensitive), then a FITS file will be created
*        with the given name and the modified Moc is written out as a
*        binary table to an extension called "BINTABLE" in the named
*        FITS file. Any pre-existing FITS file is first deleted.
*     THIS = LITERAL (Read)
*        The Moc to be modified.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     11-DEC-2018 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAMOC
      EXTERNAL AST_ISAREGION

*  Local Variables:
      CHARACTER TEXT*10
      INTEGER RESULT
      INTEGER THIS
      INTEGER REGION
      INTEGER CMODE
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Moc.
      CALL KPG1_GTOBJ( 'THIS', 'Moc', AST_ISAMOC, THIS, STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'REGION', 'Region', AST_ISAREGION, REGION,
     :                 STATUS )

*  Get the combination mode.
      CALL PAR_CHOIC( 'CMODE', 'OR', 'AND,OR,XOR', .FALSE., TEXT,
     :                STATUS )
      IF( TEXT .EQ. 'AND' ) THEN
         CMODE = AST__AND
      ELSE IF( TEXT .EQ. 'XOR' ) THEN
         CMODE = AST__XOR
      ELSE
         CMODE = AST__OR
      END IF

*  Add the region into the moc.
      CALL AST_ADDREGION( THIS, CMODE, REGION, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTADDREGION_ERR', 'Error adding a Region '//
     :                 'to a Moc.', STATUS )
      END IF

      END
