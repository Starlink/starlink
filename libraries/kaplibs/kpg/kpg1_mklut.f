      SUBROUTINE KPG1_MKLUT( IX, IY, NPNT, NVAR, FRM, TABLE, MAP,
     :                       STATUS )
*+
*  Name:
*     KPG1_MKLUT

*  Purpose:
*     Creates a Mapping to connect two one-dimensional array of values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MKLUT( IX, IY, NPNT, NVAR, FRM, TABLE, MAP, STATUS )

*  Description:
*     This routine creates a one-dimensional Mapping which translates an
*     X into a Y value on the basis of supplied tables of corresponding
*     X and Y. This is like an AST LutMap except that the LutMap class
*     requires Y to be tabulated at equal X intervals, whereas this
*     routine allows Y to be tabulated at arbitrary X intervals.

*  Arguments:
*     IX = INTEGER (Given)
*        The index of the X values within the TABLE array.
*     IY = INTEGER (Given)
*        The index of the Y values within the TABLE array.
*     NPNT = INTEGER (Given)
*        The number of values supplied for each variable in the TABLE
*        array.
*     NVAR = INTEGER (Given)
*        The number of variables described in the table. This will be at
*        least 2 (for X and Y) but may be more.
*     FRM = INTEGER (Given)
*        If not AST__NULL, then this should be an AST pointer to a Frame
*        with NVAR axes which will be used to normalise the axis values
*        before creating the LutMap. No normalisation occurs if a value of
*        AST__NULL is supplied.
*     TABLE( NPNT, NVAR ) = DOUBLE PRECISION (Given and Returned)
*        The table containing corresponding X and Y values. The table can
*        also contain values for other variables, which will be ignored.
*        These will be normalised on exit using the AST Frame supplied by
*        FRM.
*     MAP = INTEGER (Returned)
*        An AST pointer to the returned Mapping, or AST__NULL if no Mapping
*        could be created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - It is only possible to create the Mapping if the tabulated X values
*     are monotonic increasing or decreasing.
*     - The returned Mapping will have an inverse Transformation only if Y
*     increases or decreases monotonically with X.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-2005 (DSB):
*        Original version.
*     9-AUG-2005 (DSB):
*        Reposition the AST_EXPORT call to avoid error in cases where no
*        Mapping can be found.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IX
      INTEGER IY
      INTEGER NPNT
      INTEGER NVAR
      INTEGER FRM

*  Arguments Given and Returned:
      DOUBLE PRECISION TABLE( NPNT, NVAR )

*  Arguments Returned:
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION WORK( NDF__MXDIM )
      INTEGER I
      INTEGER J
      INTEGER NMAP
      INTEGER TMAP
      INTEGER MAP1
      INTEGER MAP2
*.

*  Initialise.
      MAP = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  If required, normalise the values.
      IF( FRM .NE. AST__NULL ) THEN
         DO I = 1, NPNT
            DO J = 1, NVAR
               WORK( J ) = TABLE( I, J )
            END DO
            CALL AST_NORM( FRM, WORK, STATUS )
            DO J = 1, NVAR
               TABLE( I, J ) = WORK( J )
            END DO
         END DO

*  Create a normalisation Mapping for the X axis
         NMAP = AST_NORMMAP( AST_PICKAXES( FRM, 1, IX, TMAP, STATUS ),
     :                       ' ', STATUS )
      ELSE
         NMAP = AST__NULL

      END IF

*  Create a LutMap which gives X as a function of index within the table.
      MAP1 = AST_LUTMAP( NPNT, TABLE( 1, IX ), 1.0D0, 1.0D0, ' ',
     :                   STATUS )

*  Check this LutMap can be inverted. This will be so if the X values are
*  monotonic.
      IF( AST_GETL( MAP1, 'TranInverse', STATUS ) ) THEN

*  If so, invert it so that the forward transformation of MAP1 transforms
*  X value into index value.
         CALL AST_INVERT( MAP1, STATUS )

*  Preceed it with the normalisation Mapping (if any) which normalises
*  the X value
         IF( NMAP .NE. AST__NULL ) THEN
            MAP1 = AST_CMPMAP( NMAP, MAP1, .TRUE., ' ', STATUS )
         END IF

*  Create a LutMap which gives Y as a function of index within the table.
         MAP2 = AST_LUTMAP( NPNT, TABLE( 1, IY ), 1.0D0, 1.0D0, ' ',
     :                      STATUS )

*  Combine these two Mappings in series to get a Mapping which goes
*  from X to Y (via index).
         MAP = AST_CMPMAP( MAP1, MAP2, .TRUE., ' ', STATUS )

*  Export the returned Mapping from the current AST context so that it is
*  not annulled by the following call to AST_END.
         CALL AST_EXPORT( MAP, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
