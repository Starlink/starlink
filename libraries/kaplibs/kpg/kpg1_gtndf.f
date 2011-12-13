      SUBROUTINE KPG1_GTNDF( PARAM, NDIM, EXACT, MODE, INDF, SDIM,
     :                       SLBND, SUBND, STATUS )
*+
*  Name:
*     KPG1_GTNDF

*  Purpose:
*     Gets an NDF or NDF section with a specified number of significant
*     dimensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTNDF( PARAM, NDIM, EXACT, MODE, INDF, SDIM, SLBND,
*                      SUBND, STATUS )

*  Description:
*     The supplied parameter name is associated with an NDF through the
*     environment, and an identifier is obtained for the NDF using the
*     specified access mode.  Each axis of the NDF is checked to see if
*     is significant (i.e. has a size greater than 1).  The index of
*     each significant axis is returned in SDIM, and the bounds of the
*     axis are returned in SLBND and SUBND.  If EXACT is .TRUE., an
*     error is reported if the number of significant axes is not
*     exactly NDIM.  If EXACT is .FALSE. an error is only reported if
*     the number of significant dimensions is higher than NDIM.  If
*     there are less than NDIM significant dimensions then the
*     insignificant dimensions are used (starting from the lowest) to
*     ensure that the required number of dimensions are returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     NDIM = INTEGER (Given)
*        The number of dimensions required.
*     EXACT = LOGICAL (Given)
*        This should be supplied .FALSE. if an NDF with less than NDIM
*        significant dimensions can be used.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode required for the NDF.
*     INDF = INTEGER (Returned)
*        An identifier for the NDF.
*     SDIM( NDIM ) = INTEGER (Returned)
*        The indices of the significant dimensions.
*     SLBND( NDIM ) = INTEGER (Returned)
*        The lower bounds of the significant dimensions.  These are
*        stored in the same order as the indices in SDIM.
*     SUBND( NDIM ) = INTEGER (Returned)
*        The upper bounds of the significant dimensions.  These are
*        stored in the same order as the indices in SDIM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1995 (DSB):
*        Original version.
*     8-MAR-1995 (DSB):
*        Argument EXACT added.
*     1995 March 17 (MJC):
*        Used modern-style variable declarations, added a
*        clarification to the description, and minor stylistic changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER NDIM
      LOGICAL EXACT
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER INDF
      INTEGER SDIM( NDIM )
      INTEGER SLBND( NDIM )
      INTEGER SUBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER NDIMS              ! No. of dimensions in NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDF containing the input data.
      CALL LPG_ASSOC( PARAM, MODE, INDF, STATUS )

*  Find whether or not there are exactly the required number of
*  significant dimensions and which ones they are.
      IF ( EXACT ) THEN
         CALL KPG1_SGDIM( INDF, NDIM, SDIM, STATUS )

*  If insignificant dimensions can be used, pad out the returned
*  dimensions with insignificant dimensions if the required number
*  of significant dimensions is not present.
      ELSE
         CALL KPG1_SDIMP( INDF, NDIM, SDIM, STATUS )
      END IF

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Return the dimensions and the bounds.
      IF ( STATUS .EQ. SAI__OK ) THEN
         SLBND( 1 ) = LBND( SDIM( 1 ) )
         SLBND( 2 ) = LBND( SDIM( 2 ) )
         SUBND( 1 ) = UBND( SDIM( 1 ) )
         SUBND( 2 ) = UBND( SDIM( 2 ) )
      END IF

      END
