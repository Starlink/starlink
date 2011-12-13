      SUBROUTINE SETTITLE( STATUS )
*+
*  Name:
*     SETTITLE

*  Purpose:
*     Sets a new title for an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETTITLE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets a new value for the TITLE component of an
*     existing NDF data structure. The NDF is accessed in update mode
*     and any pre-existing title is over-written with a new value.
*     Alternatively, if a "null" value (!) is given for the TITLE
*     parameter, then the NDF's title will be erased.

*  Usage:
*     settitle ndf title

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF data structure whose title is to be modified.
*     TITLE = LITERAL (Read)
*        The value to be assigned to the NDF's TITLE component (e.g.
*        "NGC1068 with a B filter" or "Ice band in HD123456").  This
*        value may later be used by other applications as a heading for
*        graphs and other forms of display where the NDF's data values
*        are plotted.  The suggested default is the current value.

*  Examples:
*     settitle ngc1068 "NGC1068 with a B filter"
*        Sets the TITLE component of the NDF structure ngc1068 to be
*        "NGC1068 with a B filter".
*     settitle ndf=myspec title="Ice band, short integration"
*        Sets the TITLE component of the NDF structure myspec to be
*        "Ice band, short integration".
*     settitle dat123 title=!
*        By specifying a null value (!), this example erases any
*        previous value of the TITLE component in the NDF structure
*        dat123.

*  Related Applications:
*     KAPPA: SETLABEL, SETUNITS.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-APR-1990 (RFWS):
*        Original version.
*     27-APR-1990 (RFWS):
*        Added examples to prologue.
*     25-JUN-1990 (RFWS):
*        Minor changes to prologue text.
*     1995 April 21 (MJC):
*        Made usage and examples lowercase.  Added closing error
*        report and Related Applications.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NDF                ! NDF identifier

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Reset any existing title.
      CALL NDF_RESET( NDF, 'Title', STATUS )

*  Obtain a new title.
      CALL NDF_CINP( 'TITLE', NDF, 'Title', STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( NDF, STATUS )

*  Write the closing error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETTITLE_ERR',
     :     'SETTITLE: Error modifying the title of an NDF.', STATUS )
      END IF

      END
