      SUBROUTINE ARD1_SRCTA( STATUS )
*+
*  Name:
*     ARD1_SRCTA

*  Purpose:
*     Read AST_ data as text from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_SRCTA( STATUS )

*  Description:
*     This is a service routine to be provided as a "source" routine
*     for the AST_CHANNEL function. It reads data from a GRP group
*     (in response to reading from an AST_ Channel) and delivers it to
*     the AST_ library for interpretation.
*
*     This routine has only a STATUS argument, so it communicates with
*     other ARD routines via global variables stored in the ARD_AST common
*     blocks. These are described below under "Global Variables used as
*     Arguments".

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Global Variables used as Arguments:
*     CMN_IGRP = INTEGER (Given)
*        A GRP identifier for the group which holds the data.
*     CMN_NXTLN = INTEGER (Given and Returned)
*        This must initially be set to the value 1, to indicate that
*        data will be read starting at the first element of the group
*        (note the routine will not operate correctly unless 1 is
*        the initial value - you cannot start reading at another point
*        in the group if you have previously read from a different
*        group). On exit it will be incremented by the number of
*        elements used to obtain data, so that it identifies the first
*        element to be used on the next invocation.

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1998 (DSB):
*        Original version, based on NDF1_RDAST.
*     26-MAY-1999 (DSB):
*        Only read the required length from each GRP element.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ public constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks.
*        CMN_IGRP = INTEGER (Read)
*           GRP identifier for group holding AST_ data.
*        CMN_NXTLN = INTEGER (Read and Write)
*           Next element to use in group holding AST_ data.
*        GRP__SZNAM = INTEGER (Read)
*           The length to read from each GRP element.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER TEXT*( GRP__SZNAM ) ! Buffer for AST_ text
      INTEGER DIM                 ! Size of GRP group
      INTEGER L                   ! Number of characters in AST_ text

      SAVE DIM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Before reading the first line, obtain the number of elements in the
*  group being read from.
      IF ( CMN_NXTLN .EQ. 1 ) CALL GRP_GRPSZ( CMN_IGRP, DIM, STATUS )

*  If any elements are left in the group, get the next one.
      IF ( CMN_NXTLN .LE. DIM ) THEN
         CALL ARD1_GET( CMN_IGRP, CMN_NXTLN, 1, TEXT, STATUS )

*  Store its used length.
         L = CHR_LEN( TEXT )

*  Increment the index of the next element to read.
         CMN_NXTLN = CMN_NXTLN + 1

*  If no elements are left to read, set L to -1 to indicate this.
      ELSE
         L = -1
      END IF

*  If there has been an error, set the number of characters to -1
*  to indicate no more data.
      IF ( STATUS .NE. SAI__OK ) L = -1

*  Send the text to the AST_ library for interpretation.
      CALL AST_PUTLINE( TEXT, L, STATUS )

      END
