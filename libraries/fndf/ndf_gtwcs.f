      SUBROUTINE NDF_GTWCS( INDF, IWCS, STATUS )
*+
*  Name:
*     NDF_GTWCS

*  Purpose:
*     Obtain world coordinate system information from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Description:
*     The routine obtains information about the world coordinate
*     systems associated with an NDF and returns an AST pointer to a
*     FrameSet which contains this information. The information may
*     then be accessed using routines from the AST library (SUN/210).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IWCS = INTEGER (Returned)
*        An AST pointer to a FrameSet which contains information about
*        the world coordinate systems associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - It is the caller's responsibility to annul the AST pointer
*     issued by this routine (e.g. by calling AST_ANNUL) when it is no
*     longer required. The NDF_ system will not perform this task
*     itself.
*     - If this routine is called with STATUS set, then a value of
*     AST__NULL will be returned for the IWCS argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The AST__NULL
*     constant is defined in the include file AST_PAR.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     2-JUL-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public contstants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

*  Initialise the returned AST_ pointer.
      IWCS = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Read the WCS information from the NDF.
      CALL NDF1_RDWCS( IACB, IWCS, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTWCS_ERR',
     :   'NDF_GTWCS: Error obtaining world coordinate system ' //
     :   'information from an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_GTWCS', STATUS )
      END IF

      END
