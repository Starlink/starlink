      SUBROUTINE TAB2CAT( STATUS )
*+
*  Name:
*     TAB2CAT

*  Purpose:
*     Converts a "tab table" into a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TAB2CAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program converts a disk file containing a "table tab" into a
*     CAT catalogue (see SUN/181).
*
*     At present this application can only work well with tab tables
*     produced by the "cat2tab" application. Which encodes enough
*     information to automatically regenerate the data types etc. of the
*     fields. If this isn't the case then an attempt is made to
*     re-generate much of the information about data types (see
*     GAI1_T2CG).

*  Usage:
*     TAB2CAT IN OUT

*  ADAM Parameters:
*     IN = _CHAR (Read)
*        Name of the input tab-table catalogue.
*     OUT = _CHAR (Read)
*       Name of the output CAT catalogue.

*  Implementation Deficiencies:
*     - Doesn't even attempt to work on catalogues produced by anything
*       other than cat2tab.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  History:
*     28-SEP-1998 (PWD):
*        Original version. Just for GAIA really not general enough
*        for other purposes.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT parameters

*  Status:
      INTEGER STATUS            ! Global status

*  Local variables:
      CHARACTER * ( 132 ) INNAM ! Name of input catalogue
      CHARACTER * ( 132 ) OUTNAM ! Name of output catalogue
      INTEGER CI                ! Catalogue identifier
      INTEGER IFD               ! FIO file identifier
      LOGICAL INOPN             ! TRUE when input file is opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the input catalogue.
      CALL PAR_GET0C( 'IN', INNAM, STATUS )

*  And attempt to open it.
      INOPN = .FALSE.
      CALL FIO_OPEN( INNAM, 'READ', 'LIST', 0, IFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  CAT says a bit too much at times. Quiet it down (formatting warnings
*  mostly).
      CALL CAT_TUNES( 'QUIET', 'YES', STATUS )

*  Now attempt to open the output catalogue. This will fail if the
*  catalogue already exists.
      CALL PAR_GET0C( 'OUT', OUTNAM, STATUS )
      CI = CAT__NOID
      CALL CAT_TOPEN( OUTNAM, 'NEW', 'WRITE', CI, STATUS )

*  Ok now do the conversion.
      CALL GAI_T2CAT( IFD, CI, STATUS )

*   Exit in error.
 99   CONTINUE
      IF ( CI .NE. CAT__NOID ) THEN
         CALL CAT_TRLSE( CI, STATUS )
      END IF
      IF ( INOPN ) THEN
         CALL FIO_CLOSE( IFD, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CAT2TAB',
     :    'CAT2TAB: Error converting catalogue.', STATUS )
      END IF

      END
