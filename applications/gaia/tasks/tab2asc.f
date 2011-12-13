      SUBROUTINE TAB2ASC( STATUS )
*+
*  Name:
*     TAB2ASC

*  Purpose:
*     Converts a "tab table" into an ASCII_HEAD catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TAB2ASC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program converts a disk file containing a "table tab" into a
*     SExtractor ASCII_HEAD catalogue. The format of an ASCII_HEAD table
*     is a header region containing a simple description of the columns
*     and then a data area in which the fields are separated by
*     spaces. The format of the header region is normally:
*
*        "# 1 NAME comment [units]"
*
*     This application has no information about the units or any meta
*     data so we just write:
*
*        "# 1 NAME"

*  Usage:
*     TAB2ASC IN OUT

*  ADAM Parameters:
*     IN = _CHAR (Read)
*        Name of the input tab-table catalogue.
*     OUT = _CHAR (Read)
*       Name of the output ASCII_HEAD catalogue.

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
*     16-OCT-1998 (PWD):
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
      LOGICAL EXISTS            ! TRUE if file exists
      INTEGER IFIN              ! FIO identifier
      INTEGER IFOUT             ! FIO identifier
      LOGICAL INOPN             ! TRUE when input file is opened
      LOGICAL OUTOPN            ! TRUE when output file is opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the input catalogue.
      CALL PAR_GET0C( 'IN', INNAM, STATUS )

*  And attempt to open it.
      INOPN = .FALSE.
      OUTOPN = .FALSE.
      CALL FIO_OPEN( INNAM, 'READ', 'LIST', 0, IFIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now attempt to open the output catalogue, we delete this it if
*  already exists.
      OUTOPN = .FALSE.
      CALL PAR_GET0C( 'OUT', OUTNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      INQUIRE( FILE = OUTNAM, EXIST = EXISTS )
      IF ( EXISTS ) THEN
         CALL FIO_ERASE( OUTNAM, STATUS )
      END IF
      CALL FIO_OPEN( OUTNAM, 'WRITE', 'LIST', 0, IFOUT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         OUTOPN = .TRUE.
      ELSE
         GO TO 99
      END IF

*  Ok now do the conversion.
      CALL GAI_T2ASC( IFIN, IFOUT, STATUS )

*   Exit in error.
 99   CONTINUE
      IF ( INOPN ) THEN
         CALL FIO_CLOSE( IFIN, STATUS )
      END IF
      IF ( OUTOPN ) THEN
         CALL FIO_CLOSE( IFOUT, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ASC2TAB',
     :    'ASC2TAB: Error converting catalogue.', STATUS )
      END IF

      END
