      SUBROUTINE CAT2TAB( STATUS )
*+
*  Name:
*     CAT2TAB

*  Purpose:
*     Converts a CAT catalogue into a "tab table".

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CAT2TAB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program converts a CAT (see SUN/181) catalogue stored in a
*     disk file, into a file in "tab table" format.
*
*     The output catalogue may be fine tuned for GAIA/Skycat by
*     supplying parameters that specify special parameters. These are:
*
*        IDCOL, XCOL, YCOL, RACOL and DECCOL.

*  Usage:
*     CAT2TAB IN OUT

*  ADAM Parameters:
*     DECCOL = INTEGER (Read)
*        The position of the dec_col column (start counting at 0).
*        [!]
*     IDCOL = INTEGER (Read)
*        The position of the id_col column (start counting at 0).
*        [!]
*     IN = _CHAR (Read)
*        Name of the input CAT accessible catalogue.
*     RACOL = INTEGER (Read)
*        The position of the ra_col column (start counting at 0).
*        [!]
*     XCOL = INTEGER (Read)
*        The position of the x_col column (start counting at 0).
*        [!]
*     YCOL = INTEGER (Read)
*        The position of the y_col column (start counting at 0).
*        [!]
*     OUT = _CHAR (Read)
*       Name of the output catalogue.

*  Implementation Deficiencies:
*     - The transformation process doesn't record all the information
*       available in the input catalogue

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
*     24-SEP-1998 (PWD):
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
      INCLUDE 'PAR_ERR'         ! PAR parameters

*  Status:
      INTEGER STATUS            ! Global status

*  Local variables:
      CHARACTER * ( 132 ) INNAM ! Name of input catalogue
      CHARACTER * ( 132 ) OUTNAM ! Name of output catalogue
      INTEGER XCOL              ! Position of X coordinates
      INTEGER YCOL              ! Position of Y coordinates
      INTEGER RACOL             ! Position of RA coordinates
      INTEGER DECCOL            ! Position of DEC coordinates
      INTEGER IDCOL             ! Position of identifier column
      LOGICAL EXISTS            ! TRUE if file exists
      INTEGER CI                ! Catalogue identifier
      INTEGER IFD               ! FIO file identifier
      INTEGER FI                ! Fortran UNIT number
      LOGICAL OUTOPN            ! TRUE when output file is opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the input catalogue.
      CALL PAR_GET0C( 'IN', INNAM, STATUS )

*  CAT says a bit too much at times. Quiet it down (formatting warnings
*  mostly).
      CALL CAT_TUNES( 'QUIET', 'YES', STATUS )

*  And attempt to open it.
      CI = CAT__NOID
      CALL CAT_TOPEN( INNAM, 'OLD', 'READ', CI, STATUS )

*  Now access the tab table. If it already exists then quietly dispose of
*  it.
      OUTOPN = .FALSE.
      CALL PAR_GET0C( 'OUT', OUTNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      INQUIRE( FILE = OUTNAM, EXIST = EXISTS )
      IF ( EXISTS ) THEN
         CALL FIO_ERASE( OUTNAM, STATUS )
      END IF
      CALL FIO_OPEN( OUTNAM, 'WRITE', 'LIST', 0, IFD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         OUTOPN = .TRUE.
         CALL FIO_UNIT( IFD, FI, STATUS )
      ELSE
         GO TO 99
      END IF

*  Check the GAIA specific keywords.
      CALL ERR_MARK
      CALL PAR_GET0I( 'IDCOL', IDCOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         IDCOL = -1
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL PAR_GET0I( 'XCOL', XCOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         XCOL = -1
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL PAR_GET0I( 'YCOL', YCOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         YCOL = -1
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL PAR_GET0I( 'RACOL', RACOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         RACOL = -1
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL PAR_GET0I( 'DECCOL', DECCOL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         DECCOL = -1
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Ok now do the conversion.
      CALL GAI_C2TAB( CI, FI, IDCOL, XCOL, YCOL, RACOL, DECCOL, STATUS )

*   Exit in error.
 99   CONTINUE
      IF ( CI .NE. CAT__NOID ) THEN
         CALL CAT_TRLSE( CI, STATUS )
      END IF
      IF ( OUTOPN ) THEN
         CALL FIO_CLOSE( IFD, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CAT2TAB',
     :    'CAT2TAB: Error converting catalogue.', STATUS )
      END IF

      END
