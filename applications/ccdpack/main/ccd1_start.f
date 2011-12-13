      SUBROUTINE CCD1_START( TASK, STATUS )
*+
*  Name:
*     CCD1_START

*  Purpose:
*     To open the log file system and write out the task introduction.
*     This routine must be the first call in any geniune CCDPACK
*     application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_START( TASK, STATUS )

*  Description:
*     The routine access the ADAM parameters LOGTO and LOGFILE.  To
*     start up the appropriate logfile actions. If a logging is
*     requested then the data information output via the CCDPACK
*     routines CCD1_MSG and CCD1_ERREP will be echoed to it. Any
*     information which is required in the log file must be output
*     using these routines. The TASK name is also output.
*
*     An additional function of this routine is also to initialise any
*     CCDPACK common blocks (note these are initialised here may also
*     be initialised during the the first call of the the appropriate
*     routines. In these cases the initialisation here is to ensure
*     that close down routines do not act inappropriately).

*  Arguments:
*     TASK = CHARACTER * ( * ) (Given)
*        The name of the current task.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1997, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1991 (PDRAPER):
*        Original version.
*     15-JAN-1992 (PDRAPER):
*        New log system.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator code.
*     14-MAY-2001 (MBT):
*        Added initialisation of keyed parameter block.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK system constants

*  Global Variables:
      INCLUDE 'CCD1_MEMCM'       ! Dynamic memory common block
*        CCD1_MEMCM( CCD1__MXPNT ) = INTEGER (Write)
*           Array of pointers to any data which is allocated by the
*           virtual memory allocation routines. These are initialised
*           to -1.

      INCLUDE 'CCD1_TMPCM'       ! Temporary workspace common block
*        CCD1_TMPPO( CCD1__MXPNT ) = INTEGER (Write)
*           Array of pointers to any data which is allocated by the disk
*           resident and mapping routines. Initialised to -1
*        CCD1_TMPLO( CCD1__MXPNT ) = CHARACTER (Write)
*           Array of locators to any data objects created by the disk
*           resident memory allocation routines. Initialised to DAT__NOLOC.

*  Arguments Given:
      CHARACTER * ( * ) TASK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_HVUSR
      LOGICAL CCD1_HVUSR         ! Returns true if a user is present
                                 ! if so the delimeter is not written.

*  Local Variables:
      INTEGER IAT                ! Current position in character buffer
      CHARACTER BUFFER * ( 80 )  ! Taskname buffer
      INTEGER I                  ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the log file.
      CALL CCD1_OPLOG( STATUS )

*  Write out the introduction.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( .NOT. CCD1_HVUSR( STATUS ) ) THEN
         CALL CCD1_MSG( ' ', '*+', STATUS )
      ELSE
         CALL CCD1_MSGL( '*+', STATUS )
      END IF

*  Clear buffer and write in name.
      BUFFER = ' '
      IAT = 4
      CALL CHR_PUTC( TASK, BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( : IAT ), STATUS )

*  Underline it.
      CALL CHR_FILL( '=', BUFFER( 5 : IAT ) )
      CALL CCD1_MSG( ' ', BUFFER( : IAT ), STATUS )

*  Intialise the memory allocation routines common blocks. These
*  initialisations are optional as they are repeated in the first
*  call of the allocation routine. Initialisation here stops
*  inappropriate actions on -1 returns to the closure mechanisms
*  when the exit status is good but no allocation have actually been
*  made.

*  Virtual memory common block.
      DO 1 I = 1, CCD1__MXPNT
         CCD1_MEMCM( I ) = -1
 1    CONTINUE

*  Disk resident - mapped memory.
      DO 2 I = 1, CCD1__MXPNT
         CCD1_TMPPO( I ) = -1
         CCD1_TMPLO( I ) = DAT__NOLOC
 2    CONTINUE

*  Initialise the keyed-variable info structures.  Although the first
*  call to CCD1_KPLD initialises this, it is necessary to do it here
*  since it must be reset for each task within a monolith.
      CALL CCD1_KPLD( ' ', CCD1__BADSI, STATUS )

      END
* $Id$
