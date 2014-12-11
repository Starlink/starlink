*+
* Name:
*    SLV_LOADW

*  Purpose:
*     Load an ADAM task as a slave.

*  Language:
*     ANSI C.

*  Invocation:
*     RESULT = SLV_LOADW( TASK, FILE, TMOUT, STATUS )

*  Description:
*     This function loads a specified ADAM task as a slave (or ensures
*     that a specified task has already been loaded). Before returning,
*     it waits until loading is complete and ensures that the task is
*     ready to receive messages.
*
*     Typically, this function is called to load an ADAM monolith to
*     which messages will subsequently be sent instructing it to carry
*     out specified actions.

*  Arguments:
*     TASK = CHARACTER * ( * ) (Given)
*        The name by which the task will be known to the ADAM message
*        system. You may also give the name of a previously loaded
*        task, in which case the function will ensure that it has been
*        loaded.  Note that slave tasks may be shared by several
*        masters, so if you want a slave for private use, you should
*        make this name unique.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the executable file containing the task to be
*        loaded. This is only used if a task with the specified name is
*        not already loaded. The standard methods for locating this
*        file are followed if necessary (e.g. using the UNIX PATH).
*
*        If you simply wish to check whether a specified task is
*        already loaded, then FILE may be blank. In this case an error
*        will result if the task is not loaded.
*     TMOUT = INTEGER (Given)
*        A timeout period (in seconds). An error will result if it
*        takes longer than the specified time to load the task and
*        establish communication with it. A zero or negative value
*        results in the function waiting indefinitely (i.e. no
*        timeout).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     SLV_LOADW = INTEGER
*        If a new task is loaded by this function, then the ID of the
*        process into which it was loaded is returned. If the task was
*        already loaded, a value of zero is returned.

*  Notes:
*     - If an error occurs (or the function is called with the STATUS
*     argument set), a value of -1 is returned.
*     - To remove the task after you have finished with it, you should
*     use the SLV_KILLW routine and pass the process ID returned by
*     SLV_LOADW.  Note, however, that this has no effect if the process
*     ID is zero (meaning that the corresponding call to SLV_LOADW
*     found the task already loaded).

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
