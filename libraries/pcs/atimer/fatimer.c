/*
*+
*  Name:
*     FATIMER

*  Purpose:
*     A Fortran callable interface to the ATIMER C routines

*  Language:
*     C

*  Description:
*     Provides a Fortran callable interface to ATIMER_SETTIMR
*     and ATIMER_CANTIM which set and cancel millisecond timers.
*     A C routine has to be written as the handler routine, specified
*     in the SSETTIMR call, which is called directly by the ATIMER system.
*     The C handler may be written as follws to call a handler written
*     in Fortran:
*
*        #include "f77.h"
*
*        extern void F77_EXTERNAL_NAME(fhandlr)( INTEGER(id) );
*
*        F77_SUBROUTINE(chandlr)( int id )
*
*        F77_LOCK( F77_CALL(fhandlr)( INTEGER_ARG(&id) ); )
*

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     18-SEP-1995 (AJC):
*        Correct declaration of handler argument in settimr.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Include Statements: */
#include "sae_par.h"
#include "f77.h"
#include "atimer.h"

/*
*+
*  Name:
*     FATIMER_CANTIM( ID, STATUS )

*  Purpose:
*     A Fortran callable interface to the ATIMER_CANTIM C routine

*  Language:
*     C

*  Invocation:
*     CALL FATIMER_CANTIM( ID, STATUS )

*  Description:
*     Cancels the ATIMER timer with the specified id.
*     The value of the id is obtained and specified in a call to
*     atimer_settimr.

*  Arguments:
*     ID = INTEGER (Given)
*        The timer id as given in the associated ATIMER_SETTIMR call.
*     STATUS = INTEGER (Given and returned)
*        The global status.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Function Definition: */
F77_SUBROUTINE(fatimer_cantim)( INTEGER(id), INTEGER(status) )
{
/* Exit if bad given status
*/
   if ( *status != SAI__OK ) return;

   atimer_cantim( *id, status );

   return;
}

/*
*+
*  Name:
*     FATIMER_SETTIMR( DELAY, ID, HANDLER, STATUS )

*  Purpose:
*     A Fortran callable interface to the ATIMER_settimr C routine

*  Language:
*     C

*  Invocation:
*     CALL FATIMER_SETTIMR( DELAY, ID, HANDLER, STATUS )

*  Description:
*     Cancels the ATIMER timer with the specified id.
*     The value of the id is obtained and specified in a call to
*     atimer_settimr.

*  Arguments:
*     DELAY = INTEGER (Given)
*        The delay time in milliseconds
*     ID = INTEGER (Given)
*        The timer id as given in the associated ATIMER_SETTIMR call.
*     HANDLER = EXTERNAL (Given)
*        The name of the rouitne to be called with the timer ID as the
*        sole argument when the specified delay time has elapsed.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1994 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Function Definition: */
F77_SUBROUTINE(fatimer_settimr)( INTEGER(delay), INTEGER(id),
                                 void (*handler)(), INTEGER(status) )
{

/* Exit if bad given status
*/
   if ( *status != SAI__OK ) return;

   atimer_settimr( *delay, *id, handler, status );

   return;
}

