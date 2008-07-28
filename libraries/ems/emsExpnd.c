/*+
 *  Name:
 *     emsExpnd

 *  Purpose:
 *     Expand and return a message.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsExpnd( text, opstr, maxlen, oplen, status )

 *  Description:
 *     Expands any tokens of the form ^NAME within the string 'text' and
 *     returns the result in 'opstr'. Undefined tokens will appear as <TOKEN>,
 *     where 'NAME' is the token name. If the expanded message exceeds the
 *     given maximum length 'maxlen', opstr will be terminated with '...'.
 *     It is the users resonsibility to ensure that enough space has been
 *     allocated for opstr.
 *     A Fortran-callable interface EMS_EXPND is also provided.

 *  Arguments:
 *     text = const char * (Given)
 *        The raw message text.
 *     opstr = char * (Returned)
 *        The expanded message text.
 *     maxlen = const int (Given)
 *        The maximum length for the expanded string
 *     oplen = int * (Returned)
 *        The length of the expanded message.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technlogy Facilities Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA
 
 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}
 
 *  History:
 *     12-MAR-2001 (AJC):
 *        Original version, modified from emsMload (now deprecated)
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     28-JUL-2008 (TIMJ):
 *        Initialise return buffer on error.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "sae_par.h"                   /* SAE_ public constant definitions */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems1.h"                      /* EMS_ private functions prototypes */
#include "ems_defs.h"                  /* EMS_ message table */

/* Function Definitons: */
void emsExpnd( const char *text, char *opstr, const int maxlen, int *oplen,
               int *status )
{
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsExpnd" );
    DEBUG( "emsExpnd", "msglev = %d", msgtab->msglev );

    /* make sure we are initialised regardless of status */
    *oplen = 0;
    opstr[0] = '\0';

    /*  Check the inherited global status. */
    if ( *status |= SAI__OK ) {

        /*  Status is not SAI__OK, so just annul the token table. */
        ems1Ktok();
    } else {

        /*  Form output message string. */
        ems1Form( text, maxlen, !msgtab->msgstm, opstr, oplen, status );
    }

    return;
}
