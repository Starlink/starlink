/*+
 *  Name:
 *     emsMload

 *  Purpose:
 *     Expand and return a message.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsMload( msg, text, opstr, oplen, status )

 *  Description:
 *     Expands any tokens of the form ^NAME within the string 'text', and
 *     returns the result in 'opstr'. Undefined tokens will appear as <NAME>,
 *     where 'NAME' is the token name. Expanded messages will be a mximum
 *     of EMS__SZMSG characters, terminated with '...' if they overflow.
 *     It is the users resonsibility to ensure that enough space has been
 *     allocated for opstr.
 *     A Fortran-callable interface EMS_MLOAD is also provided.

 *  Arguments:
 *     msg = const char * (Given)
 *        The message name.
 *     text = const char * (Given)
 *        The raw message text.
 *     opstr = char * (Returned)
 *        The expanded message text.
 *     oplen = int * (Returned)
 *        The length of the expanded message.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 1990, 1991 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     17-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_mload_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_MLOAD
 *      2-MAR-2001 (AJC):
 *        Properly import strings
 *        and remove incorrect copying at end
 *        Remove msg argument from ems1Form
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     Provides no limit on the output string length - possible overwriting,
 *     no ... termination.
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
void emsMload( const char *msg __attribute__((unused)), const char *text, char *opstr,
               int *oplen, int *status )
{
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsMload" );
    DEBUG( "emsMload", "msglev = %d", msgtab->msglev );

    /*  Check the inherited global status. */
    if ( *status |= SAI__OK ) {

        /*  Status is not SAI__OK, so just annul the token table. */
        ems1Ktok();
    } else {

        /*  Form output message string. */
        ems1Form( text, EMS__SZMSG, 0, !msgtab->msgstm, opstr, oplen,
                  status );
    }
    return;
}
