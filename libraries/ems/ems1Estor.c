/*
 *+
 *  Name:
 *     ems1Estor

 *  Purpose:
 *     Store an error message in the current context of the error table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Estor( param, plen, msg, mlen, status )

 *  Description:
 *     This routine stores an error message in the current context of the
 *     error table. If there is no room in the error table, then the last
 *     reported error message is replaced by a fault message.

 *  Arguments:
 *     param = char* (Given)
 *        The error message name.
 *     plen = char* (Given)
 *        The length of error message name.
 *     msg = char* (Given)
 *        The error message text.
 *     mlen = char* (Given)
 *        The length of error message text.
 *     status = int (Given and returned)
 *        The global status.

 *  Algorithm:
 *     -  Find a slot in the error table: if the error message table
 *     is full, then replace the last reported error message an EMS_
 *     fault message.
 *     -  Stack the message for output in the error table.
 *     -  Check if the error context level is the lowest: if it is,
 *     store the last reported status value and flush the error table.

 *  Copyright:
 *     Copyright (C) 1990 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     2-MAR-1990 (PCTR):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_ESTOR
 *     13-AUG-2001 (AJC):
 *        #include ems1.h
 *     31-MAR-2008 (PWD):
 *        Do not copy more than EMS__SZPAR and EMS__SZMSG characters
 *        to the error tables. Always null terminate these strings
 *        (previously the null could be missing if the given strings
 *        had a length greater than could be stored, and they overran).
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/*  Global Constants: */
#include "sae_par.h"
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

void ems1Estor( const char *param, int plen, const char *msg,
                int mlen, int *status )
{
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Estor" );
    DEBUG( "ems1Estor", "msglev = %d", msgtab->msglev );

    /*  Use this message table for storage. */
    ems1Estor1( msgtab, param, plen, msg, mlen, status );

    return;
}
