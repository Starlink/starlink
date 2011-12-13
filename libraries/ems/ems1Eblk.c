/*+
 *  Name:
 *     ems1Eblk

 *  Purpose:
 *     Initialize the error message tables.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     External variables

 *  Description:
 *     This routine initialises the EMS_ error message tables, the main
 *     and spare for copying.

 *  Copyright:
 *     Copyright (C) 1987 Science & Engineering Research Council.
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
 *     BDK: B.D. Kelly (ROE)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     10-JUN-1987 (BDK):
 *        Original Fortran version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_EBLK
 *     13-AUG-2001 (AJC):
 *        Cast emsTblk to void
 *     13-MAY-2008 (PWD):
 *        Use struct for message table, add spare table for emsEload
 *        to keep context.
 *     {enter_further_changes_here}

 *-

*/

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

/* The table struct, thread specific data when threading. */
static ems_msgtab_t ems_msgtab_base = {
    EMS__BASE,       /* msgdef, default error reporting context */
    EMS__BASE,       /* msglev, error context level */
    SAI__OK,         /* msglst, last reported status (level 1 only) */
    EMS__BASE,       /* msgmrk, number of markers */
    {0},             /* msgcnt, number of messages in table by level */
    {0},             /* msgpln, error parameter string lengths  */
    {0},             /* msglen, error message string lengths */
    {0},             /* msgsta, status values with messages */
    {""},            /* msgpar, error parameter strings */
    {""},            /* msgstr, error message strings */
    {EMS__NSTER},    /* msgbgs, given status values to EMS_BEGIN */
    EMS__SZOUT,      /* msgwsz, line wrapping length */
    FALSE,           /* msgrvl, whether EMS tuning is REVEAL */
    FALSE,           /* msgslt, whether EMS tuning is SILENT */
    FALSE,           /* msgstm, whether EMS tuning is STREAM */
    {0}                /* userdata, spare for additional context */
};

ems_msgtab_t *ems_msgtab = &ems_msgtab_base;

/*  Spare version for keeping a copy. */
static ems_msgtab_t ems_msgtab_base_spare = {
    EMS__BASE,       /* msgdef, default error reporting context */
    EMS__BASE,       /* msglev, error context level */
    SAI__OK,         /* msglst, last reported status (level 1 only) */
    EMS__BASE,       /* msgmrk, number of markers */
    {0},             /* msgcnt, number of messages in table by level */
    {0},             /* msgpln, error parameter string lengths  */
    {0},             /* msglen, error message string lengths */
    {0},             /* msgsta, status values with messages */
    {""},            /* msgpar, error parameter strings */
    {""},            /* msgstr, error message strings */
    {EMS__NSTER},    /* msgbgs, given status values to EMS_BEGIN */
    EMS__SZOUT,      /* msgwsz, line wrapping length */
    FALSE,           /* msgrvl, whether EMS tuning is REVEAL */
    FALSE,           /* msgslt, whether EMS tuning is SILENT */
    FALSE,           /* msgstm, whether EMS tuning is STREAM */
    {0}              /* userdata, spare for additional context */
};

ems_msgtab_t *ems_msgtab_spare = &ems_msgtab_base_spare;

void ems1Eblk()
{
    /* A dummy call to force loading of ems1Tblk as we can't do it within
     * the link script
     */
    (void)ems1Tblk();
}
