/*      BLOCK DATA EMS1_EBLK
*+
*  Name:
*     EMS1_EBLK

*  Purpose:
*     Initial contents of the error message table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     This routine initialises the EMSTB_CMN common blocks to perform
*     the initialisation of the EMS_ error message table.

*  Copyright:
*     Copyright (C) 1987, 1989, 1990, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     10-JUN-1987 (BDK):
*        Original version.
*     13-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     7-DEC-1989 (PCTR):
*        EMS_ version taken from ERR_BLK.
*     9-APR-1990 (PCTR):
*        Removed unreferenced include file.
*     14-AUG-1991 (PCTR):
*        Added initialisation of MSGBGS.
*     6-DEC-1993 (PCTR):
*        Added initialisation of MSGRVT, MSGSLT, MSGSTT.
*     23-JUL-1999 (AJC):
*        Added initialisation of MSGWSZ
*        MSGRVT->MSGRVL
*        MSGSTT->MSGSTM
*     {enter_further_changes_here}

*-
*/
#include "ems_par.h"
#include "ems_sys.h"

#ifndef EMS_MSGTB_DEFINED
#define EMS_MSGTB_DEFINED
extern int msgdef;       /* Default error reporting context */
extern int msglev;       /* Error context level */
extern int msglst;       /* Last reported status (level 1 only) */
extern int msgmrk;       /* Number of markers */
extern int msgcnt[];     /* Number of messages in table by level */
extern int msgpln[];     /* Error parameter string lengths */
extern int msglen[];     /* Error message string lengths */
extern int msgsta[];     /* Status values with messages */
extern char msgpar[][EMS__SZPAR+1];   /* Error parameter strings */
extern char msgstr[][EMS__SZMSG+1];   /* Error message strings */
extern int msgbgs[];     /* Given status values to EMS_BEGIN */
extern int msgwsz;       /* Line wrapping length */
extern Logical msgrvl;   /* Whether EMS tuning is REVEAL */
extern Logical msgslt;   /* Whether EMS tuning is SILENT */
extern Logical msgstm;   /* Whether EMS tuning is STREAM */
#endif
