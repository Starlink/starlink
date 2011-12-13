/*+
 *  Name:
 *     ems_defs

 *  Purpose:
 *     Define various structure data types.

 *  Language:
 *     Starlink C

 *  Copyright:
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
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     15-MAY-2008 (PWD):
 *        Original version.
 *     {enter_further_changes_here}

 *-
 */
#ifndef EMS_DEFS_DEFINED
#define EMS_DEFS_DEFINED

/*  The message table structure. */

typedef struct {
    int msgdef;                  /* Default error reporting context */
    int msglev;                  /* Error context level */
    int msglst;                  /* Last reported status (level 1 only) */
    int msgmrk;                  /* Number of markers */
    int msgcnt[EMS__MXLEV+1];    /* Number of messages in table by level */
    int msgpln[EMS__MXMSG+1];    /* Error parameter string lengths */
    int msglen[EMS__MXMSG+1];    /* Error message string lengths */
    int msgsta[EMS__MXMSG+1];    /* Status values with messages */
    char msgpar[EMS__MXMSG+1][EMS__SZPAR+1];   /* Error parameter strings */
    char msgstr[EMS__MXMSG+1][EMS__SZMSG+1];   /* Error message strings */
    int msgbgs[EMS__MXLEV+1];     /* Given status values to EMS_BEGIN */
    int msgwsz;                   /* Line wrapping length */
    Logical msgrvl;               /* Whether EMS tuning is REVEAL */
    Logical msgslt;               /* Whether EMS tuning is SILENT */
    Logical msgstm;               /* Whether EMS tuning is STREAM */
    int userdata[3];              /* Spares for any purpose. Initially 0. */
} ems_msgtab_t;


/*  The token table structure. */

typedef struct {
    int toklev;                     /* Token context level */
    int tokmrk;                     /* Number of markers */
    int tokcnt[EMS__MXLEV+1];       /* Number of tokens in table by level */
    int tokhiw[EMS__MXLEV+1];       /* Token table high water mark */
    int toklen[EMS__MXTOK+1];       /* Token string lengths */
    char toknam[EMS__MXTOK+1][EMS__SZNAM+1];   /* Token names */
    char tokstr[EMS__MXTOK+1][EMS__SZTOK+1];   /* Token strings */
    int userdata;                 /* Spare for any purpose. Initially 0. */
} ems_toktab_t;

/*  Thread specific data structure. */

typedef struct {
    ems_msgtab_t msgtab;       /* Thread message table */
    ems_msgtab_t msgtab_spare; /* Spare thread message table */
    ems_toktab_t toktab;       /* Thread token table */
    char buffer[EMS__SZBUF];   /* Thread buffer for temp results. */
} ems_thread_data_t;

#endif
