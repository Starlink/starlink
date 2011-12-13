/*
 *+
 *  Name:
 *     ems1Ithreadata

 *  Purpose:
 *     Create and initialise thread specific data structure.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     void ems1Ithreaddata()

 *  Description:
 *     This routine creates a ems_thread_data_t struct that contains message
 *     and token tables instances and a character buffer. It is meant to be
 *     associated as the thread-specific data item. The data created here
 *     should be freed by the ems1Fthreaddata routine (established as the
 *     destructor for the thread specific data using pthread_setspecific).

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
 *     16-MAY-2008 (PWD):
 *        Original version.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <star/mem.h>

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS1 function prototypes */
#include "ems_defs.h"                /* Token table struct */

ems_thread_data_t *ems1Ithreaddata()
{
    ems_msgtab_t *msgtab;
    ems_thread_data_t *dataPtr;
    ems_toktab_t *toktab;

    TRACE( "ems1Ithreaddata" );

    /*  Need to generate a table structure. Note this holds message and
     *  token tables and the character buffer. */
    dataPtr = (ems_thread_data_t *) starMalloc( sizeof(ems_thread_data_t) );

    /*  Need to initialise all tables. */
    ems1Imsgtab( &dataPtr->msgtab );
    ems1Imsgtab( &dataPtr->msgtab_spare );
    ems1Itoktab( &dataPtr->toktab );

    /*  Message table level should be incremented to stop any immediate
     *  flushing, so fake a mark of the stack. */
    msgtab = &dataPtr->msgtab;
    msgtab->msglev++;
    msgtab->msgmrk++;
    msgtab->msgcnt[ msgtab->msgmrk ] = msgtab->msgcnt[ msgtab->msgmrk -1 ];

    toktab = &dataPtr->toktab;
    toktab->toklev++;
    toktab->tokmrk++;
    toktab->tokcnt[ toktab->tokmrk ] = toktab->tokhiw[ toktab->tokmrk-1 ];
    toktab->tokhiw[ toktab->tokmrk ] = toktab->tokhiw[ toktab->tokmrk-1 ];

    return dataPtr;
}
