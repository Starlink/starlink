/*
 *+
 *  Name:
 *     ems1Gmsgtab

 *  Purpose:
 *     Return pointer to an internal messages table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems_msgtab_t *ems1Gmsgtab()

 *  Description:
 *     This routine returns a pointer to either the global message table
 *     or a local message table when invoked from within a thread or
 *     within nested emsMark/emsRlse calls when using the POSIX
 *     threads build. When not using POSIX threads the global table is
 *     always returned.
 *
 *     When working in a threaded application it is mandated that
 *     a pair of emsMark and emsRlse calls are made around any threaded
 *     sections.

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

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if USE_PTHREADS
#include <pthread.h>
#endif

#include <star/mem.h>

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS1 function prototypes */
#include "ems_defs.h"                /* Thread and table data structs */


/* The global error message table. */
extern ems_msgtab_t *ems_msgtab;

#if USE_PTHREADS

/*  Id of the initial thread, should be established prior to any thread
 *  creation. */
extern pthread_t ems_thread_initial_id;

/*  The thread specific data key, this should also be established prior to any
 *  thread creation. */
extern pthread_key_t ems_thread_data_key;

/*  True when the above have been set. */
extern int ems_thread_initial_set;

ems_msgtab_t *ems1Gmsgtab( void )
{
    ems_thread_data_t *dataPtr;

    TRACE( "ems1Gmsgtab" );

    /*  If the thread ID doesn't match that of the initial thread then then
     *  look for a local table. If not found create one and associate it as
     *  thread-specific data. */
    if ( ems_thread_initial_set == 0 ||
         pthread_equal( pthread_self(), ems_thread_initial_id ) ) {

        /* This is the initial thread, so we use the global table. */
        return ems_msgtab;
    }

    /*  In a thread. Look for an existing thread specific value. */
    dataPtr = (ems_thread_data_t *)pthread_getspecific( ems_thread_data_key );
    if ( dataPtr == NULL ) {

        dataPtr = ems1Ithreaddata();

        /*  And set as thread specific data. */
        pthread_setspecific( ems_thread_data_key, dataPtr );
    }

    /*  Return the table. */
    return &dataPtr->msgtab;
}
#else

ems_msgtab_t *ems1Gmsgtab( void )
{
    TRACE( "ems1Gmsgtab" );
    /* No threads, so always return global table. */
    return ems_msgtab;
}
#endif
