/*
 *+
 *  Name:
 *     ems1Fthreadata

 *  Purpose:
 *     Free thread specific data when thread exits.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     void ems1Fthreaddata()

 *  Description:
 *     This routine will be called when a thread exits and will release any
 *     resources allocated for that thread. If the thread error status is set
 *     then the error context will be merged into that of the global error
 *     table, so that the context can be reported.

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

#include <star/mem.h>

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS1 function prototypes */
#include "ems_defs.h"                /* Token table struct */

#include <pthread.h>

/* Mutex for protecting transfer to global table. */
static pthread_mutex_t foo_mutex = PTHREAD_MUTEX_INITIALIZER;

/* The global error table. */
extern ems_msgtab_t *ems_msgtab;

void ems1Fthreaddata( void *ptr )
{
    ems_msgtab_t msgtab;
    ems_thread_data_t *dataPtr = (ems_thread_data_t *) ptr;
    int i;
    int iend;
    int istart;

    TRACE( "ems1Fthreaddata" );

    /*  Check the error context and see if we need to merge this into the
     *  global one. */
    msgtab = dataPtr->msgtab;

    /*  Get indices of first and last messages. */
    iend = msgtab.msgcnt[ msgtab.msgmrk ];
    istart = msgtab.msgcnt[ msgtab.msgmrk - 1 ] + 1;
    if ( iend >= istart ) {

        /*  Enable mutex. */
        pthread_mutex_lock( &foo_mutex );

        /*  Transfer all messages, one by one */
        for ( i = istart; i <= iend; i++ ) {
            ems1Estor1( ems_msgtab, msgtab.msgpar[ i ], msgtab.msgpln[ i ],
                        msgtab.msgstr[ i ], msgtab.msglen[ i ],
                        &msgtab.msgsta[ i ] );
        }

        /*  Release mutex. */
        pthread_mutex_unlock( &foo_mutex );
    }

    /*  Free the allocated structure. */
    starFree( dataPtr );
}
#else

/*  Dummy implementation when not using threads. */
void ems1Fthreaddata( void ) {}

#endif
