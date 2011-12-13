/*
 *+
 *  Name:
 *     ems1Kerr

 *  Purpose:
 *     Clear the error message table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Kerr()

 *  Description:
 *     Clear all the error messages at the current context level.

 *  Copyright:
 *     Copyright (C) 1983 Science & Engineering Research Council.
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
 *     JRG: Jack Giddings (UCL)
 *     SLW: Sid Wright (UCL)
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     3-JAN-1983 (JRG):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_KERR
 *     13-MAR-2001 (AJC):
 *        Increase MAXTAB (error in Fortran version)
 *        Start at 1 not 0 if EMS__BASE
 *     13-AUG-2001 (AJC):
 *        #include ems1.h
 *     15-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "sae_par.h"
#include "ems_par.h"                /* EMS_ public constants */
#include "ems_sys.h"                /* EMS_ private constants */
#include "ems1.h"                   /* EMS_ private functions prototypes */
#include "ems_defs.h"               /* EMS_ message table */

/*  Local Constants: */
#define MAXTAB 14                   /*  Maximum tab index */

void ems1Kerr( void )
{
    char line[EMS__SZMSG+1];      /* Constructed output line */
    char tabs[MAXTAB+1];          /* Tab string */

    int i;                        /* Loop index */
    int iend;                     /* Ending loop value */
    int istart;                   /* Starting loop value */
    int istat;                    /* Local status */
    int lstat;                    /* Local status */
    int pstat;                    /* Local status */
    int leng;                     /* Line length */
    int mleng;                    /* Message length */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Kerr" );

    /*  Check the EMS tuning flag MSGRVL and flush the current context if
     *  required. */
    if ( msgtab->msgrvl  ) {

        /*  Initialise the local status values. */
        istat = SAI__OK;
        lstat = SAI__OK;
        pstat = SAI__OK;

        /*  Flush the current context before annulling it. First, find the
         *  first message to flush. */
        if ( msgtab->msgmrk  > EMS__BASE ) {
            istart = msgtab->msgcnt[ msgtab->msgmrk  - 1 ];
        } else {
            istart = 1;
        }

        /*  Find the last message. */
        iend = msgtab->msgcnt[ msgtab->msgmrk  ];

        /*  If there are messages to flush, loop through them. */
        strcpy (tabs, "!! (Annulled) ");

        if ( iend >= istart ) {

            for ( i = istart; i <= iend; i++ ) {
                strcpy (line, tabs);
                leng = MAXTAB;

                /*  Construct the output line and send it. */
                mleng = msgtab->msglen[ i ] ;
                if ( mleng > 0 ) strcat( line , msgtab->msgstr[ i ]  );
                ems1Prerr( line, &istat );

                /*  Check the returned status. */
                if ( istat != SAI__OK ) lstat = istat;

                /*  Only the first message gets the "!! (Annulled) " prefix. */
                strcpy (tabs, "!  (Annulled) ");
            }
        }
    }

    /*  Forget any pending messages at the current context level. */
    if ( msgtab->msgmrk  > EMS__BASE ) {
        msgtab->msgcnt[ msgtab->msgmrk  ] =
            msgtab->msgcnt[ msgtab->msgmrk - 1 ];
    } else {
        msgtab->msgcnt[ msgtab->msgmrk  ] = 0;
        msgtab->msglst  = SAI__OK;
    }

    return;
}
