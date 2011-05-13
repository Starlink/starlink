/*+
 *  Name:
 *     gaiaMon

 *  Purpose:
 *     Top-level ADAM monolith routine for the GAIA applications.

 *  Language:
 *     C

 *  Synopsis:
 *     void gaiaMon( int *status );

 *  Description:
 *     This routine obtains the name of the current action and calls
 *     the appropriate routine to perform the specified operation. An
 *     error will be reported and STATUS will be set if the action
 *     name is not recognised.

 *  Arguments:
 *     status = pointer to integer (read and write)
 *        The global status.

 *  Notes:
 *     -  Only contains GAIA internal applications.

 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Copyright:
 *     Copyright (C) 2000 Central Laboratory of the Research Councils
 *     Copyright (C) 2011 Science & Technology Facilities Council.
 *     All Rights Reserved.

 *  Authors:
 *     PWD: Peter W. Draper (Starlink - Durham University)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     19-JUL-2000 (PWD):
 *        Original version.
 *     13-MAY-2011 (TIMJ):
 *        PCS task_get_name now has a C interface.
 *     {enter_further_changes_here}

 *-
 */

/*  Include files */
#include <strings.h>
#include "sae_par.h"
#include "par_par.h"
#include "f77.h"
#include "merswrap.h"
#include "star/task_adam.h"

/*  Define macro to generate functions for invoking any Fortran
 *  actions. CNAME is the function name as will be called from C (add
 *  these to the actionStruct) and FNAME is the Fortran subroutine
 *  name.
 */
#define GENERATE_FORTRANCMD( CNAME, FNAME ) \
   extern void F77_EXTERNAL_NAME( FNAME )( INTEGER( fstatus ) ); \
   static void CNAME( int *status ) \
{ \
   DECLARE_INTEGER(fstatus); \
   F77_EXPORT_INTEGER( *status, fstatus ); \
   F77_CALL( FNAME )( INTEGER_ARG( &fstatus ) ); \
   F77_IMPORT_INTEGER( fstatus, *status ); \
   return; \
}

/*  ====================================== */
/*  Generate Fortran stubs for any actions */
/*  ====================================== */
GENERATE_FORTRANCMD( ardstatCmd,  ardstat )
GENERATE_FORTRANCMD( asc2tabCmd,  asc2tab )
GENERATE_FORTRANCMD( autocropCmd, autocrop )
GENERATE_FORTRANCMD( cat2tabCmd,  cat2tab )
GENERATE_FORTRANCMD( tab2ascCmd,  tab2asc )
GENERATE_FORTRANCMD( tab2catCmd,  tab2cat )
GENERATE_FORTRANCMD( ardspectraCmd,  ardspectra )

/*  ==============================================  */
/*  Define the list of actions that we can invoke. */
/*  ==============================================  */
static struct actionStruct {
    char const *name;              /* Name of action */
    void (*fptr)( int *status );   /* Ptr to invoking function */
} actions_[] = {
    { "ardstat",  ardstatCmd  },
    { "asc2tab",  asc2tabCmd  },
    { "autocrop", autocropCmd },
    { "cat2tab",  cat2tabCmd  },
    { "tab2asc",  tab2ascCmd  },
    { "tab2cat",  tab2catCmd  },
    { "ardspectra",  ardspectraCmd  },
};

/*  ============= */
/*  Main function */
/*  ============= */
void gaiaMon( int *status )
{
    /*  Local Variables: */
    char name[PAR__SZNAM+1];
    unsigned int i;
    struct actionStruct *action;

    /*  Check inherited global status. */
    if ( *status != SAI__OK ) return;

    /*  Get the action name. */
    taskGetName( name, PAR__SZNAM+1, status );

    /*  If the name is known then invoke the action */
    for ( i = 0; i < sizeof( actions_ ) / sizeof( *actions_ ); i++ ) {
        action = &actions_[i];
        if ( strncasecmp( action->name, name, PAR__SZNAM ) == 0 ) {
            ( action->fptr )( status );
            return;
        }
    }

    /*  If the action name is not recognised, so report an error. */
    *status = SAI__ERROR;
    msgSetc( "NAME", name );
    errRep( "GAIA_MON_ERR",
            "GAIA_MON: The action name '^NAME' is not recognised by "
            "the GAIA monolith.", status );
    return;
}
