#include "sae_par.h"
#include "f77.h"
#include "mers.h"
#include "ndf.h"
#include "star/task_adam.h"
#include "star/ndg.h"
#include "star/one.h"
#include "ems.h"
#include "par_par.h"
#include "cupid.h"
#include <string.h>

/* Declare global variables used throughout cupid */
/* ---------------------------------------------- */

/* PixelSet cache used by the ClumpFind algorithm. */
CupidPixelSet **cupid_ps_cache = NULL;
int cupid_ps_cache_size = 0;

void cupid_mon( int *status ) {
/*
*+
*  Name:
*     cupid_mon

*  Purpose:
*     Top-level CUPID function for A-task monolith on UNIX.

*  Language:
*     Starlink C

*  Type of Module:
*     ADAM A-task

*  Description:
*     This is the top-level A-task monolith function for the CUPID
*     suite of A-tasks.  Each CUPID command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Parameters:
*     status
*        Pointer to the global status variable used by the ADAM fixed part.

*  Synopsis:
*     void cupid_mon( int *status );

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.
*     29-JUL-2009 (TIMJ):
*        Call taskGetName rather than Fortran.
*        Add CUPID and version number to NDF history.
*     31-JUL-2009 (DSB):
*        Use ndgBegpv/Endpv to provide automatic provenance propagation.
*     16-OCT-2009 (DSB):
*        Use ndgBeggh/ndgEndgh to record contents of group parameters in
*        the history component of output NDFs.
*     2011-01-19 (TIMJ):
*        Add leak checking to CUPID monolith
*     5-APR-2013 (DSB):
*        Use astCheckMemory rather than astFlushMemory since we do not want
*        to free the memory used to hold the singleton workforce returned by
*        thrGetWorkforce.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables: */
   char appname[NDF__SZAPP+1];    /* Application name for NDF History */
   char buff[PAR__SZNAM+7];       /* Application name for provenance handling */
   char filter[PAR__SZNAM+PAR__SZNAM+1];
   char name[PAR__SZNAM+1];       /* C character variable to hold name */
   int ast_caching;               /* Initial value of AST MemoryCaching tuning parameter */
   int emslev1;                   /* EMS level on entry */
   int emslev2;                   /* EMS level on exit */
   int ngrp0;                     /* Number of grp ids at start */
   int ngrp1;                     /* Number of grp ids at end */
   int nloc0;                     /* Number of active HDS Locators at start */
   int nloc1;                     /* Number of active HDS Locators at end */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* For debugging, watch one of the leaked GRP identifiers listed by the
   call to grpWatch at the end of this routine (if any). */
   /* grpWatch( 3129345, status ); */

/* Read the input error message stack level */
   emsLevel( &emslev1 );

/* Obtain the command from the environment.  This returns uppercase names. */
   taskGetName( name, sizeof(name), status );

/* Update the application name in the NDF history recording
   to include the version number of the application */
   snprintf( appname, NDF__SZAPP, "%-*s (%s V%s)", PAR__SZNAM,
             name, PACKAGE_UPCASE, PACKAGE_VERSION);
   ndfHappn( appname, status );

/* Make AST use the same variable for its inherited status. */
   astWatch( status );

/* Tell AST to re-cycle memory when possible. */
   ast_caching = astTune( "MemoryCaching", 1 );

/* Get the GRP and HDS status for leak checking - need the task name
   to mask out parameter names. Also need to mask out the monlith name */
   one_strlcpy( filter, "!CUPID_MON,!", sizeof(filter), status);
   one_strlcat( filter, name, sizeof(filter), status );
   grpInfoi( NULL, 0, "NGRP", &ngrp0, status );
   hdsInfoI( NULL, "LOCATORS", filter, &nloc0, status );

/* Begin a provenance block. This causes event handlers to be registered
   with the NDF library so that a handler routine in NDG is called every
   time an NDF is opened. This handler routine keeps a record of all
   NDFs that are opened for input or output, until the block is closed
   by calling ndgEndpv. */
   ndgBegpv( status );

/* Begin a GRP NDF history block. This causes the contents of GRP groups
   to be appended to default history text added to any NDFs during the
   block. */
   ndgBeggh( status );

/* Check the string against valid A-task names---if matched then call
   the relevant A-task. */

/* Finds a low frequency background surface. */
   if( !strcmp( name, "FINDBACK" ) ) {
      findback( status );

/* Identifies emission clumps within a 2- or 3D NDF. */
   } else if( !strcmp( name, "FINDCLUMPS" ) ) {
      findclumps( status );

/* Give help on CUPID commands. */
   } else if( !strcmp( name, "CUPIDHELP" ) ) {
      cupidhelp( status );

/* Create simulated data containing clumps and noise. */
   } else if( !strcmp( name, "MAKECLUMPS" ) ) {
      makeclumps( status );

/* Extract clump parameters from another image */
   } else if( !strcmp( name, "EXTRACTCLUMPS" ) ) {
      extractclumps( status );

/* Obtain information about one or more clumps. */
   } else if( !strcmp( name, "CLUMPINFO" ) ) {
      clumpinfo( status );

/* Report an error if the command name is not recognised. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "CUPID_MON_NOCOM", "CUPID: No such command ^CMD.", status );
   }

/* End the GRP NDF history block. */
   ndgEndgh( status );

/* End the provenance block. This will result in every output NDF being
   given a provenance extension containing a record of the input NDFs
   that the application accessed in order to create the output NDF. Any
   output NDF that already contains a provenance extension is left
   unchanged (so individual application can override this automatic
   provenance handling by adding a provenance extension to the output
   NDF itself). */
   sprintf( buff, "CUPID:%s", name );
   ndgEndpv( buff, status );

/* Log the task and its parameters to a log file specified by enviromnent
   variable CUPID_LOG. */
   kpg1Lgcmd( name, "CUPID", status );

/* Re-instate the original value of the AST ObjectCaching tuning
   parameter. */
   astTune( "MemoryCaching", ast_caching );

/* Check for GRP leaks Do this in a new error reporting context so
   that we get the correct value even if an error has occurred. */
   errBegin( status );
   grpInfoi( NULL, 0, "NGRP", &ngrp1, status );

/* If there are more active groups now than there were on entry,
   there must be a problem (GRP identifiers are not being freed
   somewhere). So report it. */
   if (*status == SAI__OK && ngrp1 > ngrp0) {
     msgBlank( status );
     msgSetc( "NAME", name );
     msgSeti( "NGRP0", ngrp0 );
     msgSeti( "NGRP1", ngrp1 );
     msgOut( " ", "WARNING: The number of active "
             "GRP identifiers increased from ^NGRP0 to ^NGRP1 "
             "during execution of ^NAME (" PACKAGE_UPCASE " programming "
             " error).", status);
     msgBlank(status);
     grpWatch( 0, status );
   }
   errEnd( status );

/* Check for HDS leaks Do this in a new error reporting context so
   that we get the correct value even if an error has occurred. */
   errBegin( status );
   hdsInfoI( NULL, "LOCATORS", filter, &nloc1, status );

/* If there are more active locators now than there were on entry,
   there must be a problem (HDS locators are not being freed
   somewhere). So report it. */
   if (*status == SAI__OK && nloc1 > nloc0) {
     msgBlank( status );
     msgSetc( "NAME", name );
     msgSeti( "NLOC0", nloc0 );
     msgSeti( "NLOC1", nloc1 );
     msgOut( " ", "WARNING: The number of active "
             "HDS Locators increased from ^NLOC0 to ^NLOC1 "
             "during execution of ^NAME (" PACKAGE_UPCASE " programming "
             " error).", status);
     msgBlank(status);
     hdsShow("LOCATORS", status);
     hdsShow("FILES", status);
   }
   errEnd( status );

/* Read the exitt error message stack level */
   emsLevel( &emslev2 );

   if (*status == SAI__OK && emslev1 != emslev2 ) {
     errMark();
     msgBlank( status );
     msgSetc( "NAME", name );
     msgSeti( "LV1", emslev1);
     msgSeti( "LV2", emslev2);
     msgOut( " ", "WARNING: EMS Stack level went from ^LV1 to ^LV2"
             " during execution of ^NAME (" PACKAGE_UPCASE " programming"
             " error).", status );
     msgBlank(status);
     errRlse();
   }

/* Make AST use its own internal variable for its inherited status. */
   astWatch( NULL );

/* The astCheckMemory function does nothing unless AST has been compiled
   with the MEM_DEBUG flag. If this is the case, then it reports the number
   of memory blocks that have not been freed (useful for identifying memory
   leaks). Use astActiveMemory() below to list all active memory and
   then use astWatchMemory() at the start of this routine to get reports
   when a particular ID is used. Set a breakpoint in the debugger for
   astMemoryAlarm_ */
   astActiveMemory("Exit:");
   astCheckMemory;

}
