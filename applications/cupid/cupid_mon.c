#include "sae_par.h"
#include "f77.h"                 
#include "mers.h"
#include "ndf.h"
#include "star/task_adam.h"
#include "star/ndg.h"
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables: */
   char appname[NDF__SZAPP+1];    /* Application name for NDF History */
   char buff[PAR__SZNAM+7];       /* Application name for provenance handling */
   char name[PAR__SZNAM+1];       /* C character variable to hold name */
   int ast_caching;               /* Initial value of AST MemoryCaching tuning parameter */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

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

/* Begin a provenance block. This causes event handlers to be registered
   with the NDF library so that a handler routine in NDG is called every
   time an NDF is opened. This handler routine keeps a record of all 
   NDFs that are opened for input or output, until the block is closed 
   by calling ndgEndpv. */
   ndgBegpv( status );

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

/* End the provenance block. This will result in every output NDF being
   given a provenance extension containing a record of the input NDFs
   that the application accessed in order to create the output NDF. Any
   output NDF that already contains a provenance extension is left
   unchanged (so individual application can override this automatic
   provenance handling by adding a provenance extension to the output 
   NDF itself). */
   sprintf( buff, "CUPID:%s", name );
   ndgEndpv( buff, status );

/* Re-instate the original value of the AST ObjectCaching tuning
   parameter. */
   astTune( "MemoryCaching", ast_caching );

/* Make AST use its own internal variable for its inherited status. */
   astWatch( NULL );

/* Clear out any remaining memory allocated by AST and report
   unintentional leaks. */
   astFlushMemory( 1 );

}
