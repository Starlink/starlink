/*
*+
*  Name:
*     JSADICER

*  Purpose:
*     Dice an image or cube into JSA tiles

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_jsadicer( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates multiple output NDFs by dicing a supplied
*     2D- or 3D- NDF up into JSA tiles (i.e. it is the inverse of
*     JSAPASTER). The spatial WCS of the input NDF must matches the
*     JSA all-sky grid.

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input 2D or 3D NDF - it need not have been created by a
*        smurf task. Currently, it should be gridded on the JSA all-sky
*        pixel grid, which means that in practice it probably will have
*        been created by MAKEMAP or MAKECUBE.
*     INSTRUMENT = LITERAL (Read)
*        The JCMT instrument (different instruments have different
*        tiling schemes and pixel sizes). The following instrument
*        names are recognised (unambiguous abbreviations may be
*        supplied): "SCUBA-2(450)", "SCUBA-2(850)", "ACSIS", "DAS". The
*        dynamic default is determined from the input NDF if possible.
*        If this cannot be done, then no dynamic default is provided,
*        and the user is prompted for a value if none was supplied on
*        the command line. []
*     JSATILELIST() = _INTEGER (Write)
*        Returned holding the zero-based indices of the created JSA
*        tiles. The number of such indices is given the "NTILE" parameter
*     NTILE = _INTEGER (Write)
*        AN output parameter which is returned holding the number of
*        output NDFs created.
*     OUT = NDF (Write)
*        The base-name for the output NDFs. The names will be formed by
*        appending the tile number to the basename, preceded by an
*        underscore. A null(!) value causes the name of the input NDF to
*        be used. [!]
*     OUTFILES = LITERAL (Write)
*        The name of a text file to create, in which to put the names of
*        all the output NDFs created by this application via parameter
*        OUT (one per line). If a null (!) value is supplied no file is
*        created. [!]
*     PROJ = LITERAL (Read)
*        Determines the projection used by the output NDFs. The allowed
*        values are "HPX" (HPX projection centred on RA=0h), "HPX12" (HPX
*        projection centred on RA=12h), "XPHN" (XPH projection centred on
*        the north pole) and "XPHS" (XPH projection centred on the south
*        pole). A null (!) value causes "HPX" to be used. ["HPX"]
*     TRIM = _INTEGER (Read)
*        A zero or negative value results in each output NDF covering the
*        full area of the corresponding JSAtile. A value of one results in
*        each output NDF being cropped to the bounds of the supplied NDF. A
*        value of two or more results in each output NDF being cropped to
*        remove any blank borders. [2]

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-NOV-2013 (DSB):
*        Initial version.
*     30-JAN-2014 (DSB):
*        Changed TRIM to allow output NDFs to be trimmed of any bad
*        borders.
*     25-JUN-2014 (DSB):
*        Added parameter USEXPH.
*     8-JUL-2014 (DSB):
*        Change USEXPH to accept "North", "South" or "None" instead of 1,
*        -1 and 0.
*     1-OCT-2014 (DSB):
*        Change USEXPH to PROJ.

*  Copyright:
*     Copyright (C) 2013-2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <float.h>
#include <stdlib.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "par.h"
#include "sae_par.h"
#include "kpg_err.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "star/ndg.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/jsatiles.h"


/* Local constants */
#define FUNC_NAME "smurf_jsadicer"

/* Main entry */
void smurf_jsadicer( int *status ) {

/* Local Variables */
   AstFitsChan *fc;
   Grp *igrp = NULL;
   Grp *ogrp = NULL;
   char *pname;
   char basename[ 255 ];
   char text[ 255 ];
   int indf;
   int trim;
   int ntile;
   size_t size;
   smfJSATiling tiling;
   smf_jsaproj_t proj;

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Begin AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Get the name of the input NDF. */
   kpg1Rgndf( "IN", 1, 1, "", &igrp, &size, status );
   ndgNdfas( igrp, 1, "READ", &indf, status );

/* Get the base name for the output NDFs. */
   if( *status == SAI__OK ) {
      parGet0c( "OUT", basename, sizeof(basename), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         pname = basename;
         grpGet( igrp, 1, 1, &pname, sizeof(basename), status );
      }
   }

/* See how the output NDFs are to be trimmed. */
   parGet0i( "TRIM", &trim, status );

/* Decide what sort of projection to use. */
   parChoic( "PROJ", "HPX", "HPX,HPX12,XPHN,XPHS", 1, text, sizeof(text),
             status );
   proj = smf_jsaproj_fromstr( text, 1, status );

/* Get a FitsChan holding the contents of the FITS extension from the
   input NDF. Annul the error if the NDF has no FITS extension. */
   if( *status == SAI__OK ) {
      kpgGtfts( indf, &fc, status );
      if( *status == KPG__NOFTS ) {
         errAnnul( status );
         fc = NULL;
      }
   }

/* Select a JSA instrument and get the parameters defining the layout of
   tiles for the selected instrument. */
   smf_jsainstrument( "INSTRUMENT", fc, SMF__INST_NONE, &tiling,
                      status );

/* Create a new group to hold the names of the output NDFs that have been
   created. This group does not include any NDFs that correspond to tiles
   that contain no input data. */
   ogrp = grpNew( "", status );

/* Dice the map into output NDFs. */
   smf_jsadicer( indf, basename, trim, tiling.instrument, proj, &ntile,
                 ogrp, status );

/* Write out the list of output NDF names, annulling the error if a null
   parameter value is supplied. */
   if( *status == SAI__OK && ogrp ) {
      grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
      if( *status == PAR__NULL ) errAnnul( status );
   }

/* Write the number of tiles being created to an output parameter. */
   parPut0i( "NTILE", ntile, status );

/* Free resources. */
   grpDelet( &igrp, status );
   grpDelet( &ogrp, status );

/* End the NDF and AST context. */
   ndfEnd( status );
   astEnd;

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "JSADICER failed.",
                                      status );
}

