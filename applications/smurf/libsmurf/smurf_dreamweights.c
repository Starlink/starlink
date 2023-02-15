/*
*+
*  Name:
*     DREAMWEIGHTS

*  Purpose:
*     Generate DREAM weights matrix.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_dreamweights( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for (re)calculating the DREAM weights array
*     and inverse matrix.

*  Notes:
*     - Raw data MUST be passed in at present (this is due to a
*       limitation in smf_open_file)
*     - Should allow for a list of bad (dead) bolometers.
*     - This application interface is not finalised. Please do not rely on this
*       command in scripts.

*  ADAM Parameters:
*     CONFIG = Literal (Read)
*          Specifies values for the configuration parameters used to determine
*          the grid properties. If the string "def"
*          (case-insensitive) or a null (!) value is supplied, a set of
*          default configuration parameter values will be used.
*
*          The supplied value should be either a comma-separated list of
*          strings or the name of a text file preceded by an up-arrow
*          character "^", containing one or more comma-separated list of
*          strings. Each string is either a "keyword=value" setting, or
*          the name of a text file preceded by an up-arrow character
*          "^". Such text files should contain further comma-separated
*          lists which will be read and interpreted in the same manner
*          (any blank lines or lines beginning with "#" are
*          ignored). Within a text file, newlines can be used as
*          delimiters as well as commas. Settings are applied in the
*          order in which they occur within the list, with later
*          settings over-riding any earlier settings given for the same
*          keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The parameters available for are listed in the "Configuration
*          Parameters" sections below. Default values will be used for
*          any unspecified parameters. Unrecognised options are ignored
*          (that is, no error is reported). [current value]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NDF = NDF (Read)
*          Raw DREAM data file(s)
*     OUT = NDF (Write)
*          Output weights file(s)

*  Configuration Parameters:
*     GRIDSTEP = INTEGER
*         Scale size of the dream grid pattern. [6.28 arcsec]
*     GRIDMINMAX = INTEGER
*         Array of integers specify the extent of the DREAM
*         pattern in pixels. Order is xmin, xmax, ymin,
*         ymax [(-4,4,-4,4)]

*  Related Applications:
*     SMURF: DREAMSOLVE

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-08-22 (AGG):
*        Initial version, copied from calcmapwts written by BDK
*     2006-09-15 (AGG):
*        Factor out determining the grid parameters into
*        smf_dream_getgrid
*     2006-09-15 (AGG):
*        Factor out remaining code into smf_dream_calcweights
*     2006-11-10 (AGG):
*        Set some default values if a config file is not specified
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-05-28 (TIMJ):
*        Allow CONFIG=!
*     2008-07-11 (AGG):
*        Tidy up, ensure all pointers are freed
*     2008-07-14 (AGG):
*        Make sure Grp for config is always freed
*     2008-07-22 (TIMJ):
*        Use kaplibs instead of ndgAssoc
*     2009-08-27 (AGG):
*        Template data file is specified by NDF, not IN
*     2009-09-18 (TIMJ):
*        Document config parameters. Use vector syntax for
*        GRIDMINMAX. Remove need for malloc of gridminmax.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2006-2009 the University of British Columbia. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "smurf_typ.h"

#define FUNC_NAME "smurf_dreamwweights"
#define TASK_NAME "DREAMWEIGHTS"

void smurf_dreamweights ( int *status ) {

  /* Local Variables */
  Grp *confgrp = NULL;        /* Group containing configuration file */
  smfData *data = NULL;       /* Input data */
  int defgridminmax[] = { -4, 4, -4, 4 }; /* Default extent xmin,xmax,ymin,ymax */
  int gridminmax[4];          /* Extent of grid points array */
  int gridpts[DREAM__MXGRID][2]; /* Array of points for reconstruction grid */
  double gridstep;            /* Size of reconstruction grid in arcsec */
  size_t i;                   /* Loop counter */
  Grp *igrp = NULL;           /* Input group of NDFs */
  size_t size;                /* Size of input Grp of files */
  AstKeyMap *keymap = NULL;   /* Pointer to keymap of config settings */
  size_t ksize;               /* Size of group containing CONFIG file */
  int ngrid;                  /* Number of points in reconstruction grid */
  Grp *ogrp = NULL;           /* Group of output weights files */
  size_t outsize;             /* Size of output Grp of files */

  /* Main routine */
  ndfBegin();

  /* Get group of input raw data NDFs */
  kpg1Rgndf( "NDF", 0, 1, "", &igrp, &size, status );

  /* Get group of output files from user: assume 1 output file for
     every input file */
  kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* Read configuration settings into keymap */
  if (*status == SAI__OK) {
    kpg1Gtgrp( "CONFIG", &confgrp, &ksize, status );
    if (*status == PAR__NULL) {
      /* NULL value so provide defaults */
      errAnnul( status );
      msgOutif(MSG__VERB, " ", "No config file specified - assuming default configuration parameters", status);
      keymap = astKeyMap(" " );
      astMapPut1I( keymap, "GRIDMINMAX", 4, defgridminmax, " " );
      astMapPut0D( keymap, "GRIDSTEP", 6.28, " " );
    } else {
      kpg1Kymap( confgrp, &keymap, status );
    }
    if( confgrp ) grpDelet( &confgrp, status );
  }

  /* Determine grid parameters from inputs given above */
  smf_dream_getgrid( keymap, &gridstep, &ngrid, gridminmax, gridpts, status);
  /* Annul keymap immediately as it is no longer required */
  if (keymap) keymap = astAnnul( keymap );

  /* Loop over number of files */
  for ( i=1; (i<= size) && (*status == SAI__OK); i++) {
    /* Open file */
    smf_open_file( NULL, igrp, (int) i, "READ", 0, &data, status );

    /* Calculate weights based on this file */
    smf_dream_calcweights( data, ogrp, (int) i, gridstep, ngrid, gridminmax,
                           &(gridpts[0]), status);

    /* Immediately check status on return and abort if an error occured */
    if ( *status != SAI__OK ) {
      msgSetk("I",i);
      msgSetk("N",size);
      errRep(FUNC_NAME, "Unable to determine DREAM weights for file ^I of ^N",
             status);
    }

    smf_close_file( NULL, &data, status );
  }

  /* Free up resources */
  if ( ogrp != NULL ) {
    grpDelet( &ogrp, status);
  }
  if ( igrp != NULL ) {
    grpDelet( &igrp, status);
  }
  ndfEnd( status );

  msgOutif(MSG__VERB," ", "DREAM weights calculation completed successfully",
           status);
}
