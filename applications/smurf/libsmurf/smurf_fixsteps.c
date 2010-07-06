/*
*+
*  Name:
*     FIXSTEPS

*  Purpose:
*     Fix DC steps in a supplied SCUBA-2 time series NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_fixsteps( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine runs the DC step fixer on a supplied time series (see
*     parameter IN), and optionally writes the corrected data to an
*     output time series (see parameter OUT). It is primarily intended as
*     a debugging tool for the step fixing code. A description of the step
*     fixes performed can be written to a text file (see parameter NEWSTEPS).
*     This can then be supplied as input to a later run of this application
*     in order to check that the new results are the same as the old results
*     (see parameter OLDSTEPS). Configuration parameters for the step fixing
*     algorithm can be supplied either within a "makemap"-style configuration
*     file (see parameter CONFIG), or via command line environment parameters
*     DCFITBOX, DCLIMCORR, etc.

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*        Specifies default values for the configuration parameters used
*        by the step fixing algorithm. This should be a configuration
*        such as supplied for the MAKEMAP command.
*     DCFITBOX = REAL
*        Number of samples (box size) in which the signal RMS is measured
*        for the DC step finder. The run time default value is obtained via
*        the CONFIG parameter. [!]
*     DCLIMCORR = INTEGER
*        The detection threshold for steps that occur at the same time in
*        many bolometers. Set it to zero to suppress checks for correlated
*        steps. If dclimcorr is greater than zero, and a step is found at
*        the same time in more than "dclimcorr" bolometers, then all
*        bolometers are assumed to have a step at that time, and the step
*        is fixed no matter how small it is. The run time default value is
*        obtained via the CONFIG parameter. [!]
*     DCMAXSTEPS = INTEGER
*        The maximum number of steps that can be corrected in each minute of
*        good data (i.e. per 12000 samples) from a bolometer before the
*        entire bolometer is flagged as bad. A value of zero will cause a
*        bolometer to be rejected if any steps are found in the bolometer
*        data stream. The run time default value is obtained via the CONFIG
*        parameter. [!]
*     DCMEDIANWIDTH = INTEGER
*        The width of the median filter used to smooth a bolometer data
*        stream prior to finding DC jumps. The run time default value is
*        obtained via the CONFIG parameter. [!]
*     DCTHRESH = REAL
*        Threshold S/N to detect and flag DC (baseline) steps. The run time
*        default value is obtained via the CONFIG parameter. [!]
*     IN = NDF (Read)
*        The time series cube to be fixed. Note, the data must be time
*        ordered, not bolometer ordered.
*     NEWSTEPS = FILENAME (Write)
*        Name of a text file to create, holding a description of each
*        step that was fixed by the step fixing algorithm. The created
*        file can be re-used in a later run via the OLDSTEPS parameter.
*        If a null (!) value is supplied for NEWSTEPS, no file is
*        created. [!]
*     OLDSTEPS = FILENAME (Read)
*        Name of a text file holding a description of each step that
*        should be fixed, if the step fixing algorithm is working
*        correctly. An error is reported if there is a difference
*        between the steps fixes described in this text file, and the
*        step fixes actually produced by runnning the step fixing
*        algorithm. The text file should have been created by an
*        earlier run of this command (see parameter NEWSTEPS). If a
*        null (!) value is supplied for OLDSTEPS, no check is
*        performed. [!]
*     OUT = NDF (Write)
*        The fixed time series cube. May be null (!), in which case no
*        output NDF is created.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-JUL-2010 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "par.h"
#include "sae_par.h"
#include "star/grp.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/smf.h"


/* Local constants */
#define FUNC_NAME "smurf_fixsteps"


void smurf_fixsteps( int *status ) {

/* Local Variables */
   AstKeyMap *keymap;        /* Default config parameter values */
   AstKeyMap *sub_instruments; /* Info about sub-instruments */
   Grp *igrp = NULL;         /* Input group of files */
   Grp *ogrp = NULL;         /* Output group of files */
   dim_t dcfitbox;           /* DCFITBOX config parameter */
   dim_t dcmedianwidth;      /* DCMEDIANWIDTH config parameter */
   double dcthresh;          /* DCTHRESH config parameter */
   int dclimcorr;            /* DCLIMCORR config parameter */
   int dcmaxsteps;           /* DCMAXSTEPS config parameter */
   int itemp;                /* Intermediate value */
   int nsteps;               /* Number of new step fixes */
   size_t nrej;              /* Number of rejected bolometers */
   size_t outsize;           /* Total number of NDF names in the output group */
   size_t size;              /* Number of files in input group */
   smfData *data = NULL;     /* Output smfData */
   smfData *indata = NULL;   /* Input smfData */
   smfStepFix *newsteps = NULL; /* New step fix descriptions */
   smfWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */

/* Check inherited status */
   if (*status != SAI__OK) return;

/* begin an NDF context. */
   ndfBegin();

/* Get the name of the input NDF. */
   kpg1Rgndf( "IN", 1, 1, "", &igrp, &size, status );

/* Get output file(s) */
   kpg1Wgndf( "OUT", igrp, size, 0, "More output files required...",
               &ogrp, &outsize, status );

/* Open the input data file, read-only. */
   smf_open_file( igrp, 1, "Read", 0, &indata, status );

/* Since we will be modifying the data values, we need a deep copy. */
   data = smf_deepcopy_smfData( indata, 0, 0, status );

/* Place cleaning parameters into a keymap and set defaults. Note that we
   use the map-maker defaults file here so that we populate the locked
   keymap with all the parameters that people may come across to allow
   them to load their map-maker config directly this application. */
   sub_instruments = smf_subinst_keymap( data, NULL, 0, status );
   keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                        sub_instruments, status );
   sub_instruments = astAnnul( sub_instruments );

/* Set the default for each of the step fixing config parameters. */
   astMapGet0I( keymap, "DCMEDIANWIDTH", &itemp );
   parDef0i( "DCMEDIANWIDTH", itemp, status );

   astMapGet0I( keymap, "DCFITBOX", &itemp );
   parDef0i( "DCFITBOX", itemp, status );

   astMapGet0I( keymap, "DCLIMCORR", &itemp );
   parDef0i( "DCLIMCORR", itemp, status );

   astMapGet0I( keymap, "DCMAXSTEPS", &itemp );
   parDef0i( "DCMAXSTEPS", itemp, status );

   astMapGet0D( keymap, "DCTHRESH", &dcthresh );
   parDef0d( "DCTHRESH", dcthresh, status );

/* Get values for the config params */
   parGet0i( "DCMEDIANWIDTH", &itemp, status );
   dcmedianwidth = itemp;

   parGet0i( "DCFITBOX", &itemp, status );
   dcfitbox = itemp;

   parGet0i( "DCLIMCORR", &itemp, status );
   dclimcorr = itemp;

   parGet0i( "DCMAXSTEPS", &itemp, status );
   dcmaxsteps = itemp;

   parGet0d( "DCTHRESH", &dcthresh, status );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = smf_create_workforce( smf_get_nthread( status ), status );

/* Fix the steps. */
   smf_fix_steps( wf, data, NULL, dcthresh, dcmedianwidth, dcfitbox,
                  dcmaxsteps, dclimcorr, &nrej, &newsteps, &nsteps,
                  status );









/* ADD THE OLDSTEPS and NEWSTEPS STUFF HERE..... */










/* If required, create the output NDF. */
   if( outsize > 0 && indata && indata->file ) {
      smf_write_smfData( data, NULL, NULL, NULL, ogrp, 1,
                         indata->file->ndfid, status );
   }

/* Free resources. */
   smf_close_file( &data, status );
   smf_close_file( &indata, status );
   wf = smf_destroy_workforce( wf );
   newsteps = astFree( newsteps );
   grpDelet( &igrp, status );
   grpDelet( &ogrp, status );

/* End the NDF context. */
   ndfEnd( status );

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "FIXSTEPS failed.",
                                      status );
}

