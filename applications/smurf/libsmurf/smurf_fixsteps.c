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
*     (see parameter OLDSTEPS). A warning message will be displayed if
*     any significant difference is found between the old step fixes and the
*     new step fixes.
*
*     Configuration parameters for the step fixing algorithm can be supplied
*     either within a "makemap"-style configuration file (see parameter
*     CONFIG), or via command line environment parameters DCFITBOX,
*     DCSMOOTH, etc.

*  ADAM Parameters:
*     CHANGED = _LOGICAL (Write)
*        An output parameter to which is written a flag indicating if any
*        significant differences were found between the step fixes
*        produced by the current invocation of this program, and the step
*        fixes described in the file specified via parameter OLDSTEPS.
*     CONFIG = GROUP (Read)
*        Specifies default values for the configuration parameters used
*        by the step fixing algorithm. This should be a configuration
*        such as supplied for the MAKEMAP command. [!]
*     CONTINUE = _LOGICAL (Read)
*        This parameter is prompted for after each changed step is
*        described. If TRUE is supplied, then the program continues to
*        display details of further changed steps. If FALSE is supplied,
*        the program aborts.
*     DCFITBOX = _REAL
*        Number of samples (box size) in which the signal RMS is measured
*        for the DC step finder. The run time default value is obtained via
*        the CONFIG parameter. [!]
*     DCLIMCORR = _INTEGER
*        The detection threshold for steps that occur at the same time in
*        many bolometers. Set it to zero to suppress checks for correlated
*        steps. If dclimcorr is greater than zero, and a step is found at
*        the same time in more than "dclimcorr" bolometers, then all
*        bolometers are assumed to have a step at that time, and the step
*        is fixed no matter how small it is. The run time default value is
*        obtained via the CONFIG parameter. [!]
*     DCMAXSTEPS = _INTEGER
*        The maximum number of steps that can be corrected in each minute of
*        good data (i.e. per 12000 samples) from a bolometer before the
*        entire bolometer is flagged as bad. A value of zero will cause a
*        bolometer to be rejected if any steps are found in the bolometer
*        data stream. The run time default value is obtained via the CONFIG
*        parameter. [!]
*     DCSMOOTH = _INTEGER
*        The width of the median filter used to smooth a bolometer data
*        stream prior to finding DC jumps. The run time default value is
*        obtained via the CONFIG parameter. [!]
*     DCTHRESH = _REAL
*        Threshold S/N to detect and flag DC (baseline) steps. The run time
*        default value is obtained via the CONFIG parameter. [!]
*     FIRST = _INTEGER
*        The index of the first change to be display (the first change
*        has index 1). Each change report starts with an index followed
*        by a colon. [1]
*     IN = NDF (Read)
*        The time series cube to be fixed. Note, the data must be time
*        ordered (like the original raw data), not bolometer ordered.
*     MEANSHIFT = _LOGICAL (Read)
*        Use a mean shift filter prior to step fixing? A mean-shift filter
*        is an edge-preserving smooth. It can help to identify smaller
*        steps, but does not work well if there are strong gradients in
*        the bolometer time stream. Therefore, MEANSHIFT should only
*        be used if the common-mode signal has been subtracted. The
*        spatial width of the filter is given by DCSMOOTH, and the range
*        of data values accepted by the filter is 5 times the local RMS
*        in the original time stream. [FALSE]
*     NEWSTEPS = FILENAME (Write)
*        Name of a text file to create, holding a description of each
*        step that was fixed by the step fixing algorithm. The created
*        file can be re-used in a later run via the OLDSTEPS parameter.
*        It can also be viewed using "topcat -f ascii". If a null (!)
*        value is supplied for NEWSTEPS, no file is created. [!]
*     NFIXED = _INTEGER (Write)
*        The number of steps fixed.
*     NREJECTED = _INTEGER (Write)
*        The number of bolometers rejected due to them containing too many
*        steps.
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
*     SIZETOL = _DOUBLE (Read)
*        Gives the fraction (i.e. relative error) by which two step sizes
*        must differ for them to be considered different. In addition,
*        the absolute difference between two step sizes must also differ
*        by more than SIZETOL times the clipped RMS step size. [0.05]

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-JUL-2010 (DSB):
*        Initial version.
*     9-DEC-2011 (DSB):
*        Added MEANSHIFT parameter.
*     24-MAY-2012 (DSB):
*        Added NREJECTED and NFIXED parameters.

*  Copyright:
*     Copyright (C) 2010-2012 Science and Technology Facilities Council.
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
#include "star/grp.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/smf.h"


/* Local constants */
#define FUNC_NAME "smurf_fixsteps"

/* Prototypes for local functions: */
static int smf1_check_steps( const char *param, int first, dim_t nx,
                             double sizetol, int nold, dim_t nnew,
                             smfStepFix *oldsteps, smfStepFix *newsteps,
                             int *status );

static void smf1_write_steps( FILE *fd, smfData *data, dim_t nstep,
                              smfStepFix *steps, double dcthresh,
                              int dcsmooth, int dcfitbox,
                              int dcmaxsteps, int dclimcorr, dim_t nrej,
                              int *status );

static smfStepFix *smf1_read_steps( FILE *fd, double dcthresh0,
                                    dim_t dcsmooth0, dim_t dcfitbox0,
                                    int dcmaxsteps0, int dclimcorr0,
                                    dim_t nrej0, dim_t nstep0, int *nstep,
                                    int *status );

static double smf1_get_rmssize( dim_t nstep, smfStepFix *steps, int *status );

/* Main entry */
void smurf_fixsteps( int *status ) {

/* Local Variables */
   AstKeyMap *keymap;        /* Default config parameter values */
   AstKeyMap *sub_instruments; /* Info about sub-instruments */
   FILE *fd = NULL;          /* File descriptor */
   Grp *igrp = NULL;         /* Input group of files */
   Grp *ogrp = NULL;         /* Output group of files */
   ThrWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
   dim_t nrej;               /* Number of rejected bolometers */
   dim_t nx;                 /* Length of first pixel axis */
   double dcthresh;          /* DCTHRESH config parameter */
   double sizetol;           /* Tolerance allowed on step height */
   int changed;              /* Have any step fixes changed? */
   int dcfitbox;             /* DCFITBOX config parameter */
   int dclimcorr;            /* DCLIMCORR config parameter */
   int dcmaxsteps;           /* DCMAXSTEPS config parameter */
   int dcsmooth;             /* DCSMOOTH config parameter */
   int first;                /* Index of first change to report */
   int itemp;                /* Intermediate value */
   int meanshift;            /* Use a mean shift filter? */
   dim_t nnew;               /* Number of new step fixes */
   int nold;                 /* Number of old step fixes */
   size_t outsize;           /* Total number of NDF names in the output group */
   size_t size;              /* Number of files in input group */
   smfData *data = NULL;     /* Output smfData */
   smfData *indata = NULL;   /* Input smfData */
   smfStepFix *newsteps = NULL; /* New step fix descriptions */
   smfStepFix *oldsteps = NULL; /* Old step fix descriptions */

/* Check inherited status */
   if (*status != SAI__OK) return;

/* begin an NDF context. */
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get the name of the input NDF. */
   kpg1Rgndf( "IN", 1, 1, "", &igrp, &size, status );

/* Get output file(s) */
   kpg1Wgndf( "OUT", igrp, size, 0, "More output files required...",
               &ogrp, &outsize, status );

/* Open the input data file, read-only. */
   smf_open_file( NULL, igrp, 1, "Read", 0, &indata, status );

/* Since we will be modifying the data values, we need a deep copy. */
   data = smf_deepcopy_smfData( wf, indata, 0, 0, 0, 0, status );

/* Place cleaning parameters into a keymap and set defaults. Note that we
   use the map-maker defaults file here so that we populate the locked
   keymap with all the parameters that people may come across to allow
   them to load their map-maker config directly this application. */
   sub_instruments = smf_subinst_keymap( SMF__SUBINST_NONE, data, NULL, 0,
                                         status );
   keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                        sub_instruments, 1, status );
   sub_instruments = astAnnul( sub_instruments );

/* Set the default for each of the step fixing config parameters. */
   astMapGet0I( keymap, "DCSMOOTH", &itemp );
   parDef0i( "DCSMOOTH", itemp, status );

   astMapGet0I( keymap, "DCFITBOX", &itemp );
   parDef0i( "DCFITBOX", itemp, status );

   astMapGet0I( keymap, "DCMAXSTEPS", &itemp );
   parDef0i( "DCMAXSTEPS", itemp, status );

   astMapGet0I( keymap, "DCLIMCORR", &itemp );
   parDef0i( "DCLIMCORR", itemp, status );

   astMapGet0D( keymap, "DCTHRESH", &dcthresh );
   parDef0d( "DCTHRESH", dcthresh, status );

/* Get values for the config params */
   parGet0i( "DCSMOOTH", &itemp, status );
   dcsmooth = itemp;

   parGet0i( "DCFITBOX", &itemp, status );
   dcfitbox = itemp;

   parGet0i( "DCMAXSTEPS", &itemp, status );
   dcmaxsteps = itemp;

   parGet0i( "DCLIMCORR", &itemp, status );
   dclimcorr = itemp;

   parGet0d( "DCTHRESH", &dcthresh, status );

   parGet0l( "MEANSHIFT", &meanshift, status );

/* Fix the steps. */
   smf_fix_steps( wf, data, dcthresh, dcsmooth, dcfitbox, dcmaxsteps,
                  dclimcorr, meanshift, &nrej, &newsteps, &nnew, status );

/* Display a summary of what was done by the step fixer. */
   msgBlank( status );
   if( nrej == 0 ) {
      msgOut( "", "No bolometers were rejected", status );
   } else if( nrej == 1 ) {
      msgOut( "", "One bolometer was rejected", status );
   } else {
      msgSetk( "NREJ", nrej );
      msgOut( "", "^NREJ bolometers were rejected", status );
   }
   parPut0k( "NREJECTED", nrej, status );

   if( nnew == 0 ) {
      msgOut( "", "No steps were fixed", status );
   } else if( nnew == 1 ) {
      msgOut( "", "One step was fixed", status );
   } else {
      msgSetk( "NNEW", nnew );
      msgOut( "", "^NNEW steps were fixed", status );
   }
   parPut0k( "NFIXED", nnew, status );

/* If required, write out to a text file details of the steps that were
   fixed. */
   fd = smf_open_textfile( "NEWSTEPS", "w", "<none>", status );
   if( fd ) {
      smf1_write_steps( fd, indata, nnew, newsteps, dcthresh, dcsmooth,
                        dcfitbox, dcmaxsteps, dclimcorr, nrej, status );
      fclose( fd );
   }

/* If required, create the output NDF. */
   if( outsize > 0 && indata && indata->file ) {
      smf_write_smfData( NULL, data, NULL, NULL, ogrp, 1,
                         indata->file->ndfid, MSG__VERB, 0, NULL, NULL, status );
   }

/* Save the length of the first pixel axis. */
   nx = data ? data->dims[ 0 ] : 0;

/* Close the NDFs. */
   smf_close_file( wf, &data, status );
   smf_close_file( wf, &indata, status );

/* Attempt to open a file containing descriptions of steps fixed by a
   previous invocation of this program. */
   fd = smf_open_textfile( "OLDSTEPS", "r", "<none>", status );
   if( fd ) {

/* Get SIZETOL - the minimum significant fractional error in step sizes. */
      parGet0d( "SIZETOL", &sizetol, status );

/* Read the contents of the file, issuing a warning if the global
   properties read from the file (e.g. parameters used, no. of steps
   found, etc) differ from those of the current invocation. */
      msgBlank( status );
      oldsteps = smf1_read_steps( fd, dcthresh, dcsmooth,
                                  dcfitbox, dcmaxsteps, dclimcorr,
                                  nrej, nnew, &nold, status );

/* Get the index of the first change to report. */
      parGet0i( "FIRST", &first, status );

/* Compare the new step fixes with the old step fixes, issuing a warning
   for the first step fix that has changed. */
      changed = smf1_check_steps( "CONTINUE", first, nx, sizetol,
                                  nold, nnew, oldsteps, newsteps, status );

/* Store a flag indicating if any sstep fixes have chnaged. */
      parPut0l( "CHANGED", changed, status );

/* Tell the user if nothing has changed. */
      if( ! changed ) {
         msgOut( "", "There are no significant differences "
                 "between old and new step fixes.", status );
      }
      msgBlank( status );

/* Close the old steps file, and free the memory holding the old step
   descriptions. */
      fclose( fd );
      oldsteps = astFree( oldsteps );
   }

/* Free resources. */
   newsteps = astFree( newsteps );
   grpDelet( &igrp, status );
   grpDelet( &ogrp, status );

/* End the NDF context. */
   ndfEnd( status );

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "FIXSTEPS failed.",
                                      status );
}


static smfStepFix *smf1_read_steps( FILE *fd, double dcthresh0,
                                    dim_t dcsmooth0, dim_t dcfitbox0,
                                    int dcmaxsteps0, int dclimcorr0,
                                    dim_t nrej0, dim_t nstep0, int *nstep,
                                    int *status ) {
/*
*  Name:
*     smf1_read_steps

*  Purpose:
*     Read step descriptions from a file, and check global values.

*  Invocation:
*     smfStepFix *smf1_read_steps( FILE *fd, double dcthresh0,
*                                  dim_t dcsmooth0, dim_t dcfitbox0,
*                                  int dcmaxsteps0, int dclimcorr0,
*                                  dim_t nrej0, dim_t nstep0, int *nstep,
*                                  int *status )

*  Arguments:
*     fd = FILE * (Given)
*        A file descriptor from which to read the details of a set of
*        steps.
*     dcthresh0 = double (Given)
*        Expected value of DCTHRESH.
*     dcsmooth0 = dim_t (Given)
*        Expected value of DCSMOOTH.
*     dcfitbox0 = dim_t (Given)
*        Expected value of DCFITBOX.
*     dcmaxsteps0 = int (Given)
*        Expected value of DCMAXSTEPS.
*     dclimcorr = int (Given)
*        Expected value of DCLIMCORR.
*     nrej0 = dim_t (Given)
*        The expected number of bolometers rejected.
*     nstep0 = dim_t (Given)
*        The expected number of step fixes.
*     nstep = int * (Returned)
*        The number of steps fixes read from the file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reads information from the supplied file, returning an array of
*     step fixes. It also issues warnings if any of the global values
*     read from the file are not equal to the supplied expected values.

*  Returned Value:
*     A pointer to an array of smfStepFix structures describing the
*     steps fixes read form the file. The length of this array is equal
*     to "*nstep". The array should be freed using astFree when no longer
*     needed.

*/

/* Local Variables: */
   smfStepFix *result;
   char buf[ 256 ];
   char *c;
   int ival;
   double dval;
   double size;
   int bad;
   int corr;
   int end;
   int ibolo;
   int iline;
   int istep;
   int nc;
   int nold;
   int stage;
   int start;

/* Initialise */
   result = NULL;
   *nstep = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Indicate we have not yet reached the tabular data. */
   stage = 0;

/* Initialise the index of the next step */
   istep = 0;

/* Indicate we do not yet know how many steps are described in the text
   file. */
   nold = -1;

/* Indicate no bad lines found yet. */
   bad = 0;

/* Loop round reading lines of text from the supplied file until an
   illegal line is read or the end of file is reached. */
   iline = 0;
   while( !bad && fgets( buf, sizeof(buf), fd ) && *status == SAI__OK ) {
      iline++;

/* Remove trailing white space. */
      c = buf + strlen( buf ) - 1;
      while( isspace( *c ) && c > buf ) c--;
      c[ 1 ] = 0;

/* scanf indicates success if the strings do not fail to match before the
   end of the shorter of the two. So we need to check that sufficient
   characters are compared. Initialise the number of characters compared. */
      nc = 0;

/* If we are not yet in the tabular data, look for header lines, and
   issue a message if the old value is different to the new value. */
      if( stage == 0 ) {

         if( sscanf( buf, "# Number of steps fixed = %d%n", &ival, &nc )
                     && nc > 26 ) {
            if( (dim_t) ival != nstep0 ) {
               msgSeti( "O", ival );
               msgSetk( "N", nstep0 );
               msgOut( "", "No. of steps fixed changed from ^O to ^N",
                       status );
            }

            nold = ival;

         } else if( sscanf( buf, "# Number of bolometers rejected = %d%n",
                            &ival, &nc ) && nc > 34 ) {
            if( ival != (int) nrej0 ) {
               msgSetk( "O", ival );
               msgSetk( "N", nrej0 );
               msgOut( "", "No. of bolometers rejected changed from ^O to ^N",
                       status );
            }

         } else if( sscanf( buf, "# DCFITBOX = %d%n", &ival, &nc ) && nc > 13 ) {
            if( ival != (int) dcfitbox0 ) {
               msgSetk( "O", ival );
               msgSetk( "N", dcfitbox0 );
               msgOut( "", "Warning: DCFITBOX changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCMAXSTEPS = %d%n", &ival, &nc ) && nc > 14 ) {
            if( ival != dcmaxsteps0 ) {
               msgSetk( "O", ival );
               msgSetk( "N", dcmaxsteps0 );
               msgOut( "", "Warning: DCMAXSTEPS changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCLIMCORR = %d%n", &ival, &nc ) && nc > 14 ) {
            if( ival != dclimcorr0 ) {
               msgSetk( "O", ival );
               msgSetk( "N", dclimcorr0 );
               msgOut( "", "Warning: DCLIMCORR changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCSMOOTH = %d%n", &ival, &nc )
                    && nc > 18 ) {
            if( ival != (int) dcsmooth0 ) {
               msgSetk( "O", ival );
               msgSetk( "N", dcsmooth0 );
               msgOut( "", "Warning: DCSMOOTH changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCTHRESH = %lg%n", &dval, &nc ) && nc > 13 ) {
            if( fabs( dval - dcthresh0 ) > 1.0E-10 ) {
               msgSetd( "O", dval );
               msgSetd( "N", dcthresh0 );
               msgOut( "", "Warning: DCTHRESH changed from ^O to ^N", status );
            }

/* Look for the line that marks the start of the tabular data. */
         } else if( !strcmp( buf, "# istep start end ibolo size corr" ) ) {
            stage = 1;
            msgBlank( status );

/* Allocate the returned array. */
            result = astMalloc( nold*sizeof( *result ) );
            *nstep = nold;

/* Abort if an illegal header line is read. */
         } else if( strcmp( buf, "#" ) &&
                    strncmp( buf, "# Steps fixed in '", 18 ) ) {
            bad = 1;
         }

/* If we are now reading tabular data... */
      } else {

/* Extract the numerical values from the line of text. */
         if( sscanf( buf, "%d %d %d %d %lg %d%n", &ival, &start, &end, &ibolo,
                     &size, &corr, &nc ) == 6 && nc > 14 ) {

/* Report an error if there is a jump in the step index (indicates lines
   missing from the supplied file). */
            if( ival != istep ) {
               *status = SAI__ERROR;
               msgSetk( "I", istep );
               errRep( "", "Step ^I data not found in old steps file:", status );
               bad = 1;

/* Otherwise, store the numerical values in the next element of the
   returned array. */
            } else if( istep < nold ){
               result[ istep ].id = ival;
               result[ istep ].ibolo = ibolo;
               result[ istep ].start = start;
               result[ istep ].end = end;
               result[ istep ].size = size;
               result[ istep ].corr = corr;
            }

/* Increment the index of the next step to check. */
            istep++;

/* Abort if the numerical values cannot be read from the line of text. */
         } else {
            bad = 1;
         }
      }
   }

/* Report an error if the last line read was illegal. */
   if( bad ) {
      *status = SAI__ERROR;
      msgSetk( "I", iline );
      errRep( "", "Illegal line found in old steps file (line ^I):", status );
      msgSetc( "L", buf );
      errRep( "", "'^L'", status );

/* Report an error if the number of steps in the old file is still unknown */
   } else if( nold == -1 ) {
      *status = SAI__ERROR;
      errRep( "", "Required line not found in old steps file:", status );
      errRep( "", "'# Number of steps fixed = ...'", status );

/* Report an error if the number of lines of tabular data was wrong. */
   } else if( istep != nold ) {
      *status = SAI__ERROR;
      errRep( "", "Incorrect number of step descriptions in old steps file.",
              status );
      msgSetk( "I", istep );
      msgSetk( "N", nold );
      errRep( "", "Header says file contains ^N steps but data for ^I "
              "steps was found.", status );
   }

   return result;
}


static int smf1_check_steps( const char *param, int first, dim_t nx,
                             double sizetol, int nold, dim_t nnew,
                             smfStepFix *oldsteps, smfStepFix *newsteps,
                             int *status ){
/*
*  Name:
*     smf1_check_steps

*  Purpose:
*     Compare two sets of steps, issuing a warning for each step that
*     has changed significantly.

*  Invocation:
*     int smf1_check_steps( const char *param, int first, dim_t nx,
*                           double sizetol, int nold, dim_t nnew,
*                           smfStepFix *oldsteps, smfStepFix *newsteps,
*                           int *status )

*  Arguments:
*     param = const char * (Given)
*        Name of parameter to use when asking the user whether to
*        continue to look for further changes.
*     first = int (Given)
*        The index of the first change to report.
*     nx = dim_t (Given)
*        The length of the first axis of the bolometer array.
*     sizetol = double (Given)
*        The minimum significant relative error in step size.
*     nold = int (Given)
*        The number of steps in the "oldsteps" array.
*     nnew = dim_t (Given)
*        The number of steps in the "newsteps" array.
*     oldsteps = smfStepFix * (Given and Returned)
*        A pointer to the first element of an array of smfStepFix structures
*        describing the steps fixed in a previous invocation of this program.
*        The array is sorted on exit.
*     newsteps = smfStepFix * (Given and Returned)
*        A pointer to the first element of an array of smfStepFix structures
*        describing the steps fixed in the current invocation of this program.
*        The array is sorted on exit.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     Firstly, an attempt it made to associated each old step with a
*     corresponding new step (i.e. a new step that occurs at the same
*     time and in the same bolometer as the old step). Warning messages
*     are issued about each old step for which no corresponding new step
*     can be found, or for which the corresponding new step has a
*     significantly different height to the old step. Finally, warnings
*     messages are also issued for each new step that has not been
*     associated with an old step.

*  Returned Value:
*     Zero if no significant differences were found. Non-zero otherwise.

*/

/* Local Variables: */
   dim_t inew;
   dim_t jnew;
   double abslim;
   double dsize;
   double dsize_min;
   int *fnew;
   int *new_flags;
   int cont;
   int iold;
   int match;
   int result;
   smfStepFix *pnew;
   smfStepFix *pold;

/* Initialise the returned value. */
   result = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Find the absolute minimum significant difference between step sizes.
   This is "sizetol" times the clipped RMS step size in the new steps. */
   abslim = sizetol*smf1_get_rmssize( nnew, newsteps, status );
   msgSetd( "T", abslim );
   msgOut( "", "Ignoring differences in step size smaller than ^T",
           status );
   msgBlank( status );

/* Allocate memory to hold an array with one element for each new step.
   Each element holds zero if the new step has not yet been associated
   with any old step. Otherwise, it holds the one-based index of the
   associated old step. Initialise it to hold zero at every element. */
   new_flags = astCalloc( nnew, sizeof( *new_flags ) );
   if( *status == SAI__OK ) {

/* Loop round each old step. */
      pold = oldsteps;
      for( iold = 0; iold < nold; iold++,pold++ ) {

/* Ignore old steps with bolometer indices greater than 5000 */
         if( pold->ibolo > 5000 ) continue;

/* Indicate no new step has yet been associated with the old step. */
         jnew = -1;
         dsize_min = VAL__MAXD;
         match = 0;

/* Loop round all new steps. */
         pnew = newsteps;
         fnew = new_flags;
         for( inew = 0; inew < nnew; inew++,pnew++,fnew++ ) {

/* Ignore this new step if it has already been associated with a previous
   old step. */
            if( ! *fnew ) {

/* See if the current new and old steps occur in the same bolometer and
   have overlapping time spans. If so they are considered to be at the
   same time. */
               if( pold->ibolo == pnew->ibolo &&
                   pold->start <= pnew->end &&
                   pold->end >= pnew->start ) {

/* Get the difference in step size between the old and new steps. */
                  dsize = fabs( pold->size - pnew->size );

/* Note the index of the matching new step that is most similar in height
   to the old step. */
                  if( dsize < dsize_min ) {
                     jnew = inew;
                     dsize_min = dsize;
                  }

/* If the old and new step heights are about the same then we associate
   the new step with the old step. Store the (one based) index of the
   corresponding old step. We do not need to check any more new steps,
   so break out of the new step loop. */
                  if( dsize < abslim ||
                      dsize < sizetol*fabs( 0.5*( pold->size + pnew->size ) ) ) {
                     match = 1;
                     *fnew = iold + 1;
                     break;
                  }
               }
            }
         }

/* If a new step was found at the same time and place as the old step, and
   with the same height, pass on to the next old step. */
         if( ! match ) {

/* If no new step was found at the same time and place as the old step, an old
   step has dissappeared. */
            if( jnew == -1 ) {

/* If the old step was of significant height, tell the user. */
               if( fabs( pold->size ) > abslim ){
                  result++;

                  if( result >= first ) {
                     msgSetk( "N", result );
                     msgSetk( "I", pold->id );
                     msgSetc( "W", pold->corr ? "secondary" : "primary" );
                     msgOut( "", "^N: An old ^W step (index ^I) is no longer found:", status );

                     msgSetk( "B", pold->ibolo );
                     msgSetk( "X", pold->ibolo % nx );
                     msgSetk( "Y", pold->ibolo / nx );
                     msgOut( "", "   Bolometer = ^B (^X,^Y)", status );

                     msgSetk( "S", pold->start );
                     msgSetk( "E", pold->end );
                     msgOut( "", "   Time slice range = ^S:^E", status );

                     msgSetd( "H", pold->size );
                     msgOut( "", "   Height = ^H", status );


                     parGet0l( param, &cont, status );
                     parCancl( param, status );
                     if( !cont || *status != SAI__OK ) break;
                     msgBlank( status );
                  }
               }

/* If one or more new step were found at the same time and place as the
   old step (but with a significantly different height), tell the user
   about the change in height. */
            } else {
               pnew = newsteps + jnew;
               result++;

               if( result >= first ) {
                  msgSetk( "I", result );
                  msgSetd( "O", pold->size );
                  msgSetd( "N", pnew->size );
                  msgOut( "", "^I: Step size changed from ^O to ^N:", status );

                  msgSetk( "I", pnew->id );
                  msgSetc( "W", pnew->corr ? "secondary" : "primary" );
                  msgOut( "", "   New index = ^I (^W)", status );

                  msgSetk( "I", pold->id );
                  msgSetc( "W", pold->corr ? "secondary" : "primary" );
                  msgOut( "", "   Old index = ^I (^W)", status );

                  msgSetk( "B", pold->ibolo );
                  msgSetk( "X", pold->ibolo % nx );
                  msgSetk( "Y", pold->ibolo / nx );
                  msgOut( "", "   Bolometer = ^B (^X,^Y)", status );

                  msgSetk( "S", pold->start );
                  msgSetk( "E", pold->end );
                  msgOut( "", "   Old time slice range = ^S:^E", status );

                  msgSetk( "S", pnew->start );
                  msgSetk( "E", pnew->end );
                  msgOut( "", "   New time slice range = ^S:^E", status );

                  parGet0l( param, &cont, status );
                  parCancl( param, status );
                  if( !cont || *status != SAI__OK ) break;
                  msgBlank( status );
               }
            }
         }
      }

/* We have now checked all old steps for matching new steps. If no
   significant change has yet been found, look for new steps that
   have not been associated with an old step. */
      pnew = newsteps;
      fnew = new_flags;
      for( inew = 0; inew < nnew && cont; inew++,pnew++,fnew++ ) {
         if( ! *fnew ) {

/* If the new step is off significant height, tell the user. */
            if( fabs( pnew->size ) > abslim ){
               result++;

               if( result >= first ) {
                  msgSetk( "N", result );
                  msgSetk( "I", pnew->id );
                  msgSetc( "W", pnew->corr ? "secondary" : "primary" );
                  msgOut( "", "^N: A new ^W step (index ^I) was found:", status );

                  msgSetk( "B", pnew->ibolo );
                  msgSetk( "X", pnew->ibolo % nx );
                  msgSetk( "Y", pnew->ibolo / nx );
                  msgOut( "", "   Bolometer = ^B (^X,^Y)", status );

                  msgSetk( "S", pnew->start );
                  msgSetk( "E", pnew->end );
                  msgOut( "", "   Time slice range = ^S:^E", status );

                  msgSetd( "H", pnew->size );
                  msgOut( "", "   Height = ^H", status );

                  parGet0l( param, &cont, status );
                  parCancl( param, status );
                  if( !cont || *status != SAI__OK ) break;
                  msgBlank( status );
               }
            }
         }
      }
   }

/* Free resources. */
   new_flags = astFree( new_flags );

/* Return the result. */
   return result;
}


static void smf1_write_steps( FILE *fd, smfData *data, dim_t nstep,
                              smfStepFix *steps, double dcthresh,
                              int dcsmooth, int dcfitbox, int dcmaxsteps,
                              int dclimcorr, dim_t nrej, int *status ) {
/*
*  Name:
*     smf1_write_steps

*  Purpose:
*     Write an array of step descriptions to a disk file.

*  Invocation:
*     void smf1_write_steps( FILE *fd, smfData *data, dim_t nstep,
*                            smfStepFix *steps, double dcthresh,
*                            dim_t dcsmooth, dim_t dcfitbox, int dcmaxsteps,
*                            int dclimcorr, dim_t nrej, int *status )

*  Arguments:
*     fd = FILE * (Given)
*        A file descriptor to which the step descriptions should be
*        written.
*     data = smfData * (Given)
*        The smfData containing the data that was fixed.
*     nstep = dim_t (Given)
*        The number of steps to write.
*     steps = smfStepFix * (Given)
*        A pointer to the first element of an array of smfStepFix structures
*        describing the steps to be written. The array should have
*        "nstep" elements.
*     dcthresh = double (Given)
*        Value of DCTHRESH used to create the step fixes.
*     dcsmooth = dim_t (Given)
*        Value of DCSMOOTH used to create the step fixes.
*     dcfitbox = dim_t (Given)
*        Value of DCFITBOX used to create the step fixes.
*     dcmaxsteps = int (Given)
*        Value of DCMAXSTEPS used to create the step fixes.
*     dclimcorr = int (Given)
*        Value of DCLIMCORR used to create the step fixes.
*     nrej = dim_t (Given)
*        The number of bolometers rejected.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Write the supplied information to a disk file in a form that can be
*     read back into this program using the OLDSTEPS parameter. The
*     format used also conforms to the TOPCAT "ascii" format, and so
*     TOPCAT can be used to read the file as a catalogue.

*/

/* Local Variables: */
   dim_t istep;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Write out the global info as topcat comment lines. */
   fprintf( fd, "# Steps fixed in '%s'\n", data->file->name );
   fprintf( fd, "# Number of steps fixed = %d\n", (int) nstep );
   fprintf( fd, "# Number of bolometers rejected = %d\n", (int) nrej );
   fprintf( fd, "#\n" );
   fprintf( fd, "# DCFITBOX = %d\n", (int) dcfitbox );
   fprintf( fd, "# DCMAXSTEPS = %d\n", dcmaxsteps );
   fprintf( fd, "# DCLIMCORR = %d\n", dclimcorr );
   fprintf( fd, "# DCSMOOTH = %d\n", (int) dcsmooth );
   fprintf( fd, "# DCTHRESH = %g\n", dcthresh );
   fprintf( fd, "#\n" );

/* Write out the topcat column header line. */
   fprintf( fd, "# istep start end ibolo size corr\n" );

/* Write out the details of each step. */
   for( istep = 0; istep < nstep; istep++ ) {
      fprintf( fd, "%d %d %d %d %.*g %d\n",
                                    (int)( steps[ istep ] ).id,
                                    (int)( steps[ istep ] ).start,
                                    (int)( steps[ istep ] ).end,
                                    (int)( steps[ istep ] ).ibolo,
                           DBL_DIG, ( steps[ istep ] ).size,
                                    ( steps[ istep ] ).corr );
   }
}

static double smf1_get_rmssize( dim_t nstep, smfStepFix *steps, int *status ){
/*
*  Name:
*     smf1_get_rmssize

*  Purpose:
*     Get the clipped RMS step size.

*  Invocation:
*     double smf1_get_rmssize( dim_t nstep, smfStepFix *steps, int *status )

*  Arguments:
*     nstep = dim_t (Given)
*        Length of the "steps" array.
*     steps = smfStepFix * (Given)
*        Pointer to an array of step fix structures.
*     status = int * (given and Returned)
*        Ingerited status

*  Description:
*     This function obtains the RMS step size, iterating threee times to
*     remove steps that are larger than 3 times teh RMS step size from the
*     previous iteration.

* Returned value:
*     The clipped RMS step size.

*/

/* Local Variables: */
   double rms;
   double size;
   double sum2;
   double threshold;
   dim_t istep;
   int iter;
   int nsum;
   smfStepFix *step;

/* Initialise */
   rms = 0.0;
   threshold = VAL__MAXD;

/* Check inherited status */
   if( *status != SAI__OK ) return rms;

/* Find the RMS step size, excluding steps larger than 3 times the
   RMS of the step sizes on the previous iteration. Perform 3 clipping
   iterations. */
   for( iter = 0; iter < 3; iter++ ) {

/* Initialise running sums. */
      sum2 = 0.0;
      nsum = 0;

/* Loop round all steps. */
      step = steps;
      for( istep = 0; istep < nstep; istep++,step++ ) {
         size = step->size;

/* Check that this step size is no more than 3 times the RMS from
   the previous iteration. Only use steps that were not detected
   as correlated steps (since there will be lots of these with very small
   step sizes). */
         if( fabs( size ) < threshold && !(step->corr) ) {

/* If so, increment the running sums. */
            sum2 += size*size;
            nsum++;
         }
      }

/* Report an error and abort if all steps were rejected. */
      if( nsum == 0 ) {
         *status = SAI__ERROR;
         errRep( "", "All steps rejected when calculating RMS step size.",
                 status );
         break;
      }

/* Calculate the RMS of the remaining step sizes. */
      rms = sum2/nsum ;
      if( rms > 0.0 ) {
         rms = sqrt( rms );
      } else {
         rms = 0.0;
      }

/* Set the threshold for the next iteration. */
      threshold = 3.0*rms;
   }

/* Return the result */
   return rms;
}
