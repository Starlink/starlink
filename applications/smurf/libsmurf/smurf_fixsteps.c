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
*     CHANGED = _LOGICAL (Write)
*        An output parameter to which is written a flag indicating if any
*        significant differences were found between the step fixes
*        produced by the current invocation of this program, and the step
*        fixes described in the file specified via parameter OLDSTEPS.
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
*        It can also be viewed using "topcat -f ascii". If a null (!)
*        value is supplied for NEWSTEPS, no file is created. [!]
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
#include <ctype.h>
#include <float.h>

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
static int smf1_check_steps( FILE *fd, double dcthresh, dim_t dcmedianwidth,
                             dim_t dcfitbox, int dcmaxsteps, int dclimcorr,
                             size_t nrej, smfStepFix *steps, int nstep,
                             int *status );


/* Main entry */
void smurf_fixsteps( int *status ) {

/* Local Variables */
   AstKeyMap *keymap;        /* Default config parameter values */
   AstKeyMap *sub_instruments; /* Info about sub-instruments */
   FILE *fd = NULL;          /* File descriptor */
   Grp *igrp = NULL;         /* Input group of files */
   Grp *ogrp = NULL;         /* Output group of files */
   dim_t dcfitbox;           /* DCFITBOX config parameter */
   dim_t dcmedianwidth;      /* DCMEDIANWIDTH config parameter */
   double dcthresh;          /* DCTHRESH config parameter */
   int changed;              /* Have any step fixes changed? */
   int dclimcorr;            /* DCLIMCORR config parameter */
   int dcmaxsteps;           /* DCMAXSTEPS config parameter */
   int istep;                /* Index of current step */
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

   if( nrej == 0 ) {
      msgOut( "", "No bolometers were rejected", status );
   } else if( nrej == 1 ) {
      msgOut( "", "One bolometer was rejected", status );
   } else {
      msgSeti( "NREJ", nrej );
      msgOut( "", "^NREJ bolometers were rejected", status );
   }

   if( nsteps == 0 ) {
      msgOut( "", "No steps were fixed", status );
   } else if( nsteps == 1 ) {
      msgOut( "", "One step was fixed", status );
   } else {
      msgSeti( "NSTEPS", nsteps );
      msgOut( "", "^NSTEPS steps were fixed", status );
   }

/* If required, write out to a text file details of the steps that were
   fixed. */
   fd = smf_open_textfile( "NEWSTEPS", "w", "<none>", status );
   if( fd ) {
      fprintf( fd, "# Steps fixed in '%s'\n", indata->file->name );
      fprintf( fd, "# Number of steps fixed = %d\n", (int) nsteps );
      fprintf( fd, "# Number of bolometers rejected = %d\n", (int) nrej );
      fprintf( fd, "#\n" );
      fprintf( fd, "# DCFITBOX = %d\n", (int) dcfitbox );
      fprintf( fd, "# DCLIMCORR = %d\n", dclimcorr );
      fprintf( fd, "# DCMAXSTEPS = %d\n", dcmaxsteps );
      fprintf( fd, "# DCMEDIANWIDTH = %d\n", (int) dcmedianwidth );
      fprintf( fd, "# DCTHRESH = %g\n", dcthresh );
      fprintf( fd, "#\n" );
      fprintf( fd, "# istep start end ibolo size\n" );

      for( istep = 0; istep < nsteps; istep++ ) {
         fprintf( fd, "%d %d %d %d %.*g\n", istep,
                                       ( newsteps[ istep ] ).start,
                                       ( newsteps[ istep ] ).end,
                                       ( newsteps[ istep ] ).ibolo,
                              DBL_DIG, ( newsteps[ istep ] ).size );
      }
      fclose( fd );
   }


/* If required, compare the steps fixed by this invocation with a previous
   set of step fixes. */
   fd = smf_open_textfile( "OLDSTEPS", "r", "<none>", status );
   if( fd ) {
      msgBlank( status );
      changed = smf1_check_steps( fd, dcthresh, dcmedianwidth,
                                  dcfitbox, dcmaxsteps, dclimcorr,
                                  nrej, newsteps, nsteps, status );
      parPut0l( "CHANGED", changed, status );
      if( ! changed ) {
         msgOut( "", "There are no significant differences "
                 "between old and new step fixes.", status );
      }
      msgBlank( status );
      fclose( fd );
   }

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


/* Max size of one line of text read from the supplied file. */
#define BUFSIZE 255

static int smf1_check_steps( FILE *fd, double dcthresh, dim_t dcmedianwidth,
                             dim_t dcfitbox, int dcmaxsteps, int dclimcorr,
                             size_t nrej, smfStepFix *steps, int nstep,
                             int *status ) {
/*
*  Name:
*     smf1_check_steps

*  Purpose:
*     Compare new steps with old steps read from a text file.

*  Invocation:
*     int smf1_check_steps( FILE *fd, double dcthresh, dim_t dcmedianwidth,
*                           dim_t dcfitbox, int dcmaxsteps, int dclimcorr,
*                           size_t nrej, smfStepFix *steps, int nstep,
*                           int *status )

*  Arguments:
*     fd = FILE * (Given)
*        A file descriptor from which to read the details of the steps
*        fixed on a previous run of this program. The associated text
*        file should have been created using the NEWSTEPS parameter.
*     dcthresh = double (Given)
*        Value of DCTHRESH used in the current invocation of this program.
*     dcmedianwidth = dim_t (Given)
*        Value of DCMEDIANWIDTH used in the current invocation of this program.
*     dcfitbox = dim_t (Given)
*        Value of DCFITBOX used in the current invocation of this program.
*     dcmaxsteps = int (Given)
*        Value of DCMAXSTEPS used in the current invocation of this program.
*     dclimcorr = int (Given)
*        Value of DCLIMCORR used in the current invocation of this program.
*     nrej = size_t (Given)
*        The number of bolometers rejected in the current invocation of
*        this program.
*     steps = smfStepFix * (Given)
*        A pointer to the first element of an array of smfStepFix structures
*        describing the steps fixed in the current invocation of this program.
*     nstep = int (Given)
*        The number of fixed steps in the current invocation of this program.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reads information from the supplied file and reports any
*     significant differences with the information produced by the curent
*     invocation of this program.

*  Returned Value:
*     Zero if no significant differences were found. Non-zero otherwise.

*/

/* Local Variables: */
   char buf[ BUFSIZE + 1 ];
   char *c;
   double dval;
   double size;
   int bad;
   int end;
   int ibolo;
   int iline;
   int istep;
   int ival;
   int nc;
   int nold;
   int report;
   int result;
   int stage;
   int start;

/* Check the inherited status. */
   if( *status != SAI__OK ) return 0;

/* Indicate that no sognificant differences have been found yet. */
   result = 0;

/* Indicate we have not yet reached the tabular data. */
   stage = 0;

/* Initialise the index of the next step to compare. */
   istep = 0;

/* Indicate we do not yet know how many steps are described in the text
   file. */
   nold = -1;

/* Indicate no bad lines found yet. */
   bad = 0;

/* Indicate we have not yet reported a changed step. */
   report = 1;

/* Loop round reading lines of text from the supplied file until an
   illegal line is read or the end of file is reached. */
   iline = 0;
   while( !bad && fgets( buf, BUFSIZE, fd ) ) {
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
            if( ival != nstep ) {
               msgSeti( "O", ival );
               msgSeti( "N", nstep );
               msgOut( "", "No. of steps fixed changed from ^O to ^N",
                       status );
               result = 1;
            }

            nold = ival;

         } else if( sscanf( buf, "# Number of bolometers rejected = %d%n",
                            &ival, &nc ) && nc > 34 ) {
            if( ival != (int) nrej ) {
               msgSeti( "O", ival );
               msgSeti( "N", nrej );
               msgOut( "", "No. of bolometers rejected changed from ^O to ^N",
                       status );
               result = 1;
            }

         } else if( sscanf( buf, "# DCFITBOX = %d%n", &ival, &nc ) && nc > 13 ) {
            if( ival != (int) dcfitbox ) {
               msgSeti( "O", ival );
               msgSeti( "N", dcfitbox );
               msgOut( "", "Warning: DCFITBOX changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCLIMCORR = %d%n", &ival, &nc ) && nc > 14 ) {
            if( ival != dclimcorr ) {
               msgSeti( "O", ival );
               msgSeti( "N", dclimcorr );
               msgOut( "", "Warning: DCLIMCORR changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCMAXSTEPS = %d%n", &ival, &nc ) && nc > 14 ) {
            if( ival != dcmaxsteps ) {
               msgSeti( "O", ival );
               msgSeti( "N", dcmaxsteps );
               msgOut( "", "Warning: DCMAXSTEPS changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCMEDIANWIDTH = %d%n", &ival, &nc )
                    && nc > 18 ) {
            if( ival != (int) dcmedianwidth ) {
               msgSeti( "O", ival );
               msgSeti( "N", dcmedianwidth );
               msgOut( "", "Warning: DCMEDIANWIDTH changed from ^O to ^N", status );
            }

         } else if( sscanf( buf, "# DCTHRESH = %lg%n", &dval, &nc ) && nc > 13 ) {
            if( fabs( dval - dcthresh ) > 1.0E-10 ) {
               msgSetd( "O", dval );
               msgSetd( "N", dcthresh );
               msgOut( "", "Warning: DCTHRESH changed from ^O to ^N", status );
            }

/* Look for the line that marks the start of the tabular data. */
         } else if( !strcmp( buf, "# istep start end ibolo size" ) ) {
            stage = 1;

/* Abort if an illegal header line is read. */
         } else if( strcmp( buf, "#" ) &&
                    strncmp( buf, "# Steps fixed in '", 18 ) ) {
            bad = 1;
         }

/* If we are now reading tabular data, compare values, and report any
   significant differences. */
      } else {

         if( sscanf( buf, "%d %d %d %d %lg%n", &ival, &start, &end, &ibolo, &size, &nc )
             == 5 && nc > 14 ) {

            if( ival != istep ) {
               *status = SAI__ERROR;
               msgSeti( "I", istep );
               errRep( "", "Step ^I data not found in old steps file:", status );
               bad = 1;

            } else if( istep >= nstep ) {
               /* Just ignore old steps that are not found any more. */

            } else if( !report ) {
               /* Do nothing if we have already reported a changed step. */

            } else if( start > steps[ istep ].end ||  /* Check no overlap */
                       end < steps[ istep ].start ) {
               msgSeti( "I", istep );
               msgSeti( "OS", start );
               msgSeti( "NS", steps[ istep ].start );
               msgSeti( "OE", end );
               msgSeti( "NE", steps[ istep ].end );
               msgOut( "", "start,end: old=(^OS,^OE) new=(^NS,^NE) (step ^I)", status );
               result = 1;
               report = 0;

            } else if( ibolo != steps[ istep ].ibolo ) {
               msgSeti( "I", istep );
               msgSeti( "O", ibolo );
               msgSeti( "N", steps[ istep ].ibolo );
               msgOut( "", "ibolo: old=^O new=^N (step ^I)", status );
               result = 1;
               report = 0;

            } else if( fabs( size - steps[ istep ].size ) >
                       0.01*fabs( size + steps[ istep ].size ) ) {
               msgSeti( "I", istep );
               msgSetd( "O", size );
               msgSetd( "N", steps[ istep ].size );
               msgOut( "", "size: old=^O new=^N (step ^I)", status );
               result = 1;
               report = 0;
            }

/* Increment the index of the next step to check. */
            istep++;

/* Abort if an illegal line is read. */
         } else {
            bad = 1;
         }
      }
   }

/* Report an error if the last line read was illegal. */
   if( bad ) {
      *status = SAI__ERROR;
      msgSeti( "I", iline );
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
      msgSeti( "I", istep );
      msgSeti( "N", nold );
      errRep( "", "Header says file contains ^N steps but data for ^I "
              "steps was found.", status );
   }

   return result;
}



