/*
*+
*  Name:
*     GSD2ACSIS

*  Purpose:
*     Convert a GSD format DAS data file to an ACSIS format NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_gsd2acsis( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Opens a GSD file for reading, and checks the version (currently
*     only supports GSD version 5.3). The data are converted to ACSIS
*     format and written to disk.

*  ADAM Parameters:
*     DIRECTORY = _CHAR (Read)
*          Directory for output ACSIS files. A NULL value will use the
*          current working directory. This command will create a subdir
*          in this directory named after the observation number.
*     IN = CHAR (Read)
*          Name of the input GSD file to be converted.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OBSNUM = INT (Read)
*          Observation number for files prior to Feb 03. For newer
*          observations this parameter is not required. Default value
*          will be the observation number read from the file but prior
*          to Feb 03 this number was the number within the project
*          rather than the number from the night and may lead to name
*          clashes since ACSIS data are numbered for a UT date.

*  Related Applications:
*     SMURF: MAKECUBE, GSDSHOW;
*     CONVERT: SPECX2NDF;
*     SPECX;
*     GSDPRINT;
*     JCMTDR

*  Notes:
*     Whilst this command does a reasonable job of converting common
*     data to ACSIS format it still has to undergo extensive testing
*     to ensure that it is always doing the correct thing. Testing of
*     this command and comparing its results with SPECX maps will be
*     welcomed.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     28-JAN-2008 (JB):
*        Original version.
*     14-FEB-2008 (JB):
*        Use gsdVars struct to store headers/arrays
*     19-FEB-2008 (JB):
*        Check dasFlag
*     22-FEB-2008 (JB):
*        Include math.h
*     04-MAR-2008 (JB):
*        Check number of scans actually completed.
*     19-MAR-2008 (JB):
*        Include par.h.
*     02-APR-2008 (JB):
*        Check that this is DAS/AOSC version 5.3
*     04-APR-2008 (JB):
*        Wrap gsd calls in macro for error checking.
*     04-APR-2008 (JB):
*        Alphabetize local variables.
*     21-APR-2008 (JB):
*        Add ADAM parameter documentation.
*     22-APR-2008 (JB):
*        Use gsdac_flagBad.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
#include <math.h>

/* STARLINK includes */
#include "mers.h"
#include "gsd.h"
#include "sae_par.h"
#include "par.h"
#include "star/one.h"

/* SMURF includes */
#include "smurflib.h"
#include "libgsd/gsdac_struct.h"
#include "libgsd/gsdac.h"
#include "jcmt/state.h"

#define FUNC_NAME "smurf_gsd2acsis"
#define TASK_NAME "GSD2ACSIS"

#define MAXNAME 80
#define NRECEP 2

void smurf_gsd2acsis( int *status ) {

  /* Local variables */
  dasFlag dasFlag;            /* file structure type */
  char directory[MAXNAME];    /* directory to write the file */
  char filename[MAXNAME];     /* name of the GSD file */
  FILE *fptr;                 /* pointer to GSD file */
  struct gsdac_gsd_struct gsd; /* GSD file access parameters */
  gsdVars gsdVars;            /* GSD headers and arrays */
  char label[41];             /* GSD label */
  int nitem;                  /* number of items in GSD file */
  unsigned int nSteps;        /* number of time steps */
  float version;              /* GSD file version */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the user defined input and output file names */
  parGet0c ( "IN", filename, MAXNAME, status );

  if ( *status != SAI__OK ) return;

  parGet0c ( "DIRECTORY", directory, MAXNAME, status );

  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    one_strlcpy ( directory, ".", sizeof(directory), status );
  }

  msgOutif(MSG__VERB," ",
	     "Opening GSD file for reading", status);

  /* Open the GSD file. */
  CALLGSD( gsdOpenRead ( filename, &version, label, &nitem,
                            &fptr, &(gsd.fileDsc), &(gsd.itemDsc),
			   &(gsd.dataPtr) ),
           status,
           errRep ( FUNC_NAME, "gsdOpenRead : Could not find input GSD file.", status ); );

  if ( *status != SAI__OK ) return;

  msgOutif(MSG__VERB," ",
	     "Checking backend name", status);

  /* Check to see if this is DAS or AOSC data. */
  gsdac_get0c ( &gsd, "C1BKE", gsdVars.backend, status );

  if ( *status != SAI__OK ) return;

  if ( strncmp ( gsdVars.backend, "DAS", 3 ) != 0
       && strncmp ( gsdVars.backend, "AOSC", 4 ) != 0 ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "File does not contain DAS or AOSC data", status );
    return;
  }

  msgOutif(MSG__VERB," ",
	     "Checking version of GSD file", status);

  /* Check the version of the opened file. */
  if ( fabs(version - 5.300 ) > 0.0001 ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "GSD version is not 5.300.", status );
    return;
  }

  /* Get the file structure flag. */
  gsdac_getDASFlag ( &gsd, &dasFlag, status );

  if ( *status != SAI__OK ) return;

  if ( dasFlag == DAS_NONE )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_NONE", status);
  else if ( dasFlag == DAS_TP )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_TP", status);
  else if ( dasFlag == DAS_CONT_CAL )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_CONT_CAL", status);
  else if ( dasFlag == DAS_CROSS_CORR )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_CROSS_CORR", status);
  else {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Could not identify DAS file type.", status );
    return;
  }

  /* Get the GSD file headers and data arrays. */
  gsdac_getGSDVars ( &gsd, dasFlag, &gsdVars, status );

  /* Flag the bad values with STARLINK VAL__BAD. */
  gsdac_flagBad ( dasFlag, &gsdVars, status );

  if ( *status != SAI__OK ) {
    errRep ( FUNC_NAME, "Couldn't get GSD headers and arrays.", status );
    return;
  }

  /* Close the GSD file. */
  msgOutif(MSG__VERB," ",
	     "Closing GSD file", status);

  /* Close the GSD file. */
  CALLGSD( gsdClose ( fptr, gsd.fileDsc, gsd.itemDsc, gsd.dataPtr ),
           status,
           errRep ( FUNC_NAME, "gsdClose : Error closing GSD file.", status ); );


  /* Get the number of time steps in the observation. */
  nSteps = gsdVars.nScan * gsdVars.nScanPts;
  if ( nSteps <= 0 ) {
    *status = SAI__WARN;
    errRep ( FUNC_NAME, "Nr. steps 0: observation terminated without data", status );
  }

  if ( *status != SAI__OK ) return;

  /* Convert and write out the new file. */
  gsdac_wrtData ( &gsdVars, directory, nSteps, dasFlag, status );

  gsdac_freeArrays ( dasFlag, &gsdVars, status );

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ",
	     "Conversion completed successfully", status);
  }

}
