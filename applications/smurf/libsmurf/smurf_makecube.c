/*
*+
*  Name:
*     smurf_makecube

*  Purpose:
*     Top-level MAKECUBE implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makecube( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the MAKECUBE task.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     PIXSIZE = REAL (Read)
*          Pixel size in output image, in arcsec
*     OUT = NDF (Write)
*          Output file
*     SYSTEM = LITERAL (Read)
*          The celestial coordinate system for the output cube. One of
*          ICRS, FK5, AZEL, GALACTIC or TRACKING.
*     USERECPOS = _LOGICAL (Read)
*          If a true value is supplied, then the receptor positions are
*          read from the .MORE.ACSIS.RECEPPOS arrays in each input NDF.
*          Otherwise, the receptor positions are calculated on the basis
*          of the .MORE.ACSIS.FPLANEX/Y arrays. Both methods should (in
*          (the absence of bugs) result in identical cubes. [TRUE]

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2006 (TIMJ):
*        Clone from smurf_makemap
*     18-SEP-2006 (DSB):
*        MAKECUBE code added.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council and the University of British Columbia. All Rights
*     Reserved.

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

#define FUNC_NAME "smurf_makecube"
#define TASK_NAME "MAKECUBE"
#define LEN__METHOD 20

void smurf_makecube( int *status ) {

/* Local Variables */
   AstFrame *oskyfrm = NULL;   /* SkyFrame from the output WCS Frameset */
   AstFrame *ospecfrm = NULL;  /* SpecFrame from the output WCS Frameset */
   AstFrame *tfrm = NULL;      /* Current Frame from output WCS */
   AstFrameSet *swcsout = NULL;/* Spatial WCS FrameSet for output cube */
   AstFrameSet *wcsout = NULL; /* WCS Frameset for output cube */
   AstMapping *oskymap = NULL; /* GRID->SkyFrame Mapping from output WCS */
   AstMapping *ospecmap = NULL;/* GRID->SpecFrame Mapping from output WCS */
   AstMapping *tmap = NULL;   /* Base->current Mapping from output WCS */
   Grp *igrp = NULL;          /* Group of input files */
   Grp *ogrp = NULL;          /* Group containing output file */
   HDSLoc *weightsloc = NULL; /* HDS locator of weights array */
   char *pname = NULL;        /* Name of currently opened data file */
   char system[ 10 ];         /* Celestial coord system for output cube */
   float pixsize;             /* Spatial size of an output pixel in arcsec */
   int axes[ 2 ];             /* Indices of selected axes */
   int flag;                  /* Is group expression to be continued? */
   int ifile;                 /* Input file index */
   int lbnd_out[ 3 ];         /* Lower pixel bounds for output map */
   int moving;                /* Is the telescope base position changing? */
   int ondf;                  /* output NDF identifier */
   int outax[ 2 ];            /* Indices of corresponding output axes */
   int outsize;               /* Number of files in output group */
   int size;                  /* Number of files in input group */
   int smfflags;              /* Flags for smfData */
   int ubnd_out[ 3 ];         /* Upper pixel bounds for output map */
   int userecpos;             /* Should the RECEPPOS array be used? */
   smfData *data = NULL;      /* Pointer to data struct */
   smfData *odata = NULL;     /* Pointer to output SCUBA2 data struct */
   smfData *wdata = NULL;     /* Pointer to SCUBA2 data struct */
   smfFile *file = NULL;      /* Pointer to data file struct */
   void *data_array = NULL;   /* Pointer to the rebinned map data */
   void *var_array = NULL;    /* Pointer to the variance map */
   void *wgt_array = NULL;    /* Pointer to the weights map */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an NDF context (we do not begin an AST context since this is
   done within the calling monolith routine). */
   ndfBegin();
  
/* Get a group of input files */
   ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

/* Get the user defined spatial pixel size (the calibration for the spectral 
   axis is fixed by the first input data file - see smf_cubebounds.c). */
   parGet0r( "PIXSIZE", &pixsize, status );
   if( *status == SAI__OK && pixsize <= 0 ) {
       msgSetr( "PIXSIZE", pixsize );
       *status = SAI__ERROR;
       errRep( FUNC_NAME, "Pixel size ^PIXSIZE is < 0.", status);
   }

/* Get the celestial coordinate system for the output cube. */
   parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC",
              1, system, 10, status );

/* See of the receptor positions are to be read from the .MORE.ACSIS.RECEPPOS
   array. Otherwise, they are calculated on the basis of the
   .MORE.ACSIS.FPLANEX/Y arrays. */
   parGet0l( "USERECEP", &userecpos, status );
  
/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. */
   smf_cubebounds( igrp, size, system, 0.0, 0.0, 1, pixsize, userecpos,
                   &moving, lbnd_out, ubnd_out, &wcsout, status );

/* Get the base->current Mapping from the output WCS FrameSet, and split it 
   into two Mappings; one (oskymap) that maps the first 2 GRID axes into 
   celestial sky coordinates, and one (ospecmap) that maps the third GRID
   axis into a spectral coordinate. Also extract the SpecFrame and
   SkyFrame from the current Frame. */
   tmap = astGetMapping( wcsout, AST__BASE, AST__CURRENT );
   tfrm = astGetFrame( wcsout, AST__CURRENT );

   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   astMapSplit( tmap, 2, axes, outax, &oskymap );
   oskyfrm = astPickAxes( tfrm, 2, outax, NULL );

   axes[ 0 ] = 3;
   astMapSplit( tmap, 1, axes, outax, &ospecmap );
   ospecfrm = astPickAxes( tfrm, 1, outax, NULL );

/* Invert the spectral Mapping (for the convenience of smf_rebincube), so that
   it goes go from current Frame to output grid axis. */
   astInvert( ospecmap );

/* Create a FrameSet describing the spatial axes, and invert it. */
   swcsout = astFrameSet( astFrame( 2, "Domain=GRID", "" ), "" );
   astAddFrame( swcsout, AST__BASE, oskymap, oskyfrm );
   astInvert( swcsout );

/* Create the output NDF. */
   ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );
   smfflags = 0;
   smfflags |= SMF__MAP_VAR;
   smf_open_newfile( ogrp, 1, SMF__FLOAT, 3, lbnd_out, ubnd_out, smfflags, 
                     &odata, status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Save some useful pointers. */
   file = odata->file;
   ondf = file->ndfid;

/* Get pointers to the mapped output data, variance, and weights arrays. */
   data_array = (odata->pntr)[ 0 ];
   var_array = (odata->pntr)[ 1 ];
   weightsloc = smf_get_xloc ( odata, "ACSISRED", "WT_ARR", "WRITE", 
                               0, 0, status );
   smf_open_ndfname ( weightsloc, "WRITE", NULL, "WEIGHTS", "NEW", "_DOUBLE",
                      3, (int *) lbnd_out, (int *) ubnd_out, &wdata, status );
   if( wdata ) wgt_array = (wdata->pntr)[ 0 ];

/* Loop round all the input files, pasting each one into the output NDF. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( igrp, ifile, "READ", 1, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         errRep( FUNC_NAME, "Could not open input data file.", status );
         break;

      } else {
         if( data->file == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfFile associated with smfData.", 
                    status );
            break;

         } else if( data->hdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfHead associated with smfData.", 
                    status );
            break;

         } 
      }

/* Report the name of the input file. */
      pname =  data->file->name;
      msgSetc( "FILE", pname );
      msgSeti( "THISFILE", ifile );
      msgSeti( "NUMFILES", size );
      msgOutif( MSG__VERB, " ", 
                "SMURF_MAKECUBE: Processing ^THISFILE/^NUMFILES ^FILE",
                status );

/* Check that the input data type is single precision. */
      if( data->dtype != SMF__FLOAT ) {
         if( *status == SAI__OK ) {
            msgSetc( "FILE", pname );
            msgSetc( "DTYPE", smf_dtype_string( data, status ) );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "^FILE has ^DTYPE data type, should "
                    "be REAL.",  status );
         }
         break;
      }     

/* If the receptor positions are to calculated on the basis of FPLANEX/Y
   rather than RECEPPOS, then free the receppos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !userecpos && data->hdr->receppos ) {
         smf_free( (double *) data->hdr->receppos, status );      
         data->hdr->receppos = NULL;
      }

/* Rebin the data into the output grid. */
      smf_rebincube( data, ifile, size, swcsout, ospecfrm, ospecmap, moving,
                     lbnd_out, ubnd_out, data_array, var_array, wgt_array, 
                     status );
   
/* Close the input data file. */
      if( data != NULL ) {
	smf_close_file( &data, status );
	data = NULL;
      }
   }

L999:;

/* Close the input data file that remains open due to an early exit from
   the above loop. */
   if( data != NULL ) {
      smf_close_file( &data, status );
      data = NULL;
   }

/* Store the WCS FrameSet in the output NDF. */
   if( wcsout ) ndfPtwcs( wcsout, ondf, status );
  
/* Close the output data files. */
   if( wdata ) smf_close_file( &wdata, status );
   if( odata ) smf_close_file( &odata, status );

/* Free resources. */  
   if( igrp != NULL) grpDelet( &igrp, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/  
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, cube written.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
