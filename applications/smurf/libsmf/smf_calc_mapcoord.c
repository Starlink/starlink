/*
*+
*  Name:
*     smf_calc_mapcoord

*  Purpose:
*     Generate a pointing LUT, and write it to a MAPCOORD extension if
*     data associated with a file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_calc_mapcoord( smfData *data, AstFrameSet *outfset, int moving, 
*                        int *lbnd_out, int *ubnd_out, int flags, 
*                        int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*     flags = int (Given)
*        If set to SMF__NOCREATE_FILE don't attempt to write extension
*        to file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a MAPCOORD extension in the NDF associated
*     with data based on outfset & lbnd/ubnd_out. If a MAPCOORD
*     extension already exists and it uses the same mapping defined by
*     outfset/lbnd_out/ubnd_out it will not get re-calculated. If the routine
*     exits with clean status the smfData will have the LUT mapped and
*     the NDF ID of the table stored in the associated smfFile.
*
*     
*  Authors:
*     Edward Chapin (UBC)

*  History:
*     2006-05-16 (EC):
*        Initial version
*     2006-06-25 (EC):
*        Changed function name from smf_mapcoord to smf_calc_mapcoord
*     2006-07-10 (EC):
*        Only re-calculate LUT when necessary
*     2006-08-15 (EC):
*        Fixed off-by-one errors in *bnd_in
*     2007-07-12 (EC):
*        -adding moving to interface
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2007-11-26 (EC):
*        -Don't annul the NDF corresponding to the mapped LUT to leave it open
*        -If LUT doesn't need re-calculation, open with smf_open_mapcoord.
*        -Store the NDF ID of the LUT in the associated smfFile
*     2007-12-14 (EC):
*        -Added flags so that MAPCOORD extensions isn't always generated
*        -Assert ICD data order
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-25 (EC):
*        -explicitly close MAPCOORD extension before calculating new one
*     2008-04-21 (EC):
*        -Applied Andy's fix for off-by-one errors in nearest-neighbour calc

*  Notes:
*     This routines asserts ICD data order.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calc_mapcoord"

void smf_calc_mapcoord( smfData *data, AstFrameSet *outfset, int moving, 
			int *lbnd_out, int *ubnd_out, int flags, 
			int *status ) {

  /* Local Variables */

  AstSkyFrame *abskyfrm = NULL;/* Output SkyFrame (always absolute) */
  AstMapping *bolo2map=NULL;   /* Combined mapping bolo->map coordinates */
  int bndndf=NDF__NOID;        /* NDF identifier for map bounds */
  void *data_pntr[1];          /* Array of pointers to mapped arrays in ndf */
  int *data_index;             /* Mapped DATA_ARRAY part of NDF */
  int docalc=1;                /* If set calculate the LUT */
  int doextension=0;           /* Try to write LUT to MAPCOORD extension */
  smfFile *file=NULL;          /* smfFile pointer */
  AstObject *fstemp = NULL;    /* AstObject version of outfset */
  dim_t i;                     /* loop counter */
  dim_t j;                     /* loop counter */
  int lbnd[1];                 /* Pixel bounds for 1d pointing array */
  int lbnd_in[2];              /* Pixel bounds for asttrangrid */
  int lbnd_old[2];             /* Pixel bounds for existing LUT */
  int lbnd_temp[1];            /* Bounds for bounds NDF component */
  int lutndf=NDF__NOID;        /* NDF identifier for coordinates */
  AstMapping *map2sky_old=NULL;/* Existing mapping map->celestial coord. */
  HDSLoc *mapcoordloc=NULL;    /* HDS locator to the MAPCOORD extension */
  AstFrameSet *oldfset=NULL;   /* Pointer to existing WCS info */
  AstSkyFrame *oskyfrm = NULL; /* SkyFrame from the output WCS Frameset */
  int ubnd[1];                 /* Pixel bounds for 1d pointing array */
  int ubnd_in[2];              /* Pixel bounds for asttrangrid */
  int ubnd_old[2];             /* Pixel bounds for existing LUT */
  int ubnd_temp[1];            /* Bounds for bounds NDF component */
  int *lut = NULL;             /* The lookup table */
  dim_t nbolo=0;               /* Number of bolometers */
  dim_t ntslice=0;             /* Number of time slices */
  int nmap;                    /* Number of mapped elements */
  double *outmapcoord=NULL;    /* map coordinates for each bolometer */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  const char *system=NULL;     /* Coordinate system */
  AstCmpMap *testcmpmap=NULL;  /* Combined forward/inverse mapping */
  AstMapping *testsimpmap=NULL;/* Simplified testcmpmap */
  int xnear;                   /* x-nearest neighbour pixel */
  int ynear;                   /* y-nearest neighbour pixel */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Initialize bounds to avoid compiler warnings */
  lbnd_old[0] = 0;
  lbnd_old[1] = 0;
  ubnd_old[0] = 0;
  ubnd_old[1] = 0;

  /* Check for pre-existing LUT and de-allocate it. This will only waste
     time if the MAPCOORD extension is found to be valid and it has
     to be re-loaded from disk. */
  smf_close_mapcoord( data, status );

  /* Assert ICD data order */
  smf_dataOrder( data, 1, status );

  /* Number of bolometers in the data stream */
  if( data->ndims == 3 ) {
    nbolo = data->dims[0] * data->dims[1]; 
    ntslice = data->dims[2];
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Input smfData not time-ordered.", status);
  }

  /* If SMF__NOCREATE_FILE is not set, and file associated with an NDF,
     map a new MAPCOORD extension (or verify an existing one) */ 

  if( !(flags & SMF__NOCREATE_FILE) && data->file ) {
    doextension = 1;
  } else {
    doextension = 0;
    docalc = 1;
  }

  /* Create / check for existing MAPCOORD extension */
  if( doextension ) {
    file = data->file;

    /* Check type of file before proceeding */
    if( file->isSc2store ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,
	     "File was opened by sc2store library (raw data?)",
	     status);
    }

    if( !file->isTstream ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,	"File does not contain time stream data",status);
    }

    /* Get HDS locator to the MAPCOORD extension */
    mapcoordloc = smf_get_xloc( data, "MAPCOORD", "MAP_PROJECTION", "UPDATE", 
				0, 0, status );

    /* Obtain NDF identifier/placeholder for LUT in MAPCOORD extension*/
    lbnd[0] = 0;
    ubnd[0] = nbolo*ntslice-1;
    lutndf = smf_get_ndfid( mapcoordloc, "LUT", "UPDATE", "UNKNOWN",
			      "_INTEGER", 1, lbnd, ubnd, status );

    if( *status == SAI__OK ) {
      /* store the NDF identifier */
      file->mapcoordid = lutndf;

      /* Create sky to output grid mapping using the base coordinates to
	 get the coordinates of the tangent point if it hasn't been done
	 yet. */	  
      sky2map = astGetMapping( outfset, AST__CURRENT, AST__BASE );
      
      /* Get the system from the outfset to match each timeslice */
      system = astGetC( outfset, "system" );

      if( !astOK ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Error extracting mapping info from frameset", 
	       status);
      }
    }

    /* Before mapping the LUT, first check for existing WCS information
       and LBND/UBND for the output map. If they are already correct don't
       bother re-calculating the LUT! */

    if( *status == SAI__OK ) {

      /* Try reading in the WCS information */
      kpg1Wread( mapcoordloc, "WCS", &fstemp, status );
      oldfset = (AstFrameSet*)fstemp;

      if( *status == SAI__OK ) {

	/* Check that the old and new mappings are the same by
	   checking that combining one with the inverse of the other
	   reduces to a UnitMap. */
	
	map2sky_old = astGetMapping( oldfset, AST__BASE, AST__CURRENT );
	testcmpmap = astCmpMap( map2sky_old, sky2map, 1, "" );
	testsimpmap = astSimplify( testcmpmap );
	
	if( astIsAUnitMap( testsimpmap ) ) {

	  /* The mappings are the same, now just check the pixel
	     bounds in the output map */
	  
	  lbnd_temp[0] = 1; 
	  ubnd_temp[0] = 2; 
	  
	  bndndf = smf_get_ndfid( mapcoordloc, "LBND", "READ", "UNKNOWN",
				  "_INTEGER", 1, lbnd_temp, ubnd_temp, 
				  status );
	  
	  if( *status == SAI__OK ) {
	    ndfMap( bndndf, "DATA", "_INTEGER", "READ", data_pntr, &nmap, 
		    status );    
	    data_index = data_pntr[0];

	    if( *status == SAI__OK ) {
	      lbnd_old[0] = data_index[0];
	      lbnd_old[1] = data_index[1];
	    } 
	    ndfAnnul( &bndndf, status );
	  }
	  
	  bndndf = smf_get_ndfid( mapcoordloc, "UBND", "READ", "UNKNOWN",
				  "_INTEGER", 1, lbnd_temp, ubnd_temp, 
				  status );
	  
	  if( *status == SAI__OK ) {
	    ndfMap( bndndf, "DATA", "_INTEGER", "READ", data_pntr, &nmap, 
		    status );
      data_index = data_pntr[0];

	    if( *status == SAI__OK ) {
	      ubnd_old[0] = data_index[0];
	      ubnd_old[1] = data_index[1];
	    } 
	    ndfAnnul( &bndndf, status );
	  }
	  
	  if( *status == SAI__OK ) {
	    /* If we get this far finally do the bounds check! */
	    if( (lbnd_old[0] == lbnd_out[0]) && 
		(lbnd_old[1] == lbnd_out[1]) &&
		(ubnd_old[0] == ubnd_out[0]) &&
		(ubnd_old[1] == ubnd_out[1]) ) {
	      
	      docalc = 0; /* We don't have to re-calculate the LUT */
	      msgOutif(MSG__VERB," ","SMF_CALC_MAPCOORD: Existing LUT OK", 
		       status);
	    }
	  } 
	}

	/* Bad status / AST errors at this point due to problems with 
           MAPCOORD. Annul and continue calculating new MAPCOORD extension. */
	astClearStatus;
	errAnnul(status);

      } else {
	/* Bad status due to non-existence of MAPCOORD. Annul and continue */
	errAnnul(status);
      }
    }
    
  }

  /* If we need to calculate the LUT do it here */
  if( docalc && (*status == SAI__OK) ) {
    msgOutif(MSG__VERB," ","SMF_CALC_MAPCOORD: Calculate new LUT", 
	     status);

    if( doextension ) {
      /* Map the LUT array */
      ndfMap( lutndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap, 
	      status );
      data_index = data_pntr[0];
      if( *status == SAI__OK ) {
        lut = data_index;
      } else {
	errRep( FUNC_NAME, "Unable to map LUT in MAPCOORD extension",
		status);
      }
    } else {
      /* malloc the LUT array */
      lut = smf_malloc( nbolo*ntslice, sizeof(*(data->lut)), 0, status );
    }
      
    /* Calculate the number of bolometers and allocate space for the
       x- and y- output map coordinates */
      
    outmapcoord = smf_malloc( nbolo*2, sizeof(double), 0, status );
      
    /* Retrieve the sky2map mapping from the output frameset (actually
       map2sky) */
    oskyfrm = astGetFrame( outfset, AST__CURRENT );
    sky2map = astGetMapping( outfset, AST__BASE, AST__CURRENT );
    /* Invert it to get Output SKY to output map coordinates */
    astInvert( sky2map );
    /* Create a SkyFrame in absolute coordinates */
    abskyfrm = astCopy( oskyfrm );
    astClear( abskyfrm, "SkyRefIs" );
    astClear( abskyfrm, "SkyRef(1)" );
    astClear( abskyfrm, "SkyRef(2)" );

    if( *status == SAI__OK ) {
      /* Calculate bounds in the input array. 
	 Note: I had to swap the ranges for the two axes from what seemed to
	 make the most sense to me!  EC */
      lbnd_in[0] = 1;
      ubnd_in[0] = (data->dims)[0]; 
      lbnd_in[1] = 1;
      ubnd_in[1] = (data->dims)[1];
      
      /* Loop over time slices */
      for( i=0; i<ntslice; i++ ) {

	/* Calculate the bolometer to map-pixel transformation for tslice */
	bolo2map = smf_rebin_totmap( data, i, abskyfrm, sky2map, moving, 
				     status );	  
	
	if( *status == SAI__OK ) {
	  astTranGrid( bolo2map, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
		       2, nbolo, outmapcoord );
	  
	  for( j=0; j<nbolo; j++ ) {
	    xnear = (int) (outmapcoord[j] - 0.5);
	    ynear = (int) (outmapcoord[nbolo+j] - 0.5);
	    
	    if( (xnear >= 0) && (xnear <= ubnd_out[0] - lbnd_out[0]) &&
		(ynear >= 0) && (ynear <= ubnd_out[1] - lbnd_out[1]) ) {
	      /* Point lands on map */
	      lut[i*nbolo+j] = ynear*(ubnd_out[0]-lbnd_out[0]+1) + xnear;
	    } else {
	      /* Point lands outside map */
	      lut[i*nbolo+j] = VAL__BADI;
	    }
	  }
	}
	/* clean up ast objects */
	bolo2map = astAnnul( bolo2map );

	/* Break out of loop over time slices if bad status */
	if (*status != SAI__OK) goto CLEANUP;
      }

      /* Set the lut pointer in data to the buffer */
      data->lut = lut;

      /* Write the WCS for the projection to the extension */
      if( doextension ) {
        kpg1Wwrt( (AstObject*)outfset, "WCS", mapcoordloc, status ); 
	
	/* Write the pixel bounds for the map to the extension */
	
	lbnd_temp[0] = 1; /* Don't get confused! Bounds for NDF that will */
	ubnd_temp[0] = 2; /* contain the bounds for the output 2d map!    */
	
	bndndf = smf_get_ndfid( mapcoordloc, "LBND", "UPDATE", "UNKNOWN",
				"_INTEGER", 1, lbnd_temp, ubnd_temp, status );
      
	ndfMap( bndndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap, 
		status );    
  data_index = data_pntr[0];
	if( *status == SAI__OK ) {
	  data_index[0] = lbnd_out[0];
	  data_index[1] = lbnd_out[1];
	} else {
	  errRep( FUNC_NAME, "Unable to map LBND in MAPCOORD extension",
		  status);
	} 
	
	ndfAnnul( &bndndf, status );
	
	bndndf = smf_get_ndfid( mapcoordloc, "UBND", "UPDATE", "UNKNOWN",
				"_INTEGER", 1, lbnd_temp, ubnd_temp, status );
	ndfMap( bndndf, "DATA", "_INTEGER", "WRITE", data_pntr, &nmap, 
		status );    
  data_index = data_pntr[0];
	if( *status == SAI__OK ) {
	  data_index[0] = ubnd_out[0];
	  data_index[1] = ubnd_out[1];
	} else {
	  errRep( FUNC_NAME, "Unable to map UBND in MAPCOORD extension",
		  status);
	} 
	ndfAnnul( &bndndf, status );
      }
    }
  }

  /* Clean Up */
 CLEANUP:

  if( outmapcoord ) outmapcoord = smf_free( outmapcoord, status );
  if( testsimpmap ) testsimpmap = astAnnul( testsimpmap );
  if( testcmpmap ) testcmpmap = astAnnul( testcmpmap );
  if( map2sky_old ) map2sky_old = astAnnul( map2sky_old );	
  if( oldfset ) oldfset = astAnnul( oldfset );
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if( abskyfrm ) abskyfrm = astAnnul( abskyfrm );
  if( oskyfrm ) oskyfrm = astAnnul( oskyfrm );  
  if( mapcoordloc ) datAnnul( &mapcoordloc, status );
    
  /* If we get this far, docalc=0, and status is OK, there must be
     a good LUT in there already. Map it so that it is accessible to
     the caller; "UPDATE" so that the caller can modify it if desired. */
  if( (*status == SAI__OK) && (docalc == 0) ) {
    smf_open_mapcoord( data, "UPDATE", status );
  } 
    
}
