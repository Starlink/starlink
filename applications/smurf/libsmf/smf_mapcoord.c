/*
*+
*  Name:
*     smf_mapcoord

*  Purpose:
*     Generate a .SMURF.MAPCOORD extension to store pixel coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_mapcoord( smfData *data, AstFrameSet *outfset,
*                   int *lbnd_out, int *ubnd_out, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a .SMURF.MAPCOORD extension in the NDF
*  associated with data based on outfset & lbnd/ubnd_out
*
*     
*  Authors:
*     Edward Chapin (UBC)

*  History:
*     2006-05-16 (EC):
*        Initial version

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_mapcoord"

void smf_mapcoord( smfData *data, AstFrameSet *outfset, 
		   int *lbnd_out, int *ubnd_out, int *status ) {

  /* Local Variables */

  AstCmpMap *bolo2index=NULL;  /* Mapping from bolo->map index */
  AstCmpMap *sky2index=NULL;   /* Mapping from sky coordinates->map index */
  AstMapping *bolo2sky=NULL;   /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map=NULL;    /* Combined mapping bolo->map coordinates */
  int coordndf=NDF__NOID;      /* NDF identifier for coordinates */
  int count;                   /* counter */
  void *data_index[1];         /* Array of pointers to mapped arrays in ndf */
  smfFile *file=NULL;          /* smfFile pointer */
  dim_t i;                     /* loop counter */
  dim_t j;                     /* loop counter */
  dim_t k;                     /* loop counter */
  int lbnd[1];                 /* Pixel bounds for 1d pointing array */
  int lbnd_in[2];              /* Pixel bounds for asttrangrid */
  int ubnd[1];                 /* Pixel bounds for 1d pointing array */
  int ubnd_in[2];              /* Pixel bounds for asttrangrid */
  int *lut;                    /* The lookup table */
  /*AstMatrixMap *map2index=NULL; *//* Mapping for linear combination */
  double mat[2];               /* Matrix for linear combination */
  int nbolo;                   /* Number of bolometers */
  int nmap;                    /* Number of mapped elements */
  double *outmapcoord;         /* map coordinates for each bolometer */
  int place=NDF__NOPL;         /* NDF place holder */
  HDSLoc *smurfloc=NULL;       /* HDS locator to the SMURF extension */
  AstMapping *sky2map=NULL;    /* Mapping celestial->map coordinates */
  char stat[81];               /* Status of ndf open */
  const char *system;          /* Coordinate system */
  int there;                   /* Test for existence */
  int xnear;                   /* x-nearest neighbour pixel */
  int ynear;                   /* y-nearest neighbour pixel */

  AstMathMap *map2index=NULL;
  char func1[80], func2[80], func3[80], func4[80];
  char *fwd[1];
  char *inv[3];
  float crpix1, crpix2, cd1_1, cd2_2;
  int nx;
  int off_c, off_f;


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Number of bolometers in the data stream */
  nbolo = data->dims[0] * data->dims[1]; 

  /* If smfdata is associated with an open NDF continue */
  if( data->file != NULL ) {

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
      errRep(FUNC_NAME,
	     "File does not contain time stream data",
	     status);
    }

    /* Check for the existence of the SMURF extension */
    ndfXstat( file->ndfid, "SMURF", &there, status );

    if( *status == SAI__OK ) {
      if( there ) { 
	/* Obtain access to SMURF extension if there */
	ndfXloc( file->ndfid, "SMURF", "UPDATE", &smurfloc, status );

	msgOutif(MSG__VERB, " ", 
		 "SMF_MAPCOORD: Updating existing SMURF extension", 
		 status);
      } else {      
	/* Otherwise create it*/

	ndfXnew( file->ndfid, "SMURF", "SMURF_DATA", 0, 0, 
		 &smurfloc, status );

	msgOutif(MSG__VERB, " ", 
		 "SMF_MAPCOORD: Creating new SMURF extension", status);
      }
    } else {
      errRep( FUNC_NAME, "Unable to access SMURF extension", status);
    }

    /* Obtain NDF identifier/placeholder for coord. in scuba2 extension*/
    ndfOpen( smurfloc, "MAPCOORD", "WRITE", "UNKNOWN", 
	     &coordndf, &place, status );	

    if( *status == SAI__OK ) {

      if( (coordndf == NDF__NOID) && (place != NDF__NOPL) ) {
	/* If new NDF create it at placeholder */
	lbnd[0] = 0;
	ubnd[0] = nbolo*data->dims[2]-1;
	ndfNew( "_INTEGER", 1, lbnd, ubnd, &place, &coordndf, status );
      } 

      if( (coordndf == NDF__NOID) && (place == NDF__NOPL) ) {
	/* If both the placeholder and ndf are void set bad status */
	*status = SAI__ERROR;
	errRep(FUNC_NAME,
	       "Problem getting MAPCOORD NDF identifier",
	       status);
      }
    } else {
      errRep( FUNC_NAME, 
	      "Unable to access MAPCOORD in SMURF extension", status);
    }

    /* Map the data array */
    ndfMap( coordndf, "DATA", "_INTEGER", "WRITE", data_index, &nmap, 
	    status );    

    if( *status == SAI__OK ) {
      lut = data_index[0];
    } else {
      errRep( FUNC_NAME, "Unable to map LUT in SMURF extension",
	      status);
    }
        
    /* Calculate the number of bolometers and allocate space for the
       x- and y- output map coordinates */

    outmapcoord = smf_malloc( nbolo*2, sizeof(double), 0, status );

    if( *status == SAI__OK ) {
      /* Calculate bounds in the input array. 
	 Note: I had to swap the ranges for the two axes from what seemed to
         make the most sense to me!  EC */
      lbnd_in[0] = 0;
      ubnd_in[0] = (data->dims)[0]-1; 
      lbnd_in[1] = 0;
      ubnd_in[1] = (data->dims)[1]-1;
    
      /* Get the system from the outfset */
      system = astGetC( outfset, "system" );

      /* Loop over time slices */
      for( i=0; i<(data->dims)[2]; i++ ) {
	smf_tslice_ast( data, i, 1, status);

	if( *status == SAI__OK ) {

	  /* Get bolo -> sky mapping 
	     Set the System attribute for the SkyFframe in input WCS 
	     FrameSet and extract the IN_PIXEL->Sky mapping. */	  
	  
	  astSetC( (data->hdr)->wcs, "SYSTEM", system );
	  bolo2sky = astGetMapping( data->hdr->wcs, AST__BASE, 
				    AST__CURRENT );
	  
	  /* Create sky to output grid mapping 
	     using the base coordinates to get the coordinates of the 
	     tangent point if it hasn't been done yet. */
	  
	  if( sky2map == NULL ) { 
	    /* Extract the Sky->REF_PIXEL mapping. */
	    astSetC( outfset, "SYSTEM", system );
	    sky2map = astGetMapping( outfset, AST__CURRENT, 
				     AST__BASE );
	  }
	  
	  /* Concatenate Mappings to get IN_PIXEL->tanplane offset Mapping */
	  bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );
	  
	  /* Calculate an Ast Mapping that converts between tangent
             planet coordinates and a 1d index into a nearest-neighbour
             sampled 2d map array. */

	  /*
	  crpix1 = floor((ubnd_out[0] - lbnd_out[0])/2) + 1;
	  crpix2 = floor((ubnd_out[1] - lbnd_out[1])/2) + 1;

	  nx = (ubnd[0] - lbnd[0]) + 1;

	  sprintf( func1, "a = floor( (v + 0.5)/%d )", nx );
	  sprintf( func2, "gx = %d + nint( v + 1.5 - a*%d )", crpix1, nx );
	  sprintf( func3, "gy = %d + a + 1", crpix2 );
	    
	  sprintf( func4, "v = nint( gx - %d) - 1 + ( nint( gy -%d) - 1 )*%d",
	  crpix1, crpix2, nx );
	    
	  inv[ 0 ] = func1;
	  inv[ 1 ] = func2;
	  inv[ 2 ] = func3;
	    
	  fwd[ 0 ] = func4;
	    
	  map2index = astMathMap( 2, 1, 1, fwd, 3, inv, "" );
	  */

	  /* Concat. bolo2map and map2index to get bolo->index mapping */
	  /*bolo2index = astCmpMap( bolo2map, map2index, 1 ,"" );*/

	  /* Calc the map index for each bolometer at this time slice using
	     a nearest-neighbour sampling */

	  astTranGrid( bolo2map, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
		       2, nbolo, outmapcoord );
	  	  
	  for( j=0; j<nbolo; j++ ) {
	    xnear = (int) (outmapcoord[j] + 0.5);
	    ynear = (int) (outmapcoord[nbolo+j] + 0.5);
	    
	    
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
	bolo2sky = astAnnul( bolo2sky );
	bolo2map = astAnnul( bolo2map );
      }
      /* Break out of loop over time slices if bad status */
      if (*status != SAI__OK) goto CLEANUP;
    }
    
    
    /* Write frameset to the extension */
    /*ndfPtwcs( outfset, coordndf, status );*/
    
    /* Clean Up */
  CLEANUP:
    if (sky2map) sky2map  = astAnnul( sky2map );
    if (bolo2sky) bolo2sky = astAnnul( bolo2sky );
    if (bolo2map) bolo2map = astAnnul( bolo2map );
    
    ndfUnmap( coordndf, "DATA", status );
    ndfAnnul( &coordndf, status );
    
    datAnnul( &smurfloc, status );

    smf_free( outmapcoord, status );
        
    } else { 

    /* smfdata not associated with a file */

    *status = SAI__ERROR;
    errRep(FUNC_NAME,"No file associated with smfdata",
	   status);
 }
  

}
