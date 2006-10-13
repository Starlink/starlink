/*
 *+
 *  Name:
 *     acs_fill_smfHead

 *  Purpose:
 *     Populate smfHead with ACSIS specific information

 *  Language:
 *     ANSI C

 *  Invocation:
 *     void acs_fill_smfHead( smfHead * hdr, int indf, int * status );

 *  Description:
 *     This function opens the ACSIS extension and retrieves the
 *     deteector positions.

 *  Notes:
 *     - Focal plane coordinates are read (in arcsec) from .MORE.ACSIS.FPLANEX
 *     and .MORE.ACSIS.FPLANEY.
 *     - Detector positions in tracking coordinates are read (in radians) from 
 *     .MORE.ACSIS.RECEPPOS.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     DSB: David Berry (JAC, UCLan)

 *  History:
 *     31-JUL-2006 (TIMJ):
 *        Original version.
 *     2-OCT-2006 (DSB):
 *        Added RECEPPOS.

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

#include <string.h>

#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"
#include "ndf.h"
#include "star/slalib.h"

#include "libsmf/smf.h"
#include "smurf_par.h"
#include "acsis.h"

#define FUNC_NAME "acs_fill_smfHead"

#define EXTENSION "ACSIS"

void
acs_fill_smfHead( smfHead * hdr, int indf, int * status ) {

  HDSLoc * fxloc = NULL;  /* locator of FPLANEX */
  HDSLoc * fyloc = NULL;  /* locator of FPLANEY */
  HDSLoc * rloc = NULL;   /* locator of RECEPPOS */
  HDSLoc * xloc = NULL;   /* locator of required extension */
  double * fplanex = NULL; /* X coordinates in radians */
  double * fplaney = NULL; /* Y coordinates in radians */
  double * fpntrr = NULL; /* mapped RECEPPOS */
  double * fpntrx = NULL; /* mapped FPLANEX */
  double * fpntry = NULL; /* mapped FPLANEY */
  double * receppos = NULL;/* RECEPPOS coordinates in radians */
  double azac1;           /* Frame TCS_AZ_AC1 value */
  double azac2;           /* Frame TCS_AZ_AC2 value */
  double azd;             /* Distance from receptor to TCS_AZ_AC1/2 */
  double rx;              /* Receptor longitude */
  double ry;              /* Receptor latitude */
  double trac1;           /* Frame TCS_TR_AC1 value */
  double trac2;           /* Frame TCS_TR_AC2 value */
  double trd;             /* Distance from receptor to TCS_TR_AC1/2 */
  int iframe;             /* Frame index */
  int irec;               /* Receptor index */
  int ri;                 /* Index into receppos array */
  size_t sizer;           /* Number of RECEPPOS coordinates */
  size_t sizex;           /* Number of FPLANEX coordinates */
  size_t sizey;           /* Number of FPLANEY coordinates */
  unsigned int i;         /* loop counter */
  void * tpntr;           /* temporary pointer */



  if (*status != SAI__OK) return;

  /* Check that we have a valid NDF identifier */
  if (indf == NDF__NOID) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": require a valid NDF identifier", status );
    return;
  }

  /* Get the extension and find FPLANEX, FPLANEY and RECEPPOS */
  ndfXloc( indf, EXTENSION, "READ", &xloc, status );
  datFind( xloc, "FPLANEX", &fxloc, status );
  datFind( xloc, "FPLANEY", &fyloc, status );
  datFind( xloc, "RECEPPOS", &rloc, status );

  /* map them as a vectorized _DOUBLE */
  datMapV( fxloc, "_DOUBLE", "READ", &tpntr, &sizex, status );
  fpntrx = tpntr;
  datMapV( fyloc, "_DOUBLE", "READ", &tpntr, &sizey, status );
  fpntry = tpntr;
  datMapV( rloc, "_DOUBLE", "READ", &tpntr, &sizer, status );
  fpntrr = tpntr;

  /* sanity check */
  if (sizex != sizey && *status == SAI__OK) {
    *status = SAI__ERROR;
    msgSeti( "FX", sizex );
    msgSeti( "FY", sizey );
    errRep( " ", FUNC_NAME ": Possible corrupt file. FPLANEX size != FPLANEY"
	    " (^FX != ^FY)", status);
  }

  /* allocate memory and copy */
  if (*status == SAI__OK) {
    hdr->ndet = sizex;
    fplanex = smf_malloc( sizex, sizeof(*fplanex), 0, status );
    fplaney = smf_malloc( sizex, sizeof(*fplaney), 0, status );
    receppos = smf_malloc( sizer, sizeof(*receppos), 0, status );

    /* need to convert fplane values from arcsec to radians since they are 
       stored in arcsec in the ACSIS data files. */
    if (fplanex && fplaney) {
      for (i = 0; i < sizex; i++) {
	fplanex[i] = fpntrx[i] * DAS2R;
	fplaney[i] = fpntry[i] * DAS2R;
      }
    }

    /* just copy the receppos values since they are already in radians */
    if (receppos) {
      memcpy( receppos, fpntrr, sizer*sizeof(*receppos) );
    }

    /* now store in the header */
    hdr->fplanex = fplanex;
    hdr->fplaney = fplaney;
    hdr->detpos = receppos;

    /* The receppos values may be either in tracking coords or in azel
       coords. We determine which by seeing if the receppos values are 
       closer to the TCS_AZ_AC1/2 values (azel) or the TCS_TR_AC1/2
       values (tracking). Loop round all time frames. */
    for( iframe = 0; iframe < hdr->nframes; iframe++ ) {

    /* Get the TCS_AZ_AC1/2 and TCS_TR_AC1/2 values for this frame. */
       trac1 = ( hdr->allState)[ iframe ].tcs_tr_ac1;
       trac2 = ( hdr->allState)[ iframe ].tcs_tr_ac2;
       azac1 = ( hdr->allState)[ iframe ].tcs_az_ac1;
       azac2 = ( hdr->allState)[ iframe ].tcs_az_ac2;

    /* Check they are good. */
       if( trac1 != AST__BAD && trac2 != AST__BAD &&
           azac1 != AST__BAD && azac2 != AST__BAD ) {

    /* Initialise the index of the first receppos value for this frame. */
          ri = iframe*2*hdr->ndet;

    /* Loop round all receptors, looking for a receptor with good axis
       values. */
          for( irec = 0; irec < hdr->ndet; irec++ ) {
             rx = receppos[ ri++ ];
             ry = receppos[ ri++ ];

             if( rx != AST__BAD && ry != AST__BAD ) {

    /* Find the arc-distance between the receppos position and the tr_AC1
       position. */
                trd = slaDsep( rx, ry, trac1, trac2 );
                azd = slaDsep( rx, ry, azac1, azac2 );

    /* If the receppos values are closer to the TRACKING values, then  
       set a flag indicating that the receppos values should be treated
       as TRACKING values. Otherwise, set the flag to indicate that the
       receppos values should be treated as AZEL values. */
                hdr->dpazel = ( fabs( trd ) > fabs( azd ) );
                goto L10;
             }
          }
       }
    }

L10:;

  }

  /* free resources */
  datAnnul( &fyloc, status );
  datAnnul( &fxloc, status );
  datAnnul( &rloc, status );
  datAnnul( &xloc, status );

}
