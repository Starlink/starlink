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
 *     - Detector names are read from .MORE.ACSIS.RECEPPOS.RECEPTORS

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     DSB: David Berry (JAC, UCLan)

 *  History:
 *     31-JUL-2006 (TIMJ):
 *        Original version.
 *     2-OCT-2006 (DSB):
 *        Added RECEPPOS.
 *     4-NOV-2006 (DSB):
 *        Added RECEPTORS.
 *     13-DEC-2006 (DSB):
 *        Added TSYS.
 *     2012-03-06 (TIMJ):
 *        Use SOFA instead of SLA.

 *  Copyright:
 *     Copyright (C) 2012 Science & Technology Facilities Council.
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <string.h>

#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"
#include "ndf.h"
#include "erfa.h"

#include "libsmf/smf.h"
#include "smurf_par.h"
#include "acsis.h"

#define FUNC_NAME "acs_fill_smfHead"

#define EXTENSION "ACSIS"

void
acs_fill_smfHead( smfHead * hdr, int indf, int * status ) {

  HDSLoc * fxloc = NULL;  /* locator of FPLANEX */
  HDSLoc * fyloc = NULL;  /* locator of FPLANEY */
  HDSLoc * nloc = NULL;   /* locator of RECEPTORS */
  HDSLoc * rloc = NULL;   /* locator of RECEPPOS */
  HDSLoc * tloc = NULL;   /* locator of TSYS */
  HDSLoc * xloc = NULL;   /* locator of required extension */
  HDSLoc *tmploc;         /* Temporary HDS locator */
  char *cout;             /* Pointer to next output character */
  char *receptor;         /* String holding receptor names */
  const char *cin;        /* Pointer to next input character */
  const char *fpntrn = NULL; /* mapped RECEPTOR */
  dim_t lbnd[ 3 ];        /* NDF lower pixel bounds */
  dim_t lower[ 3 ];      /* New array lower bounds */
  dim_t ubnd[ 3 ];        /* NDF upper pixel bounds */
  dim_t upper[ 3 ];      /* New array upper bounds */
  double * fplanex = NULL;/* X coordinates in radians */
  double * fplaney = NULL;/* Y coordinates in radians */
  double * fpntrr = NULL; /* mapped RECEPPOS */
  double * fpntrt = NULL; /* mapped TSYS */
  double * fpntrx = NULL; /* mapped FPLANEX */
  double * fpntry = NULL; /* mapped FPLANEY */
  double * receppos = NULL;/* RECEPPOS coordinates in radians */
  double * tsys = NULL;   /* TSYS values */
  double azac1;           /* Frame TCS_AZ_AC1 value */
  double azac2;           /* Frame TCS_AZ_AC2 value */
  double azd;             /* Distance from receptor to TCS_AZ_AC1/2 */
  double rx;              /* Receptor longitude */
  double ry;              /* Receptor latitude */
  double trac1;           /* Frame TCS_TR_AC1 value */
  double trac2;           /* Frame TCS_TR_AC2 value */
  double trd;             /* Distance from receptor to TCS_TR_AC1/2 */
  int iframe;             /* Frame index */
  int ndim;               /* Number of used pixel axes */
  int ri;                 /* Index into receppos or TSYS array */
  size_t clen;            /* Character length */
  size_t irec;            /* Receptor index */
  size_t j;               /* Character count */
  size_t origu1;          /* Original upper bound on second pixel axis */
  size_t origu2;          /* Original upper bound on third pixel axis */
  size_t sizen;           /* Number of RECEPTOR list */
  size_t sizer;           /* Number of RECEPPOS coordinates */
  size_t sizet;           /* Number of TSYS coordinates */
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

  /* Get the extension and find the required components. */
  ndfXloc( indf, EXTENSION, "READ", &xloc, status );
  datFind( xloc, "FPLANEX", &fxloc, status );
  datFind( xloc, "FPLANEY", &fyloc, status );
  datFind( xloc, "RECEPPOS", &rloc, status );
  datFind( xloc, "RECEPTORS", &nloc, status );
  datFind( xloc, "TSYS", &tloc, status );

  /* Get the pixel index bounds of the NDF. */
  ndfBound( indf, 3, lbnd, ubnd, &ndim, status );

  /* See which, if either, of axes 2 and 3 represent a section of the
     original NDF. The original NDF had lower bound of 1 on all axes.
     The original upper bound on axis 2 (detector index) is assumed to be
     equal to the length of the "FPLANEX" array. The original upper bound
     on axis 3 (time slice index) is assumed to be equal to the length of
     the second axis of the "RECEPPOS" array. Get these original upper
     bounds. */
   datSize( fxloc, &origu1, status );
   datSize( rloc, &origu2, status );
   origu2 /= origu1*2;

  /* Get slices of the extension arrays that match the pixel index
     bounds of the NDF. Only do this if the arrays do not already match
     the pixel index bounds. First do the 1D arrays. */
  if( lbnd[ 1 ] != 1 || ubnd[ 1 ] != (int) origu1 ) {
     lower[ 0 ] = lbnd[ 1 ];
     upper[ 0 ] = ubnd[ 1 ];

     tmploc = NULL;
     datSlice( fxloc, 1, lower, upper, &tmploc, status );
     datAnnul( &fxloc, status );
     fxloc = tmploc;

     tmploc = NULL;
     datSlice( fyloc, 1, lower, upper, &tmploc, status );
     datAnnul( &fyloc, status );
     fyloc = tmploc;

     tmploc = NULL;
     datSlice( nloc, 1, lower, upper, &tmploc, status );
     datAnnul( &nloc, status );
     nloc = tmploc;
  }

  /* Now do the 2D arrays. */
  if( lbnd[ 1 ] != 1 || ubnd[ 1 ] != (int) origu1 ||
      lbnd[ 2 ] != 1 || ubnd[ 2 ] != (int) origu2 ) {
     lower[ 0 ] = lbnd[ 1 ];
     upper[ 0 ] = ubnd[ 1 ];
     lower[ 1 ] = lbnd[ 2 ];
     upper[ 1 ] = ubnd[ 2 ];

     tmploc = NULL;
     datSlice( tloc, 2, lower, upper, &tmploc, status );
     datAnnul( &tloc, status );
     tloc = tmploc;

  /* Now do the 3D arrays. */
     lower[ 0 ] = 1;
     upper[ 0 ] = 2;
     lower[ 1 ] = lbnd[ 1 ];
     upper[ 1 ] = ubnd[ 1 ];
     lower[ 2 ] = lbnd[ 2 ];
     upper[ 2 ] = ubnd[ 2 ];

     tmploc = NULL;
     datSlice( rloc, 3, lower, upper, &tmploc, status );
     datAnnul( &rloc, status );
     rloc = tmploc;
  }

  /* map numericals them as vectorized _DOUBLEs */
  datMapV( fxloc, "_DOUBLE", "READ", &tpntr, &sizex, status );
  fpntrx = tpntr;
  datMapV( fyloc, "_DOUBLE", "READ", &tpntr, &sizey, status );
  fpntry = tpntr;
  datMapV( rloc, "_DOUBLE", "READ", &tpntr, &sizer, status );
  fpntrr = tpntr;
  datMapV( tloc, "_DOUBLE", "READ", &tpntr, &sizet, status );
  fpntrt = tpntr;

  /* map strings and get the length of each one */
  datMapV( nloc, "_CHAR", "READ", &tpntr, &sizen, status );
  fpntrn = tpntr;
  datLen( nloc, &clen, status );

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
    fplanex = astMalloc( sizex*sizeof(*fplanex) );
    fplaney = astMalloc( sizex*sizeof(*fplaney) );
    receppos = astMalloc( sizer*sizeof(*receppos) );
    tsys = astMalloc( sizet*sizeof(*tsys) );
    receptor = astMalloc( sizen*(clen + 1) );

    /* need to convert fplane values from arcsec to radians since they are
       stored in arcsec in the ACSIS data files. */
    if (fplanex && fplaney) {
      for (i = 0; i < sizex; i++) {
	fplanex[i] = fpntrx[i] * ERFA_DAS2R;
	fplaney[i] = fpntry[i] * ERFA_DAS2R;
      }
    }

    /* just copy the receppos values since they are already in radians */
    if (receppos) {
      memcpy( receppos, fpntrr, sizer*sizeof(*receppos) );
    }

    /* copy the Tsys values */
    if (tsys) {
      memcpy( tsys, fpntrt, sizet*sizeof(*tsys) );
    }

    /* copy the receptor names, null terminating them. */
    if (receptor) {
      cin = fpntrn;
      cout = receptor;
      for( i = 0; i < sizen; i++ ) {
         for( j = 0; j < clen; j++ ) *(cout++) = *(cin++);
         *(cout++) = 0;
      }
    }



    /* now store in the header */
    hdr->fplanex = fplanex;
    hdr->fplaney = fplaney;
    hdr->detpos = receppos;
    hdr->tsys = tsys;
    hdr->detname = receptor;

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
                trd = eraSeps( rx, ry, trac1, trac2 );
                azd = eraSeps( rx, ry, azac1, azac2 );

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
  datAnnul( &nloc, status );
  datAnnul( &tloc, status );

}
