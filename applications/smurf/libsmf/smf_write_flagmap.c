/*
*+
*  Name:
*     smf_write_flagmap

*  Purpose:
*     Write flagmap extension to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_flagmap( ThrWorkForce *wf, smf_qual_t mask, smfArray *lut, smfArray *qua,
*                        smfDIMMData *dat, const Grp *flagrootgrp,
*                        dim_t contchunk, const dim_t *lbnd_out,
*                        const dim_t *ubnd_out, AstFrameSet *outfset,
*                        int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     mask = smf_qual_t (Given)
*        Mask indicating which quality bits should be counted when making
*        map. If zero is supplied, the output NDF is 3-dimensional and has
*        a plane for each quality bit, plus an extra plane that counts
*        samples with no flags set.
*     lut = smfArray* (Given)
*        LUT model smfArray
*     qua = smfArray* (Given)
*        QUA model smfArray
*     dat = smfDIMMData* (Given)
*        Pointer to additional map-making data passed around in a struct
*     flagrootgrp = const Grp* (Given)
*        Root name for flagmaps. Can be path to HDS container.
*     contchunk = dim_t (Given)
*        Continuous chunk number
*     lbnd_out = const dim_t * (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = const dim_t * (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine re-grids the time-domain QUALITY into a 2D map or 3D
*     cube.
*
*     If "mask" is non-zero, each pixel in the 2D map is a count of all
*     the samples that land in the pixel with at least one QUALITY bit
*     set that matches the mask. The only modified behaviour is for
*     bolometers flagged SMF__Q_BADB -- if this bit is set in the mask,
*     all quality bits from those bolometers will be ignored (i.e., not
*     included in the flag map).
*
*     If "mask" is zero, each 2D plane in the 3D cube represents a flagmap
*     for a single quality bit (i.e. each pixel counts the number of
*     samples that fall in the pixel and have the corresponding quality
*     bit set). The output NDF is 3-dimensional and has a plane for each
*     quality bit, plus an extra plane that counts samples with no flags
*     set. The pixel bounds of the 3rd axis are (-1:NBIT-1). Plane -1
*     counts the unflagged samples. Plane "i" counts samples with quality
*     bit "i" set (as defined in smf_typ.h).

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     2010-09-27 (EC):
*        Initial version.
*     2015-12-10 (DSB):
*        Added option to create a 3D cube.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia
*     Copyright (C) 2015 East Asian Observatory.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "star/one.h"
#include "star/thr.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Macro to convert a bit position to an integer value */
#define BIT_TO_VAL(bit) (1<<bit)

#define FUNC_NAME "smf_write_flagmap"

void smf_write_flagmap( ThrWorkForce *wf, smf_qual_t mask, smfArray *lut, smfArray *qua,
                        smfDIMMData *dat, const Grp *flagrootgrp,
                        dim_t contchunk, const dim_t *lbnd_out,
                        const dim_t *ubnd_out, AstFrameSet *outfset,
                        int *status ) {

  AstFrameSet *tfset;          /* Temporary FrameSet pointer */
  Grp *mgrp=NULL;               /* Temporary group for map names */
  char *pname=NULL;             /* Poiner to name */
  char name[GRP__SZNAM+1];      /* Buffer for storing names */
  char tempstr[20];             /* Temporary string */
  char tmpname[GRP__SZNAM+1];   /* temp name buffer */
  dim_t bstride;               /* Bolometer stride */
  dim_t i;                     /* loop counter */
  dim_t idx=0;                 /* index within subgroup */
  dim_t ii;                    /* array offset index */
  dim_t j;                     /* loop counter */
  dim_t lbnd3d[3];              /* Lower bounds for 3D output */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t npix;                   /* Number of pixels per plane */
  dim_t ntslice;                /* Number of time slices */
  dim_t tstride;               /* Time stride */
  dim_t ubnd3d[3];              /* Upper bounds for 3D output */
  double shift[ 1 ];            /* Shift from GRID to bit number */
  int *flagmap=NULL;            /* pointer to flagmap data */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  int ibit;                     /* Quality bit number */
  int target;                   /* Target value for incrementing pixel count */
  smfData *mapdata=NULL;        /* smfData for new map */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */

  if( *status != SAI__OK ) return;

  if( !lut || !qua || !dat || !flagrootgrp || !lbnd_out || !ubnd_out ||
      !outfset ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  /* Create a name for the flagmap, taking into account the chunk
     number. Only required if we are using a single output
     container. */
  pname = tmpname;
  grpGet( flagrootgrp, 1, 1, &pname, sizeof(tmpname), status );
  one_strlcpy( name, tmpname, sizeof(name), status );
  one_strlcat( name, ".", sizeof(name), status );

  sprintf(tempstr, "CH%02zd", contchunk);
  one_strlcat( name, tempstr, sizeof(name), status );
  mgrp = grpNew( "flagmap", status );
  grpPut1( mgrp, name, 0, status );

  msgOutf( "", "*** Writing flagmap %s", status, name );

  /* If a non-zero mask value wassupplied, the flagmap is 2-dimensional
     and each pixel value counts the number of samples flagged by any of
     the qualities included in the mask. */
  if( mask ) {

     smf_open_newfile( wf, mgrp, 1, SMF__INTEGER, 2, lbnd_out, ubnd_out, 0, &mapdata,
                       status);
     flagmap = mapdata->pntr[0];

     /* Loop over subgroup index (subarray) */
     for( idx=0; (idx<qua->ndat)&&(*status==SAI__OK); idx++ ) {

          smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                        NULL, &bstride, &tstride, status );
          qua_data = (qua->sdata[idx]->pntr)[0];
          lut_data = (lut->sdata[idx]->pntr)[0];

          /* Loop over bolometer and time slice and create map */
          for( i=0; i<nbolo; i++ ) {
            /* Skip bolometers only if SMF__Q_BADB is set both in the
               data and the mask */
            if( !(qua_data[i*bstride] & mask & SMF__Q_BADB) ) {
              for( j=0; j<ntslice; j++ ) {
                ii = i*bstride + j*tstride;
                if( (qua_data[ii] & mask) && (lut_data[ii] != VAL__BADI) ) {
                  flagmap[lut_data[ii]]++;
                }
              }
            }
          }
        }

     /* Write WCS */
     smf_set_moving( (AstFrame *) outfset, NULL, status );
     ndfPtwcs( outfset, mapdata->file->ndfid, status );

  /* If the mask is zero, the flagmap is 3-dimensional and contains a
     plane for each quality bit, plus an additional plane (plane 1)
     containing the number of unflagged samples in each pixel. */
  } else {
     lbnd3d[ 0 ] = lbnd_out[ 0 ];
     lbnd3d[ 1 ] = lbnd_out[ 1 ];
     lbnd3d[ 2 ] = -1;
     ubnd3d[ 0 ] = ubnd_out[ 0 ];
     ubnd3d[ 1 ] = ubnd_out[ 1 ];
     ubnd3d[ 2 ] = SMF__NQBITS_TSERIES - 1;

     smf_open_newfile( wf, mgrp, 1, SMF__INTEGER, 3, lbnd3d, ubnd3d, 0,
                       &mapdata, status);
     flagmap = mapdata->pntr[0];

     /* No. of pixels in one plane */
     npix = ( ubnd3d[ 1 ] - lbnd3d[ 1 ] + 1 )*( ubnd3d[ 0 ] - lbnd3d[ 0 ] + 1 );

     /* Loop over each quality bit (-1 == "no flags"). */
     for( ibit = -1; ibit < SMF__NQBITS_TSERIES; ibit++ ) {

        /* The test of each sample is performed by checking if the
           sample's quality value ANDed with "mask" is equal to "target".
           This is requires since ibit==-1 (i.e. "count all samples that
           have no flags set") requires a different logic to the other
           ibit values. */
        if( ibit == -1 ) {
           mask = SMF__Q_GOOD;
           target = 0;
        } else {
           mask = BIT_TO_VAL(ibit);
           target = mask;
        }

        /* Loop over subgroup index (subarray) */
        for( idx=0; (idx<qua->ndat)&&(*status==SAI__OK); idx++ ) {

           smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                           NULL, &bstride, &tstride, status );
           qua_data = (qua->sdata[idx]->pntr)[0];
           lut_data = (lut->sdata[idx]->pntr)[0];

           /* Loop over bolometer and time slice and create map */
           for( i=0; i<nbolo; i++ ) {

              /* Skip bolometers only if SMF__Q_BADB is set both in the
                 data and the mask */
              for( j=0; j<ntslice; j++ ) {
                 ii = i*bstride + j*tstride;
                 if( ( (qua_data[ii] & mask) == target ) &&
                     (lut_data[ii] != VAL__BADI) ) {
                   flagmap[lut_data[ii]]++;
                 }
              }
           }
        }

        /* Move the pointer on to th enext plane. */
        flagmap += npix;

     /* Next quality bit. */
     }

     /* Take a copy of the supplied FrameSet so we do not modify it. */
     tfset = astCopy( outfset );

     /* Set atributes for moving target if necessary. */
     smf_set_moving( (AstFrame *) tfset, NULL, status );

     /* Modify the WCS FrameSet so that the base and current Frames are
        3-dimensional. The current Frame is expanded by adding in a simple
        1D Frame representing quality bit, and the base Frame is expanded
        by adding in a 3rd GRID axis. Other Frames are left unchanged.
        The quality bit Frame and the new GRID axis are connected using
        a ShiftMap that gives the right zero-based bit numbers (which
        also correspond to PIXEL indices). */
     shift[ 0 ] = -2.0;
     atlAddWcsAxis( tfset, (AstMapping *) astShiftMap( 1, shift, " " ),
                    astFrame( 1, "Label(1)=Quality bit,Domain=QUALITY" ),
                    NULL, NULL, status );

     /* Store the FrameSet in the 3D NDF. */
     ndfPtwcs( tfset, mapdata->file->ndfid, status );

     tfset = astAnnul( tfset );
  }

  /* Clean up */
  if( mgrp ) grpDelet( &mgrp, status );
  smf_close_file( wf, &mapdata, status );

}
