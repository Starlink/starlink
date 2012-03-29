/*
*+
*  Name:
*     smf_get_mask

*  Purpose:
*     Get a pointer to a boolean map mask

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     unsigned char *smf_get_mask( smf_modeltype mtype, AstKeyMap *config,
*                                  smfDIMMData *dat, int flags, int *status )

*  Arguments:
*     mtype = (Given)
*        The type of model (COM, FLT or AST) for which the mask is required.
*     config = AstKeyMap * (Given)
*        Configuration parameters that control the map-maker.
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation.
*     flags = int (Given)
*        Control flags.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to the boolean mask, or NULL if no masking is to be
*     performed or if an error occurs. Note, the returned pointer should
*     not be freed by the calling routine.
*
*     The mask value is zero for pixels designated as "source" pixels
*     and non-zero for "background" pixels.

*  Description:
*     This function returns a mask of boolean flags, one for each pixel
*     in the map, if any of the zero masking options for the specified
*     DIMM model are set in the supplied configuration. A non-zero mask
*     value indicates "background" pixels, and zero represents "source"
*     pixels. A NULL pointer is returned if no masking is requested in the
*     supplied configuration, or if the requested masking is not possible
*     (for instance, it is not possible to mask COM on the first iteration
*     using an SNR or low-hits mask since the map data needed to create
*     the mask is not known until the end of the first iteration).

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-2012 (DSB):
*        Original version.
*     16-MAR-2012 (DSB):
*        Add FLT model, and ZERO_NITER parameter.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

/* Local Constants: */
#define NONE       0  /* No masking to be done */
#define LOWHITS    1  /* Mask areas with low hits */
#define CIRCLE     2  /* Mask areas in a given circle */
#define REFNDF     3  /* Mask areas specified by a reference NDF */
#define SNR        4  /* Mask areas with low SNR */
#define PREDEFINED 5  /* Use a mask created on an earlier run of smf_iteratemap */


unsigned char *smf_get_mask( smf_modeltype mtype, AstKeyMap *config,
                             smfDIMMData *dat, int flags, int *status ) {

/* Local Variables: */
   AstCircle *circle;         /* AST Region used to mask a circular area */
   AstKeyMap *subkm;          /* KeyMap holding model config values */
   char words[100];           /* Buffer for variable message words */
   const char *modname;       /* The name of the model  being masked */
   const char *skyrefis;      /* Pointer to SkyRefIs attribute value */
   dim_t i;                   /* Pixel index */
   double *pd;                /* Pointer to next element of map data */
   double *predef;            /* Pointer to mask defined by previous run */
   double *ptr;               /* Pointer to NDF  Data array */
   double *pv;                /* Pointer to next element of map variance */
   double centre[ 2 ];        /* Coords of circle centre in radians */
   double meanhits;           /* Mean hits in the map */
   double radius[ 1 ];        /* Radius of circle in radians */
   double zero_circle[ 3 ];   /* LON/LAT/Radius of circular mask */
   double zero_lowhits;       /* Fraction of mean hits at which to threshold */
   double zero_snr;           /* SNR at which to threshold */
   int *ph;                   /* Pointer to next hits value */
   int have_mask;             /* Did a mask already exist on entry? */
   int indf1;                 /* Id. for supplied reference NDF */
   int indf2;                 /* Id. for used section of reference NDF */
   int lbnd_grid[ 2 ];        /* Lower bounds of map in GRID coords */
   int mask_type;             /* Identifier for the type of mask to use */
   int nel;                   /* Number of mapped NDF pixels */
   int thresh;                /* Absolute threshold on hits */
   int ubnd_grid[ 2 ];        /* Upper bounds of map in GRID coords */
   int zero_c_n;              /* Number of zero circle parameters read */
   int zero_mask;             /* Use the reference NDF as a mask? */
   int zero_notlast;          /* Don't zero on last iteration? */
   int zero_niter;            /* Only mask for the first "niter" iterations. */
   size_t ngood;              /* Number good samples for stats */
   unsigned char **mask;      /* Address of model's mask pointer */
   unsigned char *pm;         /* Pointer to next mask pixel */
   unsigned char *result;     /* Returned mask pointer */

/* Initialise returned values */
   result = NULL;

/* Check inherited status. Also check that a map is being created.  */
   if( *status != SAI__OK || !dat || !dat->map ) return result;

/* Begin an AST context. */
   astBegin;

/* Get the sub-keymap containing the configuration parameters for the
   requested model. Also get a pointer to the mask array to use (there is
   one for each maskable model)*/
   if( mtype == SMF__COM ) {
      modname = "COM";
      mask = &(dat->com_mask);
   } else if( mtype == SMF__AST ) {
      modname = "AST";
      mask = &(dat->ast_mask);
   } else if( mtype == SMF__FLT ) {
      modname = "FLT";
      mask = &(dat->flt_mask);
   } else {
      modname = NULL;
      mask = NULL;
      *status = SAI__ERROR;
      errRepf( " ", "smf_get_mask: Unsupported model type %d supplied - "
               "must be COM, FLT or AST.", status, mtype );
   }
   subkm = NULL;
   astMapGet0A( config, modname, &subkm );

/* Get the number of iterations over which the mask is to be applied. Zero
   means all. Return with no mask if this number of iterations has
   already been performed. */
   zero_niter = 0;
   astMapGet0I( subkm, "ZERO_NITER", &zero_niter );
   if( zero_niter == 0 || dat->iter < zero_niter ) {

/* Only return a mask if this is not the last iteration, or if ZERO_NOTLAST
   is unset. */
      zero_notlast = 0;
      astMapGet0I( subkm, "ZERO_NOTLAST", &zero_notlast );
      if( !( flags & SMF__DIMM_LASTITER ) || !zero_notlast ) {

/* Determine the type of mask being used by looking for non-default
   values for the corresponding configuration parameters in the supplied
   KeyMap. These are checked in increasing priority order. Static masks
   (predefined, circles or external NDFs) may be used on any iteration, but
   dynamic masks (lowhits, snr) will only be avialable once the map has
   been determined at the end of the first iteration. This means that when
   masking anything but the AST model (which is determined after the map),
   the dynamic masks cannot be used on the first iteration. */
         mask_type = NONE;

         zero_lowhits = 0.0;
         astMapGet0D( subkm, "ZERO_LOWHITS", &zero_lowhits );
         if( zero_lowhits > 0.0 ) {
            if( mtype == SMF__AST || !( flags & SMF__DIMM_FIRSTITER ) ) {
               mask_type = LOWHITS;
            }
         } else if( zero_lowhits <  0.0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Bad value for config parameter %s.ZERO_LOWHITS (%g) - "
                     "it must not be negative.", status, modname, zero_lowhits );
         }

         if( astMapGet1D( subkm, "ZERO_CIRCLE", 3, &zero_c_n, zero_circle ) ) {
            if( zero_c_n == 1 || zero_c_n == 3 ) {
               mask_type = CIRCLE;
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( " ", "Bad number of values (%d) for config parameter "
                        "%s.ZERO_CIRCLE - must be 1 or 3.", status, zero_c_n,
                        modname );
            }
         }

         zero_mask = 0;
         astMapGet0I( subkm, "ZERO_MASK", &zero_mask );
         if( zero_mask > 0 ) mask_type = REFNDF;

         zero_snr = 0.0;
         astMapGet0D( subkm, "ZERO_SNR", &zero_snr );
         if( zero_snr > 0.0 ) {
            if( mtype == SMF__AST || !( flags & SMF__DIMM_FIRSTITER ) ) {
               mask_type = SNR;
            }
         } else if( zero_snr <  0.0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Bad value for config parameter %s.ZERO_SNR (%g) - "
                     "it must not be negative.", status, modname, zero_snr );
         }

         if( astMapHasKey( subkm, "ZERO_MASK_POINTER" ) ) {
            astMapGet0P( subkm, "ZERO_MASK_POINTER", (void **) &predef );
            if( predef ) mask_type = PREDEFINED;
         }

/* No need to create a mask if no masking was requested or possible. */
         if( mask_type != NONE ) {

/* Note if a mask existed on entry. If not, create a mask now. */
            if( *mask == NULL ) {
               have_mask = 0;
               *mask = astMalloc( dat->msize*sizeof( **mask ) );
            } else {
               have_mask = 1;
            }

/* Check the pointer can be used. */
            if( *status == SAI__OK ) {

/* Low hits masking... */
               if( mask_type == LOWHITS ) {

/* Set hits pixels with 0 hits to VAL__BADI so that stats1 ignores them */
                  ph = dat->hitsmap;
                  for( i = 0; i < dat->msize; i++,ph++ ) {
                     if( *ph == 0 ) *ph = VAL__BADI;
                  }

/* Find the mean hits in the map */
                  smf_stats1I( dat->hitsmap, 1, dat->msize, NULL, 0, 0, &meanhits,
                               NULL, NULL, &ngood, status );
                  msgOutiff( MSG__DEBUG, " ", "smf_get_mask: mean hits = %lf, ngood "
                             "= %zd", status, meanhits, ngood );

/* Create the mask */
                  thresh = meanhits*zero_lowhits;
                  ph = dat->hitsmap;
                  pm = *mask;
                  for( i = 0; i < dat->msize; i++,ph++ ) {
                     *(pm++) = ( *ph != VAL__BADI && *ph < thresh ) ? 1 : 0;
                  }

/* Report masking info. */
                  msgOutiff( MSG__DEBUG, " ", "smf_get_mask: masking %s "
                             "model at hits = %d.", status, modname, thresh );

/* Circle masking... */
               } else if( mask_type == CIRCLE ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. */
                  if( ! have_mask ) {

/* If only one parameter supplied it is radius, assume reference
   LON/LAT from the frameset to get the centre. If the SkyFrame
   represents offsets from the reference position (i.e. the source is
   moving), assume the circle is to be centred on the origin.  */
                     if( zero_c_n == 1 ) {
                        zero_circle[ 2 ] = zero_circle[ 0 ];

                        skyrefis = astGetC( dat->outfset, "SkyRefIs" );
                        if( skyrefis && !strcmp( skyrefis, "Origin" ) ) {
                           zero_circle[ 0 ] = 0.0;
                           zero_circle[ 1 ] = 0.0;
                        } else {
                           zero_circle[ 0 ] = astGetD( dat->outfset, "SkyRef(1)" );
                           zero_circle[ 1 ] = astGetD( dat->outfset, "SkyRef(2)" );
                        }

                        zero_circle[ 0 ] *= AST__DR2D;
                        zero_circle[ 1 ] *= AST__DR2D;
                     }

/* The supplied bounds are for pixel coordinates... we need bounds for grid
    coordinates which have an offset */
                     lbnd_grid[ 0 ] = 1;
                     lbnd_grid[ 1 ] = 1;
                     ubnd_grid[ 0 ] = dat->ubnd_out[ 0 ] - dat->lbnd_out[ 0 ] + 1;
                     ubnd_grid[ 1 ] = dat->ubnd_out[ 1 ] - dat->lbnd_out[ 1 ] + 1;

/* Coordinates & radius of the circular region converted from degrees
   to radians */
                     centre[ 0 ] = zero_circle[ 0 ]*AST__DD2R;
                     centre[ 1 ] = zero_circle[ 1 ]*AST__DD2R;
                     radius[ 0 ] = zero_circle[ 2 ]*AST__DD2R;

/* Create the Circle, defined in the current Frame of the FrameSet (i.e.
   the sky frame). */
                     circle = astCircle( astGetFrame( dat->outfset, AST__CURRENT), 1,
                                         centre, radius, NULL, " " );

/* Fill the mask with zeros. */
                     memset( *mask, 0, sizeof( **mask )*dat->msize );

/* Get the mapping from the sky frame (current) to the grid frame (base),
   and then set the mask to 1 for all of the values outside this circle */
                     astMaskUB( circle, astGetMapping( dat->outfset, AST__CURRENT,
                                                       AST__BASE ),
                                0, 2, lbnd_grid, ubnd_grid, *mask, 1 );

/* Report masking info. */
                     if( zero_niter == 0 ) {
                        sprintf( words, "on each iteration" );
                     } else {
                        sprintf( words, "for %d iterations", zero_niter );
                     }

                     msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model will"
                                " be masked %s using a circle of "
                                "radius %g arc-secs, centred at %s=%s, %s=%s.",
                                status, modname, words, radius[0]*AST__DR2D*3600,
                                astGetC( dat->outfset, "Symbol(1)" ),
                                astFormat( dat->outfset, 1, centre[ 0 ] ),
                                astGetC( dat->outfset, "Symbol(2)" ),
                                astFormat( dat->outfset, 2, centre[ 1 ] ) );
                  }

/* Reference NDF masking... */
               } else if( mask_type == REFNDF ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. */
                  if( ! have_mask ) {

/* Begin an NDF context. */
                     ndfBegin();

/* Get an identifier for the NDF associated with the "REF" ADAM parameter. */
                     ndfAssoc( "REF", "READ", &indf1, status );

/* Get a section from this NDF that matches the bounds of the map. */
                     ndfSect( indf1, 2, dat->lbnd_out, dat->ubnd_out, &indf2,
                              status );

/* Map the section. */
                     ndfMap( indf2, "DATA", "_DOUBLE", "READ", (void **) &ptr,
                             &nel, status );

/* Check we can use the pointer safely. */
                     if( *status == SAI__OK ) {

/* Find bad pixels in the NDF and set those pixels to 1 in the mask. */
                        pm = *mask;
                        for( i = 0; i < dat->msize; i++ ) {
                           *(pm++) = ( *(ptr++) == VAL__BADD ) ? 1 : 0;
                        }

/* Report masking info. */
                        ndfMsg( "N", indf2 );
                        msgSetc( "M", modname );
                        if( zero_niter == 0 ) {
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The ^M "
                                      "model will be masked on each iteration "
                                      "using the bad pixels in NDF '^N'.",
                                      status );
                        } else {
                           msgSeti( "I", zero_niter );
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The ^M "
                                      "model will be masked for ^I iterations "
                                      "using the bad pixels in NDF '^N'.",
                                      status );
                        }
                     }

/* End the NDF context. */
                     ndfEnd( status );
                  }

/* SNR masking... */
               } else if( mask_type == SNR ) {

/* Form the mask by thresholding the SNR values at the value of "zero_snr". */
                  pd = dat->map;
                  pv = dat->mapvar;
                  pm = *mask;
                  for( i = 0; i < dat->msize; i++,pd++,pv++ ) {
                     *(pm++) = ( *pd != VAL__BADI && *pv != VAL__BADI &&
                                 *pv >= 0.0 && *pd < zero_snr*sqrt( *pv ) ) ? 1 : 0;
                  }

/* Report masking info. */
                  if( !have_mask ) {
                     if( zero_niter == 0 ) {
                        sprintf( words, "on each iteration" );
                     } else {
                        sprintf( words, "for %d iterations", zero_niter );
                     }
                     msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                "will be masked %s using an SNR limit of %g.",
                                status, modname, words, zero_snr );
                  }

/* Predefined masking... */
               } else if( mask_type == PREDEFINED ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. */
                  if( ! have_mask ) {

/* Find bad pixels in the predefined array and set those pixels to 1 in
   the mask. */
                     pm = *mask;
                     for( i = 0; i < dat->msize; i++ ) {
                        *(pm++) = ( *(predef++) == VAL__BADD ) ? 1 : 0;
                     }

/* Report masking info. */
                     if( zero_niter == 0 ) {
                        sprintf( words, "on each iteration" );
                     } else {
                        sprintf( words, "for %d iterations", zero_niter );
                     }
                     msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                "will be masked %s using a smoothed form of "
                                "the final mask created with the previous map.",
                                status, modname, words );
                  }
               }
            }

/* Return the mask pointer if all has gone well. */
            if( *status == SAI__OK ) result = *mask;
         }
      }
   }

/* End the AST context, annulling all AST Objects created in the context. */
   astEnd;

/* Return the pointer to the boolean mask. */
   return result;
}

