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
*     unsigned char *smf_get_mask( ThrWorkForce *wf, smf_modeltype mtype,
*                                  AstKeyMap *config, smfDIMMData *dat,
*                                  int flags, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     mtype = (Given)
*        The type of model (COM, SSN, PCA, FLT or AST) for which the mask is
*        required.
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
*
*     If multiple masks are specified (e.g. ZERO_MASK and ZERO_CIRCLE),
*     they are combined into a single mask. How this is done depends on
*     the value of the "ZERO_UNION" parameter. If it is non-zero, then
*     the combined mask is the union of the individual masks (i.e. a mask
*     pixel is "source" (zero) iff one or more of the indivudal mask pixels
*     are source). If ZERO_UNION is zero, the combined mask is the
*     intersection of the individual masks (i.e. a mask pixel is "source"
*     (zero) iff all the mask pixels are source).

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-2012 (DSB):
*        Original version.
*     16-MAR-2012 (DSB):
*        Add FLT model, and ZERO_NITER parameter.
*     31-MAY-2012 (DSB):
*        Add ZERO_FREEZE parameter.
*     18-OCT-2012 (DSB):
*        Allow multiple masks to be used.
*     11-DEC-2012 (DSB):
*        Change zero_freeze so that negative values cause permanent
*        freeze, even on iteration zero (for the benefit of the skyloop.py
*        command). Also, initialise the new mask to hold the values implied
*        by the quality array associated with the current map.
*     15-FEB-2013 (DSB):
*        - Allow union or intersection of multiple masks to be used.
*        - Report an error if the mask contains fewer than 5 source pixels.
*     4-APR-2013 (DSB):
*        - Use VAL__BADD, not VAL__BADI, for checking for bad map and
*        vartiance values. This bug will have had no effect since an
*        additional check was made that mapvar is positive, and so caught
*        bad variances (since VAL__BADD is negative).
*        - When checking that the mask has significant size, do not
*        include bad map pixels (eg the map corners, etc) in the count of
*        source pixels.
*     10-APR-2013 (DSB):
*        When checking that the mask encloses a significant number of source
*        pixels, do not require source pixels to have a defined variance
*        since variances are usually not available on the first iteration.
*     9-JUL-2013 (DSB):
*        The ZERO_NITER and ZERO_FREEZE values should count the iterations
*        perfomed *after* any initial iterations for which the AST model was
*        skipped.
*     23-JAN-2014 (DSB):
*        When creating a mask from a quality array imported from an initial
*        sky NDF, only check the quality bit relevant to the mask being created.
*     25-FEB-2014 (DSB):
*        Do not rely on the FIRSTITER bit in "flags" as an indication of
*        whether a dynamic mask can be created. This is because a map may
*        be available even on the first iteration if an external map is
*        being used as the initial sky map (as used by skyloop). Instead,
*        use an explicit flag ("dat->mapok") that indicates if the map can
*        be used.
*     5-MAR-2014 (DSB):
*        No need to import mask from the initial sky map since the mask
*        will be recalculated anyway.
*     7-MAR-2014 (DSB):
*        If the mask contains fewer than 5 source pixels, issue a warning
*        rather than an error, since a null mask does not prevent a map
*        being cerated.
*     10-MAR-2014 (DSB):
*        Allow source pixels defined by SNR to be accumulated from iteration
*        to iteration, rather than replaced.
*     11-MAR-2014 (DSB):
*        Allow map to be high-pass filtered before creating the SNR mask.
*     16-MAR-2014 (DSB):
*        Allow map to be low-pass filtered before creating the SNR mask.
*     18-MAR-2014 (DSB):
*        Allow masks to be created using an algorithm like the
*        kappa:ffclean command.
*     2-APR-2014 (DSB):
*        Changed so that the SNR map, rather than the data value map, is
*        filtered using ZERO_SNR_LOPASS and ZERO_SNR_HIPASS.
*     12-JUN-2015 (DSB):
*        Allow source regions to have negative data values (as happens
*        when making maps of Q/U values). This uses the zero_snr_neg
*        flag. NOTE, this only affects ffclean masking - basic SNR
*        thresholding has always allowed sources to be negative.
*     28-AUG-2015 (DSB):
*        When creating maps of polarimetry data (Q or U), the sources can
*        be negative, so treat large negative SNR values as source rather
*        than background.
*     9-SEP-2015 (DSB):
*        Allow masks to be frozen when the convergence process reaches a
*        specified mean map change per iteration.
*     19-OCT-2016 (DSB):
*        - ZERO_NITER can now be given as a fraction in the range 0.0 to 1.0
*        to indicate a normalised map change at which to switch off masking.
*        - If a negative value is supplied for ZERO_FREEZE, the mask is
*        frozen as soon as the initial AST_SKIP iterations have been done.
*     9-DEC-2016 (DSB):
*        - Allow ZERO_SNRLO to be less than or equal to zero (i.e. now set
*        it to <undef> rather than zero to prevent it being used).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012-2014 Science & Technology Facilities Council.
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
#include "dat_par.h"

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
#define NTYPE      6  /* No. of different mask types */


unsigned char *smf_get_mask( ThrWorkForce *wf, smf_modeltype mtype,
                             AstKeyMap *config, smfDIMMData *dat, int flags,
                             int *status ) {

/* Local Variables: */
   AstCircle *circle;         /* AST Region used to mask a circular area */
   AstKeyMap *akm;            /* KeyMap holding AST config values */
   AstKeyMap *subkm;          /* KeyMap holding model config values */
   char refparam[ DAT__SZNAM ];/* Name for reference NDF parameter */
   char words[100];           /* Buffer for variable message words */
   const char *cval;          /* The ZERO_MASK string value */
   const char *modname;       /* The name of the model  being masked */
   const char *skyrefis;      /* Pointer to SkyRefIs attribute value */
   dim_t i;                   /* Pixel index */
   double *mapuse;            /* Map to use for SNR mask */
   double *mapuset;           /* Intermediate map */
   double *pd;                /* Pointer to next element of map data */
   double *predef;            /* Pointer to mask defined by previous run */
   double *ptr;               /* Pointer to NDF  Data array */
   double *pv;                /* Pointer to next element of map variance */
   double centre[ 2 ];        /* Coords of circle centre in radians */
   double meanhits;           /* Mean hits in the map */
   double radius[ 1 ];        /* Radius of circle in radians */
   double zero_circle[ 3 ];   /* LON/LAT/Radius of circular mask */
   double zero_lowhits;       /* Fraction of mean hits at which to threshold */
   double zero_niter;         /* Only mask for the first "niter" iterations. */
   double zero_snr;           /* Higher SNR at which to threshold */
   double zero_snr_hipass;    /* Size of box for high-pass smoothing SNR map */
   double zero_snrlo;         /* Lower SNR at which to threshold */
   int *ph;                   /* Pointer to next hits value */
   int abssnr;                /* Can sources be negative as well as positive? */
   int domask;                /* Apply a mask? */
   int have_mask;             /* Did a mask already exist on entry? */
   int hipass;                /* mask filter size in pixels */
   int imask;                 /* Index of next mask type */
   int indf1;                 /* Id. for supplied reference NDF */
   int indf2;                 /* Id. for used section of reference NDF */
   int isstatic;              /* Are all used masks static? */
   int lbnd_grid[ 2 ];        /* Lower bounds of map in GRID coords */
   int mask_types[ NTYPE ];   /* Identifier for the types of mask to use */
   int munion;                /* Use union of supplied masks */
   int nel;                   /* Number of mapped NDF pixels */
   int nmask;                 /* The number of masks to be combined */
   int nsource;               /* No. of source pixels in final mask */
   int skip;                  /* No. of iters for which AST is not subtracted */
   int thresh;                /* Absolute threshold on hits */
   int ubnd_grid[ 2 ];        /* Upper bounds of map in GRID coords */
   int zero_accum;            /* Accumulate source pixels? */
   int zero_c_n;              /* Number of zero circle parameters read */
   int zero_mask;             /* Use the reference NDF as a mask? */
   int zero_notlast;          /* Don't zero on last iteration? */
   int zero_snr_ffclean;      /* Define mask using ffclean algorithm? */
   int zero_snr_lopass;       /* Size of box for low-pass smoothing SNR map */
   size_t ngood;              /* Number good samples for stats */
   unsigned char **mask;      /* Address of model's mask pointer */
   unsigned char *accmask;    /* Mask to be accumulated into the new mask */
   unsigned char *newmask;    /* Individual mask work space */
   unsigned char *pa;         /* Point to next accumulated mask pixel */
   unsigned char *pm;         /* Pointer to next returned mask pixel */
   unsigned char *pn;         /* Pointer to next new mask pixel */
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
   } else if( mtype == SMF__SSN ) {
      modname = "SSN";
      mask = &(dat->ssn_mask);
   } else if( mtype == SMF__PCA ) {
      modname = "PCA";
      mask = &(dat->pca_mask);
   } else {
      modname = NULL;
      mask = NULL;
      *status = SAI__ERROR;
      errRepf( " ", "smf_get_mask: Unsupported model type %d supplied - "
               "must be COM, PCA, SSN, FLT or AST.", status, mtype );
   }
   subkm = NULL;
   astMapGet0A( config, modname, &subkm );

/* If we are masking the SSN model, or if the data values represent
   signed Q/U values, indicate that the masking should be based on the
   absolute value of the SNR. */
   abssnr = ( mtype == SMF__SSN || dat->poldata );

/* See if the source pixels should be accumulated from iteration to
   iteration, rather than being replaced. */
   astMapGet0I( subkm, "ZERO_ACCUM", &zero_accum );

/* Get the "ast.skip" value - when considering "zero_niter" and
   "zero_freeze", we only count iterations for which the AST model
   is subtracted (i.e. the ones following the initial "ast.skip"
   iterations). */
   astMapGet0A( config, "AST", &akm );
   astMapGet0I( akm, "SKIP", &skip );
   akm = astAnnul( akm );
   if( skip < 0 ) skip = -skip;

/* Get the number of iterations over which the mask is to be applied. Zero
   means all. Return with no mask if this number of iterations has
   already been performed. */
   zero_niter = 0;
   astMapGet0D( subkm, "ZERO_NITER", &zero_niter );

   if( zero_niter == VAL__BADD ) {
      domask = 0;
   } else if( zero_niter <= 0.0 ) {
      domask = 1;
   } else if( zero_niter < 1.0 ) {
      if( dat->mapchange >= zero_niter || dat->iter <= skip + 1 ) {
         domask = 1;
         dat->allow_convergence = 0; /* We've not yet reached the limit
                                        so we must do more iterations */
      } else {
         domask = 0;

/* Ensure masking is never used again, in case normalise change
   increases again as a consequence of switching off the mask. */
         astMapPut0D( subkm, "ZERO_NITER", VAL__BADD, NULL );
      }

   } else {
      domask = (dat->iter < (int)( zero_niter + 0.5 ) + skip);
      if( domask ) dat->allow_convergence = 0;
   }

   if( domask ) {

/* Only return a mask if this is not the last iteration, or if ZERO_NOTLAST
   is unset. */
      zero_notlast = 0;
      astMapGet0I( subkm, "ZERO_NOTLAST", &zero_notlast );
      if( !( flags & SMF__DIMM_LASTITER ) || !zero_notlast ) {

/* Create a list of the mask types to be combined to get the final mask by
   looking for non-default values for the corresponding configuration
   parameters in the supplied KeyMap. Static masks (predefined, circles
   or external NDFs) may be used on any iteration, but dynamic masks
   (lowhits, snr) will only be avialable once the map has been determined
   (usually at the end of the first iteration). So check that the values
   in the map are usable before creating a dynamic mask. Make a note if
   all masks being used are static. */

         isstatic = 1;
         nmask = 0;

         zero_lowhits = 0.0;
         astMapGet0D( subkm, "ZERO_LOWHITS", &zero_lowhits );
         if( zero_lowhits > 0.0 ) {
            if( dat->mapok ) {
               mask_types[ nmask++] = LOWHITS;
               isstatic = 0;
            }
         } else if( zero_lowhits <  0.0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Bad value for config parameter %s.ZERO_LOWHITS (%g) - "
                     "it must not be negative.", status, modname, zero_lowhits );
         }

         if( astMapGet1D( subkm, "ZERO_CIRCLE", 3, &zero_c_n, zero_circle ) ) {
            if( zero_c_n == 1 || zero_c_n == 3 ) {
               mask_types[ nmask++] = CIRCLE;
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( " ", "Bad number of values (%d) for config parameter "
                        "%s.ZERO_CIRCLE - must be 1 or 3.", status, zero_c_n,
                        modname );
            }
         }

         cval = NULL;
         astMapGet0C( subkm, "ZERO_MASK", &cval );
         if( cval ) {
            if( !astChrMatch( cval, "REF" ) &&
                !astChrMatch( cval, "MASK2" ) &&
                !astChrMatch( cval, "MASK3" ) ) {
               astMapGet0I( subkm, "ZERO_MASK", &zero_mask );
               cval = ( zero_mask > 0 ) ? "REF" : NULL;
            }
            if( cval ) {
               strcpy( refparam, cval );
               astChrCase( NULL, refparam, 1, 0 );
               mask_types[ nmask++] = REFNDF;
            }
         }

         zero_snr_ffclean = 0;
         zero_snr_lopass = 0;
         zero_snr_hipass = 0.0;
         zero_snr = 0.0;
         astMapGet0D( subkm, "ZERO_SNR", &zero_snr );
         if( zero_snr > 0.0 ) {
            if( dat->mapok ) {
               mask_types[ nmask++] = SNR;
               isstatic = 0;
               astMapGet0D( subkm, "ZERO_SNR_HIPASS", &zero_snr_hipass );
               astMapGet0I( subkm, "ZERO_SNR_LOPASS", &zero_snr_lopass );
               astMapGet0I( subkm, "ZERO_SNR_FFCLEAN", &zero_snr_ffclean );
            }
         } else if( zero_snr <  0.0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Bad value for config parameter %s.ZERO_SNR (%g) - "
                     "it must not be negative.", status, modname, zero_snr );
         }

         if( astMapHasKey( subkm, "ZERO_MASK_POINTER" ) ) {
            astMapGet0P( subkm, "ZERO_MASK_POINTER", (void **) &predef );
            if( predef ) mask_types[ nmask++] = PREDEFINED;
         }

/* No need to create a mask if no masking was requested or possible. */
         if( nmask > 0 ) {

/* Decide if we are using the union or intersection of the masks. */
            astMapGet0I( subkm, "ZERO_UNION", &munion );

/* Note if a mask existed on entry. If not, create a mask now. */
            if( *mask == NULL ) {
               have_mask = 0;
               *mask = astCalloc( dat->msize, sizeof( **mask ) );
            } else {
               have_mask = 1;
            }

/* If we are combining more than one mask, we need work space to hold
   an individual mask independently of the total mask. If we are using
   only one mask, then just use the main mask array. */
            if( nmask > 1 ) {
               newmask = astMalloc( dat->msize*sizeof( *newmask ) );
            } else {
               newmask = *mask;
            }

/* Save a pointer to the mask defining the pixels that must definitely be
   set as source pixels in the returned mask. */
            if( zero_accum && have_mask ) {
               accmask = *mask;
            } else {
               accmask = NULL;
            }

/* Get the number of iterations, or the mean map-change per iteration, after
   which the mask is to be frozen. Zero means "never freeze the mask". */
            double zero_freeze = 0.0;
            astMapGet0D( subkm, "ZERO_FREEZE", &zero_freeze );

/* Loop round each type of mask to be used. */
            for( imask = 0; imask < nmask && *status == SAI__OK; imask++ ){

/* If the mask is now frozen, we just return the existing mask. So leave the
   loop. */
               if( ( zero_freeze > 0 && (
                   ( zero_freeze < 1.0 && dat->mapchange < zero_freeze && dat->iter > skip + 1 ) ||
                   ( zero_freeze >= 1.0 && dat->iter > (int)( zero_freeze + 0.5 ) + skip ) ) ) ||
                   ( zero_freeze < 0 && dat->iter > skip ) ) {
                  msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s mask "
                             "is now frozen.", status, modname );
                  break;

/* Low hits masking... */
               } else if( mask_types[ imask ] == LOWHITS ) {

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
                  pn = newmask;
                  for( i = 0; i < dat->msize; i++,ph++ ) {
                     *(pn++) = ( *ph != VAL__BADI && *ph < thresh ) ? 1 : 0;
                  }

/* Report masking info. */
                  msgOutiff( MSG__DEBUG, " ", "smf_get_mask: masking %s "
                             "model at hits = %d.", status, modname, thresh );

/* Circle masking... */
               } else if( mask_types[ imask ] == CIRCLE ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. But we need to recalculate the circle
   mask if are combining it with any non-static masks. */
                  if( ! have_mask || ! isstatic ) {

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
                     memset( newmask, 0, sizeof( *newmask )*dat->msize );

/* Get the mapping from the sky frame (current) to the grid frame (base),
   and then set the mask to 1 for all of the values outside this circle */
                     astMaskUB( circle, astGetMapping( dat->outfset, AST__CURRENT,
                                                       AST__BASE ),
                                0, 2, lbnd_grid, ubnd_grid, newmask, 1 );

/* Report masking info. */
                     if( zero_niter <= 0 ) {
                        sprintf( words, "on each iteration" );
                     } else if( zero_niter < 1.0 ) {
                        sprintf( words, "until normalised change reaches %g", zero_niter );
                     } else if( zero_niter != VAL__BADD ){
                        sprintf( words, "for %d iterations", (int)( zero_niter + 0.5 ) );
                     } else {
                        words[0] = 0;
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
               } else if( mask_types[ imask ] == REFNDF ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. But we need to recalculate the NDF
   mask if are combining it with any non-static masks. */
                  if( ! have_mask || ! isstatic ) {

/* Begin an NDF context. */
                     ndfBegin();

/* Get an identifier for the NDF using the associated ADAM parameter. */
                     ndfAssoc( refparam, "READ", &indf1, status );

/* Get a section from this NDF that matches the bounds of the map. */
                     ndfSect( indf1, 2, dat->lbnd_out, dat->ubnd_out, &indf2,
                              status );

/* Map the section. */
                     ndfMap( indf2, "DATA", "_DOUBLE", "READ", (void **) &ptr,
                             &nel, status );

/* Check we can use the pointer safely. */
                     if( *status == SAI__OK ) {

/* Find bad pixels in the NDF and set those pixels to 1 in the mask. */
                        pn = newmask;
                        for( i = 0; i < dat->msize; i++ ) {
                           *(pn++) = ( *(ptr++) == VAL__BADD ) ? 1 : 0;
                        }

/* Report masking info. */
                        ndfMsg( "N", indf2 );
                        msgSetc( "M", modname );
                        if( zero_niter <= 0 ) {
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The ^M "
                                      "model will be masked on each iteration "
                                      "using the bad pixels in NDF '^N'.",
                                      status );
                        } else if( zero_niter < 1.0 ) {
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The ^M "
                                      "model will be masked on each iteration "
                                      "until the normalised change reaches %g "
                                      "using the bad pixels in NDF '^N'.",
                                      status, zero_niter );
                        } else if( zero_niter != VAL__BADD ) {
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
               } else if( mask_types[ imask ] == SNR ) {

/* Convert the high pass filter size from arc-seconds to pixels, and
   round to the nearest integer. */
                  hipass = (int)( 0.5 + zero_snr_hipass/dat->pixsize );

/* If required, form the mask using an ffclean-like algorithm. */
                  if( zero_snr_ffclean ) {
                     if( hipass > 1 ) {
                        msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                   "will be masked using an FFCLEAN algorithm "
                                   "with box=%d and thresh=%g.", status, modname,
                                   hipass, zero_snr );
                        msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The SNR "
                                   "map will first be smoothed using a box "
                                   "filter of %d pixels.", status,
                                   zero_snr_lopass );

                        mapuse = smf_ffclean( wf, dat->map, dat->mapvar,
                                              dat->mdims, hipass,
                                              zero_snr_lopass, zero_snr, 0,
                                              abssnr, NULL, status );
                        if( *status == SAI__OK ) {
                           pd = mapuse;
                           pn = newmask;
                           for( i = 0; i < dat->msize; i++ ) {
                              *(pn++) = ( *(pd++) != VAL__BADD ) ? 1 : 0;
                           }
                        }

                        mapuse = astFree( mapuse );

                     } else if( *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        errRepf( "", "smf_get_mask: Non-zero value supplied for"
                                 " %s.ZERO_SNR_FFCLEAN but zero supplied for "
                                 "%s.ZERO_SNR_HIPASS.", status, modname, modname );
                     }

/* Otherwise, form the mask by thresholding the map at the requested SNR
   level, possible after removal of low frequency structures. */
                  } else {

/* Form the SNR map. */
                     double *snrmap = astMalloc( dat->msize*sizeof( *snrmap ) );
                     if( *status == SAI__OK ) {
                        double *ps = snrmap;
                        pd = dat->map;
                        pv = dat->mapvar;

                        for( i = 0; i < dat->msize; i++,pd++,pv++ ) {
                           *(ps++) = ( *pd != VAL__BADD && *pv != VAL__BADD &&
                                       *pv >= 0.0 ) ? (*pd)/sqrt( *pv ) : VAL__BADD;
                        }

/* If required, smooth the SNR map to remove high spatial frequencies (noise,
   etc). */
                        if( zero_snr_lopass > 0 ) {
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: Smoothing "
                                      "the SNR map using a %d pixel box filter prior "
                                      "to forming the %s mask.", status,
                                      zero_snr_lopass, modname );
                           mapuset = smf_tophat2( wf, snrmap, dat->mdims,
                                                  zero_snr_lopass, 0, 1.0E-6,
                                                  1, status );
                        } else {
                           mapuset = snrmap;
                        }

/* If required, subtract off a smoothed background from the SNR map. */
                        if( hipass > 0 ) {
                           msgOutiff( MSG__DEBUG, " ", "smf_get_mask: Removing "
                                      "background from SNR map using a %g arc-sec "
                                      "box filter prior to forming the %s mask.",
                                      status, zero_snr_hipass, modname );
                           mapuse = smf_tophat2( wf, mapuset, dat->mdims,
                                                 hipass, 1, 1.0E-6, 1, status );
                        } else {
                           mapuse = mapuset;
                        }

/* Get the lower SNR limit. */
                        zero_snrlo = VAL__BADD;
                        if( astMapGet0D( subkm, "ZERO_SNRLO", &zero_snrlo ) ) {
                           if( zero_snrlo > zero_snr && *status == SAI__OK ) {
                              *status = SAI__ERROR;
                              errRepf( " ", "Bad value for config parameter "
                                       "%s.ZERO_SNRLO (%g) - it must not be higher "
                                       "than %s.ZERO_SNR (%g).", status, modname,
                                       zero_snrlo, modname, zero_snr );
                           }
                        }

/* If no lower SNR limit was supplied, just do a simple
   threshold on the SNR values to get the mask. In cases where the signal
   can be positive or negative (e.g. when masking Q/U values or using
   the SSNmodel), treat large negative SNR values in the same way as
   large positive SNR values.  */
                        if( zero_snrlo == VAL__BADD ) {
                           pd = mapuse;
                           pn = newmask;

                           if( abssnr ) {

                              if( accmask ) {
                                 pa = accmask;
                                 for( i = 0; i < dat->msize; i++,pd++ ) {
                                    if( *(pa++) == 0 ) {
                                       *(pn++) = 0;
                                    } else {
                                       *(pn++) = ( *pd != VAL__BADD &&
                                                   fabs( *pd ) < zero_snr ) ? 1 : 0;
                                    }
                                 }

                              } else {
                                 for( i = 0; i < dat->msize; i++,pd++ ) {
                                    *(pn++) = ( *pd != VAL__BADD &&
                                                fabs( *pd ) < zero_snr ) ? 1 : 0;
                                 }
                              }

                           } else {

                              if( accmask ) {
                                 pa = accmask;
                                 for( i = 0; i < dat->msize; i++,pd++ ) {
                                    if( *(pa++) == 0 ) {
                                       *(pn++) = 0;
                                    } else {
                                       *(pn++) = ( *pd != VAL__BADD &&
                                                   *pd < zero_snr ) ? 1 : 0;
                                    }
                                 }

                              } else {
                                 for( i = 0; i < dat->msize; i++,pd++ ) {
                                    *(pn++) = ( *pd != VAL__BADD &&
                                                *pd < zero_snr ) ? 1 : 0;
                                 }
                              }
                           }

/* Report masking info. */
                           if( !have_mask ) {
                              if( zero_niter <= 0 ) {
                                 sprintf( words, "on each iteration" );
                              } else if( zero_niter < 1.0 ) {
                                 sprintf( words, "until normalised changes reaches %g",
                                          zero_niter );
                              } else if( zero_niter != VAL__BADD ) {
                                 sprintf( words, "for %d iterations", (int)( zero_niter + 0.5 ) );
                              } else {
                                 words[0] = 0;
                              }
                              msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                         "will be masked %s using an SNR limit of %g.",
                                         status, modname, words, zero_snr );
                           }

/* If a lower SNR limit was supplied, create an initial
   mask by thresholding at the ZERO_SNR value, and then extend the source
   areas within the mask down to an SNR limit of ZERO_SNRLO. */
                        } else {
                           smf_snrmask( wf, abssnr, accmask, mapuse, NULL,
                                        dat->mdims, zero_snr, zero_snrlo,
                                        newmask, status );

/* Report masking info. */
                           if( !have_mask ) {
                              if( zero_niter <= 0 ) {
                                 sprintf( words, "on each iteration" );
                              } else if( zero_niter < 1.0 ) {
                                 sprintf( words, "until normalised changes reaches %g",
                                          zero_niter );
                              } else if( zero_niter != VAL__BADD ) {
                                 sprintf( words, "for %d iterations", (int)( zero_niter + 0.5 ));
                              } else {
                                 words[0] = 0;
                              }
                              msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                         "will be masked %s using an SNR limit of %g "
                                         "extended down to %g.", status, modname,
                                         words, zero_snr, zero_snrlo );
                           }
                        }

                        if( hipass > 0 ) mapuse = astFree( mapuse );
                        snrmap = astFree( snrmap );
                     }
                  }

/* Predefined masking... */
               } else if( mask_types[ imask ] == PREDEFINED ) {

/* If we had a mask on entry, then there is no need to create a new one
   since it will not have changed. But we need to recalculate the
   mask if are combining it with any non-static masks. */
                  if( ! have_mask || ! isstatic ) {

/* Find bad pixels in the predefined array and set those pixels to 1 in
   the mask. */
                     pn = newmask;
                     for( i = 0; i < dat->msize; i++ ) {
                        *(pn++) = ( *(predef++) == VAL__BADD ) ? 1 : 0;
                     }

/* Report masking info. */
                     if( zero_niter <= 0 ) {
                        sprintf( words, "on each iteration" );
                     } else if( zero_niter < 1.0 ) {
                        sprintf( words, "until normalised changes reaches %g",
                                 zero_niter );
                     } else if( zero_niter != VAL__BADD ) {
                        sprintf( words, "for %d iterations", (int)( zero_niter + 0.5 ) );
                     } else {
                        words[0] = 0;
                     }
                     msgOutiff( MSG__DEBUG, " ", "smf_get_mask: The %s model "
                                "will be masked %s using a smoothed form of "
                                "the final mask created with the previous map.",
                                status, modname, words );
                  }
               }

/* If required, add the new mask into the returned mask. If this is the
   first mask, we just copy the new mask to form the returned mask.
   Otherwise, we combine it with the existing returned mask. */
               if( ! have_mask || ! isstatic ) {
                  if( nmask > 1 ) {
                     if( imask == 0 ) {
                        memcpy( *mask, newmask, dat->msize*sizeof(*newmask));
                     } else {
                        pm = *mask;
                        pn = newmask;
                        if( munion ) {
                           for( i = 0; i < dat->msize; i++,pm++ ) {
                              if( *(pn++) == 0 ) *pm = 0;
                           }
                        } else {
                           for( i = 0; i < dat->msize; i++,pm++ ) {
                              if( *(pn++) == 1 ) *pm = 1;
                           }
                        }
                     }
                  }
               }
            }

/* Free the individual mask work array if it was used. */
            if( nmask > 1 ) newmask = astFree( newmask );

/* Check that the mask has some source pixels (i.e. pixels that have non-bad data values -
   we do not also check variance values since they are not available until the second
   iteration). We can make a map even if the mask has no source pixels at
   all, so make this a warning rather than an error. */
            if( *status == SAI__OK ) {
               nsource = 0;
               pm = *mask;
               pd = dat->map;
               for( i = 0; i < dat->msize; i++,pd++,pv++,pm++ ) {
                  if( *pd != VAL__BADD && *pm == 0 ) nsource++;
               }
               if( nsource < 5 ) {
                  msgOutf( "", "WARNING: The %s mask being used has fewer "
                           "than 5 source pixels.", status, modname );
                  if( zero_snr > 0.0 ) {
                     msgOutf( "", "Maybe your zero_snr value (%g) is too high?",
                              status, zero_snr );
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

