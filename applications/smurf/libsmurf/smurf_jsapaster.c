/*
*+
*  Name:
*     JSAPASTER

*  Purpose:
*     Paste several JSA tiles into a single mosaic.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_jsapaster( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates a single output NDF by pasting together a supplied
*     list of 2D- or 3D- JSA tiles (i.e. it is the inverse of JSADICER).
*     The spatial WCS of each input NDF must matches the JSA all-sky pixel
*     grid. The output NDF will usually be gridded using an HPX projection,
*     but an XPH projection will be used if the mosaic would cross a
*     discontinuity in the HPX projection.

*  ADAM Parameters:
*     IN = NDF (Read)
*        A group of input JSA tiles.
*     INSTRUMENT = LITERAL (Read)
*        The JCMT instrument (different instruments have different
*        tiling schemes and pixel sizes). The following instrument
*        names are recognised (unambiguous abbreviations may be
*        supplied): "SCUBA-2(450)", "SCUBA-2(850)", "ACSIS", "DAS". The
*        dynamic default is determined from the input NDF if possible.
*        If this cannot be done, then no dynamic default is provided,
*        and the user is prompted for a value if none was supplied on
*        the command line. []
*     OUT = NDF (Write)
*        The mosaic.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2014 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
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
*     MA 02110-1301, USA

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
#include <stdlib.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "par.h"
#include "sae_par.h"
#include "kpg_err.h"
#include "star/grp.h"
#include "star/atl.h"
#include "star/kaplibs.h"
#include "star/ndg.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"


/* Local constants */
#define FUNC_NAME "smurf_jsapaster"

/* Prototypes for local functions. */
static void smf1_jsapaster( int nndf, Grp *grp1, int *idlist, int indf_out,
                            Grp *grp2, int *status );

/* Main entry */
void smurf_jsapaster( int *status ) {

/* Local Variables */
   AstFitsChan *fc = NULL;
   AstFrame *sfrm;
   AstFrameSet *fs;
   AstFrameSet *iwcs2;
   AstFrameSet *iwcs2_2d;
   AstFrameSet *iwcs;
   AstFrameSet *refwcs;
   AstFrameSet *refwcs_hpx;
   AstFrameSet *refwcs_hpx12;
   AstFrameSet *refwcs_xphs;
   AstFrameSet *refwcs_xphn;
   AstKeyMap *km;
   AstMapping *smap;
   Grp *igrp = NULL;
   Grp *igrp2 = NULL;
   HDSLoc *xloc = NULL;
   char *buf;
   char ndfname[ GRP__SZNAM ];
   const char *key;
   int *lbnd;
   int *ndflist;
   int *temp;
   int *ubnd;
   int ax_spec;
   int dims[ NDF__MXDIM ];
   int dims_in[ NDF__MXDIM ];
   int dims_out[ NDF__MXDIM ];
   int idim;
   int ikey;
   int indf2;
   int indf3;
   int indf;
   int isky;
   int lbnd_hpx[ NDF__MXDIM ];
   int lbnd_hpx12[ NDF__MXDIM ];
   int lbnd_ref[ 2 ];
   int lbnd_xphn[ NDF__MXDIM ];
   int lbnd_xphs[ NDF__MXDIM ];
   int ndim;
   int nkey;
   int nndf;
   int place;
   int ref_tile;
   int there;
   int ubnd_hpx12[ NDF__MXDIM ];
   int ubnd_hpx[ NDF__MXDIM ];
   int ubnd_ref[ 2 ];
   int ubnd_xphn[ NDF__MXDIM ];
   int ubnd_xphs[ NDF__MXDIM ];
   smf_jsaproj_t proj;
   size_t index;
   size_t npix;
   size_t npix_hpx;
   size_t npix_hpx12;
   size_t npix_xphn;
   size_t npix_xphs;
   size_t size;
   size_t size2;
   smfJSATiling tiling;

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Begin AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Get group of input tiles. */
   kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

/* Get an NDF identifier for the first NDF. */
   ndgNdfas( igrp, 1, "READ", &indf, status );

/* Get the WCS FrameSet from the NDF. */
   kpg1Gtwcs( indf, &iwcs, status );

/* Identify the spectral axis - if any - and get a FrameSet
   connecting the spectral pixel axis to the spectral WCS axis.
   Check for basic SpecFrames (SPECTRUM) and DSBSPecFrames
   (DSBSPECTRUM). */
   fs = atlFrameSetSplit( iwcs, "DSBSPECTRUM", &temp, NULL, status );
   if( !fs ) fs = atlFrameSetSplit( iwcs, "SPECTRUM", &temp, NULL, status );

/* Extract the spectral Frame and the GRID->SPECTRUM Mapping from the
   FrameSet, record the one-based index of the spectral pixel axis, and
   free the array returned by atlFrameSetSplit. */
   if( fs ) {
      sfrm = astGetFrame( fs, AST__CURRENT );
      smap = astGetMapping( fs, AST__BASE, AST__CURRENT );
      ax_spec = temp[ 0 ];
      temp = astFree( temp );
   } else {
      sfrm = NULL;
      smap = NULL;
      ax_spec = 0;
   }

/* Get a FitsChan holding the contents of the FITS extension from the
   first input NDF. */
   kpgGtfts( indf, &fc, status );

/* Get the JSA tile number from the FITS extension. */
   if( !astGetFitsI( fc, "TILENUM", &ref_tile ) && *status == SAI__OK ){
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( "", "No JSA tile number found in the FITS extension of NDF "
              "'^N'.", status );
   }

/* Find the number of pixel axes in the NDF. */
   ndfDim( indf, NDF__MXDIM, dims_in, &ndim, status );

/* Select a JSA instrument and get the parameters defining the layout of
   tiles for the selected instrument. */
   smf_jsainstrument( "INSTRUMENT", fc, SMF__INST_NONE, &tiling,
                      status );

/* Get the spatial WCS for the reference tile in HPX projection. */
   smf_jsatile( ref_tile, &tiling, 0, SMF__JSA_HPX, NULL, &refwcs_hpx,
                NULL, lbnd_ref, ubnd_ref, status );

/* Make the PIXEL Frame the base Frame. */
   astSetC( refwcs_hpx, "Base", "PIXEL" );

/* Get the bounds of the mosaic if an HPX projection is used for the
   output mosaic. */
   smf_projbox( igrp, refwcs_hpx, lbnd_hpx, ubnd_hpx, dims, status );

/* Find the total number of pixels in the HPX mosaic. */
   npix_hpx = 1;
   for( idim = 0; idim < ndim; idim++ ) npix_hpx *= dims[ idim ];

/* Now do the same for HPX12 projection (like HPX but centred on RA=12h). */
   smf_jsatile( ref_tile, &tiling, 0, SMF__JSA_HPX12, NULL, &refwcs_hpx12,
                NULL, lbnd_ref, ubnd_ref, status );
   astSetC( refwcs_hpx12, "Base", "PIXEL" );
   smf_projbox( igrp, refwcs_hpx12, lbnd_hpx12, ubnd_hpx12, dims, status );
   npix_hpx12 = 1;
   for( idim = 0; idim < ndim; idim++ ) npix_hpx12 *= dims[ idim ];

/* Now do the same for XPH (north) projection. */
   smf_jsatile( ref_tile, &tiling, 0, SMF__JSA_XPHN, NULL, &refwcs_xphn,
                NULL, lbnd_ref, ubnd_ref, status );
   astSetC( refwcs_xphn, "Base", "PIXEL" );
   smf_projbox( igrp, refwcs_xphn, lbnd_xphn, ubnd_xphn, dims, status );
   npix_xphn = 1;
   for( idim = 0; idim < ndim; idim++ ) npix_xphn *= dims[ idim ];

/* Now do the same for XPH (south) projection. */
   smf_jsatile( ref_tile, &tiling, 0, SMF__JSA_XPHS, NULL, &refwcs_xphs,
                NULL, lbnd_ref, ubnd_ref, status );
   astSetC( refwcs_xphs, "Base", "PIXEL" );
   smf_projbox( igrp, refwcs_xphs, lbnd_xphs, ubnd_xphs, dims, status );
   npix_xphs = 1;
   for( idim = 0; idim < ndim; idim++ ) npix_xphs *= dims[ idim ];

/* Now find the projection which gives the smallest mosaic. */
   proj = SMF__JSA_HPX;
   npix = npix_hpx;
   lbnd = lbnd_hpx;
   ubnd = ubnd_hpx;
   refwcs = refwcs_hpx;

   if( npix_hpx12 < npix ) {
      proj = SMF__JSA_HPX12;
      npix = npix_hpx12;
      lbnd = lbnd_hpx12;
      ubnd = ubnd_hpx12;
      refwcs = refwcs_hpx12;
   }

   if( npix_xphn < npix ) {
      proj = SMF__JSA_XPHN;
      npix = npix_xphn;
      lbnd = lbnd_xphn;
      ubnd = ubnd_xphn;
      refwcs = refwcs_xphn;
   }

   if( npix_xphs < npix ) {
      proj = SMF__JSA_XPHS;
      npix = npix_xphs;
      lbnd = lbnd_xphs;
      ubnd = ubnd_xphs;
      refwcs = refwcs_xphs;
   }

/* Warn the user if the output will use an XPH projection. */
   if( proj != SMF__JSA_HPX ) {
      if( proj != SMF__JSA_HPX12 ) {
         msgOutf( "", "   The output mosaic will use an HPX projection "
                  "centred on RA = 12h.", status );
      } else {
         msgOutf( "", "   The output mosaic will use an XPH projection "
                  "centred on the %s pole..", status,
                  ( proj == SMF__JSA_XPHN ) ? "north" : "south" );
      }
   }

/* Make GRID coords the base Frame in the reference WCS. */
   astSetC( refwcs, "Base", "GRID" );

/* If a spectral axis exists in the input, add a spectral axis into
   "refwcs". At the moment we only handle cases where the spectral axis
   is axis 3 - we could change this to be more general if required, but
   there is probably no need since JSA always has axis 3 as the spectral
   axis. */
   if( ax_spec == 3 ) {
      atlAddWcsAxis(  refwcs, smap, sfrm, lbnd + 2, ubnd + 2, status );

   } else if( ax_spec > 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "The WCS axes in NDF '^N' are not in the expected JSA "
              "order.",  status );
   }

/* Form the output NDF initially by propagating the first input NDF, in
   order to retain data type, extensions, etc. Do not copy provenance as
   we will be handling provenance by hand. */
   ndfProp( indf, "Units,NoExtension(Provenance)", "OUT", &indf2, status );

/* Remove the OUTLINE  extension from the output NDF since the outline of
   the mosaic will not be the same as the outline of the first input NDF. */
   ndfXdel( indf2, "OUTLINE", status );

/* Modify the output pixel bounds to the required size. */
   ndfSbnd( ndim, lbnd, ubnd, indf2, status );

/* Get its default WCS FrameSet (the above call to ndfProp does
   not propagate WCS). */
   ndfGtwcs( indf2, &iwcs2, status );

/* Add in the reference WCS FrameSet by connecting the PIXEL Frame in the
   output NDF to the PIXEL Frame in the reference WCS using a UnitMap.
   astAddFrame connects the current Frame in the added-on FrameSet
   ("refwcs") to a specified Frame in the original FrameSet. So we need
   to change refwcs so that PIXEL is current, rather than sky. First, we
   need to calculate the index that the Sky frame will have within the
   modified FrameSet. Its original index within refwcs will be shifted
   by the number of Frames originally in "iwcs2". */
   isky = astGetI( iwcs2, "NFrame" ) + astGetI( refwcs, "Current" );
   astSetC( refwcs, "Current", "PIXEL" );
   astAddFrame( iwcs2, 2, astUnitMap( ndim, " " ), refwcs );

/* Re-instate the SKY Frame as the current Frame in the modified
   FrameSet. */
   astSetI( iwcs2, "Current", isky );

/* Store the modified WCS FrameSet in the output NDF. */
   ndfPtwcs( iwcs2, indf2, status );

/* Create a 2D version of the output WCS containing just the spatial
   axes for use by some extension NDFs. */
   iwcs2_2d = atlFrameSetSplit( iwcs2, "SKY", NULL, NULL, status );

/* Allocate an array to receive identifiers for all the input NDFs. */
   ndflist = astMalloc( size*sizeof( *ndflist ) );

/* Create group to receive paths to all NDFs found within the SMURF
   extension of any input NDF. */
   igrp2 = grpNew( " ", status );

/* Paste the input NDFs into the output NDF, and return the identifiers
   for the input NDFs and the group of extension NDFs. */
   smf1_jsapaster( size, igrp, ndflist, indf2,  igrp2, status );

/* Modify the output provenance information to record each input NDF as
   a direct parent of the output NDF. */
   ndgAddProv( indf2, "SMURF:JSAPASTER", size, ndflist, 0, status );

/* Now we need to mosaic any NDFs stored within the SMURF extension of
   each input NDF. */
   size2 = grpGrpsz( igrp2, status );
   if( size2 > 0 ) {

/* Get a locator for the output SMURF extension. */
      ndfXloc( indf2, SMURF__EXTNAME, "UPDATE", &xloc, status );

/* Create a KeyMap in which to store the identifiers for the input
   extension NDFs. */
      km = astKeyMap( " " );

/* For each extension NDF, extract the part of the path that follows
   ".more.smurf." (i.e. the NDF name ), get an NDF identifier for it and
   store the identifier in a KeyMap using the NDF name as the key (each
   KeyMap entry is a vector of NDF identifiers). */
      for( index = 0; index < size2 && *status == SAI__OK; index++ ) {
         ndgNdfas( igrp2, index + 1, "Read", &indf, status );
         buf = ndfname;
         grpGet( igrp2, index + 1, 1, &buf, GRP__SZNAM, status );
         key = strstr( ndfname, ".MORE.SMURF." );
         if( key ) {
            key += strlen( ".MORE.SMURF." );
            astMapPutElemI( km, key, -1, indf );
         }
      }

/* Loop round each entry in the KeyMap (i.e. each distinct extension
   NDF name). */
      nkey = astMapSize( km );
      for( ikey = 0; ikey < nkey && *status == SAI__OK; ikey++ ) {
         key = astMapKey( km, ikey );

/* Get the list of input NDF identifiers for this extension NDF. */
         astMapGet1I( km, key, size, &nndf, ndflist );

/* Erase any existing NDF with this name in the output SMURF extension. */
         datThere(  xloc, key, &there, status );
         if( there ) datErase( xloc, key, status );

/* Create the output extension NDF by propagation from the first input
   extension NDF. */
         ndfPlace( xloc, key, &place, status );
         ndfScopy( ndflist[ 0 ], "Units,NoExtension(Provenance)", &place,
                   &indf3, status );

/* Get the number of axes in the extension NDF. */
         ndfDim( indf3, NDF__MXDIM, dims_out, &ndim, status );

/* Modify its pixel bounds to the required size. */
         ndfSbnd( ndim, lbnd, ubnd, indf3, status );

/* Store the 2D or 3D WCS FrameSet in it. */
         if( ndim == 3 ) {
            ndfPtwcs( iwcs2, indf3, status );
         } else if( ndim == 2 ) {
            ndfPtwcs( iwcs2_2d, indf3, status );
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "SMURF extension NDF '%s' is neither 2D nor "
                     "3D.", status, key );
         }

/* Paste the input extension NDFs into the output extension NDF. */
         smf1_jsapaster( nndf, NULL, ndflist, indf3, NULL, status );

/* Close the output extension NDF. */
         ndfAnnul( &indf3, status );
      }

      datAnnul( &xloc, status );
   }

/* Free resources. */
   ndflist = astFree( ndflist );
   grpDelet( &igrp, status );
   if( igrp2 ) grpDelet( &igrp2, status );

/* End the NDF and AST context. */
   ndfEnd( status );
   astEnd;

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "JSAPASTER failed.",
                                      status );
}






static void smf1_jsapaster( int nndf, Grp *grp1, int *idlist, int indf_out,
                            Grp *grp2, int *status ){
/*
*  Name:
*     smf1_jsapaster

*  Purpose:
*     Paste a group of input JSA tiles into an output mosaic.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf1_jsapaster( int nndf, Grp *grp1, int *idlist, int indf_out,
*                          Grp *grp2, int *status )

*  Arguments:
*     nndf = int (Given)
*        The number of inpit NDFs.
*     grp1 = Grp * (Given)
*        A group containin the paths to the input NDFs. This may be NULL,
*        in which case the NDF identifiers supplied in "idlist" are used
*        to access the input NDFs.
*     idlist = int * (Given and Returned)
*        If "grp1" is NULL, then "idlist" should be supplied holding a
*        list of identifiers for the input NDFs. If "grp1" is not null,
*        then "idlist" is returned holding the NDF idntifiers for the
*        input NDFs.
*     indf_out = int (Given)
*        NDF identifier for the output mosac.
*     grp2 = Grp * (Given & Returned)
*        If not NULL, then "grp2" is returned holding the paths to all
*        NDFs found within the SMURF extension of any of the input NDFs.
*     status = int * (Given)
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrameSet *fs;
   AstFrameSet *wcs_in;
   AstFrameSet *wcs_out;
   AstMapping *g2gmap;
   char *buf;
   char *dom;
   char dtype[ NDF__SZFTP + 1 ];
   char gexp[ GRP__SZNAM + 15 ];
   char ndfname[ GRP__SZNAM ];
   char type[ NDF__SZTYP + 1 ];
   const char *proj;
   double *weightsq = NULL;
   double *weights = NULL;
   int dims_in[ NDF__MXDIM ];
   int dims_out[ NDF__MXDIM ];
   int flags;
   int index;
   int indf_in;
   int nc;
   int ndgflag;
   int ndim;
   int ndim_in;
   int nel;
   int origin[ 3 ] = { 1, 1, 1 };
   int qual;
   int var;
   int64_t nusedq;
   int64_t nused;
   size_t size2;
   size_t size_out;
   void *ipd_in;
   void *ipd_out;
   void *ipq_in;
   void *ipq_out;
   void *ipv_in;
   void *ipv_out;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Decide on the output data type to use: _REAL or _DOUBLE. */
   ndfMtype( "_REAL,_DOUBLE", indf_out, indf_out, "Data", type,
             sizeof(type), dtype, sizeof(dtype), status );

/* Get the dimensions of the output NDF. */
   ndfDim( indf_out, NDF__MXDIM, dims_out, &ndim, status );

/* Since the ndfMap "nel" argument is an int, we cannot use it, since
   the number of pixels in some HARP cubes may be larger than an int can
   hold. So calculate the number of pixels in the output from the
   dimensions. */
   size_out = 1;
   for( index = 0; index < ndim; index++ ) size_out *= dims_out[ index ];

/* Get the WCS FrameSet from the output NDF. */
   kpg1Gtwcs( indf_out, &wcs_out, status );

/* Note the Domain of the current Frame (should be one of "SKY",
   "SKY-SPECTRUM" or "SKY-DSBSPECTRUM" ). Take a copy of the string in
   case the contents of the buffer within AST get changed. */
   dom = NULL;
   nc = 0;
   dom = astAppendString( NULL, &nc, astGetC( wcs_out, "Domain" ) );

/* Invert the output WCS FrameSet so that the current Frame is GRID
   coords in the output NDF and the base Frame is SKY coords. We do this
   as preparation for calling astConvert below, so that astConvert will
   return the mapping from input GRID coords to output GRID coords. */
   astInvert( wcs_out );

/* Now loop round each input NDF. */
   for( index = 0; index < nndf && *status == SAI__OK; index++ ) {

/* Start another AST context for this specific input NDF. */
      astBegin;

/* Get an NDF identifier for the current NDF. If "grp1" was supplied, use
   it and return the NDF identifier in "idlist". Otherwise, use the NDF
   identifiers supplied in "idlist". */
      if( grp1 ) {
         ndgNdfas( grp1, index + 1, "Read", &indf_in, status );
         idlist[ index ] = indf_in;
      } else {
         indf_in = idlist[ index ];
      }

/* Get the dimensions of the input NDF. */
      ndfDim( indf_in, NDF__MXDIM, dims_in, &ndim_in, status );

/* Report an error if it has a different number of axes to the output
   NDF. */
      if( ndim_in != ndim && *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf_in );
         msgSeti( "A", ndim_in );
         msgSeti( "B", ndim );
         errRep( " ", "Input NDF '^N' has ^A pixel axes - should be ^B.",
                 status );
      }

/* Get the WCS FrameSet from the NDF. */
      kpg1Gtwcs( indf_in, &wcs_in, status );

/* Report an error if the projection is not some form of HEALPix projection. */
      proj = astGetC( wcs_in, "Projection" );
      if( proj && !strstr( proj, "HEALPix" ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf_in );
         errRep( " ", "Input NDF '^N' is not gridded on the JSA all-sky "
                 "pixel grid.", status );
      }

/* To re-bin the input data onto the output grid, we need the Mapping
   from input GRID to output GRID coords. astConvert returns the Mapping
   between current Frames, so first invert the input WCS FrameSet so that
   its GRID Frame is its current Frame. */
      astInvert( wcs_in );
      fs = astConvert( wcs_in, wcs_out, dom );

/* Report an error if the input NDF could not be aligned with the output
   NDF. */
      if( !fs && *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf_in );
         errRep( " ", "Input NDF '^N' could not be aligned with the "
                 "output NDF.", status );
      }

/* Get the Mapping. */
      g2gmap = astSimplify( astGetMapping( fs, AST__BASE, AST__CURRENT ) );

/* If this is the first input NDF, we use it to decide which array
   components are to be created in the output NDF. Map the selected
   components in the output NDF. */
      if( index == 0 ) {
         ndfState( indf_in, "Variance", &var, status );
         ndfState( indf_in, "Quality", &qual, status );

         ndfMap( indf_out, "Data", type, "Write/bad", &ipd_out, &nel,
                 status );
         if( var ) ndfMap( indf_out, "Variance", type, "Write/bad",
                           &ipv_out, &nel, status );
         if( qual ) ndfMap( indf_out, "Quality", "_UBYTE", "Write/zero",
                            &ipq_out, &nel, status );

/* Allocate memory for the weights array used by astRebinSeq. */
         weights = astMalloc( size_out*sizeof( *weights ) );
         weightsq = astMalloc( size_out*sizeof( *weightsq ) );
      }

/* Set the astRebinSeq flags for this input NDF. */
      flags = AST__USEBAD;
      if( var ) flags|= AST__USEVAR;
      if( index == 0 ) flags |= AST__REBININIT;
      if( index == nndf - 1 ) flags |= AST__REBINEND;

/* Map the data and variance arrays in the input NDF. */
      ndfMap( indf_in, "Data", type, "Read", &ipd_in, &nel, status );
      if( var ) ndfMap( indf_in, "Variance", type, "Read", &ipv_in,
                        &nel, status );

/* Rebin them into the output NDF. */
      if( !strcmp( type, "_REAL" ) ) {
         astRebinSeqF( g2gmap, 0.0, ndim, origin, dims_in, (float *) ipd_in,
                       (float *) ipv_in, AST__NEAREST, NULL, flags, 0.1,
                       1000, VAL__BADR, ndim, origin, dims_out, origin,
                       dims_in, (float *) ipd_out, (float *) ipv_out,
                       weights, &nused );
      } else {
         astRebinSeqD( g2gmap, 0.0, ndim, origin, dims_in, (double *) ipd_in,
                       (double *) ipv_in, AST__NEAREST, NULL, flags, 0.1,
                       1000, VAL__BADD, ndim, origin, dims_out, origin,
                       dims_in, (double *) ipd_out, (double *) ipv_out,
                       weights, &nused );
      }

/* Now rebin the Quality array, if it exists in the input NDF. */
      if( qual ) {

/* Map the Quality array in the input NDF. */
         ndfMap( indf_in, "Quality", "_UBYTE", "Read", &ipq_in, &nel,
                 status );

/* Rebin it into the output NDF. Since there are no bad values or
   variances for quality, cancel the corresponding flags. */
         flags &= ~(AST__USEBAD | AST__USEVAR);
         astRebinSeqUB( g2gmap, 0.0, ndim, origin, dims_in,
                        (unsigned char *) ipq_in, NULL, AST__NEAREST, NULL,
                        flags, 0.1, 1000, 0, ndim, origin, dims_out, origin,
                        dims_in, (unsigned char *) ipq_out, NULL, weightsq,
                        &nusedq );
      }

/* If "grp2" was supplied, find any NDFs stored within the SMURF extension
   of the input NDF, and store paths to them in group grp2. An error will
   be reported by ndgAsexp if none are found, which we annull. */
      if( grp1 && grp2 && *status == SAI__OK ) {
         buf = ndfname;
         grpGet( grp1, index + 1, 1, &buf, GRP__SZNAM, status );
         sprintf( gexp, "%s.more.smurf", ndfname );
         ndgAsexp( gexp, 0, NULL, &grp2, &size2, &ndgflag, status );
         if( *status != SAI__OK ) errAnnul( status );
      }

/* Unmap all array components of the input NDF. */
      ndfUnmap( indf_in, "*", status );

/* End the AST context, thus annulling all AST objects created within the
   context. */
      astBegin;
   }

/* Free resources. */
   weights = astFree( weights );
   weightsq = astFree( weightsq );
   dom = astFree( dom );

/* End the AST context. */
   astEnd;
}





