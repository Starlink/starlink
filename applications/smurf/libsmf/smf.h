/*
*+
*  Name:
*     smf.h

*  Purpose:
*     Prototypes for the libsmf library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "smf.h"

*  Description:
*     Prototypes used by the libsmf functions.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     2005-11-04 (AGG):
*        Add smf_open_file, smf_fits_rdhead and smf_fits_crchan
*     2005-11-07 (TIMJ):
*        Alphabetize.
*        Add smf_tslice_ast, add smf_fits_getI
*     2005-11-28 (TIMJ):
*        Add smf_close_file
*     2005-12-05 (AGG)
*        Add smf_flatfield and smf_check_flat
*     2005-12-06 (AGG)
*        Add smf_flatten
*     2005-12-09 (AGG)
*        Add smf_clone_data
*     2006-01-09 (AGG)
*        Add smf_tslice and smf_insert_tslice
*     2006-01-10 (AGG)
*        Add smf_scale_tau and smf_fits_getF
*     2006-01-24 (TIMJ):
*        Add smf_fits_getS
*     2006-01-24 (AGG):
*        Change floats to doubles in smf_correction_extintion, smf_scale_tau
*     2006-01-25 (AGG):
*        Add smf_dtype_check_fatal
*     2006-01-25 (TIMJ):
*        Add smf_malloc, smf_free.
*        Remove smf_fits_rdhead
*     2006-01-25 (TIMJ):
*        Add smf_create_*
*        Add smf_construct_*
*        Add smf_dtype_tostring
*     2006-01-27 (TIMJ):
*        Change API for smf_construct_smfFile, smf_construct_smfDA
*        and smf_construct_smfHead.
*     2006-02-02 (EC):
*        Add smf_mapbounds
*        Add smf_rebinmap
*     2006-02-03 (AGG):
*        Change API for smf_scale_tau, smf_correct_extinction
*     2006-02-17 (AGG):
*        Add smf_subtract_poly
*     2006-02-24 (AGG):
*        Add smf_subtract_plane
*     2006-03-23 (AGG):
*        Update API for smf_rebinmap, smf_construct_smfData, smf_construct_smfHead
*        Add smf_mapbounds approx, smf_deepcopy_smfHead & smf_deepcopy_smfData
*     2006-03-28 (AGG):
*        Update API for smf_deepcopy_smfData, add smf_deepcopy_smfDA
*     2006-03-30 (AGG):
*        Add smf_deepcopy_smfFile
*     2006-04-05 (AGG):
*        - Change API for smf_deepcopy_smfDA to accept a smfData
*          rather than smfDA
*        - Add smf_check_smfData, smf_check_smfDA, smf_check_smfFile and
*          smf_check_smfHead
*     2006-04-21 (AGG):
*        - Change API for smf_check_smfData, smf_deepcopy_smfData
*        - Add history to smf_construct_smfData
*        - Add smf_history_add, smf_history_read
*     2006-05-01 (EC):
*        - Add smf_mapcoordinates
*     2006-05-09 (AGG):
*        Add smf_get_xloc and smf_get_ndfid
*     2006-05-09 (EC):
*        Renamed smf_mapcoord to smf_calc_mapcoord
*     2006-07-07 (AGG):
*        Add smf_grp_related, smf_construct_smfGroup,
*        smf_open_related, smf_close_related and smf_close_smfGroup
*     2006-07-11 (EC):
*        Add smf_model_create, smf_model_getname
*     2006-07-26 (TIMJ):
*        Replace sc2head with JCMTState.
*     2006-07-28 (TIMJ):
*        Add tswcs argument to smf_construct_smfHead
*     2006-07-31 (TIMJ):
*        Add smf_inst_get
*     2006-08-02 (AGG):
*        Add smf_open_newfile and smf_open_ndf
*     2006-08-02 (TIMJ):
*        smf_open_newfile should take dim_t
*     2006-08-16 (EC):
*        changed interface for smf_model_getname
*     2006-09-06 (EC):
*        added smf_calc_telpos, smf_create_lutwcs and smf_telpos_get
*     2006-09-15 (AGG):
*        Added smf_dreamsolve, smf_create_smfDream,
*        smf_construct_smfDream, smf_close_smfDream, smd_dream_setjig,
*        smf_store_image
*     2006-09-15 (AGG):
*        Add smf_dream_getgrid, smf_dream_calcweights
*     2006-10-11 (AGG):
*        Update API for smf_open_newfile
*     2006-10-13 (JB):
*        Add smf_bbrebinmap
*     2006-10-26 (AGG):
*        Add smf_average_data, smf_calc_stareimage, update API to
*        smf_store_image
*     2006-11-2 (DSB):
*        Add smf_geod.
*     2006-11-3 (DSB):
*        Add smf_instap_get.
*     2006-11-20 (DSB):
*        Add smf_cubegrid and change smf_cubebounds.
*     2006-11-30 (DSB):
*        Add smf_sparsebounds.
*     2007-01-19 (AGG):
*        - Change API to smf_mapbounds: now returns the moving flag
*        - Change API to smf_rebinmap: takes the moving flag as an argument
*     2007-01-30 (AGG):
*        Update API to smf_mapbounds_approx to return moving flag
*     2007-02-06 (AGG):
*        Add tsys to smf_construct_smfHead
*     2007-02-12 (EC):
*        Add smf_model_getptr
*     2007-02-12 (DSB):
*        Add "hasoffexp" argument to smf_cubebounds.c
*     2007-02-23 (AGG):
*        Add instap to smf_construct_smfHead.c
*     2007-03-05 (EC):
*        Changed smf_correct_extinction interface
*        Add smf_model_gettype
*        Add smf_calcmodel_ext
*     2007-03-20 (TIMJ):
*        Add smf_fits_outhdr
*     2007-04-14 (DSB):
*        Add "int *nreject" to smf_rebincube.
*     2007-04-23 (DSB):
*        Big changes to smf_rebincube_xxx functions.
*     2007-04-23 (EC):
*        Add smf_terr
*     2007-06-13 (EC):
*        Add smf_open_file and smf_dtype_sz.c
*     2007-06-13 (EC):
*        Add smf_model_NDFexport
*     2007-06-22 (TIMJ):
*        Add string arg to smf_fits_add_prov
*     2007-07-05 (TIMJ):
*        Add smf_accumulate_prov.
*     2007-07-10 (EC):
*        -Add smf_open_related_model
*        -changed interface for smf_create_smfArray
*        -changed interface for smf_model_create
*        -changed interface for smf_calcmodel_*
*        -changed return value of smf_calcmodelptr
*     2007-07-12 (EC):
*        -Added moving to smf_bbrebinmap and smf_calc_mapcoord
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2007-07-16 (EC):
*        -Added copysubgroups to smf_construct_smfGroup
*     2007-08-09 (EC):
*        -Changed interface for smf_model_create
*     2007-08-17 (EC):
*        -Added nofile to smf_model_create interface
*     2007-09-13 (EC):
*        -Added smf_dataOrder
*     2007-09-13 (EC):
*        -Added smf_fft_filter and smf_concat_smfGroup
*     2007-10-12 (DSB):
*        -Added smf_choosepolbins, smf_freepolbins and smf_polext.
*     2007-10-19 (DSB):
*        -Added specunion to smf_cubebounds.
*     2007-10-25 (DSB):
*        -Added smf_checkdets.
*     2007-10-31 (EC):
*        -Added mode to smf_open_mapcoord interface
*     2007-11-8 (DSB):
*        -Added smf_sortd and smf_reorder<x>.
*     2007-11-15 (EC):
*        -Added projection information to interfaces of 
*         smf_iteratemap and smf_concat_smfGroup
*        -Added iarray to smf_model_create interface
*     2007-11-23 (DSB):
*        Added smf_updateprov.
*     2007-11-26 (DSB):
*        Added wcsout to smf_choosetiles argument list.
*     2007-11-27 (EC):
*        Added smf_fits_getL, smf_fits_setL
*     2007-12-054 (DSB):
*        Added trim to smf_choosetiles argument list.
*     2007-12-14 (EC):
*        Added flags to smf_calc_mapcoord interface.
*     2007-12-18 (DSB):
*        -Added smf_getrefwcs.
*        -Added specrefwcs and spacerefwcs to smf_cubebounds.
*     2007-12-18 (AGG):
*        New smf_free behaviour: now returns a NULL pointer if successful
*     2008-1-14 (DSB):
*        Added argument "border" to smf_choosetiles, and "trim" to
*        smf_reshapendf.
*     2008-1-15 (DSB):
*        Remove argument "trim" from smf_reshapendf.
*     2008-1-17 (DSB):
*        Added argument "alignsys" to smf_cubegrid.
*     2008-1-21 (DSB):
*        Added argument "polobs" to smf_cubebounds and smf_sparsebounds.
*     2008-01-22 (EC):
*        Added hitsmap to smf_simplerebinmap, smf_iteratemap
*     2008-01-25 (EC):
*        Added map projection information to smf_model_create interface
*     2008-2-1 (DSB):
*        Added smf_resampcube* functions.
*     2008-2-8 (EC):
*        Added smf_update_quality
*     2008-2-12 (DSB):
*        Changed interface to smf_choosepolbins, smf_rebincube*, and
*        smf_rebinsparse.
*     2008-02-12 (AGG):
*        Update API to smf_rebinmap, deprecate smf_bbrebinmap
*     2008-02-13 (AGG):
*        - smf_rebinmap: add parameters for pixel spreading scheme
*        - smf_get_spread: new routine to get parameters for chosen
*          pixel-spreading scheme
*     2008-02-26 (AGG):
*        Add rel parameter to smf_subtract_poly to get subtraction
*        relative to first time slice
*     2008-03-03 (EC):
*        Added target to smf_update_quality interface
*     2008-03-04 (EC):
*        Updated smf_calcmodel* routines to use smfDIMMData
*     2008-03-10 (AGG):
*        Add smf_create_qualname
*     2008-03-12 (EC):
*        - Updated smf_update_quality interface
*        - Added smf_correct_steps / smf_simple_stats
*     2008-03-14 (DSB):
*        - Added smf_sorti, smf_ext2km and smf_km2ext.
*     2008-03-25 (EC):
*        - Added syncbad to smf_update_quality interface
*     2008-03-27 (DSB):
*        - Added smf_getobsidss.
*     2008-03-28 (DSB):
*        - Added smf_calc_telres.
*     2008-03-31 (JB):
*        - Added smf_get_moltrans.
*     2008-03-31 (EC):
*        - Added smf_quick_noise
*     2008-04-02 (EC):
*        - Added smf_flag_spikes
*     2008-04-03 (EC):
*        - Added QUALITY to smf_simplerebinmap
*        - Added QUALITY to smf_scanfit, smf_fit_poly, smf_subtract_poly, 
*     2008-04-09 (TIMJ):
*        fix smf_created_qualname.
*        smf_create_lutwcs and smf_detpos_wcs no longer needs steptime argument
*     2008-04-14 (EC):
*        - Updated interface for smf_flag_spikes and smf_boxcar1
*     2008-04-16 (EC):
*        - Added optional external QUALITY and VARIANCE to smf_model_NDFexport
*        - Added chunk to smf_construct_smfGroup
*        - Added smf_simpleaddmap
*     2008-04-16 (AGG):
*        Add genvar to smf_rebinmap
*     2008-04-17 (EC):
*        Modified smf_grp_related interface
*     2008-04-18 (EC):
*        Modified smf_flag_spikes interface
*     2008-04-23 (EC):
*        -Added sampvar to smf_simplerebinmap
*        -Added hdr to smf_model_NDFexport
*     2008-04-24 (EC):
*        -Added smf_check_mapsize (then renamed smf_checkmem_map)
*        -Added maxmem to smf_iteratemap
*     2008-04-28 (AGG):
*        Add meansky parameter to smf_subtract_plane1/2
*     2008-04-28 (EC):
*        -Added maxconcatlen to smf_grp_related interface
*        -Added smf_checkmem_dimm
*     2008-04-28 (AGG):
*        Add smf_dump_smfData
*     2008-04-30 (TIMJ):
*        Add _clabels routines.
*     2008-05-01 (TIMJ):
*        Add check_units
*     2008-05-14 (EC):
*        -Added smf_get_projpar
*        -Modified smf_mapbounds interface to use par
*        -Moved NINT macro here from smf_cubegrid
*     2008-05-23 (TIMJ):
*        Add smf_get_taskname
*     2008-05-26 (EC):
*        Added is2d to smf_choosetiles
*     2008-05-28 (TIMJ):
*        Change smf_accumulate_prov
*     2008-06-03 (TIMJ):
*        Add smf_display_projpars
*     2008-06-04 (TIMJ):
*        smf_get_projpar new API.
*        Add smf_calc_skyframe
*     2008-06-05 (EC):
*        Removed is2d from smf_choosetiles
*     2008-06-06 (EC):
*        -Renamed smf_fft_filter to smf_filter_execute and changed interface
*        -Added smf_create_smfFilter, smf_filter_ident, smf_free_smfFilter
*     2008-06-10 (EC):
*        -Added smf_filter_r2c
*     2008-06-11 (EC):
*        -Added smf_filter_edge and smf_filter_notch
*        -Renamed smf_model_NDFexport to smf_NDFexport
*     2008-06-24 (EC):
*        Added padStart & padEnd to smf_concat_smfGroup
*     2008-07-03 (EC):
*        Switched to using dim_t in a number of function interfaces:
*        smf_calc_stats, smf_construct_smfGroup, smf_model_create,
*        smf_open_related, smf_simplerebinmap, smf_open_related_model,
*        smf_simpleaddmap
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2008 University of British Columbia.
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

#ifndef SMF_DEFINED
#define SMF_DEFINED

#include "ast.h"
#include "jcmt/state.h"  /* for inst_t */
#include "smurf_typ.h"
#include "smurf_par.h"
#include "star/grp.h"
#include "smf_typ.h"
#include "star/kaplibs.h"

/* Returns nearest integer to "x" */
#define NINT(x) ( ( (x) > 0 ) ? (int)( (x) + 0.5 ) : (int)( (x) - 0.5 ) )

void smf_addto_smfArray( smfArray *ary, smfData *data, int *status );

void smf_average_dataD( const smfData *data, int start, int nslice, 
		       const int interval, double **avdata, size_t *nelem,
                       int *status);

void smf_average_dataI( const smfData *data, int start, int nslice, 
		       const int interval, int **avdata, size_t *nelem,
                       int *status);

void smf_boxcar1 ( double *series, const size_t ninpts, size_t window, 
		   unsigned char *qual, unsigned char mask, 
		   int *status);

void smf_calc_stats( const smfData *data, const char *mode, const dim_t index,
                     dim_t lo, dim_t hi, double *mean, double *sigma, 
		     int *status);

double smf_calc_covar ( const smfData *data, const int i, const int j,
			int lo, int hi, int *status);

void smf_calc_mapcoord( smfData *data, AstFrameSet *outfset, int moving, 
			int *lbnd_out, int *ubnd_out, int flags, int *status );

void smf_calc_skyrot( smfData *data, double maxangle, int *nsamples, int *status );

void smf_calc_stareimage( smfData *data, const int naver, int *status);

void smf_calc_telpos( const double obsgeo[3], const char telName[],
                      double telpos[3],
		      int *status );

double smf_calc_wvm( const smfHead *hdr, int *status );

void smf_check_flat ( const smfData *data, int *status );

void smf_check_smfData ( const smfData *idata, smfData *odata, const int flags,
			 int *status );

void smf_check_smfDA ( const smfData *idata, smfData *odata, int *status );

void smf_check_smfFile ( const smfData *idata, smfData *odata, int *status );

void smf_check_smfHead ( const smfData *idata, smfData *odata, int *status );

void smf_clone_data ( const smfData *idata, smfData **odata, int *status );

void smf_close_file( smfData **, int *status);

void smf_close_mapcoord( smfData *data, int *status );

void smf_close_related( smfArray **relfiles, int *status );

void smf_close_smfDream( smfDream **dream, int * status );

void smf_close_smfGroup( smfGroup **group, int *status );

void smf_correct_extinction( smfData *data, const char *method, 
			     const int quick, double tau, 
			     double *allextcorr, int *status);

smfDA *
smf_construct_smfDA( smfDA * tofill, int *dksquid, double * flatcal,
		     double * flatpar, const char * flatname, int nflat,
		     int * status );
smfData *
smf_construct_smfData( smfData * tofill, smfFile * file, smfHead * hdr, 
		       smfDA * da, smf_dtype dtype, void * pntr[3], 
                       int isTordered, const dim_t dims[], int ndims,
		       int virtual, int ncoeff, double *poly, 
		       AstKeyMap *history, int * status );

smfDream *
smf_construct_smfDream( smfData *data, size_t nvert, size_t nsampcycle, 
			const int *jigvert, const double *jigpath, int * status );

smfFile *
smf_construct_smfFile(smfFile * tofill, int ndfid, int isSc2store,
		      int isTstream, const char * name,
		      int * status );
smfHead *
smf_construct_smfHead( smfHead * tofill, inst_t instrument,
		       AstFrameSet * wcs, AstFrameSet * tswcs,
                       AstFitsChan * fitshdr,
		       JCMTState * allState, dim_t curframe, 
		       const double instap[], dim_t nframes,
           smf_obsmode obsmode, smf_obstype obstype, unsigned int ndet,
		       double fplanex[], double fplaney[],
		       double detpos[], char *detname, int rpazel, 
		       double tsys[], const char title[], 
		       const char dlabel[], const char units[], 
                       const double telpos[], int * status );

smfGroup * 
smf_construct_smfGroup( Grp *igrp, dim_t **subgroups, size_t *chunk,
			const dim_t ngroups, 
                        const dim_t nrelated, const int copysubgroups, 
			int *status );

void smf_create_lutwcs( int clearcache, const double *fplane_x, 
			const double *fplane_y, const int n_pix, 
			const JCMTState *state, const double instap[2], 
                        const double telpos[3],
                        AstFrameSet **fset, int *status );

smfArray *smf_create_smfArray( int *status );

smfDA * smf_create_smfDA( int * status );

smfData* smf_create_smfData( int flags, int * status );

smfDream *smf_create_smfDream( int * status );

smfFile* smf_create_smfFile( int * status );

smfHead* smf_create_smfHead( int * status );

void smf_create_qualname( const char *mode, int indf, IRQLocs **qlocs, int *status);

smfHead * smf_deepcopy_smfHead ( const smfHead *old, int * status);

smfData * smf_deepcopy_smfData ( const smfData *old, const int rawconvert, 
				 const int flags, int * status);

smfDA * smf_deepcopy_smfDA ( const smfData *old, int * status);

smfFile * smf_deepcopy_smfFile ( const smfFile *old, int * status );

void smf_dream_calcweights ( smfData *data, const Grp *ogrp, const int index, 
			     const double gridstep, 
			     const int ngrid,
			     const int *gridminmax, int gridpts[][2],
			     int *status );

void smf_dream_getgrid( const AstKeyMap *keymap, double *gridstep, int *ngrid, 
			int **gridminmax, int gridpts[][2], int *status);

void smf_dream_setjig( char subarray[], int nsampcycle, double gridstep, 
		       double jigpath[][2], int *status);

void smf_dreamsolve( smfData *data, int *status );

int smf_dtype_check( const smfData* data, const char * type, smf_dtype itype,
		     int *status );

void smf_dtype_check_fatal( const smfData* data, const char * type, 
                            smf_dtype itype, int *status );

smf_dtype smf_dtype_fromstring( const char * dtype, int * status );

const char *smf_dtype_string( const smfData* data, int * status );

size_t smf_dtype_size( const smfData* data, int * status );

void smf_dump_smfData( const smfData *data, int showflags, int *status );

void smf_fit_poly(const smfData *data, unsigned char *quality, 
		  const int order, double *poly, int *status);

void
smf_accumulate_prov( const smfData * data, const Grp* igrp, int index, 
                     int ondf, const char *creator, int * status );

void smf_fits_crchan( size_t nfits, const char * headrec, AstFitsChan ** fits, 
                      int *status);

void smf_fits_export2DA ( AstFitsChan *fitschan, size_t *ncards, 
                          char * fitsrec,
                          int *status );

/* Do not return result since we want the interface to remain the same when a
   string is required. If we return a string we must know who should free it */
void smf_fits_getI( const smfHead * hdr, const char * cardname, int * result, 
		    int * status );
void smf_fits_getL( const smfHead * hdr, const char * cardname, int * result, 
		    int * status );
void smf_fits_getD( const smfHead * hdr, const char * cardname, 
                    double * result, int * status );
void smf_fits_getF( const smfHead * hdr, const char * cardname, 
                    float * result, int * status );
void smf_fits_getS( const smfHead * hdr, const char * cardname, 
                    char result[70], size_t len, int * status );

void
smf_fits_outhdr( AstFitsChan * inhdr, AstFitsChan ** outhdr,
		 AstKeyMap ** obsidmap, int * status );

void smf_flatfield ( const smfData *idata, smfData **odata, const int flags, 
                     int *status );

void smf_flatten ( smfData *data, int *status );

void *smf_free( void * pntr, int * status );

void smf_get_gridcoords ( double *row, double *col, int nrow, int ncol,
			     int *status );

int smf_get_ndfid ( const HDSLoc *loc, const char *name, const char *accmode, 
		    const char *state, const char *dattype, const int ndims, 
		    const int *lbnd, const int *ubnd, int *status );

void smf_get_spread( char *pabuf, int *spread, int *nparam, int *status );

HDSLoc *smf_get_xloc ( const smfData *data, const char *extname, 
			const char *extype, const char *accmode, 
			const int ndims, const int *dims, int *status );

void smf_grp_related(  Grp *igrp, const int grpsize, const int grpbywave,
                       dim_t maxlen, dim_t *maxconcatlen, smfGroup **group,
                       int *status );

void smf_history_add( smfData* data, const char * appl, 
			const char * text, int *status);

int smf_history_check( const smfData* data, const char * appl, int *status);

void smf_history_read( smfData* data, int *status);

void smf_history_write( const smfData* data, const char * appl, 
			const char * text, int *status);

void smf_insert_tslice ( smfData **idata, smfData *tdata, int index, 
                         int *status );

inst_t smf_inst_get( const smfHead * hdr, int * status );

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap, const smfArray * darks,
		     AstFrameSet *outfset, int moving, 
	             int *lbnd_out, int *ubnd_out, size_t mapmem,
                     double *map, unsigned int *hitsmap, double *mapvar, 
		     double *weights, int *status );

void * smf_malloc( size_t nelem, size_t bytes_per_elem, int zero, 
                   int * status );

void smf_mapbounds( Grp *igrp,  int size, const char *system,
                    const AstFrameSet * spacesrefwcs,
                    int alignsys, int *lbnd_out, int *ubnd_out, 
        AstFrameSet **outframeset, int *moving, smfBox ** boxes,int *status );

void smf_mapbounds_approx( Grp *igrp, size_t index, char *system, double pixsize, 
			   int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset, 
			   int *moving, int *status );

void smf_model_create( const smfGroup *igroup, smfArray **iarray,
		       dim_t nchunks, smf_modeltype mtype, int isTordered, 
		       AstFrameSet *outfset, int moving, 
		       int *lbnd_out, int *ubnd_out,
		       smfGroup **mgroup, int nofile, int leaveopen,
		       smfArray **mdata, int *status );

const char *smf_model_getname( smf_modeltype type, int *status);

smf_modeltype smf_model_gettype( const char *modelname, int *status );

void smf_open_and_flatfield ( const Grp *igrp, const Grp *ogrp, size_t index, 
                              const smfArray * darks, smfData **ffdata,
                              int *status);

void smf_open_file( const Grp * igrp, size_t index, const char * mode,
		    int withHdr, smfData ** data, int *status);

void smf_open_mapcoord( smfData *data, const char *mode, int *status );

void smf_open_ndf( const int newndf, const char accmode[],
                   const char filename[],
		   smf_dtype dtype, smfData **ndata, int *status);

void smf_open_ndfname( const HDSLoc *loc, const char accmode[],
                       const char filename[], const char extname[],
                       const char state[], const char dattype[],
                       const int ndims, const int lbnd[], const int ubnd[], 
                       const char datalabel[], const char dataunits[],
                       const AstFrameSet * wcs,
                       smfData **ndfdata, 
                       int *status);

void smf_open_newfile( const Grp * igrp, int index, smf_dtype dtype, 
		       const int ndims, const int *lbnd, const int *ubnd, 
		       int flags, smfData ** data, int *status);

void smf_open_related( const smfGroup *group, const dim_t subindex, const char *accmode,
		       smfArray **relfiles, int *status );

void * smf_realloc( void * pntr, size_t nelem, size_t bytes_per_elem,
		    int * status );

void smf_rebinmap( smfData *data, int index, int size, 
                   AstFrameSet *outframeset, int spread, const double params[], 
		   int moving, int genvar, int *lbnd_out, int *ubnd_out,
                   double *map, double *variance,
		   double *weights, int *status );

double smf_scale_tau ( const double tauwvm, const char *filter, int *status);

void smf_scanfit( smfData *data, unsigned char *quality, int order, 
		  int *status );

void smf_simplerebinmap( double *data, double *variance, int *lut, 
			 unsigned char *qual, unsigned char mask, dim_t dsize, 
			 int sampvar, int flags, double *map, 
			 double *mapweight, unsigned int *hitsmap, 
			 double *mapvar, dim_t msize, int *status );

void smf_store_image( smfData *data, HDSLoc *scu2redloc, int cycle, int ndim, 
		      int dims[], int nsampcycle, int vxmin, int vymin, 
		      double *image, double *zero, int *status);

void smf_string_to_dtype ( const char * datatype, smf_dtype *dtype, int * status );

void smf_subtract_plane( smfData *data, smfArray *array, const char *fittype, 
			 int *status);

void smf_subtract_plane1( smfData *data, const char *fittype, double *meansky,
			  int *status);

void smf_subtract_plane2( smfArray *array, const char *fittype, double *meansky,
			  int *status);

void smf_subtract_poly( smfData *data, unsigned char *quality, int rel, 
			int *status );

void smf_telpos_get( smfHead * hdr, int * status );

void smf_tslice_ast (smfData * data, int index, int needwcs, int * status );

void smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
                     int autogrid, int usedetpos, AstFrameSet *spacerefwcs, 
                     AstFrameSet *specrefwcs, double par[ 7 ], 
                     Grp *detgrp, int moving, int specunion, int lbnd[ 3 ], 
                     int ubnd[ 3 ], AstFrameSet **wcsout, int *npos, 
                     int *hasoffexp, smfBox **boxes, int *polobs, int *status );

void smf_rebincube( smfData *data, int first, int last, int *ptime, int badmask, int is2d,
                    AstSkyFrame *abskyfrm, AstMapping *oskymap, 
                    AstFrame *ospecfrm, AstMapping *ospecmap, Grp *detgrp,
                    int moving, int usewgt, 
                    int lbnd_out[ 3 ], int ubnd_out[ 3 ], 
                    int spread, const double params[], 
                    int genvar, float *data_array, float *var_array, 
                    double *wgt_array, float *texp_array, float *teff_array, 
                    double *fcon, int *nused, int *nreject, int *naccept, int *status );

void smf_rebincube_ast( smfData *data, int first, int last, int *ptime, dim_t nchan,
                        dim_t ndet, dim_t nslice, dim_t nel, dim_t nxy, 
                        dim_t nout, dim_t dim[3], AstMapping *ssmap,
                        AstSkyFrame *abskyfrm, AstMapping *oskymap, 
                        Grp *detgrp, int moving, int usewgt, int spread, 
                        const double params[], int genvar, double tfac, 
                        double fcon, float *data_array, float *var_array, 
                        double *wgt_array, float *texp_array, 
                        float *teff_array, int *good_tsys, int *nused, int *status );

void smf_rebincube_nn( smfData *data, int first, int last, int *ptime, dim_t nchan,
                       dim_t ndet, dim_t nslice, dim_t nel, dim_t nxy,
                       dim_t nout, dim_t dim[3], int badmask, int is2d,
                       AstMapping *ssmap, AstSkyFrame *abskyfrm, 
                       AstMapping *oskymap, Grp *detgrp, int moving, 
                       int usewgt, int genvar, double tcon, double fcon, 
                       float *data_array, float *var_array, double *wgt_array, 
                       float *texp_array, float *teff_array, int *nused, 
                       int *nreject, int *naccept, int *good_tsys, int *status );

void smf_rebincube_paste2d( int badmask, dim_t nchan, int nchanout, 
                            int *spectab, int *specpop, dim_t iv0, 
                            dim_t nxy, double wgt, int genvar, 
                            double invar, float *ddata, 
                            float *data_array, float *var_array,
                            double *wgt_array, int *pop_array, 
                            int *nused, int *nreject, int *naccept, 
                            float *work, int *status );

void smf_rebincube_paste3d( dim_t nchan, dim_t nout, int *spectab, dim_t iv0,
                            dim_t nxy, double wgt, int genvar, double invar, 
                            float *pdata, float *data_array, 
                            float *var_array, double *wgt_array, int *nused, 
                            int *status );

void smf_rebincube_norm2d( dim_t nout, dim_t nxy, int genvar, 
                           float *data_array, float *var_array, 
                           double *wgt_array, int *pop_array, int *status );

void smf_rebincube_norm3d( dim_t nout, dim_t nxy, int genvar, 
                            int nused, float *data_array, 
                            float *var_array, double *wgt_array, 
                            int *status );

void smf_rebincube_spectab( dim_t nchan, dim_t nchanout, AstMapping *ssmap, 
                            int **pspectab, int *status );

void smf_rebincube_init( int is2d, dim_t nxy, dim_t nout, int genvar, 
                         float *data_array, float *var_array, 
                         double *wgt_array, float *texp_array,
                         float *teff_array, int *nused, int *status );

const double *smf_rebincube_tcon( smfHead *hdr, dim_t itime, double fcon, 
                                  float *texp, float *teff, double *tcon, 
                                  int *status );

AstMapping *smf_rebin_totmap( smfData *data, dim_t itime, 
			      AstSkyFrame *abskyfrm, 
			      AstMapping *oskymap, int moving, 
			      int *status );

void smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos, 
                   int autogrid, int alignsys, Grp *detgrp, double par[ 7 ], 
                   int *moving, AstSkyFrame **skyframe, int *sparse, 
                   int *gottsys, int *status );

void smf_makefitschan( const char *system, double crpix[2], double crval[2],
                       double cdelt[2], double crota2, AstFitsChan *fc,
                       int *status );

void smf_detpos_wcs( smfHead *hdr, int index, const double telpos[3],
                     AstFrameSet **fset, int *status );

void smf_geod( const double pos[3], double *phi, double *h, double *lambda );

void smf_terr( double phi, double h, double lambda, double pos[3] );

void smf_instap_get( smfHead * hdr, int * status );


void smf_calcmodel_com( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status);

void smf_calcmodel_ast( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status);

void smf_calcmodel_noi( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status);

void smf_calcmodel_ext( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status);

smf_calcmodelptr smf_model_getptr( smf_modeltype type, int *status);

void smf_labelunit( Grp *igrp,  int size, smfData *odata, int *status );

void smf_sparsebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
                       int usedetpos, Grp *detgrp, int lbnd[ 3 ], int ubnd[ 3 ],
                       AstFrameSet **wcsout, int *hasoffexp, int *polobs, int *status );

void smf_rebinsparse( smfData *data, int first, int *ptime, AstFrame *ospecfrm, AstMapping *ospecmap, 
                      AstSkyFrame *oskyframe, Grp *detgrp, int lbnd_out[ 3 ], 
                      int ubnd_out[ 3 ], int genvar, float *data_array, 
                      float *var_array, int *ispec, float *texp_array, 
                      float *ton_array, double *fcon, int *status );


void smf_open_model( const Grp *igrp, int index, const char *mode, smfData **data,
		     int *status );

size_t smf_dtype_sz( const smf_dtype dtype, int *status );

void smf_NDFexport( const smfData *data, void *variance, 
                    unsigned char *quality, smfHead *hdr, 
                    const char *name, int *status );

void smf_open_related_model( const smfGroup *group, const dim_t subindex, 
			     const char *accmode, smfArray **relfiles, 
			     int *status );

smfTile *smf_choosetiles( Grp *igrp,  int size, int *lbnd, 
                          int *ubnd, smfBox *boxes, int spread, 
                          const double params[], AstFrameSet *wcsout, 
                          int tile_size[ 2 ], int trim, int border,
                          size_t *ntiles, int *status );

smfTile *smf_freetiles( smfTile *tiles, int size, int *status );

void smf_reshapendf( smfData **data, smfTile *tile, int *status );

void smf_dataOrder( smfData *data, int isTordered, int *status );

int ***smf_choosepolbins( Grp *igrp, int size, float binsize, float binzero,
                          AstFrameSet *wcsout2d, int *npbin, double **pangle, 
                          int *status );

int ***smf_freepolbins( int nndf, int npbin, double **pangle, int ***ptime, 
                        int *status );

void smf_polext( int ondf, double angle, int *status );

void smf_filter_execute( smfData *data, smfFilter *filt, int *status );

void smf_concat_smfGroup( smfGroup *igrp, const smfArray * darks, 
                          size_t whichchunk, int isTordered, 
                          AstFrameSet *outfset, int moving, 
                          int *lbnd_out, int *ubnd_out, dim_t padStart,
                          dim_t padEnd, int flags, smfArray **concat, 
                          int *status );

void smf_checkdets( Grp *detgrp, smfData *data, int *status );

int *smf_sortd( size_t nel, double *array, int *sorted, int *status );
int *smf_sorti( size_t nel, int *array, int *sorted, int *status );
void smf_ext2km( int indf, const char *xname, AstKeyMap *keymap, int mode, int *status );
void smf_km2ext( int indf, const char *xname, AstKeyMap *keymap, int *timeout, int *status );

void smf_reorder( const char *type, void *in, int len, int ndim, int *dims, int axis, int *index, int maxis, int *mask, void *out, int *status );
void smf_reorderr( float *in, int ndim, int *dims, int axis, int *index, int maxis, int *mask, float *out, int *status );
void smf_reorderi( int *in, int ndim, int *dims, int axis, int *index, int maxis, int *mask, int *out, int *status );
void smf_reorderd( double *in, int ndim, int *dims, int axis, int *index, int maxis, int *mask, double *out, int *status );
void smf_reorderc( char *in, int len, int ndim, int *dims, int axis, int *index, int maxis, int *mask, char *out, int *status );
void smf_updateprov( int ondf, const smfData *data, int indf, const char *creator, int *status );

void smf_fits_setL( const smfHead *hdr, const char *name, int value, 
		    const char *comment, int overwrite, int *status );

void smf_getrefwcs( const char *param, AstFrameSet **specwcs,
                    AstFrameSet **spacewcs, int *status );


void smf_resampcube( smfData *data, int index, int size, 
                     AstSkyFrame *abskyfrm, AstMapping *iskymap, 
                     AstFrame *ispecfrm, AstMapping *ispecmap, 
                     Grp *detgrp, int moving, int slbnd[ 3 ], 
                     int subnd[ 3 ], int interp, const double params[], 
                     float *in_data, float *out_data, int *overlap, 
                     int *status );


void smf_resampcube_copy( dim_t nchan, dim_t nsky, int *spectab, 
                          dim_t iv0, dim_t nxy, float *ddata, 
                          float *in_data, int *status );

void smf_resampcube_nn( smfData *data, int index, int size, dim_t nchan, 
                   dim_t ndet, dim_t nslice, dim_t nel, dim_t nxy, 
                   dim_t nsky, dim_t dim[3], AstMapping *ssmap, 
                   AstSkyFrame *abskyfrm, AstMapping *iskymap, 
                   Grp *detgrp, int moving, float *in_data, 
                   float *out_data, int *overlap, int *status );

void smf_resampcube_ast( smfData *data, int index, int size, dim_t nchan,
                         dim_t ndet, dim_t nslice, dim_t nel, dim_t nxy, 
                         dim_t nsky, dim_t dim[3], AstMapping *ssmap,
                         AstSkyFrame *abskyfrm, AstMapping *iskymap, 
                         Grp *detgrp, int moving, int interp, 
                         const double params[], float *in_data, 
                         float *out_data, int *status );

void smf_update_quality( smfData *data, unsigned char *target, int syncbad, 
			 unsigned char *badmask, double badfrac,
			 int *status );
void smf_correct_steps( smfData *data, unsigned char *quality,
                     double dcthresh, dim_t dcbox,
                      int *status );

void smf_simple_stats( double *data, dim_t start, dim_t nsamp,
                     unsigned char *qual, unsigned char mask, double *mean,
                     double *sigma, dim_t *ngood, int *status );

AstKeyMap *smf_groupscans( Grp *igrp,  int size, int *maxsyspop, 
                           int *conform, Grp **ogrp, int *status );

void smf_reportprogress( int max, int *status );

const char *smf_getobsidss( AstFitsChan *hdr, int *status );

float smf_calc_telres( AstFitsChan *hdr, int *status );

void smf_get_moltrans ( double restFreq, const char **molecule, 
                        const char **transition, int *status );

double smf_quick_noise( smfData *data, dim_t bolo, dim_t nsamp, dim_t nchunk, 
			unsigned char *quality, unsigned char mask, 
			int *status );

void smf_flag_spikes( smfData *data, unsigned char *quality, 
                      unsigned char mask, double thresh, size_t niter, 
                      size_t maxiter, size_t *aiter, size_t *nflagged,
                      int *status );

int *smf_find_bad_dets( Grp *igrp,  int size, int *nbaddet, int *status );
void smf_maskacsis( int indf, int *mask, int *status );

void smf_detmask( const char *type, void *in, int len, int ndim, int *dims_in, int maxis, int *mask, void *out, int *status );
void smf_detmaskr( float *in, int ndim, int *dims_in, int maxis, int *mask, float *out, int *status );
void smf_detmaskd( double *in, int ndim, int *dims_in, int maxis, int *mask, double *out, int *status );
void smf_detmaski( int *in, int ndim, int *dims_in, int maxis, int *mask, int *out, int *status );
void smf_detmaskc( char *in, int len, int ndim, int *dims_in, int maxis, int *mask, char *out, int *status );

void smf_kmmerge( const char *xname, AstKeyMap *keymap, int from, int into, int ndet, int *mask, int nts, int rts_num, int *status );

void smf_simpleaddmap( double *map1, double *mapweight1, 
                       unsigned int *hitsmap1, double *mapvar1, 
                       double *map2, double *mapweight2, 
                       unsigned int *hitsmap2, double *mapvar2, dim_t msize, 
                       int *status );


int *smf_find_median( float *farray, double *darray, size_t nel, 
                      int *hist, float *median, int *status );

void smf_checkmem_map( const int *lbnd, const int *ubnd, int rebin, 
		       size_t available, size_t *necessary, int *status );


void smf_checkmem_dimm( dim_t maxlen, inst_t instrument, int nrelated,
                        smf_modeltype *modeltyps, dim_t nmodels,
                        size_t available, size_t *necessary, int *status );

void smf_set_clabels( const char title[], const char label[], const char units[],
		      smfHead* hdr, int * status );

void smf_write_clabels( const smfData* data, int * status );


void smf_check_units( int count, char current[],
		      smfHead* hdr, int * status );

void smf_get_projpar( AstSkyFrame *skyframe, const double skyref[2],
                      int moving, int autogrid,
                      int nallpos, const double * allpos, float telres, 
                      double map_pa, double par[7], int *sparse, int *useauto,
                      int *status );

double smf_calc_mappa( smfHead *hdr, const char *system, AstFrame *sf, 
                       int *status );


void smf_get_taskname( char * taskname, char * prvname, int * status);

void smf_display_projpars( AstSkyFrame * skyframe, double par[7],
                           int *status);

void smf_calc_skyframe( const AstFrame * skyin, const char * system,
                        const smfHead* hdr, 
                        int alignsys, AstSkyFrame ** skyframe,
                        double skyref[2],
                        int * moving, int * status );

void
smf_store_outputbounds (int updatepars, const int lbnd_out[3],
                        const int ubnd_out[3],
                        const AstFrameSet *wcsout, 
                        const AstSkyFrame *oskyfrm, 
                        const AstMapping *oskymap, int *status);

void
smf_expand_tilegroup ( Grp * ogrp, size_t ntile, int npbin, size_t * outsize,
                       int * status);

smfFilter *smf_create_smfFilter( smfData *template, int *status );

void smf_filter_ident( smfFilter *filt, int complex, int *status );

smfFilter *smf_free_smfFilter( smfFilter *filt, int *status );

void smf_filter_r2c( smfFilter *filt, int *status );

void smf_filter_edge( smfFilter *filt, double f, int lowpass, int *status );

void smf_filter_notch( smfFilter *filt, const double f_low[], 
                       const double f_high[], size_t n, int *status );

void smf_NDFexport_smfFilter( const smfFilter *filt, const char *name, 
                              int *status );

double smf_calc_fcon( smfData *data, dim_t nchan, int report, 
                      AstMapping **specmap, AstFrame **specframe,
                      int *status );

void * smf_map_or_malloc( size_t nelem, smf_dtype type, int zero, int indf,
                          const char * comp, int * status );

const char * smf_dtype_str( smf_dtype type, int * status );

const char * smf_obstype_str( smf_obstype type, int * status );

const char * smf_obsmode_str( smf_obsmode type, int * status );

void smf_find_darks( const Grp * ingrp, Grp **outgrp, Grp **darkgrp,
                     int reduce, smfArray ** darks, int * status );

int smf_isdark( const smfData *data, int * status );

void smf_reduce_dark( const smfData *indark, smfData **outdark, 
                      int *status );

void smf_find_subarray ( const smfHead * hdr, char subarray[],
                         size_t buflen, int *subnum, int *status );

void smf_choose_darks( const smfArray *darks, const smfData *indata,
                       size_t *dark1, size_t *dark2, int * status );

void smf_subtract_dark ( smfData * indata, const smfData * dark1, 
  const smfData * dark2, smf_dark_sub_meth method, int *status );

smfData *smf_fft_data( const smfData *indata, int inverse, int *status );

int smf_isfft( const smfData *data, int * status );

void smf_calc_mode( smfHead * hdr, int * status );

#endif /* SMF_DEFINED */
