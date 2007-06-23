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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 University of British Columbia.
*     All Rights Reserved.

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

#ifndef SMF_DEFINED
#define SMF_DEFINED

#include "ast.h"
#include "jcmt/state.h"  /* for inst_t */
#include "smurf_typ.h"
#include "smurf_par.h"
#include "star/grp.h"
#include "smf_typ.h"

void smf_addto_smfArray( smfArray *ary, const smfData *data, int *status );

void smf_average_data( const smfData *data, int start, int nslice, 
		       const int interval, double **avdata, size_t *nelem, int *status);

void smf_bbrebinmap( smfData *data,  int indf, int index, int size, 
                     AstFrameSet *outfset, int *lbnd_out, int *ubnd_out, 
                     double *map, double *variance, double *weights, 
                     int *status );

void smf_boxcar1 ( double *series, const size_t ninpts, size_t window, int *status);

void smf_calc_stats( const smfData *data, const char *mode, const int index,
                     int lo, int hi, double *mean, double *sigma, 
		     int *status);

double smf_calc_covar ( const smfData *data, const int i, const int j,
			int lo, int hi, int *status);

void smf_calc_mapcoord( smfData *data, AstFrameSet *outfset, int *lbnd_out,
                        int *ubnd_out, int *status );

void smf_calc_skyrot( smfData *data, double maxangle, int *nsamples, int *status );

void smf_calc_stareimage( smfData *data, const int naver, int *status);

void smf_calc_telpos( double obsgeo[3], char telName[], double telpos[3],
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
smf_construct_smfDA( smfDA * tofill, double * flatcal,
		     double * flatpar, const char * flatname, int nflat,
		     int * status );
smfData *
smf_construct_smfData( smfData * tofill, smfFile * file, smfHead * hdr, 
		       smfDA * da, smf_dtype dtype, 
		       void * pntr[3], const dim_t dims[], int ndims,
		       int virtual, int ncoeff, double *poly, 
		       AstKeyMap *history, int * status );

smfDream *
smf_construct_smfDream( smfData *data, const int nvert, const int nsampcycle, 
			const int *jigvert, const double *jigpath, int * status );

smfFile *
smf_construct_smfFile(smfFile * tofill, int ndfid, int isSc2store,
		      int isTstream, const char * name,
		      int * status );
smfHead *
smf_construct_smfHead( smfHead * tofill, inst_t instrument,
		       AstFrameSet * wcs, AstFrameSet * tswcs,
                       AstFitsChan * fitshdr,
		       const JCMTState * allState, dim_t curframe, 
		       double instap[], dim_t nframes, unsigned int ndet,
		       const double fplanex[], const double fplaney[],
		       const double detpos[], const char *detname, int rpazel, 
		       const double tsys[], int * status );

smfGroup * 
smf_construct_smfGroup( Grp *igrp, int **subgroups, const int ngroups, 
			const int nrelated, int *status );

void smf_create_lutwcs( int clearcache, const double *fplane_x, 
			const double *fplane_y, const int n_pix, 
			const JCMTState *state, const double instap[2], 
                        const double telpos[3], double steptime, 
                        AstFrameSet **fset, int *status );

smfArray *smf_create_smfArray( const size_t size, int *status );

smfDA * smf_create_smfDA( int * status );

smfData* smf_create_smfData( int flags, int * status );

smfDream *smf_create_smfDream( int * status );

smfFile* smf_create_smfFile( int * status );

smfHead* smf_create_smfHead( int * status );

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

char *smf_dtype_string( const smfData* data, int * status );

size_t smf_dtype_size( const smfData* data, int * status );

void smf_fit_poly(const smfData *data, const int order, double *poly,  
                  int *status);

void smf_fits_add_prov( AstFitsChan * hdr, const char * keyroot,
			const AstKeyMap * idmap,
			int * status);

void smf_fits_crchan( int nfits, char * headrec, AstFitsChan ** fits, 
                      int *status);

void smf_fits_export2DA ( const AstFitsChan *fitschan, int *ncards, 
                          char fitsrec[SC2STORE__MAXFITS][SZFITSCARD], 
                          int *status );

/* Do not return result since we want the interface to remain the same when a
   string is required. If we return a string we must know who should free it */
void smf_fits_getI( const smfHead * hdr, const char * cardname, int * result, 
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

void smf_free( void * pntr, int * status );

void smf_get_gridcoords ( double *row, double *col, int nrow, int ncol,
			     int *status );

int smf_get_ndfid ( const HDSLoc *loc, const char *name, const char *accmode, 
		    const char *state, const char *dattype, const int ndims, 
		    const int *lbnd, const int *ubnd, int *status );

HDSLoc *smf_get_xloc ( const smfData *data, const char *extname, 
			const char *extype, const char *accmode, 
			const int ndims, const int *dims, int *status );

void smf_grp_related( Grp *igrp, const int grpsize, const int grpbywave, 
		      smfGroup **group, int *status );

void smf_history_add( smfData* data, const char * appl, 
			const char * text, int *status);

int smf_history_check( const smfData* data, const char * appl, int *status);

void smf_history_read( smfData* data, int *status);

void smf_history_write( const smfData* data, const char * appl, 
			const char * text, int *status);

void smf_insert_tslice ( smfData **idata, smfData *tdata, int index, 
                         int *status );

inst_t smf_inst_get( const smfHead * hdr, int * status );

void smf_iteratemap( Grp *igrp, AstKeyMap *keymap,
 		     double *map, double *variance, double *weights,
	 	     int msize, int *status );

void * smf_malloc( size_t nelem, size_t bytes_per_elem, int zero, 
                   int * status );

void smf_mapbounds( Grp *igrp,  int size, char *system, double lon_0, 
		    double lat_0, int flag, double pixsize, int *lbnd_out, 
		    int *ubnd_out, AstFrameSet **outframeset, int *moving,
		    int *status );

void smf_mapbounds_approx( Grp *igrp, int index, char *system, double pixsize, 
			   int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset, 
			   int *moving, int *status );

void smf_model_create( Grp *igrp, smf_modeltype mtype, Grp **mgrp, 
		       int *status);

char *smf_model_getname( smf_modeltype type, int *status);

smf_modeltype smf_model_gettype( const char *modelname, int *status );

void smf_open_and_flatfield ( Grp *igrp, Grp *ogrp, int index, 
			      smfData **ffdata, int *status);

void smf_open_file( Grp * igrp, int index, char * mode, int withHdr,
		    smfData ** data, int *status);

void smf_open_mapcoord( smfData *data, int *status );

void smf_open_ndf( const int newndf, char *accmode, char *filename, 
		   smf_dtype dtype, smfData **ndata, int *status);

void smf_open_ndfname( const HDSLoc *loc, char *accmode, char *filename, 
                       const char *extname,
		       const char *state, const char *dattype, const int ndims, 
		       const int *lbnd, const int *ubnd, smfData **ndfdata, 
                       int *status);

void smf_open_newfile( const Grp * igrp, int index, smf_dtype dtype, 
		       const int ndims, const int *lbnd, const int *ubnd, 
		       int flags, smfData ** data, int *status);

void smf_open_related( const smfGroup *group, const int subindex, const char *accmode,
		       smfArray **relfiles, int *status );

void * smf_realloc( void * pntr, size_t nelem, size_t bytes_per_elem,
		    int * status );

void smf_rebinmap( smfData *data, int index, int size, 
                   AstFrameSet *outframeset, int moving,
		   int *lbnd_out, int *ubnd_out,
                   double *map, double *variance,
		   double *weights, int *status );

double smf_scale_tau ( const double tauwvm, const char *filter, int *status);

void smf_scanfit( smfData *data, int order, int *status );

void smf_simplerebinmap( double *data, double *variance, int *lut, int dsize, 
			 int flags, double *map, double *mapweight, 
			 double *mapvar, int msize, int *status );

void smf_store_image( smfData *data, HDSLoc *scu2redloc, int cycle, int ndim, 
		      int dims[], int nsampcycle, int vxmin, int vymin, 
		      double *image, double *zero, int *status);

void smf_string_to_dtype ( const char * datatype, smf_dtype *dtype, int * status );

void smf_subtract_plane( smfData *data, smfArray *array, const char *fittype, 
			 int *status);

void smf_subtract_plane1( smfData *data, const char *fittype, int *status);

void smf_subtract_plane2( smfArray *array, const char *fittype, int *status);

void smf_subtract_poly( smfData *data, int *status );

void smf_telpos_get( const smfHead * hdr, int * status );

/*void smf_tslice ( const smfData *idata, smfData **tdata, int index, 
  int *status );*/

void smf_tslice_ast (smfData * data, int index, int needwcs, int * status );

void smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
                     int autogrid, int usedetpos, double par[ 7 ], 
                     Grp *detgrp, int moving, int lbnd[ 3 ], int ubnd[ 3 ], 
                     AstFrameSet **wcsout, int *npos, int *hasoffexp, int *status );

void smf_rebincube( smfData *data, int index, int size, int badmask, int is2d,
                    AstSkyFrame *abskyfrm, AstMapping *oskymap, 
                    AstFrame *ospecfrm, AstMapping *ospecmap, Grp *detgrp,
                    int moving, int usewgt, int lbnd_out[ 3 ], 
                    int ubnd_out[ 3 ], int spread, const double params[], 
                    int genvar, float *data_array, float *var_array, 
                    double *wgt_array, float *texp_array, float *teff_array, 
                    double *fcon, int *nused, int *nreject, int *naccept, int *status );

void smf_rebincube_ast( smfData *data, int index, int size, dim_t nchan,
                        dim_t ndet, dim_t nslice, dim_t nel, dim_t nxy, 
                        dim_t nout, dim_t dim[3], AstMapping *ssmap,
                        AstSkyFrame *abskyfrm, AstMapping *oskymap, 
                        Grp *detgrp, int moving, int usewgt, int spread, 
                        const double params[], int genvar, double tfac, 
                        double fcon, float *data_array, float *var_array, 
                        double *wgt_array, float *texp_array, 
                        float *teff_array, int *good_tsys, int *nused, int *status );

void smf_rebincube_nn( smfData *data, int index, int size, dim_t nchan,
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

AstMapping *smf_rebincube_totmap( smfData *data, dim_t itime, 
                                  AstSkyFrame *abskyfrm, 
                                  AstMapping *oskymap, int moving, 
                                  int *status );

void smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos, 
                   int autogrid, Grp *detgrp, double par[ 7 ], 
                   int *moving, AstSkyFrame **skyframe, int *sparse, 
                   int *gottsys, int *status );

const char *smf_convert_system( const char *label, int *status );

void smf_makefitschan( const char *system, double crval[2], double cdelt[2], 
                       double crota2, AstFitsChan *fc, int *status );

void smf_detpos_wcs( smfHead *hdr, int index, const double telpos[3],
                     double steptime, AstFrameSet **fset, int *status );

void smf_geod( double pos[3], double *phi, double *h, double *lambda );

void smf_terr( double phi, double h, double lambda, double pos[3] );

void smf_instap_get( smfHead * hdr, int * status );

void smf_calcmodel_com( smfData *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status );

void smf_calcmodel_ast( smfData *res, AstKeyMap *keymap, int *lut,  
			double *map, double *mapvar, smfData *model, 
			int flags, int *status );

void smf_calcmodel_noi( smfData *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status );

void smf_calcmodel_ext( smfData *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status );

smf_calcmodelptr *smf_model_getptr( smf_modeltype type, int *status);

void smf_labelunit( Grp *igrp,  int size, smfData *odata, int *status );

void smf_sparsebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
                       int usedetpos, Grp *detgrp, int lbnd[ 3 ], int ubnd[ 3 ],
                       AstFrameSet **wcsout, int *hasoffexp, int *status );

void smf_rebinsparse( smfData *data, int ifile, AstFrame *ospecfrm, AstMapping *ospecmap, 
                      AstSkyFrame *oskyframe, Grp *detgrp, int lbnd_out[ 3 ], 
                      int ubnd_out[ 3 ], int genvar, float *data_array, 
                      float *var_array, int *ispec, float *texp_array, 
                      float *ton_array, double *fcon, int *status );


void smf_open_model( Grp *igrp, int index, char *mode, smfData **data,
		     int *status );

size_t smf_dtype_sz( const smf_dtype dtype, int *status );

void smf_model_NDFexport( const smfData *data, const char *name, int *status );

#endif /* SMF_DEFINED */
