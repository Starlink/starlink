/*
*+
*  Name:
*     smf_iteratemap

*  Purpose:
*     Iterative map-maker

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:

*     smf_iteratemap(ThrWorkForce *wf, const Grp *igrp, const Grp *iterrootgrp,
*                    const Grp *bolrootgrp, const Grp *shortrootgrp,
*                    const Grp *flagrootgrp, const Grp *samprootgrp,
*                    AstKeyMap *keymap,
*                    const smfArray * darks, const smfArray *bbms,
*                    const smfArray * flatramps, AstKeyMap * heateffmap, AstFrameSet *outfset,
*                    int moving, int *lbnd_out, int *ubnd_out, size_t maxmem,
*                    double *map, int *hitsmap, double *exp_time,
*                    double *mapvar, smf_qual_t *mapqual, double *weights,
*                    char data_units[], double *nboloeff,
*                    size_t *numcontchunks, size_t *numinsmp, size_t *numcnvg,
*                    int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     igrp = const Grp* (Given)
*        Group of input data files
*     iterrootgrp = const Grp * (Given)
*        Root name to use for iteration output maps (if required). Can be a
*        path to an HDS container.
*     bolrootgrp = const Grp * (Given)
*        Root name to use for bolometer output maps (if required). Can be a
*        path to an HDS container.
*     shortrootgrp = const Grp * (Given)
*        Root name to use for short output maps (if required). Can be a
*        path to an HDS container.
*     flagrootgrp = const Grp * (Given)
*        Root name to use for flag output maps (if required). Can be a
*        path to an HDS container.
*     samprootgrp = const Grp * (Given)
*        Root name to use for sample output cubes (if required). Can be a
*        path to an HDS container.
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
*     darks = const smfArray * (Given)
*        Collection of dark frames. Can be NULL.
*     bbms = smfArray * (Given)
*        Masks for each subarray (e.g. returned by smf_reqest_mask call)
*     flatramps = const smfArray * (Given)
*        Collection of flatfield ramps. Will be passed to
*        smf_open_and_flatfield.
*     heateffmap = AstKeyMap * (Given)
*        Details of heater efficiency data to be applied during flatfielding.
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping if calculating
*        pointing LUT on-the-fly
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = int* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*        (if outfset specified)
*     ubnd_out = int* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*        (if outfset specified)
*     maxmem = size_t (Given)
*        Maximum memory that me be used by smf_iteratemap (bytes)
*     map = double* (Returned)
*        The output map array
*     hitsmap = int* (Returned)
*        Number of samples that land in a pixel (ignore if NULL pointer)
*     exp_time = double* (Returned)
*        Exposure time per map pixel (ignore if NULL pointer)
*     mapvar = double* (Returned)
*        Variance of each pixel in map
*     mapqual = smf_qual_t* (Returned)
*        Quality for each pixel in map
*     weights = double* (Returned)
*        Relative weighting for each pixel in map
*     data_units = char[] (Returned)
*        Data units read from the first chunk. These may be different from
*        that read from raw data due to flatfielding. Should be a buffer
*        of at least size SMF__CHARLABEL.
*     nboloeff = double * (Returned)
*        If non-NULL, will contain the effective number of bolometers used
*        to create the map.
*     numcontchunks = size_t *(Returned)
*        If non-NULL, will contain the number of continuous data chunks that
*        were processed.
*     numinsmp = size_t *(Returned)
*        If non-NULL, will contain the number of continuous data chunks that
*        did not go into the map due to insufficient samples.
*     numcnvg = size_t *(Returned)
*        If non-NULL, will contain the number of continuous data chunks that
*        did not converge (although they are still added to the map).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses an iterative algorithm to estimate a number
*     of different model signal model components which, when added
*     together, fit the observed bolometer signals. One of these
*     models is the astronomical signal which is represented both as a
*     map (the ultimate goal of map-making), and the signal it would
*     produce as the bolometers scan across it).
*
*     There are two primary loops over that data that one should be
*     aware of.  The first is the concept of a continuous chunk, or
*     "contchunk". This is a piece of un-interrupted data, usually
*     made by concatenating a number of smaller files together. This
*     operation is necessary because the SCUBA-2 data acquisition
*     system usually splits up data in to pieces that are 30 s long
*     and stores them in separate files. The other loop seen
*     throughout the code is file subgroup, or "filegroup" for
*     short. For machines with little memory it is not possible to
*     load all of the data for a given contchunk at once. A single
*     file subgroup is normally the list of single 30s files for all
*     SCUBA-2 subarrays at a given instant in time. For this
*     low-memory usage case the configuration parameter "memiter" must
*     be set to 0. Then, within each iteration of the map-maker there
*     will be a loop over "nfilegroups" of these files (i.e. the
*     number of 30 s files per subarray). For normal, "memiter=1"
*     usage, nfilegroups is simply set to 1 (i.e. there is no file i/o
*     within the iteration loop).
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-05-18 (EC):
*        Initial version.
*     2006-06-24 (EC):
*        Parameters given by keymap
*     2006-08-07 (TIMJ):
*        GRP__NOID is not a Fortran concept.
*     2006-08-16 (EC):
*        Intermediate step: Old routine works with new model container code
*     2007-02-07 (EC):
*        Fixed bugs in implementation of models, order of execution.
*     2007-02-08 (EC):
*        Changed location of AST model calculation
*        Fixed pointer bug for variance of residual in map estimate
*     2007-02-12 (EC)
*        Enabled dyanmic usage of model components using function pointers
*     2007-03-02 (EC)
*        Added noise estimation from residual
*        Fixed counter bug
*     2007-03-05 (EC)
*        Parse modelorder keyword in config file
*     2007-04-30 (EC)
*        Put map estimation in first chunk loop, only ast in second
*     2007-05-17 (EC)
*        Added missing status checks
*     2007-06-13 (EC):
*        - Use new DIMM binary file format
*     2007-06-14 (EC):
*        - If config file has exportndf set, export DIMM components to *.sdf
*     2007-07-10 (EC):
*        - Use smfGroups and smfArrays instead of groups and smfdatas
*        - fixed problem with function pointer type casting
*     2007-07-13 (EC):
*        - use arrays of pointers to all chunks to store modeldata/modelgroups
*     2007-07-20 (EC):
*        - fixed freeing of modeldata
*     2007-08-09 (EC):
*        - changed index order for model
*        - added memiter flag to config file - avoids doing file i/o
*     2007-08-17 (EC):
*        Added nofile flag to smf_model_create to avoid creating .dimm files
*        in memiter=1 case.
*     2007-11-15 (EC):
*        -Use smf_concat_smfGroup for memiter=1 case.
*        -Modified interface to hand projection from caller to concat_smfGroup
*        -Check for file name existence when calling smf_model_NDFexport
*     2007-11-15 (EC):
*        -Switch to bolo-ordering the data. Compiles but still buggy.
*     2007-12-14 (EC):
*        -set file access to UPDATE for model components (was READ)
*        -modified smf_calc_mapcoord interface
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-22 (EC):
*        Added hitsmap to interface
*     2008-01-25 (EC):
*        Handle non-flatfielded input data. Pointing LUT is now calculated
*        in smf_model_create rather than requiring calls to smf_calc_mapcoord.
*     2008-03-04 (EC):
*        -Modified model calculation to use smfDIMMData in interface
*        -Added QUAlity component
*     2008-04-03 (EC):
*        - Use QUALITY in map-maker
*        - Added data cleaning options to CONFIG file
*     2008-04-14 (EC):
*        - Fixed memory-deallocation (res/ast/qua...)
*        - Added QUALITY/VARIANCE to NDFexport
*     2008-04-16 (EC):
*        - Added outer loop to handle multiple cont. chunks for memiter=1 case
*     2008-04-17 (EC):
*        - Added maxlen to config file, modified smf_grp_related
*        - Use variance stored in NOI to estimate map
*        - Don't include VARIANCE of input files when concatenating
*        - Store VARIANCE (from NOI) in residual
*     2008-04-18 (EC):
*        Calculate and display chisquared for each chunk
*     2008-04-23 (EC):
*        - Added sample variance estimate for varmap
*        - Propagate header information to exported model components
*        - Added CHITOL config parameter to control stopping
*     2008-04-24 (EC):
*        - Improved status checking
*        - extra checks for valid pointers before exporting model components
*     2008-04-28 (EC):
*        - Added memory usage check
*     2008-04-29 (EC)
*        Check for VAL__BADD in map to avoid propagating to residual
*     2008-04-30 (EC)
*        - Undo EXTinction correction after calculating AST
*     2008-05-01 (EC)
*        - More intelligent auto-sizing of concantenated chunks
*     2008-05-02 (EC)
*        - Use different levels of verbosity in messages
*     2008-06-12 (EC)
*        - renamed smf_model_NDFexport smf_NDFexport
*        - added edge and notch frequency-domain filters
*     2008-07-03 (EC)
*        - Added padstart/padend to config files
*     2008-07-25 (TIMJ):
*        Pass darks through to smf_concat_smfGroup
*     2008-07-29 (TIMJ):
*        Steptime is now in smfHead.
*     2008-09-30 (EC):
*        Use smf_write_smfData instead of smf_NDFexport
*     2008-12-12 (EC):
*        Extra re-normalization required for GAIn model
*     2009-01-06 (EC):
*        -Added flagging of data during stationary telescope pointing
*        -apply bad pixel mask (BPM)
*     2009-01-12 (EC):
*        Move application of BPM into smf_concat_smfGroup
*     2009-03-09 (EC):
*        Don't need to call smf_calcmodel_gai because flatfield no longer
*        modified by smf_calcmodel_com
*     2009-03-20 (EC):
*        Added capability to calculate model components after AST. Use must
*        now explicitly give AST in MODELORDER keywordd from config file.
*     2009-04-15 (EC):
*        Factor cleaning parameter parsing into smf_get_cleanpar.
*     2009-04-16 (EC):
*        Option of exporting only certain model components to NDF
*     2009-04-17 (EC):
*        Factor filter generation out to smf_filter_fromkeymap
*     2009-04-20 (EC):
*        - Move flagging of stationary data to smf_model_create
*        - Optinally delete .DIMM files (deldimm=1 in CONFIG file)
*     2009-04-23 (EC):
*        Add numerous MSG__DEBUG timing messages
*     2009-09-30 (EC):
*        Fix bug in handling of AST model component and residuals
*     2009-10-06 (EC):
*        - don't automatically generate bad status if < SMF__MINSTATSAMP
*          good bolos (to enable single-detector maps)
*        - don't try to weight data at map-making stage if no noise estimate
*          is available
*     2009-10-25 (EC):
*        - Add back in option of using common-mode to flatfield data; need to
*          invert the GAIn once per iteration.
*        - add bolomap flag to config file (produce single-detector images)
*     2009-10-25 (TIMJ):
*        Add bolrootgrp argument to give us control of where the bolometer
*        maps go.
*     2009-10-28 (TIMJ):
*        Add data_units. Needed because we can only read data units after
*        the data have been flatfielded. Also check for consistency.
*     2009-11-10 (EC):
*        Add noexportsetbad dimmconfig parameter to set bad values in exported
*        files when SMF__Q_BADB bits set.
*     2009-11-12 (EC):
*        Add itermap and iterrootgrp to enable writing of intermediate maps
*        after each iteration (matching style of bolomap and bolrootgrp).
*     2009-11-13 (EC):
*        If chi^2 increases don't set converged flag; warn user.
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-01-11 (EC):
*        Add fillgaps to data pre-processing (config file)
*     2010-01-12 (TIMJ):
*        Add facility for merging keymaps from config file.
*     2010-01-18 (EC):
*        Export data before dying if SMF__INSMP status set
*     2010-01-19 (DSB)
*        - Add dcthresh2 config parameter.
*     2010-03-02 (EC)
*        When creating the continuous chunks round up to the nearest
*        integral number of files.
*     2010-03-11 (TIMJ):
*        Add flatramps argument.
*     2010-03-18 (EC):
*        Add reporting on quality flag statistics each iteration
*     2010-03-23 (DSB)
*        - Replace dcthresh2 with dcmedianwidth config parameter.
*     2010-04-13 (EC)
*        Add shortmap to config file -- creates .MORE.SMURF.SHORTMAPS extension
*     2010-04-20 (EC)
*        Add map quality
*     2010-04-22 (EC)
*        Add useful FITS headers for itermaps/bolomaps/shortmaps
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-05-05 (TIMJ):
*        Remove keymap merging of sub-instruments since this is now handled
*        during configuration file reading.
*     2010-05-07 (TIMJ):
*        Use atl instead of ast for putting values into a FITS chan.
*        Write MJD to shortmaps
*     2010-05-12 (EC):
*        Add support for collapsed quality for COM model exportation.
*     2010-05-14 (TIMJ):
*        Added map dimensions to smfDIMMData so set them
*     2010-05-18 (TIMJ):
*        Ensure that the LUT is ordered in the same way as the AST model
*     2010-05-21 (DSB):
*        Added dclimcorr argument for sm_fix_steps.
*     2010-05-27 (TIMJ):
*        Add effective number of bolometers.
*     2010-05-31 (EC):
*        Factor initial data cleaning out to smf_clean_smfData
*     2010-06-28 (TIMJ):
*        Pass in explicit exposure time array so that we can handle variable
*        step times.
*     2010-07-22 (DSB):
*        Use smf_get_padding to get padding with a dynamic default value.
*     2010-08-10 (EC):
*        Added "doclean" and "exportclean" config parameters
*     2010-08-13 (EC):
*        Factored out writing of itermap extension to smf_write_itermap
*     2010-08-20 (EC):
*        Factored bolomaps/shortmaps out to smf_write_[bolomap]/[shortmap]
*     2010-08-23 (EC):
*        -Re-name chunk --> filegroup throughout for clarity
*        -Improve commenting of the major blocks in the code
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts
*     2010-09-20 (EC):
*        If noise map generated in smf_clean_smfData, use to initialize NOI
*     2010-09-28 (EC):
*        Added .MORE.SMURF.FLAGMAPS extension
*     2010-10-04 (DSB):
*        Move estimation of required padding into smf_get_related, to avoid
*        opening all the input files an extra time.
*     2010-10-05 (EC):
*        Modified use of quit flag to exit from iterations: init to -1, set to
*        0 and do one more pass if exit criterion met, then set to 1 last time
*        through. This enables correct usage of SMF__DIMM_LASTITER even when
*        iterating to convergence.
*     2010-10-22 (EC):
*        Add downsampling capability
*     2010-10-26 (EC):
*        Add fakemap capability to add external astronomical signal
*     2011-04-15 (EC):
*        Optionally handle subarrays separately with new groupsubarray flag
*     2011-06-27 (EC):
*        Report # chunks with no samples or bad convergence, and return
*        along with ncontchunks back to the caller.
*     2011-06-29 (EC):
*        Add sampcubes extension
*     2011-09-19 (EC):
*        Add ability to write ONLY the final itermap (set itermap < 0)
*     2011-11-21 (EC):
*        -Use map directly instead of 3d cube projection for AST
*        -Check convergence of map in addition to chi^2
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2011 University of British Columbia.
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
*     MA 02111-1307, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par.h"
#include "par_par.h"
#include "star/one.h"
#include "star/atl.h"
#include "fftw3.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "jcmt/state.h"

/* Other includes */
#include "sys/time.h"


/* define __ITERATEMAP_SHOW_MEM to display memory usage... and also
   configure AST using --with-memdebug */
/*#define __ITERATEMAP_SHOW_MEM*/

#ifdef __ITERATEMAP_SHOW_MEM
void _smf_iteratemap_showmem( int *status );
void _smf_iteratemap_showmem( int *status ) {
  size_t memcurrent,mempeak;
  astMemoryStats( 0, &mempeak, &memcurrent );
  msgOutf( "", "SMURF: === current /peak memory usage: %zu / %zu MiB ===",
           status, memcurrent/SMF__MIB, mempeak/SMF__MIB );
}
#endif

#define FUNC_NAME "smf_iteratemap"

/* Main routine */
void smf_iteratemap( ThrWorkForce *wf, const Grp *igrp, const Grp *iterrootgrp,
                     const Grp *bolrootgrp, const Grp *shortrootgrp,
                     const Grp *flagrootgrp, const Grp *samprootgrp,
                     AstKeyMap *keymap,
                     const smfArray *darks, const smfArray *bbms,
                     const smfArray * flatramps, AstKeyMap * heateffmap, AstFrameSet *outfset,
                     int moving, int *lbnd_out, int *ubnd_out, size_t maxmem,
                     double *map, int *hitsmap, double * exp_time,
                     double *mapvar, smf_qual_t *mapqual, double *weights,
                     char data_units[], double * nboloeff,
                     size_t *numcontchunks, size_t *numinsmp,
                     size_t *numcnvg, int *status ) {

  /* Local Variables */
  int bolomap=0;                /* If set, produce single bolo maps */
  size_t bstride;               /* Bolometer stride */
  double *chisquared=NULL;      /* chisquared for each chunk each iter */
  double chitol=0;              /* chisquared change tolerance for stopping */
  size_t contchunk;             /* Continuous chunk in outer loop counter */
  int converged=0;              /* Has stopping criteria been met? */
  smfDIMMData dat;              /* Struct passed around to model components */
  smfData *data=NULL;           /* Temporary smfData pointer */
  int deldimm=0;                /* Delete temporary .DIMM files */
  double downsampscale;         /* Downsample factor to preserve this scale */
  int tstep = 0;                /* Time step between full WCS calculations */
  int dimmflags;                /* Control flags for DIMM model components */
  int doclean=1;                /* Are we doing data pre-processing? */
  dim_t dsize;                  /* Size of data arrays in containers */
  int ensureflat=1;             /* flatfield data as they are loaded */
  int exportclean=0;            /* Are we doing to export clean data? */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  int *exportNDF_which=NULL;    /* Which models in modelorder will be exported*/
  char *fakemap=NULL;           /* Name of external map with fake sources */
  int fakendf=NDF__NOID;        /* NDF id for fakemap */
  double fakescale;             /* Scale factor for fakemap */
  size_t count_mcnvg=0;         /* # chunks fail to converge */
  size_t count_minsmp=0;        /* # chunks fail due to insufficient samples */
  smf_qual_t flagmap=0;         /* bit mask for flagmaps */
  double *fmapdata=NULL;        /* fakemap for adding external ast signal */
  int groupsubarray;            /* Handle subarrays separately? */
  int noexportsetbad=0;         /* Don't set bad values in exported models */
  int haveast=0;                /* Set if AST is one of the models */
  int haveext=0;                /* Set if EXT is one of the models */
  int havegai=0;                /* Set if GAI is one of the models */
  int havenoi=0;                /* Set if NOI is one of the models */
  smfData *refdata=NULL;        /* Pointer to reference smfData */
  size_t i;                     /* Loop counter */
  int ii;                       /* Loop counter */
  size_t idx=0;                 /* index within subgroup */
  smfGroup *igroup=NULL;        /* smfGroup corresponding to igrp */
  int isize;                    /* Number of files in input group */
  int iter;                     /* Iteration number */
  int itermap=0;                /* If set, produce maps each iteration */
  size_t j;                     /* Loop counter */
  size_t k;                     /* Loop counter */
  size_t l;                     /* Loop counter */
  double *lastchisquared=NULL;  /* chisquared for last iter */
  double *lastmap=NULL;         /* map from the last iter */
  smfArray **lut=NULL;          /* Pointing LUT for each file */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfGroup *lutgroup=NULL;      /* smfGroup of lut model files */
  double *mapchange=NULL;       /* Array storing change (map - lastmap)/sigma*/
  double mapchange_mean=0;      /* Mean change in map */
  double mapchange_max=0;       /* Maximum change in the map */
  double *mapweightsq=NULL;     /* map weight squared */
  dim_t maxconcat;              /* Longest continuous chunk that fits in mem.*/
  dim_t maxfile;                /* Longest file length in time samples*/
  int maxiter=0;                /* Maximum number of iterations */
  double maxlen=0;              /* Max length in seconds of cont. chunk */
  int memiter=0;                /* If set iterate completely in memory */
  size_t memneeded;             /* Memory required for map-maker */
  smfArray ***model=NULL;       /* Array of pointers smfArrays for ea. model */
  smfGroup **modelgroups=NULL;  /* Array of group ptrs/ each model component */
  char *modelname=NULL;         /* Name of current model component */
  char modelnames[SMF_MODEL_MAX*4]; /* Array of all model components names */
  smf_modeltype *modeltyps=NULL;/* Array of model types */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  dim_t mdims[2];               /* Dimensions of map */
  dim_t msize;                  /* Number of elements in map */
  char name[GRP__SZNAM+1];      /* Buffer for storing exported model names */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t nfilegroups=0;          /* Number files/subarray within iteration loop*/
  size_t ncontchunks=0;         /* Number continuous chunks outside iter loop*/
  int nm=0;                     /* Signed int version of nmodels */
  dim_t nmodels=0;              /* Number of model components / iteration */
  size_t nsamples_tot = 0;      /* Number of valid samples in all chunks */
  dim_t nthetabin;              /* Number of scan angle bins */
  size_t ntgood_tot = 0;        /* Number of good time slices in all chunks */
  dim_t ntslice;                /* Number of time slices */
  size_t numdata;               /* Total number of samples in chunk */
  int numiter=0;                /* Total number iterations */
  dim_t pad=0;                  /* How many samples of padding at both ends */
  char *pname=NULL;             /* Poiner to name */
  size_t qcount_last[SMF__NQBITS_TSERIES];/* quality bit counter -- last iter */
  smfArray **qua=NULL;          /* Quality flags for each file */
  smf_qual_t *qua_data=NULL; /* Pointer to DATA component of qua */
  smfGroup *quagroup=NULL;      /* smfGroup of quality model files */
  int quit=0;                   /* flag indicates when to quit */
  int rebinflags;               /* Flags to control rebinning */
  smfArray **res=NULL;          /* Residual signal */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  smfGroup *resgroup=NULL;      /* smfGroup of model residual files */
  int sampcube;                 /* write SAMPCUBES extensions? */
  double scalevar=0;            /* scale factor for variance */
  int shortmap=0;               /* If set, produce maps every shortmap tslices*/
  double srate_maxlen=0;        /* Sample rate used to calc maxlen in samples */
  double steptime;              /* Length of a sample in seconds */
  const char *tempstr=NULL;     /* Temporary pointer to static char buffer */
  double *thetabincen=NULL;     /* Bin centres of scan angle */
  double *thetabins=NULL;       /* Bins of scan angle */
  int *thishits=NULL;           /* Pointer to this hits map */
  double *thismap=NULL;         /* Pointer to this map */
  smf_modeltype thismodel;      /* Type of current model */
  smf_qual_t *thisqual=NULL; /* Pointer to this quality map */
  double *thisvar=NULL;         /* Pointer to this variance map */
  double *thisweight=NULL;      /* Pointer to this weights map */
  double *thisweightsq=NULL;    /* Pointer to this weights map^2 */
  size_t try;                   /* Try to concatenate this many samples */
  size_t tstride;               /* Time stride */
  struct timeval tv1, tv2;      /* Timers */
  int untilconverge=0;          /* Set if iterating to convergence */
  double *var_data=NULL;        /* Pointer to DATA component of NOI */
  int varmapmethod=0;           /* Method for calculating varmap */
  dim_t whichast=0;             /* Model index of AST (must be specified) */
  dim_t whichext=0;             /* Model index of EXT if present */
  dim_t whichgai=0;             /* Model index of GAI if present */
  dim_t whichnoi=0;             /* Model index of NOI if present */
  int *whichthetabin=NULL;      /* Which scan angle bin each time slice */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Calculate number of elements in the map */
  if( (ubnd_out[0]-lbnd_out[0] < 0) || (ubnd_out[1]-lbnd_out[1] < 0) ) {
    *status = SAI__ERROR;
    msgSeti("L0",lbnd_out[0]);
    msgSeti("L1",lbnd_out[1]);
    msgSeti("U0",ubnd_out[0]);
    msgSeti("U1",ubnd_out[1]);
    errRep("", FUNC_NAME ": Invalid mapbounds: LBND=[^L0,^L1] UBND=[^U0,^U1]",
           status);
  }

  mdims[0] = ubnd_out[0] - lbnd_out[0] + 1;
  mdims[1] = ubnd_out[1] - lbnd_out[1] + 1;
  msize = mdims[0] * mdims[1];

  /* Allocate space for the previous map and difference for convergence tests */
  lastmap = astCalloc( msize, sizeof(*lastmap) );
  mapchange = astCalloc( msize, sizeof(*lastmap) );

  /* Always need to initialize this zeromask. The buffer will get
     allocated in smf_calcmodel_ast if ast.zero_circle was set. */
  dat.zeromask = NULL;

  /* Get size of the input group */
  isize = grpGrpsz( igrp, status );











  /* ***************************************************************************
     Parse the CONFIG parameters stored in the keymap, and set up
     defaults for the map-maker. We assume that all variables have
     been given defaults through the .def file.
  *************************************************************************** */

  if( *status == SAI__OK ) {
    /* Number of iterations */
    astMapGet0I( keymap, "NUMITER", &numiter );
    if( numiter == 0 ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": NUMITER cannot be 0", status);
    } else {
      if( numiter < 0 ) {
        /* If negative, iterate to convergence or abs(numiter), whichever comes
           first */
        maxiter = abs(numiter);
        untilconverge = 1;
      } else {
        /* Otherwise iterate a fixed number of times */
        maxiter = numiter;
        untilconverge = 0;
      }
    }

    if( *status == SAI__OK ) {
      /* Required positional accuracy, in output map pixels. */
      astMapGet0I( keymap, "TSTEP", &tstep );

      /* Chisquared change tolerance for stopping */
      astMapGet0D( keymap, "CHITOL", &chitol );

      if( chitol <= 0 ) {
        *status = SAI__ERROR;
        msgSetd("CHITOL",chitol);
        errRep(FUNC_NAME,
               FUNC_NAME ": CHITOL is ^CHITOL, must be > 0", status);
      }
    }

    /* Do iterations completely in memory - minimize disk I/O */
    if( *status == SAI__OK ) {
      int fcount;
      int nflags;
      char flagnames[SMF__NQBITS*SMF_QSTR_MAX];
      char *flagname;

      astMapGet0I( keymap, "MEMITER", &memiter );

      if( memiter ) {
        msgOutif(MSG__VERB, " ",
                 FUNC_NAME ": MEMITER set; perform iterations in memory",
                 status );
      } else {
        msgOutif(MSG__VERB, " ",
                 FUNC_NAME ": MEMITER not set; perform iterations on disk",
                 status );

        /* Should temporary .DIMM files be deleted at the end? */
        astMapGet0I( keymap, "DELDIMM", &deldimm );
      }

      /* Are we going to produce single-bolo maps? */
      astMapGet0I( keymap, "BOLOMAP", &bolomap );

      /* Are we going to produce maps for each iteration? */
      astMapGet0I( keymap, "ITERMAP", &itermap );

      /* Are we going to produce short maps every SHORTMAP time slices? */
      astMapGet0I( keymap, "SHORTMAP", &shortmap );

      /* Are we going to produce flagmaps? */
      if( astMapGet1C(keymap, "FLAGMAP", SMF_QSTR_MAX, SMF__NQBITS, &nflags,
                      flagnames) ) {

        /* Convert each string into a bit, and OR them together */
        for( fcount=0; fcount<nflags; fcount++ ) {
          flagname = flagnames+fcount*SMF_QSTR_MAX;
          flagmap |= smf_qual_str_to_val( flagname, NULL, status );
        }
      }

      /* Are we going to produce SAMPCUBES extensions? */
      astMapGet0I( keymap, "SAMPCUBE", &sampcube );

      /* Are we going to apply the flatfield when we load data? */
      astMapGet0I( keymap, "ENSUREFLAT", &ensureflat );

      /* Are we going to set bad values in exported models? */
      astMapGet0I( keymap, "NOEXPORTSETBAD", &noexportsetbad );

      /* Flag for performing data pre-processing */
      astMapGet0I( keymap, "DOCLEAN", &doclean );

      /* Flag for exporting data right after ckeaning */
      astMapGet0I( keymap, "EXPORTCLEAN", &exportclean );

      /* Method to use for calculating the variance map */
      astMapGet0I( keymap, "VARMAPMETHOD", &varmapmethod );

      if( varmapmethod ) {
        msgOutif(MSG__VERB, " ",
                 FUNC_NAME ": Will use sample variance to estimate"
                 " variance map", status );
      } else {
        msgOutif(MSG__VERB, " ",
                 FUNC_NAME ": Will use error propagation to estimate"
                 " variance map", status );
      }

      /* Are we downsampling the data? */
      astMapGet0D( keymap, "DOWNSAMPSCALE", &downsampscale );

      if( (*status == SAI__OK) && (downsampscale < 0) ) {
        /* If the user specified a value less than 0, the scale is
           a multiple of PIXSIZE, which is obtained from the map header.
           We fudge a local map smfData so that we can call
           smf_map_getpixsize */

        smfData *localmap = NULL;
        double pixsize;

        localmap = smf_create_smfData( 0, status );

        if( localmap && (localmap->hdr) ) {
          localmap->hdr->wcs = outfset;
          memcpy( localmap->lbnd, lbnd_out, sizeof(localmap->lbnd) );

          pixsize = smf_map_getpixsize( localmap, status );

          if( *status == SAI__OK ) {
            downsampscale = abs(downsampscale) * pixsize;
            astMapPut0D( keymap, "DOWNSAMPSCALE", downsampscale, NULL );
          }

          /* Set the WCS to null again to avoid freeing the memory */
          localmap->hdr->wcs = NULL;
        }

        if( localmap ) smf_close_file( &localmap, status );
      }

      msgOutf( "", FUNC_NAME
               ": will down-sample data to match angular scale of %lg "
               "arcsec", status, downsampscale );

      /* Adding in signal from an external fakemap? */
      astMapGet0C( keymap, "FAKEMAP", &tempstr );
      if( tempstr ) {
        fakemap = astCalloc( 255, 1 );
        one_strlcpy( fakemap, tempstr, 255, status );

        if( !memiter ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": fakemap not supported if memiter=0!",
                  status );
        }
      }

      astMapGet0D( keymap, "FAKESCALE", &fakescale );
    }

    /* Obtain sample length from header of first file in igrp */
    smf_open_file( igrp, 1, "READ", SMF__NOCREATE_DATA, &data, status );
    if( (*status == SAI__OK) && (data->hdr) ) {
        steptime = data->hdr->steptime;
    } else {
        steptime = -1;
    }
    smf_close_file( &data, status );

    /* Maximum length of a continuous chunk */
    if( *status == SAI__OK ) {
      astMapGet0D( keymap, "MAXLEN", &maxlen );

      if( maxlen < 0.0 ) {
        /* Trap negative MAXLEN */
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Negative value for MAXLEN supplied.", status);
      }
    }

    /* Type and order of models to fit from MODELORDER keyword */
    havenoi = 0;
    haveext = 0;

    if( *status == SAI__OK ) {
      astMapGet1C(keymap, "MODELORDER", 4, SMF_MODEL_MAX, &nm, modelnames);
      nmodels = (dim_t) nm;

      /* Allocate modeltyps */
      if( nmodels >= 1 ) {
        modeltyps = astCalloc( nmodels, sizeof(*modeltyps) );
        /* Extra components for exportNDF_which for 'res', 'qua' */
        exportNDF_which = astCalloc( nmodels+2, sizeof(*exportNDF_which) );
      } else {
        msgOut(" ", FUNC_NAME ": No valid models in MODELORDER",
               status );
      }

      /* Loop over names and figure out enumerated type */
      for( i=0; (*status==SAI__OK)&&(i<nmodels); i++ ) {
        modelname = modelnames+i*4; /* Pointer to current name */
        thismodel = smf_model_gettype( modelname, status );

        if( *status == SAI__OK ) {
          modeltyps[i] = thismodel;

          /* set haveast/whichast */
          if( thismodel == SMF__AST ) {
            haveast = 1;
            whichast = i;
          }

          /* set havenoi/whichnoi */
          if( thismodel == SMF__NOI ) {
            havenoi = 1;
            whichnoi = i;
          }

          /* set haveext/whichext */
          if( thismodel == SMF__EXT ) {
            haveext = 1;
            whichext = i;
          }

          /* set havegai/whichgai */
          if( thismodel == SMF__GAI ) {
            havegai = 1;
            whichgai = i;
          }
        }
      }
    }

    /* If !havenoi can't measure convergence, so turn off untilconverge */
    if( !havenoi ) {
      untilconverge = 0;
    }

    /* Fail if no AST model component was specified */
    if( (*status == SAI__OK) && !haveast ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": AST must be member of MODELORDER in config file!",
             status);
    }

    /* Will we export components to NDF files at the end? */
    if( (*status==SAI__OK) && astMapHasKey( keymap, "EXPORTNDF" ) ){
      /* There are two possibilities: (i) the user specified a "1" or
         a "0" indicating all or none of the models should be
         exported; or (ii) a vector containing the 3-character model
         names for each component that will be exported (same syntax as
         modelorder) */

      astMapGet1C(keymap, "EXPORTNDF", 4, SMF_MODEL_MAX, &nm, modelnames);
      /* Re-use variables used to parse MODELORDER. If there is a
         single element check to see if it is a single digit for the
         0/1 case. Otherwise try to find matches to each of the parsed
         MODELORDER components */

      if( (nm==1) && (strlen(modelnames)<3) ) {
        if( strtol(modelnames,NULL,10) == 1 ) {
          /* Export all of the model components */
          exportNDF = 1;
          for( i=0; (*status==SAI__OK)&&(i<=nmodels); i++ ) {
            exportNDF_which[i] = 1;
          }
        }
      } else {
        /* Selectively export components */
        for( ii=0; (*status==SAI__OK)&&(ii<nm); ii++ ) {
          modelname = modelnames+ii*4; /* Pointer to current name */
          thismodel = smf_model_gettype( modelname, status );

          /* Check to see if thismodel matches something in modeltyps */
          for( j=0; (*status==SAI__OK)&&(j<nmodels); j++ ) {
            if( thismodel == modeltyps[j] ) {
              /* Found a hit -- export this model component */
              exportNDF = 1;
              exportNDF_which[j] = 1;

              /* Need to export RES as well if NOI is specified
                 since it becomes the variance component of the
                 residual file */
              if( thismodel == SMF__NOI ) {
                exportNDF_which[nmodels]=1;
              }
            }
          }

          /* If the model type is 'qua' handle it here */
          if( thismodel == SMF__QUA ) {
            exportNDF = 1;
            /* qua will be attached to any 3d model component */
            exportNDF_which[nmodels+1]=1;
          }

          /* If the model type is 'res' handle it here */
          if( thismodel == SMF__RES ) {
            exportNDF = 1;
            exportNDF_which[nmodels]=1;
          }
        }
      }
    }
  }

  if( untilconverge ) {
    msgSeti("MAX",maxiter);
    msgOut(" ",
           FUNC_NAME ": Iterate to convergence (max ^MAX)",
           status );
    msgSetd("CHITOL",chitol);
    msgOut(" ",
           FUNC_NAME ": Stopping criteria is a change in chi^2 < ^CHITOL",
           status);
  } else {
    msgSeti("MAX",maxiter);
    msgOut(" ", FUNC_NAME ": ^MAX Iterations", status );
  }

  msgSeti("NUMCOMP",nmodels);
  msgOutif(MSG__VERB," ",
           FUNC_NAME ": ^NUMCOMP model components in solution: ",
           status);
  for( i=0; i<nmodels; i++ ) {
    msgSetc( "MNAME", smf_model_getname(modeltyps[i], status) );
    msgOutif(MSG__VERB,
             " ", "  ^MNAME", status );
  }



















  /* ***************************************************************************
     Figure out how the data are split up, both in terms of continuous
     pieces of data in time, and in terms of the number of subarrays. Divide
     up the chunks, as needed, to fit in the available memory. Also load
     in the fakemap if required.
  *************************************************************************** */

  /* Create an ordered smfGrp which keeps track of files corresponding
     to different subarrays (observed simultaneously), as well as
     time-ordering the files. Now added "chunk" to smfGroup as well --
     this is used later to concatenate _only_ continuous pieces of
     data. Maxconcat will be the length of the largest continuous
     chunk, or maxlen, whichever comes first -- but excluding padding. */

  smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    &groupsubarray, NULL, NULL, NULL, status );

  smf_grp_related( igrp, isize, 1+groupsubarray, 1, maxlen, &srate_maxlen,
                   keymap, &maxconcat, &maxfile, &igroup, NULL, &pad, status );

  if( srate_maxlen <= 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": error, sample rate used to convert maxlen_s to "
            "number of samples is <= 0", status );
  }

  /* Once we've run smf_grp_related we know how many subarrays there
     are.  We also know the maximum length of a concatenated piece of
     data, and which model components were requested. Use this
     information to check that enough memory is available -- but now
     add in the extra length required for padding. */

  if( *status == SAI__OK ) {
    size_t mapmem;

    /* Add on the padding */
    maxconcat += 2*pad;
    msgOutiff( MSG__VERB," ", FUNC_NAME ": Each time stream will be padded "
              "with %" DIM_T_FMT "  samples at start and end.", status, pad );

    /* First check memory for the map */
    smf_checkmem_map( lbnd_out, ubnd_out, 0, maxmem, &mapmem, status );

    /* Then the iterative components that are proportional to time */
    smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
                       nmodels, msize, keymap, maxmem-mapmem, maxfile,
                       &memneeded, status );

    if( *status == SMF__NOMEM ) {
      /* If we need too much memory, generate a warning message and then try
         to re-group the files using smaller contchunks */

      errAnnul( status );
      msgOutf( " ", FUNC_NAME ": *** WARNING ***\n  %zu continuous samples "
               "(%lg s, including padding) require %zu MiB > %zu MiB",
               status, maxconcat, maxconcat/srate_maxlen,
               memneeded/SMF__MIB, maxmem/SMF__MIB);

      /* Try is meant to be the largest contchunk of ~equal length
         that fits in memory. The first step uses the ratio of
         requested to available memory (rounded up to an integral
         number) to estimate the number of time steps for try. */

      try = (size_t) ceil(maxconcat / ceil((double)memneeded/(double)maxmem));

      /* Then figure out how many files this corresponds to, round up
         to get integral number of files, and finally multiply by file
         length and add on padding to get back into time steps */

      try = (size_t) ceil((double)try/(double)maxfile)*maxfile + pad;

      /*  If we exceed available memory subtract off the length of
          one file. If we don't have enough memory even for one input
          file we're hooped. */

      if( (try > (maxconcat*( (double) maxmem / (double) memneeded ))) &&
          (try > maxfile) ) {
        try -= maxfile;
      }

      if( try < (maxfile + pad) ) {
        *status = SMF__NOMEM;
        errRep( "", FUNC_NAME ": not enough memory available to break job "
                "into smaller pieces.", status );
      }

      msgOutf( "", "  Will try to re-group data in chunks < %zu samples long "
               "(%lg s)",
               status, try, ceil((double)try/srate_maxlen) );
      msgOut( " ", FUNC_NAME ": ***************", status );

      /* Close igroup if needed before re-running smf_grp_related */

      if( igroup ) {
        smf_close_smfGroup( &igroup, status );
      }

      smf_grp_related( igrp, isize, 1+groupsubarray, 1,
                       ceil((double)try/srate_maxlen), NULL, keymap,
                       &maxconcat, NULL, &igroup, NULL, NULL, status );

      /* Re-check memory usage using shorter chunks */

      smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
                         nmodels, msize, keymap, maxmem-mapmem, maxfile,
                         &memneeded, status );

    }

    msgOutf( "", FUNC_NAME ": map-making requires %zu MiB "
             "(map=%zu MiB model calc=%zu MiB)", status,
             (mapmem+memneeded)/SMF__MIB, mapmem/SMF__MIB, memneeded/SMF__MIB );
  }

  if( *status == SAI__OK ) {

    if( memiter ) {
      /* Single concatenated conchunk within the iteration loop, so no need
         to loop over files when memiter=1. */
      nfilegroups = 1;

      /* however, there are multiple large continuous pieces outside the
         iteration loop */
      ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
    } else {
      /* Otherwise number of files/subarray is just number of subbgroups in the
         input group */
      nfilegroups = igroup->ngroups;

      /* No looping over larger continuous chunks outside the iteration loop */
      ncontchunks = 1;
    }

    if( memiter ) {
      msgSeti( "NCONTCHUNKS", ncontchunks );
      msgOutif(MSG__VERB," ",
               FUNC_NAME ": ^NCONTCHUNKS large continuous chunks outside"
               " iteration loop.", status);
    } else {
      msgSeti( "NFILEGROUPS", nfilegroups );
      msgOutif(MSG__VERB, " ", FUNC_NAME
               ": ^NFILEGROUPS filegroups (files/subarray) inside iteration "
               "loop.", status);
    }
  }

  /* Load in the fakemap */
  if( fakemap && (*status==SAI__OK) ) {
    Grp *fname=NULL;
    int nfdims;
    int fdims[NDF__MXDIM];
    int nmap;
    void *ptr;

    msgOutf( "", FUNC_NAME ": loading external fakemap `%s'", status, fakemap );

    fname = grpNew( "fakemapfilename", status );
    grpPut1( fname, fakemap, 0, status );

    ndgNdfas( fname, 1, "READ", &fakendf, status );
    ndfDim( fakendf, NDF__MXDIM, fdims, &nfdims, status );

    /* Ensure that the map dimensions are correct */
    if( *status == SAI__OK ) {
      if( !((nfdims == 2) ||
            ((nfdims == 3) && (fdims[2] == 1))) ) {
        *status = SAI__ERROR;
        errRepf( "", FUNC_NAME
                 ": supplied fakemap %s is not 2-dimensional!",
                 status, fakemap );
      } else if((fdims[0] != (ubnd_out[0]-lbnd_out[0]+1)) ||
                (fdims[1] != (ubnd_out[1]-lbnd_out[1]+1))) {
        *status = SAI__ERROR;
        errRepf( "", FUNC_NAME ": supplied fakemap %s does not have the "
                 "required dimensions %i x %i", status, fakemap,
                 ubnd_out[0]-lbnd_out[0]+1, ubnd_out[1]-lbnd_out[1]+1 );
      }
    }

    /* Map the data as double precision */
    ndfMap( fakendf, "DATA", "_DOUBLE", "READ", &ptr, &nmap, status );
    fmapdata = ptr;

    if( fname ) grpDelet( &fname, status );
  }















  /* ***************************************************************************
     Start the main outer loop over continuous chunks, or "contchunks".

     There are two loops over files apart from the iteration
     number. In the memiter=1 case the idea is to concatenate all
     continuous data into several large chunks, and iterate each one
     of those to completion without any file i/o. These are called
     "contchunk". Inside the iteration loop, if memiter=0, loop over
     each group of files per subarray, per time chunk of the DA
     machines; those are called "filegroups". In this latter case
     makemap does lots of file i/o, gives a poorer map solution, but
     runs with less memory.
  *************************************************************************** */

  for( contchunk=0; contchunk<ncontchunks; contchunk++ ) {

    size_t ntgood = 0;       /* Number of good time slices in this chunk */
    size_t nsamples = 0;     /* Number of good samples in this chunk */

    smfArray *noisemaps=NULL;/* Array of noise maps for current chunk */


#ifdef __ITERATEMAP_SHOW_MEM
    _smf_iteratemap_showmem(status);
#endif

    if( memiter ) {
      msgSeti("CHUNK", contchunk+1);
      msgSeti("NUMCHUNK", ncontchunks);
      msgOut( " ",
              FUNC_NAME ": Continuous chunk ^CHUNK / ^NUMCHUNK =========",
              status);
    }

    /*** TIMER ***/
    smf_timerinit( &tv1, &tv2, status );

    if( *status == SAI__OK ) {

      /* Setup the map estimate from the current contchunk. */
      if( contchunk == 0 ) {
        /* For the first continuous chunk, calculate the map
           in-place */
        thismap = map;
        thishits = hitsmap;
        thisqual = mapqual;
        thisvar = mapvar;
        thisweight = weights;
        mapweightsq = astMalloc( msize*sizeof(*mapweightsq) );
        thisweightsq = mapweightsq;
      } else if( contchunk == 1 ) {
        /* Subsequent continuous chunks are done in new map arrays and
           then added to the first */
        thismap = astCalloc( msize, sizeof(*thismap) );
        thishits = astCalloc( msize, sizeof(*thishits) );
        thisqual = astCalloc( msize, sizeof(*thisqual) );
        thisvar = astCalloc( msize, sizeof(*thisvar) );
        thisweight = astCalloc( msize, sizeof(*thisweight) );
        thisweightsq = astCalloc( msize, sizeof(*thisweightsq) );
      }

      if( memiter ) {

        /* If memiter=1 concat everything in this contchunk into a
           single smfArray. Note that the pointing LUT gets generated in
           smf_concat_smfGroup below. */

        msgSeti("C",contchunk+1);
        msgOutif(MSG__VERB," ",
                 FUNC_NAME ": Concatenating files in continuous chunk ^C",
                 status);

        /* Allocate length 1 array of smfArrays. */
        res = astCalloc( nfilegroups, sizeof(*res) );

        /* Concatenate (no variance since we calculate it ourselves -- NOI) */
        smf_concat_smfGroup( wf, igroup, darks, bbms, flatramps, heateffmap, contchunk,
                             ensureflat, 0, outfset, moving, lbnd_out,
                             ubnd_out, pad, pad, SMF__NOCREATE_VARIANCE, tstep,
                             &res[0], NULL, status );

        /* Assign each time slice to a scan angle bin */
        if (*status == SAI__OK) {
          smf_find_thetabins( res[0]->sdata[0], 1, &thetabins, &thetabincen,
                              &nthetabin, &whichthetabin, status );
        }

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": ** %f s concatenating data",
                   status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
        _smf_iteratemap_showmem(status);
#endif

      } else {
        if( !ensureflat ) {
          msgOut( "", FUNC_NAME ": *** WARNING: ensureflat=0 not supported "
                  "if memiter set. Data will be flat-fielded! ***", status );
        }
      }
    }

    /* Allocate space for the chisquared array */
    if( havenoi && (*status == SAI__OK) ) {
      chisquared = astCalloc( nfilegroups, sizeof(*chisquared) );
      lastchisquared = astCalloc( nfilegroups, sizeof(*chisquared) );
    }

    /* Create containers for time-series model components******************* */

    msgOutif(MSG__VERB," ", FUNC_NAME ": Create model containers", status);


    /* Allocate pointers to dynamically created models */
    if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

      /* nmodel array of pointers to nfilegroups smfArray pointers */
      model = astCalloc( nmodels, sizeof(*model) );

      if( memiter != 1 ) {
        /* Array of smfgroups (one for each dynamic model component) */
        modelgroups = astCalloc( nmodels, sizeof(*modelgroups) );
      }

      for( i=0; i<nmodels; i++ ) {
        model[i] = astCalloc( nfilegroups, sizeof(**model) );
      }

    }


    /* Components that always get made */
    if( igroup && (*status == SAI__OK) ) {

      /* there is one smfArray for LUT, AST and QUA for each filegroup */
      lut = astCalloc( nfilegroups, sizeof(*lut) );
      qua = astCalloc( nfilegroups, sizeof(*qua) );

      if( memiter ) {
        /* If iterating in memory then RES has already been created from
           the concatenation of the input data. Create the other
           required models using res[0] as a template. Assert
           bolo-ordered data although the work has already been done at
           the concatenation stage. */

        smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap, NULL,
                          nfilegroups, SMF__QUA, 0, NULL, 0, NULL, NULL,
                          NULL, memiter, memiter, qua, keymap, status );

        /* Associate quality with the res model, and do cleaning
           before we start using more memory for other things. Note that
           we are guaranteed to have only one filegroup if memiter=1. If
           memiter=0 we do the cleaning the first time RES and QUA are
           opened in the first iteration instead. Once the QUA model
           has been initialized with a copy of the quality inside RES,
           we can free up the quality in RES. */

        for( idx=0; idx<res[0]->ndat; idx++ ) {
          smfData *thisqua = qua[0]->sdata[idx];
          res[0]->sdata[idx]->sidequal = thisqua;
          if( res[0]->sdata[idx]->qual ) {
            res[0]->sdata[idx]->qual = astFree( res[0]->sdata[idx]->qual );
          }
        }


        /* Since a copy of the LUT is open in res[0], use it to initialize
           the LUT model and then free it */

        smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap, NULL,
                          nfilegroups, SMF__LUT, 0, NULL, 0, NULL, NULL,
                          NULL, memiter, memiter, lut, keymap, status );

        for( i=0; (*status==SAI__OK)&&(i<res[0]->ndat); i++ ) {
          if( res[0]->sdata[i] ) {
            smf_close_mapcoord( res[0]->sdata[i], status );
          }
        }

        /* Even though EXT would normally be handled in the dynamic memory
           allocation, do it here explicitly so that we can add the fakemap
           signal to RES before cleaning. Be careful not to re-initialize
           it again later! */

        if( haveext ) {
          smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap, noisemaps,
                            nfilegroups, modeltyps[whichext], 0, NULL, 0, NULL,
                            NULL, NULL, memiter, memiter, model[whichext],
                            keymap, status);
        }

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s creating first set of static models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
        _smf_iteratemap_showmem(status);
#endif

        /* We now have RES, LUT, and EXT loaded into memory. Add fake
           astronomical signal to RES at this stage if requested */

        if( fakemap && fmapdata && (*status==SAI__OK) ) {
          double *resptr=NULL;
          int *lutptr=NULL;
          double *extptr=NULL;

          /* Add in the signal from the map */
          msgOutf( "", FUNC_NAME ": adding signal from external map %s "
                   "(* %lf) to time series", status, fakemap,
                   fakescale );

          for( idx=0; idx<res[0]->ndat; idx++ ) {
            smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL, &dsize,
                          NULL, NULL, status );

            resptr = res[0]->sdata[idx]->pntr[0];
            lutptr = lut[0]->sdata[idx]->pntr[0];

            if( haveext ) {
              /* Version in which we are applying extinction correction */

              extptr = model[whichext][0]->sdata[idx]->pntr[0];

              for( k=0; k<dsize; k++ ) {
                if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                    (extptr[k] > 0) && (fmapdata[lutptr[k]] != VAL__BADD) &&
                    (resptr[k] != VAL__BADD) ) {
                  resptr[k] += fakescale*fmapdata[lutptr[k]] / extptr[k];
                }
              }
            } else {
              /* No extinction correction */

              for( k=0; k<dsize; k++ ) {
                if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                    (fmapdata[lutptr[k]] != VAL__BADD) &&
                    (resptr[k] != VAL__BADD) ) {
                  resptr[k] += fakescale*fmapdata[lutptr[k]];
                }
              }
            }
          }
        }

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s adding fakemap signal to residual",
                   status, smf_timerupdate(&tv1,&tv2,status) );

        /* Do data cleaning */
        if( doclean ) {
          smf_clean_smfArray( wf, res[0], &noisemaps, NULL, NULL, keymap,
                              status );
        } else {
          msgOut( "", FUNC_NAME ": *** Warning *** doclean=0, "
                  "so not pre-conditioning data before map-making",
                  status );
        }

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s pre-conditioning data",
                   status, smf_timerupdate(&tv1,&tv2,status) );

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s creating last set of static models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
        _smf_iteratemap_showmem(status);
#endif

      } else {

        /* If iterating using disk i/o need to create res and other
           model components using igroup as template -- which has
           "nfilegroups" data files per subarray. In this case the
           pointing LUT probably doesn't exist, so give projection
           information to smf_model_create. Also assert bolo-ordered
           template (in this case res). */

        res = astCalloc( nfilegroups, sizeof(*res) );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, heateffmap,
                          NULL, 0, SMF__RES, 0, NULL, 0, NULL, NULL,
                          &resgroup, memiter, memiter, res, keymap, status );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, heateffmap,
                          NULL, 0, SMF__LUT, 0, outfset, moving, lbnd_out, ubnd_out,
                          &lutgroup, memiter, memiter, lut, keymap, status );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, heateffmap,
                          NULL, 0, SMF__QUA, 0, NULL, 0, NULL, NULL,
                          &quagroup, memiter, memiter, qua, keymap, status );

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s creating static models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
        _smf_iteratemap_showmem(status);
#endif

      }
    }

    /* Check units */
    if (*status == SAI__OK && res && res[0]) {
      smfData *tmpdata = res[0]->sdata[0];
      /* Check units are consistent */
      if (tmpdata && tmpdata->hdr) {
        smf_check_units( contchunk+1, data_units, tmpdata->hdr, status);
      }
    }

    /* Dynamic components */
    if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

      for( i=0; i<nmodels; i++ ) {

        if( memiter ) {
          /* Don't do SMF__LUT or SMF__EXT as they were handled earlier */
          if( (modeltyps[i] != SMF__LUT) && (modeltyps[i] != SMF__EXT) ) {
            smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap,
                              noisemaps, nfilegroups, modeltyps[i], 0, NULL, 0, NULL,
                              NULL, NULL, memiter, memiter, model[i], keymap, status);
          }

          /* Associate quality with some models */
          if( modeltyps[i] == SMF__FLT ) {
            for( idx=0; idx<res[0]->ndat; idx++ ) {
              smfData *thisqua = qua[0]->sdata[idx];
              model[i][0]->sdata[idx]->sidequal = thisqua;
            }
          }

        } else {
          smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, heateffmap,
                            noisemaps, 0, modeltyps[i], 0, NULL, 0, NULL, NULL,
                            &modelgroups[i], memiter, memiter, model[i], keymap,
                            status );
        }
      }
    }

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME": ** %f s creating dynamic models",
               status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
    _smf_iteratemap_showmem(status);
#endif

    if( *status == SAI__OK ) {

      /* Stuff pointers into smfDIMMData to pass around to model component
         solvers */
      memset( &dat, 0, sizeof(dat) ); /* Initialize structure */
      dat.res = res;
      dat.qua = qua;
      dat.lut = lut;
      dat.map = thismap;
      dat.hitsmap = thishits;
      dat.mapqual = thisqual;
      dat.mapvar = thisvar;
      dat.mapweight = thisweight;
      dat.mapweightsq = thisweightsq;
      dat.mdims[0] = mdims[0];
      dat.mdims[1] = mdims[1];
      dat.msize = msize;
      dat.outfset = outfset;
      dat.lbnd_out = lbnd_out;
      dat.ubnd_out = ubnd_out;
      dat.chisquared = chisquared;
      if( havenoi ) {
        dat.noi = model[whichnoi];
      } else {
        dat.noi = NULL;
      }
      if( haveext ) {
        dat.ext = model[whichext];
      } else {
        dat.ext = NULL;
      }
      if( havegai ) {
        dat.gai = model[whichgai];
      } else {
        dat.gai = NULL;
      }

      /* We can close noisemaps here because they will already have
         been used to initialize the NOI model if needed. */

      if( noisemaps ) smf_close_related( &noisemaps, status );

















      /* ***********************************************************************
         Start the main iteration loop.

         At this stage the full pointing solution for the data in this
         continuous chunk has been calculated, and all of the model
         containers have been created. This loop stops either if chi^2
         has converged, or we did the requested number of iterations,
         for this continuous chunk.
      *********************************************************************** */

      /* Initialize quit to -1. Once one of the stopping criterion have
         been met set to 0 and do one final loop, then set to 1 at the
         end of the last loop to exit. */
      quit = -1;
      iter = 0;

      while( quit < 1 ) {
        msgSeti("ITER", iter+1);
        msgSeti("MAXITER", maxiter);
        msgOut(" ",
               FUNC_NAME ": Iteration ^ITER / ^MAXITER ---------------",
               status);

        /* Assume we've converged until we find a filegroup that hasn't */
        if( iter > 0 ) {
          converged = 1;
        } else {
          converged = 0;
        }

















        /* *********************************************************************
           Start the inner loop over filegroups, and the calculation of
           all the model components up to AST (including models that follow
           AST in modelorder, and then wrapping back around to the beginning).
           There is only a single pass through this loop if memiter=1.
        ********************************************************************* */

        for( i=0; i<nfilegroups; i++ ) {

          if( !memiter ) {
            msgSeti("FILEGROUP", i+1);
            msgSeti("NFILEGROUPS", nfilegroups);
            msgOut(" ", FUNC_NAME ": File group ^FILEGROUP / ^NFILEGROUPS",
                   status);
          }

          /* Open model files here if looping on-disk. Otherwise everything
             is already open from the smf_model_create calls */

          if( !memiter ) {

            /* If memiter not set open this filegroup here */
            smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
            smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );

            /* Associate quality model with the res model */
            for( idx=0; idx<res[i]->ndat; idx++ ) {
              smfData *thisqua = qua[i]->sdata[idx];
              res[i]->sdata[idx]->sidequal = thisqua;
            }

            /* If memiter=0, and if this is the first iteration,
               pre-condition the data since it hasn't yet been
               done. Do it before the other models are opened to
               reduce memory usage. In the memiter=1 case we already
               did pre-conditioning right after the concatenation. */

            if( iter==0 ) {
              if( doclean ) {
                msgOut(" ", FUNC_NAME ": Pre-conditioning data", status);
                smf_clean_smfArray( wf, res[i], NULL, NULL, NULL, keymap,
                                    status );
              } else {
                msgOut( "", FUNC_NAME ": *** Warning *** doclean=0, "
                        "so not pre-conditioning data before map-making",
                        status );
              }

              /*** TIMER ***/
              msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                         ": ** %f s pre-conditioning data",
                         status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
              _smf_iteratemap_showmem(status);
#endif
            }

            /* Continue with other model components */
            smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_open_related_model( modelgroups[j], i, "UPDATE",
                                      &model[j][i], status );
            }

            /*** TIMER ***/
            msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                       ": ** %f s opening model files",
                       status, smf_timerupdate(&tv1,&tv2,status) );
          }

          /* If first iteration report on initial stats and write out
             cleaned data if requested. */
          if( iter == 0 ) {

            /* initial quality report */
            smf_qualstats_report( MSG__NORM, SMF__QFAM_TSERIES, 1, qua[i],
                                  qcount_last, &nsamples, 1, &ntgood, &numdata,
                                  status );

            /* If no good bolos left, set status */
            if( (*status==SAI__OK) &&
                (qcount_last[smf_qual_to_bit(SMF__Q_BADB,status)] >= numdata)) {
              *status = SMF__INSMP;
              errRep("", FUNC_NAME ": All bolos are bad", status );
            }

            /* Export the cleaned data here if desired */
            if( exportclean ) {
              msgOut( "", FUNC_NAME
                      ": Writing out clean data prior to map-making.",
                      status );

              for( idx=0; idx<res[i]->ndat; idx++ ) {
                int oldorder;
                data = res[i]->sdata[idx];

                /* create a file name with "_res_cln" suffix */
                smf_stripsuffix( res[i]->sdata[idx]->file->name,
                                 SMF__DIMM_SUFFIX, name, status );

                if( memiter ) {
                  /* if memiter=1, need to append "_res" to the name */
                  one_strlcat( name, "_res", SMF_PATH_MAX+1, status );
                }

                one_strlcat( name, "_cln", SMF_PATH_MAX+1, status );

                /* Use the correct order */
                oldorder = res[i]->sdata[idx]->isTordered;
                smf_dataOrder( res[i]->sdata[idx], 1, status );
                smf_dataOrder( qua[i]->sdata[idx], 1, status );

                smf_write_smfData( res[i]->sdata[idx], NULL,
                                   name, NULL, 0, NDF__NOID,
                                   MSG__VERB, status );

                /* Revert the order */
                smf_dataOrder( res[i]->sdata[idx], oldorder, status );
                smf_dataOrder( qua[i]->sdata[idx], oldorder, status );
              }
            }
          }

          msgOut(" ",
                 FUNC_NAME ": Calculate time-stream model components",
                 status);

          /* Call the model calculations in the desired order */
          if( *status == SAI__OK ) {

            /* If this is the first iteration just do all of the models up
               to AST. Subsequent iterations start at the first model after
               AST, and then loop back to the start */

            if( iter == 0 ) {
              l = 0;
            } else {
              l = whichast+1;
            }

            for( k=l; (*status==SAI__OK)&&((k%nmodels)!=whichast); k++ ) {
              /* Which model component are we on */
              j = k%nmodels;

              /* If this is the first model component and not the first
                 iteration, we need to undo EXTinction and GAIn (if used
                 for flatfielding) so that RES/NOI are in the correct
                 units. */

              if( (j==0) && (iter>0) ) {
                if( haveext ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing EXTinction from previous iteration",
                             status );
                  smf_calcmodel_ext( wf, &dat, i, keymap, model[whichext],
                                     SMF__DIMM_INVERT, status );
                }

                /*** TIMER ***/
                msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                           ": ** %f s undoing EXT",
                           status, smf_timerupdate(&tv1,&tv2,status) );

                if( havegai ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing GAIn from previous iteration",
                             status );
                  smf_calcmodel_gai( wf, &dat, i, keymap, model[whichgai],
                                     SMF__DIMM_INVERT, status );
                }

                /*** TIMER ***/
                msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                           ": ** %f s undoing GAI",
                           status, smf_timerupdate(&tv1,&tv2,status) );
              }

              /* Message stating which model we're in */
              msgSetc("MNAME", smf_model_getname(modeltyps[j],status));
              msgOutif(MSG__VERB," ", "  ^MNAME", status);
              modelptr = smf_model_getptr( modeltyps[j], status );

              /* Set up control flags for the model calculation */
              dimmflags = 0;

              if( iter==0 ) dimmflags |= SMF__DIMM_FIRSTITER;

              if( (iter==1) && (j>whichast) ) {
                /* In the case that AST is not the last model component,
                   the last models will not be calculated for the first time
                   until the start of the second iteration. */
                dimmflags |= SMF__DIMM_FIRSTITER;
              }

              if( *status == SAI__OK ) {
                (*modelptr)( wf, &dat, i, keymap, model[j], dimmflags, status );
              }

              /*** TIMER ***/
              msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                         ": ** %f s calculating model %s",
                         status, smf_timerupdate(&tv1,&tv2,status),
                         smf_model_getname(modeltyps[j], status) );

#ifdef __ITERATEMAP_SHOW_MEM
              _smf_iteratemap_showmem(status);
#endif
            }
          }

          /* Once all the other model components have been calculated put the
             previous iteration of AST back into the residual. Note that
             even though we've moved signals out from the time streams into
             the map we don't zero AST here so that we can see how much it
             has changed within smf_calcmodel_ast. */

          msgOut(" ", FUNC_NAME ": Rebin residual to estimate MAP",
                 status);

          if( *status == SAI__OK ) {

            /* Ensure we use the RES model ordering */
            smf_model_dataOrder( &dat, NULL, i, SMF__RES|SMF__LUT|SMF__QUA,
                                 res[i]->sdata[0]->isTordered, status );

            /* Loop over subgroup index (subarray) */
            for( idx=0; idx<res[i]->ndat; idx++ ) {

              /* Add last iter. of astronomical signal back in to residual */
              res_data = (res[i]->sdata[idx]->pntr)[0];
              lut_data = (lut[i]->sdata[idx]->pntr)[0];
              qua_data = (qua[i]->sdata[idx]->pntr)[0];

              if( havenoi ) {
                var_data = (dat.noi[i]->sdata[idx]->pntr)[0];
              } else {
                var_data = NULL;
              }

              smf_get_dims( res[i]->sdata[idx], NULL, NULL, NULL, NULL, &dsize,
                            NULL, NULL, status );

              for( k=0; k<dsize; k++ ) {
                if( !(qua_data[k]&SMF__Q_MOD) && (lut_data[k]!=VAL__BADI) ) {
                  double ast_data = map[lut_data[k]];
                  if( ast_data != VAL__BADD ) {
                    res_data[k] += ast_data;
                  }
                }
              }

              /* Setup rebin flags */
              rebinflags = 0;
              if( (i == 0) && (idx == 0) ) {
                /* First call to rebin clears the arrays */
                rebinflags = rebinflags | AST__REBININIT;
              }

              if( (i == nfilegroups-1) && (idx == res[i]->ndat-1) ) {
                /* Final call to rebin re-normalizes */
                rebinflags = rebinflags | AST__REBINEND;
              }

              /* Rebin the residual + astronomical signal into a map */
              smf_rebinmap1( res[i]->sdata[idx],
                             dat.noi ? dat.noi[i]->sdata[idx] : NULL,
                             lut_data, 0, 0, 0, NULL, 0, SMF__Q_GOOD,
                             varmapmethod, rebinflags, thismap, thisweight,
                             thisweightsq, thishits, thisvar, msize, &scalevar,
                             status );
            }

            /*** TIMER ***/
            msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                       ": ** %f s rebinning map",
                       status, smf_timerupdate(&tv1,&tv2,status) );

            /* If storing each iteration in an extension do it here if this
               was the last filegroup of data to be added */

            if( (itermap > 0) && (i == nfilegroups-1) ) {
              smf_write_itermap( thismap, thisvar, msize, iterrootgrp,
                                 contchunk, iter, lbnd_out, ubnd_out,
                                 outfset, res[i]->sdata[0]->hdr, qua[i],
                                 status );

              /*** TIMER ***/
              msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                         ": ** %f s writing itermap",
                         status, smf_timerupdate(&tv1,&tv2,status) );
            }
          }

          /* Close files here if memiter not set */

          if( !memiter ) {
            smf_close_related( &res[i], status );
            smf_close_related( &lut[i], status );
            smf_close_related( &qua[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_close_related( &model[j][i], status );
            }

            /*** TIMER ***/
            msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                       ": ** %f s closing model files",
                       status, smf_timerupdate(&tv1,&tv2,status) );
          }

          /* Set exit condition if bad status was set */
          if( *status != SAI__OK ) i=isize+1;
        }













        /* *********************************************************************
           Calculate the AST model component and check map convergence

           When we get here we have calculated all of the model
           components up to, but not including AST (for all
           filegroups). In addition, the previous iteration of AST has
           been placed back into the residual, and a map has been
           estimated. So, we are now in a position to take the current
           estimate of the map and project it back into the time
           domain to estimate the new AST. Finally, we look at the
           change in chisquared and we compare the current map with
           that of the previous iteration as a convergence tests.
        ********************************************************************* */

        if( *status == SAI__OK ) {
          msgOut(" ", FUNC_NAME ": Calculate ast", status);

          for( i=0; i<nfilegroups; i++ ) {

            /* Open files if memiter not set - otherwise they are still open
               from earlier call */
            if( !memiter ) {
              smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
              smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );
              smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );

              /*** TIMER ***/
              msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                         ": ** %f s opening model files",
                         status, smf_timerupdate(&tv1,&tv2,status) );
            }

            /* Calculate the AST model component. It is a special model
               because it assumes that the map contains the best current
               estimate of the astronomical sky. It gets called in this
               separate loop since the map estimate gets updated by
               each filegroup in the main model component loop */

            dimmflags=0;
            if( quit == 0 ) dimmflags |= SMF__DIMM_LASTITER;
            smf_calcmodel_ast( wf, &dat, i, keymap, NULL, dimmflags, status );

            /*** TIMER ***/
            msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                       ": ** %f s calculating AST",
                       status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
            _smf_iteratemap_showmem(status);
#endif

            /* report on the quality flags for this iterations before closing
             the quality */
            smf_qualstats_report( MSG__NORM, SMF__QFAM_TSERIES, 1, qua[i],
                                  qcount_last, &nsamples, 0, &ntgood, &numdata,
                                  status );

            /* If no good bolos left, set status */
            if( (*status==SAI__OK) &&
                (qcount_last[smf_qual_to_bit(SMF__Q_BADB, status)] >= numdata)){
              *status = SMF__INSMP;
              errRep("", FUNC_NAME ": All bolos are bad", status );
            }

            /* Check for consistency between quality and data arrays */
            for( idx=0; (*status==SAI__OK)&&(idx<res[i]->ndat); idx++ ) {
              size_t nbad;
              nbad = smf_check_quality( res[i]->sdata[idx], 0, status );
              if( nbad ) {
                msgOut( "", FUNC_NAME ": *** Possible programming error! ***",
                        status );
                msgOutf( "", FUNC_NAME ": %zu QUALITY/DATA inconsistencies "
                         "filegroup %zu subarray %zu", status, nbad, i, idx );
                msgOut( "", FUNC_NAME ": ***********************************",
                        status );
              }
            }

            /* Close files if memiter not set */
            if( !memiter ) {
              smf_close_related( &res[i], status );
              smf_close_related( &lut[i], status );
              smf_close_related( &qua[i], status );

              /*** TIMER ***/
              msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                         ": ** %f s closing models files for this filegroup",
                         status, smf_timerupdate(&tv1,&tv2,status) );
            }

            /* If NOI was present, we now have an estimate of chisquared */
            if( (*status==SAI__OK) && chisquared ) {

              if( (iter==0) && (whichnoi>whichast) ) {
                /* If NOI comes after AST in MODELORDER we can't check chi^2 or
                   convergence until next iteration. */
                msgOut( "",
                        FUNC_NAME ": Will calculate chi^2 next iteration",
                        status );
              } else {
                msgSeti("FILEGROUP",i+1);
                msgSetd("CHISQ",chisquared[i]);
                msgOut( " ",
                        FUNC_NAME ": *** CHISQUARED = ^CHISQ for filegroup "
                        "^FILEGROUP", status);

                if( ((iter > 0)&&(whichnoi<whichast)) ||
                    ((iter > 1)&&(whichnoi>whichast)) ) {
                  /* Again, we have to check if NOI was calculated at least
                     twice, which depends on NOI and AST in MODELORDER */

                  double chidiff;   /* temporary variable to store diff */

                  chidiff = chisquared[i]-lastchisquared[i];

                  msgSetd("DIFF", chidiff);
                  msgOut( " ",
                          FUNC_NAME ": *** change: ^DIFF", status );

                  if( chidiff > 0 ) {
                    msgOut( " ", FUNC_NAME
                            ": ****** WARNING! CHISQUARED Increased ******",
                            status );
                  }

                  /* Check for the stopping criterion */
                  if( untilconverge ) {
                    if( (chidiff > 0) || (-chidiff > chitol) ) {
                      /* Found a chunk that isn't converged yet */
                      converged=0;
                    }
                  }
                } else {
                  /* Can't converge until at least 2 consecutive chi^2... */
                  converged=0;
                }

                /* Update lastchisquared */
                lastchisquared[i] = chisquared[i];

              }
            }
          }
        }

        /* Calculate the absolute difference between the previous and
           current map pixels normalized by the map standard
           deviations. Once we're done, update lastmap to the current
           map. Ignore bad and zero-constrained pixels. */

        if( *status == SAI__OK ) {
          mapchange_max = 0;
          for( i=0; i<msize; i++ ) {
            if( !(thisqual[i]&SMF__MAPQ_ZERO) && (thismap[i] != VAL__BADD) &&
                (lastmap[i] != VAL__BADD) && (thisvar[i] != VAL__BADD) &&
                (thisvar[i] > 0) ) {

              mapchange[i] = fabs(thismap[i] - lastmap[i]) / sqrt(thisvar[i]);

              /* Update max */
              if( mapchange[i] > mapchange_max ) mapchange_max = mapchange[i];
            } else {
              mapchange[i] = VAL__BADD;
            }
          }

          /* Calculate the mean change */
          smf_stats1D( mapchange, 1, msize, NULL, 0, 0, &mapchange_mean, NULL,
                       NULL, NULL, status );

          memcpy( lastmap, map, msize*sizeof(*lastmap) );

          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s calculating change in the map",
                     status, smf_timerupdate(&tv1,&tv2,status) );

          msgOutf( "", FUNC_NAME ": *** NORMALIZED MAP CHANGE: %lg (mean) "
                   "%lg (max)", status, mapchange_mean, mapchange_max );
        }

        /* Increment iteration counter */
        iter++;

        if( *status == SAI__OK ) {

          /* If quit was set to 0 last time through we can now exit the
             loop */
          if( quit == 0 ) {
            quit = 1;
          } else {
            /* Check that we will exceed maxiter next time through */
            if( iter > (maxiter-1) ) {
              quit = 1;
            } else if( iter == (maxiter-1) ) {
              quit = 0;
            }

            /* Check for convergence */
            if( untilconverge && converged ) {
              quit = 0;
            }
          }

        } else {
          quit = 1;
        }
      }

      msgSeti("ITER",iter);
      msgOut( " ",
              FUNC_NAME ": ****** Completed in ^ITER iterations", status);
      if( untilconverge ) {
        if( converged ) {
          msgOut( " ",
                  FUNC_NAME ": ****** Solution CONVERGED",
                  status);
        } else {
          msgOut( " ",
                  FUNC_NAME ": ****** Solution did NOT converge",
                  status);

          /* Increment counter of how many chunks did not converge so
             that we can tell the caller */
          count_mcnvg++;
        }
      }










      /* ***********************************************************************
         The continous chunk has finished.

         The model components for this continuous chunk have converged.

         We can now do things like write out the bolomap and shortmap
         extensions, and also export model components (if requested).

         We also add the map estimated from this contchunk to those from
         previous contchunks if necessary.
      *********************************************************************** */

      /* ---------------------------------------------------------------- */
      /* All of bolomap, shortmap and sampcube require AST to be added back
         into RES so that we can re-rebin maps. We can only currently
         do these operations only if memiter is true. */

      if( memiter && (bolomap || shortmap || sampcube) ) {
        for( idx=0; (idx<res[0]->ndat)&&(*status==SAI__OK); idx++ ){
          smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                        &dsize, NULL, NULL, status );

          res_data = res[0]->sdata[idx]->pntr[0];
          lut_data = lut[0]->sdata[idx]->pntr[0];
          qua_data = qua[0]->sdata[idx]->pntr[0];

          /* Add ast back into res. Mask should match ast_calcmodel_ast. */
          for( k=0; k<dsize; k++ ) {
            if( !(qua_data[k]&SMF__Q_MOD) && (lut_data[k]!=VAL__BADI) ) {
              double ast_data = map[lut_data[k]];
              if( ast_data != VAL__BADD ) {
                res_data[k] += ast_data;
              }
            }
          }

        }
      }

      /* Create sub-maps for each bolometer if requested. */

      if( bolomap ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** bolomap=1, but memiter=0", status );
        } else {
          smf_write_bolomap( res[0], lut[0], qua[0], &dat, msize,
                             bolrootgrp, varmapmethod, lbnd_out, ubnd_out,
                             outfset, status );

          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s writing bolomap",
                     status, smf_timerupdate(&tv1,&tv2,status) );
        }
      }

      /* Create short maps using every SHORTMAP samples if requested */

      if( shortmap ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** shortmap=1, but memiter=0", status );
        } else {
          smf_write_shortmap( shortmap, res[0], lut[0], qua[0], &dat,
                              msize, shortrootgrp, contchunk, varmapmethod,
                              lbnd_out, ubnd_out, outfset, status );
          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s writing shortmap",
                     status, smf_timerupdate(&tv1,&tv2,status) );
        }
      }

      /* If we're writing out only the final map from each chunk, do it here */
      if( itermap < 0 ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** shortmap=1, but memiter=0", status );
        } else {
          smf_write_itermap( thismap, thisvar, msize, iterrootgrp,
                             contchunk, iter, lbnd_out, ubnd_out,
                             outfset, res[0]->sdata[0]->hdr, qua[0],
                             status );

          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s writing itermap",
                     status, smf_timerupdate(&tv1,&tv2,status) );
        }
      }

      /* Create a data sample cube where the first two dimensions
         match the map, and the third dimension enumerates samples that
         land in each pixel */

      if( sampcube ) {
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** sampcube=1, but memiter=0",
                  status );
        } else {
          smf_write_sampcube( res[0], lut[0], qua[0], &dat, thishits,
                              samprootgrp, contchunk, lbnd_out, ubnd_out,
                              status );

          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s writing sampcube",
                     status, smf_timerupdate(&tv1,&tv2,status) );
        }
      }

      /* Now we can remove AST from RES again before continuing */

      if( memiter && (bolomap || shortmap || sampcube) ) {
        for( idx=0; (idx<res[0]->ndat)&&(*status==SAI__OK); idx++ ){
          smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                        &dsize, NULL, NULL, status );

          res_data = res[0]->sdata[idx]->pntr[0];
          lut_data = lut[0]->sdata[idx]->pntr[0];
          qua_data = qua[0]->sdata[idx]->pntr[0];

          /* Add ast back into res. Mask should match ast_calcmodel_ast. */
          for( k=0; k<dsize; k++ ) {
            if( !(qua_data[k]&SMF__Q_MOD) && (lut_data[k]!=VAL__BADI) ) {
              double ast_data = map[lut_data[k]];
              if( ast_data != VAL__BADD ) {
                res_data[k] -= ast_data;
              }
            }
          }

        }
      }
      /* ---------------------------------------------------------------- */

      /* Create maps indicating locations of flags matching bitmask */

      if( flagmap ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** shortmap=1, but memiter=0", status );
        } else {
          smf_write_flagmap( flagmap, lut[0], qua[0], &dat, flagrootgrp,
                             contchunk, lbnd_out, ubnd_out, outfset, status );
          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s writing flagmap",
                     status, smf_timerupdate(&tv1,&tv2,status) );
        }
      }

      /* Export DIMM model components to NDF files.
         Note that we don't do LUT since it is originally an extension in the
         input flatfielded data.
         Also - check that a filename is defined in the smfFile! */

      if( exportNDF && ((*status == SAI__OK) || (*status == SMF__INSMP)) ) {

        errBegin( status );

        msgOut(" ", FUNC_NAME ": Export model components to NDF files.",
               status);

        for( i=0; i<nfilegroups; i++ ) {  /* filegroup loop */
            msgSeti("FILEGROUP", i+1);
            msgSeti("NFILEGROUPS", nfilegroups);
            msgOutif(MSG__VERB, "", FUNC_NAME
                     ": File group ^FILEGROUP / ^NFILEGROUPS", status);

          /* Open each filegroup, loop over smfArray elements and
             export, then close filegroup. DIMM open/close not needed
             if memiter set.  Note that QUA and NOI get stuffed into
             the QUALITY and VARIANCE components of the residual. Also
             notice that everything must be changed to time-ordered
             data before writing ICD-compliant files. */

          if( !memiter ) { /* Open if we're doing disk i/o */
            /* Fixed components */
            smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
            smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );

            /* Dynamic components */
            for( j=0; j<nmodels; j++ ) {
              smf_open_related_model( modelgroups[j], i, "UPDATE",
                                      &model[j][i], status );
            }
          }

          /* Loop over subarray, re-order, set bad values wherever a
             SMF__Q_BADB flag is encountered (if requested), and
             export */
          for( idx=0; idx<res[i]->ndat; idx++ ) {
            smf_dataOrder( qua[i]->sdata[idx], 1, status );
            smf_dataOrder( res[i]->sdata[idx], 1, status );
            smf_dataOrder( lut[i]->sdata[idx], 1, status );

            /* Get quality array strides for smf_update_valbad */
            smf_get_dims( qua[i]->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                          NULL, &bstride, &tstride, status );

            for( j=0; j<nmodels; j++ ) {

              /* Check for existence of the model for this subarray - in
                 some cases, like COM, there is only a file for one subarray,
                 unlike RES from which the range of idx is derived */
              if( model[j][i]->sdata[idx] ) {
                smf_dataOrder( model[j][i]->sdata[idx], 1, status );
                if( *status == SMF__WDIM ) {
                  /* fails if not 3-dimensional data. Just annul and write out
                     data as-is. */
                  errAnnul(status);
                  model[j][i]->sdata[idx]->isTordered=1;
                }
              }
            }

            if( *status == SAI__OK ) {
              if( memiter ) {
                /* Pointer to the header in the concatenated data */
                refdata = res[i]->sdata[idx];
              } else {
                /* Open the header of the original input file in memiter=0
                   case since it won't have been stored in the .DIMM files */
                smf_open_file( igrp, resgroup->subgroups[i][idx], "READ",
                               SMF__NOCREATE_DATA, &data, status );
                if( *status == SAI__OK ) {
                  refdata = data;
                }
              }
            }

            /* QUA becomes the quality component of RES. NOI becomes
               the variance component of RES if present. */
            if( *status == SAI__OK ) {
              if( havenoi ) {
                var_data = (model[whichnoi][i]->sdata[idx]->pntr)[0];
              } else {
                var_data = NULL;
              }

              qua_data = (qua[i]->sdata[idx]->pntr)[0];

              if( exportNDF_which[nmodels] ) {
                if( (res[i]->sdata[idx]->file->name)[0] ) {
                  smf_model_createHdr( res[i]->sdata[idx], SMF__RES, refdata,
                                       status );
                  smf_stripsuffix( res[i]->sdata[idx]->file->name,
                                   SMF__DIMM_SUFFIX, name, status );

                  /* if memiter=1, need to append "_res" to the name */
                  if( memiter ) {
                    one_strlcat( name, "_res", SMF_PATH_MAX+1, status );
                  }

                  if( !noexportsetbad ) {
                    smf_update_valbad( res[i]->sdata[idx], SMF__NUL,
                                       NULL, 0, 0, SMF__Q_BADB, status );
                  }

                  smf_write_smfData( res[i]->sdata[idx],
                                     (havenoi && exportNDF_which[whichnoi]) ?
                                     dat.noi[i]->sdata[idx] : NULL,
                                     name, NULL, 0, NDF__NOID,
                                     MSG__VERB, status );
                } else {
                  msgOut( " ",
                          "SMF__ITERATEMAP: Can't export RES -- NULL filename",
                          status);
                }
              }

              if( exportNDF_which[whichast] ) {
                /* Create a smfData to hold the map projected into a data
                   cube and then write it out. */

                if( (res[i]->sdata[idx]->file->name)[0] ) {
                  smfData *ast=NULL;
                  double *ast_data=NULL;
                  const char *astname=NULL;
                  const char *resname=NULL;
                  char workstr[GRP__SZNAM+1];

                  /* Since AST only exists as a time-series model if exporting,
                     we work out the AST container filename based on RES here */

                  astname = smf_model_getname( SMF__AST, status );
                  resname = smf_model_getname( SMF__RES, status );

                  smf_stripsuffix( res[i]->sdata[idx]->file->name,
                                   SMF__DIMM_SUFFIX, workstr, status );
                  smf_stripsuffix( workstr, resname, name, status );

                  one_strlcat( name, "_" , sizeof(name), status );
                  one_strlcat( name, astname, sizeof(name), status );

                  /* Create the smfData, fill it with the projected
                     map data, create its header, associate a quality
                     array, and set bad values */

                  ast = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL,
                                               SMF__DOUBLE, NULL, NULL,
                                               SMF__QFAM_NULL,
                                               qua[i]->sdata[idx], 0, 1,
                                               res[i]->sdata[idx]->dims,
                                               res[i]->sdata[idx]->lbnd,
                                               res[i]->sdata[idx]->ndims,
                                               0, 0, NULL, NULL, status );

                  smf_get_dims( ast, NULL, NULL, NULL, NULL, &dsize,
                                NULL, NULL, status );

                  ast->pntr[0] = astCalloc( dsize, smf_dtype_size(ast,status) );

                  if( *status == SAI__OK ) {
                    ast_data = ast->pntr[0];
                    lut_data = (lut[i]->sdata[idx]->pntr)[0];

                    for( j=0; j<dsize; j++ ) {
                      if( lut_data[j] != VAL__BADI ) {
                        ast_data[j] = map[lut_data[j]];
                      } else {
                        ast_data[j] = VAL__BADD;
                      }
                    }
                  }

                  smf_model_createHdr( ast, SMF__AST, refdata, status );

                  if( !noexportsetbad ) {
                    smf_update_valbad( ast, SMF__NUL, NULL, 0, 0, SMF__Q_BADB,
                                       status );
                  }

                  /* Export AST */
                  smf_write_smfData( ast, NULL, name, NULL, 0, NDF__NOID,
                                     MSG__VERB, status );

                  /* Clean up */
                  smf_close_file( &ast, status );
                } else {
                  msgOut( " ",
                          "SMF__ITERATEMAP: Can't export AST -- NULL filename",
                          status);
                }

              }
            }

            /* Dynamic components excluding NOI/AST */
            for( j=0; j<nmodels; j++ )
              /* Remember to check again whether model[j][i]->sdata[idx] exists
                 for cases like COM */
              if( (*status == SAI__OK) && (modeltyps[j] != SMF__NOI) &&
                  (modeltyps[j] != SMF__AST) && model[j][i]->sdata[idx] &&
                  exportNDF_which[j] ) {
                if( (model[j][i]->sdata[idx]->file->name)[0] ) {

                  smf_model_createHdr( model[j][i]->sdata[idx], modeltyps[j],
                                       refdata,status );
                  smf_stripsuffix( model[j][i]->sdata[idx]->file->name,
                                   SMF__DIMM_SUFFIX, name, status );

                  if( !noexportsetbad ) {
                    smf_update_valbad( model[j][i]->sdata[idx], modeltyps[j],
                                       qua_data, bstride, tstride, SMF__Q_BADB,
                                       status );
                  }

                  /* decide if we're writing quality: has to be requested,
                     and either need to have 3d data array
                     (check array dimensions), or we can supply a collapsed
                     quality in the special case of COM */

                  if( modeltyps[j] == SMF__COM && qua_data ) {
                    smf_qual_t *tempqual = NULL;
                    smfData * com = model[j][i]->sdata[idx];

                    smf_collapse_quality( qua_data, com->qfamily, nbolo,
                                          ntslice, bstride, tstride, 0,
                                          &tempqual, status );

                    com->sidequal = smf_construct_smfData( NULL, NULL, NULL,
                                                           NULL, NULL,
                                                           SMF__QUALTYPE,
                                                           NULL, tempqual,
                                                           SMF__QFAM_TSERIES,
                                                           NULL, 0, 1,
                                                           com->dims,
                                                           com->lbnd,
                                                           com->ndims, 0, 0,
                                                           NULL, NULL, status );
                  }

                  smf_write_smfData( model[j][i]->sdata[idx], NULL,
                                     name, NULL, 0, NDF__NOID,
                                     MSG__VERB, status );

                  /* if we had temporary quality free it */
                  if ( modeltyps[j] == SMF__COM && qua_data ) {
                    smf_close_file( &(model[j][i]->sdata[idx]->sidequal),
                                    status );
                  }

                } else {
                  msgSetc("MOD",smf_model_getname(modeltyps[j], status) );
                  msgOut( " ",
                          "SMF__ITERATEMAP: Can't export ^MOD: NULL filename",
                          status);
                }
              }

            /* Close the input file containing the header */
            if( !memiter ) {
              smf_close_file( &data, status );
            }
          }

          if( !memiter ) { /* Close files if doing disk i/o */
            smf_close_related( &res[i], status );
            smf_close_related( &qua[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_close_related( &model[j][i], status );
            }
          }
        }

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s Exporting models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

        errEnd( status );
      }
    }

#ifdef __ITERATEMAP_SHOW_MEM
    _smf_iteratemap_showmem(status);
#endif

    /* If we get here and there is a SMF__INSMP we probably flagged
       all of the data as bad for some reason. In a multi-contchunk
       map it is annoying to have the whole thing die here. So, annul
       the error, warn the user, and then continue on... This will
       also help us to properly free up resources used by this
       chunk. However, we use the count_minsmp to remember that this
       happened, and will tell the caller. */

    if( *status == SMF__INSMP ) {
      errAnnul( status );
      msgOut(""," ************************* Warning! *************************",
             status );
      msgOut(""," This continuous chunk failed due to insufficient samples.",
              status );
      msgOut(""," This can be due to strict bad-bolo flagging.", status );
      msgOut(""," Another possibility is that the data are bad.", status );
      msgOut(""," Annuling the bad status and trying to continue...", status);
      msgOut(""," ************************************************************",
             status );

      /* Remember how many chunks failed due to lack of samples */
      count_minsmp++;
    } else {
      /* In the multiple contchunk case, add this map to the total if
         we got here with clean status */
      if( contchunk >= 1 ) {
        msgOut( " ", FUNC_NAME ": Adding map estimated from this continuous"
                " chunk to total", status);
        smf_addmap1( map, weights, hitsmap, mapvar, mapqual, thismap,
                     thisweight, thishits, thisvar, thisqual, msize, status );
      }

      /* Add this chunk of exposure time to the total. We assume the array was
         initialised to zero and will not contain bad values. */
      if( *status == SAI__OK ) {
        steptime = res[0]->sdata[0]->hdr->steptime;
        for (i=0; i<msize; i++) {
          if ( thishits[i] != VAL__BADI) {
            exp_time[i] += steptime * (double)thishits[i];
          }
        }
      }

    }




    /* *************************************************************************
       Clean up temporary resources associated with this continuous chunk
       before continuing in the outer loop.
    ************************************************************************* */

    /* Cleanup things used specifically in this contchunk */
    if( !memiter && deldimm ) {
      msgOutif(MSG__VERB," ",
               FUNC_NAME ": Cleaning up " SMF__DIMM_SUFFIX " files",
               status);

      /* Delete temporary .DIMM files if requested */
      for( i=0; i<nfilegroups; i++ ) { /* Loop over filegroup */
        pname = name;

        /* static model components */
        for( j=0; (resgroup)&&(j<resgroup->nrelated); j++ ) {
          grpGet( resgroup->grp, resgroup->subgroups[i][j], 1, &pname,
                  GRP__SZNAM, status );
          if( *status == SAI__OK ) {
            remove(name);
          }
        }

        for( j=0; (lutgroup)&&(j<lutgroup->nrelated); j++ ) {
          grpGet( lutgroup->grp, lutgroup->subgroups[i][j], 1, &pname,
                  GRP__SZNAM, status );
          if( *status == SAI__OK ) {
            remove(name);
          }
        }

        for( j=0; (quagroup)&&(j<quagroup->nrelated); j++ ) {
          grpGet( quagroup->grp, quagroup->subgroups[i][j], 1, &pname,
                  GRP__SZNAM, status );
          if( *status == SAI__OK ) {
            remove(name);
          }
        }

        /* dynamic model components */
        for( k=0; k<nmodels; k++ ) {
          for( j=0; (modelgroups[k])&&(j<(modelgroups[k])->nrelated); j++ ) {
            grpGet( (modelgroups[k])->grp, (modelgroups[k])->subgroups[i][j],
                    1, &pname, GRP__SZNAM, status );
            if( *status == SAI__OK ) {
              remove(name);
            }
          }
        }
      }
    }

    /* fixed model smfGroups */
    if( resgroup ) smf_close_smfGroup( &resgroup, status );
    if( lutgroup ) smf_close_smfGroup( &lutgroup, status );
    if( quagroup ) smf_close_smfGroup( &quagroup, status );

    /* fixed model smfArrays */
    if( res ) {
      for( i=0; i<nfilegroups; i++ ) {
        if( res[i] ) smf_close_related( &res[i], status );
      }
      res = astFree( res );
    }

    if( lut ) {
      for( i=0; i<nfilegroups; i++ ) {
        if( lut[i] ) smf_close_related( &lut[i], status );
      }
      lut = astFree( lut );
    }

    if( qua ) {
      for( i=0; i<nfilegroups; i++ ) {
        if( qua[i] ) smf_close_related( &qua[i], status );
      }
      qua = astFree( qua );
    }

    /* dynamic model smfGroups */
    if( modelgroups ) {
      for( i=0; i<nmodels; i++ ) {
        if( modelgroups[i] ) smf_close_smfGroup( &modelgroups[i], status );
      }

      /* Free array of smfGroup pointers */
      modelgroups = astFree( modelgroups );
    }

    /* dynamic model smfArrays */
    if( model ) {
      for( i=0; i<nmodels; i++ ) {
        if( model[i] ) {
          for( j=0; j<nfilegroups; j++ ) {
            /* Close each model component smfArray for each filegroup */
            if( model[i][j] )
              smf_close_related( &(model[i][j]), status );
          }

          /* Free array of smfArray pointers for this model */
          model[i] = astFree( model[i] );
        }
      }
      model = astFree( model );
    }

    /* Keep track of total number of samples and total number of time slices */
    ntgood_tot += ntgood;
    nsamples_tot += nsamples;

    /* Free chisquared array */
    chisquared = astFree( chisquared );
    lastchisquared = astFree( lastchisquared );

    /* Free scan angle binning arrays */
    thetabincen = astFree( thetabincen );
    thetabins = astFree( thetabins );
    whichthetabin = astFree( whichthetabin );
  }














  /* ***************************************************************************
     Final cleanup

     We have finished the outermost loop over continuous chunks. Free
     all of the memory we allocated.
  *************************************************************************** */

  /* Report the total number of effective bolometers */
  if (nboloeff) *nboloeff = 0.0;
  if (ntgood_tot > 0) {
    msgOutiff(MSG__NORM, "",
              "Total samples available from all chunks: %zu (%g bolos)",
              status, nsamples_tot, (double)nsamples_tot / (double)ntgood_tot );
    if (nboloeff) *nboloeff = (double)nsamples_tot / (double)ntgood_tot;

    /* Set status if no good samples */
    if( (*status==SAI__OK) && (nsamples_tot==0) ) {
      *status = SMF__INSMP;
      errRep("", FUNC_NAME ": No good samples", status );
    }
  }

  /* The second set of map arrays get freed in the multiple contchunk case */
  if( thismap != map ) thismap = astFree( thismap );
  if( thishits != hitsmap ) thishits = astFree( thishits );
  if( thisqual != mapqual ) thisqual = astFree( thisqual );
  if( thisvar != mapvar ) thisvar = astFree( thisvar );
  if( thisweight != weights ) thisweight = astFree( thisweight );
  if( thisweightsq != mapweightsq ) thisweightsq = astFree( thisweightsq );
  mapweightsq = astFree( mapweightsq );

  modeltyps = astFree( modeltyps );
  exportNDF_which = astFree( exportNDF_which );

  if( igroup ) {
    smf_close_smfGroup( &igroup, status );
  }

  if( dat.zeromask ) dat.zeromask = astFree( dat.zeromask );

  fakemap = astFree( fakemap );
  if( fakendf != NDF__NOID ) ndfAnnul( &fakendf, status );

  if( lastmap ) lastmap = astFree( lastmap );
  if( mapchange ) mapchange = astFree( mapchange );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();

  /* Report count_minsmp, count_mcnvg, as well as reporting back
     continuous chunk counters to caller */
  if( count_minsmp || count_mcnvg ) {
    msgOut("","*************** Warning! ***************",
           status );
    msgOutf( "", "Of %zu continuous chunk:", status, ncontchunks );
    if( count_minsmp ) {
      msgOutf( "", "     %zu failed due to insufficient samples", status,
               count_minsmp );
    }
    if( count_mcnvg ) {
      msgOutf( "", "     %zu did not converge", status, count_mcnvg );
    }
    msgOut("","****************************************",
           status );
  }

  if( numcontchunks) *numcontchunks = ncontchunks;
  if( numinsmp ) *numinsmp = count_minsmp;
  if( numcnvg ) *numcnvg = count_mcnvg;
}
