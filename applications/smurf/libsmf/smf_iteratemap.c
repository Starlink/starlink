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

*     smf_iteratemap(smfWorkForce *wf, const Grp *igrp, const Grp *iterrootgrp,
*                    const Grp *bolrootgrp, const Grp *shortrootgrp,
*                    AstKeyMap *keymap, const smfArray * darks,
*                    const smfArray *bbms, const smfArray * flatramps,
*                    AstFrameSet *outfset, int moving, int *lbnd_out,
*                    int *ubnd_out, size_t maxmem, double *map, int *hitsmap,
*                    double *exp_time, double *mapvar, smf_qual_t *mapqual,
*                    double *weights, char data_units[], double *nboloeff,
*                    int *status );

*  Arguments:
*     wf = smfWorkForce * (Given)
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
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
*     darks = const smfArray * (Given)
*        Collection of dark frames. Can be NULL.
*     bbms = smfArray * (Given)
*        Masks for each subarray (e.g. returned by smf_reqest_mask call)
*     flatramps = const smfArray * (Given)
*        Collection of flatfield ramps. Will be passed to
*        smf_open_and_flatfield.
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
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2010 University of British Columbia.
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


#define FUNC_NAME "smf_iteratemap"

/* Main routine */
void smf_iteratemap( smfWorkForce *wf, const Grp *igrp, const Grp *iterrootgrp,
                     const Grp *bolrootgrp, const Grp *shortrootgrp,
                     AstKeyMap *keymap, const smfArray *darks,
                     const smfArray *bbms, const smfArray * flatramps,
                     AstFrameSet *outfset, int moving, int *lbnd_out,
                     int *ubnd_out, size_t maxmem, double *map,
                     int *hitsmap, double * exp_time, double *mapvar,
                     smf_qual_t *mapqual, double *weights, char data_units[],
                     double * nboloeff, int *status ) {

  /* Local Variables */
  smfArray **ast=NULL;          /* Astronomical signal */
  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  smfGroup *astgroup=NULL;      /* smfGroup of ast model files */
  int bolomap=0;                /* If set, produce single bolo maps */
  size_t bstride;               /* Bolometer stride */
  double *chisquared=NULL;      /* chisquared for each chunk each iter */
  double chitol=0;              /* chisquared change tolerance for stopping */
  size_t contchunk;             /* Continuous chunk in outer loop counter */
  int converged=0;              /* Has stopping criteria been met? */
  smfDIMMData dat;              /* Struct passed around to model components */
  smfData *data=NULL;           /* Temporary smfData pointer */
  int deldimm=0;                /* Delete temporary .DIMM files */
  int tstep = 0;                /* Time step between full WCS calculations */
  int dimmflags;                /* Control flags for DIMM model components */
  int doclean=1;                /* Are we doing data pre-processing? */
  dim_t dsize;                  /* Size of data arrays in containers */
  double dtemp;                 /* temporary double */
  int ensureflat=1;             /* flatfield data as they are loaded */
  int exportclean=0;            /* Are we doing to export clean data? */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  int *exportNDF_which=NULL;    /* Which models in modelorder will be exported*/
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
  smfArray **lut=NULL;          /* Pointing LUT for each file */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfGroup *lutgroup=NULL;      /* smfGroup of lut model files */
  double *mapweightsq=NULL;     /* map weight squared */
  dim_t maxconcat;              /* Longest continuous chunk that fits in mem.*/
  dim_t maxfile;                /* Longest file length in time samples*/
  int maxiter=0;                /* Maximum number of iterations */
  dim_t maxlen=0;               /* Max length in timeslices of cont. chunk */
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
  size_t ntgood_tot = 0;        /* Number of good time slices in all chunks */
  dim_t ntslice;                /* Number of time slices */
  size_t numdata;               /* Total number of samples in chunk */
  int numiter=0;                /* Total number iterations */
  dim_t padEnd=0;               /* How many samples of padding at the end */
  dim_t padStart=0;             /* How many samples of padding at the start */
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
  double scalevar=0;            /* scale factor for variance */
  int shortmap=0;               /* If set, produce maps every shortmap tslices*/
  double steptime;              /* Length of a sample in seconds */
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

  /* Get size of the input group */
  isize = grpGrpsz( igrp, status );











  /* ***************************************************************************
     Parse the CONFIG parameters stored in the keymap, and set up
     defaults for the map-maker. We assume that all variables have
     been given defaults through the .def file.
  **************************************************************************** */

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
      astMapGet0D( keymap, "MAXLEN", &dtemp );

      if( dtemp < 0.0 ) {
        /* Trap negative MAXLEN */
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Negative value for MAXLEN supplied.", status);
      } else if( dtemp == 0 ) {
        /* 0 is OK... gets ignored later */
        maxlen = 0;
      } else if( steptime > 0 ) {
        maxlen = (dim_t) (dtemp / steptime);
      } else {
        /* Trap invalid sample length in header */
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Invalid STEPTIME in FITS header.", status);
      }
    }

    /* Padding */
    padStart = padEnd = smf_get_padding( keymap, steptime, 1, status );

    /* Type and order of models to fit from MODELORDER keyword */
    havenoi = 0;
    haveext = 0;

    if( *status == SAI__OK ) {
      astMapGet1C(keymap, "MODELORDER", 4, SMF_MODEL_MAX, &nm, modelnames);
      nmodels = (dim_t) nm;

      /* Allocate modeltyps */
      if( nmodels >= 1 ) {
        modeltyps = astCalloc( nmodels, sizeof(*modeltyps), 1 );
        /* Extra components for exportNDF_which for 'res', 'qua' */
        exportNDF_which = astCalloc( nmodels+2, sizeof(*exportNDF_which), 1);
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














  /* ****************************************************************************
     Figure out how the data are split up, both in terms of continuous
     pieces of data in time, and in terms of the number of subarrays. Divide
     up the chunks, as needed, to fit in the available memory.
  **************************************************************************** */

  /* Create an ordered smfGrp which keeps track of files corresponding
     to different subarrays (observed simultaneously), as well as
     time-ordering the files. Now added "chunk" to smfGroup as well --
     this is used later to concatenate _only_ continuous pieces of
     data. Maxconcat will be the length of the largest continuous
     chunk, or maxlen, whichever comes first -- but excluding padding. */

  smf_grp_related( igrp, isize, 1, maxlen, &maxconcat, &maxfile,
                   &igroup, NULL, status );

  /* Once we've run smf_grp_related we know how many subarrays there
     are.  We also know the maximum length of a concatenated piece of
     data, and which model components were requested. Use this
     information to check that enough memory is available -- but now
     add in the extra length required for padding. */

  if( *status == SAI__OK ) {
    size_t mapmem;

    /* Add on the padding */
    maxconcat += padStart + padEnd;

    /* First check memory for the map */
    smf_checkmem_map( lbnd_out, ubnd_out, 0, maxmem, &mapmem, status );

    /* Then the iterative components that are proportional to time */
    smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
                       nmodels, keymap, maxmem-mapmem, maxfile, &memneeded,
                       status );

    msgOutf( "", FUNC_NAME ": map-making requires %zu Mb "
             "(map=%zu Mb model calc=%zu Mb)", status,
             (mapmem+memneeded)/SMF__MB, mapmem/SMF__MB, memneeded/SMF__MB );

    if( *status == SMF__NOMEM ) {
      /* If we need too much memory, generate a warning message and then try
         to re-group the files using smaller contchunks */

      errAnnul( status );
      msgOut( " ", FUNC_NAME ": *** WARNING ***", status );
      msgSeti( "LEN", maxconcat );
      msgSeti( "AVAIL", maxmem/SMF__MB );
      msgSeti( "NEED", memneeded/SMF__MB );
      msgOut( " ", "  ^LEN continuous samples (including padding) require "
              "^NEED Mb > ^AVAIL Mb",
              status );

      /* Try is meant to be the largest contchunk of ~equal length that fit in
         memory */
      try = maxconcat / ((size_t) ((double)memneeded/maxmem)+1)+1;

      /* Round up to get integral number of files including padding at start,
         but subtract off one file if it exceeds available memory. If we
         don't have enough memory even for one input file we're hooped. */

      try = ((try/maxfile)*maxfile + padStart);

      if( (try > (maxconcat*( (double) maxmem / (double) memneeded ))) &&
          (try > maxfile) ) {
        try -= maxfile;
      }

      if( try < (maxfile + padStart) ) {
        *status = SMF__NOMEM;
        errRep( "", FUNC_NAME ": not enough memory available to break job "
                "into smaller pieces.", status );
      }

      msgOutf( "", "  Will try to re-group data in chunks < %zu samples long",
               status, try );
      msgOut( " ", FUNC_NAME ": ***************", status );

      /* Close igroup if needed before re-running smf_grp_related */

      if( igroup ) {
        smf_close_smfGroup( &igroup, status );
      }

      smf_grp_related( igrp, isize, 1, try, &maxconcat, NULL, &igroup, NULL,
                       status );
    }
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
















  /* ****************************************************************************
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
  **************************************************************************** */

  for( contchunk=0; contchunk<ncontchunks; contchunk++ ) {

    size_t ntgood = 0;    /* Number of good time slices in this chunk */
    size_t nsamples = 0;  /* Number of good samples in this chunk */

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
        mapweightsq = astCalloc( msize, sizeof(*mapweightsq), 0 );
        thisweightsq = mapweightsq;
      } else if( contchunk == 1 ) {
        /* Subsequent continuous chunks are done in new map arrays and
           then added to the first */
        thismap = astCalloc( msize, sizeof(*thismap), 1 );
        thishits = astCalloc( msize, sizeof(*thishits), 1 );
        thisqual = astCalloc( msize, sizeof(*thisqual), 1 );
        thisvar = astCalloc( msize, sizeof(*thisvar), 1 );
        thisweight = astCalloc( msize, sizeof(*thisweight), 1 );
        thisweightsq = astCalloc( msize, sizeof(*thisweightsq), 1 );
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
        res = astCalloc( nfilegroups, sizeof(*res), 1 );

        /* Concatenate (no variance since we calculate it ourselves -- NOI) */
        smf_concat_smfGroup( wf, igroup, darks, bbms, flatramps, contchunk,
                             ensureflat, 0, outfset, moving, lbnd_out,
                             ubnd_out, padStart, padEnd,
                             SMF__NOCREATE_VARIANCE, tstep, &res[0], status );

        /*** TIMER ***/
        msgOutiff( MSG__DEBUG, "", FUNC_NAME ": ** %f s concatenating data",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      } else {
        if( !ensureflat ) {
          msgOut( "", FUNC_NAME ": *** WARNING: ensureflat=0 not supported "
                  "if memiter set. Data will be flat-fielded! ***", status );
        }
      }
    }

    /* Allocate space for the chisquared array */
    if( havenoi && (*status == SAI__OK) ) {
      chisquared = astCalloc( nfilegroups, sizeof(*chisquared), 1 );
      lastchisquared = astCalloc( nfilegroups, sizeof(*chisquared), 1 );
    }

    /* Create containers for time-series model components */
    msgOutif(MSG__VERB," ", FUNC_NAME ": Create model containers", status);

    /* Components that always get made */
    if( igroup && (*status == SAI__OK) ) {

      /* there is one smfArray for LUT, AST and QUA for each filegroup */
      lut = astCalloc( nfilegroups, sizeof(*lut), 1 );
      ast = astCalloc( nfilegroups, sizeof(*ast), 1 );
      qua = astCalloc( nfilegroups, sizeof(*qua), 1 );

      if( memiter ) {
        /* If iterating in memory then RES has already been created from
           the concatenation of the input data. Create the other
           required models using res[0] as a template. Assert
           bolo-ordered data although the work has already been done at
           the concatenation stage. */

        smf_model_create( wf, NULL, res, darks, bbms, flatramps, nfilegroups,
                          SMF__LUT, 0,
                          NULL, 0, NULL, NULL,
                          NULL, memiter,
                          memiter, lut, keymap, status );

        smf_model_create( wf, NULL, res, darks, bbms, flatramps, nfilegroups,
                          SMF__AST, 0,
                          NULL, 0, NULL, NULL,
                          NULL, memiter,
                          memiter, ast, keymap, status );

        smf_model_create( wf, NULL, res, darks, bbms, flatramps, nfilegroups,
                          SMF__QUA, 0,
                          NULL, 0, NULL, NULL,
                          NULL, memiter,
                          memiter, qua, keymap, status );

        /* Since a copy of the LUT is still open in res[0] free it up here */
        for( i=0; i<res[0]->ndat; i++ ) {
          if( res[0]->sdata[i] ) {
            smf_close_mapcoord( res[0]->sdata[i], status );
          }
        }

      } else {

        /* If iterating using disk i/o need to create res and other
           model components using igroup as template -- which has
           "nfilegroups" data files per subarray. In this case the
           pointing LUT probably doesn't exist, so give projection
           information to smf_model_create. Also assert bolo-ordered
           template (in this case res). */

        res = astCalloc( nfilegroups, sizeof(*res), 1 );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, 0,
                          SMF__RES, 0,
                          NULL, 0, NULL, NULL,
                          &resgroup, memiter,
                          memiter, res, keymap, status );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, 0,
                          SMF__LUT, 0,
                          outfset, moving, lbnd_out, ubnd_out,
                          &lutgroup, memiter,
                          memiter, lut, keymap, status );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, 0,
                          SMF__AST, 0,
                          NULL, 0, NULL, NULL,
                          &astgroup, memiter,
                          memiter, ast, keymap, status );

        smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, 0,
                          SMF__QUA, 0,
                          NULL, 0, NULL, NULL,
                          &quagroup, memiter,
                          memiter, qua, keymap, status );
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

    /*** TIMER ***/
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": ** %f s creating static models",
               status, smf_timerupdate(&tv1,&tv2,status) );

    /* Dynamic components */
    if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

      /* nmodel array of pointers to nfilegroups smfArray pointers */
      model = astCalloc( nmodels, sizeof(*model), 1 );

      if( memiter != 1 ) {
        /* Array of smfgroups (one for each dynamic model component) */
        modelgroups = astCalloc( nmodels, sizeof(*modelgroups), 1 );
      }

      for( i=0; i<nmodels; i++ ) {
        model[i] = astCalloc( nfilegroups, sizeof(**model), 1 );

        if( memiter ) {
          smf_model_create( wf, NULL, res, darks, bbms, flatramps, nfilegroups,
                            modeltyps[i], 0,
                            NULL, 0, NULL, NULL,
                            NULL, memiter, memiter, model[i], keymap,
                            status );

        } else {
          smf_model_create( wf, igroup, NULL, darks, bbms, flatramps, 0,
                            modeltyps[i], 0,
                            NULL, 0, NULL, NULL, &modelgroups[i],
                            memiter, memiter, model[i], keymap,
                            status );
        }
      }
    }

    /*** TIMER ***/
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": ** %f s creating dynamic models",
               status, smf_timerupdate(&tv1,&tv2,status) );

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











      /* ************************************************************************
         Start the main iteration loop.

         At this stage the full pointing solution for the data in this
         continuous chunk has been calculated, and all of the model
         containers have been created. This loop stops either if chi^2
         has converged, or we did the requested number of iterations,
         for this continuous chunk.
      ************************************************************************ */

      quit = 0;
      iter = 0;

      while( !quit ) {
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




        /* **********************************************************************
           Start the inner loop over filegroups, and the calculation of
           all the model components up to AST (including models that follow
           AST in modelorder, and then wrapping back around to the beginning).
           There is only a single pass through this loop if memiter=1.
        ********************************************************************** */

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
            smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );
            smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );
            smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_open_related_model( modelgroups[j], i, "UPDATE",
                                      &model[j][i], status );
            }

            /*** TIMER ***/
            msgOutiff( MSG__DEBUG, "", FUNC_NAME
                       ": ** %f s opening model files",
                       status, smf_timerupdate(&tv1,&tv2,status) );
          }

          /* If first iteration pre-condition the data */
          if( iter == 0 ) {

            /* Associate quality model with the res and ast model */
            for( idx=0; idx<res[i]->ndat; idx++ ) {
              smfData *thisqua = qua[i]->sdata[idx];
              res[i]->sdata[idx]->sidequal = thisqua;
              ast[i]->sdata[idx]->sidequal = thisqua;
            }

            if( doclean ) {
              msgOut(" ", FUNC_NAME ": Pre-conditioning data", status);
              for( idx=0; idx<res[i]->ndat; idx++ ) {
                data = res[i]->sdata[idx];
                smf_clean_smfData( wf, data, keymap, status );
              }
            } else {
              msgOut( "", FUNC_NAME ": *** Warning *** doclean=0, "
                      "so not pre-conditioning data before map-making",
                      status );
            }

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

            /*** TIMER ***/
            msgOutiff( MSG__DEBUG, "", FUNC_NAME
                       ": ** %f s pre-conditioning data",
                       status, smf_timerupdate(&tv1,&tv2,status) );

            /* Export the cleaned data here if desired */
            if( exportclean ) {
              msgOut( "", FUNC_NAME
                      ": Writing out clean data prior to map-making.",
                      status );

              for( idx=0; idx<res[i]->ndat; idx++ ) {
                int oldorder;
                data = res[i]->sdata[idx];

                /* create a file name with "_res_cln" suffix */
                smf_model_stripsuffix( res[i]->sdata[idx]->file->name,
                                       name, status );

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
                                   status );

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
                msgOutiff( MSG__DEBUG, "", FUNC_NAME
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
                msgOutiff( MSG__DEBUG, "", FUNC_NAME
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
              msgOutiff( MSG__DEBUG, "", FUNC_NAME
                         ": ** %f s calculating model",
                         status, smf_timerupdate(&tv1,&tv2,status) );
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

            /* Ensure we use the AST model ordering */
            smf_model_dataOrder( &dat, NULL, i, SMF__RES|SMF__LUT|SMF__QUA,
                                 ast[i]->sdata[0]->isTordered, status );

            /* Loop over subgroup index (subarray) */
            for( idx=0; idx<res[i]->ndat; idx++ ) {

              /* Add last iter. of astronomical signal back in to residual */
              ast_data = (ast[i]->sdata[idx]->pntr)[0];
              res_data = (res[i]->sdata[idx]->pntr)[0];
              lut_data = (lut[i]->sdata[idx]->pntr)[0];
              qua_data = (qua[i]->sdata[idx]->pntr)[0];

              if( havenoi ) {
                var_data = (dat.noi[i]->sdata[idx]->pntr)[0];
              } else {
                var_data = NULL;
              }

              smf_get_dims( ast[i]->sdata[idx], NULL, NULL, NULL, NULL, &dsize,
                            NULL, NULL, status );

              for( k=0; k<dsize; k++ ) {
                if( !(qua_data[k]&SMF__Q_MOD) && (ast_data[k]!=VAL__BADD) ) {
                  res_data[k] += ast_data[k];
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
                             lut_data, 0, 0, 0, SMF__Q_GOOD,
                             varmapmethod, rebinflags, thismap, thisweight,
                             thisweightsq, thishits, thisvar, msize, &scalevar,
                             status );
            }

            /*** TIMER ***/
            msgOutiff( MSG__DEBUG, "", FUNC_NAME
                       ": ** %f s rebinning map",
                       status, smf_timerupdate(&tv1,&tv2,status) );

            /* If storing each iteration in an extension do it here if this
               was the last filegroup of data to be added */

            if( itermap && (i == nfilegroups-1) ) {
              smf_write_itermap( thismap, thisvar, msize, iterrootgrp,
                                 contchunk, iter, lbnd_out, ubnd_out,
                                 outfset, res[i]->sdata[0]->hdr, qua[i],
                                 status );

            }
          }

          /* Close files here if memiter not set */

          if( !memiter ) {
            smf_close_related( &res[i], status );
            smf_close_related( &ast[i], status );
            smf_close_related( &lut[i], status );
            smf_close_related( &qua[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_close_related( &model[j][i], status );
            }

            /*** TIMER ***/
            msgOutiff( MSG__DEBUG, "", FUNC_NAME
                       ": ** %f s closing model files",
                       status, smf_timerupdate(&tv1,&tv2,status) );
          }

          /* Set exit condition if bad status was set */
          if( *status != SAI__OK ) i=isize+1;

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








        /* **********************************************************************
           Calculate the AST model component.

           When we get here we have calculated all of the model components up
           to, but not including AST (for all filegroups). In addition, the
           previous iteration of AST has been placed back into the residual,
           and a map has been estimated. So, we are now in a position to
           take the current estimate of the map and project it back into
           the time domain to estimate the new AST.
        ********************************************************************** */

        if( *status == SAI__OK ) {
          msgOut(" ", FUNC_NAME ": Calculate ast", status);

          for( i=0; i<nfilegroups; i++ ) {

            /* Open files if memiter not set - otherwise they are still open
               from earlier call */
            if( !memiter ) {
              smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );
              smf_open_related_model( resgroup, i, "UPDATE", &res[i], status );
              smf_open_related_model( lutgroup, i, "UPDATE", &lut[i], status );
              smf_open_related_model( quagroup, i, "UPDATE", &qua[i], status );

              /*** TIMER ***/
              msgOutiff( MSG__DEBUG, "", FUNC_NAME
                         ": ** %f s opening model files",
                         status, smf_timerupdate(&tv1,&tv2,status) );
            }

            /* Calculate the AST model component. It is a special model
               because it assumes that the map contains the best current
               estimate of the astronomical sky. It gets called in this
               separate loop since the map estimate gets updated by
               each filegroup in the main model component loop */

            dimmflags=0;
            if( iter==(maxiter-1) ) dimmflags |= SMF__DIMM_LASTITER;
            smf_calcmodel_ast( wf, &dat, i, keymap, ast, dimmflags, status );

            /*** TIMER ***/
            msgOutiff( MSG__DEBUG, "", FUNC_NAME
                       ": ** %f s calculating AST",
                       status, smf_timerupdate(&tv1,&tv2,status) );


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
              smf_close_related( &ast[i], status );
              smf_close_related( &res[i], status );
              smf_close_related( &lut[i], status );
              smf_close_related( &qua[i], status );

              /*** TIMER ***/
              msgOutiff( MSG__DEBUG, "", FUNC_NAME
                         ": ** %f s closing models files for this filegroup",
                         status, smf_timerupdate(&tv1,&tv2,status) );
            }
          }
        }

        /* Increment iteration counter */
        iter++;

        if( *status == SAI__OK ) {

          /* Check that we've exceeded maxiter */
          if( iter >= maxiter ) {
            quit = 1;
          }

          /* Check for convergence */
          if( untilconverge && converged ) {
            quit = 1;
          }

        } else {
          quit = 1;
        }
      }

      msgSeti("ITER",iter);
      msgOut( " ",
              FUNC_NAME ": ****** Completed in ^ITER iterations", status);
      if( untilconverge && converged ) {
        msgOut( " ",
                FUNC_NAME ": ****** Solution CONVERGED",
                status);
      }










      /* ************************************************************************
         The continous chunk has finished.

         The model components for this continuous chunk have converged.

         We can now do things like write out the bolomap and shortmap
         extensions, and also export model components (if requested).

         We also add the map estimated from this contchunk to those from
         previous contchunks if necessary.
      ************************************************************************ */

      /* Create sub-maps for each bolometer if requested. We add AST back
         into the residual, and rebin that single for each detector that
         is flagged as being OK */

      if( bolomap && (*status == SAI__OK) ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** bolomap=1, but memiter=0", status );
        } else {
          smf_write_bolomap( ast, res, lut, qua, &dat, msize, bolrootgrp,
                             contchunk, varmapmethod, lbnd_out, ubnd_out,
                             outfset, status );
        }
      }

      /* Create short maps using every SHORTMAP samples if requested */

      if( shortmap && (*status == SAI__OK) ) {
        /* Currently only support memiter=1 case to avoid having to do
           a separate filegroup loop. */
        if( !memiter ) {
          msgOut( "", FUNC_NAME
                  ": *** WARNING *** shortmap=1, but memiter=0", status );
        } else {
          smf_write_shortmap( shortmap, ast, res, lut, qua, &dat, msize,
                              shortrootgrp, contchunk, varmapmethod, lbnd_out,
                              ubnd_out, outfset, status );
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
            smf_open_related_model( astgroup, i, "UPDATE", &ast[i], status );

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
            smf_dataOrder( ast[i]->sdata[idx], 1, status );

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
                  smf_model_stripsuffix( res[i]->sdata[idx]->file->name,
                                         name, status );

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
                                     status );
                } else {
                  msgOut( " ",
                          "SMF__ITERATEMAP: Can't export RES -- NULL filename",
                          status);
                }
              }

              if( exportNDF_which[whichast] ) {

                if( (ast[i]->sdata[idx]->file->name)[0] ) {
                  smf_model_createHdr( ast[i]->sdata[idx], SMF__AST, refdata,
                                       status );
                  smf_model_stripsuffix( ast[i]->sdata[idx]->file->name,
                                         name, status );

                  if( !noexportsetbad ) {
                    smf_update_valbad( ast[i]->sdata[idx], SMF__NUL,
                                       NULL, 0, 0, SMF__Q_BADB, status );
                  }

                  /* Also export QUAlity if requested */
                  smf_write_smfData( ast[i]->sdata[idx], NULL, name,
                                     NULL, 0, NDF__NOID, status );
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
                  smf_model_stripsuffix( model[j][i]->sdata[idx]->file->name,
                                         name, status );

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
                                                           NULL, SMF__QUALTYPE,
                                                           NULL, tempqual,
                                                           SMF__QFAM_TSERIES,
                                                           NULL, 1, com->dims,
                                                           com->lbnd,
                                                           com->ndims, 0, 0,
                                                           NULL, NULL, status );
                  }

                  smf_write_smfData( model[j][i]->sdata[idx], NULL,
                                     name, NULL, 0, NDF__NOID, status );

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
            smf_close_related( &ast[i], status );

            for( j=0; j<nmodels; j++ ) {
              smf_close_related( &model[j][i], status );
            }
          }
        }

        /*** TIMER ***/
        msgOutiff( MSG__DEBUG, "", FUNC_NAME
                   ": ** %f s Exporting models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

        errEnd( status );
      }
    }

    /* If we get here and there is a SMF__INSMP we probably flagged all
       of the data as bad for some reason. In a multi-contchunk map it is
       annoying to have the whole thing die here. So, annul the error,
       warn the user, and then continue on... This will also help us
       to properly free up resources used by this chunk. */

    if( *status == SMF__INSMP ) {
      errAnnul( status );
      msgOut("", " ************************* Warning! *************************",
             status );
      msgOut("", " This continuous chunk failed due to insufficient samples.",
              status );
      msgOut("", " This is often due to strict bad-bolo flagging.", status );
      msgOut("", " Annuling the bad status and trying to continue...", status);
      msgOut("", " ************************************************************",
             status );
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
      steptime = res[0]->sdata[0]->hdr->steptime;
      for (i=0; (i<msize) && (*status == SAI__OK); i++) {
        if ( map[i] != VAL__BADD) {
          exp_time[i] += steptime * (double)hitsmap[i];
        }
      }
    }




    /* **************************************************************************
       Clean up temporary resources associated with this continuous chunk
       before continuing in the outer loop.
    ************************************************************************** */

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

        for( j=0; (astgroup)&&(j<astgroup->nrelated); j++ ) {
          grpGet( astgroup->grp, astgroup->subgroups[i][j], 1, &pname,
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
    if( astgroup ) smf_close_smfGroup( &astgroup, status );
    if( lutgroup ) smf_close_smfGroup( &lutgroup, status );
    if( quagroup ) smf_close_smfGroup( &quagroup, status );

    /* fixed model smfArrays */
    if( res ) {
      for( i=0; i<nfilegroups; i++ ) {
        if( res[i] ) smf_close_related( &res[i], status );
      }
      res = astFree( res );
    }

    if( ast ) {
      for( i=0; i<nfilegroups; i++ ) {
        if( ast[i] ) smf_close_related( &ast[i], status );
      }
      ast = astFree( ast );
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
    if( chisquared) chisquared = astFree( chisquared );
    if( lastchisquared) lastchisquared = astFree( lastchisquared );
  }














  /* ****************************************************************************
     Final cleanup

     We have finished the outermost loop over continuous chunks. Free
     all of the memory we allocated.
  **************************************************************************** */

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
  if( mapweightsq ) mapweightsq = astFree( mapweightsq );

  if( modeltyps ) modeltyps = astFree( modeltyps );
  if( exportNDF_which ) exportNDF_which = astFree( exportNDF_which );

  if( igroup ) {
    smf_close_smfGroup( &igroup, status );
  }

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}
