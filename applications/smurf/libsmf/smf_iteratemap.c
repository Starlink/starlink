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
*                    AstKeyMap *akeymap,
*                    const smfArray * darks, const smfArray *bbms,
*                    const smfArray * flatramps, AstKeyMap * heateffmap, AstFrameSet *outfset,
*                    int moving, int *lbnd_out, int *ubnd_out, fts2Port fts_port, size_t maxmem,
*                    double *map, int *hitsmap, double *exp_time,
*                    double *mapvar, smf_qual_t *mapqual, double *weights,
*                    char data_units[], double *nboloeff,
*                    size_t *numcontchunks, size_t *ncontig, int *memlow,
*                    size_t *numinsmp, size_t *numcnvg, int *iters,
*                    int *masked, int *status );

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
*     akeymap = AstKeyMap* (Given)
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
*     fts_port = fts2Port (Given)
*        FTS-2 port.
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
*     ncontig = size_t * (Returned)
*        If non-NULL, will contain the number of continuous chunks within
*        the supplied data.
*     memlow = int * (Returned)
*        If non-NULL, will be non-zerp if the data was chunked due to
*        insufficient memory.
*     numinsmp = size_t *(Returned)
*        If non-NULL, will contain the number of continuous data chunks that
*        did not go into the map due to insufficient samples.
*     numcnvg = size_t *(Returned)
*        If non-NULL, will contain the number of continuous data chunks that
*        did not converge (although they are still added to the map).
*     iters = int * (Returned)
*        Normally returned equal to -1, but if the application is interupted
*        using control-C it will be returned holding the number of iterations
*        that were completed.
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
*     The primary loop over that data uses the concept of a continuous
*     chunk, or "contchunk". This is a piece of un-interrupted data,
*     usually made by concatenating a number of smaller files together.
*     This operation is necessary because the SCUBA-2 data acquisition
*     system usually splits up data in to pieces that are 30 s long
*     and stores them in separate files.

*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     AGM: Gaelen Marsden (UBC)
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
*     2012-01-16 (DSB):
*        Fix a memory leak (zeromask was freed at the wrong time).
*     2012-01-31 (DSB):
*        - Ensure the supplied KeyMap is unchanged on exit, so that we can
*        re-run smf_iteratemap a second time if required.
*        - Ensure all AST objects created in this function are deleted before leaving.
*     2012-02-22 (DSB):
*        - Refactor j-loop that puts AST back into the residuals. Ticket #932.
*        - Allow fakemap to have arbitrary pixel bounds. Ticket #930.
*        - Remove memiter=0 case. Ticket #885.
*     2012-05-01 (DSB):
*        Add control-C handler to allow controlled premature exit.
*     2012-06-05 (DSB):
*        Allow the old FLT model to be added back onto the residuals at the start
*        of each iteration, rather than just before finding a new FLT (in
*        smf_calcmodel_flt). This is controlled by new config parameter FLT.UNDOFIRST.
*     2012-10-22 (DSB):
*        If ( abs(itermaps) > 1 ) then include a quality component in each itermap.
*     2012-11-20 (EC):
*        Add some commented-out code to apply the MCE response to fakemap data
*     2012-11-21 (DSB):
*        Add config parameter fakemce to indicate if the fakmap data
*        should be smoothed using the MCE response.
*     2012-11-26 (DSB):
*        If time-streams are being delayed, add an equal and opposite
*        delay to the fakemap data.
*     2012-12-04 (DSB):
*        Re-initialise the "lastmap" array (that holds the map created by
*        the previous iteration) to zero at the start of each chunk.
*     2013-6-21 (DSB):
*        Ensure bad values in pre-cleaned data are flaged in the quality
*        array.
*     2013-7-3 (DSB):
*        - Change handling of interupts - user is now asked what to do when
*        an interupt is detected, using parameter INTOPTION.
*        - Added returned argument "iters".
*        - If an initial sky is supplied that was created by a previous
*        interupted run of makemap, start counting iterations from where
*        the previous run of makemap left off.
*     2013-7-23 (DSB):
*        Do not converge until any initial iterations that skip the AST
*        model have been done.
*     2013-10-25 (AGM):
*        Allocate extra memory for maps for alternate rebinning scheme
*     2014-08-01 (DSB):
*        - Allow LUT model to be exported.
*        - Undo COM as a separate step at start of each iteration.
*     2014-01-16 (DSB):
*        Do not allocate models to hold AST since the astronomical signal is
*        determined from the current map.
*     2014-01-27 (DSB):
*        Dump the itermap after the map quality array has been set (i.e.
*        after smf_calcmodel_ast). Previously each itermap has the quality
*        associated with the previous iteration.
*     2014-02-13 (DSB):
*        If a different filter size is used on the last iteration, return
*        the map variances from the penultimate iteration.
*     2014-02-28 (DSB):
*        If the AST model was skipped on some initial iterations, do not
*        converge until at least one complete iteration has been
*        performed that included an AST Model. Previously, termination
*        could occur as soon as the initial AST-free iterations had been
*        completed.
*     2014-03-10 (DSB):
*        Provide an option to skip the AST model on all iterations,
*        terminating using the normal maptol criterion. This is done by
*        setting a negative value for AST.SKIP.
*     2014-03-11 (DSB):
*        Allow the map to be lagged between iterations. May help to
*        prevent oscillations in the SNR mask from iteration to iteration.
*     2014-03-27 (DSB):
*        When calculating the mean and max mapchange at the end of each
*        iteration, exclude any map pixels that have a very low number of
*        hits (fewer than 10% of the mean number of hits per pixel) since
*        these will have unreliable (potentially tiny) variances.
*     2014-04-4 (DSB):
*        If the AST mask area drops to zero pixels, there are no bright
*        pixels in the map. But this is no reason to report an error - a
*        map can still be made, albeit it will contain only background
*        areas. So annull the error when reporting mapchange values in
*        this case.
*     2014-05-26 (DSB):
*        Add the "hitslimit" config parameter, that allows map pixels
*        with very low hits to be set bad.
*     2014-05-27 (DSB):
*        Add config parameters epsin and epsout to allow a constant error
*        map to be subtracted from the map at the end of each iteration.
*        Currently, the epsout parameter can only be used if the data
*        is processed in a single continuous chunk (an error is reported
*        otherwise). Ideally, each chunk should probably have its own
*        independent error map, since the errors may be chunk-dependent.
*     2014-5-29 (DSB):
*        If the AST mask is found to contain zero pixels, then ensure one
*        more iteration is performed in order to ensure any "xxx_LAST"
*        parameter values (i.e. values to be used on the last iteration
*        only) are used.
*     2014-6-11 (DSB):
*        If chunking causes the last chunk to contain very few time slices,
*        reduce the max chunk size so that all chunks have more equal sizes.
*     2014-7-18 (DSB):
*        Added config parameter memcheck.
*     2014-8-19 (DSB):
*        Added arguments ncontig and memlow.
*     2014-09-22 (DSB):
*        When running from SKYLOOP, and if ast.filt_diff is set, filter the
*        map change after all chunks have been combined, rather than after
*        each individual chunk.
*     2014-10-10 (DSB):
*        If there is insufficient memory to avoid chunking, try the
*        calculation again but without the extra work-space needed to
*        support multi-threading in smf_rebinmap1.
*     2014-12-18 (DSB):
*        Added SSN.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008-2014 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Need sigaction to be prototyped */
#define _POSIX_C_SOURCE 200809L

/* Some compilers need this to get SA_RESTART */
#define _BSD_SOURCE

#include <stdio.h>
#include <signal.h>
#include <unistd.h>

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


/* Prototypes for local static functions. */
static void smf1_iteratemap( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfIterateMapData {
   double *epsout;
   double *err1;
   double *err2;
   double *err3;
   double *res_data;
   double *thismap;
   int *lut_data;
   int operation;
   size_t d1;
   size_t d2;
   smf_qual_t *qua_data;
   smf_qual_t *thisqual;
} SmfIterateMapData;

#define FUNC_NAME "smf_iteratemap"

/* A flag used to indicate that an interupt has occurred. */
volatile sig_atomic_t smf_interupt = 0;

/* Main routine */
void smf_iteratemap( ThrWorkForce *wf, const Grp *igrp, const Grp *iterrootgrp,
                     const Grp *bolrootgrp, const Grp *shortrootgrp,
                     const Grp *flagrootgrp, const Grp *samprootgrp,
                     AstKeyMap *akeymap,
                     const smfArray *darks, const smfArray *bbms,
                     const smfArray * flatramps, AstKeyMap * heateffmap, AstFrameSet *outfset,
                     int moving, int *lbnd_out, int *ubnd_out, fts2Port fts_port, size_t maxmem,
                     double *map, int *hitsmap, double * exp_time,
                     double *mapvar, smf_qual_t *mapqual, double *weights,
                     char data_units[], double * nboloeff,
                     size_t *numcontchunks,  size_t *ncontig, int *memlow,
                     size_t *numinsmp, size_t *numcnvg, int *iters,
                     int *status ) {

  /* Local Variables */
  float ast_filt_diff;          /* Size of map-change filter */
  int ast_skip;                 /* Number of iterations with no AST model */
  int flt_skip;                 /* Number of iterations with no FLT model */
  int bolomap=0;                /* If set, produce single bolo maps */
  size_t bstride;               /* Bolometer stride */
  double *chisquared=NULL;      /* chisquared for each chunk each iter */
  double chitol=VAL__BADD;      /* chisquared change tolerance for stopping */
  int chunking;                 /* Will we be chunking due to low memory? */
  double chunkweight;           /* The relative weight to give to the
                                   current chunk when adding into the running
                                   sum map. */
  size_t contchunk;             /* Continuous chunk in outer loop counter */
  int converged=0;              /* Has stopping criteria been met? */
  smfDIMMData dat;              /* Struct passed around to model components */
  smfData *data=NULL;           /* Temporary smfData pointer */
  double fakedelay = 0.0;       /* Extra fake time stream delay, in seconds */
  double downsampscale;         /* Downsample factor to preserve this scale */
  int dimmflags;                /* Control flags for DIMM model components */
  int doclean=1;                /* Are we doing data pre-processing? */
  dim_t dsize;                  /* Size of data arrays in containers */
  int ensureflat=1;             /* flatfield data as they are loaded */
  double *epsbuf1=NULL;         /* Buffer for diff map */
  double *epsbuf2=NULL;         /* Buffer for diff map */
  double *epsbuf3=NULL;         /* Buffer for diff map */
  char *epsin=NULL;             /* Name of external error map to use */
  char *epsout=NULL;            /* Name of external error map to create */
  double *emapdata=NULL;        /* epsin data values */
  int epsndf=NDF__NOID;         /* NDF id for epsin */
  int exportclean=0;            /* Are we doing to export clean data? */
  int exportNDF=0;              /* If set export DIMM files to NDF at end */
  int *exportNDF_which=NULL;    /* Which models in modelorder will be exported*/
  char *fakemap=NULL;           /* Name of external map with fake sources */
  int fakemce;                  /* Smooth fake data with MCE response? */
  int fakendf=NDF__NOID;        /* NDF id for fakemap */
  double fakescale;             /* Scale factor for fakemap */
  double *fakestream = NULL;    /* Time series data from fake map */
  int firstiter;                /* First iteration in this invocation of makemap? */
  size_t count_mcnvg=0;         /* # chunks fail to converge */
  size_t count_minsmp=0;        /* # chunks fail due to insufficient samples */
  smf_qual_t flagmap=0;         /* bit mask for flagmaps */
  int flt_undofirst = 1;        /* Undo FLT model at start of iteration? */
  double *fmapdata=NULL;        /* fakemap for adding external ast signal */
  int groupsubarray;            /* Handle subarrays separately? */
  int noexportsetbad=0;         /* Don't set bad values in exported models */
  int haveast=0;                /* Set if AST is one of the models */
  int haveext=0;                /* Set if EXT is one of the models */
  int havecom=0;                /* Set if COM is one of the models */
  int haveflt=0;                /* Set if FLT is one of the models */
  int havessn=0;                /* Set if SSN is one of the models */
  int havegai=0;                /* Set if GAI is one of the models */
  int havenoi=0;                /* Set if NOI is one of the models */
  double hitslim;               /* Min fraction of hits allowed in a map pixel */
  smfData *refdata=NULL;        /* Pointer to reference smfData */
/*  size_t i;                     */ /* Loop counter */
  size_t idat;                  /* smfData counter */
  size_t imodel;                /* Model counter */
  size_t importsky = 0;         /* Subtract a supplied initial sky map? */
  int intopt;                   /* Interupt option */
  size_t ipix;                  /* Pixel counter */
  int ii;                       /* Loop counter */
  size_t idx=0;                 /* index within subgroup */
  smfGroup *igroup=NULL;        /* smfGroup corresponding to igrp */
  int isize;                    /* Number of files in input group */
  int iter;                     /* Iteration number */
  int itermap=0;                /* If set, produce maps each iteration */
  int itsdone;                  /* Number of previously completed iterations */
  int iw;                       /* Thread index */
  size_t j;                     /* Loop counter */
  size_t k;                     /* Loop counter */
  AstKeyMap *keymap=NULL;       /* Copy of supplied keymap */
  AstKeyMap *kmap=NULL;         /* Pointer to model-specific keys */
  size_t l;                     /* Loop counter */
  double *lastchisquared=NULL;  /* chisquared for last iter */
  double *lastmap=NULL;         /* map from the last iter */
  int last_skipped=0;           /* Was the AST model skipped on the previous iteration? */
  smfData *localmap = NULL;     /* A temporary container for the map */
  smfArray **lut=NULL;          /* Pointing LUT for each file */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfGroup *lutgroup=NULL;      /* smfGroup of lut model files */
  double *mapchange=NULL;       /* Array storing change (map - lastmap)/sigma*/
  double mapchange_mean=0;      /* Mean change in map */
  double mapchange_l2;          /* Mean change from previous iteration */
  double mapchange_l3;          /* Mean change from previous iteration */
  double mapchange_max=0;       /* Maximum change in the map */
  double maptol_rate=VAL__BADD; /* Min rate of change of mean map change */
  double maptol=VAL__BADD;      /* map change tolerance for stopping */
  int maptol_mean;              /* Use mean map change instead of max map change? */
  double *mapweights=NULL;      /* Weight for each pixel including chunk weight */
  double *mapweightsq=NULL;     /* Sum of bolometer weights squared */
  dim_t maxconcat;              /* Longest continuous chunk that fits in mem.*/
  dim_t maxconcat2;             /* Better estimate of longest chunk */
  dim_t maxfile;                /* Longest file length in time samples*/
  int maxiter=0;                /* Maximum number of iterations */
  double maxlen=0;              /* Max length in seconds of cont. chunk */
  int memcheck=0;               /* Are we just doing a memory check? */
  size_t memneeded;             /* Memory required for map-maker */
  smfArray ***model=NULL;       /* Array of pointers smfArrays for ea. model */
  char *modelname=NULL;         /* Name of current model component */
  char modelnames[SMF_MODEL_MAX*4]; /* Array of all model components names */
  smf_modeltype *modeltyps=NULL;/* Array of model types */
  smf_calcmodelptr modelptr=NULL; /* Pointer to current model calc function */
  dim_t mdims[2];               /* Dimensions of map */
  dim_t msize;                  /* Number of elements in map */
  int mw;                       /* No. of threads to use when rebinning data into a map */
  char name[GRP__SZNAM+1];      /* Buffer for storing exported model names */
  dim_t nbolo;                  /* Number of bolometers */
  size_t ncontchunks=0;         /* Number continuous chunks outside iter loop*/
  int nhitslim=0;               /* Min number of hits allowed in a map pixel */
  int nm=0;                     /* Signed int version of nmodels */
  dim_t nmodels=0;              /* Number of model components / iteration */
  int noidone;                  /* Has the NOI model been calculated yet? */
  int noi_export;               /* Export the compressed NOI model? */
  int noi_usevar;               /* Use the input Variances to make the NOI model? */
  size_t nsamples_tot = 0;      /* Number of valid samples in all chunks */
  dim_t nthetabin;              /* Number of scan angle bins */
  size_t ntgood_tot = 0;        /* Number of good time slices in all chunks */
  dim_t ntslice;                /* Number of time slices */
  size_t numdata;               /* Total number of samples in chunk */
  int numiter=0;                /* Total number iterations */
  int nw;                       /* Number of worker threads */
  dim_t pad=0;                  /* How many samples of padding at both ends */
  double pixsize = 0.0;         /* Pixel size */
  size_t qcount_last[SMF__NQBITS_TSERIES];/* quality bit counter -- last iter */
  smfArray **qua=NULL;          /* Quality flags for each file */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */
  smfGroup *quagroup=NULL;      /* smfGroup of quality model files */
  int quit=0;                   /* flag indicates when to quit */
  int rate_limited=0;           /* Was the MAPTOL_RATE limit hit? */
  int rebinflags;               /* Flags to control rebinning */
  smfArray **res=NULL;          /* Residual signal */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  smfGroup *resgroup=NULL;      /* smfGroup of model residual files */
  int reuse_var;                /* Reuse map variances from previous iteration? */
  int sampcube;                 /* write SAMPCUBES extensions? */
  double scalevar=0;            /* scale factor for variance */
  int shortmap=0;               /* If set, produce maps every shortmap tslices*/
  double srate_maxlen=0;        /* Sample rate used to calc maxlen in samples */
  double steptime;              /* Length of a sample in seconds */
  double sumchunkweights;       /* Sum of all chunk weights */
  const char *tempstr=NULL;     /* Temporary pointer to static char buffer */
  double *thetabincen=NULL;     /* Bin centres of scan angle */
  double *thetabins=NULL;       /* Bins of scan angle */
  int *thishits=NULL;           /* Pointer to this hits map */
  double *thismap=NULL;         /* Pointer to this map */
  smf_modeltype thismodel;      /* Type of current model */
  smf_qual_t *thisqual=NULL;    /* Pointer to this quality map */
  double *thisvar=NULL;         /* Pointer to this variance map */
  double *thisweight=NULL;      /* Pointer to this weights map */
  double *thisweightsq=NULL;    /* Pointer to this weights map^2 */
  size_t try;                   /* Try to concatenate this many samples */
  size_t tstride;               /* Time stride */
  struct timeval tv1, tv2;      /* Timers */
  double tol;                   /* Map change value to compare to maptol */
  int untilconverge=0;          /* Set if iterating to convergence */
  int varmapmethod=0;           /* Method for calculating varmap */
  dim_t whichast=0;             /* Model index of AST (must be specified) */
  dim_t whichext=0;             /* Model index of EXT if present */
  dim_t whichflt=0;             /* Model index of FLT if present */
  dim_t whichssn=0;             /* Model index of SSN if present */
  dim_t whichcom=0;             /* Model index of COM if present */
  dim_t whichgai=0;             /* Model index of GAI if present */
  dim_t whichnoi=0;             /* Model index of NOI if present */
  int *whichthetabin=NULL;      /* Which scan angle bin each time slice */
  SmfIterateMapData *job_data = NULL;  /* Array of job descriptions */
  SmfIterateMapData *pdata;     /* Pointer to next job description */

  /* initalise */
  *iters = -1;
  if( memlow ) *memlow = 0;
  if( ncontig ) *ncontig = 0;

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Begin an AST context so all AST objects created in (or under) this
     function are automatically annulled by the call to astEnd at the end of
     this function. */
  astBegin;

  /* If this is an interactive session, establish an interupt handler that
     sets the smf_interupt flag non-zero when an interupt occurs. */
  if( isatty( STDIN_FILENO ) ) {
    struct sigaction action;
    action.sa_handler = smf_handler;
    sigemptyset( &action.sa_mask );
    action.sa_flags = SA_RESTART | SA_RESETHAND;
    sigaction( SIGINT, &action, NULL );
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Allocate job data for threads. */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* This function modifies the contents of the config keymap. So take a
     copy of the supplied keymap to avoid modifying it. */
  keymap = astCopy( akeymap );

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
  lastmap = astMalloc( msize*sizeof(*lastmap) );
  mapchange = astMalloc( msize*sizeof(*mapchange) );

  /* Always need to initialize these masks. The buffers will get
     allocated in smf_get_mask if ast.zero_circle (etc) was set. */
  dat.ast_mask = NULL;
  dat.com_mask = NULL;
  dat.flt_mask = NULL;
  dat.ssn_mask = NULL;

  /* Get size of the input group */
  isize = grpGrpsz( igrp, status );











  /* ***************************************************************************
     Parse the CONFIG parameters stored in the keymap, and set up
     defaults for the map-maker. We assume that all variables have
     been given defaults through the .def file.
  *************************************************************************** */

  if( *status == SAI__OK ) {
    if( *status == SAI__OK ) {

      /* See if the NOI model is to be derived from the Variance
         components of the input data files. */
      if( astMapGet0A( keymap, "NOI", &kmap ) ) {
         astMapGet0I( kmap, "USEVAR", &noi_usevar );

      /* See if the NOI model is to be exported in compressed form in its
         own NDF (i.e. NOT as the Variance component of the "_res" ndf). */
         astMapGet0I( kmap, "EXPORT", &noi_export );
         kmap = astAnnul( kmap );
      }

      /* See if the FLT model is to be undone at the start of the
         iteration or when the FLT model is updated. */
      if( astMapGet0A( keymap, "FLT", &kmap ) ) {
         astMapGet0I( kmap, "UNDOFIRST", &flt_undofirst );
         kmap = astAnnul( kmap );
      }

      /* Chisquared change tolerance for stopping */
      astMapGet0D( keymap, "CHITOL", &chitol );

      if( (chitol != VAL__BADD) && (chitol <= 0) ) {
        *status = SAI__ERROR;
        msgSetd("CHITOL",chitol);
        errRep(FUNC_NAME,
               FUNC_NAME ": CHITOL is ^CHITOL, must be > 0", status);
      }

      /* Normalized map pixel change tolerance for stopping */
      astMapGet0D( keymap, "MAPTOL", &maptol );

      if( (maptol != VAL__BADD) && (maptol <= 0) ) {
        *status = SAI__ERROR;
        msgSetd("MAPTOL",maptol);
        errRep(FUNC_NAME,
               FUNC_NAME ": MAPTOL is ^MAPTOL, must be > 0", status);
      }

      /* Does maptol refer to mean map change or max map change? */
      astMapGet0I( keymap, "MAPTOL_MEAN", &maptol_mean );

      /* Abort if the "mean change in map value" changes by less than
         "MAPTOL_RATE" between iterations. */
      astMapGet0D( keymap, "MAPTOL_RATE", &maptol_rate );

      /* A negative AST.SKIP value over-rides NUMITER. */
      ast_skip = 0;
      ast_filt_diff = 0.0;
      if( astMapGet0A( keymap, "AST", &kmap ) ) {
         astMapGet0I( kmap, "SKIP", &ast_skip );

         /* Remove low spatial frequencies in map-change? If non-zero,
            ast.filt_diff gives the filter size, in arc-secs. */
         astMapGet0F( kmap, "FILT_DIFF", &ast_filt_diff );
         kmap = astAnnul( kmap );
      }

      /* Number of iterations */
      if( ast_skip < 0 ) {
         numiter = ast_skip;
      } else {
         astMapGet0I( keymap, "NUMITER", &numiter );
      }

      if( numiter == 0 ) {
        *status = SAI__ERROR;
        errRep("", FUNC_NAME ": NUMITER cannot be 0", status);
      } else {
        if( (numiter < 0) && ((chitol != VAL__BADD)||(maptol != VAL__BADD)) ) {
          /* If negative, iterate to convergence or abs(numiter),
             whichever comes first -- if we have provided a stopping
             criterion */
          maxiter = abs(numiter);
          untilconverge = 1;
        } else {
          /* Otherwise iterate a fixed number of times */
          maxiter = numiter;
          untilconverge = 0;
        }
      }

      /* Lower limit for acceptable hits per pixel, expressed as a
         fraction of the mean hits per pixel over the map (excluding
         pixels that get no hits). */
      astMapGet0D( keymap, "HITSLIMIT", &hitslim );
    }

    /* Do iterations completely in memory - minimize disk I/O */
    if( *status == SAI__OK ) {
      int fcount;
      int nflags;
      char flagnames[SMF__NQBITS*SMF_QSTR_MAX];
      char *flagname;

      /* Are we going to produce single-bolo maps? */
      astMapGet0I( keymap, "BOLOMAP", &bolomap );

      /* Are we going to produce maps for each iteration? */
      astMapGet0I( keymap, "ITERMAP", &itermap );

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

      /* Flag indicating that the memory should be checked but no map
         should be created */
      astMapGet0I( keymap, "MEMCHECK", &memcheck );
      if( ( memcheck < 0 || memcheck > 2 ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "Bad value %d supplied for config parameter "
                  "'memcheck' - must be 0, 1 or 2.", status, memcheck );
      }

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

      /* Get the pixel size from the map header. We fudge a local map
         smfData so that we can call smf_map_getpixsize */
      localmap = smf_create_smfData( 0, status );
      if( localmap && (localmap->hdr) ) {
        localmap->hdr->wcs = outfset;
        memcpy( localmap->lbnd, lbnd_out, sizeof(localmap->lbnd) );
        pixsize = smf_map_getpixsize( localmap, status );

        /* Set the WCS to null again to avoid freeing the memory */
        localmap->hdr->wcs = NULL;
      }

      if( localmap ) smf_close_file( wf, &localmap, status );

      /* Are we downsampling the data? If the user specified a value
         less than 0, the scale is a multiple of PIXSIZE. */
      astMapGet0D( keymap, "DOWNSAMPSCALE", &downsampscale );
      if( (*status == SAI__OK) && (downsampscale < 0) ) {
         downsampscale = abs(downsampscale) * pixsize;
         astMapPut0D( keymap, "DOWNSAMPSCALE", downsampscale, NULL );
      }

      msgOutf( "", FUNC_NAME
               ": will down-sample data to match angular scale of %lg "
               "arcsec", status, downsampscale );

      /* Adding in signal from an external fakemap? */
      tempstr = NULL;
      astMapGet0C( keymap, "FAKEMAP", &tempstr );
      if( tempstr ) {
        fakemap = astCalloc( 255, 1 );
        one_strlcpy( fakemap, tempstr, 255, status );
      }

      astMapGet0D( keymap, "FAKESCALE", &fakescale );
      astMapGet0I( keymap, "FAKEMCE", &fakemce );
      astMapGet0D( keymap, "FAKEDELAY", &fakedelay );

      /* Subtracting an error map after each iteration? */
      tempstr = NULL;
      astMapGet0C( keymap, "EPSIN", &tempstr );
      if( tempstr ) {
        epsin = astCalloc( 255, 1 );
        one_strlcpy( epsin, tempstr, 255, status );
      }

      /* Dump the error map after the final iteration? */
      tempstr = NULL;
      astMapGet0C( keymap, "EPSOUT", &tempstr );
      if( tempstr ) {
        epsout = astCalloc( 255, 1 );
        one_strlcpy( epsout, tempstr, 255, status );
      }

    }

    /* Obtain sample length from header of first file in igrp */
    smf_open_file( wf, igrp, 1, "READ", SMF__NOCREATE_DATA, &data, status );
    if( (*status == SAI__OK) && (data->hdr) ) {
        steptime = data->hdr->steptime;
    } else {
        steptime = -1;
    }
    smf_close_file( wf, &data, status );

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
        /* Extra components for exportNDF_which for 'res', 'qua' and 'lut' */
        exportNDF_which = astCalloc( nmodels+3, sizeof(*exportNDF_which) );
      } else {
        msgOut(" ", FUNC_NAME ": No valid models in MODELORDER",
               status );
      }

      /* Loop over names and figure out enumerated type */
      for( imodel = 0; (*status == SAI__OK) && (imodel < nmodels); imodel++ ) {
        modelname = modelnames+imodel*4; /* Pointer to current name */
        thismodel = smf_model_gettype( modelname, status );

        if( *status == SAI__OK ) {
          modeltyps[imodel] = thismodel;

          /* set haveast/whichast */
          if( thismodel == SMF__AST ) {
            haveast = 1;
            whichast = imodel;
          }

          /* set havenoi/whichnoi */
          if( thismodel == SMF__NOI ) {
            havenoi = 1;
            whichnoi = imodel;
          }

          /* set haveext/whichext */
          if( thismodel == SMF__EXT ) {
            haveext = 1;
            whichext = imodel;
          }

          /* set haveflt/whichflt */
          if( thismodel == SMF__FLT ) {
            haveflt = 1;
            whichflt = imodel;
          }

          /* set havessn/whichssn */
          if( thismodel == SMF__SSN ) {
            havessn = 1;
            whichssn = imodel;
          }

          /* set havegai/whichgai */
          if( thismodel == SMF__GAI ) {
            havegai = 1;
            whichgai = imodel;
          }

          /* set havecom/whichcom */
          if( thismodel == SMF__COM ) {
            havecom = 1;
            whichcom = imodel;
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
        if( strtol(modelnames,NULL,10) == 1 && *status == SAI__OK ) {
          /* Export all of the model components */
          exportNDF = 1;
          for( imodel = 0; imodel <= nmodels; imodel++ ) {
            exportNDF_which[imodel] = 1;
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

          /* If the model type is 'lut' handle it here */
          if( thismodel == SMF__LUT ) {
            exportNDF = 1;
            exportNDF_which[nmodels+2]=1;
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

    if( chitol != VAL__BADD ) {
      msgOutf( "", FUNC_NAME ": stop when change in chi^2 < %lg", status,
               chitol );
    }

    if( maptol != VAL__BADD ) {
      msgOutf( "", FUNC_NAME ": stop when mean normalized map change < %lg",
               status, maptol );
    }
  } else {
    msgSeti("MAX",maxiter);
    msgOut(" ", FUNC_NAME ": ^MAX Iterations", status );
  }

  msgSeti("NUMCOMP",nmodels);
  msgOutif(MSG__VERB," ",
           FUNC_NAME ": ^NUMCOMP model components in solution: ",
           status);
  for( imodel = 0; imodel < nmodels; imodel++ ) {
    msgSetc( "MNAME", smf_model_getname(modeltyps[imodel], status) );
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

  smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    &groupsubarray, NULL, NULL, NULL, NULL, NULL, NULL, status );

  smf_grp_related( igrp, isize, 1+groupsubarray, 1, maxlen, &srate_maxlen,
                   keymap, &maxconcat, &maxfile, &igroup, NULL, &pad, status );

  if( srate_maxlen <= 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": error, sample rate used to convert maxlen_s to "
            "number of samples is <= 0", status );
  }

  if (igroup) {
    if( ncontig ) *ncontig = igroup->chunk[igroup->ngroups-1]+1;
    msgOutf( " ", FUNC_NAME ": provided data are in %" DIM_T_FMT
             " continuous chunks, the largest of which has %zu samples "
             "(%lg s)", status, igroup->chunk[igroup->ngroups-1]+1,
             maxconcat, maxconcat/srate_maxlen );
  }

  /* Once we've run smf_grp_related we know how many subarrays there
     are.  We also know the maximum length of a concatenated piece of
     data, and which model components were requested. Use this
     information to check that enough memory is available -- but now
     add in the extra length required for padding. */

  if( *status == SAI__OK ) {
    size_t mapmem;  /* memory needed for map */
    size_t maxdimm; /* maximum memory available just for model components */

    /* Add on the padding */
    maxconcat += 2*pad;
    msgOutiff( MSG__VERB," ", FUNC_NAME ": Each time stream will be padded "
              "with %" DIM_T_FMT "  samples at start and end.", status, pad );

    /* First check memory for the map and subtract off total memory to
       see what is available for model components. */
    smf_checkmem_map( lbnd_out, ubnd_out, 0, nw, maxmem, epsout, &mapmem,
                      status );
    maxdimm = maxmem-mapmem;

    /* Then the iterative components that are proportional to time */
    smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
                       nmodels, msize, keymap, maxdimm, maxfile,
                       &memneeded, status );

    /* If we need too much memory, generate a warning message and then
       see if we would have enough memory if we were to not use
       multi-threading in the function that rebins time-series data into
       a map. */
    if( *status == SMF__NOMEM ) {
      errAnnul( status );
      msgOutf( " ", FUNC_NAME ": *** WARNING ***\n  %zu continuous samples "
               "(%lg s, including padding) require %zu MiB > %zu MiB",
               status, maxconcat, maxconcat/srate_maxlen,
               memneeded/SMF__MIB, maxdimm/SMF__MIB);

      msgOut( "", "  Will try again without multi-threaded rebinning...",
              status );

    /* Check memory for the map again, this time without multi-threading
       in smf_rebinmap1. */
      smf_checkmem_map( lbnd_out, ubnd_out, 0, 1, maxmem, epsout, &mapmem,
                        status );
      maxdimm = maxmem-mapmem;

    /* Then the iterative components that are proportional to time */
      smf_checkmem_dimm( maxconcat, INST__SCUBA2, igroup->nrelated, modeltyps,
                         nmodels, msize, keymap, maxdimm, maxfile,
                         &memneeded, status );

    /* Annul the error and use a better message if there was still
       insufficient memory to avoid chunking. In this case we revert to
       using multi-threading in smf_rebinmap1. */
      if( *status == SMF__NOMEM ){
         chunking = 1;
         mw = nw;
         errAnnul( status );
         msgOutf( " ", FUNC_NAME ": %zu MiB available with less multi-threading "
                  "- still too low.", status, maxdimm/SMF__MIB );
      } else {
         chunking = 0;
         mw = 1;
         msgOutf( " ", FUNC_NAME ": %zu MiB available with less multi-threading "
                  "- so we can avoid chunking at the expense of slower "
                  "map-making.", status, maxdimm/SMF__MIB );
      }
    } else {
      chunking = 0;
      mw = nw;
    }

    /* Note if there was insufficient memory to avoid chunking. */
    if( memlow ) *memlow = chunking;

    /* If we need too much memory, generate a warning message and then try
       to re-group the files using smaller contchunks. */
    if( chunking && memcheck == 0 ) {

      /* Try is meant to be the largest contchunk of ~equal length
         that fits in memory. The first step uses the ratio of
         requested to available memory (rounded up to an integral
         number) to estimate the number of time steps for try. */
      ncontchunks = ceil((double)memneeded/(double)maxdimm);
      try = (size_t) ceil( maxconcat / ncontchunks );

      /* Then figure out how many files this corresponds to, round up
         to get integral number of files, and finally multiply by file
         length and add on padding to get back into time steps */

      try = (size_t) ceil((double)try/(double)maxfile)*maxfile + pad;

      /*  If we exceed available memory subtract off the length of
          one file. If we don't have enough memory even for one input
          file we're hooped. */

      if( (try > (maxconcat*( (double) maxdimm / (double) memneeded ))) &&
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

      /* Close igroup if needed before re-running smf_grp_related */

      if( igroup ) {
        smf_close_smfGroup( &igroup, status );
      }

      smf_grp_related( igrp, isize, 1+groupsubarray, 1,
                       ceil((double)try/srate_maxlen), NULL, keymap,
                       &maxconcat2, NULL, &igroup, NULL, NULL, status );

      /* If the chosen number of groups, each of the new chunk size, does
         not account for all the available data, then there will a little
         bit left over requiring another, very small, chunk which may
         cause an error because it is so small. To avoid this, increment
         the number of chunks and calculate a new chunk size. This will
         not change the number of chunks actually used, but may help to
         make them of more equal size. */
      if( maxconcat2*ncontchunks < maxconcat - 2*pad && *status == SAI__OK ) {
         ncontchunks++;
         try = (size_t) ceil( maxconcat / ncontchunks );
         try = (size_t) ceil((double)try/(double)maxfile)*maxfile + pad;
         if( (try > (maxconcat*( (double) maxdimm / (double) memneeded ))) &&
             (try > maxfile) ) try -= maxfile;

         if( try < (maxfile + pad) ) {
           *status = SMF__NOMEM;
           errRep( "", FUNC_NAME ": not enough memory available to break job "
                   "into smaller pieces.", status );
         }

         msgOutf( "", "  Will try to re-group data in chunks < %zu samples long "
                  "(%lg s)", status, try, ceil((double)try/srate_maxlen) );

         if( igroup ) smf_close_smfGroup( &igroup, status );

         smf_grp_related( igrp, isize, 1+groupsubarray, 1,
                          ceil((double)try/srate_maxlen), NULL, keymap,
                          &maxconcat2, NULL, &igroup, NULL, NULL, status );
      }
      msgOut( " ", FUNC_NAME ": ***************", status );

      /* Re-check memory usage using shorter chunks */
      if( *status == SAI__OK ) {
         smf_checkmem_dimm( maxconcat2, INST__SCUBA2, igroup->nrelated, modeltyps,
                            nmodels, msize, keymap, maxdimm, maxfile,
                            &memneeded, status );
      }
    }

    msgOutf( "", FUNC_NAME ": map-making requires %zu MiB "
             "(map=%zu MiB model calc=%zu MiB)", status,
             (mapmem+memneeded)/SMF__MIB, mapmem/SMF__MIB, memneeded/SMF__MIB );
  }

  /* If we are just checking the available memory, and not actually
     creating a map, we can now exit. We do this by reporting an error so
     that the output map will be deleted etc. */
  if( memcheck == 2 && *status == SAI__OK ) {
    *status = SMF__MEMCHK;
    errRep( "", FUNC_NAME ": memory is sufficient to avoid chunking, but "
            "no map will be created since config parameter memcheck is "
            "set to 2", status );
  }

  if( *status == SAI__OK ) {

    /* There are multiple large continuous pieces outside the
       iteration loop */
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;

    msgSeti( "NCONTCHUNKS", ncontchunks );
    msgOutif(MSG__VERB," ",
             FUNC_NAME ": ^NCONTCHUNKS large continuous chunks outside"
             " iteration loop.", status);
  }

  /* Load in the fakemap */
  if( fakemap && (*status==SAI__OK) ) {
    int nmap, tndf;
    void *ptr;
    msgOutf( "", FUNC_NAME ": loading external fakemap `%s'", status, fakemap );

    /* Open the NDF, get a section from it matching the bounds of the
       output map, then close the original NDF - retaining the section. .  */
    ndfFind( NULL, fakemap, &tndf, status );
    ndfSect( tndf, 2, lbnd_out, ubnd_out, &fakendf, status );
    ndfAnnul( &tndf, status );

    /* Map the data as double precision */
    ndfMap( fakendf, "DATA", "_DOUBLE", "READ", &ptr, &nmap, status );
    fmapdata = ptr;
  }

  /* Load in the error map */
  if( epsin && (*status==SAI__OK) ) {
    int nmap, tndf;
    void *ptr;
    msgOutf( "", FUNC_NAME ": loading external error map `%s'", status, epsin );

    /* Open the NDF, get a section from it matching the bounds of the
       output map, then close the original NDF - retaining the section. .  */
    ndfFind( NULL, epsin, &tndf, status );
    ndfSect( tndf, 2, lbnd_out, ubnd_out, &epsndf, status );
    ndfAnnul( &tndf, status );

    /* Map the data as double precision */
    ndfMap( epsndf, "DATA", "_DOUBLE", "READ", &ptr, &nmap, status );
    emapdata = ptr;
  }

  /* Report an error if epsout specified but we are creating more than one
     chunk. Otherwise allocate two map-sized buffers to store the second
     and third penultimate difference maps. The "mapchange" array will
     hold the first penultimate difference map. */
  if( epsout ) {
     if( ncontchunks > 1 && *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": error, config parameter EPSOUT cannot be "
                "used since the data is being split into multiple chunks.",
                status );
     } else {
        epsbuf1 = astMalloc( msize*sizeof( *epsbuf1 ) );
        epsbuf2 = astMalloc( msize*sizeof( *epsbuf2 ) );
        epsbuf3 = astMalloc( msize*sizeof( *epsbuf3 ) );
     }
  }




  /* ***************************************************************************
     Start the main outer loop over continuous chunks, or "contchunks".

     There is one loop over files apart from the iteration number. The
     idea is to concatenate all continuous data into several large chunks,
     and iterate each one of those to completion without any file i/o.
     These are called "contchunk".
   *************************************************************************** */
  sumchunkweights = 0.0;

  for( contchunk=0; contchunk<ncontchunks  && !smf_interupt; contchunk++ ) {

    size_t ntgood = 0;       /* Number of good time slices in this chunk */
    size_t nsamples = 0;     /* Number of good samples in this chunk */

    smfArray *noisemaps=NULL;/* Array of noise maps for current chunk */


#ifdef __ITERATEMAP_SHOW_MEM
    _smf_iteratemap_showmem(status);
#endif

    msgSeti("CHUNK", contchunk+1);
    msgSeti("NUMCHUNK", ncontchunks);
    msgOut( " ",
            FUNC_NAME ": Continuous chunk ^CHUNK / ^NUMCHUNK =========",
            status);

    /*** TIMER ***/
    smf_timerinit( &tv1, &tv2, status );

    if( *status == SAI__OK ) {

      /* Setup the map estimate from the current contchunk. */
      if( contchunk == 0 ) {
        /* For the first continuous chunk, calculate the map
           in-place */
        mapweights = astCalloc( msize, sizeof(*mapweights) );
        mapweightsq = astMalloc( msize*sizeof(*mapweightsq) );

        /* submaps for multithread rebinning -- each thread needs its own map.
           Requires copying onto input maps on first contchunk */
        thismap = astCalloc( mw*msize, sizeof(*thismap) );
        thisweight = astCalloc( mw*msize, sizeof(*thisweight) );
        thisweightsq = astCalloc( mw*msize, sizeof(*thisweightsq) );
        thisvar = astCalloc( mw*msize, sizeof(*thisvar) );
        thishits = astCalloc( mw*msize, sizeof(*thishits) );
        thisqual = astCalloc( mw*msize, sizeof(*thisqual) );
      }

      /* Concat everything in this contchunk into a single smfArray. Note
         that the pointing LUT gets generated in smf_concat_smfGroup below. */

      msgSeti("C",contchunk+1);
      msgOutif(MSG__VERB," ",
               FUNC_NAME ": Concatenating files in continuous chunk ^C",
               status);

      /* Allocate length 1 array of smfArrays. */
      res = astCalloc( 1, sizeof(*res) );

      /* Concatenate */
      smf_concat_smfGroup( wf, keymap, igroup, darks, bbms, flatramps, heateffmap,
                           contchunk, ensureflat, 0, outfset, moving,
                           lbnd_out, ubnd_out, fts_port, pad, pad,
                           noi_usevar?0:SMF__NOCREATE_VARIANCE, &res[0],
                           NULL, status );

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




















    }

    /* Allocate space for the chisquared array */
    if( havenoi && (*status == SAI__OK) ) {
      chisquared = astCalloc( 1, sizeof(*chisquared) );
      lastchisquared = astCalloc( 1, sizeof(*chisquared) );
    }

    /* Zero the array holding the map created on the previous iteration. */
    memset( lastmap, 0, msize*sizeof(*lastmap) );

    /* Create containers for time-series model components******************* */

    msgOutif(MSG__VERB," ", FUNC_NAME ": Create model containers", status);


    /* Allocate pointers to dynamically created models */
    if( igroup && (nmodels > 0) && (*status == SAI__OK) ) {

      /* nmodel array of pointers to a single smfArray pointer */
      model = astCalloc( nmodels, sizeof(*model) );

      for( imodel = 0; imodel < nmodels; imodel++ ) {
        model[imodel] = astCalloc( 1, sizeof(**model) );
      }

    }


    /* Components that always get made */
    if( igroup && (*status == SAI__OK) ) {

      /* there is one smfArray for LUT, AST and QUA */
      lut = astCalloc( 1, sizeof(*lut) );
      qua = astCalloc( 1, sizeof(*qua) );

      /* If iterating in memory then RES has already been created from
         the concatenation of the input data. Create the other
         required models using res[0] as a template. Assert
         bolo-ordered data although the work has already been done at
         the concatenation stage. */

      smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap,
                        NULL, 1, SMF__QUA, 0, NULL, 0, NULL, NULL, NO_FTS,
                        NULL, NULL, qua, keymap, status );

      /* Associate quality with the res model, and do cleaning before we
         start using more memory for other things. Note that we are
         guaranteed to have only one filegroup. Once the QUA model has
         been initialized with a copy of the quality inside RES, we can
         free up the quality in RES. */

      for( idx=0; idx<res[0]->ndat; idx++ ) {
        smfData *thisqua = qua[0]->sdata[idx];
        res[0]->sdata[idx]->sidequal = thisqua;
        if( res[0]->sdata[idx]->qual ) {
          res[0]->sdata[idx]->qual = astFree( res[0]->sdata[idx]->qual );
        }
      }


      /* Since a copy of the LUT is open in res[0], use it to initialize
         the LUT model and then free it */

      smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap,
                        NULL, 1, SMF__LUT, 0, NULL, 0, NULL, NULL, NO_FTS,
                        qua, NULL, lut, keymap, status );

      if( *status == SAI__OK ) {
         for( idat = 0; idat < res[0]->ndat; idat++ ) {
           if( res[0]->sdata[idat] ) {
             smf_close_mapcoord( res[0]->sdata[idat], status );
           }
         }
      }

      /* Even though EXT would normally be handled in the dynamic memory
         allocation, do it here explicitly so that we can add the fakemap
         signal to RES before cleaning. Be careful not to re-initialize
         it again later! */

      if( haveext ) {
        smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap,
                          noisemaps, 1, modeltyps[whichext], 0, NULL, 0, NULL,
                          NULL, NO_FTS, qua, NULL, model[whichext], keymap, status);
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

          /* If we will later be filtering the data to remove the MCE response
             or delay, we need to apply the opposite effects the fake data
             before adding it to the real data, so that the later filtering
             will affect only the real data and not the fake data. */
          if( fakemce || fakedelay != 0.0 ) {

             /* Sample the fake map at the position of each sample,
                applying extinction correction or not as
                required. Note that fakestream is set to 0 wherever
                there are no data, bad value encountered, etc. The
                quality normally flags wherever there are gaps (which
                then get filled), but in the case where fmapdata are
                missing values, QUALITY won't know about it, and we'll
                get junk when we do the filtering. A better way to do
                this would be to actually (temporarily) set some sort
                of quality (so that we can gap fill)... but probably
                not worth the effort. */
             fakestream = astGrow( fakestream, dsize, sizeof(*fakestream));
             if( haveext ) {

               extptr = model[whichext][0]->sdata[idx]->pntr[0];

               for( k=0; k<dsize; k++ ) {
                 if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                     (extptr[k] > 0) && (fmapdata[lutptr[k]] != VAL__BADD) &&
                     (resptr[k] != VAL__BADD) ) {
                   fakestream[k] = fakescale*fmapdata[lutptr[k]] / extptr[k];
                 } else {
                   fakestream[k] = 0;
                 }
               }
             } else {
               for( k=0; k<dsize; k++ ) {
                 if( (resptr[k] != VAL__BADD) && (lutptr[k] != VAL__BADI) &&
                     (fmapdata[lutptr[k]] != VAL__BADD) &&
                     (resptr[k] != VAL__BADD) ) {
                   fakestream[k] = fakescale*fmapdata[lutptr[k]];
                 } else {
                   fakestream[k] = 0;
                 }
               }
             }

             /* Apply any delay specified by the "fakedelay" config parameter,
                and also smooth with the MCE response. These are done in the
                opposite order to that used in smf_clean_smfArray. We
                temporarily hijack the RES smfData for this purpose. */
             res[0]->sdata[idx]->pntr[0] = fakestream;

             smfFilter *filt = smf_create_smfFilter(res[0]->sdata[idx], status);
             if( fakedelay != 0.0 ) {
               msgOutiff( MSG__VERB, "", FUNC_NAME
                          ": delay fake data by %.4lf s",
                          status, fakedelay );
               smf_filter_delay( filt, fakedelay, status );
             }

             if( fakemce ) {
               msgOutif( MSG__VERB, "", FUNC_NAME
                          ": convolve fake data with anti-aliasing filter",
                         status );
               smf_filter_mce( filt, 1, status );
             }

             smf_update_quality( wf, res[0]->sdata[idx], 1, NULL, 0, 0.05, status );
             smf_filter_execute( wf, res[0]->sdata[idx], filt, 0, 0, status );

             filt = smf_free_smfFilter( filt, status );

             res[0]->sdata[idx]->pntr[0] = resptr;

             /* Add the modified fake time stream data onto the residuals. */
             for( k=0; k<dsize; k++ ) {
                if( resptr[k] != VAL__BADD && fakestream[k] != VAL__BADD ){
                  resptr[k] += fakestream[k];
                }
             }

          /* If we will not be filtering the data later, we do not need
             to find the intermediate fake time stream data. */
          } else {
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

        fakestream = astFree( fakestream );

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

        /* Ensure all bad values in the pre-cleaned data have a bad
           quality. */
        for( idx=0; idx<res[0]->ndat; idx++ ) {
           smf_update_quality( wf, res[0]->sdata[idx], 1, NULL, 0, 0.25, status );
        }

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

      for( imodel = 0; imodel < nmodels; imodel++ ) {

        /* Don't do SMF__LUT or SMF__EXT as they were handled earlier.
           Also we do not need to allocate models to hold SMF__AST as the
           AST values are calculated on-the-fly from the current map.  */
        if( (modeltyps[imodel] != SMF__LUT) && (modeltyps[imodel] != SMF__EXT) &&
            (modeltyps[imodel] != SMF__AST) ) {
          smf_model_create( wf, NULL, res, darks, bbms, flatramps, heateffmap,
                            noisemaps, 1, modeltyps[imodel], 0, NULL, 0, NULL, NULL,
                            NO_FTS, qua, NULL, model[imodel], keymap, status );
        }

        /* Associate quality with some models */
        if( ( modeltyps[imodel] == SMF__FLT ||
              modeltyps[imodel] == SMF__SSN ) && *status == SAI__OK ) {
          for( idx=0; idx<res[0]->ndat; idx++ ) {
            smfData *thisqua = qua[0]->sdata[idx];
            model[imodel][0]->sdata[idx]->sidequal = thisqua;
          }
        }

      }


































    }

    /*** TIMER ***/
    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME": ** %f s creating dynamic models",
               status, smf_timerupdate(&tv1,&tv2,status) );

#ifdef __ITERATEMAP_SHOW_MEM
    _smf_iteratemap_showmem(status);
#endif

    /* We initialize the structure even if status is bad to prevent
       downstream problems when freeing resources. */
    memset( &dat, 0, sizeof(dat) );

    if( *status == SAI__OK ) {

      /* Stuff pointers into smfDIMMData to pass around to model component
         solvers */
      dat.ast_skipped = 1;
      dat.res = res;
      dat.qua = qua;
      dat.lut = lut;
      dat.map = thismap;
      dat.lastmap = lastmap;
      dat.hitsmap = thishits;
      dat.mapqual = thisqual;
      dat.mapvar = thisvar;
      dat.mapweight = thisweight;
      dat.mapweightsq = thisweightsq;
      dat.mapok = 0;
      dat.mdims[0] = mdims[0];
      dat.mdims[1] = mdims[1];
      dat.msize = msize;
      dat.outfset = outfset;
      dat.lbnd_out = lbnd_out;
      dat.ubnd_out = ubnd_out;
      dat.chisquared = chisquared;
      dat.pixsize = pixsize;
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
      if( havecom ) {
        dat.com = model[whichcom];
      } else {
        dat.com = NULL;
      }

      /* We can close noisemaps here because they will already have
         been used to initialize the NOI model if needed. */

      if( noisemaps ) smf_close_related( wf, &noisemaps, status );

      /* Allow an initial guess at the sky brightness to be supplied, in
         which case copy it into "thismap", sample it and subtract it from
         the cleaned data. The initial guess is returned in "lastmap". */
      importsky = smf_initial_sky( wf, keymap, &dat, &itsdone, status );

      /* If an initial sky was imported, copy it into the "lastmap" array
         so that we get a reasonable value for the normalised change in the
         map at the end of the iteration. */
      if( importsky && *status == SAI__OK ) memcpy( lastmap, dat.map,
                                                    msize*sizeof(*lastmap) );








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
         end of the last loop to exit. If an initial sky was supplied
         that was created by a previous interupted run of makemap, we
         start counting iterations from where the previous run of makemap
         left off.  */
      quit = -1;
      iter = ( itsdone == -1 ) ? 0 : itsdone;

      /* The NOI model has not been calculated yet. */
      noidone = 0;

      /* The mean mapchange for the previous two iterations. */
      mapchange_l2 = mapchange_l3 = VAL__BADD;
      rate_limited = 0;

      /* The "iter" variable counts how many iterations have been done in
         total, including any from a previous run of makemap if an initial sky
         image was given that was generated by makemap. We also need a
         flag which indicates if this is the first iteration, ignoring
         any such previous iterations. */
      firstiter = 1;

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

        /* If we have reached the last possible iteration, we will quit
           after this iteration. */
        if( iter + 1 >= maxiter ) quit = 0;

        /* Some models (e.g. AST) need to know the iteration number, so
           store it now. */
        dat.iter = iter;










        /* *********************************************************************
           Start the calculation of all the model components up to AST
           (including models that follow AST in modelorder, and then wrapping
           back around to the beginning).
        ********************************************************************* */


        /* If first iteration report on initial stats and write out
           cleaned data if requested. */
        if( iter == 0 ) {

          /* initial quality report */
          smf_qualstats_report( wf, MSG__NORM, SMF__QFAM_TSERIES, 1, qua[0],
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

            for( idx=0; idx<res[0]->ndat; idx++ ) {
              int oldorder;
              data = res[0]->sdata[idx];

              /* create a file name with "_res_cln" suffix */
              smf_stripsuffix( res[0]->sdata[idx]->file->name,
                               SMF__DIMM_SUFFIX, name, status );
              one_strlcat( name, "_res_cln", SMF_PATH_MAX+1, status );

              /* Use the correct order */
              oldorder = res[0]->sdata[idx]->isTordered;
              smf_dataOrder( wf, res[0]->sdata[idx], 1, status );
              smf_dataOrder( wf, qua[0]->sdata[idx], 1, status );

              smf_write_smfData( wf, res[0]->sdata[idx], NULL,
                                 name, NULL, 0, NDF__NOID,
                                 MSG__VERB, 0, status );

              /* Revert the order */
              smf_dataOrder( wf, res[0]->sdata[idx], oldorder, status );
              smf_dataOrder( wf, qua[0]->sdata[idx], oldorder, status );
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
               iteration, we need to undo some models (COM, GAI, EXT, FLT ).
               Undo them in the reverse order to which they were done. */

            if( (j==0) && !firstiter ) {
              dim_t jj = whichast - 1;
              while( jj != whichast ) {

                if( jj == whichcom && havecom ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing COM from previous iteration",
                             status );
                  smf_calcmodel_com( wf, &dat, 0, keymap, model[whichcom],
                                     SMF__DIMM_INVERT, status );
                  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                             ": ** %f s undoing COM",
                             status, smf_timerupdate(&tv1,&tv2,status) );

                } else if( jj == whichext && haveext ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing EXTinction from previous iteration",
                             status );
                  smf_calcmodel_ext( wf, &dat, 0, keymap, model[whichext],
                                     SMF__DIMM_INVERT, status );
                  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                             ": ** %f s undoing EXT",
                             status, smf_timerupdate(&tv1,&tv2,status) );

                } else if( jj == whichgai && havegai ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing GAIn from previous iteration",
                             status );
                  smf_calcmodel_gai( wf, &dat, 0, keymap, model[whichgai],
                                     SMF__DIMM_INVERT, status );
                  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                             ": ** %f s undoing GAI",
                             status, smf_timerupdate(&tv1,&tv2,status) );

                } else if( jj == whichflt && haveflt && flt_undofirst ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing FLT from previous iteration",
                             status );
                  smf_calcmodel_flt( wf, &dat, 0, keymap, model[whichflt],
                                     SMF__DIMM_INVERT, status );
                  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                             ": ** %f s undoing FLT",
                             status, smf_timerupdate(&tv1,&tv2,status) );

                } else if( jj == whichssn && havessn ) {
                  msgOutiff( MSG__VERB, "",
                             "  ** undoing SSN from previous iteration",
                             status );
                  smf_calcmodel_ssn( wf, &dat, 0, keymap, model[whichssn],
                                     SMF__DIMM_INVERT, status );
                  msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                             ": ** %f s undoing SSN",
                             status, smf_timerupdate(&tv1,&tv2,status) );
                }

                if( jj <= 0 ) {
                  jj = nmodels - 1;
                } else {
                  jj--;
                }

              }
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
            if( quit == 0 ) dimmflags |= SMF__DIMM_LASTITER;

            if( *status == SAI__OK ) {

              /* Before subtraction of the model, dump the original
                 residuals. */
              smf_diagnostics( wf, 0, &dat, contchunk, keymap, model[j],
                               modeltyps[j], dimmflags, status );

              /* Estimate the new model and subtract it from the residuals. */
              (*modelptr)( wf, &dat, 0, keymap, model[j], dimmflags, status );

              /* After subtraction of the model, dump the model itself
                 and the modified residuals. */
              smf_diagnostics( wf, 1, &dat, contchunk, keymap, model[j],
                               modeltyps[j], dimmflags, status );

              /* Set a flag if we now have a NOI model. */
              if( modeltyps[j] == SMF__NOI ) noidone = 1;

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
          smf_model_dataOrder( wf, &dat, NULL, 0, SMF__RES|SMF__LUT|SMF__QUA,
                               res[0]->sdata[0]->isTordered, status );

          /* Loop over subgroup index (subarray), placing last map
             estimate (AST) into RES. */
          for( idx=0; idx<res[0]->ndat; idx++ ) {

            res_data = (res[0]->sdata[idx]->pntr)[0];
            lut_data = (lut[0]->sdata[idx]->pntr)[0];
            qua_data = (qua[0]->sdata[idx]->pntr)[0];

            smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                          &dsize, NULL, NULL, status );

            /* Set up jobs to add last iter. of astronomical signal back in
               to residual. Note that if this is the first iteration we do
               not yet have a map estimate so we skip this step (in multiple
               chunk case thismap will still contain the old map from
               the previous chunk). Ignore map pixels that have been
               constrained to zero. We also skip this bit if the
               subtraction of AST was skipped on the previous invocation
               of smf_calcmodel_ast. */
            if( ( iter > 0 || importsky ) && !dat.ast_skipped ) {

              /* First find how many samples to process in each worker thread. */
              size_t sampstep = dsize/nw;
              if( sampstep == 0 ) sampstep = 1;

              /* Store the range of samples to be processed by each thread.
                 Ensure that the last thread picks up any left-over samples. */
              for( iw = 0; iw < nw; iw++ ) {
                pdata = job_data + iw;
                pdata->d1 = iw*sampstep;
                if( iw < nw - 1 ) {
                  pdata->d2 = pdata->d1 + sampstep - 1;
                } else {
                  pdata->d2 = dsize - 1 ;
                }

                /* Store other values common to all jobs. */
                pdata->qua_data = qua_data;
                pdata->res_data = res_data;
                pdata->lut_data = lut_data;
                pdata->thismap = thismap;
                pdata->thisqual = thisqual;
                pdata->operation = 1;

                /* Submit the job to the workforce. */
                thrAddJob( wf, 0, pdata, smf1_iteratemap, 0, NULL, status );
              }

              /* Wait for all jobs to complete. */
              thrWait( wf, status );
            }
          }

          /* See if the map variances calculated on the previous iteration
             should be retained. This is the case if we are doing the
             last iteration, the FLT model is being used and a different
             FLT filter has been used for this iteration. */
          reuse_var = 0;
          if( quit == 0 && haveflt && astMapGet0A( keymap, "FLT", &kmap ) ) {
             double val, lastval;
             astMapGet0D( kmap, "FILT_EDGE_LARGESCALE", &val );
             lastval = val;
             astMapGet0D( kmap, "FILT_EDGE_LARGESCALE_LAST", &lastval );
             if( val != lastval ) {
                reuse_var = 1;
                msgOutif( MSG__VERB, "", FUNC_NAME
                          ": FLT.FILT_EDGE_LARGESCALE_LAST is set so the "
                          "map variances from the penultimate iteration "
                          "will be returned.", status );
             }
             kmap = astAnnul( kmap );
          }

          /* Loop over subgroup index (subarray) again. This time rebin and
             calculate the new map. */
          for( idx=0; idx<res[0]->ndat; idx++ ) {

            res_data = (res[0]->sdata[idx]->pntr)[0];
            lut_data = (lut[0]->sdata[idx]->pntr)[0];
            qua_data = (qua[0]->sdata[idx]->pntr)[0];

            smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                          &dsize, NULL, NULL, status );

            /* Setup rebin flags */
            rebinflags = 0;
            if( idx == 0 ) {
              /* First call to rebin clears the arrays */
              rebinflags = rebinflags | AST__REBININIT;
            }

            if( idx == res[0]->ndat-1 ) {
              /* Final call to rebin re-normalizes */
              rebinflags = rebinflags | AST__REBINEND;
            }

            /* Rebin the residual + astronomical signal into a map */
            smf_rebinmap1( ( mw > 1 ) ? wf : NULL, res[0]->sdata[idx],
                           dat.noi ? dat.noi[0]->sdata[idx] : NULL,
                           lut_data, 0, 0, 0, NULL, 0, SMF__Q_GOOD,
                           varmapmethod, rebinflags, thismap, thisweight,
                           thisweightsq, thishits, reuse_var ? NULL : thisvar,
                           msize, &scalevar, status );
          }

          /* Indicate the map arrays within the supplied smfDIMMData
             structure now contain usable values. */
          dat.mapok = 1;

          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s rebinning map",
                     status, smf_timerupdate(&tv1,&tv2,status) );

          /* If required, modify the map to remove low frequencies changes
             between the new map and the old map. We do not do this if
             the AST model was skipped on the previous iteration. */
          if( ast_filt_diff > 0.0 && !dat.ast_skipped ) {

             /* Can only do this if we have a map from the previous
                iteration (i.e. this is not the ifrst iter). */
             if( importsky || iter > 1 ) {

                /* Do not do it on the final iteration since we want the
                   final map to contain all the residual flux. If run
                   from skyloop (as indicated by numiter being 1), we
                   always defer this until after the full map has been
                   made from all chunks.  */
                if( quit == -1 && abs( numiter ) > 1 ) {
                   smf_filter_mapchange( wf, &dat, ast_filt_diff, status );
                }
             }
          }

          /* If required, subtract an external error map from the map
             created above. */
          if( emapdata && !dat.ast_skipped ) {
              double *p1 = thismap;
              double *p2 = lastmap;

              p1 = thismap;
              p2 = emapdata;
              for( ipix = 0; ipix < msize; ipix++,p1++,p2++ ) {
                if( *p1 != VAL__BADD && *p2 != VAL__BADD ) {
                  *p1 -= *p2;
                } else {
                  *p1 = VAL__BADD;
                }
              }

              msgOutf( "", FUNC_NAME ": subtracted external error map `%s'", status, epsin );
          }

          /* Replace the map with a linear combination of the original map
            and the map from the previous iteration. This may help to
            damp oscillations in the map from iteration to iteration. */
          if( iter > 1 ) {
            double maplag = 0.0;
            astMapGet0D( keymap, "MAPLAG", &maplag );
            if( iter > 1 && maplag != 0.0 ) {
              double *p1 = thismap;
              double *p2 = lastmap;
              double w1 = 1.0 - maplag;
              for( ipix = 0; ipix < msize; ipix++,p1++,p2++ ) {
                if( *p1 != VAL__BADD && *p2 != VAL__BADD ) {
                   *p1 = w1*( *p1 ) + maplag*( *p2 );
                }
              }
            }
          }
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

          /* Calculate the AST model component. It is a special model
             because it assumes that the map contains the best current
             estimate of the astronomical sky. It gets called in this
             separate loop since the map estimate gets updated by
             each filegroup in the main model component loop */

          dimmflags=0;
          if( iter == 0 ) dimmflags |= SMF__DIMM_FIRSTITER;
          if( quit == 0 ) dimmflags |= SMF__DIMM_LASTITER;


          /* Before subtraction of the model, dump the original
             residuals. */
          smf_diagnostics( wf, 0, &dat, contchunk, keymap, NULL,
                           SMF__AST, dimmflags, status );

          /* Remember if no AST model was subtracted from the
             previous iteration. The dat.ast_skipped flag is updated
             within smf_calcmodel_ast, so we store its current value
             now, before it is changed. It is used to decide on
             convergence. But if all iterations are being skipped
             we leave it set to zero to prevent it influencing the
             termination critirion. */
          if( ast_skip > 0 ) last_skipped = dat.ast_skipped;

          /* Estimate the AST model and subtract from the residuals. */
          smf_calcmodel_ast( wf, &dat, 0, keymap, NULL, dimmflags, status );

          /*** TIMER ***/
          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s calculating AST",
                     status, smf_timerupdate(&tv1,&tv2,status) );

          /* If storing each iteration in an extension do it here if this
             was the last filegroup of data to be added */

          if( itermap > 0 ) {
            smf_write_itermap( wf, thismap, thisvar,
                               ( itermap > 1 ) ? thisqual : NULL, msize,
                               iterrootgrp, contchunk, iter, lbnd_out,
                               ubnd_out, outfset, res[0]->sdata[0]->hdr,
                               qua[0], status );

            /*** TIMER ***/
            msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                       ": ** %f s writing itermap",
                       status, smf_timerupdate(&tv1,&tv2,status) );
          }

          /* After subtraction of the model, dump the model itself
             and the modified residuals. */
          smf_diagnostics( wf, 1, &dat, contchunk, keymap, NULL,
                           SMF__AST, dimmflags, status );


          /* And dump RES as a model in its own right. */
          smf_diagnostics( wf, 1, &dat, contchunk, keymap, NULL,
                           SMF__RES, dimmflags,  status );


#ifdef __ITERATEMAP_SHOW_MEM
            _smf_iteratemap_showmem(status);
#endif

          /* report on the quality flags for this iterations before closing
           the quality */
          smf_qualstats_report( wf, MSG__NORM, SMF__QFAM_TSERIES, 1, qua[0],
                                qcount_last, &nsamples, 0, &ntgood, &numdata,
                                status );

          /* If no good bolos left, set status */
          if( (*status==SAI__OK) &&
              (qcount_last[smf_qual_to_bit(SMF__Q_BADB, status)] >= numdata)){
            *status = SMF__INSMP;
            errRep("", FUNC_NAME ": All bolos are bad", status );
          }

          /* Check for consistency between quality and data arrays */
          for( idx=0; (*status==SAI__OK)&&(idx<res[0]->ndat); idx++ ) {
            size_t nbad;
            nbad = smf_check_quality( wf, res[0]->sdata[idx], 0, status );
            if( nbad ) {
              msgOut( "", FUNC_NAME ": *** Possible programming error! ***",
                      status );
              msgOutf( "", FUNC_NAME ": %zu QUALITY/DATA inconsistencies "
                       "subarray %zu", status, nbad, idx );
              msgOut( "", FUNC_NAME ": ***********************************",
                      status );
            }
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
              msgSetd("CHISQ",chisquared[0]);
              msgOut( " ",
                      FUNC_NAME ": *** CHISQUARED = ^CHISQ", status);

              if( ((iter > 0)&&(whichnoi<whichast)) ||
                  ((iter > 1)&&(whichnoi>whichast)) ) {
                /* Again, we have to check if NOI was calculated at least
                   twice, which depends on NOI and AST in MODELORDER */

                double chidiff;   /* temporary variable to store diff */

                chidiff = chisquared[0]-lastchisquared[0];

                msgSetd("DIFF", chidiff);
                msgOut( " ",
                        FUNC_NAME ": *** change: ^DIFF", status );

                if( chidiff > 0 ) {
                  msgOut( " ", FUNC_NAME
                          ": ****** WARNING! CHISQUARED Increased ******",
                          status );
                }

                /* Check for the chi^2 stopping criterion */
                if( untilconverge && (chitol!=VAL__BADD) ) {
                  if( (chidiff > 0) || (-chidiff > chitol) ) {
                    /* Found a chunk that isn't converged yet */
                    converged=0;
                  }
                }

                /* If the AST model was skipped on the previous iteration,
                   we have not converged yet. */
                if( untilconverge && last_skipped ) converged=0;

              } else {
                /* Can't converge until at least 2 consecutive chi^2... */
                converged=0;
              }

              /* Update lastchisquared */
              lastchisquared[0] = chisquared[0];

            }
          }
        }

        /* Calculate the absolute difference between the previous and
           current map pixels normalized by the map standard
           deviations. Once we're done, update lastmap to the current
           map. Ignore bad and zero-constrained pixels. */

        if( *status == SAI__OK ) {

          /* Pixels with very low hits will have unreliable variances. So
             exclude pixels with hits below "hitslim" times the mean from
             the mapchange estimate. */
          if( hitslim > 0.0 ) {
            int *ph = thishits;
            int ngood = 0;
            nhitslim = 0;
            for( ipix = 0; ipix < msize; ipix++,ph++ ) {
               if( *ph > 0 ) {
                  nhitslim += *ph;
                  ngood++;
               }
            }
            nhitslim *= hitslim/ngood;
          } else {
            nhitslim = 0;
          }

          /* If we will be dumping the final error map, we need to retain
             copies of the three most recent difference maps. "epsbuf3"
             currently holds the most recent difference map, so copy it to
             epsbuf1 or epsbuf2 (which ever is oldest) before changing
             its contents. */
          if( epsout ) memcpy( ( iter % 2 == 0 ) ? epsbuf1 : epsbuf2,
                               epsbuf3, msize*sizeof(*epsbuf3) );

          mapchange_max = 0;
          for( ipix = 0; ipix < msize; ipix++ ) {

            if( thismap[ipix] != VAL__BADD && lastmap[ipix] != VAL__BADD ) {
              double vdiff = thismap[ipix] - lastmap[ipix];
              if( epsout) epsbuf3[ipix] = vdiff;

              if( !(thisqual[ipix]&SMF__MAPQ_AST) && (thisvar[ipix] != VAL__BADD)
                  && (thisvar[ipix] > 0) && (thishits[ipix] > nhitslim) ) {
                mapchange[ipix] = fabs( vdiff ) / sqrt(thisvar[ipix]);

                /* Update max */
                if( mapchange[ipix] > mapchange_max ) mapchange_max = mapchange[ipix];
              } else {
                mapchange[ipix] = VAL__BADD;
              }

            } else {
              mapchange[ipix] = VAL__BADD;
              if( epsout ) epsbuf3[ipix] = VAL__BADD;
            }
          }

          /* Calculate the mean change */
          smf_stats1D( mapchange, 1, msize, NULL, 0, 0, &mapchange_mean, NULL,
                       NULL, NULL, status );

          /* If there were insufficient samples in the masked area, then
             just annul the error since it just means that there are no
             bright sources in the map. */
          if( *status == SMF__INSMP ) {
             errAnnul( status );
             mapchange_mean = 0.0;
             mapchange_max = 0.0;

             if( quit < 0 ) {
                msgOut( "", FUNC_NAME ": *** No source pixels found", status );

             /* There is almost no point in doing any more iterations, since
                the AST model is zero and so we'll get exactly the same map
                on subsequent iterations. But it may be that a different
                set of config parameters have been specified for the final
                iteration (the "xxx_LAST" parameters), so we do one more
                in order to ensure these parameters are used. Setting
                mapchange_xxx to zero above will cause the following code
                to think that convergence has been achieved and will so
                trigger a final iteration. */
                msgOutif( MSG__VERB, "", FUNC_NAME ":     Doing one more iteration "
                          "to use any '..._LAST' config parameter values", status );
             }
             last_skipped = 0;
             converged = 1;

          }

          msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                     ": ** %f s calculating change in the map",
                     status, smf_timerupdate(&tv1,&tv2,status) );

          msgOutf( "", FUNC_NAME ": *** NORMALIZED MAP CHANGE: %lg (mean) "
                   "%lg (max)", status, mapchange_mean, mapchange_max );

          /* Check for the map change stopping criterion. Do not modify
             the converged flag on the extra iteration that is done after
             convergence has been reached (i.e. when quit=0).  We do not
             allow convergence to be reached until we have done at least
             one iteration in which the AST model was not skipped. */

          tol = maptol_mean ? mapchange_mean : mapchange_max;
          if( untilconverge &&
              ( ( (maptol!=VAL__BADD) && (tol > maptol) ) ||
                last_skipped ) && quit == -1 ) {
            /* Map hasn't converged yet */
            converged=0;
          }


          /* if the mean mapchange becomes constant, we'll never get to
             convergence, so quit. */
          if( !last_skipped && maptol_rate != VAL__BADD && mapchange_l3 != VAL__BADD ) {
             double mmc = ( mapchange_mean + mapchange_l2 + mapchange_l3 )/3;
             double mclim = maptol_rate*mmc;
             if( fabs( mapchange_mean - mmc ) < mclim &&
                 fabs( mapchange_l2 - mmc ) < mclim &&
                 fabs( mapchange_l3 - mmc ) < mclim ) {
                msgOutf( "", FUNC_NAME ": *** Normalised map change has "
                         "not changed significantly over the previous 3 "
                         "iterations - quiting immediately.", status );
                quit = 1;
                rate_limited = 1;
             }
          }
          mapchange_l3 = mapchange_l2;
          mapchange_l2 = mapchange_mean;
        }

        /* Increment iteration counter */
        iter++;
        firstiter = 0;

        if( *status == SAI__OK ) {

          /* If quit was set to 0 last time through we can now exit the
             loop */
          if( quit == 0 ) {
            quit = 1;
          } else if( quit < 0 ){
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

            /* Check to see if a forced exit is required as a result of an
               interupt. If so, indicate that one last iteration should be
               performed before terminating. */
            if( smf_interupt && *status == SAI__OK ) {
               msgBlank( status );
               msgBlank( status );

               msgOut( "", ">>>> Interupt detected!!! What should we do "
                       "now? Options are: ", status );
               msgOut( "", "1 - abort immediately with an error status",
                       status );
               msgOut( "", "2 - close the application returning the current "
                       "output map", status );
               msgOut( "", "3 - do one more iteration to finialise the "
                       "map and then close", status );
               msgBlank( status );
               msgOut( "", "NOTE - another interupt will abort the "
                       "application, potentially leaving files in an "
                       "unclean state.", status );
               msgBlank( status );
               parCancl( "INTOPTION", status );
               parGdr0i( "INTOPTION", 3, 1, 3, 1, &intopt, status );
               msgBlank( status );

               if( intopt == 1 ) {
                  *status = SAI__ERROR;
                  errRep( "", "Application aborted by an interupt.", status );
               } else if( intopt == 2 ) {
                  *iters = iter;
                  quit = 1;
               } else {
                  *iters = iter + 1;
                  quit = 0;
               }
            }
          }

        } else {
          quit = 1;
        }

/* If another iteration is to be done, copy the map created by this
   iteration into the "lastmap" array. Otherwise, leave "lastmap" unchanged
   so that it is available later on, if needed. */
        if( quit < 1 ) memcpy( lastmap, thismap, msize*sizeof(*lastmap) );
      }

      /* If we are dumping the final error map, check we have done at
         least 3 iterations. */
      if( epsout && *status == SAI__OK ) {
         if( iter < 4 ) {
            *status = SAI__ERROR;
            errRep( "", FUNC_NAME ": error, config parameter EPSOUT cannot be "
                    "used since too few iterations have been performed.",
                    status );

         /* Otherwise map the epsout NDF data array and store the median of
            the three last difference maps in it. */
         } else {
            int place, tndf, el;
            double *ip;
            size_t pixstep;

            msgOutf( "", FUNC_NAME ": creating output error map `%s'",
                     status, epsout );
            ndfPlace( NULL, epsout, &place, status );
            ndfNew( "_DOUBLE", 2, lbnd_out, ubnd_out, &place, &tndf, status );
            ndfMap( tndf, "DATA", "_DOUBLE", "WRITE", (void **) &ip, &el,
                    status );

            pixstep = msize/nw;
            if( pixstep == 0 ) pixstep = 1;

            for( iw = 0; iw < nw; iw++ ) {
              pdata = job_data + iw;
              pdata->d1 = iw*pixstep;
              if( iw < nw - 1 ) {
                pdata->d2 = pdata->d1 + pixstep - 1;
              } else {
                pdata->d2 = msize - 1;
              }

              pdata->epsout = ip;
              pdata->err1 = epsbuf1;
              pdata->err2 = epsbuf2;
              pdata->err3 = epsbuf3;
              pdata->operation = 2;

              /* Submit the job to the workforce. */
              thrAddJob( wf, 0, pdata, smf1_iteratemap, 0, NULL, status );
            }

            /* Wait for all jobs to complete. */
            thrWait( wf, status );

            ndfAnnul( &tndf, status );
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

      /* Set map pixels bad if they have very low hits. */
      if( hitslim > 0 ) {
        int *ph = thishits;
        int nrej = 0;
        for( ipix = 0; ipix < msize; ipix++ ) {
          if( *(ph++) < nhitslim ) {
            thismap[ ipix ] = thisvar[ ipix ] = thisweight[ ipix ] = VAL__BADD;
            nrej++;
          }
        }
        if( nrej > 0 ) {
           msgOutf( "", "Setting %d map pixels bad because they contain "
                    "fewer than %d samples (=%g of the mean samples per pixel).",
                    status, nrej, nhitslim, hitslim );
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

      /* Are we going to produce short maps every SHORTMAP time slices?
         If the user supplies -1, this will be interpreted as "1.0
         seconds", so trap this and revert to the -1 value used to
         indicate that a map should be created each time a full pass
         through the scan pattern has # been completed. */
      dim_t dimval;
      if( smf_get_nsamp( keymap, "SHORTMAP", res[0]->sdata[0], &dimval,
                         status ) == -1.0 ) {
         shortmap = -1;
      } else {
         shortmap = dimval;
      }

      if( bolomap || shortmap || sampcube ) {

        /* Ensure we use the RES model ordering. */
        smf_model_dataOrder( wf, &dat, NULL, 0, SMF__RES|SMF__LUT|SMF__QUA,
                             res[0]->sdata[0]->isTordered, status );

        for( idx=0; (idx<res[0]->ndat)&&(*status==SAI__OK); idx++ ){
          smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                        &dsize, NULL, NULL, status );

          res_data = res[0]->sdata[idx]->pntr[0];
          lut_data = lut[0]->sdata[idx]->pntr[0];
          qua_data = qua[0]->sdata[idx]->pntr[0];

          /* Add ast back into res. Mask should match ast_calcmodel_ast. */
          for( k=0; k<dsize; k++ ) {
            if( !(qua_data[k]&SMF__Q_MOD) && (lut_data[k]!=VAL__BADI) ) {
              double ast_data = thismap[lut_data[k]];
              if( ast_data != VAL__BADD ) {
                res_data[k] += ast_data;
              }
            }
          }

        }
      }

      /* Create sub-maps for each bolometer if requested. */
      if( bolomap ) {
        smf_write_bolomap( wf, res[0], lut[0], qua[0], &dat, msize,
                           bolrootgrp, varmapmethod, lbnd_out, ubnd_out,
                           outfset, NULL, status );

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s writing bolomap",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      }

      /* Create short maps using every SHORTMAP samples if requested */

      if( shortmap ) {
        smf_write_shortmap( wf, shortmap, res[0], lut[0], qua[0], &dat,
                            msize, shortrootgrp, contchunk, varmapmethod,
                            lbnd_out, ubnd_out, outfset, status );
        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s writing shortmap",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      }

      /* If we're writing out only the final map from each chunk, do it here */
      if( itermap < 0 ) {
        smf_write_itermap( wf, thismap, thisvar,
                           ( itermap < -1 ) ? thisqual : NULL, msize,
                           iterrootgrp, contchunk, iter, lbnd_out,
                           ubnd_out, outfset, res[0]->sdata[0]->hdr,
                           qua[0], status );
        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s writing itermap",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      }

      /* Create a data sample cube where the first two dimensions
         match the map, and the third dimension enumerates samples that
         land in each pixel */

      if( sampcube ) {
        smf_write_sampcube( wf, res[0], lut[0], qua[0], &dat, thishits,
                            samprootgrp, contchunk, lbnd_out, ubnd_out,
                            status );

        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s writing sampcube",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      }

      /* Now we can remove AST from RES again before continuing */

      if( bolomap || shortmap || sampcube ) {

        /* Ensure we use the RES model ordering.  */
        smf_model_dataOrder( wf, &dat, NULL, 0, SMF__RES|SMF__LUT|SMF__QUA,
                             res[0]->sdata[0]->isTordered, status );

        for( idx=0; (idx<res[0]->ndat)&&(*status==SAI__OK); idx++ ){
          smf_get_dims( res[0]->sdata[idx], NULL, NULL, NULL, NULL,
                        &dsize, NULL, NULL, status );

          res_data = res[0]->sdata[idx]->pntr[0];
          lut_data = lut[0]->sdata[idx]->pntr[0];
          qua_data = qua[0]->sdata[idx]->pntr[0];

          /* Remove ast from res again. Mask should match ast_calcmodel_ast. */
          for( k=0; k<dsize; k++ ) {
            if( !(qua_data[k]&SMF__Q_MOD) && (lut_data[k]!=VAL__BADI) ) {
              double ast_data = thismap[lut_data[k]];
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
        smf_write_flagmap( wf, flagmap, lut[0], qua[0], &dat, flagrootgrp,
                           contchunk, lbnd_out, ubnd_out, outfset, status );
        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s writing flagmap",
                   status, smf_timerupdate(&tv1,&tv2,status) );
      }


      /* If required export NOI in compressed form to its own NDF. This
         is different to how NOI is exported via the "exportNDF" config
         parameter.  */
      if( noi_export && havenoi ) {

         /* If NOI is being exported but has not yet been calculated
            (e.g. if numiter=1), calculate it now. This is useful for
            SKYLOOP, which runs makemap with numiter=1. */
         if( !noidone ) smf_calcmodel_noi( wf, &dat, 0, keymap,
                                           model[whichnoi],
                                           SMF__DIMM_FIRSTITER, status );

         for( idx=0; idx<res[0]->ndat; idx++ ) {

            /* Get the NDF name for the exported NOI model. */
            smf_stripsuffix( res[0]->sdata[idx]->file->name, SMF__DIMM_SUFFIX,
                             name, status );
            one_strlcat( name, "_noi", SMF_PATH_MAX+1, status );

            /* Export it. */
            smf_export_noi( dat.noi[0]->sdata[idx], name, dat.noi_boxsize,
                            status );
         }
      }


      /* Export DIMM model components to NDF files.
         Also - check that a filename is defined in the smfFile! */

      if( exportNDF && ((*status == SAI__OK) || (*status == SMF__INSMP)) ) {
        errBegin( status );
        msgOut(" ", FUNC_NAME ": Export model components to NDF files.",
               status);

        /* Loop over smfArray elements and export. Note that QUA and NOI
           get stuffed into the QUALITY and VARIANCE components of the
           residual. Also notice that everything must be changed to
           time-ordered data before writing ICD-compliant files. */

        /* Loop over subarray, re-order, set bad values wherever a
           SMF__Q_BADB flag is encountered (if requested), and
           export */
        for( idx=0; idx<res[0]->ndat; idx++ ) {
          smf_dataOrder( wf, qua[0]->sdata[idx], 1, status );
          smf_dataOrder( wf, res[0]->sdata[idx], 1, status );
          smf_dataOrder( wf, lut[0]->sdata[idx], 1, status );

          /* Get quality array strides for smf_update_valbad */
          smf_get_dims( qua[0]->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                        NULL, &bstride, &tstride, status );

          for( j=0; j<nmodels; j++ ) {

            /* Check for existence of the model for this subarray - in
               some cases, like COM, there is only a file for one subarray,
               unlike RES from which the range of idx is derived */
            if( (modeltyps[j] != SMF__AST) && model[j][0]->sdata[idx] ) {
              smf_dataOrder( wf, model[j][0]->sdata[idx], 1, status );
              if( *status == SMF__WDIM ) {
                /* fails if not 3-dimensional data. Just annul and write out
                   data as-is. */
                errAnnul(status);
                model[j][0]->sdata[idx]->isTordered=1;
              }
            }
          }

          /* Pointer to the header in the concatenated data */
          if( *status == SAI__OK ) refdata = res[0]->sdata[idx];

          /* QUA becomes the quality component of RES. NOI becomes
             the variance component of RES if present. */
          if( *status == SAI__OK ) {
            qua_data = (qua[0]->sdata[idx]->pntr)[0];

            if( exportNDF_which[nmodels] ) {
              if( (res[0]->sdata[idx]->file->name)[0] ) {

                smf_model_createHdr( res[0]->sdata[idx], SMF__RES, refdata,
                                     status );
                smf_stripsuffix( res[0]->sdata[idx]->file->name,
                                 SMF__DIMM_SUFFIX, name, status );
                one_strlcat( name, "_res", SMF_PATH_MAX+1, status );

                if( !noexportsetbad ) {
                  smf_update_valbad( res[0]->sdata[idx], SMF__NUL,
                                     NULL, 0, 0, SMF__Q_BADB, status );
                }

                smf_write_smfData( wf, res[0]->sdata[idx],
                                   (havenoi && exportNDF_which[whichnoi]) ?
                                   dat.noi[0]->sdata[idx] : NULL,
                                   name, NULL, 0, NDF__NOID,
                                   MSG__VERB, 0, status );
              } else {
                msgOut( " ",
                        "SMF__ITERATEMAP: Can't export RES -- NULL filename",
                        status);
              }
            }


          /* LUT is stored in a separate NDF. */
            lut_data = (lut[0]->sdata[idx]->pntr)[0];

            if( exportNDF_which[nmodels+2] ) {
              if( (res[0]->sdata[idx]->file->name)[0] ) {

                smf_model_createHdr( lut[0]->sdata[idx], SMF__RES, refdata,
                                     status );
                smf_stripsuffix( res[0]->sdata[idx]->file->name,
                                 SMF__DIMM_SUFFIX, name, status );
                one_strlcat( name, "_lut", SMF_PATH_MAX+1, status );

                smf_write_smfData( wf, lut[0]->sdata[idx], NULL, name, NULL, 0,
                                   NDF__NOID, MSG__VERB, 0, status );
              } else {
                msgOut( " ",
                        "SMF__ITERATEMAP: Can't export LUT -- NULL filename",
                        status);
              }
            }

            if( exportNDF_which[whichast] ) {
              /* Create a smfData to hold the map projected into a data
                 cube and then write it out. */

              if( (res[0]->sdata[idx]->file->name)[0] ) {
                smfData *ast=NULL;
                double *ast_data=NULL;
                const char *astname=NULL;
                const char *resname=NULL;
                char workstr[GRP__SZNAM+1];

                /* Since AST only exists as a time-series model if exporting,
                   we work out the AST container filename based on RES here */

                astname = smf_model_getname( SMF__AST, status );
                resname = smf_model_getname( SMF__RES, status );

                smf_stripsuffix( res[0]->sdata[idx]->file->name,
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
                                             qua[0]->sdata[idx], 0, 1,
                                             res[0]->sdata[idx]->dims,
                                             res[0]->sdata[idx]->lbnd,
                                             res[0]->sdata[idx]->ndims,
                                             0, 0, NULL, NULL, status );

                smf_get_dims( ast, NULL, NULL, NULL, NULL, &dsize,
                              NULL, NULL, status );

                ast->pntr[0] = astCalloc( dsize, smf_dtype_size(ast,status) );

                if( *status == SAI__OK ) {
                  ast_data = ast->pntr[0];
                  lut_data = (lut[0]->sdata[idx]->pntr)[0];

                  for( j=0; j<dsize; j++ ) {
                    if( lut_data[j] != VAL__BADI ) {
                      ast_data[j] = thismap[lut_data[j]];
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
                smf_write_smfData( wf, ast, NULL, name, NULL, 0, NDF__NOID,
                                   MSG__VERB, 0, status );

                /* Clean up */
                smf_close_file( wf, &ast, status );
              } else {
                msgOut( " ",
                        "SMF__ITERATEMAP: Can't export AST -- NULL filename",
                        status);
              }

            }
          }

          /* Dynamic components excluding NOI/AST */
          for( j=0; j<nmodels; j++ ) {

            /* Remember to check again whether model[j][0]->sdata[idx] exists
               for cases like COM */
            if( (*status == SAI__OK) && (modeltyps[j] != SMF__NOI) &&
                (modeltyps[j] != SMF__AST) && model[j][0]->sdata[idx] &&
                exportNDF_which[j] ) {
              if( (model[j][0]->sdata[idx]->file->name)[0] ) {

                smf_model_createHdr( model[j][0]->sdata[idx], modeltyps[j],
                                     refdata,status );
                smf_stripsuffix( model[j][0]->sdata[idx]->file->name,
                                 SMF__DIMM_SUFFIX, name, status );

                if( !noexportsetbad ) {
                  smf_update_valbad( model[j][0]->sdata[idx], modeltyps[j],
                                     qua_data, bstride, tstride, SMF__Q_BADB,
                                     status );
                }

                /* decide if we're writing quality: has to be requested,
                   and either need to have 3d data array
                   (check array dimensions), or we can supply a collapsed
                   quality in the special case of COM */

                if( modeltyps[j] == SMF__COM && qua_data ) {
                  smf_qual_t *tempqual = NULL;
                  smfData * com = model[j][0]->sdata[idx];

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

                /* Decide if we can dump the EXT model as 1D or if it
                   needs to be 3D. */
                int single = 0;
                if( modeltyps[j] == SMF__EXT ) {
                  astMapGet0A( keymap, "EXT", &kmap );
                  if( astMapHasKey( kmap, "ALLQUICK" ) ) {
                     astMapGet0I( kmap, "ALLQUICK", &single );
                  }
                  kmap = astAnnul( kmap );
                }

                smf_write_smfData( wf, model[j][0]->sdata[idx], NULL,
                                   name, NULL, 0, NDF__NOID,
                                   MSG__VERB, single, status );

                /* if we had temporary quality free it */
                if ( modeltyps[j] == SMF__COM && qua_data ) {
                  smf_close_file( wf, &(model[j][0]->sdata[idx]->sidequal),
                                  status );
                }

              } else {
                msgSetc("MOD",smf_model_getname(modeltyps[j], status) );
                msgOut( " ",
                        "SMF__ITERATEMAP: Can't export ^MOD: NULL filename",
                        status);
              }
            }
          }
        }


        /*** TIMER ***/
        msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME
                   ": ** %f s Exporting models",
                   status, smf_timerupdate(&tv1,&tv2,status) );

        errEnd( status );
      }

/* Free the zero masks. */
      dat.ast_mask = astFree( dat.ast_mask );
      dat.com_mask = astFree( dat.com_mask );
      dat.flt_mask = astFree( dat.flt_mask );
      dat.ssn_mask = astFree( dat.ssn_mask );
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
      if( msgIflev( NULL, status ) >= MSG__VERB ) {
         errFlush( status );
      } else {
         errAnnul( status );
      }
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
    } else if( *status == SAI__OK ){

      /* In the multiple contchunk case, add this map to the total if
         we got here with clean status. First get the weight for this
         chunk. */
      chunkweight = smf_chunkweight( res[0]->sdata[0], keymap,
                                     contchunk, status );

      /* on first chunk, copy thismap onto map
         subsquent chunks get added below */
      if( contchunk == 0 ) {
        memcpy( map, thismap, msize*sizeof(*map) );
        memcpy( weights, thisweight, msize*sizeof(*weights) );
        memcpy( mapweightsq, thisweightsq, msize*sizeof(*mapweightsq) );
        memcpy( mapvar, thisvar, msize*sizeof(*mapvar) );
        memcpy( hitsmap, thishits, msize*sizeof(*hitsmap) );
        memcpy( mapqual, thisqual, msize*sizeof(*mapqual) );
      }

      if( ncontchunks > 1 ) {
        msgOut( " ", FUNC_NAME ": Adding map estimated from this continuous"
                " chunk to total", status);
        smf_addmap1( contchunk, map, mapweights, weights, hitsmap, mapvar,
                     mapqual, thismap, thisweight, thishits, thisvar, thisqual,
                     msize, chunkweight, status );
      }

      /* Add this chunk of exposure time to the total. We assume the array was
         initialised to zero and will not contain bad values. */
      if( *status == SAI__OK ) {
        steptime = res[0]->sdata[0]->hdr->steptime;
        for (ipix = 0; ipix < msize; ipix++ ) {
          if ( thishits[ipix] != VAL__BADI) {
            exp_time[ipix] += chunkweight*steptime * (double)thishits[ipix];
          }
        }
      }
      /* Update the sum of all chunk weights. */
      sumchunkweights += chunkweight;
    }

    /* *************************************************************************
       Clean up temporary resources associated with this continuous chunk
       before continuing in the outer loop.
    ************************************************************************* */

    /* fixed model smfGroups */
    if( resgroup ) smf_close_smfGroup( &resgroup, status );
    if( lutgroup ) smf_close_smfGroup( &lutgroup, status );
    if( quagroup ) smf_close_smfGroup( &quagroup, status );

    /* fixed model smfArrays */
    if( res ) {
      if( res[0] ) smf_close_related( wf, &res[0], status );
      res = astFree( res );
    }

    if( lut ) {
      if( lut[0] ) smf_close_related( wf, &lut[0], status );
      lut = astFree( lut );
    }

    if( qua ) {
      if( qua[0] ) smf_close_related( wf, &qua[0], status );
      qua = astFree( qua );
    }

    /* dynamic model smfArrays */
    if( model ) {
      for( imodel = 0; imodel < nmodels; imodel++ ) {
        if( model[imodel] ) {

          /* Close each model component smfArray */
          if( model[imodel][0] ) smf_close_related( wf, &(model[imodel][0]), status );

          /* Free array of smfArray pointers for this model */
          model[imodel] = astFree( model[imodel] );
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

  /* Normalise the returned exposure times to a mean chunk weight of unity. */
  if( *status == SAI__OK ) {
    double meanw = sumchunkweights/ncontchunks;
    for (ipix = 0; ipix < msize; ipix++ ) {
       exp_time[ipix] /= meanw;
    }
  }

  /* If we are running frmo skyloop (as indicated by the fact that we are
     doing only one iteration), then we now consider whether or not to
     filter the differences between the new map and the old map in order to
     suppress the growth of smooth large scale structures. */
  if( ast_filt_diff > 0.0 && numiter == 1 && importsky ) {
    dat.map = map;
    smf_filter_mapchange( wf, &dat, ast_filt_diff, status );
  }

  /* Store a flag indicating if the MAPTOL_RATE limit was hit. */
  parPut0l( "RATE_LIMITED", rate_limited, status );





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
  mapweights = astFree( mapweights );

  modeltyps = astFree( modeltyps );
  exportNDF_which = astFree( exportNDF_which );

  if( igroup ) {
    smf_close_smfGroup( &igroup, status );
  }

  epsout = astFree( epsout );
  epsbuf1 = astFree( epsbuf1 );
  epsbuf2 = astFree( epsbuf2 );
  epsbuf3 = astFree( epsbuf3 );

  epsin = astFree( epsin );
  if( epsndf != NDF__NOID ) ndfAnnul( &epsndf, status );

  fakemap = astFree( fakemap );
  if( fakendf != NDF__NOID ) ndfAnnul( &fakendf, status );

  lastmap = astFree( lastmap );
  mapchange = astFree( mapchange );
  job_data = astFree( job_data );

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


  /* Annull all AST objects created in (or under) this function. */
  astEnd;

}


static void smf1_iteratemap( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_iteratemap

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_iteratemap.

*  Invocation:
*     smf1_iteratemap( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfIterateMapData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfIterateMapData *pdata;
   double *pr;
   int *pl;
   size_t idata;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfIterateMapData *) job_data_ptr;

/* Add last iter. of astronomical signal back in to residual. Ignore map
   pixels that have been constrained to zero. */
   if( pdata->operation == 1 ) {
      pr = pdata->res_data +  pdata->d1;
      pq = pdata->qua_data +  pdata->d1;
      pl = pdata->lut_data +  pdata->d1;
      for( idata = pdata->d1; idata <= pdata->d2; idata++,pq++,pl++,pr++ ) {
         if( !( *pq & SMF__Q_MOD ) && *pl != VAL__BADI ) {
            double ast_data = pdata->thismap[ *pl ];
            if( ast_data != VAL__BADD &&
                !(pdata->thisqual[ *pl ] & SMF__MAPQ_AST ) ) {
               *pr += ast_data;
            }
         }
      }

/* Find the median of the three last difference maps. */
   } else if( pdata->operation == 2 ) {
      double *p0, *p1, *p2, *p3;

      p0 = pdata->epsout +  pdata->d1;
      p1 = pdata->err1 +  pdata->d1;
      p2 = pdata->err2 +  pdata->d1;
      p3 = pdata->err3 +  pdata->d1;
      for( idata = pdata->d1; idata <= pdata->d2; idata++,p0++,p1++,p2++,p3++ ) {
         if( *p1 != VAL__BADD && *p2 != VAL__BADD && *p3 != VAL__BADD ) {
            if( *p1 > *p2 ) {
               if( *p2 > *p3 ) {
                  *p0 = *p2;
               } else {
                  *p0 = *p3;
               }
            } else {
               if( *p1 > *p3 ) {
                  *p0 = *p1;
               } else {
                  *p0 = *p3;
               }
            }
         } else {
            *p0 = VAL__BADD;
         }
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_iteratemap: Illegal operation %d requested.",
               status, pdata->operation );
   }

}

