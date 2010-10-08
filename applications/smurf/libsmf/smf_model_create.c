/*
*+
*  Name:
*     smf_model_create

*  Purpose:
*     Create group of containers for iterative map-maker model components

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_model_create( smfWorkForce *wf, const smfGroup *igroup,
*                       smfArray **iarray, const smfArray *darks,
*                       const smfArray *bbms, const smfArray* flatramps,
*                       const smfArray *noisemaps, dim_t nchunks,
*                       smf_modeltype mtype, int isTordered,
*                       AstFrameSet *outfset, int moving, int *lbnd_out,
*                       int *ubnd_out, smfGroup **mgroup, int nofile,
*                       int leaveopen, smfArray **mdata, AstKeyMap *keymap,
*                       int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     igroup = const smfGroup * (Given)
*        NDG group identifier for input template files
*     iarray = const smfArray ** (Given)
*        If igroup unspecified, use an array of smfArrays as the template
*        instead. In this case nchunks must also be specified.
*     darks = const smfArray * (Given)
*        Collection of darks that can be applied to non-flatfielded data.
*        Can be NULL.
*     bbms = const smfArray * (Given)
*        Masks for each subarray (e.g. returned by smf_reqest_mask call)
*     flatramps = const smfArray * (Given)
*        Collection of flatfield ramps. Passed to smf_open_and_flatfield.
*     noisemaps = const smfArray * (Given)
*        smfArray of 2d smfData's containing externally-calculated noise maps
*        which, if supplied, are ued to initialize the NOI model.
*     nchunks = dim_t (Given)
*        If iarray specified instead of igroup, nchunks gives number of
*        smfArrays in iarray (otherwise it is derived from igroup).
*     mtype = smf_modeltype (Given)
*        Type of model component to create
*     isTordered = int (Given)
*        If 0, ensure template data is ordered by bolometer. If 1 ensure
*        template data is ordered by time slice (default ICD ordering).
*        Ignored if not creating SMF__LUT.
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping if calculating
*        pointing LUT on-the-fly. Ignored if not creating SMF__LUT.
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*        (if outfset specified)
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*        (if outfset specified)
*     mgroup = smfGroup ** (Returned)
*        Pointer to smfGroup pointer that will contain model file names
*     nofile = int (Given)
*        If set don't create a file on disk - just create the smfArray. In
*        this case leaveopen is implied (and overrides user supplied value)
*     leaveopen = int (Given)
*        If true, don't close files once created and store in mdata
*     mdata = smfArray ** (Given and Returned)
*        Container to store data if leaveopen is set: array of
*        smfArray pointers. The top-level array must already be
*        allocated (same number of elements as ngroups in igroup), but
*        the individual smfArrays get allocated here.
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a group of input (template) data files, this routine
*     creates new NDF files with dimensions appropriate for the model
*     parameters.  For example, a common-model signal is represented
*     by a 1-dimensional array as a function of time. The names of the
*     containers are the same as the input template, with a suffix
*     added. The containers can be stored in smfArrays if leavelopen
*     is set. In this case it is up to the caller to first generate an
*     array of smfArray pointers (mdata) which then get new smfArrays
*     assigned to them. In this case it is up to the caller to close
*     the smfArrays. If a SMF__LUT component is being calculated, the
*     projection information must be supplied: outfset, moving and ?bnd_out.

*  Notes:
*     QUAlity components are initialized to 0. Before using the caller
*     should synchronize the SMF__Q_BADDA bits with
*     smf_update_quality.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-07-06 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Propagate inputs to residual, create others with smf_open_newfile
*     2007-02-07 (EC):
*        - Simplified container files.
*        - In copyinput case map data array so that it gets copied.
*     2007-02-12 (EC):
*        Now form the grpex here for the model container names
*     2007-03-02 (EC):
*        - Map variance to ensure creation for RESidual container
*        - Set initial variance to 1
*     2007-06-13 (EC):
*        - Use new DIMM binary file format
*     2007-06-25 (EC)
*        Header length is now static / padded to multiple of pagesize
*     2007-07-10 (EC):
*        Use smfGroups & smfArrays instead of groups and smfDatas
*     2007-07-13 (EC):
*        Only create one smfData per subgroup for SMF__COM models
*     2007-07-16 (EC):
*        -Changed smf_construct_smfGroup interface
*     2007-08-09 (EC):
*        -use mmap rather than fwrite for creation
*        -option to leave files open, store in smfArrays
*     2007-08-17 (EC):
*        Added nofile flag
*     2007-08-21 (EC):
*        Fixed up warnings caused by ambiguous pointer math
*     2007-11-15 (EC):
*        -Added ability to create models from a smfArray template,
*         requiring a change to the interface.
*        -Fixed memory allocation bug
*     2007-11-28 (EC):
*        -Added ability to assert the dataOrder (isTordered parameter)
*     2007-12-14 (EC):
*        -template is now loaded into refdata and copies to idata
*        -properly set isTordered in created smfData
*        -don't unmap the header portion of the model in DIMM files
*     2008-01-24 (EC):
*        -Template can now be non-flatfielded.
*        -Better file names for models based on iarray
*     2008-03-03 (EC):
*        -handle QUAlity
*     2008-04-16 (EC):
*        -added chunk to smfGroup
*     2008-04-17 (EC):
*        -improved initialization for SMF__NOI and SMF__QUA
*     2008-04-30 (EC):
*        -Calculate and store extinction coefficients for SMF__EXT
*     2008-06-24 (TIMJ):
*        - const mname since smf_model_getname now returns const
*     2008-07-03 (EC):
*        Changed nchunks to dim_t
*     2008-07-11 (TIMJ):
*        Use one_strlcpy/strlcat
*     2008-08-20 (EC):
*        Create smfHead and propagate steptime
*     2009-03-12 (EC):
*        Add SMF__FLT
*     2009-04-20 (EC):
*        Add flagstat to interface
*     2009-07-31 (EC):
*        Switch to 2d variance array (one value for each bolometer)
*     2009-09-29 (EC):
*        Allow multiple extinction correction methods
*     2010-02-12 (TIMJ):
*        Do not scale the CSO tau to filter and then ask the extinction
*        function to scale again.
*     2010-02-16 (TIMJ):
*        Add support for AUTO extinction tau source. Remove call to
*        smf_cso2filt_tau since smf_correct_extinction will do that
*        anyhow.
*     2010-03-11 (TIMJ):
*        Add darks and mask arguments to sync with smf_concat_smfGroup
*        Add flatramps argument.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-05-12 (EC):
*        Convert COM to 3d from 1d (but spatial axes have length 1)
*     2010-05-13 (TIMJ):
*        Add spatial plane fitting model
*     2010-05-28 (EC):
*        Replacing dead dark squids with average is optional (dks.replacebad)
*     2010-05-28 (TIMJ):
*        Add SMO smoothing model
*     2010-06-08 (EC):
*        Add TWO component model
*     2010-06-10 (EC):
*        Add dark squid cleaning
*     2010-06-28 (AGG):
*        Update POSIX version to 200809L to build on Fedora 13
*     2010-09-20 (EC):
*        Optionally initialize NOI using externally-supplied array
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
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
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Need ftruncate to be prototyped */
#define _POSIX_C_SOURCE 200809L

/* General includes */
#include <sys/mman.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_model_create"

void smf_model_create( smfWorkForce *wf, const smfGroup *igroup,
                       smfArray **iarray, const smfArray *darks,
                       const smfArray *bbms, const smfArray *flatramps,
                       const smfArray *noisemaps,dim_t nchunks,
                       smf_modeltype mtype, int isTordered,
                       AstFrameSet *outfset, int moving,
                       int *lbnd_out, int *ubnd_out, smfGroup **mgroup,
                       int nofile, int leaveopen, smfArray **mdata,
                       AstKeyMap *keymap, int *status ) {

  /* Local Variables */
  size_t bstride;               /* Bolometer stride in data array */
  void *buf=NULL;               /* Pointer to total container buffer */
  size_t buflen = 0;            /* datalen + headlen */
  int copyinput=0;              /* If set, container is copy of input */
  smfData *data = NULL;         /* Data struct for file */
  size_t datalen=0;             /* Size of data buffer in bytes */
  void *dataptr=NULL;           /* Pointer to data portion of buffer */
  smf_extmeth extmeth;          /* method of extinction correction */
  int fd=0;                     /* File descriptor */
  int flag=0;                   /* Flag */
  char fname_grpex[GRP__SZNAM+1];/* String for holding filename grpex */
  dim_t gain_box=0;             /* No. of time slices in a block */
  smfDIMMHead head;             /* Header for the file */
  size_t headlen=0;             /* Size of header in bytes */
  void *headptr=NULL;           /* Pointer to header portion of buffer */
  dim_t i;                      /* Loop counter */
  dim_t ibase;                  /* Base offset */
  smfData *idata=NULL;          /* Pointer to input smfdata data */
  int idx=0;                    /* Index within subgroup */
  size_t isize=0;               /* Number of files in input group */
  int ival;                     /* Integer value */
  dim_t j;                      /* Loop counter */
  size_t k;                     /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  dim_t l;                      /* Loop counter */
  size_t len = 0;               /* size of buffer */
  Grp *mgrp=NULL;               /* Temporary group to hold model names */
  const char *mname=NULL;       /* String model component name */
  size_t msize=0;               /* Number of files in model group */
  char name[GRP__SZNAM+1];      /* Name of container file without suffix */
  dim_t nblock=0;               /* Number of time blocks */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata=0;                /* Number of elements in data array */
  size_t nflag;                 /* Number of flagged samples */
  dim_t nrel=0;                 /* Number of related elements (subarrays) */
  dim_t ntslice=0;              /* Number of time slices */
  int oflag=0;                  /* Flags for opening template file */
  char *pname=NULL;             /* Poiner to fname */
  char suffix[] = SMF__DIMM_SUFFIX; /* String containing model suffix */
  double tau;                   /* tau */
  smf_tausrc tausrc;            /* Type of tau monitor */
  dim_t thisnrel;               /* Number of related items for this model */
  size_t tstride;               /* Time slice stride in data array */
  struct timeval tv1;           /* Timer */
  struct timeval tv2;           /* Timer */
  int zeropad;                  /* Pad with zeros instead of artificial data? */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Start a timer to see how long this takes */
  smf_timerinit( &tv1, &tv2, status );

  /* Check to see if igroup or iarray is being used for template */
  if( igroup == NULL ) {
    if( iarray == NULL ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Neither igroup nor iarray specified",
             status);
    } else if( nchunks <= 0 ) {
      *status = SAI__ERROR;
      msgSeti("NCHUNKS",nchunks);
      errRep(FUNC_NAME,
             "iarray specified but invalid number of chunks, ^NCHUNKS",
             status);
    } else {
      /* Since we're using smfArrays as template, no associated files */
      nofile = 1;

      /* We have at most SMF__MXSMF related objects (subarrays) at each time
         chunk */
      nrel = SMF__MXSMF;

      /* NULL mgroup since we won't be using it */
      mgroup = NULL;
    }
  } else {
    /* Get number of time chunks and related objects from igroup */
    nchunks = igroup->ngroups;
    nrel = igroup->nrelated;
  }

  /* If nofile is set, leaveopen=0 is meaningless */
  if( nofile ) leaveopen = 1;

  /* If using igroup as a template use group expressions to make filenames */
  if( igroup != NULL ) {
    /* Get size of the input group */
    isize = grpGrpsz( igroup->grp, status );

    /* Create group of NDF names with model name suffix */
    mgrp = grpNew( "model component", status );
    mname = smf_model_getname( mtype, status );

    /* Form a group expression for the filename */
    len = sizeof(fname_grpex);
    one_strlcpy( fname_grpex, "./*_", len, status );
    one_strlcat( fname_grpex, mname, len, status );
    one_strlcat( fname_grpex, suffix, len, status );

    ndgCrexp( fname_grpex, igroup->grp, &mgrp, &msize, &flag, status );

    if( (*status == SAI__OK) && (msize != isize) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Couldn't create group of NDF model containers.",
             status);
    }

    /* Now that we have the Grp of names, create a new smfGroup with the same
       grouping as igroup. mgroup has a copy of mgrp inside, so free up
       mgrp afterward. */

    *mgroup = smf_construct_smfGroup( mgrp, igroup->subgroups, igroup->chunk,
                                      igroup->ngroups, igroup->nrelated, 1,
                                      status );

    if( mgrp ) grpDelet( &mgrp, status );
  }

  /* Check the mtype to decide how we should open the template files, and
     decide if we will propagate the template to the model file */

  oflag = 0;

  /* Only map head if creating LUT, EXT, QUA or DKS */
  if( (mtype != SMF__LUT) && (mtype != SMF__EXT) && (mtype != SMF__QUA) &&
      (mtype != SMF__DKS) ) {
    oflag |= SMF__NOCREATE_HEAD;
  }

  if( mtype == SMF__RES ) {
    /* Propagate input if RES */
    copyinput = 1;
  } else if( (mtype != SMF__DKS) && (mtype != SMF__NOI) ) {
    /* For types other than DKS/NOI don't need data array */
    oflag |= SMF__NOCREATE_DATA;
  }

  /* Loop over time chunks */
  if( *status == SAI__OK ) for( i=0; i<nchunks; i++ ) {

      /* For models that only have one file per subgroup fix up
         mgroup such that only the first filename in each subgroup
         is used. Do this by setting remaining elements of mgroup->subgroups
         to 0. nrel is 1. */

      if( mtype == SMF__COM ) {
        if (mgroup != NULL) {
          for( j=1; j<(*mgroup)->nrelated; j++ ) {
            (*mgroup)->subgroups[i][j] = 0;
          }
        }

        nrel = 1;
      }

      /* Check to see how many related elements in this chunk */

      thisnrel = 0;

      for( j=0; j<nrel; j++ ) {
        /* Check mgroup if we're using igroup as a template */
        if( mgroup != NULL ) {

          /* Check for non-zero grp index at this position */
          if( (*mgroup)->subgroups[i][j] != 0 ) {
            thisnrel = j+1;
          } else {
            /* If grp index is 0 we've reached the end of the subarrays. Set
               the exit condition */
            j=nrel;
          }
        }
      }

      if( mgroup == NULL ) {
        /* Otherwise just check the iarray */
        thisnrel = iarray[i]->ndat;

        /* Ensure that thisnrel isn't bigger than nrel */
        if( thisnrel > nrel ) {
          thisnrel = nrel;
        }
      }

      /* Loop over subarrays */
      for( j=0; j<thisnrel; j++ ) {

        /* Open the relevant template file if using igroup */
        if( igroup ) {

          /* obtain grp idx for j'th element of i'th subgroup */
          idx=(*mgroup)->subgroups[i][j];

          /* Only continue if there is a valid idx */
          if( idx > 0 ) {

            /* Open the template file - flags are set above depending
               on the type of model. If we're reading data, then
               do an open_and_flatfield */

            if( !(oflag&SMF__NOCREATE_DATA) ) {
              smf_open_and_flatfield( igroup->grp, NULL, idx, darks, flatramps,
                                      &idata, status );
              smf_apply_mask( idata, bbms, SMF__BBM_DATA, 0, status );

            } else {
              smf_open_file( igroup->grp, idx, "READ", oflag, &idata, status );
            }

            /* Calculate the LUT if necessary */

            if( mtype == SMF__LUT ) {
              int tstep = 0;
              astMapGet0I( keymap, "TSTEP", &tstep );
              smf_calc_mapcoord( wf, idata, outfset, moving, lbnd_out,
                                 ubnd_out, SMF__NOCREATE_FILE, tstep,
                                 status );
            }

          }

        } else {
          /* Otherwise obtain a pointer to the relevant smfData in the
             template smfArray at this time chunk */
          idata = iarray[i]->sdata[j];
        }

        /* Assert the data order in the template */
        smf_dataOrder( idata, isTordered, status );

        /* Check that the template is time-varying data */
        if( *status == SAI__OK ) {
          if( idata->ndims != 3 ) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Template data is not time-varying!",
                   status);
          }
        }

        if( *status == SAI__OK ) {

          /* initialize the header */
          memset( &head, 0, sizeof(head) );
          head.data.dtype=SMF__NULL;

          /* All models use TSERIES quality */
          head.data.qfamily = SMF__QFAM_TSERIES;

          /* Determine dimensions of model component */
          switch( mtype ) {

          case SMF__CUM: /* Cumulative model */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            if (idata && idata->hdr) {
              smf_set_clabels( "Cumulative Signal", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__RES: /* Model residual */
            /* Not much here since copyinput set */
            if (idata && idata->hdr) {
              smf_set_clabels( "Residual Signal", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__AST: /* Time-domain projection of map */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            if (idata && idata->hdr) {
              smf_set_clabels( "Astronomical Signal", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__COM: /* Common-mode at each time step */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;

            if (idata && idata->hdr) {
              smf_set_clabels( "Common-mode Signal", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }

            if( isTordered ) { /* T is 3rd axis if time-ordered */
              head.data.lbnd[0] = 0;
              head.data.lbnd[1] = 0;
              head.data.lbnd[2] = 1;

              head.data.dims[0] = 1;
              head.data.dims[1] = 1;
              head.data.dims[2] = (idata->dims)[2];
            } else {           /* T is 1st axis if bolo-ordered */
              head.data.lbnd[0] = 1;
              head.data.lbnd[1] = 0;
              head.data.lbnd[2] = 0;

              head.data.dims[0] = (idata->dims)[0];
              head.data.dims[1] = 1;
              head.data.dims[2] = 1;
            }
            break;

          case SMF__NOI: /* Noise model */
            /* Currently just one variance for each bolometer */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            if (idata && idata->hdr) {
              smf_set_clabels( "Noise Variance", "Variance",
                               idata->hdr->units, &head.hdr, status );
            }
            one_strlcat(head.hdr.units, "**2", sizeof(head.hdr.units), status);

            if( isTordered )  { /* T is 3rd axis if time-ordered */
              head.data.dims[0] = (idata->dims)[0];
              head.data.dims[1] = (idata->dims)[1];
              head.data.dims[2] = 1;

              head.data.lbnd[0] = (idata->lbnd)[0];
              head.data.lbnd[1] = (idata->lbnd)[1];
              head.data.lbnd[2] = 1;
            } else {           /* T is 1st axis if bolo-ordered */
              head.data.dims[0] = 1;
              head.data.dims[1] = (idata->dims)[1];
              head.data.dims[2] = (idata->dims)[2];

              head.data.lbnd[0] = 1;
              head.data.lbnd[1] = (idata->lbnd)[1];
              head.data.lbnd[2] = (idata->lbnd)[2];
            }
            break;

          case SMF__EXT: /* Extinction correction - gain for each bolo/time */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            smf_set_clabels( "Extinction Correction", "1/Extinction",
                             "", &head.hdr, status );
            break;

          case SMF__LUT: /* Pointing LookUp Table for each data point */
            head.data.dtype = SMF__INTEGER;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }
            break;

          case SMF__QUA: /* Quality byte for each data point */
            head.data.dtype = SMF__QUALTYPE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }
            break;

          case SMF__DKS: /* Scaled Dark SQUID */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 2;
            /* Store column dark squid followed by gain+off+corr each row */
            if( isTordered ) { /* T is 3rd axis if time-ordered */
              head.data.dims[0] = (idata->dims)[2] +
                (idata->dims)[SC2STORE__ROW_INDEX]*3;
              head.data.dims[1] = (idata->dims)[SC2STORE__COL_INDEX];
            } else {           /* T is 1st axis if bolo-ordered */
              head.data.dims[0] = (idata->dims)[0] +
                (idata->dims)[1+SC2STORE__ROW_INDEX]*3;
              head.data.dims[1] = (idata->dims)[1+SC2STORE__COL_INDEX];
            }
            smf_set_clabels( "Dark Squid Model", "Value", "",
                             &head.hdr, status );
            break;

          case SMF__GAI: /* Gain/offset for each bolometer */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3; /* Gain, Offset, Correlation coefficient */

            smf_set_clabels( "Common-mode Gain/Offset/Correlation", "Value",
                             "", &head.hdr, status );

            /* Get the number of blocks into which to split each
               time series. Each box (except possibly the last one)
               contains "gain_box" time slices. */

            astMapGet0A( keymap, "COM", &kmap );
            astMapGet0I( kmap, "GAIN_BOX", &ival );
            gain_box = ival;
            kmap = astAnnul( kmap );

            if( isTordered ) {
              ntslice = (idata->dims)[2];
            } else {
              ntslice = (idata->dims)[0];
            }
            nblock = ntslice/gain_box;
            if( nblock == 0 ) nblock = 1;

            /* Note that we're using the time axis to store the coefficients */
            if( isTordered ) {
              head.data.dims[0] = (idata->dims)[0];
              head.data.dims[1] = (idata->dims)[1];
              head.data.dims[2] = 3*nblock;

              head.data.lbnd[0] = (idata->lbnd)[0];
              head.data.lbnd[1] = (idata->lbnd)[1];
              head.data.lbnd[2] = 1;
            } else {
              head.data.dims[0] = 3*nblock;
              head.data.dims[1] = (idata->dims)[1];
              head.data.dims[2] = (idata->dims)[2];

              head.data.lbnd[0] = 1;
              head.data.lbnd[1] = (idata->lbnd)[1];
              head.data.lbnd[2] = (idata->lbnd)[2];
            }
            break;

          case SMF__FLT: /* Frequency domain filter */
            /* We will use a frequency domain filter to remove noise, but
               store what we removed with a time-domain representation for
               easy visualization. */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            if (idata && idata->hdr) {
              smf_set_clabels( "Filtered-out Signal", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__PLN: /* Spatial plane fitting */
            /* Fit a plane to each time slice. Have a choice of
               storing the fit itself in the model or else storing
               the coefficients of the fit. Start with the easy
               to visualise but memory-hungry case. */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            if (idata && idata->hdr) {
              smf_set_clabels( "Fitted plane", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__SMO: /* Time series smoothing */
            /* Smooth the time series and subtract it */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 3;
            for( k=0; k<3; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }

            if (idata && idata->hdr) {
              smf_set_clabels( "Smoothed background", "Signal",
                               idata->hdr->units, &head.hdr, status );
            }
            break;

          case SMF__TWO: /* TWO-component common-mode */
            head.data.dtype = SMF__DOUBLE;
            head.data.ndims = 2;
            /* Each component is stored as a single vector: the time
               series for the component followed by the nbolo
               coefficients */
            if( isTordered ) { /* T is 3rd axis if time-ordered */
              head.data.dims[0] = (idata->dims)[2] +
                (idata->dims)[0]*(idata->dims)[1];
            } else {           /* T is 1st axis if bolo-ordered */
              head.data.dims[0] = (idata->dims)[0] +
                (idata->dims)[1]*(idata->dims)[2];
            }

            /* The second axis is length 2, for the two components */
            head.data.dims[1] = 2;

            smf_set_clabels( "Two Component Common-mode", "Value", "",
                             &head.hdr, status );
            break;


          default:
            *status = SAI__ERROR;
            msgSetc( "TYPE", smf_model_getname(mtype, status) );
            errRep(FUNC_NAME, "Don't know how to handle model type ^TYPE",
                   status);
          }

          /* Propagate information from template if copying */
          if( copyinput ) { /* If copying input, copy data dims directly */
            head.data.dtype = idata->dtype; /* Inherit type from template */
            head.data.ndims = idata->ndims;

            for( k=0; k<head.data.ndims; k++ ) {
              head.data.dims[k] = (idata->dims)[k];
              head.data.lbnd[k] = (idata->lbnd)[k];
            }
          }

          /* Set the data-ordering flag in the header */
          head.data.isTordered = idata->isTordered;

          /* Other info from the header */
          if( idata->hdr ) {
            head.hdr.steptime = idata->hdr->steptime;
          }

          /* Calculate the size of the data buffer. Format:

             Header:
             smfDIMMHead struct

             Data:
             buf   = [smf_dtype] * dims[0] * dims[1] * ...
          */

          /* Get the size of the header and data section */
          smf_calc_mmapsize( sizeof(head), &(head.data), &headlen, &datalen,
                             &buflen, status );

          if( mgroup != NULL ) {
            /* Obtain a character string corresponding to the file name
               if we used a group as the template */
            pname = name;
            grpGet( (*mgroup)->grp, idx, 1, &pname, GRP__SZNAM, status );
          } else {
            /* Otherwise get the name from the smfArray */
            mname = smf_model_getname( mtype, status );

            len = sizeof(name);
            one_strlcpy( name, idata->file->name, len, status );
            one_strlcat( name, "_", len, status );
            one_strlcat( name, mname, len, status );
          }

          if( nofile ) {
            /* If there is no file associated with the data, use malloc
               to allocate memory but don't initialize since we do that
               later */

            dataptr = astCalloc( datalen, 1, 0 );

          } else {
            /* If we are writing a file create and map it here */

            if( (fd = open( name, O_RDWR | O_CREAT | O_TRUNC,
                            S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH )) == -1 ) {
              *status = SAI__ERROR;
              errRep( FUNC_NAME, "Unable to open model container file",
                      status );
            }

            /* First truncate the file to make it the correct size, and then
               map it (without the ftruncate bus errors are generated under
               linux when the memory is subsequently accessed...) */

            if( *status == SAI__OK ) {
              if( ftruncate( fd, buflen ) == -1 ) {
                *status = SAI__ERROR;
                errRep( FUNC_NAME, "Unable to re-size container file",
                        status );
              } else if( (buf = mmap( 0, buflen,
                                      PROT_READ | PROT_WRITE,
                                      MAP_SHARED, fd, 0 ) ) == MAP_FAILED ) {
                *status = SAI__ERROR;
                errRep( FUNC_NAME, "Unable to map model container file",
                        status );
              }
            }

            if( *status == SAI__OK ) {
              headptr = buf;
              dataptr = (char *)buf + headlen;

              /* Fill the header. memset to 0 first since much of this space is
                 padding to make it a multiple of the page size */
              memset( headptr, 0, headlen );
              memcpy( headptr, &head, sizeof(head) );
            }
          }

          /* Initialize the data buffer */
          if( *status == SAI__OK ) {
            if( copyinput ) {
              /* memcpy because target and source are same type */
              memcpy( dataptr, (idata->pntr)[0], datalen );

            } else if( mtype == SMF__LUT ) {
              /* If this is a LUT copy it over from the template */
              if( idata->lut ) {
                /* dataptr can be mmap'd or malloc'd memory */
                memcpy( dataptr, idata->lut, datalen );
              } else {
                *status = SAI__ERROR;
                errRep(FUNC_NAME, "No LUT present in template for LUT model",
                       status);
              }

            } else if( mtype == SMF__NOI ) {
              int calcfirst;

              astMapGet0A( keymap, "NOI", &kmap );
              astMapGet0I( kmap, "CALCFIRST", &calcfirst );
              astMapGet0I( kmap, "ZEROPAD", &zeropad );
              kmap = astAnnul( kmap );

              smf_get_dims( &(head.data), NULL, NULL, NULL, NULL, &ndata,
                            NULL, NULL, status);

              if( calcfirst ) {
                /* If calcfirst flag is set, initialize NOI using noise
                   measured in the bolometer now (i.e. before the first
                   iteration). Use externally-supplied noise values
                   if provided. */

                if( noisemaps ) {
                  memcpy( dataptr, noisemaps->sdata[j]->pntr[0],
                          ndata*smf_dtype_size(noisemaps->sdata[j], status) );
                } else {
                  if( idata && idata->pntr[0] ) {

                    /* Need to gap fill first to ensure data padding is ok */
                    smf_fillgaps( wf, idata,
                                  zeropad ? SMF__Q_GAP : SMF__Q_GAP | SMF__Q_PAD,
                                  status );

                    smf_bolonoise( wf, idata, 0, 0.5, SMF__F_WHITELO,
                                   SMF__F_WHITEHI, 0, 0, zeropad ? SMF__MAXAPLEN : SMF__BADSZT,
                                   dataptr, NULL, NULL, status );
                  } else {
                    *status = SAI__ERROR;
                    errRep(FUNC_NAME,
                           "Possible programming error. No bolo data.",
                           status);
                  }
                }

              } else {
                /* Otherwise initialize values to 1, avoid divide-by-zero */
                if( head.data.dtype == SMF__DOUBLE ) {
                  for( l=0; l<ndata; l++ ) {
                    ((double *) dataptr)[l] = 1;
                  }
                } else {
                  /* Generate error message if NOI is not double... */
                  *status = SAI__ERROR;
                  errRep(FUNC_NAME,
                         "Possible programming error. NOI should be DOUBLE.",
                         status);
                }
              }

            } else if( mtype == SMF__QUA ) {
              /* If this is a QUA, and quality available in template copy it */
              if( idata->qual ) {
                memcpy( dataptr, idata->qual, datalen );
              }

            } else if( mtype == SMF__EXT ) {
              /* In this case run smf_correct_extinction on the input data
                 (with only the header mapped) and store the correction
                 factors in the model bufffer */

              tau = VAL__BADD;
              astMapGet0A( keymap, "EXT", &kmap );

              /* Use sub-keymap containing EXT parameters */
              smf_get_extpar( kmap, &tausrc, &extmeth, status );
              if( tausrc == SMF__TAUSRC_CSOTAU ||
                  tausrc == SMF__TAUSRC_AUTO ) {
                /* In AUTO mode we can let people specify a fallback CSO tau.
                   In CSO mode a hard-coded value will be used for all files. */
                if( astMapGet0D( kmap, "CSOTAU", &tau ) ) {
                  msgOutf( "", "*** EXTINCTION WARNING: single opacity value "
                           "of CSO %g %s be used for ALL input files.", status,
                           tau, ( tausrc == SMF__TAUSRC_CSOTAU ? "will" : "might" ) );
                }
              } else if( tausrc == SMF__TAUSRC_TAU ) {
                if( astMapGet0D( kmap, "FILTERTAU", &tau ) ) {
                  msgOutf( "", "*** EXTINCTION WARNING: single opacity value "
                           "of %g will be used for ALL input files.", status, tau );
                }
              }

              /* Trap case where FILTERTAU requsted but no value given */
              if( (tausrc==SMF__TAUSRC_TAU) && (tau==VAL__BADD) ) {
                  *status = SAI__ERROR;
                  errRep( "", FUNC_NAME
                         ": FILTERTAU requested but no value provided",
                         status );
              }

              smf_correct_extinction( idata, tausrc, extmeth, kmap, tau,
                                      (double *) dataptr, status );

              kmap = astAnnul( kmap );


            } else if( mtype == SMF__DKS ) {
              int replacebad;

              /* First set the entire buffer to 0 */
              memset( dataptr, 0, datalen );

              /* Apply basic DKS cleaning parameters */
              if( idata->da && idata->da->dksquid ) {
                smfArray *array=NULL;
                smfData *dksquid = idata->da->dksquid;

                msgOutif( MSG__VERB, "", FUNC_NAME ": cleaning dark squids",
                          status);

                /* fudge the header so that we can get at JCMTState */
                dksquid->hdr = idata->hdr;

                /* clean darks using cleandk.* parameters */
                astMapGet0A( keymap, "CLEANDK", &kmap );
                array = smf_create_smfArray( status );
                smf_addto_smfArray( array, dksquid, status );
                smf_clean_smfArray( wf, array, NULL, kmap, status );
                if( array ) {
                  array->owndata = 0;
                  smf_close_related( &array, status );
                }
                if( kmap ) kmap = astAnnul( kmap );

                /* Unset hdr pointer so that we don't accidentally close it */
                dksquid->hdr = NULL;

              } else {
                *status = SAI__ERROR;
                errRep( "", FUNC_NAME
                        ": Reference smfData does not contain dark squids",
                        status );
              }

              /* Check for additional special DKS initialzation parameters */
              astMapGet0A( keymap, "DKS", &kmap );
              astMapGet0I( kmap, "REPLACEBAD", &replacebad );
              kmap = astAnnul( kmap );

              /* Initialize the model to hold an un-smoothed copy of
                 the dark squids (get smoothed later in smf_calcmodel_dks).
                 Since we need to pass a smfData to clean_dksquid, kludge
                 head.data so that its pntr[0] temporarily points to the
                 model data array. */
              head.data.pntr[0] = dataptr;
              smf_clean_dksquid(idata, 0, 0, &(head.data), 1, 1,
                                replacebad, status);
              head.data.pntr[0] = NULL;
            } else if( mtype == SMF__GAI ) {
              /* Initialize gain to 1, offset to 0, correlation to 0 */
              smf_get_dims( &(head.data), NULL, NULL, &nbolo, NULL, NULL,
                             &bstride, &tstride, status);

              for( k=0; k<nblock; k++ ) {
                ibase = k*tstride;
                for( l=0; l<nbolo; l++ ) {
                  ((double *)dataptr)[ibase] = 1.0;
                  ((double *)dataptr)[nblock*tstride + ibase] = 0.0;
                  ((double *)dataptr)[2*nblock*tstride + ibase] = 0.0;
                  ibase += bstride;
                }
              }

            } else if( mtype == SMF__TWO ) {
              smfData *gdata=NULL;
              Grp *ggrp=NULL;
              double *gptr1=NULL;
              double *gptr2=NULL;
              const char *amapname = NULL;
              const char *bmapname = NULL;

              /* Load in gains from external files, setting un-defined values
                 to VAL__BADD */

              astMapGet0A( keymap, "TWO", &kmap );
              astMapGet0C( kmap, "AMAP", &amapname );
              astMapGet0C( kmap, "BMAP", &bmapname );
              kmap = astAnnul( kmap );

              smf_get_dims( idata, NULL, NULL, &nbolo, &ntslice, NULL,
                            NULL, NULL, status);

              ggrp = grpNew( "Gain Images", status );
              grpPut1( ggrp, amapname, 0, status );
              grpPut1( ggrp, bmapname, 0, status );

              /* First gain map */
              smf_open_file( ggrp, 1, "READ", 0, &gdata, status );
              gptr1 = dataptr;
              gptr1 += ntslice;
              gptr2 = gdata->pntr[0];
              for( k=0; k<nbolo; k++ ) {
                if( gptr2[k] == 0 ) {
                  gptr1[k] = VAL__BADD;
                } else {
                  gptr1[k] = gptr2[k];
                }
              }
              smf_close_file( &gdata, status );

              /* Second gain map */
              smf_open_file( ggrp, 2, "READ", 0, &gdata, status );
              gptr1 = dataptr;
              gptr1 += ntslice + nbolo + ntslice;
              gptr2 = gdata->pntr[0];
              for( k=0; k<nbolo; k++ ) {
                if( gptr2[k] == 0 ) {
                  gptr1[k] = VAL__BADD;
                } else {
                  gptr1[k] = gptr2[k];
                }
              }
              smf_close_file( &gdata, status );

              grpDelet( &ggrp, status );
            } else {
              /* otherwise zero the buffer */
              memset( dataptr, 0, datalen );
            }
          }


          if( *status == SAI__OK ) {

            /* If leaveopen set, pack the data into a smfArray */
            if( leaveopen ) {

              /* If this is the first element of the subgroup create
                 the smfArray */
              if( j == 0 ) {
                mdata[i] = smf_create_smfArray( status );
              }

              /* Create a smfData for this element of the subgroup */
              flag = SMF__NOCREATE_DA;

              data = smf_create_smfData( flag, status );

              if( *status == SAI__OK ) {
                data->qfamily = head.data.qfamily;
                data->isTordered = head.data.isTordered;
                data->dtype = head.data.dtype;
                data->ndims = head.data.ndims;
                memcpy( data->dims, head.data.dims, sizeof( head.data.dims ) );
                memcpy( data->lbnd, head.data.lbnd, sizeof( head.data.lbnd ) );
                data->hdr->steptime = head.hdr.steptime;

                /* Data pointer points to mmap'd memory AFTER HEADER */
                data->pntr[0] = dataptr;

                /* Store the file descriptor to enable us to unmap when we
                   close */
                if( !nofile ) {
                  data->file->fd = fd;
                }

                /* Copy the DIMM filename into the smfFile. Even though
                   there may not be an associated file on disk we store
                   the name here in case we wish to export the data
                   to an NDF file at a later point. */
                one_strlcpy( data->file->name, name, sizeof(data->file->name),
                             status );

                /* Add the smfData to the smfArray */
                smf_addto_smfArray( mdata[i], data, status );
              }

            } else if (!nofile) {

              /* If leaveopen not set (and there is a file) write buffer to
                 file and close container */

              if( msync( buf, buflen, MS_ASYNC ) == -1 ) {
                *status = SAI__ERROR;
                errRep( FUNC_NAME, "Unable to sync model container file",
                        status );
              } else if( munmap( buf, buflen ) == -1 ) {
                *status = SAI__ERROR;
                errRep( FUNC_NAME, "Unable to unmap model container file",
                        status );
              } else if( close( fd ) == -1 ) {
                *status = SAI__ERROR;
                errRep( FUNC_NAME, "Unable to close model container file",
                        status );
              }
            }
          }

          /* Close the input template file if it was opened here */
          if( igroup ) {
            smf_close_file( &idata, status );
          }
        }

        /* Set loop exit condition if bad status was set */
        if( *status != SAI__OK ) {
          j = thisnrel;
        }
      }

      /* Set loop exit condition if bad status was set */
      if( *status != SAI__OK ) {
        i = nchunks;
      }
    }

  msgOutiff( SMF__TIMER_MSG, "",
             "Created model %s in %.3f s",
             status, smf_model_getname( mtype, status), smf_timerupdate( &tv1, &tv2, status ) );

}
