/*
 *+
 *  Name:
 *     smf_grp_related

 *  Purpose:
 *     Create a group of related files

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SMURF subroutine

 *  Invocation:
 *     smf_grp_related( Grp *igrp, const int grpsize, const int grpbywave, 
 *                      dim_t maxlen, dim_t *maxconcatlen, smfGroup **group, 
 *                      int *status );

 *  Arguments:
 *     igrp = Grp* (Given)
 *        Input Grp
 *     grpsize = const int (Given)
 *        Size of input Grp
 *     grpbywave = const int (Given)
 *        Flag to denote whether to group files by common wavelength
 *     maxlen = dim_t (Given)
 *        If set, maximum length of a continuous chunk in time samples. If 0
 *        don't enforce a maximum length.
 *     maxconcatlen = dim_t* (Returned)
 *        The actual length in time samples of the longest continuous chunk.
 *        Can be NULL.
 *     group = smfGroup ** (Returned)
 *        Returned smfGroup
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine groups related files together and populates a
 *     smfGroup. The smfGroup contains a copy of the input Grp, the
 *     number of subgroups within the smfGroup, and an array of
 *     pointers to integer arrays which contain the Grp index values
 *     corresponding to related files. This method reduces the number
 *     of Grps required to 1, and allows new Grps to be created on
 *     demand so that the maximum Grp number is not exceeded. In addition,
 *     continuous subsets of the input data are identified and stored in
 *     the "chunk" component of group. The caller may optionally specify 
 *     a maximum sample length (in time) for these continuous pieces. In
 *     this case, the length of each continuously flagged region is truncated
 *     to the minimum complete set of files that does not exceed the limit
 *     (e.g. files are not broken up into several smaller pieces).

 *  Notes:
 *     Resources allocated with this routine should be freed by calling
 *     smf_close_smfGroup

 *  Authors:
 *     Andy Gibb (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     Ed Chapin (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2006-06-24 (AGG):
 *        Initial version
 *     2006-07-26 (TIMJ):
 *        sc2head no longer used. Use JCMTState instead.
 *     2006-10-11 (AGG):
 *        Additional checks for relatedness: wavelength and dimensions
 *        of data array
 *     2007-07-16 (EC):
 *        -Changed smf_construct_smfGroup interface
 *     2007-10-29 (EC):
 *        Modified interface to smf_open_file; use SMF__NOCREATE_DATA flag.
 *     2007-12-14 (EC):
 *        Actually use SMF__NOCREATE_DATA in smf_open_file call.
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-04-16 (EC):
 *        Fill chunk component of smfGroup based on time stamps
 *     2008-04-17 (EC):
 *        - check for the same subarrays in each chunk
 *        - added maxlen to interface
 *     2008-04-18 (EC):
 *        - check for very short chunks and remove from smfGroup
 *        - fixed memory unallocation bug
 *     2008-04-24 (EC):
 *        - Determine actual nrelated for these data and store in smfGroup
 *     2008-04-28 (EC):
 *        - Added maxconcatlen to interface
 *     2008-04-29 (EC):
 *        -Fixed bug in calculation of actual nrelated (subarrays)
 *     2008-05-03 (EC):
 *        Check absolute value of time step to find discontinuities
 *     2008-07-11 (TIMJ):
 *        sizeof(int) does not work when your ints are changed to longs.
 *        Always use sizeof(*var).
 *     2008-07-18 (TIMJ):
 *        Use smf_find_subarray
 *     2008-07-29 (TIMJ):
 *        Steptime is now in smfHead.
 *     2008-08-20 (EC):
 *        Establish continuity based on OBSIDSS, SEQCOUNT and NSUBSCAN
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2008 University of British Columbia.
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_grp_related"

void smf_grp_related(  Grp *igrp, const int grpsize, const int grpbywave, 
                       dim_t maxlen, dim_t *maxconcatlen, smfGroup **group, 
                       int *status ) {

  /* Local variables */
  dim_t *all_len = NULL;      /* Lengths of each chunk */
  size_t allOK = 1;           /* Flag to determine whether to continue */
  size_t *chunk=NULL;         /* Array of flags for continuous chunks */
  int chunkend;               /* Index to end of chunk */
  int chunkstart;             /* Index to start of chunk */
  smfData *data = NULL;       /* Current smfData */
  smfData *data2 = NULL;      /* Second smfData */
  double *ends = NULL;        /* Array of ending RTS_END values */
  int frame;                  /* Variable to store either first or last frame*/
  smfHead *hdr=NULL;          /* Header for current file */
  smfHead *hdr2=NULL;         /* Second header */
  int i;                      /* Loop counter for index into Grp */
  dim_t isub;                 /* loop counter over subgroup */
  dim_t *indices = NULL;      /* Array of indices to be stored in subgroup */
  int j;                      /* Loop counter */
  int k = 0;                  /* Incremental counter for subgroup indices */
  int *keepchunk=NULL;        /* Flag for chunks that will be kept */
  double *lambda = NULL;      /* Wavelength - only used if grpbywave is true */
  int matchsubsys;            /* Flag for matched subarrays */
  dim_t maxconcat=0;          /* Longest continuous chunk length */
  int maxnelem=0;             /* Max elem (subarrays) encountered */
  dim_t *nbolx = NULL;        /* Number of bolometers in X direction */
  dim_t *nboly = NULL;        /* Number of bolometers in Y direction */
  int nelem;                  /* Number of elements in index array */
  size_t *new_chunk=NULL;     /* chunks associated with new_subgroups */
  dim_t new_ngroups=0;        /* counter for new_subgroups */
  dim_t **new_subgroups=NULL; /* subgroups that are long enough */
  int ngroups = 0;            /* Counter for subgroups to be stored */
  dim_t nx;                   /* (data->dims)[0] */
  dim_t ny;                   /* (data->dims)[1] */
  double obslam;              /* Observed wavelength from FITS header (m) */
  double opentime;            /* RTS_END value at beginning of written data */
  int refnsubscan;            /* reference subscan file counter */
  char refobsidss[SZFITSCARD];/* reference obsidss */
  int refseqcount;            /* reference sequence counter */
  int *refsubsys=NULL;        /* Array of template subarrays */
  int seqcount;               /* Sequence counter */
  int nsubscan;               /* subscan file counter */
  double *starts = NULL;      /* Array of starting RTS_END values */
  double steptime = 0;        /* Length of a sample */
  dim_t **subgroups = NULL;   /* Array containing index arrays to parent Grp */
  int subsysnum;              /* Subsystem numeric id. 0 - 8 */
  size_t thischunk;           /* Current chunk that we're on */
  dim_t thislen;              /* Length of current time chunk */
  int thisnelem;              /* Number of elements (subarrays) at this chunk*/
  dim_t totlen;               /* Total length of continuous time chunk */
  double writetime;           /* RTS_END value at end of written data */

  if ( *status != SAI__OK ) return;

  /* Check that the Grp size is reasonable */
  if ( grpsize < 1 || grpsize > GRP__MAXG ) {
    if ( *status == SAI__OK ) {
      msgSeti("SZ",grpsize);
      msgSeti("MAX",GRP__MAXG);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid input Grp size, ^SZ. Must lie between 1 and ^MAX", status);
    }
  }

  /* Allocate space for groups array - can't be any bigger than
     grpsize so use that. Initialize everything to NULL */
  subgroups = smf_malloc( grpsize, sizeof(*subgroups), 1, status );
  starts = smf_malloc( grpsize, sizeof(*starts), 1, status );
  ends = smf_malloc( grpsize, sizeof(*ends), 1, status );
  nbolx = smf_malloc( grpsize, sizeof(*nbolx), 1, status );
  nboly = smf_malloc( grpsize, sizeof(*nboly), 1, status );
  chunk = smf_malloc( grpsize, sizeof(*chunk), 1, status );
  new_subgroups = smf_malloc( grpsize, sizeof(*new_subgroups), 1, status );
  new_chunk = smf_malloc( grpsize, sizeof(*new_chunk) , 1, status );

  if ( *status != SAI__OK ) goto CLEANUP;

  /* Do we want to group by wavelength? */
  if ( grpbywave == 1 ) {
    /* If yes, then we only need to allocate space for 4 elements */
    nelem = SMF__MXSMF;
    /* And define the wavelength array */
    lambda = smf_malloc( grpsize, sizeof(*lambda), 1, status );
  } else {
    /* OK we might have data from up to 8 subarrays so allocate
       twice as much space */
    nelem = 2*SMF__MXSMF;
  }

  /* Loop over files in input Grp: remember Grps are indexed from 1 */
  for (i=1; i<=grpsize; i++) {
    /* First step: open file and read start/end RTS_END values */
    smf_open_file( igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );
    hdr = data->hdr;

    /* Set header for first time slice */
    frame = 0;
    smf_tslice_ast( data, frame, 0, status );
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to retrieve first timeslice", status);
      return;
    }
    opentime = hdr->state->rts_end;
    /* Set header for last time slice */
    frame = (data->dims)[2] - 1;
    smf_tslice_ast( data, frame, 0, status );
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to retrieve final timeslice", status);
      return;
    }
    writetime = hdr->state->rts_end;
    /* Retrieve wavelength if necessary */
    if ( grpbywave ) {
      smf_fits_getD(hdr, "WAVELEN", &obslam, status );
    }
    if ( *status != SAI__OK ) return;

    /* Now get data dimensions */
    nx = (data->dims)[0];
    ny = (data->dims)[1];
    /* Now to check if it's a related file... */
    for ( j=0; j<grpsize; j++ ) {
      /* Does the subgroup exist? */
      if ( subgroups[j] != 0 ) {
        /* If yes, are we grouping by wavelength? */
        if ( grpbywave ) {
          if ( obslam != lambda[j] ) {
            allOK = 0;
          }
        }
        if ( allOK ) {
          /* Then check if the RTS_END values match */
          indices = subgroups[j];
          if ( opentime == starts[j] && writetime == ends[j] ) {
            if ( nx == nbolx[j] && ny == nboly[j]) {
              /* Store in first available slot in current subgroup.
                 No point starting at 0 because it WILL be occupied if
                 we've reached this point */
              for ( k=1; k<nelem; k++) {
                if (indices[k] == 0) {
                  indices[k] = i;
                  goto CLOSE;
                }
              }
            } else {
              msgOutif(MSG__VERB," ", 
                       "Start and end times match but data arrays are of different size", 
                       status);
            }
          }
        } /* Close if allOK */
      } else {
        /* If not, there's nothing to match so create it and add the
           current index */
        indices = smf_malloc( nelem, sizeof(*indices), 1, status);
        /* Initialize the pointers to NULL */
        if ( *status != SAI__OK ) {
          errRep(FUNC_NAME, "Unable to allocate memory to store index array", status);
          return;
        }
        indices[0] = i;
        subgroups[j] = indices;
        /* Add new open/end times to arrays */
        starts[j] = opentime;
        ends[j] = writetime;
        nbolx[j] = nx;
        nboly[j] = ny;
        if ( grpbywave ) {
          lambda[j] = obslam;
        }
        ngroups++;
        goto CLOSE;
      }
    }
  CLOSE:
    smf_close_file( &data, status );
  }
  /* Sanity check - make sure that the number of groups is less than
     the grpsize */
  if ( ngroups > grpsize ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("G",grpsize);
      msgSeti("S",ngroups);
      errRep(FUNC_NAME, 
             "Number of subgroups, ^S, exceeds grpsize, ^G. Possible programming error", 
             status);
    }
  }

  /* At this stage assume the subgroups are time-sorted, and that each
     file within the group is itself continuous. Also assume that the
     subarrays are sorted (second index of subgroups). Check the last
     sample of each file with the first sample of the next to set up
     continuous chunk flags. Also check that all of the same subarrays
     are present in subsequent chunks flagged "continuous". */

  refsubsys = smf_malloc( nelem, sizeof(*refsubsys), 0, status );
  all_len = smf_malloc( grpsize, sizeof(*all_len), 1, status ); 

  thislen = 0;
  totlen = 0;
  thischunk = 0;
  maxconcat = 0;
  refseqcount=0;
  refnsubscan=0;
  refobsidss[0] = '\0';

  for( i=0; i<ngroups; i++ ) {

    /* Open header of the first file at each time */
    smf_open_file( igrp, subgroups[i][0], "READ", SMF__NOCREATE_DATA, &data, 
                   status );


    /* Read the SEQCOUNT and NSUBSCAN header values */
    hdr = data->hdr;
    smf_fits_getI( hdr, "SEQCOUNT", &seqcount, status );
    smf_fits_getI( hdr, "NSUBSCAN", &nsubscan, status );

    if( *status == SAI__OK ) {

      /* Length of chunk */
      if( data->isTordered ) {
        thislen = data->dims[2];
      } else {
        thislen = data->dims[0];
      }

      /* Store length to check chunk lengths later */
      all_len[i] = thislen;

      /* Check that an individual file is too long */
      if( maxlen && (thislen > maxlen) ) {
        *status = SAI__ERROR;
        msgSeti("THISLEN",thislen);
        msgSeti("MAXLEN",maxlen);
        errRep(FUNC_NAME, 
               "Length of file time steps exceeds maximum (^THISLEN>^MAXLEN)",
               status);
      } 

      /* Add length to running total */
      totlen += thislen;

      /* Update maxconcat */
      if( totlen > maxconcat ) {
        maxconcat = totlen;
      }
    }

    if( i == 0 ) {
      /* length of a sample */
      steptime = hdr->steptime;
    }

    /* Set header to first time slice and obtain RTS_END */
    frame = 0;
    smf_tslice_ast( data, frame, 0, status );

    if( *status == SAI__OK ) {
      if( i > 0 ) {
        /* check that we have the same subarrays */
        matchsubsys = 1;

        for( j=0; j<nelem; j++ ) {
          if( subgroups[i][j] > 0 ) {
            /* Increment subarray counter */
            thisnelem++;

            if( j==0 ) {
              /* Look at header of file that is already opened */
              smf_find_subarray( hdr, NULL, 0, &subsysnum, status );
            } else {
              /* Otherwise open header in new file */
              smf_open_file( igrp, subgroups[i][j], "READ", 
                             SMF__NOCREATE_DATA, &data2, status );
              if( *status == SAI__OK ) {
                hdr2 = data2->hdr;
                smf_find_subarray( hdr2, NULL, 0, &subsysnum, status );
              }
            }	

            /* Close the new file that we've opened */
            if( j>0 ) {
              smf_close_file( &data2, status );
            }
          } else {
            /* Flag this subsystem spot as empty */
            subsysnum = -1;
          }

          if( subsysnum != refsubsys[j] ) {
            matchsubsys = 0;
          }
        }

        /* check against writetime from the last file to see if we're on the
           same chunk. Also check that the subsystems match, and that the
           continuous chunk doesn't exceed maxlen */

        if( !( !strncmp(refobsidss, hdr->obsidss, sizeof(refobsidss)) && 
              (seqcount==refseqcount) && ((nsubscan-refnsubscan)==1) ) || 
            !matchsubsys || (maxlen && (totlen > maxlen)) ) {

          /* Found a discontinuity */
          thischunk++;

          /* Since this piece puts us over the limit, it is now the initial
             total length for the next chunk. */
          totlen = thislen;
        }
      }

      /* update refobsidss, refseqcount, refnsubscan */
      one_strlcpy( refobsidss, hdr->obsidss, sizeof(refobsidss), status );
      refseqcount = seqcount;
      refnsubscan = nsubscan;
    }

    /* Obtain the last RTS_END from this file */
    if( *status == SAI__OK ) {
      frame = (data->dims)[2] - 1;
    }

    smf_tslice_ast( data, frame, 0, status );
    
    if( *status == SAI__OK ) {
      /* Populate the reference subsystem array */
      for( j=0; j<nelem; j++ ) {
        if( subgroups[i][j] > 0 ) {
          if( j==0 ) {
            /* Look at header of file that is already opened */
            smf_find_subarray( hdr, NULL, 0, &subsysnum, status );
          } else {
            /* Otherwise open header in new file */
            smf_open_file( igrp, subgroups[i][j], "READ", SMF__NOCREATE_DATA, 
                           &data2, status );
            if (*status == SAI__OK) {
              hdr2 = data2->hdr;
              smf_find_subarray( hdr2, NULL, 0, &subsysnum, status );
            }
          }	
          if( *status == SAI__OK ) {
            refsubsys[j] = subsysnum;
          }
          /* Close the new file that we've opened */
          if( j>0 ) {
            smf_close_file( &data2, status );
          }
        } else if( *status == SAI__OK ) {
          /* Flag this subsystem slot as empty */
          refsubsys[j] = -1;
        }
      }

      /* Store thischunk in chunk */
      chunk[i] = thischunk;
    }

    /* Close file */
    smf_close_file( &data, status );

  }

  /* Now that the continuous chunks are flagged, check for any chunk that
     is shorter than SMF__MINCHUNKSAMP in length (time) and remove it */

  keepchunk = smf_malloc( ngroups, sizeof(*keepchunk), 0, status );

  if( *status == SAI__OK ) {

    for( i=0; i<ngroups; i++ ) {
      keepchunk[i] = 1;
    }

    totlen = all_len[0];
    chunkstart = 0;

    for( i=1; i<=ngroups; i++ ) {
      
      /* Chunk finished? */
      if( (i==ngroups) || (chunk[i] != chunk[i-1]) ) {
	
        chunkend = i-1;
	
        /* Flag the pieces of last chunk as bad if it was too short */
        if( totlen < SMF__MINCHUNKSAMP ) {
          for( j=chunkstart; j<=chunkend; j++ ) {
            keepchunk[j] = 0;
          }
	  
          /* Warning message */
          msgSeti("LEN",totlen);
          msgSeti("MIN",SMF__MINCHUNKSAMP);
          msgOut( " ", "SMF_GRP_RELATED: ignoring short chunk (^LEN<^MIN)", 
                  status);
        }
	
        /* Re-set the chunk start/length */
        chunkstart = i;
        totlen = 0;
      }
      
      /* Add to the length of the current chunk if we're not at the end */
      if( i<ngroups ) totlen += all_len[i];
    }

    /* Determine max subarrays */

    maxnelem = 0;
    if( *status == SAI__OK ) for( i=0; i<ngroups; i++ ) {
        thisnelem = 0;
      
        for( j=0; j<nelem; j++ ) {
          if( subgroups[i][j] > 0 ) {
            /* Increment subarray counter */
            thisnelem++;
          }
        }
      
        /* update maxnelem based on this chunk */
        if( thisnelem > maxnelem ) {
          maxnelem = thisnelem;
        }
      }

    /* Create the new subgroups array from the chunks that we're keeping */
    new_ngroups=0;

    for( i=0; i<ngroups; i++ ) {
      if( keepchunk[i] ) {
        indices = smf_malloc( maxnelem, sizeof(*indices), 1, status);
        if( *status == SAI__OK ) {
          memcpy( indices, subgroups[i], maxnelem*sizeof(*indices) );
          new_subgroups[new_ngroups] = indices;
          new_chunk[new_ngroups] = chunk[i];
          new_ngroups++;
        }
      }
    }
  }

  
  /* Create the smfGroup */
    
  *group = smf_construct_smfGroup( igrp, new_subgroups, new_chunk, new_ngroups,
                                   maxnelem, 0, status );

  /* Return maxconcatlen if requested */
  if( maxconcatlen ) {
    *maxconcatlen = maxconcat; 
  }

 CLEANUP:
  starts = smf_free( starts, status );
  ends = smf_free( ends, status );
  nbolx = smf_free( nbolx, status );
  nboly = smf_free( nboly, status );
  refsubsys = smf_free( refsubsys, status );
  keepchunk = smf_free( keepchunk, status );
  chunk = smf_free( chunk, status );
  all_len = smf_free( all_len, status); 
  lambda = smf_free( lambda, status );

  if( subgroups ) {
    for( i=0; i<ngroups; i++ ) {
      subgroups[i] = smf_free(subgroups[i], status);
    }
    subgroups = smf_free( subgroups, status );
  }
  
  if( *status != SAI__OK ) {
    new_chunk = smf_free(new_chunk, status);

    if( new_subgroups ) {
      for( isub=0; isub<new_ngroups; i++ ) {
        new_subgroups[isub] = smf_free( new_subgroups[isub], status );
      }
      new_subgroups = smf_free( new_subgroups, status );
    }

  }

}
