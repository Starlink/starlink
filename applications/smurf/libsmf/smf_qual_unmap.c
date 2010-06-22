/*
*+
*  Name:
*     smf_qual_unmap

*  Purpose:
*     Unmap a quality array, copying values if appropriate

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qual_t * smf_qual_unmap( int indf, smf_qfam_t family,
*                                  smf_qual_t * qual, int * status );

*  Arguments:
*     indf = int (Given)
*        NDF identifier
*     family = smf_qfam_t (Given)
*        Indicate which family of quality bits is represented in "qual".
*     qual = const smf_qual_t * (Given)
*        Pointer to quality data to be unmapped or freed. Should not be used
*        after calling this routine.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Returns a NULL pointer.

*  Description:
*     Unmaps quality values associated with an NDF. If the internal SMURF quality is
*     the same size as NDF quality (unsigned char) then this will simply
*     be a call to ndfUnmap. If the size differs and the NDF has been opened with
*     WRITE access the QUALITY component will be mapped and the values will be
*     copied after first compressing them to fit in 8 bytes.
*
*     Quality names are stored in the file corresponding to which bits were
*     present and which family was being used.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The pointer to quality must have been obtained by a call to
*     smf_qual_map.
*     - The quality names of the items written by this routine are recorded
*     using IRQ.

*  History:
*     2010-06-17 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

#include "smf.h"

#include "ndf.h"
#include "sae_par.h"
#include "ast.h"
#include "star/irq.h"
#include "mers.h"
#include "irq_err.h"

#define FUNC_NAME "smf_qual_unmap"

smf_qual_t * smf_qual_unmap( int indf, smf_qfam_t family, smf_qual_t * qual, int * status ) {
  int canwrite = 0;   /* can we write to the file? */
  int wasmapped = 0;  /* Was the QUALITY mapped? */
  size_t NQBITS = 0;  /* Number of quality bits in this family */

  if (*status != SAI__OK) return NULL;

  /* see if the data were mapped */
  wasmapped = ( SMF__QUALTYPE == SMF__UBYTE ? 1 : 0 );

  /* if we do not have an NDF identifier we just free the memory */
  if (indf == NDF__NOID) goto CLEANUP;

  /* work out how many quality items are in this family */
  switch( family ) {
  case SMF__QFAM_TSERIES:
    NQBITS = SMF__NQBITS_TSERIES;
    break;
  case SMF__QFAM_MAP:
    NQBITS = SMF__NQBITS_MAP;
    break;
  default:
    /* note that TCOMP is not an allowed quality because SMURF should not be
       using it anywhere in a permanent way. */
    *status = SAI__ERROR;
    ndfMsg( "NDF", indf );
    errRepf( "", "Unsupported quality family '%s' for quality unmapping of "
             "file ^NDF", status, smf_qfamily_str(family,status) );
    goto CLEANUP;
  }

  /* See if we have WRITE access to the file */
  ndfIsacc( indf, "WRITE", &canwrite, status );

  /* if we have WRITE access and the data were not mapped we have
     to copy to the file. Also check we have a non-NULL input pointer.
     If the data were mapped we still have to make sure the quality names
     are stored. */
  if ( canwrite && qual ) {
    int highbit = -1; /* highest bit used */
    int lowbit = -1;  /* Lowest bit used */
    int nout;
    int nqual = 0;
    void *qpntr[1];
    size_t qcount[SMF__NQBITS]; /* statically allocate the largest array */
    IRQLocs *qlocs;
    smf_qual_t * qmap;
    int there;

    /* how many pixels in NDF (assumed to be number in quality) */
    ndfSize( indf, &nout, status );

    /* Work out which bits are actually used */
    if (*status == SAI__OK) {
      size_t i;
      size_t k;
      /* now we try to be a bit clever. It may be a mistake since we have to
         do multiple passes through "qual". First determine how many quality
         bits are actually set. */
      for (i = 0; i<nout; i++) {
        /* try all the bits */
        for( k=0; k<NQBITS; k++ ) {
          if( qual[i] & BIT_TO_VAL(k) ) {
            qcount[k]++;
          }
        }
      }

      /* see how many we got */
      for (k=0; k<NQBITS; k++) {
        if ( qcount[k] ) {
          nqual++;
          highbit = k;
          if (lowbit < 0) lowbit = k;
        }
      }
    }

    /* we need to unmap if we are about to use IRQ */
    if (wasmapped) {
      ndfMsg( "FILE", indf );
      msgOutif( MSG__DEBUG20, "", "Unmapped QUALITY ^FILE", status );
      if (indf != NDF__NOID) ndfUnmap( indf, "QUALITY", status );
      return NULL;
    }

    /* for IRQ we need to ensure the SMURF extension exists so open and annul it. */
    if (*status == SAI__OK) {
      char xname[DAT__SZNAM+1];
      irqFind( indf, &qlocs, xname, status );
      if (*status == IRQ__NOQNI) {
        errAnnul( status );
        ndfXstat( indf, SMURF__EXTNAME, &there, status );
        if (!there) {
          HDSLoc * smurfloc = NULL;
          /* Create SMURF extension if it does not already exist */
          ndfXnew( indf, SMURF__EXTNAME, SMURF__EXTTYPE, 0, NULL, &smurfloc, status );
          if (smurfloc) datAnnul( &smurfloc, status );
        }
        irqNew( indf, SMURF__EXTNAME, &qlocs, status );
      }
    }

    if (wasmapped) {
      if (*status == SAI__OK) {
        printf("ASSUME THIS WAS MAPPED QUALITY %p\n",qual);
        if (highbit < 8) {
          size_t k;

          /* Just write out the quality names. Depends on family
             and we have to start at bit 0 */
          for (k=lowbit; k<=highbit; k++) {
            const char * qdesc = NULL; /* Description of quality */
            const char * qstr = NULL;  /* Quality string identifier */
            qstr = smf_qual_str( family, 1, k, &qdesc, status );

            /* Set the quality name */
            if (qstr) {
              irqAddqn( qlocs, qstr, 0, qdesc, status );
            } else {
              char buff[10];
              sprintf(buff, "BIT%d", (int)k );
              irqAddqn( qlocs, buff, 0, "Unknown quality", status );
            }
          }

        } else {
          *status = SAI__ERROR;
          errRep("", "Too many quality bits being used!",
                 status );
        }
      }
    } else {
      /* malloced so we need to map and copy over the values. IRQ
         names need to be set BEFORE we copy. */

      /* Map the quality component with WRITE access */
      ndfMap( indf, "QUALITY", "_UBYTE", "WRITE", &qpntr[0], &nout, status );
      qmap = qpntr[0];
      printf("MAPPED QUALITY %p\n",qmap);
      /* we assume the number of elements in "qual" is the same as in "qmap" */
      if (*status == SAI__OK) {
        size_t i;
        size_t k;

        /* if we only have 8 or fewer bits active we can just compress
           by mapping them to the lower 8 bits. This will work if we also
           set the IRQ quality names in the NDF. */
        if (nqual == 0 ) {
          /* easy */
          memset( qmap, 0, nout * smf_dtype_sz( SMF__UBYTE, status ) );
        } else if ( nqual <= 8 ) {

          /* and the quality names. Start at lowbit and go to highbit
           knowing that we have shifted them down so that lowbit in qual
           is bit 0 in NDF. */
          for (k=lowbit; k<=highbit; k++) {
            if (qcount[k]) {
              const char * qdesc = NULL; /* Description of quality */
              const char * qstr = NULL;  /* Quality string identifier */
              qstr = smf_qual_str( family, 1, k, &qdesc, status );

              irqAddqn( qlocs, qstr, 0, qdesc, status );
            }
          }

          /* shift them down */
          for (i=0; i<nout; i++) {
            size_t curbit = 0;
            qmap[i] = 0;

            for (k=lowbit; k<=highbit; k++) {
              if (qcount[k] && qual[k]&BIT_TO_VAL(k)) {
                qmap[i] |= BIT_TO_VAL(curbit);
                curbit++;
              }
            }
          }

        } else {

          /* Quality names are now needed and we have to write them
             all out because we have not compressed the bits in the
             output quality array we've only compressed the input */

          for (k=0; k<=SMF__NQBITS_TCOMP; k++) {
            const char * qdesc = NULL; /* Description of quality */
            const char * qstr = NULL;  /* Quality string identifier */
            qstr = smf_qual_str( SMF__QFAM_TCOMP, 1, k, &qdesc, status );

            /* Set the quality name */
            irqAddqn( qlocs, qstr, 0, qdesc, status );
          }

          /* compress them */
          for (i = 0; i<nout; i++) {
            qmap[i] = 0;
            if (qual[i]) {
              if ( qual[i] & (SMF__Q_BADDA|SMF__Q_BADB) ) {
                qmap[i] |= SMF__TCOMPQ_BAD;
              }
              if ( qual[i] & (SMF__Q_APOD|SMF__Q_PAD) ) {
                qmap[i] |= SMF__TCOMPQ_ENDS;
              }
              if ( qual[i] & (SMF__Q_JUMP|SMF__Q_SPIKE) ) {
                qmap[i] |= SMF__TCOMPQ_BLIP;
              }
              if ( qual[i] & (SMF__Q_COM) ) {
                qmap[i] |= SMF__TCOMPQ_MATCH;
              }
              if ( qual[i] & (SMF__Q_STAT) ) {
                qmap[i] |= SMF__TCOMPQ_TEL;
              }
              if (qmap[i] == 0 ) {
                /* something went wrong. We missed a quality bit somewhere */
                msgOutiff(MSG__QUIET, "", FUNC_NAME ": Untested quality bit found"
                          " in position %zu with value %u", status,
                          i, (unsigned int)qual[i]);
              }
            }
          }

        }
      }
    }


    /* release IRQ resources */
    irqRlse( &qlocs, status );
  }

 CLEANUP:
  /* Tidy up */
  if (!wasmapped) {
    /* assume smf_quality_map malloced it */
    qual = astFree( qual );
  }
  return NULL;

}
