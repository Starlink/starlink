/*
*+
*  Name:
*     smf_qual_map

*  Purpose:
*     Map a quality array and maybe copy to malloced buffer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qual_t * smf_qual_map( int indf, const char mode[], smf_qfam_t *family,
*               size_t *nmap, int * status );

*  Arguments:
*     indf = int (Given)
*        NDF identifier
*     mode = const char [] (Given)
*        Access mode to use for mapping
*     family = smf_qfam_t * (Returned)
*        Indicate which family of quality bits is represented in "qual". Can be NULL.
*        Will only have a meaningful value if the file has been opened in READ or
*        UPDATE mode (otherwise the values in the array are fresh).
*     nmap = size_t * (Returned)
*        Number of elements mapped or malloced
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     smf_qual_t *
*        Pointer to quality values. Free using smf_qual_unmap.

*  Description:
*     Map the QUALITY component of an NDF using the supplied access mode.
*     If the internal SMURF quality is not _UBYTE an array will be
*     malloced and the mapped data will be copied over. Additionally
*     the QUALITY itself will then be unmapped. If the internal SMURF
*     quality is unsigned char the pointer to the mapped array will
*     be returned directly.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Unmap the array using smf_qual_unmap to make sure that malloced
*     memory is freed properly.

*  History:
*     2010-06-16 (TIMJ):
*        Initial version
*     2011-04-11 (TIMJ):
*        Simplify initialisation logic. Always initialise.
*     2011-04-26 (DSB):
*        Added SMF__Q_LOWAP.
*     2011-09-19 (DSB):
*        Added SMF__Q_BADEF.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
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

#include "smf.h"
#include "smf_err.h"

#include "star/irq.h"
#include "ndf.h"
#include "sae_par.h"
#include "ast.h"
#include "irq_err.h"
#include "mers.h"

/* number of NDF quality bits */
#define NDFBITS 8

smf_qual_t * smf_qual_map( int indf, const char mode[], smf_qfam_t *family,
                           size_t *nmap, int * status ) {

  size_t i;             /* Loop counter */
  int itemp = 0;        /* temporary int */
  smf_qfam_t lfamily = SMF__QFAM_NULL; /* Local quality family */
  size_t nout;          /* Number of elements mapped */
  size_t numqn = 0;     /* number of quality names */
  IRQLocs *qlocs = NULL;/* IRQ Quality */
  unsigned char *qmap;  /* pointer to mapped unsigned bytes */
  void *qpntr[1];       /* Somewhere to put the mapped pointer */
  smf_qual_t *retval = NULL; /* Returned pointer */
  char xname[DAT__SZNAM+1];  /* Name of extension holding quality names */


  if (*status != SAI__OK) return retval;

  /* how many elements do we need */
  ndfSize( indf, &itemp, status );
  nout = itemp;
  if (nmap) *nmap = nout;

  /* malloc the QUALITY buffer. Initialise to zero to simplify logic
     below. It is difficult to determine in advance which case can use
     initialisation. */
  retval = astCalloc( nout, sizeof(*retval) );

  /* READ and UPDATE mode require that the QUALITY is processed
     and copied before being returned. WRITE mode means that the
     buffer contains no information to copy yet. WRITE/ZERO
     and WRITE/BAD also require that we do not do any quality
     handling */
  if ( strncmp(mode, "WRITE",5) == 0 ) {
    /* WRITE and WRITE/ZERO are actually treated the same way
       because we always initialise */
    if ( strcmp( mode, "WRITE/BAD") == 0 ) {
      for (i=0; i<nout; i++) {
        retval[i] = VAL__BADQ;
      }
    }

    /* unmap the NDF buffer and return the pointer */
    if (family) *family = lfamily;
    return retval;
  }

  /* Map the quality component (we always need to do this) */
  ndfMap( indf, "QUALITY", "_UBYTE", mode, &qpntr[0], &itemp, status );
  qmap = qpntr[0];

  /* Need to find out what quality names are in play so we
     can work out which family to translate them to */
  irqFind( indf, &qlocs, xname, status );
  numqn = irqNumqn( qlocs, status );

  if ( *status == IRQ__NOQNI || numqn == 0) {
    /* do not have any names defined so we have no choice
       in copying the values directly out the file */
    if (*status != SAI__OK) errAnnul( status );

    /* simple copy with type conversion */
    for (i=0; i<nout; i++) {
      retval[i] = qmap[i];
    }

  } else {
    IRQcntxt contxt = 0;
    int ndfqtosmf[NDFBITS];     /* NDF bit (arr index) and SMURF alternative */
    int ndfqtoval[NDFBITS];     /* NDF bit (arr index) and corresponding Qual value */
    int ndfqval[NDFBITS];       /* Bit values for NDF quality */
    int identity = 1;     /* Is this a simple identity map? */

    /* prefill the mapping with bit to bit mapping */
    for (i=0; i<NDFBITS; i++) {
      ndfqtosmf[i] = i;
      ndfqtoval[i] = BIT_TO_VAL(i);
      ndfqval[i] = ndfqtoval[i];
    }

    /* Now translate each name to a bit */
    for (i = 0; i < numqn; i++) {
      char qname[IRQ__SZQNM+1];
      char commnt[IRQ__SZCOM+1];
      int fixed;
      int value;
      int bit;
      int done;
      smf_qual_t qval;
      smf_qfam_t tmpfam = 0;

      irqNxtqn( qlocs, &contxt, qname, &fixed, &value, &bit,
                commnt,sizeof(commnt), &done, status );
      bit--; /* IRQ starts at 1 */

      /* Now convert the quality name to a quality value
         and convert that to a bit. These should all be
         less than 9 bits because they are in the NDF file. */
      qval = smf_qual_str_to_val( qname, &tmpfam, status );

      if (*status == SMF__BADQNM || tmpfam == SMF__QFAM_NULL ) {
        /* annul status and just copy this bit from the file
           to SMURF without change. This might result in a clash
           of bits but we either do that or drop out the loop
           and assume everything is broken */
        if (*status != SAI__OK) errAnnul(status);
        ndfqtosmf[bit] = bit;
        ndfqtoval[bit] = BIT_TO_VAL(bit);

      } else {
        if (lfamily == SMF__QFAM_NULL) {
          lfamily = tmpfam;
        } else if (lfamily != tmpfam) {
          msgOutif(MSG__QUIET, "",
                   "WARNING: Quality names in file come from different families",
                   status );
        }
        ndfqtosmf[bit] = smf_qual_to_bit( qval, status );
        ndfqtoval[bit] = qval;

        /* not a 1 to 1 bit translation */
        if (bit != ndfqtosmf[bit]) identity = 0;
      }
    }

    /* Now copy from the file and translate the bits. If this is an
       identity mapping or we do not know the family then we go quick. */
    if ( (identity && lfamily != SMF__QFAM_TCOMP) || lfamily == SMF__QFAM_NULL) {
      for (i=0; i<nout; i++) {
        retval[i] = qmap[i];
      }
    } else {
      if (*status == SAI__OK) {
        for (i=0; i<nout; i++) {
          if (qmap[i] == 0) {
            /* Output buffer would be set to zero but it already has that value */
          } else {
            /* this becomes a very laborious bitwise copy.
             One to one mapping for TSERIES and MAP families. */
            if (lfamily == SMF__QFAM_TSERIES ||
                lfamily == SMF__QFAM_MAP ) {
              size_t k;
              for (k=0; k<NDFBITS; k++) {
                if ( qmap[i] & ndfqval[k] ) retval[i] |= ndfqtoval[k];
              }

            } else if (lfamily == SMF__QFAM_TCOMP) {
              size_t k;
              /* This requires some guess work */
              for (k=0; k<NDFBITS; k++) {
                if ( qmap[i] & ndfqval[k]) {
                  if (ndfqtoval[k] & SMF__TCOMPQ_BAD ) {
                    retval[i] |= (SMF__Q_BADB|SMF__Q_BADDA);
                  } else if (ndfqtoval[k] & SMF__TCOMPQ_ENDS) {
                    retval[i] |= (SMF__Q_PAD|SMF__Q_APOD);
                  } else if (ndfqtoval[k] & SMF__TCOMPQ_BLIP) {
                    retval[i] |= (SMF__Q_SPIKE | SMF__Q_JUMP | SMF__Q_FILT | SMF__Q_LOWAP | SMF__Q_BADEF);
                  } else if (ndfqtoval[k] & SMF__TCOMPQ_MATCH) {
                    retval[i] |= SMF__Q_COM;
                  } else if (ndfqtoval[k] & SMF__TCOMPQ_TEL) {
                    retval[i] |= SMF__Q_STAT;
                  } else {
                    /* just do the normal mapping */
                    retval[i] |= ndfqtoval[k];
                  }
                }
              }

            } else {
              *status = SAI__ERROR;
              errRep("", "Did not recognize quality family. "
                     "(Possible programming error)", status );
              break;
            }

          }
        }
      }

      /* we have uncompressed */
      if (lfamily == SMF__QFAM_TCOMP) lfamily = SMF__QFAM_TSERIES;

    }

  }

  /* Free quality */
  irqRlse( &qlocs, status );

  /* no longer need the mapped data */
  ndfUnmap( indf, "QUALITY", status );

  if (family) *family = lfamily;
  return retval;
}
