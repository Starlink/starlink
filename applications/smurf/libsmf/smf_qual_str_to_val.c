/*
*+
*  Name:
*     smf_qual_str_to_val

*  Purpose:
*     Translate a quality name into a SMURF quality value and family

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qual_t smf_qual_str_to_val( const char *qname, smf_qfam_t * family, int * status );

*  Arguments:
*     qname = const char * (Given)
*        Quality name (as returned by smf_qual_str)
*     family = smf_qfam_t * (Returned)
*        Quality family associated with this quality name. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Quality value corresponding to the supplied QNAME.
*     Returns 0 if none could be found and also sets
*     status to bad in that case.

*  Description:
*     Translates a quality name as defined in smf_qual_str into a
*     a value and, optionally, a quality family.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Must keep in sync with smf_qual_str and smf_typ.h, and ensure that
*     the string representations do not exceed SMF_QSTR_MAX characters.

*  History:
*     2010-06-17 (TIMJ):
*        Initial version
*     2010-07-06 (TIMJ):
*        Add SMF__Q_NOISE
*     2011-04-15 (TIMJ):
*        Add SMF__Q_EXT
*     2011-04-15 (DSB):
*        Add SMF__Q_LOWAP
*     2011-09-19 (DSB):
*        Add SMF__Q_BADEF
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
#include "mers.h"

#include "sae_par.h"
#include "smf_err.h"

#include <strings.h>

smf_qual_t smf_qual_str_to_val( const char *qname, smf_qfam_t * family, int * status ) {

  smf_qual_t retval = 0;
  smf_qfam_t lfamily = SMF__QFAM_NULL;

  if (*status != SAI__OK) return retval;

  /* lots of string comparisons...*/
  if (strcmp( qname, "BADDA" ) == 0) {
    retval = SMF__Q_BADDA;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "BADBOL") == 0 ) {
    retval = SMF__Q_BADB;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "SPIKE") == 0 ) {
    retval = SMF__Q_SPIKE;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "DCJUMP") == 0 ) {
    retval = SMF__Q_JUMP;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "PAD") == 0 ) {
    retval = SMF__Q_PAD;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "APOD") == 0 ) {
    retval = SMF__Q_APOD;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "STAT") == 0 ) {
    retval = SMF__Q_STAT;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "COM") == 0 ) {
    retval = SMF__Q_COM;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "FILT") == 0 ) {
    retval = SMF__Q_FILT;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "NOISE") == 0 ) {
    retval = SMF__Q_NOISE;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "EXT") == 0 ) {
    retval = SMF__Q_EXT;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "AST") == 0 ) {
    retval = SMF__MAPQ_AST;
    lfamily = SMF__QFAM_MAP;
  } else if ( strcmp(qname, "COM") == 0 ) {
    retval = SMF__MAPQ_COM;
    lfamily = SMF__QFAM_MAP;
  } else if ( strcmp(qname, "FLT") == 0 ) {
    retval = SMF__MAPQ_FLT;
    lfamily = SMF__QFAM_MAP;
  } else if ( strcmp(qname, "BADBOLO") == 0 ) {
    retval = SMF__TCOMPQ_BAD;
    lfamily = SMF__QFAM_TCOMP;
  } else if ( strcmp(qname, "ENDS") == 0 ) {
    retval = SMF__TCOMPQ_ENDS;
    lfamily = SMF__QFAM_TCOMP;
  } else if ( strcmp(qname, "BLIP") == 0 ) {
    retval = SMF__TCOMPQ_BLIP;
    lfamily = SMF__QFAM_TCOMP;
  } else if ( strcmp(qname, "MATCH") == 0 ) {
    retval = SMF__TCOMPQ_MATCH;
    lfamily = SMF__QFAM_TCOMP;
  } else if ( strcmp(qname, "TEL") == 0 ) {
    retval = SMF__TCOMPQ_TEL;
    lfamily = SMF__QFAM_TCOMP;
  } else if ( strcmp(qname, "LOWAP") == 0 ) {
    retval = SMF__Q_LOWAP;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "BADEF") == 0 ) {
    retval = SMF__Q_BADEF;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "RING") == 0 ) {
    retval = SMF__Q_RING;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "SSN") == 0 ) {
    retval = SMF__Q_SSN;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "PCA") == 0 ) {
    retval = SMF__Q_PCA;
    lfamily = SMF__QFAM_TSERIES;
  } else if ( strcmp(qname, "IP") == 0 ) {
    retval = SMF__Q_IP;
    lfamily = SMF__QFAM_TSERIES;
  } else {
    *status = SMF__BADQNM;
    errRepf( "", "Unrecognized quality name (%s)",
             status, qname );
  }

  if (family) *family = lfamily;
  return retval;

}
