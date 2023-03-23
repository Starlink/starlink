/*
*+
*  Name:
*     smf_select_cqualpntr

*  Purpose:
*     Select the relevant pointer to quality information from a const smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     const smf_qual_t * smf_select_cqualpntr( const smfData *data,
*                                smf_qfam_t * qfamily, int * status );

*  Arguments:
*     data = const smfData * data (Given)
*        Input smfData.
*     qfamily = smf_qfam_t * qfamily (Returned)
*        If non-NULL will return the quality family associated with
*        the selected quality pointer.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Decides which pointer in the smfData should be used for quality
*     handling. The quality will be selected in the following order:
*
*     - If the "qual" pointer is defined return it
*     - If the "sidequal" smfData is non-null and has a "qual" pointer
*       return that.
*     - If the "sidequal" smfData is non-null and pntr[0] is defined
*       with data type SMF__QUALTYPE that is the quality.
*     - If all that has failed look at the primary pntr[0] to decide
*       if it looks like a quality array after all.
*
*     Else a NULL pointer is returned.

*  Returned Value:
*     const smf_qual_t *
*        Relevant pointer to quality or NULL if none could be found.

*  Notes:
*     See also smf_select_qualpntr

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-07-07 (TIMJ):
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf.h"

#include "sae_par.h"

#pragma GCC diagnostic ignored "-Wcast-qual"

const smf_qual_t * smf_select_cqualpntr( const smfData * data,
                                        smf_qfam_t *qfamily, int * status ) {
  smf_qual_t * retval = NULL;

  /* horrible hack so that we do not need to duplicate code from
     smf_select_qualpntr */

  retval = smf_select_qualpntr( (smfData*)data, qfamily, status );

  return (const smf_qual_t *) retval;
}
