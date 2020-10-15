/*
*+
*  Name:
*     smf_accumulate_prov

*  Purpose:
*     Store file provenance from Grp information using smf_updateprov

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_accumulate_prov( const smfData * data, const Grp * igrp,
*                    dim_t index, int ondf, const char * creator,
*                    NdgProvenance * modprov, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Input file from which provenance should be obtained. If no
*        NDF identifier is present the Grp will be used. Can be NULL.
*     igrp = const Grp * (Given)
*        NDG group identifier. Not used if the "data" struct contains
*        an NDF identifier.
*     index = int (Given)
*        Index corresponding to required file in group of Grp is used.
*     ondf = int (Given)
*        The output NDF identifier. NDF__NOID may be supplied if and only
*        if a non-NULL value is supplied for "modprov".
*     creator = const char * (Given)
*        String to associate with the provenance entry. Usually SMURF:TASKNAME
*     modprov = NdgProvenance ** (Given & Returned)
*        If "modprov" is NULL, any existing provenance is read from the
*        supplied output NDF, updated to include the input NDF as an ancestor,
*        and then immediately written back to the output NDF. The local
*        provenance structure is then freed.
*
*        If "modprov" is non-NULL but "*modprov" is NULL, any existing
*        provenance is read from the supplied output NDF. If no output
*        NDF is supplied, a new empty provenance structure is created to
*        store the output provenance. Either way, the output provenance is
*        updated to include the input NDF as an ancestor, but it is not
*        written back to the output NDF, or freed. Instead, a pointer to
*        the output provenance structure is returned in "*modprov".
*
*        If both "modprov" and "*modprov" are non-NULL, any supplied
*        output NDF identifier is ignored. Instead, the supplied provenance
*        structure pointed to by "*modprov" is updated to include the
*        input NDF as an ancestor.
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function incrementally updates the provenance information in
*     an output file based on an input provenance. It is a small wrapper
*     around smf_updateprov because when raw data are read using sc2store
*     functions the input NDF identifier is not present in the smfData
*     struct and so the input file must be reopened. If the smfData contains
*     an NDF identifier the input file will not be reopened.

*  Notes:
*     - If an external provenance pointer is provided the caller
*     is responsible for writing the provenance to the output file and
*     freeing the structure.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-07-05 (TIMJ):
*        Initial version.
*     2007-07-06 (TIMJ):
*        Strip path from filename
*     2008-05-28 (TIMJ):
*        Now a wrapper around smf_updateprov
*     2010-10-15 (TIMJ):
*        Add modprov parameter
*     2011-31-01 (DSB):
*        Update docs for modprov parameter.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2011 Science and Technology Facilities Council.
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
#include "ndf.h"
#include "star/ndg.h"
#include "sae_par.h"

void
smf_accumulate_prov( const smfData * data, const Grp* igrp, dim_t index,
                     int ondf, const char *creator, NdgProvenance ** modprov,
                     int * status ) {

  int indf = NDF__NOID;  /* input NDF identifier */
  int opened = 0;  /* We had to open the file */

  if (*status != SAI__OK) return;

    /* Propagate provenance to the output file - need access to the
       input file again if we read from raw data. Must use updateprov
       to prevent all the extensions being listed in provenance via
       sc2store */
  if (data && data->file && data->file->ndfid != NDF__NOID) {
    indf = data->file->ndfid;
  } else {
    ndgNdfas( igrp, index, "READ", &indf, status );
    opened = 1;
  }

  smf_updateprov( ondf, data, indf, creator, modprov, status );

  if (opened) {
    ndfAnnul( &indf, status );
  }

  return;
}
