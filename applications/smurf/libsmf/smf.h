/*
*+
*  Name:
*     smf.h

*  Purpose:
*     Prototypes for the libsmf library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "smf.h"

*  Description:
*     Prototypes used by the libsmf functions.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     2005-11-04 (AGG):
*        Add smf_open_file, smf_fits_rdhead and smf_fits_crchan
*     2005-11-07 (TIMJ):
*        Alphabetize.
*        Add smf_tslice_ast, add smf_fits_getI
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef SMF_DEFINED
#define SMF_DEFINED

#include "ast.h"
#include "smurf_typ.h"
#include "star/grp.h"
#include "smf_typ.h"

void smf_correct_extinction(AstFrameSet *, const dim_t[], 
			    float, float *, int *);

void smf_fits_crchan( int nfits, char * headrec, AstFitsChan ** fits, int *status);

/* Do not return the result since we want the interface to remain the same when a
   string is required. If we return a string we have to know who should free it */
void smf_fits_getI( const smfHead * hdr, const char * cardname, int * result, int * status );
void smf_fits_getD( const smfHead * hdr, const char * cardname, double * result, int * status );
void smf_fits_getS( const smfHead * hdr, const char * cardname, char result[70], int * status );

void smf_fits_rdhead( int indf, AstFitsChan ** fchan, int *status);

void smf_open_file( Grp * igrp, int index, char * mode, smfData ** data, int *status);

void smf_tslice_ast (smfData * data, int index, int * status );

#endif /* SMF_DEFINED */
