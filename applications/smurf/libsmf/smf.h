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
*     2005-11-28 (TIMJ):
*        Add smf_close_file
*     2005-12-05 (AGG)
*        Add smf_flatfield and smf_check_flat
*     2005-12-06 (AGG)
*        Add smf_flatten
*     2005-12-09 (AGG)
*        Add smf_clone_data
*     2006-01-09 (AGG)
*        Add smf_tslice and smf_insert_tslice
*     2006-01-10 (AGG)
*        Add smf_scale_tau and smf_fits_getF
*     2006-01-24 (TIMJ):
*        Add smf_fits_getS
*     2006-01-24 (AGG):
*        Change floats to doubles in smf_correction_extintion, smf_scale_tau
*     2006-01-25 (AGG):
*        Add smf_dtype_check_fatal
*     2006-01-25 (TIMJ):
*        Add smf_malloc, smf_free.
*        Remove smf_fits_rdhead
*     2006-01-25 (TIMJ):
*        Add smf_create_*
*        Add smf_construct_*
*        Add smf_dtype_tostring
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

void smf_check_flat ( const smfData *data, int *status );

void smf_clone_data ( const smfData *idata, smfData **odata, int *status );

void smf_close_file( smfData **, int *status);

void smf_correct_extinction( smfData *data, const char *method, double tau, int *status);

smfData* smf_create_smfData( int flags, int * status );
smfFile* smf_create_smfFile( int * status );
smfHead* smf_create_smfHead( int * status );
smfDA*   smf_create_smfDA( int * status );


smfData *
smf_construct_smfData( smfData * tofill, smfFile * file, smfHead * hdr, 
		       smfDA * da, smf_dtype dtype, void * pntr[3], 
		       const dim_t dims[], int ndims,
		       int virtual, int * status );
smfDA *
smf_construct_smfDA( smfDA * tofill, int *dksquid, double * flatcal,
		     double * flatpar, const char * flatname, int nflat,
		     int * status );
smfFile *
smf_construct_smfFile(smfFile * tofill, int ndfid, int isSc2store,
		      int isTstream, const char * name, HDSLoc* xloc, 
		      int * status );
smfHead *
smf_construct_smfHead( smfHead * tofill, sc2head * sc2head,
		       AstFrameSet * wcs, AstFitsChan * fitshdr,
		       dim_t curslice, int * status );

int smf_dtype_check( const smfData* data, const char * type, smf_dtype itype,
		     int *status );

void smf_dtype_check_fatal( const smfData* data, const char * type, smf_dtype itype,
		     int *status );

char * smf_dtype_string( const smfData* data, int * status );

size_t smf_dtype_size( const smfData* data, int * status );

smf_dtype
smf_dtype_tostring( const char * dtype, int * status );

/*void smf_extcorr( smfData *data, double tau, int *status);*/

void smf_fits_crchan( int nfits, char * headrec, AstFitsChan ** fits, int *status);

/* Do not return the result since we want the interface to remain the same when a
   string is required. If we return a string we have to know who should free it */
void smf_fits_getI( const smfHead * hdr, const char * cardname, int * result, int * status );
void smf_fits_getD( const smfHead * hdr, const char * cardname, double * result, int * status );
void smf_fits_getF( const smfHead * hdr, const char * cardname, float * result, int * status );
void smf_fits_getS( const smfHead * hdr, const char * cardname, char result[70], size_t len, int * status );

void smf_flatfield ( const smfData *idata, smfData **odata, int *status );

void smf_flatten ( smfData *data, int *status );

void smf_free( void * pntr, int * status );

int smf_history_check( const smfData* data, const char * appl, int *status);

void smf_history_write( const smfData* data, const char * appl, 
			const char * text, int *status);

void smf_insert_tslice ( smfData **idata, smfData *tdata, int index, int *status );

void * smf_malloc( size_t nelem, size_t bytes_per_elem, int zero, int * status );

void smf_open_and_flatfield ( Grp *igrp, Grp *ogrp, int index, 
			      smfData **ffdata, int *status);

void smf_open_file( Grp * igrp, int index, char * mode, int withHdr,
		    smfData ** data, int *status);
double smf_scale_tau ( const double tauwvm, const int filter, int *status);

void smf_tslice ( const smfData *idata, smfData **tdata, int index, int *status );

void smf_tslice_ast (smfData * data, int index, int * status );

#endif /* SMF_DEFINED */
