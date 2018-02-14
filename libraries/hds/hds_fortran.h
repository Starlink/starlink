#if !defined( HDS_FORTRAN_INCLUDED ) /* hds_fortran.h already included? */
#define HDS_FORTRAN_INCLUDED 1
/*
*+
*  Name:
*     hds_fortran.h

*  Type of Module:
*     C include file.

*  Purpose:
*     Define Fortran wrapper helper routines

*  Description:
*     This file defines the prototypes for functions that should be
*     used by Fortran/C wrapper routines to export/import HDS locators
*     in the format suitable for Fortran.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  Notes:
*     These routines are not intended for general use.

*  History:
*     18-NOV-2005 (TIMJ):
*        Original version

*-
*/

#include <limits.h>

#include "dat_par.h"

/* Relative location of type definitions depends on whether we are
   building the library or using the installed version */
#if HDS_INTERNAL_INCLUDES
#  include "hds_types.h"
#else
#  include "star/hds_types.h"
#endif

#include "f77.h"

/* This function converts an HDSLoc to a Fortran string buffer */

/* The optional freeing of memory is problematic since it stops
   us declaring clocator as const */

void datExportFloc ( HDSLoc **clocator, int free, int loc_length,
		     char flocator[DAT__SZLOC], int * status);

/* Convert a Fortran locator to a C HDSLoc struct. Memory is allocated
   by this routine that will be freed when the locator is annulled. */

void datImportFloc( const char flocator[DAT__SZLOC], int loc_length,
		    HDSLoc **clocator, int * status);

/* Convert an array of dimensions (hdsdim) to an array of fortran
   integer dimensions */
F77_INTEGER_TYPE *
hdsDimC2F( int ndim, const hdsdim dims[],
	   F77_INTEGER_TYPE fdims[DAT__MXDIM], int * status );

/* Convert an array of Fortran INTEGER dimensions into an array
   of hdsdim */

hdsdim *
hdsDimF2C( int ndim, const F77_INTEGER_TYPE fdims[],
	   hdsdim cdims[DAT__MXDIM], int * status );

/* Set message token for hdsdim integer. */

void dat1emsSetHdsdim( const char * token, hdsdim value );


/* Macro to convert a single hdsdim integer to a single
   Fortran integer, checking for truncation. Will set status
   to DAT__DTRNC if truncation occurs. There is no optimizaton
   for the case where the hdsdim is a signed int.
*/

#define HDSDIM2INT( subname, cint, fint, status )			\
  if ( cint > (hdsdim)INT_MAX ) {					\
    fint = 0;								\
    if (*status == SAI__OK) {						\
      *status = DAT__DTRNC;						\
      dat1emsSetHdsdim( "DIM", cint );					\
      emsRep( " ",							\
	      subname							\
	      "Dimension truncated when exporting to Fortran int (^DIM)", \
	      status );							\
    }									\
  } else {								\
    fint = (F77_INTEGER_TYPE)cint;					\
  }


/* There are also a drop in replacement for the CNF Locator macros,
   although inherited status has been added as an argument */

/* Convert a C locator to a Fortran locator
   Note that we do not free the memory assoicated with the locator
   and we assume DAT__SZLOC for the Fortran locator */

/* This really should be able to take a const pointer but currently
   datExportFloc can either free or not free so it can't be const */

#define HDS_EXPORT_CLOCATOR( cloc, floc, status )	\
  datExportFloc( (HDSLoc**)&cloc, 0, DAT__SZLOC, floc, status )

/* Convert a Fortran locator to a C locator */

#define HDS_IMPORT_FLOCATOR( floc, cloc, status ) \
  datImportFloc( floc, DAT__SZLOC, cloc, status )

#endif /* _INCLUDED */

