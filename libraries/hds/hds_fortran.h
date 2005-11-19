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



#include "dat_par.h"

/* Relative location of type definitions depends on whether we are
   building the library or using the installed version */
#if HDS_INTERNAL_INCLUDES
#  include "hds_types.h"
#else
#  include "star/hds_types.h"
#endif

/* This function converts an HDSLoc to a Fortran string buffer */

/* The optional freeing of memory is problematic since it stops
   us declaring clocator as const */

void datExportFloc ( HDSLoc **clocator, int free, int loc_length, 
		     char flocator[DAT__SZLOC], int * status);

/* Convert a Fortran locator to a C HDSLoc struct. Memory is allocated
   by this routine that will be freed when the locator is annulled. */

void datImportFloc( const char flocator[DAT__SZLOC], int loc_length, 
		    HDSLoc **clocator, int * status);


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

