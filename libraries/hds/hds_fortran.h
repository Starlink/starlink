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

void datExportFloc ( HDSLoc **clocator, int free, int loc_length, 
		     char flocator[DAT__SZLOC], int * status);

#endif /* _INCLUDED */

