/*
*+

*  Name:
*     PAR_PAR

*  Purpose:
*     Defines the PAR_ global constants.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     INCLUDE FILE

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran include file par_par by the Perl script fchead.
*     {enter_new_authors_here}

*  History:
*     10-Jun-1998 (fhead):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef PAR_PAR_DEFINED
#define PAR_PAR_DEFINED

/*  Global Constants: */
/*   Size of PAR Name */
#define PAR__SZNAM 15 
/*   Size of PAR Type */
#define PAR__SZTYP 15 
/*   Size of PAR Mode */
#define PAR__SZMOD 15 
/*   Maximum No. of dimensions */
#define PAR__MXDIM 7 

/*  PAR states.  For historical reasons these must have the same */
/*  values as their SUBPAR counterparts. */
/*   Ground state */
#define PAR__GROUND 0 
/*   Active state */
#define PAR__ACTIVE 1 
/*   Cancelled state */
#define PAR__CANCEL 2 
/*   Null state */
#define PAR__NULLST 3 

#endif  /* PAR_PAR_DEFINED */

/*. */
