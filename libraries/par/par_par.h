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

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
