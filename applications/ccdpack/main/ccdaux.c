#include "cnf.h"
#include "dat_par.h"
#include "sae_par.h"
#include "ccdaux.h"

F77_SUBROUTINE(ccd1_mall)( INTEGER(QUAN), CHARACTER(TYPE),
                           POINTER(POINT), INTEGER(STATUS)
                           TRAIL(TYPE) );

   void *ccdMall( const char *type, int size, int *status ) {
/*
*+
*  Name:
*     ccdMall

*  Purpose:
*     C wrapper for fortran CCD1_MALL routine.

*  Language:
*     Starlink C

*  Arguments:
*     type = const char *
*        HDS type of memory to allocate, as a null-terminated string.
*     size = int
*        Number of elements of type type to allocate.
*     status = int
*        The global status.

*  Return Value:
*     A pointer to a block of memory which will hold size elements of
*     type type.  This pointer has been registered with the CCDPACK
*     memory allocation machinery (and a fortiori the CNF memory
*     allocation machinery) and so must be deallocated using CCD1_MFREE.
*     The pointer returned is a C pointer, and thus suitable for direct
*     use by C code.  If it is to be used by Fortran code it must
*     be processed with the function cnfFptr.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
      DECLARE_CHARACTER( ftype, DAT__SZTYP );
      F77_POINTER_TYPE ptr;

      if ( *status != SAI__OK ) return (void *) NULL;

      cnfExprt( type, ftype, DAT__SZTYP );
      F77_CALL(ccd1_mall)( INTEGER_ARG(&size), CHARACTER_ARG(ftype),
                           POINTER_ARG(&ptr), INTEGER_ARG(status)
                           TRAIL_ARG(ftype) );
      return cnfCptr( ptr );
   }


/* $Id$ */
