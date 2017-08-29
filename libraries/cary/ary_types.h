#if !defined( ARY_TYPES_INCLUDED )   /* Protect against multiple inclusion*/
#define ARY_TYPES_INCLUDED 1

/*
*  Name:
*     ary_types.h

*  Purpose:
*     Defines public data types and constants used by ARY.

*  Description:
*     This file defines all the public data types and constants
*     used within the C version of ARY.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     20-JUN-2017 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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

*/

/* Maximum number of array dimensions. */
#define ARY__MXDIM 7

/* Maximum size of a string describing an array access type, e.g.
   'DELETE'. */
#define ARY__SZACC 6

/* Maximum length of a string describing an array's "form", e.g.
   'SIMPLE'. */
#define ARY__SZFRM 10

/* Maximum length of a string describing the full type of an array
   (including whether it is complex), e.g. 'COMPLEX_REAL'. */
#define ARY__SZFTP 20

/* Maximum length of a string describing the "mapping mode" used to map
   an array for access, e.g. 'WRITE/ZERO'. */
#define ARY__SZMMD 11

/* Maximum length of a string describing an array's numeric type, e.g.
   '_INTEGER'. */
#define ARY__SZTYP 8

/* An opaque data type used by public identifiers for ACB objects. Each
   ACB object has an associated integer identifier value. This ID value
   is used directly by the F77 API, but is converted to a pointer for
   use by the C API. This conversion is done by copying the bit pattern of
   the integer value and treating it as a pointer to an object of the
   following "Ary" type. In general, the resulting pointer will not be a
   valid C pointer (i.e. attemting to dereference it will cause an error). */
typedef struct Ary {
   void *dummy;
} Ary;

/* An opaque data type used by public identifiers for PCB objects. Each
   PCB object has an associated integer identifier value. This ID is
   value used directly by the F77 API, but is converted to a pointer for
   use by the C API. This conversion is done by copying the bit pattern of
   the integer value and treating it as a pointer to an object of the
   following "AryPlace" type. In general, the resulting pointer will not be a
   valid C pointer (i.e. attemting to dereference it will cause an error). */
typedef struct AryPlace {
   void *dummy;
} AryPlace;

#endif
