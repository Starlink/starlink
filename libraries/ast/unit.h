#if !defined( UNIT_INCLUDED )  /* Include this file only once */
#define UNIT_INCLUDED
/*
*+
*  Name:
*     unit.h

*  Purpose:
*     Define the interface to the Unit module.

*  Description:
*     This module defines functions which identify units and transform
*     between them.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Functions Defined:
*     Public:
*        None.
*
*     Protected:

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     10-DEC-2002 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
#include "mapping.h"             /* Coordinate mappings */

/* C header files. */
/* --------------- */

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS)            /* Protected  */
AstMapping *astUnitMapper_( const char *, const char *, const char *,
                            char **, int * );
const char *astUnitLabel_( const char *, int * );
double astUnitAnalyser_( const char *, double[9], int * );
const char *astUnitNormaliser_( const char *, int * );
#endif

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */

#if defined(astCLASS)            /* Protected */
#define astUnitMapper(in,out,inlab,outlab) astINVOKE(O,astUnitMapper_(in,out,inlab,outlab,STATUS_PTR))
#define astUnitAnalyser(in,powers) astUnitAnalyser_(in,powers,STATUS_PTR)
#define astUnitNormaliser(in) astUnitNormaliser_(in,STATUS_PTR)
#define astUnitLabel(sym) astINVOKE(O,astUnitLabel_(sym,STATUS_PTR))
#endif
#endif



