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
*     <COPYRIGHT_STATEMENT>

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
                            char ** );
const char *astUnitLabel_( const char * );
double astUnitAnalyser_( const char *, double[9] );
const char *astUnitNormaliser_( const char * );
#endif

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */

#if defined(astCLASS)            /* Protected */
#define astUnitMapper(in,out,inlab,outlab) astINVOKE(O,astUnitMapper_(in,out,inlab,outlab))
#define astUnitAnalyser(in,powers) astUnitAnalyser_(in,powers) 
#define astUnitNormaliser(in) astUnitNormaliser_(in) 
#define astUnitLabel(sym) astINVOKE(O,astUnitLabel_(sym))
#endif
#endif
