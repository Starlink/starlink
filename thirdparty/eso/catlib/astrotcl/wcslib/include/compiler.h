/* static char sccsid[] = "@(#) ST-ECF os/h/compiler.h	4.1	10/16/92"; */
/* @(#)compiler.h	1.1.1.1 (ESO-IPG) 7/11/91 20:24:30 */
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.TYPE 		Header
.NAME 		compiler.h
.LANGUAGE 	C
.AUTHOR		Francois Ochsenbein [ESO-IPG]
.CATEGORY	Compiler-specfic Definitions
.COMMENTS 	This module includes constants depending on the compiler.
.ENVIRONMENT	
.VERSION 1.0	Created file
------------------------------------------------------------*/

#ifndef COMPILER_DEF

#define COMPILER_DEF	0

/*===========================================================================
 *             Define Here Your Specific Implementation
 *===========================================================================*/

#if 0				/* Example */
#define CC_ENV		_TURBOC
#define _TEMPLATES_	1	/* Function templates	*/
#endif				/* End of Example */

/*===========================================================================
 *             Function Templates Facilities
 *===========================================================================*/

/*===========================================================================
 *             List of supported Compilers
 *===========================================================================*/

#define _TURBOC		1

#define CC_TURBOC	(CC_ENV == _TURBOC)

#ifndef CC_ENV
/*===========================================================================
 *             Definition of Default Compiler
 *===========================================================================*/
#define CC_ENV	0
#endif


#ifndef _TEMPLATES_
/*===========================================================================
 *             Definition of Allowed  Function Templates
 *===========================================================================*/
# ifndef OSDEFOS_DEF
#include "osdefos.h"
# endif
#if OS_VMS
# define _TEMPLATES_	1
#else
# define _TEMPLATES_	0
#endif
#endif

#endif
