#if !defined ( STAR_MEM1_INCLUDED )   /* Include file only once */
#define STAR_MEM1_INCLUDED

/*
*  Name:
*     mem1.h

*  Purpose:
*     Private include file for starmem

*  Description:
*     This include file declares the private interface to the
*     starmem library.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     08-FEB-2006 (TIMJ):
*        Original version.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

/* Internal system header files */
#include <stdio.h>

#define USE_AST_MALLOC 0

#if USE_AST_MALLOC
#  include "ast.h"
#endif

/* If debugging set to true. Debugging messages controlled by STARMEM_PRINT_MALLOC
   environment variable if this is true. Will cause overhead. */
#define STARMEM_DEBUG 0

/* Prototypes for the Doug Lea Malloc implementation - DLMALLOC */
#define USE_DL_PREFIX
#include "dlmalloc.h"

/* Define the allowed malloc types */

typedef enum STARMEM_MALLOCS {
  STARMEM__NULL,
  STARMEM__SYSTEM,
  STARMEM__AST,
  STARMEM__DL,
  STARMEM__GC,
} STARMEM_MALLOCS;

/* State variables - set in mem1_globals.c */
extern STARMEM_MALLOCS STARMEM_MALLOC;
extern int STARMEM_INITIALISED;

#if STARMEM_DEBUG
/* Display debug messages */
extern int STARMEM_PRINT_MALLOC;
#endif

/* Display scarce information messages */
extern int STARMEM_PRINT_INFO;

/* Macro to simplify fatal abort */
#define starMemFatal( text ) fprintf(stderr, "starMem: Fatal error in " __FILE__ ": " text "\n"); abort();

#define starMemFatalGC starMemFatal( "GC requested but not available" )

#define starMemFatalNone starMemFatal( "Unable to determine malloc scheme.")

#define starMemFatalAST starMemFatal( "Unable to use AST library for malloc." )

/* STAR_MEM1_INCLUDED */
#endif
