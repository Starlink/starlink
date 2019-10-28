#if !defined( IRQ_INCLUDED )  /* Include this file only once */
#define IRQ_INCLUDED
/*
*+
*  Name:
*     irq.h

*  Purpose:
*     Define the C interface to the IRQ library.

*  Description:
*     This module defines the C interface to the functions of the IRQ
*     library. The file irq.c contains C wrappers for the Fortran IRQ
*     routines.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David .S. Berry
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     2009 August 24 (MJC):
*        Original version based upon DSB's kaplibs.h.
*     2010-06-17 (TIMJ):
*        Add irqNxtqn and irqNumqn
*     {enter_changes_here}

*-
*/

#include "ast.h"
#include "star/grp.h"
#include "star/hds.h"
#include "star/hds_fortran.h"

/* Macros */
/* ====== */

/* An illegal IRQ_ identifier value. This value can sometimes be
   specified by an application in place of an IRQ_ identifier in order
   to supress some operation. */
#define IRQ__NOID 0

/* The name of the structure holding the quality names information.  */
#define IRQ__QINAM QUALITY_NAMES

/* The type of the structure holding the quality names information. */
#define IRQ__QITYP QUALITY_NAMES

/* Maximum length of descriptive comments stored with each quality name. */
#define IRQ__SZCOM 50

/* Maximum length of a quality expression. */
#define IRQ__SZQEX 255

/* Maximum length of a quality name. */
#define IRQ__SZQNM 15



/* Type definitions */
/* ================ */

/* A structure used to pass a group of five HDS locators to and from IRQ
   functions. */

typedef struct IRQLocs {
   HDSLoc *loc[ 5 ];
} IRQLocs;

/* Just hide the integer-ness of the context variable */
typedef int IRQcntxt;


/* Prototypes for public functions */
/* =============================== */

void irqAddqn( const IRQLocs *, const char *, int, const char *, int * );
void irqDelet( int, int * );
void irqFind( int, IRQLocs **, char[DAT__SZNAM + 1], int * );
void irqGetqn( const IRQLocs *, const char *, int *, int *, int *, char *, int, int * );
void irqNew( int, const char *, IRQLocs **, int * );
void irqRbit( const IRQLocs *, const char *, int *, int * );
void irqRlse( IRQLocs **, int * );
void irqRwqn( const IRQLocs *, const char *, int, int, int *, int * );
void irqSetqm( const IRQLocs *, int, const char *, int, float *, int *, int * );
void irqSetqm8( const IRQLocs *, int, const char *, int64_t, float *, int64_t *, int * );
void irqFxbit( const IRQLocs *, const char *, int, int *, int * );
void irqNxtqn( const IRQLocs *locs, IRQcntxt *contxt, char *qname, int *fixed, int *value,
               int *bit, char *commnt, int commnt_len, int *done, int *status );
int irqNumqn( const IRQLocs *locs, int *status );

#endif
