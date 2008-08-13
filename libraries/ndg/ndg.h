#if !defined( NDG_ADAM_INCLUDED )  /* Include this file only once */
#define NDG_ADAM_INCLUDED
/*
*  Name:
*     ndg_adam.h

*  Purpose:
*     Define the ADAM C interface to the NDG library.

*  Description:
*     This module defines the C interface to the functions of the ADAM NDG
*     library. The file ndg_adam.c contains C wrappers for the Fortran 
*     GRP routines.

*  Notes:
*     - Given the size of the NDg library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use NDG from C extend this file (and
*     ndg_adam.c) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David .S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Copy from grp.h
*     20-DEC-2005 (TIMJ):
*        Add ndgAsexp
*     12-JUL-2006 (TIMJ):
*        Add ndgNdfcr
*     8-AUG-2006 (DSB):
*        Added ndgGtsup
*     2-NOV-2007 (DSB):
*        Added ndgPtprv, ndgBegpv and ndgEndpv.
*     4-JUL-2008 (TIMJ):
*        Add some sprinkling of const-ness.
*     15-JUL-2008 (TIMJ):
*        Use size_t for index to match new Grp interface.
*     13-AUG-2008 (DSB):
*        Added ndgCopy.

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007, 2008 Science & Technology Facilities Council.
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

/* Need Grp type definitions */
#include "star/grp.h"

/* Need AST type definitions. */
#include "ast.h"

/* Need HDS dim type */
#include "star/hds_types.h"

/* Public function prototypes */
/* -------------------------- */
void ndgAsexp( const char grpexp[], int verb, const Grp *igrp1, Grp **igrp2, size_t *size, int *flag, int *status );
void ndgAssoc( const char *param, int verb, Grp **igrp, size_t *size, int *flag, int *status );
void ndgCreat( const char *param, const Grp *igrp0, Grp **igrp, size_t *size, int *flag, int *status);
void ndgNdfas( const Grp *igrp, size_t index, const char mode[], int *indf, int *status );
void ndgNdfcr( const Grp *igrp, size_t index, const char ftype[], int ndim,
	       const hdsdim lbnd[], const hdsdim ubnd[], int *indf, int *status );
void ndgNdfpr( int indf1, const char clist[], const Grp *igrp, size_t index, int *indf2, int *status);
void ndgGtsup( const Grp *grp, size_t i, const char const *fields[6], size_t len, int *status );
void ndgCpsup( const Grp *igrp1, size_t i, Grp *igrp2, int * status );
void ndgBegpv( int *status );
void ndgEndpv( const char *creator, int *status );
void ndgPtprv( int indf1, int indf2, HDSLoc *more, int isroot, const char *creator, int *status );
void ndgCtprv( int indf, int *nanc, int *status );
void ndgGtprv( int indf, int ianc, HDSLoc **prov, int *status );
void ndgMdprv( int indf, int ianc, HDSLoc *prov, int *status );
void ndgRmprv( int indf, int ianc, int *status );
void ndgRtprv( int indf, AstKeyMap **roots, int *status );
void ndgFmprv( int indf, int base, AstKeyMap **keymap, int *status );
Grp *ndgCopy( const Grp *grp1, size_t indxlo, size_t indxhi, int reject,
	      int *status );

#endif
