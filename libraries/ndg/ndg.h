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
*     2010-05-05 (TIMJ):
*        Add ndgAddgh

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

/* Need Grp type definitions */
#include "star/grp.h"

/* Need AST type definitions. */
#include "ast.h"

/* Need HDS dim type */
#include "star/hds_types.h"

/* Type Definitions: */
/* ----------------- */
typedef AstKeyMap NdgProvenance;

/* Public function prototypes */
/* -------------------------- */
AstKeyMap *ndgGetProv( NdgProvenance *prov, int ianc, int *status );
AstKeyMap *ndgRootProv( NdgProvenance *prov, int *status );
Grp *ndgCopy( const Grp *grp1, size_t indxlo, size_t indxhi, int reject, int *status );
NdgProvenance *ndgCopyProv( NdgProvenance *prov, int clense, int *status );
NdgProvenance *ndgFreeProv( NdgProvenance *prov, int *status );
NdgProvenance *ndgReadProv( int indf, const char *creator, int *status );
NdgProvenance *ndgReadVotProv( const char *xml, const char *path, const char *creator, int *status );
const char *ndgWriteVotProv( NdgProvenance *prov, int *status );
int ndgCountProv( NdgProvenance *prov, int *status );
int ndgIsHiddenProv( NdgProvenance *, int, int * );
void ndgAddgh( const char param[], const Grp * igrp, int * status );
void ndgAddProv( int indf, const char *creator, int nndf, int *ndfs, int, int *status );
void ndgAsexp( const char grpexp[], int verb, const Grp *igrp1, Grp **igrp2, size_t *size, int *flag, int *status );
void ndgAssoc( const char *param, int verb, Grp **igrp, size_t *size, int *flag, int *status );
void ndgBeggh( int *status );
void ndgBegpv( int *status );
void ndgCpsup( const Grp *igrp1, size_t i, Grp *igrp2, int * status );
void ndgCreat( const char *param, const Grp *igrp0, Grp **igrp, size_t *size, int *flag, int *status);
void ndgCrexp( const char grpexp[], const Grp *igrp0, Grp **igrp, size_t *size, int *flag, int *status );
void ndgEndgh( int *status );
void ndgEndpv( const char *creator, int *status );
void ndgFormatProv( NdgProvenance *prov, int base, AstKeyMap **keymap, int *status );
void ndgGtsup( const Grp *grp, size_t i, char *const fields[6], size_t len, int *status );
void ndgHideProv( NdgProvenance *, int, int * );
void ndgModifyProv( NdgProvenance *prov, int ianc, AstKeyMap *km, int *status );
void ndgNdfas( const Grp *igrp, size_t index, const char mode[], int *indf, int *status );
void ndgNdfcr( const Grp *igrp, size_t index, const char ftype[], int ndim, const int lbnd[], const int ubnd[], int *indf, int *status );
void ndgNdfpr( int indf1, const char clist[], const Grp *igrp, size_t index, int *indf2, int *status);
void ndgNdfco( int indf1, const Grp *igrp, size_t index, int *indf2, int *status);
void ndgPutProv( NdgProvenance *prov, int indf, AstKeyMap *more, int isroot, int *status );
void ndgRemoveProv( NdgProvenance *prov, int nanc, int *anc, int *status );
void ndgUnhashProv( NdgProvenance *, int * );
void ndgUnhideProv( NdgProvenance *, int, int * );
void ndgWriteProv( NdgProvenance *prov, int indf, int whdef, int *status );

AstXmlElement *ndgHds2vot( const HDSLoc *loc, AstXmlElement *elem, int *status );
AstXmlElement *ndgPutParam0( AstXmlElement *elem, const char *name, const char *datatype, const char *value, int *status );
AstXmlElement *ndgPutParam( AstXmlElement *elem, const char *name, int ndim, hdsdim *dim, const char *datatype, const char *values, int *status );
AstXmlElement *ndgPutGroup( AstXmlElement *elem, const char *name, int *status );
HDSLoc *ndgVot2hds( AstXmlElement *elem, HDSLoc *ploc, int *status );
const char *ndgGetAttrib( AstXmlElement *elem, const char *name, const char *method, int *status );
void ndgHwrgh( int indf, int *status );
void ndgHltgh( int new, int *old, int *status );
void ndgHltpv( int new, int *old, int *status );
void ndgMoreg( int indf, Grp **igrp, size_t *size, int *status );

#endif
