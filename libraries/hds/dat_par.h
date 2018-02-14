#if !defined( DAT_PAR_INCLUDED ) /* dat_par.h already included? */
#define DAT_PAR_INCLUDED 1
/*
*+
*  Name:
*     dat_par.h

*  Purpose:
*     Define public global constants for the dat_ and hds_ routines.

*  Language:
*     ANSI C

*  Type of Module:
*     Global constants (macro) include file.

*  Description:
*     This file contains macro definitions for global constants which
*     are used by the dat_ and hds_ routines within the HDS package and
*     which may also be needed by software which calls these routines.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     Generated automatically by the dat_par_h program.
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*-
*/

/* Remove earlier definitions if clash with private dat1.h version */
#ifdef DAT__MXDIM
#undef DAT__MXDIM
#endif
#ifdef DAT__NOLOC
#undef DAT__NOLOC
#endif
#ifdef DAT__NOWLD
#undef DAT__NOWLD
#endif
#ifdef DAT__ROOT
#undef DAT__ROOT
#endif
#ifdef DAT__SZGRP
#undef DAT__SZGRP
#endif
#ifdef DAT__SZLOC
#undef DAT__SZLOC
#endif
#ifdef DAT__SZMOD
#undef DAT__SZMOD
#endif
#ifdef DAT__SZNAM
#undef DAT__SZNAM
#endif
#ifdef DAT__SZTYP
#undef DAT__SZTYP
#endif
#ifdef DAT__FLEXT
#undef DAT__FLEXT
#endif
#ifdef DAT__SZFLX
#undef DAT__SZFLX
#endif

/* Global Constants: */

#define DAT__MXDIM 7            /* Maximum number of object dimensions  */
#define DAT__NOLOC "<NOT A LOCATOR> " /* Null (invalid) locator value */
#define DAT__NOWLD 0            /* Null wild-card search context */
#define DAT__ROOT "<ROOT LOCATOR>  " /* Root locator value */
#define DAT__SZGRP 15            /* Size of group name */
#define DAT__SZLOC 16            /* Size of locator */
#define DAT__SZMOD 15            /* Size of access mode string */
#define DAT__SZNAM 15            /* Size of object name */
#define DAT__SZTYP 15            /* Size of type string */
#define DAT__SZFLX 4            /* Size of file extension DAT__FLEXT */
#define DAT__FLEXT ".sdf" /* Default HDS file extension */

/*. */
#endif
