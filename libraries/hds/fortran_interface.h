
/*
 *+
 *  Name:
 *    fortran_interface.h

 *  Purpose:
 *    Provide prototypes for Fortran->C interface

 *  Description:
 *    Prototypes of the Fortran HDS interface.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *    13-JUL-2005 (TIMJ):
 *      Initial version

 *  Notes:
 *    - Should not be used outside HDS. Only exists to fix compiler
 *      warnings as these functions are only ever accessed from Fortran.

 *  Copyright:
 *    Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

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

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include "f77.h"              /* F77 <-> C interface macros                  */
#include "cnf.h"              /* F77 <-> C string handling functions         */
#include "hds1_types.h"
#include "hds_types.h"
#include "hds.h"              /* HDS C interface			     */
#include "dat_par.h"          /* DAT__ constant definitions                  */
#include "ems.h"
#include "ems_par.h"
#include "dat_err.h"


/* Prototypes of Fortran interface  - not public */

F77_SUBROUTINE(dat_alter)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_annul)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_basic)( CHARACTER(locator),
                           CHARACTER(mode),
                           F77_POINTER_TYPE *pntr,
                           F77_INTEGER_TYPE *len,
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator)
                           TRAIL(mode) );

F77_SUBROUTINE(dat_ccopy)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           CHARACTER(name),
                           CHARACTER(locator3),
                           F77_INTEGER_TYPE *status
 	                   TRAIL(locator1)
                           TRAIL(locator2)
                           TRAIL(name)
                           TRAIL(locator3) );

F77_SUBROUTINE(dat_cctyp)( INTEGER(size), CHARACTER(type)
                           TRAIL(type) );

F77_SUBROUTINE(dat_cell)( CHARACTER(locator1),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE subs[],
                          CHARACTER(locator2),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(locator2) );

F77_SUBROUTINE(dat_chscn)( CHARACTER(name),
			   INTEGER(status)
			   TRAIL(name) );

F77_SUBROUTINE(dat_clen)( CHARACTER(locator),
                          F77_INTEGER_TYPE *clen,
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) );

F77_SUBROUTINE(dat_clone)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator1)
                           TRAIL(locator2) );

F77_SUBROUTINE(dat_coerc)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *ndim,
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator1)
                           TRAIL(locator2) );

F77_SUBROUTINE(dat_copy)( CHARACTER(locator1),
                          CHARACTER(locator2),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator1)
                          TRAIL(locator2)
                          TRAIL(name) );

F77_SUBROUTINE(dat_drep)( CHARACTER(locator),
                          CHARACTER(format),
                          CHARACTER(order),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator)
                          TRAIL(format)
                          TRAIL(order) );

F77_SUBROUTINE(dat_erase)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) );

F77_SUBROUTINE(dat_ermsg)( F77_INTEGER_TYPE *status,
			   F77_INTEGER_TYPE *length,
                           CHARACTER(msg)
                           TRAIL(msg) );

F77_SUBROUTINE(dat_find)( CHARACTER(locator1),
                          CHARACTER(name),
                          CHARACTER(locator2),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(name)
                          TRAIL(locator2) );

F77_SUBROUTINE(dat_get)( CHARACTER(locator),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_BYTE_TYPE values[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
			 TRAIL(values));

F77_SUBROUTINE(dat_getc)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(values),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(values)
			  );

F77_SUBROUTINE(dat_getd)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_DOUBLE_TYPE values[],
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_geti)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_getk)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER8_TYPE *values,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_getl)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_LOGICAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_getr)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_REAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

/* ================================== */
/*  datGet0x                          */
/* ================================== */

F77_SUBROUTINE(dat_get0c)( CHARACTER(locator),
			   CHARACTER(value),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(value) );

F77_SUBROUTINE(dat_get0d)( CHARACTER(locator),
			   DOUBLE(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get0r)( CHARACTER(locator),
			   REAL(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get0i)( CHARACTER(locator),
			   INTEGER(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get0k)( CHARACTER(locator),
			   INTEGER8(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get0l)( CHARACTER(locator),
			   LOGICAL(value),
			   INTEGER(status)
			   TRAIL(locator) );

/*=======================================*/
/* DAT_GET1x - Get 1D array values       */
/*=======================================*/

F77_SUBROUTINE(dat_get1c)( CHARACTER(locator),
			   INTEGER(maxval),
			   CHARACTER(values),
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(values) );

F77_SUBROUTINE(dat_get1d)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_DOUBLE_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get1i)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_INTEGER_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get1k)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_INTEGER8_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get1r)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_REAL_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_get1l)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_LOGICAL_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

/*=======================================*/
/* DAT_GETVx - Get vectorized array values       */
/*=======================================*/

F77_SUBROUTINE(dat_getvd)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_DOUBLE_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_getvi)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_INTEGER_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_getvk)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_INTEGER8_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_getvr)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_REAL_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_getvl)( CHARACTER(locator),
			   INTEGER(maxval),
			   F77_LOGICAL_TYPE *values,
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_getvc)( CHARACTER(locator),
			   INTEGER(maxval),
			   CHARACTER(values),
			   INTEGER(actval),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(values) );

F77_SUBROUTINE(dat_index)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *index,
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) );

F77_SUBROUTINE(dat_len)( CHARACTER(locator),
                         F77_INTEGER_TYPE *len,
                         F77_INTEGER_TYPE *status
                         TRAIL(locator) );

F77_SUBROUTINE(dat_map)( CHARACTER(locator),
                         CHARACTER(type),
                         CHARACTER(mode),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_POINTER_TYPE *pntr,
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
                         TRAIL(mode) );

F77_SUBROUTINE(dat_mapc)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapd)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapi)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapk)( CHARACTER(locator),
                           CHARACTER(mode),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_POINTER_TYPE *pntr,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(mode) );

F77_SUBROUTINE(dat_mapl)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapr)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapv)( CHARACTER(locator),
			  CHARACTER(type),
                          CHARACTER(mode),
                          F77_POINTER_TYPE *pntr,
			  F77_INTEGER_TYPE *actval,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(type)
                          TRAIL(mode) );

F77_SUBROUTINE(dat_mapn)( CHARACTER(locator),
                         CHARACTER(type),
                         CHARACTER(mode),
			 INTEGER(ndim),
                         F77_POINTER_TYPE *pntr,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
			 TRAIL(mode) );

F77_SUBROUTINE(dat_mould)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_move)( CHARACTER(locator1),
                          CHARACTER(locator2),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(locator2)
                          TRAIL(name) );

F77_SUBROUTINE(dat_msg)( CHARACTER(token), CHARACTER(locator)
			 TRAIL(token) TRAIL(locator) );

F77_SUBROUTINE(dat_name)( CHARACTER(locator),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(name) );

F77_SUBROUTINE(dat_ncomp)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ncomp,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_new)( CHARACTER(locator),
                         CHARACTER(name),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(name)
                         TRAIL(type) );

F77_SUBROUTINE(dat_newc)( CHARACTER(locator),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *len,
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(name) );

F77_SUBROUTINE(dat_new0)( CHARACTER(locator),
                          CHARACTER(name),
			  CHARACTER(type),
                          INTEGER(status)
                          TRAIL(locator)
                          TRAIL(name)
			  TRAIL(type) );

F77_SUBROUTINE(dat_new0d)( CHARACTER(locator),
                          CHARACTER(name),
                          INTEGER(status)
                          TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new0i)( CHARACTER(locator),
                          CHARACTER(name),
                          INTEGER(status)
                          TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new0k)( CHARACTER(locator),
                            CHARACTER(name),
                            INTEGER(status)
                            TRAIL(locator)
			    TRAIL(name) );

F77_SUBROUTINE(dat_new0r)( CHARACTER(locator),
                          CHARACTER(name),
                          INTEGER(status)
                          TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new0l)( CHARACTER(locator),
                          CHARACTER(name),
                          INTEGER(status)
                          TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new0c)( CHARACTER(locator),
                          CHARACTER(name),
			  INTEGER(len),
                          INTEGER(status)
                          TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new1)( CHARACTER(locator),
                          CHARACTER(name),
			  CHARACTER(type),
			  INTEGER(len),
                          INTEGER(status)
                          TRAIL(locator)
                          TRAIL(name)
			  TRAIL(type) );

F77_SUBROUTINE(dat_new1d)( CHARACTER(locator),
			   CHARACTER(name),
			   INTEGER(len),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new1i)( CHARACTER(locator),
			   CHARACTER(name),
			   INTEGER(len),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new1k)( CHARACTER(locator),
			    CHARACTER(name),
			    INTEGER(len),
			    INTEGER(status)
			    TRAIL(locator)
			    TRAIL(name) );

F77_SUBROUTINE(dat_new1l)( CHARACTER(locator),
			   CHARACTER(name),
			   INTEGER(len),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new1r)( CHARACTER(locator),
			   CHARACTER(name),
			   INTEGER(len),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_new1c)( CHARACTER(locator),
			   CHARACTER(name),
			   INTEGER(len),
			   INTEGER(nelem),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(name) );

F77_SUBROUTINE(dat_paren)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) );

F77_SUBROUTINE(dat_prec)( CHARACTER(locator),
			  INTEGER(nbytes),
			  INTEGER(status)
			  TRAIL(locator) );

F77_SUBROUTINE(dat_prim)( CHARACTER(locator),
                          F77_LOGICAL_TYPE *reply,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_prmry)( F77_LOGICAL_TYPE *set,
                           CHARACTER(locator),
                           F77_LOGICAL_TYPE *prmry,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_putc)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(values),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(values) );

F77_SUBROUTINE(dat_putd)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_DOUBLE_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_puti)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_putk)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER8_TYPE *values,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_putr)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_REAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_putl)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_LOGICAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_put)( CHARACTER(locator),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_BYTE_TYPE values[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
			 TRAIL(values)
			 );


F77_SUBROUTINE(dat_put1c)( CHARACTER(locator),
			   INTEGER(nval),
			   CHARACTER(values),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(values) );

F77_SUBROUTINE(dat_put1d)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_DOUBLE_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put1i)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_INTEGER_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put1k)( CHARACTER(locator),
			    INTEGER(nval),
			    F77_INTEGER8_TYPE *values,
			    INTEGER(status)
			    TRAIL(locator) );

F77_SUBROUTINE(dat_put1r)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_REAL_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put1l)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_LOGICAL_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_putvd)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_DOUBLE_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_putvi)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_INTEGER_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_putvk)( CHARACTER(locator),
			    INTEGER(nval),
			    F77_INTEGER8_TYPE *values,
			    INTEGER(status)
			    TRAIL(locator) );

F77_SUBROUTINE(dat_putvr)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_REAL_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_putvl)( CHARACTER(locator),
			   INTEGER(nval),
			   F77_LOGICAL_TYPE *values,
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_putvc)( CHARACTER(locator),
			   INTEGER(nval),
			   CHARACTER(values),
			   INTEGER(status)
			   TRAIL(locator)
			   TRAIL(values) );

F77_SUBROUTINE(dat_put0c)( CHARACTER(locator),
			   CHARACTER(value),
			   INTEGER(status)
			   TRAIL(locator) TRAIL(value) );

F77_SUBROUTINE(dat_put0d)( CHARACTER(locator),
			   DOUBLE(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put0r)( CHARACTER(locator),
			   REAL(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put0i)( CHARACTER(locator),
			   INTEGER(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_put0k)( CHARACTER(locator),
			    INTEGER8(value),
			    INTEGER(status)
			    TRAIL(locator) );

F77_SUBROUTINE(dat_put0l)( CHARACTER(locator),
			   LOGICAL(value),
			   INTEGER(status)
			   TRAIL(locator) );

F77_SUBROUTINE(dat_ref)( CHARACTER(locator),
			 CHARACTER(ref),
			 INTEGER(reflen),
			 INTEGER(status)
			 TRAIL(locator)
			 TRAIL(ref) );

F77_SUBROUTINE(dat_refct)( CHARACTER(locator),
                           F77_INTEGER_TYPE *refct,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_renam)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) );

F77_SUBROUTINE(dat_reset)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_retyp)( CHARACTER(locator),
                           CHARACTER(type),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(type) );

F77_SUBROUTINE(dat_shape)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndimx,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *ndim,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_size)( CHARACTER(locator),
                          F77_INTEGER_TYPE *size,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) );

F77_SUBROUTINE(dat_slice)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE diml[],
                           FORTRAN_INDEX_TYPE dimu[],
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) );

F77_SUBROUTINE(dat_state)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_struc)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_temp)( CHARACTER(type),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(locator),
                          F77_INTEGER_TYPE *status
                          TRAIL(type)
                          TRAIL(locator) );

F77_SUBROUTINE(dat_there)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) );

F77_SUBROUTINE(dat_type)( CHARACTER(locator),
                          CHARACTER(type),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(type) );

F77_SUBROUTINE(dat_unmap)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_valid)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(dat_vec)( CHARACTER(locator1),
                         CHARACTER(locator2),
                         F77_INTEGER_TYPE *status
                         TRAIL(locator1)
                         TRAIL(locator2) );

F77_SUBROUTINE(dat_where)( CHARACTER(locator),
                           F77_INTEGER_TYPE *block,
                           F77_INTEGER_TYPE *offset,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) );

F77_SUBROUTINE(hds_copy)( CHARACTER(locator),
                          CHARACTER(file),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(file)
                          TRAIL(name) );

F77_SUBROUTINE(hds_erase)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator) );

F77_SUBROUTINE(hds_ewild) ( F77_INTEGER_TYPE *iwld,
                            F77_INTEGER_TYPE *status );

F77_SUBROUTINE(hds_flush)( CHARACTER(group),
                           F77_INTEGER_TYPE *status
                           TRAIL(group) );

F77_SUBROUTINE(hds_free)( CHARACTER(locator),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) );

F77_SUBROUTINE(hds_group)( CHARACTER(locator),
                           CHARACTER(group),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(group) );

F77_SUBROUTINE(hds_gtune) ( CHARACTER(param_str),
                            F77_INTEGER_TYPE *value,
                            F77_INTEGER_TYPE *status
                            TRAIL(param_str) );

F77_SUBROUTINE(hds_link)( CHARACTER(locator),
                          CHARACTER(group),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator)
                          TRAIL(group) );

F77_SUBROUTINE(hds_lock)( CHARACTER(locator),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) );

F77_SUBROUTINE(hds_new)( CHARACTER(file),
                         CHARACTER(name),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         CHARACTER(locator),
                         F77_INTEGER_TYPE *status
                         TRAIL(file)
                         TRAIL(name)
                         TRAIL(type)
	                 TRAIL(locator) );

F77_SUBROUTINE(hds_open)( CHARACTER(file),
                          CHARACTER(mode),
                          CHARACTER(locator),
                          F77_INTEGER_TYPE *status
                          TRAIL(file)
                          TRAIL(mode)
	                  TRAIL(locator) );

F77_SUBROUTINE(hds_infoi)( CHARACTER(locator),
			   CHARACTER(topic),
			   CHARACTER(extra),
			   INTEGER(result),
			   F77_INTEGER_TYPE *status
			   TRAIL(locator) TRAIL(topic) TRAIL(extra) );

F77_SUBROUTINE(hds_show)( CHARACTER(topic),
                          F77_INTEGER_TYPE *status
                          TRAIL(topic) );

F77_SUBROUTINE(hds_state) (int *state,
                           int *status );

F77_SUBROUTINE(hds_stop) ( F77_INTEGER_TYPE *status );

F77_SUBROUTINE(hds_trace)( CHARACTER(locator),
			   F77_INTEGER_TYPE *nlev,
                           CHARACTER(path),
                           CHARACTER(file),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(path)
                           TRAIL(file) );

F77_SUBROUTINE(hds_tune) ( CHARACTER(param_str),
                           F77_INTEGER_TYPE *value,
                           F77_INTEGER_TYPE *status
                           TRAIL(param_str) );

F77_SUBROUTINE(hds_wild) ( CHARACTER(fspec),
                           CHARACTER(mode),
                           F77_INTEGER_TYPE *iwld,
                           CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(fspec)
                           TRAIL(mode)
                           TRAIL(locator) );

/*=================================================================*/
/*  Deprecated routines!                                           */
/*=================================================================*/

F77_SUBROUTINE(dat_conv)( CHARACTER(locator),
                          CHARACTER(type),
                          F77_LOGICAL_TYPE *reply,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(type));

F77_SUBROUTINE(hds_close)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator) );

/*==============================================*/
/* Obsolete routines that have no C counterpart */
/*==============================================*/

F77_SUBROUTINE(dat_rcera)( CHARACTER(locator), CHARACTER(cname), INTEGER(status)
                           TRAIL(locator) TRAIL(cname) );

F77_SUBROUTINE(dat_tune) ( CHARACTER(name),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(name) );

F77_SUBROUTINE(dat_rcopy)( CHARACTER(locator1),
                          CHARACTER(locator2),
                          CHARACTER(name),
                          INTEGER(status)
	                  TRAIL(locator1)
                          TRAIL(locator2)
			   TRAIL(name) );

F77_SUBROUTINE(dat_ertxt)(CHARACTER(text), INTEGER(status) TRAIL(text) );

F77_SUBROUTINE(dat_erdsc)( CHARACTER(locator), INTEGER(status) TRAIL(locator) );

F77_SUBROUTINE(dat_erdsn)( CHARACTER(locator), CHARACTER(cmp),
			   INTEGER(status) TRAIL(locator) TRAIL(cmp) );
