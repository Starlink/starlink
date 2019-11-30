/*
*+
*  Name:
*     par.c

*  Purpose:
*     C interface to fortran library.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, UClan)
*     {enter_authors_here}

*  History:
*     17-APR-2006 (TIMJ):
*         Add prolog.
*     15-MAY-2008 (DSB):
*         Add status checks on entry to avoid segmentation violations
*         caused by passing unchecked NULL pointers into RTL functions
*         such as strlen.
*     25-JUL-2008 (TIMJ):
*         Use F77_CREATE_EXPORT_CHARACTER to completely avoid strlen
*         null pointer risk.
*     16-APR-2009 (DSB):
*         Export maxval in the wrapper for parGet1c.
*     10-MAY-2011 (TIMJ):
*         Need to make sure that we return unmodified argument if status
*         is bad
*-
*/

#include <string.h>
#include "f77.h"
#include "par.h"
#include "sae_par.h"
F77_SUBROUTINE(par_cancl)( CHARACTER(param),
                           INTEGER(status)
                           TRAIL(param) );

void parCancl( const char *param,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param, fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_cancl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_choic)( CHARACTER(param),
                           CHARACTER(defaul),
                           CHARACTER(opts),
                           LOGICAL(null),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(defaul)
                           TRAIL(opts)
                           TRAIL(value) );

void parChoic( const char *param,
               const char *defaul,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fdefaul);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_LOGICAL(fnull);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(defaul,fdefaul);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_choic)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fdefaul),
                        CHARACTER_ARG(fopts),
                        LOGICAL_ARG(&fnull),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fdefaul)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fdefaul);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_choiv)( CHARACTER(param),
                           INTEGER(maxval),
                           CHARACTER(opts),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(opts)
                           TRAIL(values) );

void parChoiv( const char *param,
               int maxval,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_choiv)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        CHARACTER_ARG(fopts),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      factval);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def0c)( CHARACTER(param),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(value) );

void parDef0c( const char *param,
               const char *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0c)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def0d)( CHARACTER(param),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(param) );

void parDef0d( const char *param,
               double value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_DOUBLE(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0d)( CHARACTER_ARG(fparam),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def0i)( CHARACTER(param),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(param) );

void parDef0i( const char *param,
               int value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def0l)( CHARACTER(param),
                           LOGICAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parDef0l( const char *param,
               int value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_LOGICAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0l)( CHARACTER_ARG(fparam),
                        LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def0r)( CHARACTER(param),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parDef0r( const char *param,
               float value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_REAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0r)( CHARACTER_ARG(fparam),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1c)( CHARACTER(param),
                           INTEGER(nval),
                           CHARACTER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parDef1c( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nval);
   F77_EXPORT_CHARACTER_ARRAY_P(values,fvalues,fvalues_length,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1c)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1d)( CHARACTER(param),
                           INTEGER(nval),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parDef1d( const char *param,
               int nval,
               const double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nval);
   F77_EXPORT_DOUBLE_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1d)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1i)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parDef1i( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1l)( CHARACTER(param),
                           INTEGER(nval),
                           LOGICAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parDef1l( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nval);
   F77_EXPORT_LOGICAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1l)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_LOGICAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1r)( CHARACTER(param),
                           INTEGER(nval),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parDef1r( const char *param,
               int nval,
               const float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_REAL_ARRAY(fvalues,nval);
   F77_EXPORT_REAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1r)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defnc)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           CHARACTER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parDefnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nvalues);
   F77_EXPORT_CHARACTER_ARRAY_P(values,fvalues,fvalues_length,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defnc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_CHARACTER(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defnd)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           DOUBLE_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parDefnd( const char *param,
               int ndim,
               const int *maxd,
               const double *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvalues);
   F77_EXPORT_DOUBLE_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defnd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_DOUBLE(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defni)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parDefni( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvalues);
   F77_EXPORT_INTEGER_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defni)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_INTEGER(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defnl)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           LOGICAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parDefnl( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nvalues);
   F77_EXPORT_LOGICAL_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defnl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_LOGICAL(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defnr)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           REAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parDefnr( const char *param,
               int ndim,
               const int *maxd,
               const float *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_REAL_ARRAY(fvalues,nvalues);
   F77_EXPORT_REAL_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defnr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_REAL(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exacc)( CHARACTER(param),
                           INTEGER(nvals),
                           CHARACTER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parExacc( const char *param,
               int nvals,
               char *const *values,
               int values_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nvals);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exacc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      nvals);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exacd)( CHARACTER(param),
                           INTEGER(nvals),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parExacd( const char *param,
               int nvals,
               double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvals);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exacd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_DOUBLE_ARRAY(fvalues,values,nvals);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exaci)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parExaci( const char *param,
               int nvals,
               int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exaci)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exacl)( CHARACTER(param),
                           INTEGER(nvals),
                           LOGICAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parExacl( const char *param,
               int nvals,
               int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nvals);
   F77_ASSOC_LOGICAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exacl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_LOGICAL_ARRAY(fvalues,values,nvals);
   F77_FREE_LOGICAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exacr)( CHARACTER(param),
                           INTEGER(nvals),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parExacr( const char *param,
               int nvals,
               float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_REAL_ARRAY(fvalues,nvals);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exacr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_REAL_ARRAY(fvalues,values,nvals);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr0d)( CHARACTER(param),
                           DOUBLE(defaul),
                           DOUBLE(vmin),
                           DOUBLE(vmax),
                           LOGICAL(null),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr0d( const char *param,
               double defaul,
               double vmin,
               double vmax,
               int null,
               double *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fdefaul);
DECLARE_DOUBLE(fvmin);
DECLARE_DOUBLE(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_DOUBLE(defaul,fdefaul);
   F77_EXPORT_DOUBLE(vmin,fvmin);
   F77_EXPORT_DOUBLE(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr0d)( CHARACTER_ARG(fparam),
                        DOUBLE_ARG(&fdefaul),
                        DOUBLE_ARG(&fvmin),
                        DOUBLE_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_DOUBLE(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr0i)( CHARACTER(param),
                           INTEGER(defaul),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           LOGICAL(null),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr0i( const char *param,
               int defaul,
               int vmin,
               int vmax,
               int null,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fdefaul);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(defaul,fdefaul);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr0i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fdefaul),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr0r)( CHARACTER(param),
                           REAL(defaul),
                           REAL(vmin),
                           REAL(vmax),
                           LOGICAL(null),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr0r( const char *param,
               float defaul,
               float vmin,
               float vmax,
               int null,
               float *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fdefaul);
DECLARE_REAL(fvmin);
DECLARE_REAL(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_REAL(defaul,fdefaul);
   F77_EXPORT_REAL(vmin,fvmin);
   F77_EXPORT_REAL(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr0r)( CHARACTER_ARG(fparam),
                        REAL_ARG(&fdefaul),
                        REAL_ARG(&fvmin),
                        REAL_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_REAL(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr1d)( CHARACTER(param),
                           INTEGER(nvals),
                           DOUBLE_ARRAY(defaul),
                           DOUBLE(vmin),
                           DOUBLE(vmax),
                           LOGICAL(null),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr1d( const char *param,
               int nvals,
               const double *defaul,
               double vmin,
               double vmax,
               int null,
               double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_DOUBLE_ARRAY_DYN(fdefaul);
DECLARE_DOUBLE(fvmin);
DECLARE_DOUBLE(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_DOUBLE_ARRAY(fdefaul,nvals);
   F77_EXPORT_DOUBLE_ARRAY(defaul,fdefaul,nvals);
   F77_EXPORT_DOUBLE(vmin,fvmin);
   F77_EXPORT_DOUBLE(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvals);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr1d)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        DOUBLE_ARRAY_ARG(fdefaul),
                        DOUBLE_ARG(&fvmin),
                        DOUBLE_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fdefaul);
   F77_IMPORT_DOUBLE_ARRAY(fvalues,values,nvals);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr1i)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER_ARRAY(defaul),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           LOGICAL(null),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr1i( const char *param,
               int nvals,
               const int *defaul,
               int vmin,
               int vmax,
               int null,
               int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER_ARRAY_DYN(fdefaul);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER_ARRAY(fdefaul,nvals);
   F77_EXPORT_INTEGER_ARRAY(defaul,fdefaul,nvals);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr1i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER_ARRAY_ARG(fdefaul),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fdefaul);
   F77_IMPORT_INTEGER_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr1r)( CHARACTER(param),
                           INTEGER(nvals),
                           REAL_ARRAY(defaul),
                           REAL(vmin),
                           REAL(vmax),
                           LOGICAL(null),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr1r( const char *param,
               int nvals,
               const float *defaul,
               float vmin,
               float vmax,
               int null,
               float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_REAL_ARRAY_DYN(fdefaul);
DECLARE_REAL(fvmin);
DECLARE_REAL(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_REAL_ARRAY(fdefaul,nvals);
   F77_EXPORT_REAL_ARRAY(defaul,fdefaul,nvals);
   F77_EXPORT_REAL(vmin,fvmin);
   F77_EXPORT_REAL(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_REAL_ARRAY(fvalues,nvals);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr1r)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        REAL_ARRAY_ARG(fdefaul),
                        REAL_ARG(&fvmin),
                        REAL_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fdefaul);
   F77_IMPORT_REAL_ARRAY(fvalues,values,nvals);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdrvd)( CHARACTER(param),
                           INTEGER(maxval),
                           DOUBLE(vmin),
                           DOUBLE(vmax),
                           DOUBLE_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGdrvd( const char *param,
               int maxval,
               double vmin,
               double vmax,
               double *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_DOUBLE(fvmin);
DECLARE_DOUBLE(fvmax);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_DOUBLE(vmin,fvmin);
   F77_EXPORT_DOUBLE(vmax,fvmax);
   F77_CREATE_DOUBLE_ARRAY(fvalues,maxval);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdrvd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        DOUBLE_ARG(&fvmin),
                        DOUBLE_ARG(&fvmax),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_DOUBLE_ARRAY(fvalues,values,*actval);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdrvi)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           INTEGER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGdrvi( const char *param,
               int maxval,
               int vmin,
               int vmax,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_CREATE_INTEGER_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdrvi)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER_ARRAY(fvalues,values,*factval);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdrvr)( CHARACTER(param),
                           INTEGER(maxval),
                           REAL(vmin),
                           REAL(vmax),
                           REAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGdrvr( const char *param,
               int maxval,
               float vmin,
               float vmax,
               float *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_REAL(fvmin);
DECLARE_REAL(fvmax);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_REAL(vmin,fvmin);
   F77_EXPORT_REAL(vmax,fvmax);
   F77_CREATE_REAL_ARRAY(fvalues,maxval);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdrvr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        REAL_ARG(&fvmin),
                        REAL_ARG(&fvmax),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_REAL_ARRAY(fvalues,values,*actval);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_get0c)( CHARACTER(param),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(value) );

void parGet0c( const char *param,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0c)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   }
   F77_FREE_CHARACTER(fvalue);


   return;
}
F77_SUBROUTINE(par_get0d)( CHARACTER(param),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGet0d( const char *param,
               double *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0d)( CHARACTER_ARG(fparam),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_DOUBLE(fvalue,*value);
   }

   return;
}
F77_SUBROUTINE(par_get0i)( CHARACTER(param),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGet0i( const char *param,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(fvalue,*value);
   }

   return;
}
F77_SUBROUTINE(par_get0l)( CHARACTER(param),
                           LOGICAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGet0l( const char *param,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0l)( CHARACTER_ARG(fparam),
                        LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_LOGICAL(fvalue,*value);
   }

   return;
}
F77_SUBROUTINE(par_get0r)( CHARACTER(param),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGet0r( const char *param,
               float *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0r)( CHARACTER_ARG(fparam),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_REAL(fvalue,*value);
   }

   return;
}
F77_SUBROUTINE(par_get1c)( CHARACTER(param),
                           INTEGER(maxval),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parGet1c( const char *param,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1c)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
                                  *actval);
   }
   F77_FREE_CHARACTER(fvalues);

   return;
}
F77_SUBROUTINE(par_get1d)( CHARACTER(param),
                           INTEGER(maxval),
                           DOUBLE_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGet1d( const char *param,
               int maxval,
               double *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_DOUBLE_ARRAY(fvalues,maxval);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1d)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_DOUBLE_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_DOUBLE(fvalues);

   return;
}
F77_SUBROUTINE(par_get1i)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGet1i( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_INTEGER_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_INTEGER(fvalues);

   return;
}
F77_SUBROUTINE(par_get1l)( CHARACTER(param),
                           INTEGER(maxval),
                           LOGICAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGet1l( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_LOGICAL_ARRAY(fvalues,maxval);
   F77_ASSOC_LOGICAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1l)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_LOGICAL_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_LOGICAL(fvalues);

   return;
}
F77_SUBROUTINE(par_get1r)( CHARACTER(param),
                           INTEGER(maxval),
                           REAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGet1r( const char *param,
               int maxval,
               float *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_REAL_ARRAY(fvalues,maxval);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1r)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_REAL_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_REAL(fvalues);

   return;
}
F77_SUBROUTINE(par_getnc)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           CHARACTER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parGetnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getnc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
                                  nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_CHARACTER(fvalues);
   F77_FREE_INTEGER(factd);

   return;
}
F77_SUBROUTINE(par_getnd)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           DOUBLE_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parGetnd( const char *param,
               int ndim,
               const int *maxd,
               double *values,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvalues);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getnd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_DOUBLE_ARRAY(fvalues,values,nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_DOUBLE(fvalues);
   F77_FREE_INTEGER(factd);

   return;
}
F77_SUBROUTINE(par_getni)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parGetni( const char *param,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvalues);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getni)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER_ARRAY(fvalues,values,nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_INTEGER(fvalues);
   F77_FREE_INTEGER(factd);


   return;
}
F77_SUBROUTINE(par_getnl)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           LOGICAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parGetnl( const char *param,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nvalues);
   F77_ASSOC_LOGICAL_ARRAY(fvalues,values);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getnl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_LOGICAL_ARRAY(fvalues,values,nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_LOGICAL(fvalues);
   F77_FREE_INTEGER(factd);

   return;
}
F77_SUBROUTINE(par_getnr)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           REAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parGetnr( const char *param,
               int ndim,
               const int *maxd,
               float *values,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_REAL_ARRAY(fvalues,nvalues);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getnr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_REAL_ARRAY(fvalues,values,nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_REAL(fvalues);
   F77_FREE_INTEGER(factd);


   return;
}
F77_SUBROUTINE(par_getvc)( CHARACTER(param),
                           INTEGER(maxval),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parGetvc( const char *param,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
                                  *actval);
   }
   F77_FREE_CHARACTER(fvalues);

   return;
}
F77_SUBROUTINE(par_getvd)( CHARACTER(param),
                           INTEGER(maxval),
                           DOUBLE_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGetvd( const char *param,
               int maxval,
               double *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_DOUBLE_ARRAY(fvalues, maxval);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_DOUBLE_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_DOUBLE(fvalues);

   return;
}
F77_SUBROUTINE(par_getvi)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGetvi( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvi)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_INTEGER_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_INTEGER(fvalues);

   return;
}
F77_SUBROUTINE(par_getvl)( CHARACTER(param),
                           INTEGER(maxval),
                           LOGICAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGetvl( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_LOGICAL_ARRAY(fvalues,maxval);
   F77_ASSOC_LOGICAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_LOGICAL_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_LOGICAL(fvalues);

   return;
}
F77_SUBROUTINE(par_getvr)( CHARACTER(param),
                           INTEGER(maxval),
                           REAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGetvr( const char *param,
               int maxval,
               float *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_REAL_ARRAY(fvalues,maxval);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_REAL_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_REAL(fvalues);

   return;
}
F77_SUBROUTINE(par_geven)( CHARACTER(param),
                           INTEGER(defaul),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           LOGICAL(null),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGeven( const char *param,
               int defaul,
               int vmin,
               int vmax,
               int null,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fdefaul);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(defaul,fdefaul);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_geven)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fdefaul),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_godd)( CHARACTER(param),
                          INTEGER(defaul),
                          INTEGER(vmin),
                          INTEGER(vmax),
                          LOGICAL(null),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(param) );

void parGodd( const char *param,
              int defaul,
              int vmin,
              int vmax,
              int null,
              int *value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fdefaul);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(defaul,fdefaul);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_godd)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fdefaul),
                       INTEGER_ARG(&fvmin),
                       INTEGER_ARG(&fvmax),
                       LOGICAL_ARG(&fnull),
                       INTEGER_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grm1d)( CHARACTER(param),
                           INTEGER(nvals),
                           DOUBLE_ARRAY(defaul),
                           DOUBLE_ARRAY(vmin),
                           DOUBLE_ARRAY(vmax),
                           LOGICAL(null),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGrm1d( const char *param,
               int nvals,
               const double *defaul,
               const double *vmin,
               const double *vmax,
               int null,
               double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_DOUBLE_ARRAY_DYN(fdefaul);
DECLARE_DOUBLE_ARRAY_DYN(fvmin);
DECLARE_DOUBLE_ARRAY_DYN(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_DOUBLE_ARRAY(fdefaul,nvals);
   F77_EXPORT_DOUBLE_ARRAY(defaul,fdefaul,nvals);
   F77_CREATE_DOUBLE_ARRAY(fvmin,nvals);
   F77_EXPORT_DOUBLE_ARRAY(vmin,fvmin,nvals);
   F77_CREATE_DOUBLE_ARRAY(fvmax,nvals);
   F77_EXPORT_DOUBLE_ARRAY(vmax,fvmax,nvals);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvals);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grm1d)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        DOUBLE_ARRAY_ARG(fdefaul),
                        DOUBLE_ARRAY_ARG(fvmin),
                        DOUBLE_ARRAY_ARG(fvmax),
                        LOGICAL_ARG(&fnull),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fdefaul);
   F77_FREE_DOUBLE(fvmin);
   F77_FREE_DOUBLE(fvmax);
   F77_IMPORT_DOUBLE_ARRAY(fvalues,values,nvals);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grm1i)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER_ARRAY(defaul),
                           INTEGER_ARRAY(vmin),
                           INTEGER_ARRAY(vmax),
                           LOGICAL(null),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGrm1i( const char *param,
               int nvals,
               const int *defaul,
               const int *vmin,
               const int *vmax,
               int null,
               int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER_ARRAY_DYN(fdefaul);
DECLARE_INTEGER_ARRAY_DYN(fvmin);
DECLARE_INTEGER_ARRAY_DYN(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER_ARRAY(fdefaul,nvals);
   F77_EXPORT_INTEGER_ARRAY(defaul,fdefaul,nvals);
   F77_CREATE_INTEGER_ARRAY(fvmin,nvals);
   F77_EXPORT_INTEGER_ARRAY(vmin,fvmin,nvals);
   F77_CREATE_INTEGER_ARRAY(fvmax,nvals);
   F77_EXPORT_INTEGER_ARRAY(vmax,fvmax,nvals);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grm1i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER_ARRAY_ARG(fdefaul),
                        INTEGER_ARRAY_ARG(fvmin),
                        INTEGER_ARRAY_ARG(fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fdefaul);
   F77_FREE_INTEGER(fvmin);
   F77_FREE_INTEGER(fvmax);
   F77_IMPORT_INTEGER_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grm1r)( CHARACTER(param),
                           INTEGER(nvals),
                           REAL_ARRAY(defaul),
                           REAL_ARRAY(vmin),
                           REAL_ARRAY(vmax),
                           LOGICAL(null),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGrm1r( const char *param,
               int nvals,
               const float *defaul,
               const float *vmin,
               const float *vmax,
               int null,
               float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_REAL_ARRAY_DYN(fdefaul);
DECLARE_REAL_ARRAY_DYN(fvmin);
DECLARE_REAL_ARRAY_DYN(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_REAL_ARRAY(fdefaul,nvals);
   F77_EXPORT_REAL_ARRAY(defaul,fdefaul,nvals);
   F77_CREATE_REAL_ARRAY(fvmin,nvals);
   F77_EXPORT_REAL_ARRAY(vmin,fvmin,nvals);
   F77_CREATE_REAL_ARRAY(fvmax,nvals);
   F77_EXPORT_REAL_ARRAY(vmax,fvmax,nvals);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_REAL_ARRAY(fvalues,nvals);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grm1r)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        REAL_ARRAY_ARG(fdefaul),
                        REAL_ARRAY_ARG(fvmin),
                        REAL_ARRAY_ARG(fvmax),
                        LOGICAL_ARG(&fnull),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fdefaul);
   F77_FREE_REAL(fvmin);
   F77_FREE_REAL(fvmax);
   F77_IMPORT_REAL_ARRAY(fvalues,values,nvals);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grmvd)( CHARACTER(param),
                           INTEGER(maxval),
                           DOUBLE_ARRAY(vmin),
                           DOUBLE_ARRAY(vmax),
                           DOUBLE_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGrmvd( const char *param,
               int maxval,
               const double *vmin,
               const double *vmax,
               double *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_DOUBLE_ARRAY_DYN(fvmin);
DECLARE_DOUBLE_ARRAY_DYN(fvmax);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_DOUBLE_ARRAY(fvmin,maxval);
   F77_EXPORT_DOUBLE_ARRAY(vmin,fvmin,maxval);
   F77_CREATE_DOUBLE_ARRAY(fvmax,maxval);
   F77_EXPORT_DOUBLE_ARRAY(vmax,fvmax,maxval);
   F77_CREATE_DOUBLE_ARRAY(fvalues,maxval);
   F77_ASSOC_DOUBLE_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grmvd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        DOUBLE_ARRAY_ARG(fvmin),
                        DOUBLE_ARRAY_ARG(fvmax),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fvmin);
   F77_FREE_DOUBLE(fvmax);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_DOUBLE_ARRAY(fvalues,values,*actval);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grmvi)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER_ARRAY(vmin),
                           INTEGER_ARRAY(vmax),
                           INTEGER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGrmvi( const char *param,
               int maxval,
               const int *vmin,
               const int *vmax,
               int *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER_ARRAY_DYN(fvmin);
DECLARE_INTEGER_ARRAY_DYN(fvmax);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER_ARRAY(fvmin,maxval);
   F77_EXPORT_INTEGER_ARRAY(vmin,fvmin,maxval);
   F77_CREATE_INTEGER_ARRAY(fvmax,maxval);
   F77_EXPORT_INTEGER_ARRAY(vmax,fvmax,maxval);
   F77_CREATE_INTEGER_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grmvi)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER_ARRAY_ARG(fvmin),
                        INTEGER_ARRAY_ARG(fvmax),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fvmin);
   F77_FREE_INTEGER(fvmax);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER_ARRAY(fvalues,values,*actval);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grmvr)( CHARACTER(param),
                           INTEGER(maxval),
                           REAL_ARRAY(vmin),
                           REAL_ARRAY(vmax),
                           REAL_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGrmvr( const char *param,
               int maxval,
               const float *vmin,
               const float *vmax,
               float *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_REAL_ARRAY_DYN(fvmin);
DECLARE_REAL_ARRAY_DYN(fvmax);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_REAL_ARRAY(fvmin,maxval);
   F77_EXPORT_REAL_ARRAY(vmin,fvmin,maxval);
   F77_CREATE_REAL_ARRAY(fvmax,maxval);
   F77_EXPORT_REAL_ARRAY(vmax,fvmax,maxval);
   F77_CREATE_REAL_ARRAY(fvalues,maxval);
   F77_ASSOC_REAL_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grmvr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        REAL_ARRAY_ARG(fvmin),
                        REAL_ARRAY_ARG(fvmax),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fvmin);
   F77_FREE_REAL(fvmax);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_REAL_ARRAY(fvalues,values,*actval);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gtd0l)( CHARACTER(param),
                           LOGICAL(defaul),
                           LOGICAL(null),
                           LOGICAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGtd0l( const char *param,
               int defaul,
               int null,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_LOGICAL(fdefaul);
DECLARE_LOGICAL(fnull);
DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_LOGICAL(defaul,fdefaul);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gtd0l)( CHARACTER_ARG(fparam),
                        LOGICAL_ARG(&fdefaul),
                        LOGICAL_ARG(&fnull),
                        LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_LOGICAL(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_maxc)( CHARACTER(param),
                          CHARACTER(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(value) );

void parMaxc( const char *param,
              const char *value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_maxc)( CHARACTER_ARG(fparam),
                       CHARACTER_ARG(fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam)
                       TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_maxd)( CHARACTER(param),
                          DOUBLE(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMaxd( const char *param,
              double value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_DOUBLE(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_maxd)( CHARACTER_ARG(fparam),
                       DOUBLE_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_maxi)( CHARACTER(param),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMaxi( const char *param,
              int value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_maxi)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_maxr)( CHARACTER(param),
                          REAL(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMaxr( const char *param,
              float value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_REAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_maxr)( CHARACTER_ARG(fparam),
                       REAL_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_minc)( CHARACTER(param),
                          CHARACTER(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(value) );

void parMinc( const char *param,
              const char *value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_minc)( CHARACTER_ARG(fparam),
                       CHARACTER_ARG(fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam)
                       TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mind)( CHARACTER(param),
                          DOUBLE(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMind( const char *param,
              double value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_DOUBLE(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mind)( CHARACTER_ARG(fparam),
                       DOUBLE_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mini)( CHARACTER(param),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMini( const char *param,
              int value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mini)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_minr)( CHARACTER(param),
                          REAL(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMinr( const char *param,
              float value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_REAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_minr)( CHARACTER_ARG(fparam),
                       REAL_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mix0d)( CHARACTER(param),
                           CHARACTER(defaul),
                           DOUBLE(vmin),
                           DOUBLE(vmax),
                           CHARACTER(opts),
                           LOGICAL(null),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(defaul)
                           TRAIL(opts)
                           TRAIL(value) );

void parMix0d( const char *param,
               const char *defaul,
               double vmin,
               double vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fdefaul);
DECLARE_DOUBLE(fvmin);
DECLARE_DOUBLE(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_LOGICAL(fnull);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(defaul,fdefaul);
   F77_EXPORT_DOUBLE(vmin,fvmin);
   F77_EXPORT_DOUBLE(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mix0d)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fdefaul),
                        DOUBLE_ARG(&fvmin),
                        DOUBLE_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        LOGICAL_ARG(&fnull),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fdefaul)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fdefaul);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mix0i)( CHARACTER(param),
                           CHARACTER(defaul),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           CHARACTER(opts),
                           LOGICAL(null),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(defaul)
                           TRAIL(opts)
                           TRAIL(value) );

void parMix0i( const char *param,
               const char *defaul,
               int vmin,
               int vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fdefaul);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_LOGICAL(fnull);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(defaul,fdefaul);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mix0i)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fdefaul),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        LOGICAL_ARG(&fnull),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fdefaul)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fdefaul);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mix0r)( CHARACTER(param),
                           CHARACTER(defaul),
                           REAL(vmin),
                           REAL(vmax),
                           CHARACTER(opts),
                           LOGICAL(null),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(defaul)
                           TRAIL(opts)
                           TRAIL(value) );

void parMix0r( const char *param,
               const char *defaul,
               float vmin,
               float vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fdefaul);
DECLARE_REAL(fvmin);
DECLARE_REAL(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_LOGICAL(fnull);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(defaul,fdefaul);
   F77_EXPORT_REAL(vmin,fvmin);
   F77_EXPORT_REAL(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mix0r)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fdefaul),
                        REAL_ARG(&fvmin),
                        REAL_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        LOGICAL_ARG(&fnull),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fdefaul)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fdefaul);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mixvd)( CHARACTER(param),
                           INTEGER(maxval),
                           DOUBLE(vmin),
                           DOUBLE(vmax),
                           CHARACTER(opts),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(opts)
                           TRAIL(values) );

void parMixvd( const char *param,
               int maxval,
               double vmin,
               double vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_DOUBLE(fvmin);
DECLARE_DOUBLE(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_DOUBLE(vmin,fvmin);
   F77_EXPORT_DOUBLE(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*actval,factval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mixvd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        DOUBLE_ARG(&fvmin),
                        DOUBLE_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      *actval);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mixvi)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER(vmin),
                           INTEGER(vmax),
                           CHARACTER(opts),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(opts)
                           TRAIL(values) );

void parMixvi( const char *param,
               int maxval,
               int vmin,
               int vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER(fvmin);
DECLARE_INTEGER(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_INTEGER(vmin,fvmin);
   F77_EXPORT_INTEGER(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*actval,factval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mixvi)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER_ARG(&fvmin),
                        INTEGER_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      *actval);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mixvr)( CHARACTER(param),
                           INTEGER(maxval),
                           REAL(vmin),
                           REAL(vmax),
                           CHARACTER(opts),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(opts)
                           TRAIL(values) );

void parMixvr( const char *param,
               int maxval,
               float vmin,
               float vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_REAL(fvmin);
DECLARE_REAL(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_REAL(vmin,fvmin);
   F77_EXPORT_REAL(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*actval,factval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mixvr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        REAL_ARG(&fvmin),
                        REAL_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      *actval);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_promt)( CHARACTER(param),
                           CHARACTER(prompt),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(prompt) );

void parPromt( const char *param,
               const char *prompt,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fprompt);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(prompt,fprompt);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_promt)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fprompt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fprompt) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fprompt);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0c)( CHARACTER(param),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(value) );

void parPut0c( const char *param,
               const char *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0c)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0d)( CHARACTER(param),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(param) );

void parPut0d( const char *param,
               double value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_DOUBLE(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0d)( CHARACTER_ARG(fparam),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0i)( CHARACTER(param),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(param) );

void parPut0i( const char *param,
               int value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0l)( CHARACTER(param),
                           LOGICAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parPut0l( const char *param,
               int value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_LOGICAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0l)( CHARACTER_ARG(fparam),
                        LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0r)( CHARACTER(param),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(param) );

void parPut0r( const char *param,
               float value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_REAL(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0r)( CHARACTER_ARG(fparam),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1c)( CHARACTER(param),
                           INTEGER(nval),
                           CHARACTER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parPut1c( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nval);
   F77_EXPORT_CHARACTER_ARRAY_P(values,fvalues,fvalues_length,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1c)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1d)( CHARACTER(param),
                           INTEGER(nval),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPut1d( const char *param,
               int nval,
               const double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nval);
   F77_EXPORT_DOUBLE_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1d)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1i)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPut1i( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1i)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1l)( CHARACTER(param),
                           INTEGER(nval),
                           LOGICAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPut1l( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nval);
   F77_EXPORT_LOGICAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1l)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_LOGICAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1r)( CHARACTER(param),
                           INTEGER(nval),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPut1r( const char *param,
               int nval,
               const float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_REAL_ARRAY(fvalues,nval);
   F77_EXPORT_REAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1r)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putnc)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           CHARACTER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parPutnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nvalues);
   F77_EXPORT_CHARACTER_ARRAY_P(values,fvalues,fvalues_length,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putnc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_CHARACTER(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putnd)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           DOUBLE_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parPutnd( const char *param,
               int ndim,
               const int *maxd,
               const double *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nvalues);
   F77_EXPORT_DOUBLE_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putnd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_DOUBLE(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putni)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parPutni( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER_ARRAY(fvalues,nvalues);
   F77_EXPORT_INTEGER_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putni)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_INTEGER(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putnl)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           LOGICAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parPutnl( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nvalues);
   F77_EXPORT_LOGICAL_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putnl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_LOGICAL(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putnr)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           REAL_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parPutnr( const char *param,
               int ndim,
               const int *maxd,
               const float *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_REAL_ARRAY(fvalues,nvalues);
   F77_EXPORT_REAL_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putnr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_REAL(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvc)( CHARACTER(param),
                           INTEGER(nval),
                           CHARACTER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(values) );

void parPutvc( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,nval);
   F77_EXPORT_CHARACTER_ARRAY_P(values,fvalues,fvalues_length,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvc)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvd)( CHARACTER(param),
                           INTEGER(nval),
                           DOUBLE_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPutvd( const char *param,
               int nval,
               const double *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_DOUBLE_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_DOUBLE_ARRAY(fvalues,nval);
   F77_EXPORT_DOUBLE_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvd)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        DOUBLE_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_DOUBLE(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvi)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPutvi( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvi)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvl)( CHARACTER(param),
                           INTEGER(nval),
                           LOGICAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPutvl( const char *param,
               int nval,
               const int *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_LOGICAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_LOGICAL_ARRAY(fvalues,nval);
   F77_EXPORT_LOGICAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvl)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        LOGICAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_LOGICAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvr)( CHARACTER(param),
                           INTEGER(nval),
                           REAL_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPutvr( const char *param,
               int nval,
               const float *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_REAL_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_REAL_ARRAY(fvalues,nval);
   F77_EXPORT_REAL_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvr)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        REAL_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_REAL(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_state)( CHARACTER(param),
                           INTEGER(state),
                           INTEGER(status)
                           TRAIL(param) );

void parState( const char *param,
               int *state,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fstate);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_state)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fstate),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstate,*state);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_unset)( CHARACTER(param),
                           CHARACTER(which),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(which) );

void parUnset( const char *param,
               const char *which,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fwhich);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(which,fwhich);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_unset)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fwhich),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fwhich) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fwhich);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}



/*    ---------------   K (_INT64) functions -------------------- */


F77_SUBROUTINE(par_def0k)( CHARACTER(param),
                           INTEGER8(value),
                           INTEGER(status)
                           TRAIL(param) );

void parDef0k( const char *param,
               int64_t value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER8(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def0k)( CHARACTER_ARG(fparam),
                        INTEGER8_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_def1k)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parDef1k( const char *param,
               int nval,
               const int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER8_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_def1k)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_defnk)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER8_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parDefnk( const char *param,
               int ndim,
               const int *maxd,
               const int64_t *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvalues);
   F77_EXPORT_INTEGER8_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_defnk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_INTEGER8(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_exack)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parExack( const char *param,
               int nvals,
               int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_exack)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER8_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr0k)( CHARACTER(param),
                           INTEGER8(defaul),
                           INTEGER8(vmin),
                           INTEGER8(vmax),
                           LOGICAL(null),
                           INTEGER8(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr0k( const char *param,
               int64_t defaul,
               int64_t vmin,
               int64_t vmax,
               int null,
               int64_t *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fdefaul);
DECLARE_INTEGER8(fvmin);
DECLARE_INTEGER8(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER8(defaul,fdefaul);
   F77_EXPORT_INTEGER8(vmin,fvmin);
   F77_EXPORT_INTEGER8(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr0k)( CHARACTER_ARG(fparam),
                        INTEGER8_ARG(&fdefaul),
                        INTEGER8_ARG(&fvmin),
                        INTEGER8_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER8_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER8(fvalue,*value);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdr1k)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER8_ARRAY(defaul),
                           INTEGER8(vmin),
                           INTEGER8(vmax),
                           LOGICAL(null),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGdr1k( const char *param,
               int nvals,
               const int64_t *defaul,
               int64_t vmin,
               int64_t vmax,
               int null,
               int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER8_ARRAY_DYN(fdefaul);
DECLARE_INTEGER8(fvmin);
DECLARE_INTEGER8(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER8_ARRAY(fdefaul,nvals);
   F77_EXPORT_INTEGER8_ARRAY(defaul,fdefaul,nvals);
   F77_EXPORT_INTEGER8(vmin,fvmin);
   F77_EXPORT_INTEGER8(vmax,fvmax);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdr1k)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER8_ARRAY_ARG(fdefaul),
                        INTEGER8_ARG(&fvmin),
                        INTEGER8_ARG(&fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fdefaul);
   F77_IMPORT_INTEGER8_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_gdrvk)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER8(vmin),
                           INTEGER8(vmax),
                           INTEGER8_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGdrvk( const char *param,
               int maxval,
               int64_t vmin,
               int64_t vmax,
               int64_t *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER8(fvmin);
DECLARE_INTEGER8(fvmax);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_INTEGER8(vmin,fvmin);
   F77_EXPORT_INTEGER8(vmax,fvmax);
   F77_CREATE_INTEGER8_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_gdrvk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER8_ARG(&fvmin),
                        INTEGER8_ARG(&fvmax),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER8_ARRAY(fvalues,values,*factval);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_get0k)( CHARACTER(param),
                           INTEGER8(value),
                           INTEGER(status)
                           TRAIL(param) );

void parGet0k( const char *param,
               int64_t *value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get0k)( CHARACTER_ARG(fparam),
                        INTEGER8_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER8(fvalue,*value);
   }

   return;
}
F77_SUBROUTINE(par_get1k)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER8_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGet1k( const char *param,
               int maxval,
               int64_t *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_get1k)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_INTEGER8_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_INTEGER8(fvalues);

   return;
}
F77_SUBROUTINE(par_getnk)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER8_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parGetnk( const char *param,
               int ndim,
               const int *maxd,
               int64_t *values,
               int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for (i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvalues);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_ASSOC_INTEGER_ARRAY(factd,actd);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getnk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER8_ARRAY(fvalues,values,nvalues);
     F77_IMPORT_INTEGER_ARRAY(factd,actd,ndim);
   }
   F77_FREE_INTEGER8(fvalues);
   F77_FREE_INTEGER(factd);


   return;
}
F77_SUBROUTINE(par_getvk)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER8_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGetvk( const char *param,
               int maxval,
               int64_t *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_getvk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);
   if (*status == SAI__OK) {
     F77_IMPORT_INTEGER(factval,*actval);
     F77_IMPORT_INTEGER8_ARRAY(fvalues,values,*actval);
   }
   F77_FREE_INTEGER8(fvalues);

   return;
}
F77_SUBROUTINE(par_grm1k)( CHARACTER(param),
                           INTEGER(nvals),
                           INTEGER8_ARRAY(defaul),
                           INTEGER8_ARRAY(vmin),
                           INTEGER8_ARRAY(vmax),
                           LOGICAL(null),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parGrm1k( const char *param,
               int nvals,
               const int64_t *defaul,
               const int64_t *vmin,
               const int64_t *vmax,
               int null,
               int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnvals);
DECLARE_INTEGER8_ARRAY_DYN(fdefaul);
DECLARE_INTEGER8_ARRAY_DYN(fvmin);
DECLARE_INTEGER8_ARRAY_DYN(fvmax);
DECLARE_LOGICAL(fnull);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nvals,fnvals);
   F77_CREATE_INTEGER8_ARRAY(fdefaul,nvals);
   F77_EXPORT_INTEGER8_ARRAY(defaul,fdefaul,nvals);
   F77_CREATE_INTEGER8_ARRAY(fvmin,nvals);
   F77_EXPORT_INTEGER8_ARRAY(vmin,fvmin,nvals);
   F77_CREATE_INTEGER8_ARRAY(fvmax,nvals);
   F77_EXPORT_INTEGER8_ARRAY(vmax,fvmax,nvals);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvals);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grm1k)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnvals),
                        INTEGER8_ARRAY_ARG(fdefaul),
                        INTEGER8_ARRAY_ARG(fvmin),
                        INTEGER8_ARRAY_ARG(fvmax),
                        LOGICAL_ARG(&fnull),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fdefaul);
   F77_FREE_INTEGER8(fvmin);
   F77_FREE_INTEGER8(fvmax);
   F77_IMPORT_INTEGER8_ARRAY(fvalues,values,nvals);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_grmvk)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER8_ARRAY(vmin),
                           INTEGER8_ARRAY(vmax),
                           INTEGER8_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param) );

void parGrmvk( const char *param,
               int maxval,
               const int64_t *vmin,
               const int64_t *vmax,
               int64_t *values,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER8_ARRAY_DYN(fvmin);
DECLARE_INTEGER8_ARRAY_DYN(fvmax);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_CREATE_INTEGER8_ARRAY(fvmin,maxval);
   F77_EXPORT_INTEGER8_ARRAY(vmin,fvmin,maxval);
   F77_CREATE_INTEGER8_ARRAY(fvmax,maxval);
   F77_EXPORT_INTEGER8_ARRAY(vmax,fvmax,maxval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,maxval);
   F77_ASSOC_INTEGER8_ARRAY(fvalues,values);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_grmvk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER8_ARRAY_ARG(fvmin),
                        INTEGER8_ARRAY_ARG(fvmax),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fvmin);
   F77_FREE_INTEGER8(fvmax);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER8_ARRAY(fvalues,values,*actval);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_maxk)( CHARACTER(param),
                          INTEGER8(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMaxk( const char *param,
              int64_t value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER8(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_maxk)( CHARACTER_ARG(fparam),
                       INTEGER8_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mink)( CHARACTER(param),
                          INTEGER8(value),
                          INTEGER(status)
                          TRAIL(param) );

void parMink( const char *param,
              int64_t value,
              int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER8(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mink)( CHARACTER_ARG(fparam),
                       INTEGER8_ARG(&fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mix0k)( CHARACTER(param),
                           CHARACTER(defaul),
                           INTEGER8(vmin),
                           INTEGER8(vmax),
                           CHARACTER(opts),
                           LOGICAL(null),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(defaul)
                           TRAIL(opts)
                           TRAIL(value) );

void parMix0k( const char *param,
               const char *defaul,
               int64_t vmin,
               int64_t vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_CHARACTER_DYN(fdefaul);
DECLARE_INTEGER8(fvmin);
DECLARE_INTEGER8(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_LOGICAL(fnull);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_CREATE_EXPORT_CHARACTER(defaul,fdefaul);
   F77_EXPORT_INTEGER8(vmin,fvmin);
   F77_EXPORT_INTEGER8(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_EXPORT_LOGICAL(null,fnull);
   F77_CREATE_CHARACTER(fvalue,value_length-1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mix0k)( CHARACTER_ARG(fparam),
                        CHARACTER_ARG(fdefaul),
                        INTEGER8_ARG(&fvmin),
                        INTEGER8_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        LOGICAL_ARG(&fnull),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fdefaul)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fdefaul);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_CHARACTER(fvalue,fvalue_length,value);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_mixvk)( CHARACTER(param),
                           INTEGER(maxval),
                           INTEGER8(vmin),
                           INTEGER8(vmax),
                           CHARACTER(opts),
                           CHARACTER_ARRAY(values),
                           INTEGER(actval),
                           INTEGER(status)
                           TRAIL(param)
                           TRAIL(opts)
                           TRAIL(values) );

void parMixvk( const char *param,
               int maxval,
               int64_t vmin,
               int64_t vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fmaxval);
DECLARE_INTEGER8(fvmin);
DECLARE_INTEGER8(fvmax);
DECLARE_CHARACTER_DYN(fopts);
DECLARE_CHARACTER_ARRAY_DYN(fvalues);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(maxval,fmaxval);
   F77_EXPORT_INTEGER8(vmin,fvmin);
   F77_EXPORT_INTEGER8(vmax,fvmax);
   F77_CREATE_EXPORT_CHARACTER(opts,fopts);
   F77_CREATE_CHARACTER_ARRAY(fvalues,values_length-1,maxval);
   F77_EXPORT_INTEGER(*actval,factval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_mixvk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fmaxval),
                        INTEGER8_ARG(&fvmin),
                        INTEGER8_ARG(&fvmax),
                        CHARACTER_ARG(fopts),
                        CHARACTER_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&factval),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam)
                        TRAIL_ARG(fopts)
                        TRAIL_ARG(fvalues) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_CHARACTER(fopts);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_CHARACTER_ARRAY_P(fvalues,fvalues_length,values,values_length,
      *actval);
   F77_FREE_CHARACTER(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put0k)( CHARACTER(param),
                           INTEGER8(value),
                           INTEGER(status)
                           TRAIL(param) );

void parPut0k( const char *param,
               int64_t value,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER8(fvalue);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER8(value,fvalue);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put0k)( CHARACTER_ARG(fparam),
                        INTEGER8_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_put1k)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPut1k( const char *param,
               int nval,
               const int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER8_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_put1k)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putnk)( CHARACTER(param),
                           INTEGER(ndim),
                           INTEGER_ARRAY(maxd),
                           INTEGER8_ARRAY(values),
                           INTEGER_ARRAY(actd),
                           INTEGER(status)
                           TRAIL(param) );

void parPutnk( const char *param,
               int ndim,
               const int *maxd,
               const int64_t *values,
               const int *actd,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmaxd);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER_ARRAY_DYN(factd);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   if( *status != SAI__OK ) return;

   for(i=ndim,nvalues=1;i;i--) nvalues*=maxd[i-1];
   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fmaxd,ndim);
   F77_EXPORT_INTEGER_ARRAY(maxd,fmaxd,ndim);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nvalues);
   F77_EXPORT_INTEGER8_ARRAY(values,fvalues,nvalues);
   F77_CREATE_INTEGER_ARRAY(factd,ndim);
   F77_EXPORT_INTEGER_ARRAY(actd,factd,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putnk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmaxd),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARRAY_ARG(factd),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER(fmaxd);
   F77_FREE_INTEGER8(fvalues);
   F77_FREE_INTEGER(factd);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(par_putvk)( CHARACTER(param),
                           INTEGER(nval),
                           INTEGER8_ARRAY(values),
                           INTEGER(status)
                           TRAIL(param) );

void parPutvk( const char *param,
               int nval,
               const int64_t *values,
               int *status ) {

DECLARE_CHARACTER_DYN(fparam);
DECLARE_INTEGER(fnval);
DECLARE_INTEGER8_ARRAY_DYN(fvalues);
DECLARE_INTEGER(fstatus);

   if( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,fparam);
   F77_EXPORT_INTEGER(nval,fnval);
   F77_CREATE_INTEGER8_ARRAY(fvalues,nval);
   F77_EXPORT_INTEGER8_ARRAY(values,fvalues,nval);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_LOCK( F77_CALL(par_putvk)( CHARACTER_ARG(fparam),
                        INTEGER_ARG(&fnval),
                        INTEGER8_ARRAY_ARG(fvalues),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam) ); )

   F77_FREE_CHARACTER(fparam);
   F77_FREE_INTEGER8(fvalues);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}

