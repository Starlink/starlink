/*
*+
*  Name:
*     par.c

*  Purpose:
*     C prototypes for PAR library

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_authors_here}

*  History:
*     17-APR-2006 (TIMJ):
*         Add prolog.

*-
*/

#ifndef PARWRAP_DEFINED
#define PARWRAP_DEFINED

void parCancl( const char *param,
               int *status );

void parChoic( const char *param,
               const char *defaul,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status );

void parChoiv( const char *param,
               int maxval,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parDef0c( const char *param,
               const char *value,
               int *status );

void parDef0d( const char *param,
               double value,
               int *status );

void parDef0i( const char *param,
               int value,
               int *status );

void parDef0l( const char *param,
               int value,
               int *status );

void parDef0r( const char *param,
               float value,
               int *status );

void parDef1c( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status );

void parDef1d( const char *param,
               int nval,
               const double *values,
               int *status );

void parDef1i( const char *param,
               int nval,
               const int *values,
               int *status );

void parDef1l( const char *param,
               int nval,
               const int *values,
               int *status );

void parDef1r( const char *param,
               int nval,
               const float *values,
               int *status );

void parDefnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               const int *actd,
               int *status );

void parDefnd( const char *param,
               int ndim,
               const int *maxd,
               const double *values,
               const int *actd,
               int *status );

void parDefni( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status );

void parDefnl( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status );

void parDefnr( const char *param,
               int ndim,
               const int *maxd,
               const float *values,
               const int *actd,
               int *status );

void parExacc( const char *param,
               int nvals,
               char *const *values,
               int values_length,
               int *status );

void parExacd( const char *param,
               int nvals,
               double *values,
               int *status );

void parExaci( const char *param,
               int nvals,
               int *values,
               int *status );

void parExacl( const char *param,
               int nvals,
               int *values,
               int *status );

void parExacr( const char *param,
               int nvals,
               float *values,
               int *status );

void parGdr0d( const char *param,
               double defaul,
               double vmin,
               double vmax,
               int null,
               double *value,
               int *status );

void parGdr0i( const char *param,
               int defaul,
               int vmin,
               int vmax,
               int null,
               int *value,
               int *status );

void parGdr0r( const char *param,
               float defaul,
               float vmin,
               float vmax,
               int null,
               float *value,
               int *status );

void parGdr1d( const char *param,
               int nvals,
               const double *defaul,
               double vmin,
               double vmax,
               int null,
               double *values,
               int *status );

void parGdr1i( const char *param,
               int nvals,
               const int *defaul,
               int vmin,
               int vmax,
               int null,
               int *values,
               int *status );

void parGdr1r( const char *param,
               int nvals,
               const float *defaul,
               float vmin,
               float vmax,
               int null,
               float *values,
               int *status );

void parGdrvd( const char *param,
               int maxval,
               double vmin,
               double vmax,
               double *values,
               int *actval,
               int *status );

void parGdrvi( const char *param,
               int maxval,
               int vmin,
               int vmax,
               int *values,
               int *actval,
               int *status );

void parGdrvr( const char *param,
               int maxval,
               float vmin,
               float vmax,
               float *values,
               int *actval,
               int *status );

void parGet0c( const char *param,
               char *value,
               int value_length,
               int *status );

void parGet0d( const char *param,
               double *value,
               int *status );

void parGet0i( const char *param,
               int *value,
               int *status );

void parGet0l( const char *param,
               int *value,
               int *status );

void parGet0r( const char *param,
               float *value,
               int *status );

void parGet1c( const char *param,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parGet1d( const char *param,
               int maxval,
               double *values,
               int *actval,
               int *status );

void parGet1i( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status );

void parGet1l( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status );

void parGet1r( const char *param,
               int maxval,
               float *values,
               int *actval,
               int *status );

void parGetnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               int *actd,
               int *status );

void parGetnd( const char *param,
               int ndim,
               const int *maxd,
               double *values,
               int *actd,
               int *status );

void parGetni( const char *param,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status );

void parGetnl( const char *param,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status );

void parGetnr( const char *param,
               int ndim,
               const int *maxd,
               float *values,
               int *actd,
               int *status );

void parGetvc( const char *param,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parGetvd( const char *param,
               int maxval,
               double *values,
               int *actval,
               int *status );

void parGetvi( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status );

void parGetvl( const char *param,
               int maxval,
               int *values,
               int *actval,
               int *status );

void parGetvr( const char *param,
               int maxval,
               float *values,
               int *actval,
               int *status );

void parGeven( const char *param,
               int defaul,
               int vmin,
               int vmax,
               int null,
               int *value,
               int *status );

void parGodd( const char *param,
              int defaul,
              int vmin,
              int vmax,
              int null,
              int *value,
              int *status );

void parGrm1d( const char *param,
               int nvals,
               const double *defaul,
               const double *vmin,
               const double *vmax,
               int null,
               double *values,
               int *status );

void parGrm1i( const char *param,
               int nvals,
               const int *defaul,
               const int *vmin,
               const int *vmax,
               int null,
               int *values,
               int *status );

void parGrm1r( const char *param,
               int nvals,
               const float *defaul,
               const float *vmin,
               const float *vmax,
               int null,
               float *values,
               int *status );

void parGrmvd( const char *param,
               int maxval,
               const double *vmin,
               const double *vmax,
               double *values,
               int *actval,
               int *status );

void parGrmvi( const char *param,
               int maxval,
               const int *vmin,
               const int *vmax,
               int *values,
               int *actval,
               int *status );

void parGrmvr( const char *param,
               int maxval,
               const float *vmin,
               const float *vmax,
               float *values,
               int *actval,
               int *status );

void parGtd0l( const char *param,
               int defaul,
               int null,
               int *value,
               int *status );

void parMaxc( const char *param,
              const char *value,
              int *status );

void parMaxd( const char *param,
              double value,
              int *status );

void parMaxi( const char *param,
              int value,
              int *status );

void parMaxr( const char *param,
              float value,
              int *status );

void parMinc( const char *param,
              const char *value,
              int *status );

void parMind( const char *param,
              double value,
              int *status );

void parMini( const char *param,
              int value,
              int *status );

void parMinr( const char *param,
              float value,
              int *status );

void parMix0d( const char *param,
               const char *defaul,
               double vmin,
               double vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status );

void parMix0i( const char *param,
               const char *defaul,
               int vmin,
               int vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status );

void parMix0r( const char *param,
               const char *defaul,
               float vmin,
               float vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status );

void parMixvd( const char *param,
               int maxval,
               double vmin,
               double vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parMixvi( const char *param,
               int maxval,
               int vmin,
               int vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parMixvr( const char *param,
               int maxval,
               float vmin,
               float vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parPromt( const char *param,
               const char *prompt,
               int *status );

void parPut0c( const char *param,
               const char *value,
               int *status );

void parPut0d( const char *param,
               double value,
               int *status );

void parPut0i( const char *param,
               int value,
               int *status );

void parPut0l( const char *param,
               int value,
               int *status );

void parPut0r( const char *param,
               float value,
               int *status );

void parPut1c( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status );

void parPut1d( const char *param,
               int nval,
               const double *values,
               int *status );

void parPut1i( const char *param,
               int nval,
               const int *values,
               int *status );

void parPut1l( const char *param,
               int nval,
               const int *values,
               int *status );

void parPut1r( const char *param,
               int nval,
               const float *values,
               int *status );

void parPutnc( const char *param,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               const int *actd,
               int *status );

void parPutnd( const char *param,
               int ndim,
               const int *maxd,
               const double *values,
               const int *actd,
               int *status );

void parPutni( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status );

void parPutnl( const char *param,
               int ndim,
               const int *maxd,
               const int *values,
               const int *actd,
               int *status );

void parPutnr( const char *param,
               int ndim,
               const int *maxd,
               const float *values,
               const int *actd,
               int *status );

void parPutvc( const char *param,
               int nval,
               char *const *values,
               int values_length,
               int *status );

void parPutvd( const char *param,
               int nval,
               const double *values,
               int *status );

void parPutvi( const char *param,
               int nval,
               const int *values,
               int *status );

void parPutvl( const char *param,
               int nval,
               const int *values,
               int *status );

void parPutvr( const char *param,
               int nval,
               const float *values,
               int *status );

void parState( const char *param,
               int *state,
               int *status );

void parUnset( const char *param,
               const char *which,
               int *status );




/* ----- k functions (_INT64) --------------- */





void parDef0k( const char *param,
               int64_t value,
               int *status );
void parDef1k( const char *param,
               int nval,
               const int64_t *values,
               int *status );

void parDefnk( const char *param,
               int ndim,
               const int *maxd,
               const int64_t *values,
               const int *actd,
               int *status );

void parExack( const char *param,
               int nvals,
               int64_t *values,
               int *status );

void parGdr0k( const char *param,
               int64_t defaul,
               int64_t vmin,
               int64_t vmax,
               int null,
               int64_t *value,
               int *status );

void parGdr1k( const char *param,
               int nvals,
               const int64_t *defaul,
               int64_t vmin,
               int64_t vmax,
               int null,
               int64_t *values,
               int *status );

void parGdrvk( const char *param,
               int maxval,
               int64_t vmin,
               int64_t vmax,
               int64_t *values,
               int *actval,
               int *status );

void parGet0k( const char *param,
               int64_t *value,
               int *status );

void parGet1k( const char *param,
               int maxval,
               int64_t *values,
               int *actval,
               int *status );

void parGetnk( const char *param,
               int ndim,
               const int *maxd,
               int64_t *values,
               int *actd,
               int *status );
void parGetvk( const char *param,
               int maxval,
               int64_t *values,
               int *actval,
               int *status );

void parGrm1k( const char *param,
               int nvals,
               const int64_t *defaul,
               const int64_t *vmin,
               const int64_t *vmax,
               int null,
               int64_t *values,
               int *status );
void parGrmvk( const char *param,
               int maxval,
               const int64_t *vmin,
               const int64_t *vmax,
               int64_t *values,
               int *actval,
               int *status );
void parMaxk( const char *param,
              int64_t value,
              int *status );

void parMink( const char *param,
              int64_t value,
              int *status );

void parMix0k( const char *param,
               const char *defaul,
               int64_t vmin,
               int64_t vmax,
               const char *opts,
               int null,
               char *value,
               int value_length,
               int *status );

void parMixvk( const char *param,
               int maxval,
               int64_t vmin,
               int64_t vmax,
               const char *opts,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void parPut0k( const char *param,
               int64_t value,
               int *status );

void parPut1k( const char *param,
               int nval,
               const int64_t *values,
               int *status );

void parPutnk( const char *param,
               int ndim,
               const int *maxd,
               const int64_t *values,
               const int *actd,
               int *status );

#endif  /* PARWRAP_DEFINED */
