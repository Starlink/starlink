/*
*+
*  Name:
*     sx.h

*  Purpose:
*     Public includes for Starlink DX interface

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*-
*/

#include <dx/dx.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>


struct fpair;
typedef struct fpair {
    Field infld;
    Field outfld;
    Object pos;
    int postag;
    Object data;
    Type datatype;
    int datalen;
    struct fpair *next;
                     } Fpair;

Error SXSampleD( int nfld, int ndim, int veclen[], int ninpos, float *inpos,
                 double *indata[], int noutpos, float *outpos, double
                 *outdata[], float *lbout, float *ubout, int nclose,
                 float maxrad, float *rscale, int nscale, float power,
                 float coexp, char type, int *outbad );

Error SXSampleF( int nfld, int ndim, int veclen[], int ninpos, float *inpos,
                 float *indata[], int noutpos, float *outpos, float
                 *outdata[], float *lbout, float *ubout, int nclose,
                 float maxrad, float *rscale, int nscale, float power,
                 float coexp, char type, int *outbad );

int *SXInitClose( int ndim, int npos, float *pos, float *lbnd,
                  float *ubnd, float *lbndl, int *gdim, float *rdcell );

Error SXFindClose( int ndim, int npos, float *pos, float *samp,
                   int nclose, float rdcell, float *lbndl, int *gdim,
                   int *work, int *posres, float *disres );

Error SXFindRadius( int ndim, int npos, float *pos, float *samp,
                    float maxrad, float rdcell, float *lbndl, int *gdim,
                    int *work, int *nres, int **posres, float **disres );

void SXSort( int n, float *d, int *m );

float *SXGet1r( char *name, Object in, int *n );

int   *SXGet1i( char *name, Object in, int *n );

float *SXPut1r( char *name, int n, Object *out );

Error  SXGet0rs( char *param, Object in, float hi, float lo, int nopt,
                 char *opt[], float *rval, int *sval );

Error  SXGet0is( char *param, Object in, int hi, int lo, int nopt,
                 char *opt[], int *ival, int *sval );

Object SXMakeOut( Object in, Field template, int typecheck, int maxrank,
                  int maxshape, char *dep );

Error SXGetInvPos( Object field, int nel, int ndim, float *data,
                   char *dep );

Error SXSetInvPosD( Object field, int nel, int ndim, double *data,
                    char *dep );

Error SXSetInvPosF( Object field, int nel, int veclen, float *data,
                    char *dep );

float *SXGetGrid( Object in, int *nsamp, int *ndim, float *lbnd, float
                  *ubnd, Object *grid );

Error SXBinF( int nfld, int veclen[], int ninpos, float *indata[],
              int nbin, float *outdata[], int *map, int *counts,
              char type, int *outbad );

Error SXBinD( int nfld, int veclen[], int ninpos, double *indata[],
              int nbin, double *outdata[], int *map, int *counts,
              char type, int *outbad );

Interpolator SXGetIntp( Object in, int *ncon, int *ndim, Object *grid );

