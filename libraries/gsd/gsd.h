/* Top of gsd.h */
#if !defined GSD_H
#define GSD_H
/*+
 * Name:
 *    gsd.h

 * Purpose:
 *    External include file for GSD library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    C function.

 * Invocation:
 *    #include "gsd.h"

 * Description:
 *    {What it does}

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    30 Nov 1994 (hme):
 *       Original version.

 * Copyright:
 *    Copyright (C) 1994-1999 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved. 

 *-
 */

#include <stdio.h>

int gsdOpenRead( char *file, float *version, char *label, int *nitem,
   FILE **fptr, void **file_dsc, void **item_dsc, char **data_ptr );
int gsdClose( FILE *fptr, void *file_dsc, void *item_dsc, char *data_ptr );
int gsdFind( void *file_dsc, void *item_dsc, char *name, int *itemno,
   char *unit, char *type, char *array );
int gsdItem( void *file_dsc, void *item_dsc, int itemno, char *name,
   char *unit, char *type, char *array );
int gsdInqSize( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int maxdims,
   char **dimnames, char **dimunits, int *dimvals, int *actdims, int *size );

int gsdGet0b( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, char *value );
int gsdGet0l( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, char *value );
int gsdGet0w( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, short *value );
int gsdGet0i( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, int *value );
int gsdGet0r( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, float *value );
int gsdGet0d( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, double *value );
int gsdGet0c( void *file_dsc, void *item_dsc, char *data_ptr,
   int itemno, char *value );

int gsdGet1b( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals );
int gsdGet1l( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals );
int gsdGet1w( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   short *values, int *actvals );
int gsdGet1i( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   int *values, int *actvals );
int gsdGet1r( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   float *values, int *actvals );
int gsdGet1d( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   double *values, int *actvals );
int gsdGet1c( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals );

#endif
/* Bottom of gsd.h */
