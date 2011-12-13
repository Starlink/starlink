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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    timj: Tim Jenness (JAC, Hawaii)

 * History:
 *    30 Nov 1994 (hme):
 *       Original version.
 *    04 Jul 2008 (timj):
 *       use proper GSD structs rather than void. use const.

 * Copyright:
 *    Copyright (C) 2008 Science and Technology Facilities Council.
 *    Copyright (C) 1994-1999 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *-
 */

#include <stdio.h>

/* Public view of GSD structs - do not define if we are internal build */
#ifndef GSD1_H
typedef struct file_descriptor GSDFileDesc;
typedef struct item_descriptor GSDItemDesc;
#endif

int gsdOpenRead( const char *file, float *version, char *label, int *nitem,
                 FILE **fptr, GSDFileDesc **file_dsc, GSDItemDesc **item_dsc,
                 char **data_ptr );
int gsdClose( FILE *fptr, GSDFileDesc *file_dsc, GSDItemDesc *item_dsc, char *data_ptr );

int gsdFind( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc, const char name[],
             int *itemno, char *unit, char *type, char *array );

int gsdItem( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc, int itemno,
             char *name, char *unit, char *type, char *array );

int gsdInqSize( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
                const char *data_ptr,
                int itemno, int maxdims,
                char **dimnames, char **dimunits, int *dimvals, int *actdims, int *size );

int gsdGet0b( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value );

int gsdGet0l( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value );

int gsdGet0w( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, short *value );

int gsdGet0i( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, int *value );

int gsdGet0r( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, float *value );

int gsdGet0d( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, double *value );

int gsdGet0c( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value );

int gsdGet1b( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              char *values, int *actvals );

int gsdGet1l( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              char *values, int *actvals );

int gsdGet1w( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              short *values, int *actvals );

int gsdGet1i( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              int *values, int *actvals );

int gsdGet1r( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              float *values, int *actvals );

int gsdGet1d( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              double *values, int *actvals );

int gsdGet1c( const GSDFileDesc *file_dsc_arg, const GSDItemDesc *item_dsc_arg,
              const char *data_ptr, int itemno,
              int ndims, const int dimvals[], const int start[], const int end[],
              char *values, int *actvals );

#endif
/* Bottom of gsd.h */
