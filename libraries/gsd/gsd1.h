/* Top of gsd1.h */
#if !defined GSD1_H
#define GSD1_H
/*+
 * Name:
 *    gsd1.h

 * Purpose:
 *    Internal include file for GSD library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    C function.

 * Invocation:
 *    #include "gsd1.h"

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
 *    rpt: Remo Tilanus (JACH)
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    timj: Tim Jenness (JAC, Hawaii)

 * History:
 *    04 Feb 1994 or 02 Apr 1994 (rpt):
 *       Original version.
 *    29 Nov 1994 (hme):
 *       Remove the system-dependent static external host_order from this file.
 *       Remove the static external gsd_byte as well, used only in one routine.
 *    3 Jul 2008 (timj):
 *       Use typedef for struct. Consting.

 * Copyright:
 *    Copyright (C) 2008 Science and Technology Facilities Council.
 *    Copyright (C) 1994-1999 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *-
 */

#include <stdio.h>

/* Data type is one of seven: GSD type starts at 1.
 * Also define size of each data type.
 */

#define  GSD_NTYPES    7

#define  GSD_SZBYTE    1
#define  GSD_SZLOGICAL 1
#define  GSD_SZWORD    2
#define  GSD_SZINTEGER 4
#define  GSD_SZREAL    4
#define  GSD_SZDOUBLE  8
#define  GSD_SZCHAR   16

enum type_tag
{  typ_byte  =  1,
   typ_logical,
   typ_word,
   typ_int,
   typ_real,
   typ_double,
   typ_char
};

/* Define the GSD file descriptor.
 */

typedef struct file_descriptor
{  float version;         /* GSD file format version for SPECX         */
   int   max_no_items;    /* Maximum number of items in file ?         */
   int   no_items;        /* Number of items in this file    ?         */
   int   str_data;        /* Start of data area in file - byte number  */
   int   end_data;        /* End of data area - byte number            */
   char  comment[40];
   int size;
} GSDFileDesc;

/* Define the GSD item descriptor.
 */

typedef struct item_descriptor
{  char  array;
   char  name[15];
   short namelen;
   char  unit[10];
   short unitlen;
   short data_type;
   int   location;
   int   length;
   int   no_dims;
   int   dimnumbers[5];
} GSDItemDesc;

/* Collection of prototypes for the internal routines.
 */

void gsd1_swap_data( char *ch_ptr, enum type_tag d_type, int d_length );
short int gsd1_sswap( short int *var );
int       gsd1_lswap( int       *var );
float     gsd1_fswap( float     *var );
double    gsd1_dswap( double    *var );

void gsd2_nativb( unsigned char *bytes );
void gsd2_nativl( unsigned char *bytes );
void gsd2_nativw( unsigned char *bytes );
void gsd2_nativi( unsigned char *bytes );
void gsd2_nativr( unsigned char *bytes );
void gsd2_nativd( unsigned char *bytes );
void gsd2_nativc( unsigned char *bytes );
void gsd2_nativa( char *ch_ptr, enum type_tag d_type, int d_length );

int gsd2_copya( enum type_tag itype, enum type_tag otype,
		size_t size, const unsigned char *in, unsigned char *out );


int gsd1_rdfildsc( FILE *fptr, struct file_descriptor *file_dsc );
int gsd1_rdhead(   FILE *fptr, const struct file_descriptor *file_dsc,
   struct item_descriptor *item_ptr );
int gsd1_rddata( FILE *fptr, const struct item_descriptor *item_ptr,
   char *data_ptr, int startitem, int noitems );
int gsd1_getval( const struct file_descriptor *file_dsc,
   const struct item_descriptor *item_dsc, const char *data_ptr,
   int mode, short data_type, char *name, int *itemno,
   int first, int last, char *gsdval );

#endif
/* Bottom of gsd1.h */
