/*+
 * Name:
 *    gsd1_rdhead

 * Purpose:
 *    Read GSD header items.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    int gsd1_rdhead( FILE *fptr, const struct file_descriptor *file_dsc,
 *       struct item_descriptor *item_ptr );

 * Description:
 *    Routine to read header items and header from GSD file. It stores the
 *    found items into the item descriptor item_ptr.

 * Arguments:
 *    FILE *fptr (Given)
 *       C file descriptor.
 *    const struct file_descriptor *file_dsc (Given)
 *       GSD header file descriptor.
 *    struct item_descriptor *item_ptr (Returned)
 *       Array of item descriptors. This should be of size file_dsc->no_items
 *       or more. Otherwise this routine overwrites unrelated memory.

 * Returned value:
 *    int gsd1_rdhead();
 *       The return value is (-1000-i) if the fread failed on the i-th item (i
 *       counts from zero). Such failure means that not enough bytes were left
 *       in the file to read the i-th item. If all items could be read the
 *       return value is zero. This does not mean that any item contains valid
 *       information, neither that the file in question is actually a GSD file.

 * Implementation Status:
 *    This routine looks at how many bytes this C implementation needs for the
 *    structures that are a GSD descriptors. It then read that many bytes in
 *    one chunk with fread. Unless fread is very clever, problems may arise
 *    when the struct contains padding bytes. There will also be a problem if
 *    in the file there are padding bytes that this C implementation would not
 *    want in the struct.
 *
 *    This could be fixed by reading each struct member with its own fread.

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
 *       Split off gsd.c.
 *    02 Dec 1994 (hme):
 *       ANSI C. Do use numbers 0 and 1 for the array flag. This is supposed to
 *       be a logical. '0' equals 48, which is true in C, but false in Fortran.
 *       Null-terminate name and unit.
 *    08 Dec 1994 (hme):
 *       Use gsd2_nativx routines.
 *    04 Jul 2008 (timj):
 *       use const

 * Copyright:
 *    Copyright (C) 2008 Science and Technology Facilities Council.
 *    Copyright (C) 1994-1999 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *-
 */

#include <ctype.h>
#include <stdio.h>
#include "gsd1.h"

/*:
 */

int gsd1_rdhead( FILE *fptr, const struct file_descriptor *file_dsc,
   struct item_descriptor *item_ptr )
{
   struct item_descriptor *item_ptr2;
   size_t size_of_item;
   int i, j;

/*.
 */

   item_ptr2 = item_ptr;
   size_of_item = sizeof( struct item_descriptor );

   for ( i = 0; i < file_dsc->no_items; i++, item_ptr2++ )
   {
/*    Read the item from file.
 */
      if ( fread( item_ptr2, size_of_item, 1, fptr ) != 1 )
         return ( -1000 - i );

/*    Strings in the structures are not null-terminated. They must be
 *    terminated when retrieved.
 *    Convert name to upper case
 *    and swap byte order for numeric items from VAX to machine order.
 */
      for ( j = 0; j < 15; j++ )
         item_ptr2->name[j] = toupper(item_ptr2->name[j]);
      (void) gsd2_nativl( (unsigned char *) &item_ptr2->array     );
      (void) gsd2_nativw( (unsigned char *) &item_ptr2->namelen   );
      (void) gsd2_nativw( (unsigned char *) &item_ptr2->unitlen   );
      (void) gsd2_nativw( (unsigned char *) &item_ptr2->data_type );
      (void) gsd2_nativi( (unsigned char *) &item_ptr2->location  );
      (void) gsd2_nativi( (unsigned char *) &item_ptr2->length    );
      (void) gsd2_nativi( (unsigned char *) &item_ptr2->no_dims   );
      for ( j = 0; j < 5; j++ )
         (void) gsd2_nativi( (unsigned char *) &item_ptr2->dimnumbers[j] );
   }

   return 0;
}
