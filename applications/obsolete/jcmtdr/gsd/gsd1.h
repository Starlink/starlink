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

 * Authors:
 *    rpt: Remo Tilanus (JACH)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    04 Feb 1994 or 02 Apr 1994 (rpt):
 *       Original version.
 *    29 Nov 1994 (hme):
 *       Remove the system-dependent static external host_order from this file.
 *       Remove the static external gsd_byte as well, used only in one routine.
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

struct file_descriptor
{  float version;         /* GSD file format version for SPECX         */
   int   max_no_items;    /* Maximum number of items in file ?         */
   int   no_items;        /* Number of items in this file    ?         */
   int   str_data;        /* Start of data area in file - byte number  */
   int   end_data;        /* End of data area - byte number            */
   char  comment[40];      
   int size;              
};

/* Define the GSD item descriptor.
 */

struct item_descriptor
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
};

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


int gsd1_rdfildsc( FILE *fptr, struct file_descriptor *file_dsc );
int gsd1_rdhead(   FILE *fptr, struct file_descriptor *file_dsc,
   struct item_descriptor *item_ptr );
int gsd1_rddata(   FILE *fptr, struct item_descriptor *item_ptr,
   char *data_ptr, int startitem, int noitems );
int gsd1_getval( struct file_descriptor *file_dsc,
   struct item_descriptor *item_dsc, char *data_ptr,
   int mode, short data_type, char *name, int *itemno,
   int first, int last, char *gsdval );

#endif
/* Bottom of gsd1.h */
