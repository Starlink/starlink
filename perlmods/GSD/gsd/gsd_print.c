/*+
 * Name:
 *    gsd_print(main)

 * Purpose:
 *    Print a GSD file.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Application.

 * Usage:
 *    gsd_print file

 * Arguments:
 *    file:
 *       The name of the input GSD file.

 * Description:
 *    This routine prints the contents of a GSD file. It is a translation to C
 *    of the old GSD_PRINT Fortran programme. The details of the output
 *    formatting may differ. Also output goes to stdout and must be re-directed
 *    to a file by the user.

 * Return Value:
 *    int main();
 *       Status.
 *        1: No file name given.
 *        2: Could not read file.
 *        3: Item access failed.
 *        4: Scalar data access failed.
 *        5: Array enquiry failed.
 *        6: Array access failed.
 *        0: Otherwise.

 * Authors:
 *    jhf: Jon Fairclough (UKTH)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    11 Nov 1986 (jhf):
 *       Original version.
 *    01 Dec 1994 (hme):
 *       Translation to C. Use Unix-ported GSD library.
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gsd.h"

#define HEADER_FORMAT \
"------------------------------------------------------------\
--------------------\n G S D     P R I N T\n\
------------------------------------------------------------\
--------------------\n\n FILENAME     : %s\n\
 GSD VERSION  : %6.3f\n LABEL        : %s\n NITEMS       : %3d\n\n\n\
 NAME            UNIT          TYPE      TABLE     VALUE or SIZE\n\
------------------------------------------------------------\
--------------------\n\n"

/*:
 */

int main( int argc, char *argv[] )
{
   FILE  *fptr = NULL;      /* C file descriptor */

   void  *file_dsc = NULL;  /* GSD file descriptor */
   void  *item_dsc = NULL;  /* GSD item descriptors */
   char  *data_ptr = NULL;  /* Collective data from GSD file */

   char  *message[] =
   {  "gsd_print: OK.\n",
      "Usage: gsd_print filename\n",
      "gsd_print: Could not read file.\n",
      "gsd_print: Item access failed.\n",
      "gsd_print: Scalar data access failed.\n",
      "gsd_print: Array enquiry failed.\n",
      "gsd_print: Array access failed.\n"
   };

   char   dimnames[5][16];  /* Names of array dimensions */
   char  *dimnam[5];        /* Pointer array for dimnames */
   char   dimunits[5][11];  /* Units of array dimensions */
   char  *dimunt[5];        /* Pointer array for dimunits */
   char   label[41];        /* File label */
   char   name[16];         /* Item name */
   char   unit[11];         /* Item unit */
   char   type;             /* Data type code (BLWIRDC) */
   char   table;            /* Arrayness of item */
   int    actdims;          /* Acutal dimensionality of array */
   int    actvals;          /* Actual size of array part */
   int    dimvals[5];       /* Array dimensions */
   int    i, j;             /* Loop indices */
   int    nitem;            /* Number of items in file */
   int    number;           /* Loop counter counting the item */
   int    one = 1;          /* Itself, but can be pointed to */
   int    size;             /* Total size of array */
   int    status;           /* Status to be returned */
   float  version;          /* File format version */

   size_t cell;
   char  *p;
   char  *buffer = NULL;
   char   bvalue; char   lvalue; short  wvalue;     int    ivalue;
   float  rvalue; double dvalue; char   cvalue[17];

/*.
 */

/* Set string array pointers.
 */
   for ( i = 0; i < 5; i++ )
   {  dimnam[i] = dimnames[i]; dimunt[i] = dimunits[i];
   }

/* Check that file name is given.
 */
   if ( argc < 2 ) { status = 1; goto abort; }

/* Open and read the file.
 */
   status = gsdOpenRead( argv[1], &version, label, &nitem,
      &fptr, &file_dsc, &item_dsc, &data_ptr );
   if ( status ) { status = 2; goto abort; }

/* Write header.
 */
   (void) printf( HEADER_FORMAT, argv[1], version, label, nitem );

/* Loop through items.
 */
   for ( number = 1; number <= nitem && !status; number++ )
   {
/*    Get the item name, unit, type, arrayness.
 */
      status = gsdItem( file_dsc, item_dsc, number,
         name, unit, &type, &table );
      if ( status ) { status = 3; goto abort; }

/*    Processing for scalar.
 */
      if ( !table )
      {  switch ( type )
         {  case 'B':
               status = gsdGet0b( file_dsc, item_dsc, data_ptr,
                  number, &bvalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %d\n",
                   name, unit, type, bvalue );
               break;
            case 'L':
               status = gsdGet0l( file_dsc, item_dsc, data_ptr,
                  number, &lvalue );
               if ( status ) { status = 4; goto abort; }
               if ( lvalue )
                  (void) printf( " %15s %10s    %c         F         T\n",
                     name, unit, type );
               else
                  (void) printf( " %15s %10s    %c         F         F\n",
                     name, unit, type );
               break;
            case 'W':
               status = gsdGet0w( file_dsc, item_dsc, data_ptr,
                  number, &wvalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %d\n",
                  name, unit, type, wvalue );
               break;
            case 'I':
               status = gsdGet0i( file_dsc, item_dsc, data_ptr,
                  number, &ivalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %d\n",
                  name, unit, type, ivalue );
               break;
            case 'R':
               status = gsdGet0r( file_dsc, item_dsc, data_ptr,
                  number, &rvalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %.8G\n",
                  name, unit, type, rvalue );
               break;
            case 'D':
               status = gsdGet0d( file_dsc, item_dsc, data_ptr,
                  number, &dvalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %.15G\n",
                  name, unit, type, dvalue );
               break;
            case 'C':
               status = gsdGet0c( file_dsc, item_dsc, data_ptr,
                  number, cvalue );
               if ( status ) { status = 4; goto abort; }
               (void) printf( " %15s %10s    %c         F         %s\n",
                  name, unit, type, cvalue );
               break;
            default:
               break;
         }  /* End of switch type */

      }  /* End of if not array block */

/*    Processing for array.
 */
      else
      {
/*       Find out size of array.
 */
         status = gsdInqSize( file_dsc, item_dsc, data_ptr, number, 5,
            dimnam, dimunt, dimvals, &actdims, &size );
         if ( status ) { status = 5; goto abort; }

/*       Write shape information.
 */
         (void) printf( " %15s %10s    %c         T         %10d%10d\n",
            name, unit, type, size, actdims );
         for ( i = 0; i < actdims; i++ )
            (void) printf(
                "  DIMNAMES = %15s DIMUNITS = %10s DIMVALS = %10d\n",
                dimnames[i], dimunits[i], dimvals[i] );

/*       Get the array and write it. Depends on type.
 */
         switch ( type )
         {  case 'B':
               cell = sizeof(char);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1b( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) printf( " %4d ", *p ); p += cell; j++;
                  if ( j > 11 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'L':
               cell = sizeof(char);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1l( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  if ( *p ) (void) printf( " T " );
                  else      (void) printf( " F " );
                  p += cell; j++;
                  if ( j > 11 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'W':
               cell = sizeof(short);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1w( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, (short *) buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) printf( " %6d ", *(short*)p ); p += cell; j++;
                  if ( j > 7 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'I':
               cell = sizeof(int);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1i( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, (int *) buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) printf( " %10d ", *(int*)p ); p += cell; j++;
                  if ( j > 5 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'R':
               cell = sizeof(float);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1r( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, (float *) buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) printf( " %14.8G ", *(float*)p ); p += cell; j++;
                  if ( j > 3 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'D':
               cell = sizeof(double);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1d( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, (double *) buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) printf( " %21.15G ", *(double*)p ); p += cell; j++;
                  if ( j > 2 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            case 'C':
               cell = 16 * sizeof(char);
               buffer = malloc( size * cell );
               if ( !buffer ) { status = 6; goto abort; }
               status = gsdGet1c( file_dsc, item_dsc, data_ptr,
                  number, 1, &size, &one, &size, buffer, &actvals );
               if ( status ) { status = 6; goto abort; }
               for ( i = j = 0, p = buffer; i < size; i++ )
               {  (void) memcpy( cvalue, p, 16 ); cvalue[16] = '\0';
                  (void) printf( " %16s ", cvalue );
                  p += cell; j++;
                  if ( j > 3 ) { (void) printf( "\n" ); j = 0; }
               }
               if ( j > 0 ) (void) printf( "\n" );
               break;
            default:
               break;
         }  /* End of switch type */

/*       Release the array.
 */
         (void) free( buffer );

      }  /* End of if not array else block */

   }  /* End of for loop through items */

/* Tidy up.
 */
   abort:
   (void) gsdClose( fptr, file_dsc, item_dsc, data_ptr );
   if ( status ) (void) fprintf( stderr, message[status] );
   return status;
}
