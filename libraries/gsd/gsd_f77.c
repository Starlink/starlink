/*
*+
* Name:
*    gsd_f77.c

* Purpose:
*    Fortran binding for GSD library.

* Language:
*    ANSI C

* Type of Module:
*    Fortran-callable C function.

* Description:
*    This file contains the Fortran binding of the GSD library.

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
*    05 Dec 1994 (hme):
*       Original version.
*    16 Dec 1999 (timj):
*       Fix prototype warning in gsdGet1b
*    20 Sep 2005 (timj):
*       Update CNF usage and fix cnfFree vs free usage.
*    04 Jul 2008 (timj):
*       use proper GSD structs rather than void. use const.


* Copyright:
*    Copyright (C) 2008 Science and Technology Facilities Council.
*    Copyright (C) 1994-2005 Particle Physics and Astronomy Research Council.
*    All Rights Reserved.

*-
 */

#include <stdlib.h>
#include "f77.h"
#include "gsd1.h"
#include "gsd.h"

F77_SUBROUTINE(gsd_open_read)( CHARACTER(file), INTEGER(fd), REAL(version),
   CHARACTER(label), INTEGER(no_items), INTEGER(status)
   TRAIL(file) TRAIL(label) );
F77_SUBROUTINE(gsd_close)( INTEGER(fd), INTEGER(status) );

F77_SUBROUTINE(gsd_find)( INTEGER(fd), CHARACTER(name), INTEGER(number),
   CHARACTER(unit), CHARACTER(type), LOGICAL(array), INTEGER_ARRAY(index),
   INTEGER(status) TRAIL(name) TRAIL(unit) TRAIL(type) );
F77_SUBROUTINE(gsd_item)( INTEGER(fd), INTEGER(number), CHARACTER(name),
   CHARACTER(unit), CHARACTER(type), LOGICAL(array), INTEGER_ARRAY(index),
   INTEGER(status) TRAIL(name) TRAIL(unit) TRAIL(type) );

F77_SUBROUTINE(gsd_inq_size)( INTEGER(fd), INTEGER(number), INTEGER(maxdims),
   CHARACTER_ARRAY(dimnames), CHARACTER_ARRAY(dimunits),
   INTEGER_ARRAY(dimvals), INTEGER(actdims), INTEGER(size), INTEGER(status)
   TRAIL(dimnames) TRAIL(dimunits) );

F77_SUBROUTINE(gsd_get0b)( INTEGER_ARRAY(index), BYTE(bvalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0l)( INTEGER_ARRAY(index), BYTE(lvalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0w)( INTEGER_ARRAY(index), WORD(wvalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0i)( INTEGER_ARRAY(index), INTEGER(ivalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0r)( INTEGER_ARRAY(index), REAL(rvalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0d)( INTEGER_ARRAY(index), DOUBLE(dvalue),
   INTEGER(status) );
F77_SUBROUTINE(gsd_get0c)( INTEGER_ARRAY(index), CHARACTER(cvalue),
   INTEGER(status) TRAIL(cvalue) );

F77_SUBROUTINE(gsd_get1b)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   BYTE_ARRAY(bvalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1l)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   BYTE_ARRAY(lvalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1w)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   WORD_ARRAY(wvalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1i)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   INTEGER_ARRAY(ivalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1r)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   REAL_ARRAY(rvalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1d)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   DOUBLE_ARRAY(dvalues), INTEGER(actvals), INTEGER(status) );
F77_SUBROUTINE(gsd_get1c)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   CHARACTER_ARRAY(cvalues), INTEGER(actvals), INTEGER(status)
   TRAIL(cvalues) );

#define SAI__ERROR 148013867
#define GSD__MAXFILE     100
#define GSD__MAXDIM        5

static FILE *gsd_fptr[GSD__MAXFILE];   /* C file descriptors         */
static GSDFileDesc *gsd_fdsc[GSD__MAXFILE];   /* GSD file descriptors       */
static GSDItemDesc *gsd_idsc[GSD__MAXFILE];   /* GSD item descriptor arrays */
static char *gsd_dptr[GSD__MAXFILE];   /* Data pointers              */
static int   gsd_used[GSD__MAXFILE] =  /* Flags whether slot used    */
{  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};


/*+
 * Name:
 *    gsd_open_read

 * Purpose:
 *    Open a GSD file for reading and map it.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_OPEN_READ( FILE, FD, VERSION, LABEL, NO_ITEMS, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to
 *    gsdOpenRead.

 * Arguments:
 *    FILE = CHARACTER * ( * ) (Given)
 *       GSD file specification.
 *    FD = INTEGER (Returned)
 *       Number oof the GSD file.
 *    VERSION = REAL (Returned)
 *       GSD file version number.
 *    LABEL = CHARACTER * ( * ) (Returned)
 *       GSD file label. Internal length is 40 characters.
 *    NO_ITEMS = INTEGER (Returned)
 *       The number of GSD items in the file.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_open_read)( CHARACTER(file), INTEGER(fd), REAL(version),
   CHARACTER(label), INTEGER(no_items), INTEGER(status)
   TRAIL(file) TRAIL(label) )
{
   GENPTR_CHARACTER(file)
   GENPTR_INTEGER(fd)
   GENPTR_REAL(version)
   GENPTR_CHARACTER(label)
   GENPTR_INTEGER(no_items)
   GENPTR_INTEGER(status)

   extern FILE *gsd_fptr[];
   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   char *cfile = NULL;  /* File name               */
   char  clabel[41];    /* File label              */
   int   cfd;           /* File number minus 1     */
   int   cnitem;        /* Number of items in file */
   int   cstatus;       /* Internal status         */
   float cversion;      /* GSD file version        */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Find a free slot (file number).
 */
   for ( cfd = 0; gsd_used[cfd] && cfd < GSD__MAXFILE ; cfd++ );
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }

/* Convert given file name to C string. Open the file. Flag slot as used.
 */
   cfile = cnfCreim( file, file_length );
   cstatus = gsdOpenRead( cfile, &cversion, clabel, &cnitem,
      &gsd_fptr[cfd], &gsd_fdsc[cfd], &gsd_idsc[cfd], &gsd_dptr[cfd] );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }
   gsd_used[cfd] = 1;

/* Convert file number, version, label, number of items to Fortran.
 */
   *fd = cfd + 1;
   *version = cversion;
   *no_items = cnitem;
   cnfExprt( clabel, label, label_length );

/* Return.
 */
   abort:
   if ( cfile ) cnfFree( cfile );
   return;
}


/*+
 * Name:
 *    gsd_close

 * Purpose:
 *    Close a GSD file.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_CLOSE( FD, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to
 *    gsdClose.

 * Arguments:
 *    FD = INTEGER (Given)
 *       Number oof the GSD file.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status. This routine executes even if the given status is
 *       set. It may change the status if it fails, but returns the status
 *       unchanged if it succeeds.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_close)( INTEGER(fd), INTEGER(status) )
{
   GENPTR_INTEGER(fd)
   GENPTR_INTEGER(status)

   extern FILE *gsd_fptr[];
   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;        /* File number minus 1     */
   int   cstatus;    /* Internal status         */

/*.
 */

/* Convert slot.
 */
   cfd = *fd - 1;
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Close the file. Flag slot as free.
 */
   cstatus = gsdClose(
      gsd_fptr[cfd], gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd] );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }
   gsd_used[cfd] = 0;

/* Return.
 */
   abort:
   return;
}


/*+
 * Name:
 *    gsd_find

 * Purpose:
 *    Look up a GSD name and get its properties.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_FIND( FD, NAME, NUMBER, UNIT, TYPE, ARRAY, INDEX, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to gsdFind.

 * Arguments:
 *    FD = INTEGER (Given)
 *       Number of the GSD file.
 *    NAME = CHARACTER * ( * ) (Given)
 *       Name of the GSD item. Internally, item names have 15 characters.
 *    NUMBER = INTEGER (Returned)
 *       Ordinal number of the GSD item in the file.
 *    UNIT = CHARACTER * ( * ) (Returned)
 *       Unit of the GSD item. Internally, units strings have 10 characters.
 *    TYPE = CHARACTER * ( * ) (Returned)
 *       Data type of the GSD item. One of B, L, W, I, R, D, C.
 *    ARRAY = LOGICAL (Returned)
 *       True if the item is an array. If true then use the GSD_INQ_SIZE
 *       routine to get the dimension values and array size.
 *    INDEX( 2 ) = INTEGER (Returned)
 *       Index for accessing the GSD item. The two elements are in fact the
 *       file number and the ordinal number of the item.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_find)( INTEGER(fd), CHARACTER(name), INTEGER(number),
   CHARACTER(unit), CHARACTER(type), LOGICAL(array), INTEGER_ARRAY(index),
   INTEGER(status) TRAIL(name) TRAIL(unit) TRAIL(type) )
{
   GENPTR_INTEGER(fd)
   GENPTR_CHARACTER(name)
   GENPTR_INTEGER(number)
   GENPTR_CHARACTER(unit)
   GENPTR_CHARACTER(type)
   GENPTR_LOGICAL(array)
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern int   gsd_used[];

   char  carray;     /* Arrayness       */
   char *cname = NULL; /* Item name       */
   char  ctype;      /* Data type       */
   char  ctype2[2];  /* dto.            */
   char  cunit[11];  /* Item unit       */
   int   cfd;        /* File number     */
   int   cnumber;    /* Item number     */
   int   cstatus;    /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert slot.
 */
   cfd = *fd - 1;
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Convert given item name to C string. Find the item.
 */
   cname = cnfCreim( name, name_length );
   cstatus = gsdFind( gsd_fdsc[cfd], gsd_idsc[cfd], cname, &cnumber,
      cunit, &ctype, &carray );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert number, unit, type, arrayness, index.
 */
   *number = cnumber;
   if ( carray ) *array = F77_TRUE; else *array = F77_FALSE;
   index[0] = *fd; index[1] = cnumber;
   ctype2[0] = ctype; ctype2[1] = '\0';
   cnfExprt( ctype2, type, type_length );
   cnfExprt( cunit,  unit, unit_length );

/* Return.
 */
   abort:
   if ( cname ) cnfFree( cname );
   return;
}


/*+
 * Name:
 *    gsd_item

 * Purpose:
 *    Look up a GSD item by number and get its properties.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_ITEM( FD, NUMBER, NAME, UNIT, TYPE, ARRAY, INDEX, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to gsdItem.

 * Arguments:
 *    FD = INTEGER (Given)
 *       Number of the GSD file.
 *    NUMBER = INTEGER (Given)
 *       Ordinal number of the GSD item in the file.
 *    NAME = CHARACTER * ( * ) (Returned)
 *       Name of the GSD item. Internally, item names have 15 characters.
 *    UNIT = CHARACTER * ( * ) (Returned)
 *       Unit of the GSD item. Internally, units strings have 10 characters.
 *    TYPE = CHARACTER * ( * ) (Returned)
 *       Data type of the GSD item. One of B, L, W, I, R, D, C.
 *    ARRAY = LOGICAL (Returned)
 *       True if the item is an array. If true then use the GSD_INQ_SIZE
 *       routine to get the dimension values and array size.
 *    INDEX( 2 ) = INTEGER (Returned)
 *       Index for accessing the GSD item. The two elements are in fact the
 *       file number and the ordinal number of the item.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_item)( INTEGER(fd), INTEGER(number), CHARACTER(name),
   CHARACTER(unit), CHARACTER(type), LOGICAL(array), INTEGER_ARRAY(index),
   INTEGER(status) TRAIL(name) TRAIL(unit) TRAIL(type) )
{
   GENPTR_INTEGER(fd)
   GENPTR_INTEGER(number)
   GENPTR_CHARACTER(name)
   GENPTR_CHARACTER(unit)
   GENPTR_CHARACTER(type)
   GENPTR_LOGICAL(array)
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern int   gsd_used[];

   char  carray;     /* Arrayness       */
   char  cname[GSD_NAMELEN+1];  /* Item name       */
   char  ctype;      /* Data type       */
   char  ctype2[2];  /* dto.            */
   char  cunit[11];  /* Item unit       */
   int   cfd;        /* File number     */
   int   cnumber;    /* Item number     */
   int   cstatus;    /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert slot.
 */
   cfd = *fd - 1;
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Find the item.
 */
   cnumber = *number;
   cstatus = gsdItem( gsd_fdsc[cfd], gsd_idsc[cfd], cnumber, cname,
      cunit, &ctype, &carray );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert number, unit, type, arrayness, index.
 */
   if ( carray ) *array = F77_TRUE; else *array = F77_FALSE;
   index[0] = *fd; index[1] = cnumber;
   ctype2[0] = ctype; ctype2[1] = '\0';
   cnfExprt( ctype2, type, type_length );
   cnfExprt( cunit,  unit, unit_length );
   cnfExprt( cname,  name, name_length );

/* Return.
 */
   abort:
   return;
}


/*+
 * Name:
 *    gsd_inq_size

 * Purpose:
 *    Inquire the size of an array.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_INQ_SIZE( FD, NUMBER, MAXDIMS, DIMNAMES, DIMUNITS, DIMVALS,
 *       ACTDIMS, SIZE, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to
 *    gsdInqSize.

 * Arguments:
 *    FD = INTEGER (Given)
 *       Number of the GSD file.
 *    NUMBER = INTEGER (Given)
 *       Ordinal number of the GSD item in the file.
 *    MAXDIMS = INTEGER (Given)
 *       Maximum number of dimensions required.
 *    DIMNAMES( MAXDIMS ) = CHARACTER * ( * ) (Returned)
 *       Names of dimensioning variables. Internally 15 characters.
 *    DIMUNITS( MAXDIMS ) = CHARACTER * ( * ) (Returned)
 *       Units of dimensioning variables. Internally 10 characters.
 *    DIMVALS( MAXDIMS ) = INTEGER (Returned)
 *       Values of dimensioning variables.
 *    ACTDIMS = INTEGER (Returned)
 *       Actual number of dimensions.
 *    SIZE = INTEGER (Returned)
 *       Total size in cells of the array data type.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_inq_size)( INTEGER(fd), INTEGER(number), INTEGER(maxdims),
   CHARACTER_ARRAY(dimnames), CHARACTER_ARRAY(dimunits),
   INTEGER_ARRAY(dimvals), INTEGER(actdims), INTEGER(size), INTEGER(status)
   TRAIL(dimnames) TRAIL(dimunits) )
{
   GENPTR_INTEGER(fd)
   GENPTR_INTEGER(number)
   GENPTR_INTEGER(maxdims)
   GENPTR_CHARACTER_ARRAY(dimnames)
   GENPTR_CHARACTER_ARRAY(dimunits)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER(actdims)
   GENPTR_INTEGER(size)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   char  adimnames[GSD__MAXDIM][GSD_NAMELEN+1];  /* Space for names        */
   char  adimunits[GSD__MAXDIM][11];  /* Space for units        */
   char *cdimnames[GSD__MAXDIM];      /* Pointers for names     */
   char *cdimunits[GSD__MAXDIM];      /* Pointers for units     */
   int   cactdims;                    /* Actual dimensionality  */
   int   cdimvals[GSD__MAXDIM];       /* Dimension values       */
   int   cfd;                         /* File number            */
   int   cmaxdims;                    /* Maximum dimensionality */
   int   cnumber;                     /* Item number            */
   int   csize;                       /* Array size             */
   int   cstatus;                     /* Internal status        */
   int   i;                           /* Temporary integer      */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Have the cdim* pointers point to the actual spaces adim*.
 */
   for ( i = 0; i < GSD__MAXDIM; i++ )
   {  cdimnames[i] = adimnames[i]; cdimunits[i] = adimunits[i];
   }

/* Correct given maxdims downwards. There is no problem, since the caller
 * cannot expect sensible values beyond actdims anyway.
 */
   cmaxdims = *maxdims; if ( cmaxdims > GSD__MAXDIM ) cmaxdims = GSD__MAXDIM;

/* Convert slot.
 */
   cfd = *fd - 1;
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Inquire array.
 */
   cnumber = *number;
   cstatus = gsdInqSize( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, cmaxdims, cdimnames, cdimunits, cdimvals, &cactdims, &csize );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert to Fortran the names, units, values, actdims, and size.
 */
   for ( i = 0; i < cactdims; i++ )
   {  cnfExprt( cdimnames[i],
         dimnames + i * dimnames_length, dimnames_length );
      cnfExprt( cdimunits[i],
         dimunits + i * dimunits_length, dimunits_length );
      dimvals[i] = cdimvals[i];
   }
   *actdims = cactdims;
   *size    = csize;

/* Return.
 */
   abort:
   return;
}


/*+
 * Name:
 *    gsd_get0{blwirdc}

 * Purpose:
 *    Get a scalar value from a GSD file.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_GET0<T>( INDEX, <T>VALUE, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to gsdGet0x.

 * Arguments:
 *    INDEX( 2 ) = INTEGER (Given)
 *       Index for accessing the GSD item. The two elements are in fact the
 *       file number and the ordinal number of the item.
 *    <T>VALUE = <TYPE> (Returned)
 *       The value. If the internal data type differs from <TYPE> and
 *       both are numeric then conversion will be performed. Conversion
 *       is by casting in the C language.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    05 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_get0b)( INTEGER_ARRAY(index), BYTE(bvalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_BYTE(bvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   char  cbvalue;     /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0b( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &cbvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *bvalue = cbvalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0l)( INTEGER_ARRAY(index), BYTE(lvalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_BYTE(lvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   char  clvalue;     /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0l( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &clvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *lvalue = clvalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0w)( INTEGER_ARRAY(index), WORD(wvalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_WORD(wvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   short cwvalue;     /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0w( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &cwvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *wvalue = cwvalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0i)( INTEGER_ARRAY(index), INTEGER(ivalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(ivalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   civalue;     /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0i( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &civalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *ivalue = civalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0r)( INTEGER_ARRAY(index), REAL(rvalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_REAL(rvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   float crvalue;     /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0r( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &crvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *rvalue = crvalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0d)( INTEGER_ARRAY(index), DOUBLE(dvalue),
   INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_DOUBLE(dvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   double cdvalue;     /* The value       */
   int    cfd;         /* File number     */
   int    cnumber;     /* Item number     */
   int    cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0d( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, &cdvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   *dvalue = cdvalue;

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get0c)( INTEGER_ARRAY(index), CHARACTER(cvalue),
   INTEGER(status) TRAIL(cvalue) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_CHARACTER(cvalue)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   char  ccvalue[17]; /* The value       */
   int   cfd;         /* File number     */
   int   cnumber;     /* Item number     */
   int   cstatus;     /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get value.
 */
   cstatus = gsdGet0c( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, ccvalue );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Convert value.
 */
   cnfExprt( ccvalue, cvalue, cvalue_length );

/* Return.
 */
   abort:
   return;
}


/*+
 * Name:
 *    gsd_get1{blwirdc}

 * Purpose:
 *    Get an array from a GSD file.

 * Language:
 *    ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invokation from Fortran 77:
 *    CALL GSD_GET1<T>( INDEX, NO_DIMS, DIMVALS, START, END,
 *       <T>VALUES, ACTVALS, STATUS )

 * Description:
 *    This routine emulates the old Fortran binding and interfaces to gsdGet1x.
 *    For a character array the length of individual strings must have
 *    been declared as 16.

 * Arguments:
 *    INDEX( 2 ) = INTEGER (Given)
 *       Index for accessing the GSD item. The two elements are in fact the
 *       file number and the ordinal number of the item.
 *    NO_DIMS = INTEGER (Given)
 *       The number of dimensions in the array <T>VALUES.
 *       GSD_INQ_SIZE returns the number of dimensions used when writing the
 *       array. The calling routine is free to supply any value if it is wished
 *       to map the array differently.
 *    DIMVALS( NO_DIMS ) = INTEGER (Given)
 *       The bounds of the dimensions in the array <T>VALUES.
 *       GSD_INQ_SIZE returns the bounds of the array used when writing the
 *       array. The calling routine is free to supply any set of values if it
 *       is wished to map the array differently.
 *    START( NO_DIMS ) = INTEGER (Given)
 *       Cell specification of the starting value.
 *       These are the array element subscripts of the start value.
 *    END( NO_DIMS ) = INTEGER (Given)
 *       Cell specification of the end value.
 *       These are the array element subscripts of the end value.
 *    <T>VALUE( * ) = <TYPE> (Returned)
 *       The array values. No type conversion is performed. If the internal
 *       data type does not match the type of the routine, then it returns with
 *       an error.
 *    ACTVALS = INTEGER (Returned)
 *       The actual number of values returned.
 *    STATUS = INTEGER (Given and Returned)
 *       Inherited status.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    06 Dec 1994 (hme):
 *       Original version.
 *-
 */

F77_SUBROUTINE(gsd_get1b)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   BYTE_ARRAY(bvalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_BYTE_ARRAY(bvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1b( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, (char *)bvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1l)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   BYTE_ARRAY(lvalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_BYTE_ARRAY(lvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1l( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, (char *) lvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1w)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   WORD_ARRAY(wvalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_WORD_ARRAY(wvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1w( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, wvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1i)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   INTEGER_ARRAY(ivalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_INTEGER_ARRAY(ivalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1i( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, ivalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1r)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   REAL_ARRAY(rvalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_REAL_ARRAY(rvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1r( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, rvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1d)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   DOUBLE_ARRAY(dvalues), INTEGER(actvals), INTEGER(status) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_DOUBLE_ARRAY(dvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1d( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, dvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}

/*:
 */

F77_SUBROUTINE(gsd_get1c)( INTEGER_ARRAY(index), INTEGER(no_dims),
   INTEGER_ARRAY(dimvals), INTEGER_ARRAY(start), INTEGER_ARRAY(end),
   CHARACTER_ARRAY(cvalues), INTEGER(actvals), INTEGER(status)
   TRAIL(cvalues) )
{
   GENPTR_INTEGER_ARRAY(index)
   GENPTR_INTEGER(no_dims)
   GENPTR_INTEGER_ARRAY(dimvals)
   GENPTR_INTEGER_ARRAY(start)
   GENPTR_INTEGER_ARRAY(end)
   GENPTR_CHARACTER_ARRAY(cvalues)
   GENPTR_INTEGER(actvals)
   GENPTR_INTEGER(status)

   extern GSDFileDesc *gsd_fdsc[];
   extern GSDItemDesc *gsd_idsc[];
   extern char *gsd_dptr[];
   extern int   gsd_used[];

   int   cfd;      /* File number     */
   int   cnumber;  /* Item number     */
   int   cstatus;  /* Internal status */

/*.
 */

/* Check status.
 */
   if ( *status ) return;

/* Check that the strings have the correct length.
 */
   if ( cvalues_length != 16 ) { *status = SAI__ERROR; goto abort; }

/* Convert index.
 */
   cfd = index[0] - 1; cnumber = index[1];
   if ( cfd >= GSD__MAXFILE ) { *status = SAI__ERROR; goto abort; }
   if ( !gsd_used[cfd] )      { *status = SAI__ERROR; goto abort; }

/* Get values. Contrary to the get0x routines, we pass down the given pointer
 * and do not cast afterwards.
 */
   cstatus = gsdGet1c( gsd_fdsc[cfd], gsd_idsc[cfd], gsd_dptr[cfd],
      cnumber, *no_dims, dimvals, start, end, cvalues, actvals );
   if ( cstatus ) { *status = SAI__ERROR; goto abort; }

/* Return.
 */
   abort:
   return;
}
