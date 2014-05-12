/*
*class++
*  Name:
*     FitsTable

*  Purpose:
*     A representation of a FITS binary table.

*  Constructor Function:
c     astFitsTable
f     AST_FITSTABLE

*  Description:
*     The FitsTable class is a representation of a FITS binary table. It
*     inherits from the Table class. The parent Table is used to hold the
*     binary data of the main table, and a FitsChan (encapsulated within
*     the FitsTable) is used to hold the FITS header.
*
*     Note - it is not recommended to use the FitsTable class to store
*     very large tables.
*
*     FitsTables are primarily geared towards the needs of the "-TAB"
*     algorithm defined in FITS-WCS paper 2, and so do not support all
*     features of FITS binary tables. In particularly, they do not
*     provide any equivalent to the following features of FITS binary
*     tables: "heap" data (i.e. binary data following the main table),
*     columns holding complex values, columns holding variable length
*     arrays, scaled columns, column formats, columns holding bit values,
*     8-byte integer values or logical values.

*  Inheritance:
*     The FitsTable class inherits from the Table class.

*  Attributes:
*     The FitsTable class does not define any new attributes beyond
*     those which are applicable to all Tables.

*  Functions:
c     In addition to those functions applicable to all Tables, the
c     following functions may also be applied to all FitsTables:
f     In addition to those routines applicable to all Tables, the
f     following routines may also be applied to all FitsTables:
*
c     - astColumnNull: Get/set the null value for a column of a FitsTable
c     - astColumnSize: Get number of bytes needed to hold a full column of data
c     - astGetColumnData: Retrieve all the data values stored in a column
c     - astGetTableHeader: Get the FITS headers from a FitsTable
c     - astPutColumnData: Store data values in a column
c     - astPutTableHeader: Store FITS headers within a FitsTable
f     - AST_COLUMNNULL: Get/set the null value for a column of a FitsTable
f     - AST_COLUMNSIZE: Get number of bytes needed to hold a full column of data
f     - AST_GETCOLUMNDATA: Retrieve all the data values stored in a column
f     - AST_GETTABLEHEADER: Get the FITS headers from a FitsTable
f     - AST_PUTCOLUMNDATA: Store data values in a column
f     - AST_PUTTABLEHEADER: Store FITS headers within a FitsTable

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     25-NOV-2010 (DSB):
*        Original version.
*     2-OCT-2012 (DSB):
*        Check for Infs as well as NaNs.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS FitsTable

/* The KeyMap key use to store the null value for a column. */
#define NULLKEY "Null"

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "table.h"               /* Tables (parent class) */
#include "channel.h"             /* I/O channels */
#include "pointset.h"            /* For astCheckNaN(F) functions */
#include "fitstable.h"           /* Interface definition for this class */


/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <limits.h>
#include <stdio.h>
#include <string.h>


/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_equal)( AstObject *, AstObject *, int * );
static int (* parent_getobjsize)( AstObject *, int * );
static void (* parent_addcolumn)( AstTable *, const char *, int, int, int *, const char *, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(FitsTable)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(FitsTable,Class_Init)
#define class_vtab astGLOBAL(FitsTable,Class_Vtab)


/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstFitsTableVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstFitsTable *astFitsTableId_( void *, const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstFitsChan *GetTableHeader( AstFitsTable *, int * );
static char *MakeKey( const char *, int, char *, int, int * );
static int ColumnNull( AstFitsTable *, const char *, int, int, int *, int *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static size_t ColumnSize( AstFitsTable *, const char *, int * );
static void AddColumn( AstTable *, const char *, int, int, int *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void CopyStrings( int, size_t, const char *, char *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void GenerateColumns( AstFitsTable *, AstFitsChan *, int * );
static void GetColumnData( AstFitsTable *, const char *, float, double, size_t, void *, int *, int * );
static void PurgeHeader( AstFitsTable *, int * );
static void PutColumnData( AstFitsTable *, const char *, int, size_t, void *, int * );
static void PutTableHeader( AstFitsTable *, AstFitsChan *, int * );
static void UpdateHeader( AstFitsTable *, const char *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */

static void AddColumn( AstTable *this, const char *name, int type,
                       int ndim, int *dims, const char *unit, int *status ) {
/*
*  Name:
*     AddColumn

*  Purpose:
*     Add a new column definition to a FitsTable.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void AddColumn( AstTable *this, const char *name, int type, int ndim,
*                     int *dims, const char *unit, int *status )

*  Class Membership:
*     FitsTable member function (over-rides the astAddColumn method
*     inherited from the Table class).

*  Description:
*     Adds the definition of a new column to the supplied table. Initially,
*     the column contains a null value for every row. Values may be added
*     subsequently using the methods of the KeyMap class.
*
*     The FitsTable class extend the method inherited from the parent
*     Table class in order to prevent the addition of columns with properties
*     not supported by FITS binary tables.

*  Parameters:
*     this
*        Pointer to the Table.
*     name
*        The column name. Trailing spaces are ignored (all other spaces
*        are significant). The supplied string is converted to upper case.
*     type
*        The data type associated with the column. One of AST__INTTYPE
*        (for integer), AST__SINTTYPE (for short int), AST__BYTETYPE (for
*        unsigned bytes - i.e. unsigned chars), AST__DOUBLETYPE (for double
*        precision floating point), AST__FLOATTYPE (for single precision
*        floating point), AST__STRINGTYPE (for character string). Note,
*        pointers and undefined values cannot be stored in a FitsTable
*        column.
*     ndim
*        The number of dimensions spanned by the values stored in a single
*        cell of the column. Zero if the column holds scalar values.
*     dims
*        An array holding the the lengths of each of the axes spanned by
*        the values stored in a single cell of the column. Ignored if the
*        column holds scalara values.
*     unit
*        A string specifying the units of the column. Supply a blank
*        string if the column is unitless.
*     status
*        Pointer to the inherited status.

*  Notes:
*     - This function returns without action if a column already exists in
*     the Table with the supplied name and properties. However an error is
*     reported if any of the properties differ.
*/

/* Local Variables: */
   const char *text;     /* Data type string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Report an error if the supplied data type is supported by the Table
   class but not by the FitsTable class. */
   if( type == AST__OBJECTTYPE ) {
      text = "Object pointer";

   } else if( type == AST__POINTERTYPE ) {
      text = "generic pointer";

   } else if( type == AST__UNDEFTYPE ) {
      text = "undefined type";

   } else {
      text = NULL;
   }

   if( text ) {
      astError( AST__NAXIN, "astAddColumn(%s): Bad data type (%s) supplied "
                "for new column %s. The %s class does not support %s "
                "columns.", status, astGetClass( this ), text, name,
                astGetClass( this ), text );

/* Otherwise, invoke the parent method to add the column. */
   } else {
      (*parent_addcolumn)( this, name, type, ndim, dims, unit, status );
   }
}

static int ColumnNull( AstFitsTable *this, const char *column, int set,
                       int newval, int *wasset, int *hasnull, int *status ){
/*
*++
*  Name:
c     astColumnNull
f     AST_COLUMNNULL

*  Purpose:
*     Get or set the null value for an integer column of a FITS table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     int astColumnNull( AstFitsTable *this, const char *column, int set,
c                        int newval, int *wasset, int *hasnull )
f     RESULT = AST_COLUMNNULL( THIS, COLUMN, SET, NEWVAL, WASSET, HASNULL,
f                              STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
*     This function allows a null value to be stored with a named
*     integer-valued column in a FitsTable. The supplied null value is
*     assigned to the TNULLn keyword in the FITS header associated with
*     the FitsTable. A value in the named column is then considered to be
*     null if 1) it equals the null value supplied to this function, or
*     2) no value has yet been stored in the cell.
*
*     As well as setting a new null value, this function also returns the
*     previous null value. If no null value has been set previously, a
*     default value will be returned. This default will be an integer
*     value that does not currently occur anywhere within the named column.
*     If no such value can be found, what happens depends on whether the
*     column contains any cells in which no values have yet been stored.
*     If so, an error will be reported. Otherwise (i.e. if there are no
*     null values in the column), an arbitrary value of zero will be
*     returned as the function value, and no TNULLn keyword will be
*     stored in the FITS header.
*
*     A flag is returned indicating if the returned null value was set
*     explicitly by a previous call to this function, or is a default
*     value.
*
*     A second flag is returned indicating if the named column contains
*     any null values (i.e. values equal to the supplied null value, or
*     cells to which no value has yet been assigned).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     column
f     COLUMN = CHARACTER * ( * ) (Given)
*        The character string holding the name of the column. Trailing
*        spaces are ignored.
c     set
f     SET = LOGICAL (Given)
c        If non-zero, the value supplied for parameter "newval"
f        If .TRUE., the value supplied for argument NEWVAL
*        will be stored as the current null value, replacing any value
*        set by a previous call to this function.
c        If zero, the value supplied for parameter "newval"
f        If .FALSE., the value supplied for argument NEWVAL
*        is ignored and the current null value is left unchanged.
c     newval
f     NEWVAL = INTEGER (Given)
*        The new null value to use. Ignored if
c        "set" is zero.
f        SET is .FALSE.
*        An error will be reported if the supplied value is outside the
*        range of values that can be stored in the integer data type
*        associated with the column.
c     wasset
f     WASSET = LOGICAL (Returned)
c        Pointer to an int that will be returned non-zero
f        .TRUE. will be returned
*        if the returned null value was set previously via an
*        earlier invocation of this function.
c        Zero
f        .FALSE.
*        is returned otherwise. If the named column does not exist, or an
*        error occurs, a value of
c        zero is returned.
f        .FALSE. is returned.
c     hasnull
f     HASNULL = LOGICAL (Returned)
c        Pointer to an int that will be returned non-zero
f        .TRUE. will be returned
*        if and only if the named column currently contains any values
*        equal to the null value on exit (i.e.
c        "newval" if "set" is non-zero,
f        NEWVAL if SET is .TRUE.
*        or the returned function value otherwise), or contains any empty
*        cells. If the named column does not exist, or an error occurs, a
*        value of
c        zero is returned.
f        .FALSE. is returned.
c        If a NULL pointer is supplied for "hasnull", no check on the
c        presence of null values will be performed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astColumnNull()
f     AST_COLUMNNULL = INTEGER
*        The null value that was in use on entry to this function. If a
*        null value has been set by a previous invocation of this
*        function, it will be returned. Otherwise, if
c        "set" is non-zero, the supplied "newval"
f        SET is .TRUE., the supplied NEWVAL
*        value is returned. Otherwise, a default value is chosen (if
*        possible) that does not currently occur in the named column. If
*        all available values are in use in the column, an error is
*        reported if and only if the column contains any empty cells.
*        Otherwise, a value of zero is returned. A value of zero is also
*        returned if the named column does not exist, or an error occurs.

*  Notes:
*     - The FITS binary table definition allows only integer-valued
*     columns to have an associated null value. This routine will return
*     without action if the column is not integer-valued.

*--
*/

/* Local Variables: */
   AstKeyMap *col_km;      /* KeyMap holding named column definition */
   AstKeyMap *cols;        /* KeyMap holding all column definitions */
   char key[ AST__MXCOLKEYLEN + 1 ]; /* Current cell key string */
   int *cell;              /* Pointer to array of cell values */
   int foundhi;            /* Has an occurrence of "nullhi" been found yet? */
   int foundlo;            /* Has an occurrence of "nulllo" been found yet? */
   int gotresult;          /* Has a usable value been put into "result"? */
   int idim;               /* Index of current axis in each column's value */
   int iel;                /* Index of current element within cell value */
   int imax;               /* Maximum storable value */
   int imin;               /* Minimum storable value */
   int irow;               /* Index of current row in table */
   int ndim;               /* Number of axes in each column's value */
   int nel;                /* Total number of values in each cell */
   int nrow;               /* Number of rows in table */
   int null;               /* The null value on exit */
   int nullfound;          /* Has a null value been found in the column yet? */
   int nullhi;             /* Higher candidate default null value */
   int nulllo;             /* Lower candidate default null value */
   int result;             /* Returned value */
   int type;               /* Column data type */

/* Initialise */
   result = 0;
   *wasset = 0;
   if( hasnull ) *hasnull = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Store the max and min integer values that can be store din the column
   data type. */
   type = astGetColumnType( this, column );
   if( type == AST__BYTETYPE ) {
      imin = 0;
      imax = UCHAR_MAX;

   } else if( type == AST__SINTTYPE ) {
      imin = SHRT_MIN;
      imax = SHRT_MAX;

   } else if( type == AST__INTTYPE ) {
      imin = INT_MIN;
      imax = INT_MAX;

   } else {
      imax = 0;
      imin = 0;
   }

/* Check the named column contains integer values of any length. */
   if( imax > imin ) {

/* Get the KeyMap holding information about all columns. */
      cols = astColumnProps( this );

/* Get the KeyMap holding information about the named column. */
      if( astMapGet0A( cols, column, &col_km ) ) {

/* If the column definition already includes a null value, put it into
   "result". Also store the "*wasset" flag that indicates if the returned
   null value is a default value or not. */
         *wasset = astMapGet0I( col_km, NULLKEY, &result );

/* If a new null value is to be established... */
         if( set ) {

/* If there was no previously set null value, return the new null value
   as the function value. */
            if( ! *wasset ) result = newval;

/* Indicate we now know what the returned function value is. */
            gotresult = 1;

/* Save the null value that will be in use when this function exits. */
            null = newval;

/* Check the supplied value is in range. If so store it in the column
   keymap. Otherwise report an error. */
            if( null >= imin && null <= imax ) {
               astMapPut0I( col_km, NULLKEY, null, NULL );

            } else if( astOK ) {
               astError( AST__BADNULL, "astColumnNull(%s): Supplied null "
                         "value (%d) is outside the range of integers "
                         "that can be stored in column '%s'.", status,
                         astGetClass( this ), newval, column );
            }

/* If no new null value was supplied, the null value on exit will be the
   previously set value, if any. */
         } else {
            null = result;
            gotresult = *wasset;
         }

/* The rest is only needed if we need to find a default result value, or if
   we need to check if there are any null values in the table. */
         if( !gotresult || hasnull ) {

/* Get the total number of values in each cell of the column. */
            nel = astGetColumnLength( this, column );

/* Allocate memory to hold the values in a single cell of the column,
   stored as ints. */
            cell = astMalloc( nel*sizeof( int ) );

/* No null values found yet. */
            nullfound = 0;

/* On the first pass round the following loop, we search for occurrences
   of the highest and lowest integer values allowed in the column. If no
   such occurrences are found we use one or the other as the default null
   value. If occurrences of both of these values are found, we change the
   values and start the search again. */
            nullhi = imax;
            nulllo = imin;
            foundlo = 0;
            foundhi = 0;

/* Loop round all rows in the Table. */
            nrow = astGetNrow( this );
            for( irow = 1; irow <= nrow && astOK; irow++ ) {

/* Format the cell name. */
               (void) MakeKey( column, irow, key, AST__MXCOLKEYLEN + 1,
                               status );

/* Attempt to get the values in the cell */
               if( astMapGet1I( this, key, nel, &nel, cell ) ) {

/* Get the number of dimensions. */
                  ndim = astGetColumnNdim( this, column );

/* If we know what the null value is on exit, check the cell for such null
   values (but only if the caller want s to know). Skip this check after the
   first null is found. */
                  if( gotresult ) {
                     if( ! nullfound ) {
                        for( idim = 0; idim < ndim; idim++ ) {
                           if( cell[ idim ] == null ) {
                              nullfound = 1;
                              break;
                           }
                        }
                     }

/* If we do not yet know what the returned value is, we try to find an
   integer value within the allowed data range that is not currently used in
   the column. For the moment, this is a no-brain algorithm that will
   become untenable for large tables. Need to fix it when it is apparent
   that it is causing a problem. */
                  } else if( nulllo <= nullhi ) {

/* See if the current cell contains any occurrences of either of the
   two currently nominated null values. Is so, increment the matched
   nominated null value, and start again at row 1. */
                     for( iel = 0; iel < nel; iel++ ) {

                        if( cell[ iel ] == nulllo ) {
                           foundlo = 1;
                        } else if( cell[ iel ] == nullhi ) {
                           foundhi = 1;
                        }

                        if( foundlo && foundhi ) {
                           nullhi--;
                           nulllo++;
                           irow = 0;
                           foundlo = 0;
                           foundhi = 0;
                           continue;
                        }

                     }
                  }

/* If the column does not contain anything in the current cell, we know
   there is at least one null value in the column, so store a non-zero value
   in the returned flag. */
               } else {
                  nullfound = 1;
               }

/* If we now have a value for the result and know that there are nulls in
   the column, we can leave the loop. */
               if( gotresult && nullfound ) break;
            }

/* Return the "null found" flag if required. */
            if( hasnull ) *hasnull = nullfound;

/* If we have not yet stored the default null value to be returned as the
   function value, do so now. If no unused value could be found, and
   there are missing cells in the table, report an error. */
            if( !gotresult ) {
               if( !foundhi ) {
                  result = nullhi;

               } else if( !foundlo ) {
                  result = nulllo;

               } else if( nullfound && astOK ) {
                  astError( AST__BADNULL, "astColumnNull(%s): Cannot find "
                            "an unused value to use as the null value in "
                            "column '%s'.", status, astGetClass( this ),
                            column );
               }
            }

/* Free resources */
            cell = astFree( cell );
         }
         col_km = astAnnul( col_km );
      }
      cols = astAnnul( cols );
   }

/* Return null values if an error occurred. */
   if( !astOK ) {
      result = 0;
      *wasset = 0;
      if( hasnull ) *hasnull = 0;
   }

/* Return the result. */
   return result;
}

static size_t ColumnSize( AstFitsTable *this, const char *column, int *status ){
/*
*++
*  Name:
c     astColumnSize
f     AST_COLUMNSIZE

*  Purpose:
*     Get the number of bytes needed to hold a full column of data.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     size_t astColumnSize( AstFitsTable *this, const char *column,
c                           int *hasnull )
f     RESULT = AST_COLUMNSIZE( THIS, COLUMN, STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
*     This function returns the number of bytes of memory that must be
*     allocated prior to retrieving the data from a column using
c     astGetColumnData.
f     AST_GETCOLUMNDATA.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     column
f     COLUMN = CHARACTER * ( * ) (Given)
*        The character string holding the name of the column. Trailing
*        spaces are ignored.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astColumnNull()
f     AST_COLUMNNULL = INTEGER
*        The number of bytes required to store the column data.

*  Notes:
*     - An error will be reported if the named column does not exist in
*     the FitsTable.
*     - Zero will be returned as the function value in an error occurs.

*--
*/

/* Local Variables: */
   size_t result;          /* Returned value */
   int type;               /* Column data type */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Find the number of bytes needed to hold a single element of the value
   in a column cell. */
   type = astGetColumnType( this, column );
   if( type == AST__INTTYPE ) {
      result = sizeof( int );

   } else if(  type == AST__DOUBLETYPE ){
      result = sizeof( double );

   } else if(  type == AST__STRINGTYPE ){
      result = astGetColumnLenC( this, column )*sizeof( char );

   } else if(  type == AST__FLOATTYPE ){
      result = sizeof( float );

   } else if(  type == AST__SINTTYPE ){
      result = sizeof( short int );

   } else if(  type == AST__BYTETYPE ){
      result = sizeof( char );

   } else if( astOK ) {
      astError( AST__INTER, "astColumnSize(%s): Unsupported column type "
                "%d (internal AST programming error).", status,
                astGetClass( this ), type );
   }

/* Multiply it by the number of elements per value. */
   result *= astGetColumnLength( this, column );

/* Multiply it by the number of values per column (i.e. the number of rows). */
   result *= astGetNrow( this );

/* Return zero if an error occurred. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static void CopyStrings( int nval, size_t nb, const char *cbuf, char *pout,
                         int *status ){
/*
*  Name:
*     CopyStrings

*  Purpose:
*     Remove terminating nulls from an array of fixed-length strings.

*  Type:
*     Private function.

*  Synopsis:
*     void CopyStrings( int nval, size_t nb, const char *cbuf, char *pout,
*                       int *status )

*  Description:
*     This function copies null terminated strings from "cbuf" to "pout",
*     removing the terminating nulls in the process. Thus each output string
*     is one character shorter than the corresponding input string.

*  Parameters:
*     nval
*        The number of strings to copy.
*     nb
*        The maximum length of each string, excluding trailing null.
*     cbuf
*        The input array holding "nval" adjacent strings, each occupying
*        ( nb + 1 ) characters (the last one is the trailing null).
*     pout
*        The output array to which "nval" adjacent strings are written,
*        each occupying ( nb ) characters (i.e. no trailing null).
*     status
*        Pointer to inherited status.

*/

/* Local Variables: */
   int i;

/* Check the global error status. */
   if ( !astOK ) return;

/* Copy the first "nb" characters of each string. */
   for( i = 0; i < nval; i++ ) {
      memcpy( pout, cbuf, nb );

/* Increment the pointer to the start of the next output string. */
      pout += nb;

/* Increment the pointer to the start of the next input string. */
      cbuf += nb + 1;
   }

}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two FitsTables are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitstable.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     FitsTable member function (over-rides the astEqual protected
*     method inherited from the astTable class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two FitsTables are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a FitsTable).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the FitsTables are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFitsTable *that;
   AstFitsTable *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two FitsTable structures. */
   this = (AstFitsTable *) this_object;
   that = (AstFitsTable *) that_object;

/* Check the second object is a FitsTable. We know the first is a
   FitsTable since we have arrived at this implementation of the virtual
   function. */
   if( astIsAFitsTable( that ) ) {

/* Check the FitsTables are equal when compared as Tables. */
      if( (*parent_equal)( this_object, that_object, status ) ) {

/* Check the headers are equal.  */
         result = astEqual( this->header, that->header );
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void GenerateColumns( AstFitsTable *this, AstFitsChan *header,
                             int *status ) {
/*
*  Name:
*     GenerateColumns

*  Purpose:
*     Add new column definitions to a FitsTable as defined by a FITS
*     header.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void GenerateColumns( AstFitsTable *this, AstFitsChan *header,
*                           int *status )

*  Class Membership:
*     FitsTable member function

*  Description:
*     For each binary table column defined in the supplied FITS header,
*     this function adds an equivalent column to the FitsTable.

*  Parameters:
*     this
*        Pointer to the FitsTable.
*     header
*        Pointer to a FitsChan holding the column definitions.
*     status
*        Pointer to the inherited status.

*/

/* Local Variables: */
   char *cval;
   char *name;
   char *p;
   char *unit;
   char buff[ 50 ];
   char code;
   char keyword[ 20 ];
   double dval;
   int *dims;
   int icol;
   int idim;
   int ival;
   int nc;
   int ncol;
   int ndim;
   int nel;
   int repeat;
   int type;
   int wasset;

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise */
   type = AST__BADTYPE;

/* Get the number of columns defined in the header. */
   if( !astGetFitsI( header, "TFIELDS", &ncol ) ) ncol = 0;

/* Add a column definition to the FitsTable for each column in the header. */
   for( icol = 0; icol < ncol; icol++ ) {

/* Get the TFORMi keyword that defines the column data type and shape from
   the header. Report an error if it is missing. */
      sprintf( keyword, "TFORM%d", icol + 1 );
      if( !astGetFitsS( header, keyword, &cval ) && astOK ) {
         astError( AST__NOFTS, "astFitsTable: Supplied FITS binary table "
                   "header does not contain the required keyword '%s'.",
                   status, keyword );
      }

/* Extract the repeat count and data type code from the TFORM string. */
      if( sscanf( cval, "%d%n", &repeat, &nc ) == 0 ) {
         repeat = 1;
         nc = 0;
      } else if( repeat < 0 && astOK ) {
         astError( AST__BDFTS, "astFitsTable: Keyword '%s' in supplied FITS "
                   "binary table header has unsupported value '%s'.", status,
                   keyword, cval );
      }
      code = cval[ nc ];

/* Get the corresponding KeyMap data type. Report an error if the FITS
   data type is not supported by the KeyMap class. */
      if( code == 'B' ) {
         type = AST__BYTETYPE;

      } else if( code == 'I' ) {
         type = AST__SINTTYPE;

      } else if( code == 'J' ) {
         type = AST__INTTYPE;

      } else if( code == 'D' ) {
         type = AST__DOUBLETYPE;

      } else if( code == 'E' ) {
         type = AST__FLOATTYPE;

      } else if( code == 'A' ) {
         type = AST__STRINGTYPE;

      } else if( astOK ){
         astError( AST__BDFTS, "astFitsTable: Keyword '%s' in supplied FITS "
                   "binary table header has unsupported value '%s'.", status,
                   keyword, cval );
      }

/* The TTYPEi keyword gives the column name. Create a column name based
   on the index of the column. */
      sprintf( keyword, "TTYPE%d", icol + 1 );
      if( !astGetFitsS( header, keyword, &cval ) ) {
         sprintf( buff, "FCOLUMN%d", icol + 1 );
         cval = buff;
      }
      name = astStore( NULL, cval, strlen( cval ) + 1 );

/* Column units. */
      sprintf( keyword, "TUNIT%d", icol + 1 );
      if( !astGetFitsS( header, keyword, &cval ) ) {
         buff[ 0 ] = 0;
         cval = buff;
      }
      unit = astStore( NULL, cval, strlen( cval ) + 1 );

/* Column shape is defined by the TDIMi keyword - in the form
   "(i,j,k,...)". where i, j, k ... are the dimensions. If it is missing
   then the field is assumed to be a 1D vector with the length specified by
   the repeat count in the TFORMn keyword, or a scalar (if repeat cound
   is one). */
      sprintf( keyword, "TDIM%d", icol + 1 );
      if( astGetFitsS( header, keyword, &cval ) ) {

/* Count the commas in the keyword value. This equals one less than the
   number of dimensions. */
         ndim = 1;
         p = cval;
         while( *p ) {
            if( *(p++) == ',' ) ndim++;
         }

/* Allocate memory for the dimensions. */
         dims = astMalloc( ndim*sizeof( int ) );

/* Find each dimension and copy it into the above memory. Also find the
   total number of elements (nel). */
         nel = 1;
         idim = 0;
         p = cval;
         if( *p == '(' ) p++;
         while( sscanf( p, "%d%n", dims + idim, &nc ) ) {
            nel *= dims[ idim ];
            idim++;
            p += nc;
            if( *p == ',' ) p++;
         }

/* For strings, the first TDIM value gives the length of the string, so
   reduce the number of dimensions by one. */
         if( type == AST__STRINGTYPE ) {
            ndim--;
            dims++;
         }

      } else {
         nel = repeat;
         if( nel == 1 ) {
            ndim = 0;
            dims = NULL;
         } else {
            ndim = 1;
            dims = astMalloc( sizeof( int ) );
            if( dims ) *dims = nel;
         }
      }

/* Check the total number of elements equal the repeat count from the
   TFORM keyword. */
      if( repeat != nel && astOK ) {

         sprintf( keyword, "TFORM%d", icol + 1 );
         astGetFitsS( header, keyword, &cval );
         strcpy( buff, cval );

         sprintf( keyword, "TDIM%d", icol + 1 );
         if( !astGetFitsS( header, keyword, &cval ) ) cval = " ";

         astError( AST__BDFTS, "astFitsTable: Supplied FITS binary table "
                   "header contains inconsistent TFORM (%s) and TDIM (%s) "
                   "keywords for field %d.", status, buff, cval, icol + 1 );
      }

/* Check any TSCALi value is 1.0 */
      sprintf( keyword, "TSCAL%d", icol + 1 );
      if( astGetFitsF( header, keyword, &dval ) && dval != 1.0 && astOK ) {
         astError( AST__BDFTS, "astFitsTable: Supplied FITS binary table "
                   "header contains scaled columns which are not "
                   "supported by AST.", status );
      }

/* Check any TZEROi value is 0.0 */
      sprintf( keyword, "TSCAL%d", icol + 1 );
      if( astGetFitsF( header, keyword, &dval ) && dval != 0.0 && astOK ) {
         astError( AST__BDFTS, "astFitsTable: Supplied FITS binary table "
                   "header contains scaled columns which are not "
                   "supported by AST.", status );
      }

/* Add the column to the table. */
      astAddColumn( this, name, type, ndim, dims, unit );

/* Set the null value, if present. */
      sprintf( keyword, "TNULL%d", icol + 1 );
      if( astGetFitsI( header, keyword, &ival ) ) {
         (void) astColumnNull( this, name, 1, ival, &wasset, NULL );
      }

/* Free resources. */
      dims = astFree( dims - ( ( type == AST__STRINGTYPE ) ? 1 : 0 ) );
      name = astFree( name );
      unit = astFree( unit );

   }
}

static void GetColumnData( AstFitsTable *this, const char *column,
                           float fnull, double dnull, size_t mxsize,
                           void *coldata, int *nelem, int *status ){
/*
*++
*  Name:
c     astGetColumnData
f     AST_GETCOLUMNDATA

*  Purpose:
*     Retrieve all the data values stored in a column.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astGetColumnData( AstFitsTable *this, const char *column,
c                            float fnull, double dnull, size_t mxsize,
c                            void *coldata, int *nelem )
f     CALL AST_GETCOLUMNDATA( THIS, COLUMN, RNULL, DNULL, MXSIZE,
f                             COLDATA, NELEM, STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
c     This function
f     This routine
*     copies all data values from a named column into a supplied buffer

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsTable.
c     column
f     COLUMN = CHARACTER * ( * ) (Given)
*        The character string holding the name of the column. Trailing
*        spaces are ignored.
c     fnull
f     RNULL = REAL (Given)
*        The value to return in
c        "coldata"
f        COLDATA
*        for any cells for which no value has been stored in the
*        FitsTable. Ignored if the column's data type is not
*        AST__FLOATTYPE. Supplying
c        AST__NANF
f        AST__NANR
*        will cause a single precision IEEE NaN value to be used.
c     dnull
f     DNULL = REAL (Given)
*        The value to return in
c        "coldata"
f        COLDATA
*        for any cells for which no value has been stored in the
*        FitsTable. Ignored if the column's data type is not
*        AST__DOUBLETYPE. Supplying AST__NAN will cause a double precision
*        IEEE NaN value to be used.
c     mxsize
f     MXSIZE = INTEGER (Given)
*        The size of the
c        "coldata"
f        COLDATA
*        array, in bytes. The amount of memory needed to hold the data
*        from a column may be determined using
c        astColumnSize.
f        AST_COLUMNSIZE.
*        If the supplied array is too small to hold all the column data,
*        trailing column values will be omitted from the returned array,
*        but no error will be reported.
c     coldata
f     COLDATA( * ) = BYTE (Given)
c        A pointer to an
f        An
*        area of memory in which to return the data
*        values currently stored in the column. The values are stored in
*        row order. If the column holds non-scalar values, the elements
*        of each value are stored in "Fortran" order. No data type
*        conversion is performed - the data type of each returned value
*        is the data type associated with the column when the column was
*        added to the table. If the column holds strings, the returned
*        strings will be null terminated. Any excess room at the end of
*        the array will be left unchanged.
c     nelem
f     NELEM = INTEGER (Return)
*        The number of elements returned in the
c        "coldata"
f        COLDATA
*        array. This is the product of the number of rows returned and
*        the number of elements in each column value.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
f     - The RNULL and DNULL arguments
c     - The "fnull" and "dnull" parameters
*     specify the value to be returned for any empty cells within columns
*     holding floating point values. For columns holding integer values,
*     the value returned for empty cells is the value returned by the
c     astColumNull function.
f     AST_COLUMNNULL functiom.
*     For columns holding string values, the ASCII NULL character is returned
*     for empty cells.
*--
*/

/* Local Variables: */
   char *cbuf;       /* Array of strings returned by astMapGet1C */
   char key[ AST__MXCOLKEYLEN + 1 ]; /* Current cell key string */
   int iel;          /* Index of current element */
   int irow;         /* Index of value being copied */
   int nel;          /* No. of elements per value */
   int nrow;         /* No. of values to copy */
   int nval;         /* Number of values read from KeyMap entry */
   int ok;           /* Was the value found in the KeyMap? */
   int type;         /* Data type */
   int wasset;       /* Was the integer null value set explicitly? */
   size_t nb;        /* No. of bytes for a single element of a value */
   size_t nbv;       /* No. of bytes per value */
   void *pnull;      /* Pointer to a buffer holding a null value */
   void *pout;       /* Pointer to next output element */

/* Initialise */
   *nelem = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise */
   nb = 0;

/* Find the number of bytes needed to hold a single element of the value
   in a column cell. */
   type = astGetColumnType( this, column );
   if( type == AST__INTTYPE ) {
      nb = sizeof( int );

   } else if( type == AST__DOUBLETYPE ){
      nb = sizeof( double );

   } else if( type == AST__STRINGTYPE ){
      nb = astGetColumnLenC( this, column )*sizeof( char );

   } else if( type == AST__FLOATTYPE ){
      nb = sizeof( float );

   } else if( type == AST__SINTTYPE ){
      nb = sizeof( short int );

   } else if( type == AST__BYTETYPE ){
      nb = sizeof( char );

   } else if( astOK ) {
      astError( AST__INTER, "astGetColumnData(%s): Unsupported column type "
                "%d (internal AST programming error).", status,
                astGetClass( this ), type );
   }

/* Get the number of elements per value, and the number of bytes per value. */
   nel = astGetColumnLength( this, column );
   nbv = nb*nel;

/* Initialise a pointer to the next element of the output array to write to. */
   pout = coldata;

/* Get the number of rows in the table. */
   nrow = astGetNrow( this );

/* For string columns, the buffer returned by astMapGet1C will include a
   null character at the end of each string. This is not required for the
   fixed-length string format used by FITS binary tables, so for each row we
   produce a copy of the string returned by astMapGet1C excluding the
   trailing nulls. Allocate a buffer to receive the string returned by
   astMapGet1C. */
   if(  type == AST__STRINGTYPE ) {
      cbuf = astMalloc( ( nb + 1 )*nel );
   } else {
      cbuf = NULL;
   }

/* If required, substitute NaN values for the supplied null values. */
   fnull = astCheckNaNF( fnull );
   dnull = astCheckNaN( dnull );

/* Indicate we have not yet determined a null value for the column */
   pnull = NULL;

/* Reduce the number of rows to be returned if the returned array is too
   small to hold all rows. */
   if( mxsize < nbv*nrow ) nrow = mxsize/nbv;

/* Loop round the returned rows rows. */
   for( irow = 1; irow <= nrow; irow++ ) {

/* Format the cell name. */
      (void) MakeKey( column, irow, key, AST__MXCOLKEYLEN + 1,
                      status );

/* Get the values in the current cell of the column, using its native
   data type. For floating point, convert any NaNs into the appropriate
   null value (do not need to do this if the null value is itself NaN). */
      if( type == AST__INTTYPE ) {
         ok = astMapGet1I( this, key, nel, &nval, pout );

      } else if(  type == AST__DOUBLETYPE ){
         ok = astMapGet1D( this, key, nel, &nval, pout );

         if( ok && astISFINITE(dnull) ) {
            for( iel = 0; iel < nel; iel++ ) {
               if( !astISFINITE( ((double *)pout)[ iel ] ) ) {
                  ((double *)pout)[ iel ] = dnull;
               }
            }
         }

      } else if(  type == AST__FLOATTYPE ){
         ok = astMapGet1F( this, key, nel, &nval, pout );

         if( ok && astISFINITE(fnull) ) {
            for( iel = 0; iel < nel; iel++ ) {
               if( !astISFINITE( ((float *)pout)[ iel ] ) ) {
                  ((float *)pout)[ iel ] = fnull;
               }
            }
         }

      } else if(  type == AST__SINTTYPE ){
         ok = astMapGet1S( this, key, nel, &nval, pout );

      } else if(  type == AST__BYTETYPE ){
         ok = astMapGet1B( this, key, nel, &nval, pout );

      } else if(  type == AST__STRINGTYPE ){
         ok = astMapGet1C( this, key, nb + 1, nel, &nval, cbuf );

/* Copy the strings returned by astMapGet1C into the returned array,
   omitting the trailing null at the end of each string. */
         CopyStrings( nval, nb, cbuf, pout, status );

      } else {
         ok = 0;
      }

/* If the cell could not be found, return a suitable number of column null
   values. */
      if( !ok ) {

/* Determine the null value to use, if this has not already been done. */
         if( !pnull ) {

/* Allocate a buffer to hold a single null value */
            pnull = astMalloc( nb );
            if( astOK ) {

/* Copy the appropriate null value into the buffer allocated above. */
               if( type == AST__INTTYPE ) {
                  *( (int *) pnull ) = astColumnNull( this, column, 0, 0,
                                                      &wasset, NULL );
               } else if(  type == AST__DOUBLETYPE ){
                  *( (double *) pnull ) = dnull;

               } else if(  type == AST__FLOATTYPE ){
                  *( (float *) pnull ) = fnull;

               } else if(  type == AST__STRINGTYPE ){
                  memset( pnull, 0, nb );

               } else if(  type == AST__SINTTYPE ){
                  *( (short int *) pnull ) = astColumnNull( this, column, 0, 0,
                                                            &wasset, NULL );
               } else if(  type == AST__BYTETYPE ){
                  *( (unsigned char *) pnull ) = astColumnNull( this, column, 0, 0,
                                                                &wasset, NULL );
               }
            }
         }

/* Append the right number of nulls to the returned array. */
         for( iel = 0; iel < nel; iel++ ) {
            memcpy( pout, pnull, nb );
            pout += nb;
         }

/* If the cell was found in the table, just increment the pointer to the next
   returned value. */
      } else {
         pout += nbv;
      }
   }

/* Free resources. */
   cbuf = astFree( cbuf );
   pnull = astFree( pnull );

/* Return the number of returned elements. */
   *nelem = nel*nrow;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitstable.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     FitsTable member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied FitsTables,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the FitsTable.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The FitsTable size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFitsTable *this;            /* Pointer to FitsTable structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the FitsTable structure. */
   this = (AstFitsTable *) this_object;

/* Invoke the GetObjSize method inherited from the parent Table class, and
   then add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astGetObjSize( this->header );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static AstFitsChan *GetTableHeader( AstFitsTable *this, int *status ) {
/*
*++
*  Name:
c     astGetTableHeader
f     AST_GetTableHeader

*  Purpose:
*     Get the FITS headers from a FitsTable.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     AstFitsChan *astGetTableHeader( AstFitsTable *this )
f     RESULT = AST_GETTABLEHEADER( THIS, STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
*     This function returns a pointer to a FitsChan holding copies of
*     the FITS headers associated with a FitsTable.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsTable.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetTableHeader()
f     AST_GetTableHeader = INTEGER
*        A pointer to a deep copy of the FitsChan stored within the
*        FitsTable.

*  Notes:
*     - The returned pointer should be annulled using
c     astAnnul
f     AST_ANNUL
*     when it is no longer needed.
*     - Changing the contents of the returned FitsChan will have no effect
*     on the FitsTable. To modify the FitsTable, the modified FitsChan must
*     be stored in the FitsTable using
c     astPutTableHeader.
f     AST_PUTTABLEHEADER.

*--
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Ensure the fixed value headers are up-to-date in the FitsChan stored
   in the FitsTable. */
   UpdateHeader( this, "astGetTableHeader", status );

/* Reset the current card to the first card. */
   astClearCard( this->header );

/* Return a deep copy of the FitsChan. */
   return astCopy( this->header );
}

void astInitFitsTableVtab_(  AstFitsTableVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitFitsTableVtab

*  Purpose:
*     Initialise a virtual function table for a FitsTable.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitstable.h"
*     void astInitFitsTableVtab( AstFitsTableVtab *vtab, const char *name )

*  Class Membership:
*     FitsTable vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the FitsTable class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstTableVtab *table;          /* Pointer to Table component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitTableVtab( (AstTableVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAFitsTable) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstTableVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->GetTableHeader = GetTableHeader;
   vtab->PutTableHeader = PutTableHeader;
   vtab->ColumnNull = ColumnNull;
   vtab->ColumnSize = ColumnSize;
   vtab->GetColumnData = GetColumnData;
   vtab->PutColumnData = PutColumnData;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   table = (AstTableVtab *) vtab;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

   parent_addcolumn = table->AddColumn;
   table->AddColumn = AddColumn;

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "FitsTable", "FITS binary table" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static char *MakeKey( const char *column, int irow, char *buf, int len,
                      int *status ){
/*
*  Name:
*     MakeKey

*  Purpose:
*     Construct a key for a column cell from a column name and row number.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitstable.h"
*     char *MakeKey( const char *column, int irow, char *buf, int len,
*                    int *status )

*  Class Membership:
*     FitsTable member function

*  Description:
*     This function constructs a key for a column cell from a column name
*     and row number. An error is reported if the buffer is too short.

*  Parameters:
*     column
*        Pointer to the column name. Trailing white space is ignored.
*     irow
*        One-based index of the row.
*     buf
*        Pointer to a buffer in which to store the returned key.
*     len
*        The length of the buffer.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A copy of "buf".

*/

/* Local Variables: */
   char *result;
   char rbuf[ 40 ];
   int collen;
   int nc;

/* Initialise. */
   result = buf;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Format the column number. */
   nc = sprintf( rbuf, "%d", irow );

/* Get the used length of the column name (i.e. excluding trailing white
   space). */
   collen = astChrLen( column );

/* For the total length of the returned string. */
   nc += collen + 3;

/* If the buffer is large enough, store the returned string. */
   if( len >= nc ) {
      sprintf( buf, "%.*s(%s)", collen, column, rbuf );
   } else {
      astError( AST__INTER, "MakeKey(FitsTable): Internal buffer is too "
                "short to hold Table cell name '%.*s(%s)' (internal AST "
                "programming error).", status, collen, column, rbuf );
   }

/* Return the result, */
   return result;
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     FitsTable member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstFitsTable *this;         /* Pointer to FitsTable structure */
   int result;             /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the FitsTable structure. */
   this = (AstFitsTable *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->header, mode, extra, fail );

   return result;

}
#endif

static void PurgeHeader( AstFitsTable *this, int *status ) {
/*
*  Name:
*     PurgeHeader

*  Purpose:
*     Remove fixed-value keywords from the table header.

*  Type:
*     Private function.

*  Synopsis:
*     void PurgeHeader( AstFitsTable *this, int *status )

*  Description:
*     This function ensures that the headers that are determined by the
*     table contents or by the FITS standard do not exist in the header
*     of the supplied FitsTable.

*  Parameters:
*     this
*        Pointer to the FitsTable.
*     status
*        Pointer to inherited status.

*/

/* Local Constants: */
#define nfixed 14  /* Number of fixed-value keywords to check for */

/* Local Variables: */
   int ifixed;

/* A list of FITS keywords that have values that are fixed by the FITS
   standard or by the contents of the Table. */
   const char *fixed[] = { "XTENSION", "BITPIX", "NAXIS", "NAXIS1",
                           "NAXIS2", "PCOUNT", "GCOUNT", "TFIELDS",
                           "TFORM%d", "TTYPE%d", "TNULL%d", "THEAP",
                           "TDIM%d", "TUNIT%d" };

/* Check the global error status. */
   if ( !astOK ) return;

/* Remove headers that have fixed values. */
   for( ifixed = 0; ifixed < nfixed; ifixed++ ) {
      astClearCard( this->header );
      while( astFindFits( this->header, fixed[ ifixed ], NULL, 0 ) ) {
         astDelFits( this->header );
      }
   }

/* Undefine local constants */
#undef nfixed
}

static void PutColumnData( AstFitsTable *this, const char *column,
                           int clen, size_t size, void *coldata, int *status ){
/*
*++
*  Name:
c     astPutColumnData
f     AST_PUTCOLUMNDATA

*  Purpose:
*     Store new data values for all rows of a column.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astPutColumnData( AstFitsTable *this, const char *column,
c                            int clen, size_t size, void *coldata )
f     CALL AST_PUTCOLUMNDATA( THIS, COLUMN, CLEN, SIZE, COLDATA, STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
c     This function
f     This routine
*     copies data values from a supplied buffer into a named column. The
*     first element in the buffer becomes the first element in the first
*     row of the column. If the buffer does not completely fill the
*     column, then any trailing rows are filled with null values.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsTable.
c     column
f     COLUMN = CHARACTER * ( * ) (Given)
*        The character string holding the name of the column. Trailing
*        spaces are ignored.
c     clen
f     CLEN = INTEGER (Given)
*        If the column holds character strings, then this must be set to
*        the length of each fixed length string in the supplied array.
*        This is often determined by the appropriate TFORMn keyword in
*        the binary table header. The supplied value is ignored if the
*        column does not hold character data.
c     size
f     SIZE = INTEGER (Given)
*        The size of the
c        "coldata"
f        COLDATA
*        array, in bytes. This should be an integer multiple of the
*        number of bytes needed to hold the full vector value stored in a
*        single cell of the column. An error is reported if this is not
*        the case.
c     coldata
f     COLDATA( * ) = BYTE (Given)
c        A pointer to an
f        An
*        area of memory holding the data to copy into the column. The values
*        should be stored in row order. If the column holds non-scalar values,
*        the elements of each value should be stored in "Fortran" order. No
*        data type conversion is performed.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*--
*/

/* Local Variables: */
   char key[ AST__MXCOLKEYLEN + 1 ]; /* Current cell key string */
   char **carray;    /* Pointer to array of null terminated string pointers */
   int irow;         /* Index of value being copied */
   int iel;          /* Index of current element */
   int nel;          /* No. of elements per value */
   int nrow;         /* No. of values to copy */
   int type;         /* Data type */
   size_t nb;        /* No. of bytes for a single element of a value */
   size_t nbv;       /* No. of bytes per value */
   void *pin;        /* Pointer to next input array element */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise */
   nb = 0;

/* Find the number of bytes in the supplied array holding a single element
   of the value in a column cell. */
   type = astGetColumnType( this, column );
   if( type == AST__INTTYPE ) {
      nb = sizeof( int );

   } else if(  type == AST__DOUBLETYPE ){
      nb = sizeof( double );

   } else if(  type == AST__STRINGTYPE ){
      nb = clen*sizeof( char );

   } else if(  type == AST__FLOATTYPE ){
      nb = sizeof( float );

   } else if(  type == AST__SINTTYPE ){
      nb = sizeof( short int );

   } else if(  type == AST__BYTETYPE ){
      nb = sizeof( char );

   } else if( astOK ) {
      astError( AST__INTER, "astPutColumnData(%s): Unsupported column type "
                "%d (internal AST programming error).", status,
                astGetClass( this ), type );
   }

/* Get the number of elements per value, and the number of bytes (in the
   supplied array) per value. */
   nel = astGetColumnLength( this, column );
   nbv = nb*nel;

/* Initialise a pointer to the next element of the supplied array to read. */
   pin = coldata;

/* Get the number of rows to copy from the supplied array. */
   nrow = nbv ? size / nbv : 0;

/* As yet we have no array of null terminated character pointers. */
   carray = NULL;

/* Report an error if the supplied array does not hold an exact number of
   column cells. */
   if( nrow*nbv != size && astOK ) {
      astError( AST__BADSIZ, "astPutColumnData(%s): The supplied array size "
                "(%d bytes) is not an exact multiple of the size of one "
                "column value (%d bytes).", status, astGetClass( this ),
                (int) size, (int) nbv );
   }

/* Loop round the rows to be copied. */
   for( irow = 1; irow <= nrow; irow++ ) {

/* Format the cell name. */
      (void) MakeKey( column, irow, key, AST__MXCOLKEYLEN + 1,
                      status );

/* Put the next value into the current cell of the column, using its native
   data type. Skip floating point values that are entirely NaN. */
      if( type == AST__INTTYPE ) {
         astMapPut1I( this, key, nel, pin, NULL );

      } else if(  type == AST__DOUBLETYPE ){
         for( iel = 0; iel < nel; iel++ ) {
            if( astISFINITE( ((double *)pin)[ iel ] ) ) {
               astMapPut1D( this, key, nel, pin, NULL );
               break;
            }
         }

      } else if(  type == AST__FLOATTYPE ){
         for( iel = 0; iel < nel; iel++ ) {
            if( astISFINITE( ((double *)pin)[ iel ] ) ) {
               astMapPut1F( this, key, nel, pin, NULL );
               break;
            }
         }

      } else if(  type == AST__SINTTYPE ){
         astMapPut1S( this, key, nel, pin, NULL );

      } else if(  type == AST__BYTETYPE ){
         astMapPut1B( this, key, nel, pin, NULL );

/* If each cell in the column holds an array of strings, we need to
   convert the fixed length strings in the supplied array into an array
   of pointers to null terminated strings. */
      } else if(  type == AST__STRINGTYPE ){
         carray = astStringArray( pin, nel, clen );
         astMapPut1C( this, key, nel, (const char ** ) carray, NULL );
         carray = astFree( carray );
      }

/* Increment the pointer to the next input value. */
      pin += nbv;
   }

/* Remove any remaining cells already present in this column. */
   nrow = astGetNrow( this );
   for( ; irow <= nrow; irow++ ) {
      (void) MakeKey( column, irow, key, AST__MXCOLKEYLEN + 1,
                      status );
      astMapRemove( this, key );
   }
}

static void PutTableHeader( AstFitsTable *this, AstFitsChan *header,
                            int *status ) {
/*
*++
*  Name:
c     astPutTableHeader
f     AST_PUTTABLEHEADER

*  Purpose:
*     Store new FITS headers in a FitsTable.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astPutTableHeader( AstFitsTable *this, AstFitsChan *header )
f     CALL AST_PUTTABLEHEADER( THIS, HEADER, STATUS )

*  Class Membership:
*     FitsTable method.

*  Description:
c     This function
f     This routine
*     stores new FITS headers in the supplied FitsTable. Any existing
*     headers are first deleted.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FitsTable.
c     header
f     HEADER = INTEGER (Given)
*        Pointer to a FitsChan holding the headers for the FitsTable.
*        A deep copy of the supplied FitsChan is stored in the FitsTable,
*        replacing the current FitsChan in the Fitstable. Keywords that
*        are fixed either by the properties of the Table, or by the FITS
*        standard, are removed from the copy (see "Notes:" below).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - The attributes of the supplied FitsChan, together with any source
*     and sink functions associated with the FitsChan, are copied to the
*     FitsTable.
*     - Values for the following keywords are generated automatically by
*     the FitsTable (any values for these keywords in the supplied
*     FitsChan will be ignored): "XTENSION", "BITPIX", "NAXIS", "NAXIS1",
*     "NAXIS2", "PCOUNT", "GCOUNT", "TFIELDS", "TFORM%d", "TTYPE%d",
*     "TNULL%d", "THEAP", "TDIM%d".

*--
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Annul the existing FitsChan. */
   (void) astAnnul( this->header );

/* Store a deep copy of the supplied FitsChan in the FitsTable. */
   this->header = astCopy( header );

/* Remove headers that have fixed values. */
   PurgeHeader( this, status );
}

static void UpdateHeader( AstFitsTable *this, const char *method,
                          int *status ) {
/*
*  Name:
*     UpdateHeader

*  Purpose:
*     Ensure FITS headers accurately describe the current table contents.

*  Type:
*     Private function.

*  Synopsis:
*     void UpdateHeader( AstFitsTable *this, const char *method,
*                        int *status )

*  Description:
*     This function ensures that the FITS headers that are determined by
*     the table contents or by the FITS standard are up to date.

*  Parameters:
*     this
*        Pointer to the FitsTable.
*     method
*        Pointer to a string holding the name of the method to include in
*        error messages.
*     status
*        Pointer to inherited status.

*/

/* Local Variables: */
   char *dimbuf;
   char buf[ 20 ];
   char code;
   char keyword[ 14 ];
   const char *unit;
   const char *name;
   int *dims;
   int hasNull;
   int icol;
   int idim;
   int nc;
   int ncol;
   int ndim;
   int nel;
   int null;
   int rowsize;
   int set;
   int slen;
   int type;

/* Check inherited status */
   if( !astOK ) return;

/* Remove any existing headers that will have values stored for them by
   this function. */
   PurgeHeader( this, status );

/* Store headers that have fixed values regardless of the contents of the
   table. Rewind the FitsChan first so they go at the start of the header. */
   astClearCard( this->header );
   astSetFitsS( this->header, "XTENSION", "BINTABLE", NULL, 0 );
   astSetFitsI( this->header, "BITPIX", 8, NULL, 0 );
   astSetFitsI( this->header, "NAXIS", 2, NULL, 0 );
   astSetFitsI( this->header, "PCOUNT", 0, NULL, 0 );
   astSetFitsI( this->header, "GCOUNT", 1, NULL, 0 );

/* The number of columns. */
   ncol = astGetNcolumn( this );
   astSetFitsI( this->header, "TFIELDS", ncol, NULL, 0 );

/* Add column-specific keywords, one for each column in the Table. */
   dims = NULL;
   dimbuf = NULL;
   rowsize = 0;
   for( icol = 1; icol <= ncol && astOK; icol++ ){

/* Get the name, type and shape of the current column. */
      name = astColumnName( this, icol );
      nel = astGetColumnLength( this, name );
      type = astGetColumnType( this, name );
      unit = astGetColumnUnit( this, name );
      ndim = astGetColumnNdim( this, name );
      dims = astGrow( dims, ndim, sizeof( int ) );
      if( astOK ) {
         astColumnShape( this, name, ndim, &ndim, dims );

/* Get the FITS code that describes the column data type. Also increment
   the number of bytes (rowsize) used to describe a whole row. */
         slen = 0;
         code = ' ';
         if( type == AST__BYTETYPE ) {
            code = 'B';
            rowsize += nel;

         } else if( type == AST__SINTTYPE ) {
            code = 'I';
            rowsize += 2*nel;

         } else if( type == AST__INTTYPE ) {
            code = 'J';
            rowsize += 4*nel;

         } else if( type == AST__DOUBLETYPE ) {
            code = 'D';
            rowsize += 8*nel;

         } else if( type == AST__FLOATTYPE ) {
            code = 'E';
            rowsize += 4*nel;

         } else if( type == AST__STRINGTYPE ) {
            code = 'A';

/* Use the maximum length of the strings in the current column (excluding
   null) to scale the FITS repeat count. */
            slen = astGetColumnLenC( this, name );
            nel *= slen;
            rowsize += nel;

/* Report an error if the data type is not supported by FITS. */
         } else if( astOK ) {
            astError( AST__INTER, "%s(%s): Illegal type %d for column '%s' "
                      "in a %s (internal AST programming error).", status,
                      method, astGetClass( this ), type, name,
                      astGetClass( this ) );
         }

/* Start the TFORMn keyword value. This is the number of values in each
   cell, and is not needed if the cell contains only one value. */
         nc = sprintf( buf, "%d", nel );

/* Add the data type code to complete the TFORMn value, and store it in
   the FitsChan. */
         sprintf( buf + nc, "%c", code );
         sprintf( keyword, "TFORM%d", icol );
         astSetFitsS( this->header, keyword, buf, NULL, 0 );

/* Column name. */
         sprintf( keyword, "TTYPE%d", icol );
         astSetFitsS( this->header, keyword, name, NULL, 0 );

/* Column units. */
         if( astChrLen( unit ) ) {
            sprintf( keyword, "TUNIT%d", icol );
            astSetFitsS( this->header, keyword, unit, NULL, 0 );
         }

/* Column null value (integer columns only). Only store a TNULLn keyword
   if the NULL attribute has been set for the column, or if the column
   contains missing (i.e. null) values. */
         if( type == AST__BYTETYPE || type == AST__SINTTYPE ||
             type == AST__INTTYPE ) {
            null = astColumnNull( this, name, 0, 0, &set, &hasNull );
            if( set || hasNull ) {
               sprintf( keyword, "TNULL%d", icol );
               astSetFitsI( this->header, keyword, null, NULL, 0 );
            }
         }

/* Array dimensions (only needed for non-scalars). */
         if( ndim > 0 ) {
            dimbuf = astGrow( dimbuf, ndim, 15 );
            if( astOK ) {

/* For strings, the first dimension is the length of the fixed-length
   strings that make up the array. */
               if( type != AST__STRINGTYPE ) {
                  nc = sprintf( dimbuf, "(%d", dims[ 0 ] );
               } else {
                  nc = sprintf( dimbuf, "(%d,%d", slen, dims[ 0 ] );
               }

/* Append the second and subsequent dimensions to the buffer. */
               for( idim = 1; idim < ndim; idim++ ) {
                  nc += sprintf( dimbuf + nc, ",%d", dims[ idim ] );
               }
               sprintf( dimbuf + nc, ")" );

/* Store the buffered string as the value for keyword TDIMn in the
   FitsChan. */
               sprintf( keyword, "TDIM%d", icol );
               astSetFitsS( this->header, keyword, dimbuf, NULL, 0 );
            }
         }
      }
   }

/* Insert the NAXISi keywords into the header, following the NAXIS value
   (i.e. starting at card 4). */
   astSetCard( this->header, 4 );
   astSetFitsI( this->header, "NAXIS1", rowsize, NULL, 0 );
   astSetFitsI( this->header, "NAXIS2", astGetNrow( this ), NULL, 0 );

/* Free resources. */
   dims = astFree( dims );
   dimbuf = astFree( dimbuf );
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for FitsTable objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for FitsTable objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy, including a copy of the component
*     Mappings within the FitsTable.
*/

/* Local Variables: */
   AstFitsTable *in;                /* Pointer to input FitsTable */
   AstFitsTable *out;               /* Pointer to output FitsTable */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output FitsTables. */
   in = (AstFitsTable *) objin;
   out = (AstFitsTable *) objout;

/* Make copies of the component Tables and store pointers to them in the
   output FitsTable structure. */
   out->header = astCopy( in->header );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for FitsTable objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for FitsTable objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstFitsTable *this;              /* Pointer to FitsTable */

/* Obtain a pointer to the FitsTable structure. */
   this = (AstFitsTable *) obj;

/* Annul the pointers to the component Tables. */
   this->header = astAnnul( this->header );

}


/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for FitsTable objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the FitsTable class to an output Channel.

*  Parameters:
*     this
*        Pointer to the FitsTable whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFitsTable *this;             /* Pointer to the FitsTable structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FitsTable structure. */
   this = (AstFitsTable *) this_object;

/* Write out values representing the instance variables for the FitsTable
   class.  Note, the primitive data in the FitsTable will be written out
   by the parent Table Dump function. This function deals just with the
   extra information held in the FitsTable structure. */

/* Write out the FITS header. */
   astWriteObject( channel, "Header", 1, 0, this->header, "FITS headers" );
}










/* Standard class functions. */
/* ========================= */
/* Implement the astIsAFitsTable and astCheckFitsTable functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(FitsTable,Table)
astMAKE_CHECK(FitsTable)

AstFitsTable *astFitsTable_( void *header_void, const char *options, int *status, ...) {
/*
*++
*  Name:
c     astFitsTable
f     AST_FITSTABLE

*  Purpose:
*     Create a FitsTable.

*  Type:
*     Public function.

*  Synopsis:
c     #include "fitstable.h"
c     AstFitsTable *astFitsTable( AstFitsChan *header, const char *options, ... )
f     RESULT = AST_FITSTABLE( HEADER, OPTIONS, STATUS )

*  Class Membership:
*     FitsTable constructor.

*  Description:
*     This function creates a new FitsTable and optionally initialises
*     its attributes.
*
*     The FitsTable class is a representation of a FITS binary table. It
*     inherits from the Table class. The parent Table is used to hold the
*     binary data of the main table, and a FitsChan is used to hold the FITS
*     header. Note, there is no provision for binary data following the main
*     table (such data is referred to as a "heap" in the FITS standard).
*
*     Note - it is not recommended to use the FitsTable class to store
*     very large tables.

*  Parameters:
c     header
f     HEADER = INTEGER (Given)
*        Pointer to an optional FitsChan containing headers to be stored
*        in the FitsTable.
c        NULL
f        AST__NULL
*        may be supplied if the new FitsTable is to be left empty. If
*        supplied, and if the headers describe columns of a FITS binary
*        table, then equivalent (empty) columns are added to the FitsTable.
*        Each column has the same index in the FitsTable that it has in
*        the supplied header.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new FitsTable. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new FitsTable. The syntax used is identical to that for the
f        AST_SET routine.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFitsTable()
f     AST_FITSTABLE = INTEGER
*        A pointer to the new FitsTable.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list described above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFitsTable *new;            /* Pointer to new FitsTable */
   AstFitsChan *header;          /* Pointer to header FitsChan */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate pointers to the header FitsChan provided. */
   header = header_void ? astCheckFitsChan( header_void ) : NULL;

/* Initialise the FitsTable, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitFitsTable( NULL, sizeof( AstFitsTable ), !class_init,
                           &class_vtab, "FitsTable", header );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new FitsTable's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new FitsTable. */
   return new;
}

AstFitsTable *astFitsTableId_( void *header_void, const char *options, ... ) {
/*
*  Name:
*     astFitsTableId_

*  Purpose:
*     Create a FitsTable.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fitstable.h"
*     AstFitsTable *astFitsTableId_( AstFitsChan *header, const char *options, ... )

*  Class Membership:
*     FitsTable constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astFitsTable constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astFitsTable_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astFitsTable_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astFitsTable_.

*  Returned Value:
*     The ID value associated with the new FitsTable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFitsChan *header;          /* Genuine C poitner to header FitsChan */
   AstFitsTable *new;            /* Pointer to new FitsTable */
   int *status;                  /* Pointer to inherited status value */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain a FitsChan pointer from any ID supplied and validate the
   pointer to ensure it identifies a valid FitsChan. */
   header = header_void ? astCheckFitsChan( astMakePointer( header_void ) ) : NULL;

/* Initialise the FitsTable, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitFitsTable( NULL, sizeof( AstFitsTable ), !class_init,
                           &class_vtab, "FitsTable", header );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new FitsTable's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new FitsTable. */
   return astMakeId( new );
}

AstFitsTable *astInitFitsTable_( void *mem, size_t size, int init,
                                 AstFitsTableVtab *vtab, const char *name,
                                 AstFitsChan *header, int *status ) {
/*
*+
*  Name:
*     astInitFitsTable

*  Purpose:
*     Initialise a FitsTable.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitstable.h"
*     AstFitsTable *astInitFitsTable( void *mem, size_t size, int init,
*                                     AstFitsTableVtab *vtab, const char *name,
*                                     AstFitsChan *header )

*  Class Membership:
*     FitsTable initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new FitsTable object. It allocates memory (if necessary) to accommodate
*     the FitsTable plus any additional data associated with the derived class.
*     It then initialises a FitsTable structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a FitsTable at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the FitsTable is to be initialised.
*        This must be of sufficient size to accommodate the FitsTable data
*        (sizeof(FitsTable)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the FitsTable (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the FitsTable
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the FitsTable's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new FitsTable.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     header
*        If not NULL, a FitsChan that is used to populate the FitsTable
*        with headers and columns.

*  Returned Value:
*     A pointer to the new FitsTable.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstFitsTable *new;              /* Pointer to new FitsTable */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitFitsTableVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a Table structure (the parent class) as the first component
   within the FitsTable structure, allocating memory if necessary. Specify that
   the Table should be defined in both the forward and inverse directions. */
   new = (AstFitsTable *) astInitTable( mem, size, 0, (AstTableVtab *) vtab,
                                     name );
   if ( astOK ) {

/* Initialise the FitsTable data. */
/* ---------------------------- */
      new->header = astFitsChan( NULL, NULL, " ", status );

/* If a header was supplied, add equivalent columns to the FitsTable, and
   store the header. */
      if( header ) {
         GenerateColumns( new, header, status );
         PutTableHeader( new, header, status );
      }

/* If an error occurred, clean up by deleting the new FitsTable. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new FitsTable. */
   return new;
}

AstFitsTable *astLoadFitsTable_( void *mem, size_t size, AstFitsTableVtab *vtab,
                                 const char *name, AstChannel *channel,
                                 int *status ) {
/*
*+
*  Name:
*     astLoadFitsTable

*  Purpose:
*     Load a FitsTable.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fitstable.h"
*     AstFitsTable *astLoadFitsTable( void *mem, size_t size, AstFitsTableVtab *vtab,
*                                     const char *name, AstChannel *channel )

*  Class Membership:
*     FitsTable loader.

*  Description:
*     This function is provided to load a new FitsTable using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     FitsTable structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a FitsTable at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the FitsTable is to be
*        loaded.  This must be of sufficient size to accommodate the
*        FitsTable data (sizeof(FitsTable)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FitsTable (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FitsTable structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstFitsTable) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FitsTable. If this is NULL, a pointer
*        to the (static) virtual function table for the FitsTable class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "FitsTable" is used instead.

*  Returned Value:
*     A pointer to the new FitsTable.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFitsTable *new;                /* Pointer to the new FitsTable */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this FitsTable. In this case the
   FitsTable belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstFitsTable );
      vtab = &class_vtab;
      name = "FitsTable";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitFitsTableVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built FitsTable. */
   new = astLoadTable( mem, size, (AstTableVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "FitsTable" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* FitsChan holding table headers. */
      new->header = astReadObject( channel, "header", NULL );

/* If an error occurred, clean up by deleting the new FitsTable. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new FitsTable pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */

AstFitsChan *astGetTableHeader_( AstFitsTable *this, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,FitsTable,GetTableHeader))(this,status);
}

void astPutTableHeader_( AstFitsTable *this, AstFitsChan *header, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FitsTable,PutTableHeader))(this,header,status);
}

int astColumnNull_( AstFitsTable *this, const char *column, int set,
                    int newval, int *wasset, int *hasnull, int *status ){
   *wasset = 0;
   if( hasnull ) *hasnull = 0;
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FitsTable,ColumnNull))(this,column,set,newval,wasset,hasnull,status);
}

size_t astColumnSize_( AstFitsTable *this, const char *column, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FitsTable,ColumnSize))(this,column,status);
}

void astGetColumnData_( AstFitsTable *this, const char *column, float fnull,
                        double dnull, size_t mxsize, void *coldata, int *nelem,
                        int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,FitsTable,GetColumnData))(this,column,fnull,dnull,mxsize,
                                               coldata,nelem,status);
}

void astPutColumnData_( AstFitsTable *this, const char *column, int clen,
                        size_t size, void *coldata, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,FitsTable,PutColumnData))(this,column,clen,size,coldata,status);
}








