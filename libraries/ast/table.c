/*
*class++
*  Name:
*     Table

*  Purpose:
*     A 2-dimensional table of values.

*  Constructor Function:
c     astTable
f     AST_TABLE

*  Description:
*     The Table class is a type of KeyMap that represents a two-dimensional
*     table of values. The
c     astMapGet... and astMapPut...
f     AST_MAPGET... and AST_MAPPUT...
*     methods provided by the KeyMap class should be used for storing and
*     retrieving values from individual cells within a Table. Each entry
*     in the KeyMap represents a single cell of the table and has an
*     associated key of the form "<COL>(i)" where "<COL>" is the name of a
*     table column and "i" is the row index (the first row is row 1). Keys
*     of this form should always be used when using KeyMap methods to access
*     entries within a Table.
*
*     Columns must be declared using the
c     astAddColumn
f     AST_ADDCOLUMN
*     method before values can be stored within them. This also fixes the
*     type and shape of the values that may be stored in any cell of the
*     column. Cells may contain scalar or vector values of any data type
*     supported by the KeyMap class. Multi-dimensional arrays may also be
*     stored, but these must be vectorised when storing and retrieving
*     them within a table cell. All cells within a single column must
*     have the same type and shape (specified when the column is declared).
*
*     Note - since accessing entries within a KeyMap is a relatively slow
*     process, it is not recommended to use the Table class to store
*     very large tables.

*  Inheritance:
*     The Table class inherits from the KeyMap class.

*  Attributes:
*     In addition to those attributes common to all KeyMaps, every
*     Table also has the following attributes:
*
*     - NColumn: The number of columns currently in the Table
*     - NRow: The number of rows currently in the Table

*  Functions:
c     In addition to those functions applicable to all KeyMaps, the
c     following functions may also be applied to all Tables:
f     In addition to those routines applicable to all KeyMaps, the
f     following routines may also be applied to all Tables:
*
c     - astAddColumn: Add a new column definition to a Table.
c     - astColumnName: Return the name of the column with a given index
c     - astColumnShape: Return the shape of the values in a named column
c     - astColumnType: Return the data type of the values in a named column
c     - astRemoveColumn: Remove a column from a Table.
c     - astRemoveRow: Remove a row from a Table.
f     - AST_ADDCOLUMN: Add a new column definition to a Table.
f     - AST_COLUMNNAME: Return the name of the column with a given index
f     - AST_COLUMNSHAPE: Return the shape of the values in a named column
f     - AST_COLUMNTYPE: Return the data type of the values in a named column
f     - AST_REMOVECOLUMN: Remove a column from a Table.
f     - AST_REMOVEROW: Remove a row from a Table.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     22-NOV-2010 (DSB):
*        Original version.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Table

/* Fixed KeyMap entry names */
#define NAME "Name"
#define TYPE "Type"
#define SHAPE "Shape"
#define NROW "NRow"

/* Maximum length of a column name */
#define MXCOLNAMLEN 100

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "keymap.h"              /* Coordinate keymaps (parent class) */
#include "channel.h"             /* I/O channels */
#include "table.h"               /* Interface definition for this class */


/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>


/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_equal)( AstObject *, AstObject *, int * );
static int (* parent_getobjsize)( AstObject *, int * );
static int (* parent_mapget0a)( AstKeyMap *, const char *, AstObject * *, int *);
static int (* parent_mapget0c)( AstKeyMap *, const char *, const char **, int *);
static int (* parent_mapget0d)( AstKeyMap *, const char *, double *, int *);
static int (* parent_mapget0f)( AstKeyMap *, const char *, float *, int *);
static int (* parent_mapget0i)( AstKeyMap *, const char *, int *, int *);
static int (* parent_mapget0p)( AstKeyMap *, const char *, void **, int *);
static int (* parent_mapget0s)( AstKeyMap *, const char *, short int *, int *);
static int (* parent_mapget1a)( AstKeyMap *, const char *, int, int *, AstObject **, int * );
static int (* parent_mapget1c)( AstKeyMap *, const char *, int, int, int *, char *, int * );
static int (* parent_mapget1d)( AstKeyMap *, const char *, int, int *, double *, int * );
static int (* parent_mapget1f)( AstKeyMap *, const char *, int, int *, float *, int * );
static int (* parent_mapget1i)( AstKeyMap *, const char *, int, int *, int *, int * );
static int (* parent_mapget1p)( AstKeyMap *, const char *, int, int *, void **, int * );
static int (* parent_mapget1s)( AstKeyMap *, const char *, int, int *, short int *, int * );
static int (* parent_mapgetelema)( AstKeyMap *, const char *, int, AstObject **, int * );
static int (* parent_mapgetelemc)( AstKeyMap *, const char *, int, int, char *, int * );
static int (* parent_mapgetelemd)( AstKeyMap *, const char *, int, double *, int * );
static int (* parent_mapgetelemf)( AstKeyMap *, const char *, int, float *, int * );
static int (* parent_mapgetelemi)( AstKeyMap *, const char *, int, int *, int * );
static int (* parent_mapgetelemp)( AstKeyMap *, const char *, int, void **, int * );
static int (* parent_mapgetelems)( AstKeyMap *, const char *, int, short int *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_mapput0a)( AstKeyMap *, const char *, AstObject *, const char *, int *);
static void (* parent_mapput0c)( AstKeyMap *, const char *, const char *, const char *, int *);
static void (* parent_mapput0d)( AstKeyMap *, const char *, double, const char *, int *);
static void (* parent_mapput0f)( AstKeyMap *, const char *, float, const char *, int *);
static void (* parent_mapput0i)( AstKeyMap *, const char *, int, const char *, int *);
static void (* parent_mapput0p)( AstKeyMap *, const char *, void *, const char *, int *);
static void (* parent_mapput0s)( AstKeyMap *, const char *, short int, const char *, int *);
static void (* parent_mapput1a)( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
static void (* parent_mapput1c)( AstKeyMap *, const char *, int, const char *const [], const char *, int * );
static void (* parent_mapput1d)( AstKeyMap *, const char *, int, const double *, const char *, int * );
static void (* parent_mapput1f)( AstKeyMap *, const char *, int, const float *, const char *, int * );
static void (* parent_mapput1i)( AstKeyMap *, const char *, int, const int *, const char *, int * );
static void (* parent_mapput1p)( AstKeyMap *, const char *, int, void *const [], const char *, int * );
static void (* parent_mapput1s)( AstKeyMap *, const char *, int, const short int *, const char *, int * );
static void (* parent_mapputelema)( AstKeyMap *, const char *, int, AstObject *, int * );
static void (* parent_mapputelemc)( AstKeyMap *, const char *, int, const char *, int * );
static void (* parent_mapputelemd)( AstKeyMap *, const char *, int, double, int * );
static void (* parent_mapputelemf)( AstKeyMap *, const char *, int, float, int * );
static void (* parent_mapputelemi)( AstKeyMap *, const char *, int, int, int * );
static void (* parent_mapputelemp)( AstKeyMap *, const char *, int, void *, int * );
static void (* parent_mapputelems)( AstKeyMap *, const char *, int, short int, int * );
static void (* parent_mapremove)( AstKeyMap *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static void (* parent_mapputu)( AstKeyMap *, const char *, const char *, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif


/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Table)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Table,Class_Init)
#define class_vtab astGLOBAL(Table,Class_Vtab)
#define getattrib_buff astGLOBAL(Table,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];

/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstTableVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstTable *astTableId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static const char *TypeString( int );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static int GetNColumn( AstTable *, int * );
static int MapGet0A( AstKeyMap *, const char *, AstObject **, int * );
static int MapGet0C( AstKeyMap *, const char *, const char **, int * );
static int MapGet0D( AstKeyMap *, const char *, double *, int * );
static int MapGet0F( AstKeyMap *, const char *, float *, int * );
static int MapGet0I( AstKeyMap *, const char *, int *, int * );
static int MapGet0P( AstKeyMap *, const char *, void **, int * );
static int MapGet0S( AstKeyMap *, const char *, short int *, int * );
static int MapGet1A( AstKeyMap *, const char *, int, int *, AstObject **, int * );
static int MapGet1C( AstKeyMap *, const char *, int, int, int *, char *, int * );
static int MapGet1D( AstKeyMap *, const char *, int, int *, double *, int * );
static int MapGet1F( AstKeyMap *, const char *, int, int *, float *, int * );
static int MapGet1I( AstKeyMap *, const char *, int, int *, int *, int * );
static int MapGet1P( AstKeyMap *, const char *, int, int *, void **, int * );
static int MapGet1S( AstKeyMap *, const char *, int, int *, short int *, int * );
static int MapGetElemA( AstKeyMap *, const char *, int, AstObject **, int * );
static int MapGetElemC( AstKeyMap *, const char *, int, int, char *, int * );
static int MapGetElemD( AstKeyMap *, const char *, int, double *, int * );
static int MapGetElemF( AstKeyMap *, const char *, int, float *, int * );
static int MapGetElemI( AstKeyMap *, const char *, int, int *, int * );
static int MapGetElemP( AstKeyMap *, const char *, int, void **, int * );
static int MapGetElemS( AstKeyMap *, const char *, int, short int *, int * );
static int ParseKey( AstTable *, const char *, int, char *, int *, AstKeyMap **, const char *, int * );
static void AddColumn( AstTable *, const char *, int, int, int *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void MapPut0A( AstKeyMap *, const char *, AstObject *, const char *, int * );
static void MapPut0C( AstKeyMap *, const char *, const char *, const char *, int * );
static void MapPut0D( AstKeyMap *, const char *, double, const char *, int * );
static void MapPut0F( AstKeyMap *, const char *, float, const char *, int * );
static void MapPut0I( AstKeyMap *, const char *, int, const char *, int * );
static void MapPut0P( AstKeyMap *, const char *, void *, const char *, int * );
static void MapPut0S( AstKeyMap *, const char *, short int, const char *, int * );
static void MapPut1A( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
static void MapPut1C( AstKeyMap *, const char *, int, const char *const [], const char *, int * );
static void MapPut1D( AstKeyMap *, const char *, int, const double *, const char *, int * );
static void MapPut1F( AstKeyMap *, const char *, int, const float *, const char *, int * );
static void MapPut1I( AstKeyMap *, const char *, int, const int *, const char *, int * );
static void MapPut1P( AstKeyMap *, const char *, int, void *const [], const char *, int * );
static void MapPut1S( AstKeyMap *, const char *, int, const short int *, const char *, int * );
static void MapPutElemA( AstKeyMap *, const char *, int, AstObject *, int * );
static void MapPutElemC( AstKeyMap *, const char *, int, const char *, int * );
static void MapPutElemD( AstKeyMap *, const char *, int, double, int * );
static void MapPutElemF( AstKeyMap *, const char *, int, float, int * );
static void MapPutElemI( AstKeyMap *, const char *, int, int, int * );
static void MapPutElemP( AstKeyMap *, const char *, int, void *, int * );
static void MapPutElemS( AstKeyMap *, const char *, int, short int, int * );
static void RemoveColumn( AstTable *, const char *, int * );
static void RemoveRow( AstTable *, int, int * );
static void MapPutU( AstKeyMap *, const char *, const char *, int * );
static const char *ColumnName( AstTable *, int index, int * );
static int ColumnType( AstTable *, const char *, int * );
static void ColumnShape( AstTable *, const char *, int, int *, int *, int *);

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

static const char *GetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );

static int GetNRow( AstTable *, int * );
static void SetNRow( AstTable *, int, int * );

static int GetNColumn( AstTable *, int * );


/* Member functions. */
/* ================= */
static void AddColumn( AstTable *this, const char *name, int type,
                       int ndim, int *dims, int *status ) {
/*
*++
*  Name:
c     astAddColumn
f     AST_ADDCOLUMN

*  Purpose:
*     Add a new column definition to a table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     void astAddColumn( AstTable *this, const char *name, int type, int ndim,
c                        int *dims )
f     CALL AST_ADDCOLUMN( THIS, NAME, TYPE, NDIM, DIMS, STATUS )

*  Class Membership:
*     Table method.

*  Description:
*     Adds the definition of a new column to the supplied table. Initially, the
*     column contains a null value for every row. Values may be added subsequently
*     using the methods of the KeyMap class.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     name
f     NAME = CHARACTER * ( * ) (Given)
*        The column name. Trailing spaces are ignored (all other spaces
*        are significant). Case is significant.
c     type
f     TYPE = INTEGER (Given)
*        The data type associated with the column. One of AST__INTTYPE (for
*        integer), AST__SINTTYPE (for
c        short int),
d        INTEGER*2),
*        AST__DOUBLETYPE (for double
*        precision floating point), AST__FLOATTYPE (for single
*        precision floating point), AST__STRINGTYPE (for character string),
*        AST__OBJECTTYPE (for AST Object pointer), AST__POINTERTYPE (for
*        arbitrary C pointer).
c     ndim
f     NDIM = INTEGER (Given)
*        The number of dimensions spanned by the values stored in a single
*        cell of the column. Zero if the column holds scalar values.
c     dims
f     DIMS( NDIM ) = INTEGER (Given)
*        An array holding the the lengths of each of the axes spanned by
*        the values stored in a single cell of the column. Ignored if the
*        column holds scalara values.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - This
c     function
f     routine
*     returns without action if a column already exists in the Table
*     with the supplied name and properties. However an error is
*     reported if any of the properties differ.

*--
*/

/* Local Variables: */
   AstKeyMap *col_km;    /* KeyMap holding new column details */
   int *olddims;         /* Shape of pre-existing column */
   int idim;             /* Axis index */
   int namlen;           /* Used length of "name" */
   int nval;             /* Number of values returned */
   int oldtype;          /* Data type of pre-existing column */

/* Check the global error status. */
   if ( !astOK ) return;

/* Verify supplied values. */
   namlen = astChrLen( name );
   if( namlen == 0 ) {
      astError( AST__BADKEY, "astAddColumn(%s): Illegal blank column name "
               "supplied.", status, astGetClass( this ) );

   } else if( namlen > MXCOLNAMLEN ) {
      astError( AST__BADKEY, "astAddColumn(%s): Column name '%s' is too "
               "long (must be no more than %d characters).", status,
               astGetClass( this ), name, MXCOLNAMLEN );

   } else if( ndim < 0 ) {
      astError( AST__NAXIN, "astAddColumn(%s): No of axes (%d) for values in "
               "new column %s is invalid.", status, astGetClass( this ),
               ndim, name );

   } else if( TypeString( type ) == NULL ) {
      astError( AST__NAXIN, "astAddColumn(%s): Bad data type supplied (%d) "
                "for new column %s.", status, astGetClass( this ), type,
                name );

   } else {
      for( idim = 0; idim < ndim; idim++ ) {
         if( dims[ idim ] < 1 ) {
            astError( AST__DIMIN, "astAddColumn(%s): Length of axis %d (%d) "
                      "for new column %s is invalid.", status,
                      astGetClass( this ), idim + 1, dims[ idim ], name );
            break;
         }
      }
   }

/* If there is already a column with the given name, check its properties
   match the supplied properties. */
   if( astOK ) {
      if( astMapGet0A( this->columns, name, &col_km ) ) {

         astMapGet0I( col_km, TYPE, &oldtype );
         if( oldtype != type && astOK ) {
            astError( AST__OLDCOL, "astAddColumn(%s): A column called "
                      "%s already exists in the table with a different "
                      "data type (%s).", status, astGetClass( this ),
                      name, TypeString( oldtype ) );
         }


         if( ndim != astMapLength( col_km, SHAPE ) && astOK ) {
            astError( AST__OLDCOL, "astAddColumn(%s): A column called "
                      "%s already exists in the table with a different "
                      "number of axes (%d).", status, astGetClass( this ),
                      name, astMapLength( col_km, SHAPE ) );
         }

         if( ndim > 0 && astOK ) {
            olddims = astMalloc( sizeof( int )*ndim );
            (void) astMapGet1I( col_km, SHAPE, ndim, &nval, olddims );
            for( idim = 0; idim < ndim && astOK; idim++ ) {
               if( dims[ idim ] != olddims[ idim ] ) {
                  astError( AST__OLDCOL, "astAddColumn(%s): A column called "
                            "%s already exists in the table with a different "
                            "shape.", status, astGetClass( this ), name );
               }
            }
            olddims = astFree( olddims );
         }

/* Otherwise, add a new column to the table. */
      } else {

/* Add a suitable entry describing the column to the Columns KeyMap. */
         col_km = astKeyMap( " ", status );
         astMapPut0C( col_km, NAME, name, NULL );
         astMapPut0I( col_km, TYPE, type, NULL );
         if( ndim ) astMapPut1I( col_km, SHAPE, ndim, dims, NULL );

/* Put the column KeyMap into the KeyMap holding details of all columns.
   Use the column name as the key. */
         astMapPut0A( this->columns, name, col_km, NULL );
      }

/* Annul the local KeyMa[p pointer. */
      col_km = astAnnul( col_km );

   }
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Table member function (over-rides the astClearAttrib protected
*     method inherited from the KeyMap class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Table, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Table.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstTable *this;               /* Pointer to the Table structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) this_object;

/* Check the attribute name and clear the appropriate attribute. */
   /* None yet */

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   if ( !strcmp( attrib, "nrow" ) ||
        !strcmp( attrib, "ncolumn" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static const char *ColumnName( AstTable *this, int index, int *status ) {
/*
*++
*  Name:
c     astColumnName
f     AST_COLUMNNAME

*  Purpose:
*     Get the name of the column at a given index within the Table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     const char *astColumnName( AstTable *this, int index )
f     RESULT = AST_COLUMNNAME( THIS, INDEX, STATUS )

*  Class Membership:
*     Table method.

*  Description:
*     This function returns a string holding the name of the column with
*     the given index within the Table.
*
*     This function is intended primarily as a means of iterating round all
*     the columns in a Table. For this purpose, the number of columns in
*     the Table is given by the NColumn attribute of the Table. This function
*     could then be called in a loop, with the index value going from
c     zero to one less than NColumn.
f     one to NColumn.
*
*     Note, the index associated with each column is not necessarily
*     related to the order in which the columns are added to the Table.
*     Adding or removing columns may change the indices associated with
*     other columns.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     index
f     INDEX = INTEGER (Given)
*        The index into the list of columns. The first column has index
*        one, and the last has index "NColumn".
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astColumnName()
c        A pointer to a null-terminated string containing the column name.
f     AST_COLUMNNAME = CHARACTER * ( AST__SZCHR )
f        The column name.

*  Notes:
c     - The returned pointer is guaranteed to remain valid and the
c     string to which it points will not be over-written for a total
c     of 50 successive invocations of this function. After this, the
c     memory containing the string may be re-used, so a copy of the
c     string should be made if it is needed for longer than this.
c     - A NULL pointer will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any
c     reason.
f     - A blank string will be returned if this function is invoked
f     with STATUS set to an error value, or if it should fail for any
f     reason.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Issue a more useful error message than that issued by astMapKey if the
   index is invalid. */
   if( index < 1 || index > astMapSize( this->columns ) ) {
      astError( AST__MPIND, "astColumnName(%s): Cannot find column "
                "%d (zero-based) of the %s - invalid index.", status,
                astGetClass( this ), index, astGetClass( this ) );
   }

/* Return a pointer to the required column name. */
   return astMapKey( this->columns, index - 1 );
}

static void ColumnShape( AstTable *this, const char *column, int mxdim,
                         int *ndim, int *dims, int *status ){
/*
*++
*  Name:
c     astColumnShape
f     AST_COLUMNSHAPE

*  Purpose:
*     Returns the shape of the values in a named column.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     void astColumnShape( AstTable *this, const char *column, int mxdim,
c                          int *ndim, int *dims, int *status )
f     CALL AST_COLUMNSHAPE( THIS, COLUMN, MXDIM, NDIM, DIMS, STATUS )

*  Class Membership:
*     Table method.

*  Description:
c     This function
f     This routine
*     returns the number of dimensions spaned by each value in a named
*     column of a Table, together with the length of each dimension.
*     These are the values supplied when the column was created using
c     astAddColumn.
f     AST_ADDCOLUMN.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     column
f     COLUMN = CHARACTER * ( * ) (Given)
*        The character string holding the name of the column. Trailing
*        spaces are ignored.
c     mxdim
f     MXDIM = INTEGER (Given)
*        The length of the
c        "dims" array.
f        DIMS array.
c     ndim
f     NDIM = INTEGER (Returned)
c        Pointer to an int in which to return the
f        The
*        number of dimensions spanned by values in the named column.
*        This will be zero if the column contains scalar values.
c     dims
f     DIMS( MXDIM ) = INTEGER (Returned)
c        Pointer to an
f        An
*        array in which to return the length of each dimension. Any
*        excess trailing elements will be filled with the value 1.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - No error is reported if the requested column cannot be found in the
*     given Table. A value of zero is returned for
c     "ndim" and the supplied values in "dims"
f     NDIM and the supplied values in DIMS
*     are left unchanged.
*     - A value of zero is returned for
c     "ndim"
f     NDIM
*     if an error occurs.

*--
*/

/* Local Variables: */
   AstKeyMap *col_km;      /* Pointer to KeyMap holding column info */
   int idim;               /* Axis index */

/* Initialise */
   *ndim = 0;

/* Check the inherited status. */
   if( !astOK ) return;

/* Get the KeyMap holding information about the requested column. */
   if( astMapGet0A( this->columns, column, &col_km ) ) {

/* Get the shape of the column values. */
      (void) astMapGet1I( col_km, SHAPE, mxdim, ndim, dims );

/* Fill excess array elements with 1. */
      for( idim = *ndim; idim < mxdim; idim++ ) dims[ idim ] = 1;

/* Annul the KeyMap pointer. */
      col_km = astAnnul( col_km );
   }

/* If an error has occurred, set ndim to zero. */
   if( !astOK ) *ndim = 0;

}

static int ColumnType( AstTable *this, const char *column, int *status ) {
/*
*++
*  Name:
c     astColumnType
f     AST_COLUMNTYPE

*  Purpose:
*     Get the data type of a column in a Table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     int astColumnType( AstTable *this, const char *column )
f     RESULT = AST_COLUMNTYPE( THIS, COLUMN, STATUS )

*  Class Membership:
*     Table method.

*  Description:
*     This function returns a value indicating the data type of a
*     named column in a Table. This is the data type which was used
*     when the column was added to the Table using
c     astAddColumn.
f     AST_ADDCOLUMN.

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
c     astColumnType()
f     AST_COLUMNTYPE = INTEGER
*        One of AST__INTTYPE (for integer), AST__SINTTYPE (for
c        short int),
f        INTEGER*2),
*        AST__DOUBLETYPE (for double
*        precision floating point), AST__FLOATTYPE (for single
*        precision floating point), AST__STRINGTYPE (for character string),
*        AST__OBJECTTYPE (for AST Object pointer), AST__POINTERTYPE (for
*        arbitrary C pointer). AST__BADTYPE is returned if the supplied
*        column is not found in the Table.

*  Notes:
*     - A function value of AST__BADTYPE will be returned if an error has
*     already occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   AstKeyMap *col_km;      /* Pointer to KeyMap holding column info */
   int result;             /* Returned value */

/* Initialise */
   result = AST__BADTYPE;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the KeyMap holding information about the requested column. */
   if( astMapGet0A( this->columns, column, &col_km ) ) {

/* Get the column data type. */
      (void) astMapGet0I( col_km, TYPE, &result );

/* Annul the KeyMap pointer. */
      col_km = astAnnul( col_km );
   }

/* Return AST__BADTYPE if an error occurred. */
   if( !astOK ) result = AST__BADTYPE;

/* Return the result. */
   return result;
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two Tables are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     Table member function (over-rides the astEqual protected
*     method inherited from the astKeyMap class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two Tables are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a Table).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the Tables are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTable *that;
   AstTable *this;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two Table structures. */
   this = (AstTable *) this_object;
   that = (AstTable *) that_object;

/* Check the second object is a Table. We know the first is a
   Table since we have arrived at this implementation of the virtual
   function. */
   if( astIsATable( that ) ) {

/* Check the Tables are equal when compared as KeyMaps. */
      if( (*parent_equal)( this_object, that_object, status ) ) {

/* Check the Columns KeyMaps are equal.  */
         result = astEqual( this->columns, that->columns );
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Table member function (over-rides the protected astGetAttrib
*     method inherited from the KeyMap class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Table, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Table.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Table, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Table. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTable *this;               /* Pointer to the Table structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Int attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an
   appropriate format.  Set "result" to point at the result string. */

/* NColumn */
/* ------- */
   if( !strcmp( attrib, "ncolumn" ) ) {
      ival = astGetNColumn( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* NRow */
/* ---- */
   } else if( !strcmp( attrib, "nrow" ) ) {
      ival = astGetNRow( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;
}

static int GetNColumn( AstTable *this, int *status ) {
/*
*+
*  Name:
*     astGetNColumn

*  Purpose:
*     Get the number of columns in a Table.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "table.h"
*     int astGetNColumn( AstTable *this )

*  Class Membership:
*     Table method.

*  Description:
*     This function returns the number of columns currently in the Table.

*  Parameters:
*     this
*        Pointer to the Table.

*  Returned Value:
*     Number of columns.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the result. */
   return astMapSize( this->columns );
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
*     #include "table.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     Table member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied Tables,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Table.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Table size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTable *this;            /* Pointer to Table structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the Table structure. */
   this = (AstTable *) this_object;

/* Invoke the GetObjSize method inherited from the parent KeyMap class, and
   then add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astGetObjSize( this->columns );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

void astInitTableVtab_(  AstTableVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitTableVtab

*  Purpose:
*     Initialise a virtual function table for a Table.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "table.h"
*     void astInitTableVtab( AstTableVtab *vtab, const char *name )

*  Class Membership:
*     Table vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Table class.

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
   AstKeyMapVtab *keymap;        /* Pointer to KeyMap component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitKeyMapVtab( (AstKeyMapVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsATable) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstKeyMapVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->AddColumn = AddColumn;
   vtab->RemoveColumn = RemoveColumn;
   vtab->RemoveRow = RemoveRow;
   vtab->GetNRow = GetNRow;
   vtab->SetNRow = SetNRow;
   vtab->GetNColumn = GetNColumn;
   vtab->ColumnName = ColumnName;
   vtab->ColumnType = ColumnType;
   vtab->ColumnShape = ColumnShape;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   keymap = (AstKeyMapVtab *) vtab;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;

   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;

   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;

   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

   parent_mapremove = keymap->MapRemove;

/* Define convenience macros for overriding methods inherited from the
   parent KeyMap class using all data type supported by KeyMap. */
#define OVERRIDE(method,code,methodlc,codelc) \
   parent_##methodlc##codelc = keymap->method##code; \
   keymap->method##code = method##code; \

#define OVERRIDE_METHOD(method,methodlc) \
   OVERRIDE(method,A,methodlc,a) \
   OVERRIDE(method,P,methodlc,p) \
   OVERRIDE(method,C,methodlc,c) \
   OVERRIDE(method,D,methodlc,d) \
   OVERRIDE(method,F,methodlc,f) \
   OVERRIDE(method,I,methodlc,i) \
   OVERRIDE(method,S,methodlc,s)

/* Use these macros to override the required methods. */
   OVERRIDE_METHOD(MapPut0,mapput0)
   OVERRIDE_METHOD(MapGet0,mapget0)
   OVERRIDE_METHOD(MapPut1,mapput1)
   OVERRIDE_METHOD(MapGet1,mapget1)
   OVERRIDE_METHOD(MapPutElem,mapputelem)
   OVERRIDE_METHOD(MapGetElem,mapgetelem)
   OVERRIDE(MapPut,U,mapput,u)

/* Remove the macros. */
#undef OVERRIDE_METHOD
#undef OVERRIDE

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the copy constructor, destructor and class dump function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "Table", "Two-dimensional table of data values" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
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
*     Table member function (over-rides the astManageLock protected
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
   AstTable *this;         /* Pointer to Table structure */
   int result;             /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the Table structure. */
   this = (AstTable *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->columns, mode, extra, fail );

   return result;

}
#endif

/*
*  Name:
*     MapGet0<X>

*  Purpose:
*     Get a scalar value from a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int MapGet0<X>( AstKeyMap *this, const char *key, <X>type *value );

*  Class Membership:
*     Table member function (over-rides the astMapGet0<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for retrieving a scalar value from a
*     cell of a Table. You should replace <X> in the generic function name
*     MapGet0<X> by an appropriate 1-character type code (see the "Data
*     Type Codes" section in the astMapGet0<X> docs). The stored value is
*     converted to the data type indiced by <X> before being returned (an
*     error is reported if it is not possible to convert the stored value
*     to the requested data type).

*  Parameters:
*     this
*        Pointer to the Table.
*     key
*        A character string identifying the cell from which the value is
*        to be retrieved. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     value
*        A pointer to a buffer in which to return the requested value.
*        If the requested cell is not found, or if it is found but has an
*        undefined value (see astMapPutU), then the contents of the buffer
*        on entry to this function will be unchanged on exit. For pointer
*        types ("A" and "C"), the buffer should be a suitable pointer, and
*        the address of this pointer should be supplied as the "value"
*        parameter.
*     status
*        Pointer to inherited status value.

*  Returned Value:
*     A non-zero value is returned if the requested key name was found, and
*     does not have an undefined value (see astMapPutU). Zero is returned
*     otherwise.

*  Notes:
*     - No error is reported if the requested cell cannot be found in the
*     given KeyMap, but a zero value will be returned as the function value.
*     The supplied buffer will be returned unchanged.
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the stored value is a vector value, then the first value in
*     the vector will be returned.
*     - A string pointer returned by astMapGet0C is guaranteed to remain
*     valid and the string to which it points will not be over-written for
*     a total of 50 successive invocations of this function. After this,
*     the memory containing the string may be re-used, so a copy of
*     the string should be made if it is needed for longer than this.
*     - If the returned value is an AST Object pointer, the Object's reference
*     count is incremented by this call. Any subsequent changes made to
*     the Object using the returned pointer will be reflected in any
*     any other active pointers for the Object. The returned pointer
*     should be annulled using astAnnul when it is no longer needed.
*/

/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPGET0(X,Xlc,Xtype,Itype) \
static int MapGet0##X( AstKeyMap *this_keymap, const char *key, Xtype *value, \
                       int *status ) { \
\
/* Local Variables: */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int irow;           /* Row index within key string */ \
   int result;         /* Returned flag */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, astGetKeyError( this ), colname, &irow, \
                 NULL, "astMapGet0" #X, status ) ) { \
\
/* If the row index is larger than the current number of rows in the \
   table, do nothing more. */ \
      if( irow <= astGetNRow( this ) ){ \
\
/* Use the astMapGet0<X> method in the parent keyMap class to get the \
   cell contents. */ \
         result = (*parent_mapget0##Xlc)( this_keymap, key, value, status ); \
      } \
   } \
\
/* If an error occurred, return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPGET0(I,i,int,AST__INTTYPE)
MAKE_MAPGET0(D,d,double,AST__DOUBLETYPE)
MAKE_MAPGET0(F,f,float,AST__FLOATTYPE)
MAKE_MAPGET0(C,c,const char *,AST__STRINGTYPE)
MAKE_MAPGET0(A,a,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGET0(P,p,void *,AST__POINTERTYPE)
MAKE_MAPGET0(S,s,short int,AST__SINTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET0

/*
*  Name:
*     MapGet1<X>

*  Purpose:
*     Get a vector value from a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int MapGet1<X>( AstKeyMap *this, const char *key, int mxval,
*                     int *nval, <X>type *value )
*     int MapGet1C( AstKeyMap *this, const char *key, int l, int mxval,
*                   int *nval, const char *value )

*  Class Membership:
*     Table member function (over-rides the astMapGet1<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for retrieving a vector value from a
*     cell of a Table. You should replace <X> in the generic function name
*     MapGet1<X> by an appropriate 1-character type code (see the "Data
*     Type Codes" section in the astMapGet1<X> docs). The stored value is
*     converted to the data type indiced by <X> before being returned (an
*     error is reported if it is not possible to convert the stored value
*     to the requested data type).
*
*     Note, the MapGet1C function has an extra parameter "l" which
*     specifies the maximum length of each string to be stored in the
*     "value" buffer (see the "astMapGet1C" docs).

*  Parameters:
*     this
*        Pointer to the Table.
*     key
*        A character string identifying the cell from which the value is
*        to be retrieved. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     mxval
*        The number of elements in the "value" array.
*     nval
*        The address of an integer in which to put the number of elements
*        stored in the "value" array. Any unused elements of the array are
*        left unchanged.
*     value
*        A pointer to an array in which to return the requested values.
*        If the requested cell is not found, or if it is found but has an
*        undefined value (see astMapPutU), then the contents of the buffer
*        on entry to this function will be unchanged on exit.
*     status
*        Pointer to inherited status value.

*  Returned Value:
*     A non-zero value is returned if the requested key name was found, and
*     does not have an undefined value (see astMapPutU). Zero is returned
*     otherwise.

*  MapGet1C:
*     The "value" buffer supplied to the MapGet1C function should be a
*     pointer to a character array with "mxval*l" elements, where "l" is
*     the maximum length of a string to be returned. The value of "l"
*     should be supplied as an extra parameter following "key" when
*     invoking MapGet1C, and should include space for a terminating
*     null character.

*  Notes:
*     - No error is reported if the requested cell cannot be found in the
*     given KeyMap, but a zero value will be returned as the function value.
*     The supplied buffer will be returned unchanged.
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the stored value is a scalar value, then the value will be
*     returned in the first element of the supplied array, and "nval"
*     will be returned set to 1.
*/

/* Define a macro to implement the function for a specific data type
   (excluding "C" since that needs an extra parameter). */
#define MAKE_MAPGET1(X,Xlc,Xtype,Itype) \
static int MapGet1##X( AstKeyMap *this_keymap, const char *key, int mxval, int *nval, \
                       Xtype *value, int *status ) { \
\
/* Local Variables: */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int irow;           /* Row index within key string */ \
   int result;         /* Returned flag */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, astGetKeyError( this ), colname, &irow, \
                 NULL, "astMapGet1" #X, status ) ) { \
\
/* If the row index is larger than the current number of rows in the \
   table, do nothing more. */ \
      if( irow <= astGetNRow( this ) ){ \
\
/* Use the astMapGet1<X> method in the parent keyMap class to get the \
   cell contents. */ \
         result = (*parent_mapget1##Xlc)( this_keymap, key, mxval, nval, \
                   value, status ); \
      } \
   } \
\
/* If an error occurred, return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPGET1(I,i,int,AST__INTTYPE)
MAKE_MAPGET1(D,d,double,AST__DOUBLETYPE)
MAKE_MAPGET1(F,f,float,AST__FLOATTYPE)
MAKE_MAPGET1(A,a,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGET1(P,p,void *,AST__POINTERTYPE)
MAKE_MAPGET1(S,s,short int,AST__SINTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET1


static int MapGet1C( AstKeyMap *this_keymap, const char *key, int l, int mxval,
                     int *nval, char *value, int *status ) {
/*
*  Name:
*     MapGet1C

*  Purpose:
*     Get a vector value from a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int MapGet1C( AstKeyMap *this, const char *key, int l, int mxval,
*                   int *nval, const char *value )

*  Class Membership:
*     Table member function (over-rides the astMapGet1C method inherited
*     from the KeyMap class).

*  Description:
*     This is the implementation of MapGet1<X> for <X> = "C". We
*     cannot use the MAKE_MAPGET1 macro for this because the string
*     version of this function has an extra parameter giving the maximum
*     length of each string which can be stored in the supplied buffer.

*  Parameters:
*     (see MapGet1<X>)
*/

/* Local Variables: */
   AstTable *this;     /* Pointer to Table structure */
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */
   int irow;           /* Row index within key string */
   int result;         /* Returned flag */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Table structure. */
   this = (AstTable *) this_keymap;

/* Check the supplied key looks like a table cell key, and get the
   the column name and the row number. Also checks that the table
   contains a column with the specified name. */
   if( ParseKey( this, key, astGetKeyError( this ), colname, &irow,
                 NULL, "astMapGet1C", status ) ) {

/* If the row index is larger than the current number of rows in the
   table, do nothing more. */
      if( irow <= astGetNRow( this ) ){

/* Use the astMapGet1<X> method in the parent keyMap class to get the
   cell contents. */
         result = (*parent_mapget1c)( this_keymap, key, l, mxval, nval,
                   value, status );
      }
   }

/* If an error occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

/*
*  Name:
*     MapGetElem<X>

*  Purpose:
*     Get a single element of a vector value from a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int MapGetElem<X>( AstKeyMap *this, const char *key, int elem,
*                        <X>type *value, int *status )
*     int MapGetElemC( AstKeyMap *this, const char *key, int l, int elem,
*                      char *value, int *status )

*  Class Membership:
*     Table member function (over-rides the astMapGetElem<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for retrieving a single element of a vector
*     value from a cell of a Table. You should replace <X> in the generic
*     function name MapGetElem<X> by an appropriate 1-character type code
*     (see the "Data Type Codes" section in the astMapGetElem<X> docs). The
*     stored value is converted to the data type indiced by <X> before being
*     returned (an error is reported if it is not possible to convert the
*     stored value to the requested data type).
*
*     Note, the MapGetElemC function has an extra parameter "l" which
*     specifies the maximum length of each string to be stored in the
*     "value" buffer (see the "MapGetElemC" docs).

*  Parameters:
*     this
*        Pointer to the Table.
*     key
*        A character string identifying the cell from which the value is
*        to be retrieved. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     elem
*        The index of the vector element to modify, starting at zero.
*        If the index is outside the range of the vector, an error will
*        be reported.
*     value
*        A pointer to a buffer in which to return the requested values.
*        If the requested cell is not found, or if it is found but has an
*        undefined value (see astMapPutU), then the contents of the buffer
*        on entry to this function will be unchanged on exit.
*     status
*        Pointer to inherited status value.

*  Returned Value:
*     A non-zero value is returned if the requested key name was found, and
*     does not have an undefined value (see astMapPutU). Zero is returned
*     otherwise.

*  MapGetElemC:
*     The "value" buffer supplied to the MapGetElemC function should be a
*     pointer to a character array with "l" elements, where "l" is
*     the maximum length of a string to be returned. The value of "l"
*     should be supplied as an extra parameter following "key" when
*     invoking MapGetElemC, and should include space for a terminating
*     null character.

*  Notes:
*     - No error is reported if the requested cell cannot be found in the
*     given KeyMap, but a zero value will be returned as the function value.
*     The supplied buffer will be returned unchanged.
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the stored value is a scalar value, then the value will be
*     returned in the first element of the supplied array, and "nval"
*     will be returned set to 1.
*/

/* Define a macro to implement the function for a specific data type
(excluding "C" since that needs an extra parameter). */
#define MAKE_MAPGETELEM(X,Xlc,Xtype,Itype) \
static int MapGetElem##X( AstKeyMap *this_keymap, const char *key, int elem, \
                          Xtype *value, int *status ) { \
\
/* Local Variables: */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int irow;           /* Row index within key string */ \
   int result;         /* Returned flag */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, astGetKeyError( this ), colname, &irow, \
                 NULL, "astMapGetElem" #X, status ) ) { \
\
/* If the row index is larger than the current number of rows in the \
   table, do nothing more. */ \
      if( irow <= astGetNRow( this ) ){ \
\
/* Use the astMapGetElem<X> method in the parent keyMap class to get the \
   cell contents. */ \
         result = (*parent_mapgetelem##Xlc)( this_keymap, key, elem, \
                                             value, status ); \
      } \
   } \
\
/* If an error occurred, return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPGETELEM(I,i,int,AST__INTTYPE)
MAKE_MAPGETELEM(D,d,double,AST__DOUBLETYPE)
MAKE_MAPGETELEM(F,f,float,AST__FLOATTYPE)
MAKE_MAPGETELEM(A,a,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGETELEM(P,p,void *,AST__POINTERTYPE)
MAKE_MAPGETELEM(S,s,short int,AST__SINTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPGETELEM

static int MapGetElemC( AstKeyMap *this_keymap, const char *key, int l,
                        int elem, char *value, int *status ) {
/*
*  Name:
*     MapGetElemC

*  Purpose:
*     Get a single element of a vector value from a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int MapGetElemC( AstKeyMap *this, const char *key, int l, int elem,
*                      char *value, int *status )

*  Class Membership:
*     Table member function (over-rides the astMapGetElemC method inherited
*     from the KeyMap class).

*  Description:
*     This is the implementation of MapGetElem<X> for <X> = "C". We
*     cannot use the MAKE_MAPGETELEM macro for this because the string
*     version of this function has an extra parameter giving the maximum
*     length of each string which can be stored in the supplied buffer.

*  Parameters:
*     (see MapGetElem<X>)
*/

/* Local Variables: */
   AstTable *this;     /* Pointer to Table structure */
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */
   int irow;           /* Row index within key string */
   int result;         /* Returned flag */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the Table structure. */
   this = (AstTable *) this_keymap;

/* Check the supplied key looks like a table cell key, and get the
   the column name and the row number. Also checks that the table
   contains a column with the specified name. */
   if( ParseKey( this, key, astGetKeyError( this ), colname, &irow,
                 NULL, "astMapGetElemC", status ) ) {

/* If the row index is larger than the current number of rows in the
   table, do nothing more. */
      if( irow <= astGetNRow( this ) ){

/* Use the astMapGetElem<X> method in the parent keyMap class to get the
   cell contents. */
         result = (*parent_mapgetelemc)( this_keymap, key, l, elem,
                                         value, status );
      }
   }

/* If an error occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

/*
*  Name:
*     MapPut0<X>

*  Purpose:
*     Stores a scalar value in a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void MapPut0<X>( AstKeyMap *this, const char *key, <X>type value,
*                      const char *comment, int *status )

*  Class Membership:
*     Table member function (over-rides the astMapPut0<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for storing a scalar value in a cell of
*     a Table. You should use a function which matches the data type of the
*     data you wish to add to the Table by replacing <X> in the generic
*     function name MapPut0<X> by an appropriate 1-character type code (see
*     the "Data Type Codes" section in the astMapPut0<X> docs).

*  Parameters:
*     this
*        Pointer to the Table in which to store the supplied value.
*     key
*        A character string identifying the cell in which the value is
*        to be stored. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     value
*        The value to be stored. The data type of this value should match
*        the 1-character type code appended to the function name (e.g. if
*        you are using astMapPut0A, the type of this value should be "pointer
*        to AstObject"). An error will be reported if this data type is
*        different to the data type assigned to the column when it was
*        created via astAddColumn.
*     comment
*        A pointer to a null-terminated comment string to be stored with the
*        value. A NULL pointer may be supplied, in which case no comment is
*        stored.
*     status
*        Pointer to inherited status value.

*  Notes:
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - The new value will replace any old value already stored in the
*     Table for the specified cell.
*     - If the stored value is an AST Object pointer, the Object's reference
*     count is incremented by this call. Any subsequent changes made to
*     the Object using the returned pointer will be reflected in any
*     any other active pointers for the Object, including any obtained
*     later using astMapget0A. The reference count for the Object will be
*     decremented when the KeyMap is destroyed, or the entry is removed or
*     over-written with a different pointer.

*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT0(X,Xlc,Xtype,Itype,ValExp) \
static void MapPut0##X( AstKeyMap *this_keymap, const char *key, Xtype value, \
                        const char *comment, int *status ) { \
\
/* Local Variables: */ \
   AstKeyMap *col_km;  /* KeyMap holding details of the requested column */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int irow;           /* Row index within key string */ \
   int type;           /* Data type of the requested column */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, 1, colname, &irow, &col_km, "astMapPut0" #X, \
                 status ) ) { \
\
/* Check the column holds scalar values of the type implied by the <X> \
   code in the function name. */ \
      (void) astMapGet0I( col_km, TYPE, &type ); \
      if( type != Itype && astOK ) { \
         astError( AST__BADTYP, "astMapPut0" #X "(%s): Failed to store a " \
                   #Xtype " value for cell \"%s\": column %s holds %s " \
                   "values.", status, astGetClass( this ), key, colname, \
                   TypeString( type ) ); \
      } \
\
      if( astMapHasKey( col_km, SHAPE ) && astOK ) { \
         astError( AST__BADTYP, "astMapPut0" #X "(%s): Failed to store a " \
                   "scalar value for cell \"%s\": column %s holds vector " \
                   " values.", status, astGetClass( this ), key, colname ); \
      } \
\
/* If the row index is larger than the current number of rows in the \
   table, update the number of rows in the table. */ \
      if( irow > astGetNRow( this ) ) astSetNRow( this, irow ); \
\
/* Use the astMapPut0<X> method in the parent keyMap class to store the \
   new cell contents. */ \
      (*parent_mapput0##Xlc)( this_keymap, key, value, comment, status ); \
\
/* Free resources. */ \
      col_km = astAnnul( col_km ); \
   } \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT0(I,i,int,AST__INTTYPE,value)
MAKE_MAPPUT0(D,d,double,AST__DOUBLETYPE,value)
MAKE_MAPPUT0(F,f,float,AST__FLOATTYPE,value)
MAKE_MAPPUT0(C,c,const char *,AST__STRINGTYPE,astStore(NULL,value,strlen(value)+1))
MAKE_MAPPUT0(A,a,AstObject *,AST__OBJECTTYPE,(value?astClone(value):NULL))
MAKE_MAPPUT0(P,p,void *,AST__POINTERTYPE,value)
MAKE_MAPPUT0(S,s,short int,AST__SINTTYPE,value)

/* Undefine the macro. */
#undef MAKE_MAPPUT0

/*
*  Name:
*     MapPut1<X>

*  Purpose:
*     Stores a vectorised value in a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void MapPut1<X>( AstKeyMap *this, const char *key, int size,
*                      const <X>type value[], const char *comment,
*                      int *status );

*  Class Membership:
*     Table member function (over-rides the astMapPut1<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for storing a vectorised value in a cell of
*     a Table. You should use a function which matches the data type of the
*     data you wish to add to the Table by replacing <X> in the generic
*     function name MapPut1<X> by an appropriate 1-character type code (see
*     the "Data Type Codes" section in the astMapPut1<X> docs).

*  Parameters:
*     this
*        Pointer to the Table in which to store the supplied value.
*     key
*        A character string identifying the cell in which the value is
*        to be stored. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     size
*        The number of elements in the supplied array of values.
*     value
*        The value to be stored. The data type of this value should match
*        the 1-character type code appended to the function name (e.g. if
*        you are using astMapPut0A, the type of this value should be "pointer
*        to AstObject"). An error will be reported if this data type is
*        different to the data type assigned to the column when it was
*        created via astAddColumn.
*     comment
*        A pointer to a null-terminated comment string to be stored with the
*        value. A NULL pointer may be supplied, in which case no comment is
*        stored.
*     status
*        Pointer to inherited status value.

*  Notes:
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - The new value will replace any old value already stored in the
*     Table for the specified cell.

*/

/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT1(X,Xlc,Xtype,Itype,ValExp) \
static void MapPut1##X( AstKeyMap *this_keymap, const char *key, int size, \
                        Xtype value[], const char *comment, \
                        int *status ) { \
\
/* Local Variables: */ \
   AstKeyMap *col_km;  /* KeyMap holding details of the requested column */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int *dims;          /* Array holding dimensions of each cell column */ \
   int idim;           /* Current axis index */ \
   int irow;           /* Row index within key string */ \
   int ndim;           /* Number of dimensions for each column cell */ \
   int nel;            /* Number of elements in each cell column */ \
   int nval;           /* Number of values returned */ \
   int type;           /* Data type of the requested column */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, 1, colname, &irow, &col_km, "astMapPut1" #X, \
                 status ) ) { \
\
/* Check the column holds vector values of the type implied by the <X> \
   code in the function name. */ \
      (void) astMapGet0I( col_km, TYPE, &type ); \
      if( type != Itype && astOK ) { \
         astError( AST__BADTYP, "astMapPut1" #X "(%s): Failed to store " \
                   #Xtype " values for cell \"%s\": column %s holds %s values.", \
                   status, astGetClass( this ), key, colname, \
                   TypeString( type ) ); \
      } \
\
/* Check the column holds vectors with length equal to the supplied vector. */ \
      ndim = astMapLength( col_km, SHAPE ); \
      if( ndim == 0 && astOK ) { \
         astError( AST__BADTYP, "astMapPut1" #X "(%s): Failed to store a " \
                   "vector value for cell \"%s\": column %s holds scalar " \
                   "values.", status, astGetClass( this ), key, colname ); \
\
      } else { \
         dims = astMalloc( sizeof( int )*ndim ); \
         if( astOK ) { \
            (void) astMapGet1I( col_km, SHAPE, ndim, &nval, dims ); \
\
            nel = 1; \
            for( idim = 0; idim < ndim && astOK; idim++ ) { \
               nel *= dims[ idim ]; \
            } \
\
            if( nel != size && astOK ) { \
               astError( AST__BADTYP, "astMapPut1" #X "(%s): Failed to " \
                         "store a vector value for cell \"%s\": column " \
                         "%s needs %d values per cell but %d were supplied.", \
                         status, astGetClass( this ), key, colname, nel, \
                         size ); \
\
/* If all is OK, update the number of rows in the table if required, and \
   store the vector in the parent KeyMap. */ \
            } else { \
               if( irow > astGetNRow( this ) ) astSetNRow( this, irow ); \
               (*parent_mapput1##Xlc)( this_keymap, key, size, value, \
                                       comment, status ); \
            } \
         } \
\
/* Free resources. */ \
         dims = astFree( dims ); \
      } \
      col_km = astAnnul( col_km ); \
   } \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT1(D,d,const double,AST__DOUBLETYPE,value[i])
MAKE_MAPPUT1(F,f,const float,AST__FLOATTYPE,value[i])
MAKE_MAPPUT1(I,i,const int,AST__INTTYPE,value[i])
MAKE_MAPPUT1(C,c,const char *const,AST__STRINGTYPE,astStore(NULL,value[i],strlen(value[i])+1))
MAKE_MAPPUT1(A,a,AstObject *const,AST__OBJECTTYPE,(value[i]?astClone(value[i]):NULL))
MAKE_MAPPUT1(P,p,void *const,AST__POINTERTYPE,value[i])
MAKE_MAPPUT1(S,s,const short int,AST__SINTTYPE,value[i])

/* Undefine the macro. */
#undef MAKE_MAPPUT1

/*
*  Name:
*     MapPutElem<X>

*  Purpose:
*     Put a value into an element of a vector value in a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void MapPutElem<X>( AstKeyMap *this, const char *key, int elem,
*                         <X>type *value, int *status )

*  Class Membership:
*     Table member function (over-rides the astMapPutElem<X> method inherited
*     from the KeyMap class).

*  Description:
*     This is a set of functions for storing a value in a single element
*     of a vector value stored in a cell of a Table. You should use a
*     function which matches the data type of the data you wish to add to
*     the Table by replacing <X> in the generic function name MapPutElem<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section in the astMapPutElem<X> docs).

*  Parameters:
*     this
*        Pointer to the Table in which to store the supplied value.
*     key
*        A character string identifying the cell in which the value is
*        to be stored. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     elem
*        The index of the vector element to modify, starting at zero.
*        If the index is outside the range of the vector, an error will
*        be reported.
*     value
*        The value to be stored. The data type of this value should match
*        the 1-character type code appended to the function name (e.g. if
*        you are using astMapPut0A, the type of this value should be "pointer
*        to AstObject"). An error will be reported if this data type is
*        different to the data type assigned to the column when it was
*        created via astAddColumn.
*     status
*        Pointer to inherited status value.

*  Notes:
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - The new value will replace any old value already stored in the
*     Table for the specified cell.
*     - If the stored value is an AST Object pointer, the Object's reference
*     count is incremented by this call. Any subsequent changes made to
*     the Object using the returned pointer will be reflected in any
*     any other active pointers for the Object, including any obtained
*     later using astMapget0A. The reference count for the Object will be
*     decremented when the KeyMap is destroyed, or the entry is removed or
*     over-written with a different pointer.

*/
/* Define a macro to implement the function for a specific data type. */

#define MAKE_MAPPUTELEM(X,Xlc,Xtype,Itype) \
static void MapPutElem##X( AstKeyMap *this_keymap, const char *key, int elem, \
                           Xtype value, int *status ) { \
\
/* Local Variables: */ \
   AstKeyMap *col_km;  /* KeyMap holding details of the requested column */ \
   AstTable *this;     /* Pointer to Table structure */ \
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */ \
   int *dims;          /* Array holding dimensions of each cell column */ \
   int idim;           /* Current axis index */ \
   int irow;           /* Row index within key string */ \
   int ndim;           /* Number of dimensions for each column cell */ \
   int nel;            /* Number of elements in each cell column */ \
   int nval;           /* Number of values returned */ \
   int type;           /* Data type of the requested column */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Get a pointer to the Table structure. */ \
   this = (AstTable *) this_keymap; \
\
/* Check the supplied key looks like a table cell key, and get the \
   the column name and the row number. Also checks that the table \
   contains a column with the specified name. */ \
   if( ParseKey( this, key, 1, colname, &irow, &col_km, "astMapPutElem" #X, \
                 status ) ) { \
\
/* Check the column holds vector values of the type implied by the <X> \
   code in the function name. */ \
      (void) astMapGet0I( col_km, TYPE, &type ); \
      if( type != Itype && astOK ) { \
         astError( AST__BADTYP, "astMapPutElem" #X "(%s): Failed to store a " \
                   #Xtype " value in cell \"%s\": column %s holds %s values.", \
                   status, astGetClass( this ), key, colname, \
                   TypeString( type ) ); \
      } \
\
/* Check the column holds vectors with length large enough to hold the \
   supplied element. */ \
      ndim = astMapLength( col_km, SHAPE ); \
      if( ndim == 0 && astOK ) { \
         astError( AST__BADTYP, "astMapPutElem" #X "(%s): Failed to store a " \
                   "vector element value for cell \"%s\": column %s holds " \
                   "scalar values.", status, astGetClass( this ), key, \
                   colname ); \
\
      } else { \
         dims = astMalloc( sizeof( int )*ndim ); \
         if( astOK ) { \
            (void) astMapGet1I( col_km, SHAPE, ndim, &nval, dims ); \
\
            nel = 1; \
            for( idim = 0; idim < ndim && astOK; idim++ ) { \
               nel *= dims[ idim ]; \
            } \
\
            if( nel <= elem && astOK ) { \
               astError( AST__BADTYP, "astMapPutElem" #X "(%s): Failed to " \
                         "store a value for element %d (zero-based) of " \
                         "cell \"%s\": column %s has only %d values per " \
                         "cell.", status, astGetClass( this ), elem, key, \
                         colname, nel ); \
\
/* If all is OK, update the number of rows in the table if required, and \
   store the value in the parent KeyMap. */ \
            } else { \
               if( irow > astGetNRow( this ) ) astSetNRow( this, irow ); \
               (*parent_mapputelem##Xlc)( this_keymap, key, elem, value, \
                                       status ); \
            } \
         } \
\
/* Free resources. */ \
         dims = astFree( dims ); \
      } \
      col_km = astAnnul( col_km ); \
   } \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUTELEM(I,i,int,AST__INTTYPE)
MAKE_MAPPUTELEM(D,d,double,AST__DOUBLETYPE)
MAKE_MAPPUTELEM(F,f,float,AST__FLOATTYPE)
MAKE_MAPPUTELEM(A,a,AstObject *,AST__OBJECTTYPE)
MAKE_MAPPUTELEM(P,p,void *,AST__POINTERTYPE)
MAKE_MAPPUTELEM(C,c,const char *,AST__STRINGTYPE)
MAKE_MAPPUTELEM(S,s,short int,AST__SINTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPPUTELEM

static void MapPutU( AstKeyMap *this_keymap, const char *key, const char *comment,
                     int *status ) {
/*
*  Name:
*     MapPutU

*  Purpose:
*     Stores a undefined value in a cell of a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void MapPutU( AstKeyMap *this, const char *key, const char *comment,
*                   int *status )

*  Class Membership:
*     Table member function (over-rides the astMapPutU method inherited
*     from the KeyMap class).

*  Description:
*     This function adds a new cell to a Table, but no value is stored with
*     the cell. The cell therefore has a special data type represented by
*     symbolic constant AST__UNDEFTYPE.
*
*     An example use is to add cells with undefined values to a Table
*     prior to locking them with the MapLocked attribute. Such cells
*     can act as placeholders for values that can be added to the KeyMap
*     later.

*  Parameters:
*     this
*        Pointer to the Table in which to store the supplied value.
*     key
*        A character string identifying the cell in which the value is
*        to be stored. It should have the form "COLNAME(irow)", where
*        "COLNAME" is replaced by the name of a column that has been
*        defined previously using the astAddColumn method, and "irow" is
*        an integer row index (the first row is row 1).
*     comment
*        A pointer to a null-terminated comment string to be stored with the
*        value. A NULL pointer may be supplied, in which case no comment is
*        stored.
*     status
*        Pointer to inherited status value.

*  Notes:
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - The new undefined value will replace any old value already stored in
*     the Table for the specified cell.

*/

/* Local Variables: */
   AstTable *this;     /* Pointer to Table structure */
   char colname[ MXCOLNAMLEN + 1 ]; /* Column name read from string */
   int irow;           /* Row index within key string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Table structure. */
   this = (AstTable *) this_keymap;

/* Check the supplied key looks like a table cell key, and get the
   the column name and the row number. Also checks that the table
   contains a column with the specified name. */
   if( ParseKey( this, key, 1, colname, &irow, NULL, "astMapPutU",
                 status ) ) {

/* If the row index is larger than the current number of rows in the
   table, update the number of rows in the table. */
      if( irow > astGetNRow( this ) ) astSetNRow( this, irow );

/* Use the astMapPutU method in the parent keyMap class to store the
   new cell contents. */
      (*parent_mapputu)( this_keymap, key, comment, status );
   }
}

static int ParseKey( AstTable *this, const char *key, int report,
                     char colname[ MXCOLNAMLEN + 1 ], int *irow,
                     AstKeyMap **col_km, const char *method, int *status ){
/*
*  Name:
*     ParseKey

*  Purpose:
*     Find the column name and row index in a KeyMap key.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int ParseKey( AstTable *this, const char *key, int report,
*                   char colname[ MXCOLNAMLEN + 1 ], int *irow,
*                   AstKeyMap **col_km, const char *method, int *status )

*  Class Membership:
*     Table member function

*  Description:
*     This function checks that the supplied KeyMap key conforms to the
*     format expected for Table cells: i.e. "COLNAME(irow)", where
*     "COLNAME" is the name of a column and "irow" is an integer row
*     index (the first row is row 1). An error is reported if this is not
*     the case.

*  Parameters:
*     this
*        Pointer to the table.
*     key
*        The key string to test.
*     report
*        If non-zero, an error will be reported if the key does not
*        correspond to a cell of an existing column. Otherwise, no error
*        will be reported.
*     colname
*        A buffer in which to return the column name.
*     irow
*        Address of an int in which to return the row index.
*     col_km
*        Address of a KeyMap pointer in which to return a pointer to the
*        KeyMap holding the information about the column. The returned
*        KeyMap pointer should be annulled using astAnnul when no lngerr
*        needed. If "col_km" is NULL, no KeyMap pointer is returned.
*     method
*        Pointer to a string holding the name of the method to include in
*        any error message.
*     status
*        Address of the inherited status value.

*  Returned Value:
*     Zero is returned if the key is not a valid Table cell key, or if an
*     error occurs.

*/

/* Local Variables: */
   int result;         /* Returned flag */
   int collen;         /* Length of column name */
   int nctot;          /* Number of characters read */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check the supplied key looks like a table cell key, and extract the
   column name and row number. */
   nctot = 0;
   if( 1 == astSscanf( key, "%*[^(]%n(%d) %n", &collen, irow, &nctot )
       && ( nctot >= strlen( key ) ) ) {

/* Check the column name is not too long. */
      if( collen > MXCOLNAMLEN ) {
         if( report ) {
            astError( AST__BADKEY, "%s(%s): Failed to store a value for cell "
                      "\"%s\": column name is too long.", status, method,
                      astGetClass( this ), key );
         }

/* Check the row index is positive. */
      } else if( *irow < 1 ) {
         if( report ) {
            astError( AST__BADKEY, "%s(%s): Failed to store a value for cell "
                      "\"%s\": row index %d is invalid.", status, method,
                      astGetClass( this ), key, *irow );
         }

/* If the key looks OK so far... */
      } else {

/* Copy the column name to the returned buffer and terminate it. */
         strncpy( colname, key, collen );
         colname[ collen ] = 0;

/* check that the column exists in the Table, returning a pointer to the
   column KeyMap is reequired. */
         if( col_km ) {
            result = astMapGet0A( this->columns, colname, col_km );
         } else {
            result = astMapHasKey( this->columns, colname );
         }

/* Report an error if the table does not contain the specified column. */
         if( !result && astOK && report) {
            astError( AST__BADKEY, "%s(%s): Failed to store a value for "
                      "cell \"%s\": the table does not contain a column "
                      "called '%s'.", status, method, astGetClass( this ),
                      key, colname );
         }
      }

/* Report an error if the cell key has the wrong format. */
   } else if( report ) {
      astError( AST__BADKEY, "%s(%s): Failed to store a value for cell "
                "\"%s\": the cell name is invalid.", status, method,
                astGetClass( this ), key );
   }

/* Return the result.*/
   return result;
}

static void RemoveColumn( AstTable *this, const char *name, int *status ) {
/*
*++
*  Name:
c     astRemoveColumn
f     AST_REMOVECOLUMN

*  Purpose:
*     Remove a column from a table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     void astRemoveColumn( AstTable *this, const char *name )
f     CALL AST_REMOVECOLUMN( THIS, NAME, STATUS )

*  Class Membership:
*     Table method.

*  Description:
*     This function removes a specified column from the supplied table.
*     The
c     function
f     routine
*     returns without action if the named column does not exist in the
*     Table (no error is reported).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     name
f     NAME = CHARACTER * ( * ) (Given)
*        The column name. Trailing spaces are ignored (all other spaces
*        are significant). Case is significant.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*--
*/

/* Local Variables: */
   char key[ MXCOLNAMLEN + 24 ]; /* Cell key string */
   int irow;             /* Row index */
   int namlen;           /* Used length of "name" */
   int nrow;             /* Number of rows in table */

/* Check the global error status. */
   if ( !astOK ) return;

/* Verify supplied values. */
   namlen = astChrLen( name );
   if( namlen == 0 ) {
      astError( AST__BADKEY, "astRemoveColumn(%s): Illegal blank column name "
               "supplied.", status, astGetClass( this ) );
   }

/* Get the number of rows in the table. */
   nrow = astGetNRow( this );

/* If there is no column with the given name in the Table, do nothing more. */
   if( astOK && astMapHasKey( this->columns, name ) ) {

/* Remove the column description from the columns keymap. */
      astMapRemove( this->columns, name );

/* Remove any column cells with defined values from the parent KeyMap. */
      for( irow = 1; irow <= nrow; irow++ ) {
         sprintf( key, "%.*s(%d)", namlen, name, irow );
         (*parent_mapremove)( (AstKeyMap *) this, key, status );
      }
   }
}

static void RemoveRow( AstTable *this, int index, int *status ) {
/*
*++
*  Name:
c     astRemoveRow
f     AST_REMOVEROW

*  Purpose:
*     Remove a row from a table.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "table.h"
c     void astRemoveRow( AstTable *this, int index )
f     CALL AST_REMOVEROW( THIS, INDEX, STATUS )

*  Class Membership:
*     Table method.

*  Description:
*     This function removes a specified row from the supplied table.
*     The
c     function
f     routine
*     returns without action if the row does not exist in the
*     Table (no error is reported).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Table.
c     index
f     INDEX = INTEGER (Given)
*        The index of the row to be removed. The first row has index 1.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*--
*/

/* Local Variables: */
   char key[ MXCOLNAMLEN + 24 ]; /* Cell key string */
   const char *col;              /* Column name */
   int icol;                     /* Column index */
   int ncol;                     /* Number of columns in table */
   int nrow;                     /* Number of rows in table */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the number of rows in the table. */
   nrow = astGetNRow( this );

/* Do nothing if the specified row is out of bounds. */
   if( index > 0 && index <= nrow ) {

/* Loop round all columns in the table. */
      ncol = astMapSize( this->columns );
      for( icol = 0; icol < ncol; icol++ ) {
         col = astMapKey( this->columns, icol );

/* Remove the cell of the current column at the requested row. */
         sprintf( key, "%s(%d)", col, index );
         (*parent_mapremove)( (AstKeyMap *) this, key, status );
      }

/* If the removed row was the last row, reduce the number of rows in the
   Table. */
      if( index == nrow ) astSetNRow( this, index - 1 );
   }
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     Table member function (over-rides the astSetAttrib protected
*     method inherited from the KeyMap class).

*  Description:
*     This function assigns an attribute value for a Table, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the Table.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstTable *this;              /* Pointer to the Table structure */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */
   /* None as yet */

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   if ( MATCH( "ncolumn" ) ||
        MATCH( "nrow" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Table member function (over-rides the astTestAttrib protected
*     method inherited from the KeyMap class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Table's attributes.

*  Parameters:
*     this
*        Pointer to the Table.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTable *this;               /* Pointer to the Table structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) this_object;

/* Check the attribute name and test the appropriate attribute. */
   /* None as yet */

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   if ( !strcmp( attrib, "ncolumn" ) ||
        !strcmp( attrib, "nrow" ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static const char *TypeString( int type ) {
/*
*  Name:
*     TypeString

*  Purpose:
*     Return a pointer to a string describing a data type.

*  Type:
*     Private function.

*  Synopsis:
*      const char *TypeString( int type );

*  Description:
*     This function returns a pointer to a string describing a data type.

*  Parameters:
*     type
*        The integer data type code.

*  Returned Value:
*     Pointer to a a string descirbing the data type (typically the C
*     data type).

*/

/* Local Variables: */
   const char *result;

/* Compare the supplied type code against each supported value. */
   if( type == AST__INTTYPE ) {
      result = "int";

   } else if( type == AST__DOUBLETYPE ) {
      result = "double";

   } else if( type == AST__STRINGTYPE ) {
      result = "string";

   } else if( type == AST__OBJECTTYPE ) {
      result = "Object";

   } else if( type == AST__FLOATTYPE ) {
      result = "float";

   } else if( type == AST__POINTERTYPE ) {
      result = "pointer";

   } else if( type == AST__SINTTYPE ) {
      result = "short int";

   } else if( type == AST__UNDEFTYPE ) {
      result = "undefined";

   } else {
      result = NULL;
   }

/* Return the result. */
   return result;
}


/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/*
*att++
*  Name:
*     NRow

*  Purpose:
*     The number of rows in the table.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute holds the index of the last row to which any
*     contents have been added using any of the
*     astMapPut...
*     AST_MAPPUT...
*     functions. The first row has index 1.

*  Applicability:
*     Table
*        All Tables have this attribute.

*att--
*/
astMAKE_GET(Table,NRow,int,0,this->nrow)
astMAKE_SET(Table,NRow,int,nrow,value)

/*
*att++
*  Name:
*     NColumn

*  Purpose:
*     The number of columns in the table.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute holds the number of columns currently in the table. Columns
*     are added and removed using the
c     astAddColumn and astRemoveColumn
f     AST_ADDCOLUMN and AST_REMOVECOLUMN
*     functions.

*  Applicability:
*     Table
*        All Tables have this attribute.

*att--
*/


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Table objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Table objects.

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
*     Mappings within the Table.
*/

/* Local Variables: */
   AstTable *in;                /* Pointer to input Table */
   AstTable *out;               /* Pointer to output Table */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Tables. */
   in = (AstTable *) objin;
   out = (AstTable *) objout;

/* Make copies of the component KeyMaps and store pointers to them in the
   output Table structure. */
   out->columns = astCopy( in->columns );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Table objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Table objects.

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
   AstTable *this;              /* Pointer to Table */

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) obj;

/* Annul the pointers to the component KeyMaps. */
   this->columns = astAnnul( this->columns );

}


/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Table objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Table class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Table whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstTable *this;             /* Pointer to the Table structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Table structure. */
   this = (AstTable *) this_object;

/* Write out values representing the instance variables for the Table
   class.  Note, the primitive data in the Table will be written out
   by the parent KeyMap Dump function. This function deals just with the
   extra information held in the Table structure. */

/* Write out the number of rows in the table. */
   astWriteInt( channel, "NRow", 1, 1, astGetNRow( this ),
                "Number of rows in table" );

/* Write out the KeyMap holding parameters describing each column. */
   astWriteObject( channel, "Columns", 1, 0, this->columns, "KeyMap holding "
                   "column parameters" );
}










/* Standard class functions. */
/* ========================= */
/* Implement the astIsATable and astCheckTable functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Table,KeyMap)
astMAKE_CHECK(Table)

AstTable *astTable_( const char *options, int *status, ...) {
/*
*++
*  Name:
c     astTable
f     AST_TABLE

*  Purpose:
*     Create a Table.

*  Type:
*     Public function.

*  Synopsis:
c     #include "table.h"
c     AstTable *astTable( const char *options, ... )
f     RESULT = AST_TABLE( OPTIONS, STATUS )

*  Class Membership:
*     Table constructor.

*  Description:
*     This function creates a new empty Table and optionally initialises
*     its attributes.
*
*     The Table class is a type of KeyMap that represents a two-dimensional
*     table of values. The
c     astMapGet... and astMapPut...
f     AST_MAPGET... and AST_MAPPUT...
*     methods provided by the KeyMap class should be used for storing and
*     retrieving values from individual cells within a Table. Each entry
*     in the KeyMap represents a single cell of the table and has an
*     associated key of the form "<COL>(i)" where "<COL>" is the name of a
*     table column and "i" is the row index (the first row is row 1). Keys
*     of this form should always be used when using KeyMap methods to access
*     entries within a Table.
*
*     Columns must be declared using the
c     astAddColumn
f     AST_ADDCOLUMN
*     method before values can be stored within them. This also fixes the
*     type and shape of the values that may be stored in any cell of the
*     column. Cells may contain scalar or vector values of any data type
*     supported by the KeyMap class. Multi-dimensional arrays may also be
*     stored, but these must be vectorised when storing and retrieving
*     them within a table cell. All cells within a single column must
*     have the same type and shape (specified when the column is declared).
*
*     Note - since accessing entries within a KeyMap is a relatively slow
*     process, it is not recommended to use the Table class to store
*     very large tables.

*  Parameters:
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new Table. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Table. The syntax used is identical to that for the
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
c     astTable()
f     AST_TABLE = INTEGER
*        A pointer to the new Table.

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
   AstTable *new;              /* Pointer to new Table */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the Table, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitTable( NULL, sizeof( AstTable ), !class_init, &class_vtab,
                       "Table" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Table's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Table. */
   return new;
}

AstTable *astTableId_( const char *options, ... ) {
/*
*  Name:
*     astTableId_

*  Purpose:
*     Create a Table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "table.h"
*     AstTable *astTableId_( const char *options, ... )

*  Class Membership:
*     Table constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astTable constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astTable_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astTable_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astTable_.

*  Returned Value:
*     The ID value associated with the new Table.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTable *new;                /* Pointer to new Table */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the Table, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitTable( NULL, sizeof( AstTable ), !class_init, &class_vtab,
                         "Table" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Table's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Table. */
   return astMakeId( new );
}

AstTable *astInitTable_( void *mem, size_t size, int init,
                         AstTableVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitTable

*  Purpose:
*     Initialise a Table.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "table.h"
*     AstTable *astInitTable( void *mem, size_t size, int init,
*                                 AstTableVtab *vtab, const char *name )

*  Class Membership:
*     Table initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Table object. It allocates memory (if necessary) to accommodate
*     the Table plus any additional data associated with the derived class.
*     It then initialises a Table structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Table at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Table is to be initialised.
*        This must be of sufficient size to accommodate the Table data
*        (sizeof(Table)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Table (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Table
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Table's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Table.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).

*  Returned Value:
*     A pointer to the new Table.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstTable *new;              /* Pointer to new Table */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitTableVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Initialise a KeyMap structure (the parent class) as the first component
   within the Table structure, allocating memory if necessary. Specify that
   the KeyMap should be defined in both the forward and inverse directions. */
   new = (AstTable *) astInitKeyMap( mem, size, 0, (AstKeyMapVtab *) vtab,
                                     name );
   if ( astOK ) {

/* Initialise the Table data. */
/* ---------------------------- */
      new->nrow = 0;
      new->columns = astKeyMap( " ", status );

/* If an error occurred, clean up by deleting the new Table. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Table. */
   return new;
}

AstTable *astLoadTable_( void *mem, size_t size, AstTableVtab *vtab,
                         const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadTable

*  Purpose:
*     Load a Table.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "table.h"
*     AstTable *astLoadTable( void *mem, size_t size, AstTableVtab *vtab,
*                             const char *name, AstChannel *channel )

*  Class Membership:
*     Table loader.

*  Description:
*     This function is provided to load a new Table using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Table structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Table at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the Table is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Table data (sizeof(Table)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Table (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Table structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstTable) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Table. If this is NULL, a pointer
*        to the (static) virtual function table for the Table class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Table" is used instead.

*  Returned Value:
*     A pointer to the new Table.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstTable *new;                /* Pointer to the new Table */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Table. In this case the
   Table belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstTable );
      vtab = &class_vtab;
      name = "Table";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitTableVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Table. */
   new = astLoadKeyMap( mem, size, (AstKeyMapVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Table" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* The number of rows. */
      new->nrow = astReadInt( channel, "nrow", 0 );

/* KeyMap holding table global parameters. */
      new->columns = astReadObject( channel, "columns", NULL );

/* If an error occurred, clean up by deleting the new Table. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Table pointer. */
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

void astAddColumn_( AstTable *this, const char *name, int type,
                    int ndim, int *dims, int *status ) {
   if ( !astOK ) return;
   return (**astMEMBER(this,Table,AddColumn))(this,name,type,ndim,dims,status);
}
void astRemoveColumn_( AstTable *this, const char *name, int *status ){
   if ( !astOK ) return;
   return (**astMEMBER(this,Table,RemoveColumn))(this,name,status);
}
void astRemoveRow_( AstTable *this, int index, int *status ){
   if ( !astOK ) return;
   return (**astMEMBER(this,Table,RemoveRow))(this,index,status);
}
int astGetNColumn_( AstTable *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Table,GetNColumn))( this, status );
}
const char *astColumnName_( AstTable *this, int index, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Table,ColumnName))(this,index,status);
}
int astColumnType_( AstTable *this, const char *column, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Table,ColumnType))(this,column,status);
}
void astColumnShape_( AstTable *this, const char *column, int mxdim,
                      int *ndim, int *dims, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,Table,ColumnShape))( this, column, mxdim, ndim,
                                          dims, status );
}
