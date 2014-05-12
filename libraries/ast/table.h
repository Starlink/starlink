#if !defined( TABLE_INCLUDED )  /* Include this file only once */
#define TABLE_INCLUDED
/*
*+
*  Name:
*     table.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Table class.

*  Invocation:
*     #include "table.h"

*  Description:
*     This include file defines the interface to the Table class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.

*  Inheritance:
*     The Table class inherits from the KeyMap class.

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
*     22-NOV-2010 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "keymap.h"              /* Parent class */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#if defined(astCLASS)            /* Protected */

/* Maximum length of a column name */
#define AST__MXCOLNAMLEN 100

/* Maximum length of a key for a column cell */
#define AST__MXCOLKEYLEN ( AST__MXCOLNAMLEN + 23 )

#endif


/* Type Definitions. */
/* ================= */
/* Table structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstTable {

/* Attributes inherited from the parent class. */
   AstKeyMap keymap;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   int nrow;                     /* Mo. of rows in table */
   AstKeyMap *columns;           /* KeyMap holding column definitions */
   AstKeyMap *parameters;        /* KeyMap holding parameter definitions */
} AstTable;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstTableVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstKeyMapVtab keymap_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstKeyMap *(* ColumnProps)( AstTable *, int * );
   AstKeyMap *(* ParameterProps)( AstTable *, int * );
   const char *(* ColumnName)( AstTable *, int, int * );
   const char *(* ParameterName)( AstTable *, int, int * );
   const char *(* GetColumnUnit)( AstTable *, const char *, int * );
   int (* GetColumnLenC)( AstTable *, const char *, int * );
   int (* GetColumnLength)( AstTable *, const char *, int * );
   int (* GetColumnNdim)( AstTable *, const char *, int * );
   int (* GetColumnType)( AstTable *, const char *, int * );
   int (* GetNcolumn)( AstTable *, int * );
   int (* GetNparameter)( AstTable *, int * );
   int (* GetNrow)( AstTable *, int * );
   int (* HasColumn)( AstTable *, const char *, int * );
   int (* HasParameter)( AstTable *, const char *, int * );
   void (* AddColumn)( AstTable *, const char *, int, int, int *, const char *, int * );
   void (* AddParameter)( AstTable *, const char *, int * );
   void (* ColumnShape)( AstTable *, const char *, int, int *, int *, int * );
   void (* PurgeRows)( AstTable *, int * );
   void (* RemoveColumn)( AstTable *, const char *, int * );
   void (* RemoveParameter)( AstTable *, const char *, int * );
   void (* RemoveRow)( AstTable *, int, int * );
   void (* SetNrow)( AstTable *, int, int * );
} AstTableVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstTableGlobals {
   AstTableVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstTableGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitTableGlobals_( AstTableGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Table)           /* Check class membership */
astPROTO_ISA(Table)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstTable *astTable_( const char *, int *, ...);
#else
AstTable *astTableId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTable *astInitTable_( void *, size_t, int, AstTableVtab *,
                         const char *, int * );

/* Vtab initialiser. */
void astInitTableVtab_( AstTableVtab *, const char *, int * );

/* Loader. */
AstTable *astLoadTable_( void *, size_t, AstTableVtab *,
                         const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astAddColumn_( AstTable *, const char *, int, int, int *, const char *, int * );
void astAddParameter_( AstTable *, const char *, int * );
void astRemoveColumn_( AstTable *, const char *, int * );
void astRemoveParameter_( AstTable *, const char *, int * );
void astRemoveRow_( AstTable *, int, int * );
void astPurgeRows_( AstTable *, int * );
const char *astColumnName_( AstTable *, int, int * );
const char *astParameterName_( AstTable *, int, int * );
void astColumnShape_( AstTable *, const char *, int, int *, int *, int * );
int astHasColumn_( AstTable *, const char *, int * );
int astHasParameter_( AstTable *, const char *, int * );

#if defined(astCLASS)            /* Protected */
AstKeyMap *astColumnProps_( AstTable *, int * );
AstKeyMap *astParameterProps_( AstTable *, int * );
const char *astGetColumnUnit_( AstTable *, const char *, int * );
int astGetColumnLenC_( AstTable *, const char *, int * );
int astGetColumnLength_( AstTable *, const char *, int * );
int astGetColumnNdim_( AstTable *, const char *, int * );
int astGetColumnType_( AstTable *, const char *, int * );
int astGetNcolumn_( AstTable *, int * );
int astGetNparameter_( AstTable *, int * );
int astGetNrow_( AstTable *, int * );
void astSetNrow_( AstTable *, int, int * );
#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them
   to validate their own arguments. We must use a cast when passing
   object pointers (so that they can accept objects from derived
   classes). */

/* Check class membership. */
#define astCheckTable(this) astINVOKE_CHECK(Table,this,0)
#define astVerifyTable(this) astINVOKE_CHECK(Table,this,1)

/* Test class membership. */
#define astIsATable(this) astINVOKE_ISA(Table,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astTable astINVOKE(F,astTable_)
#else
#define astTable astINVOKE(F,astTableId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitTable(mem,size,init,vtab,name) \
astINVOKE(O,astInitTable_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitTableVtab(vtab,name) astINVOKE(V,astInitTableVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadTable(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTable_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckTable to validate Table pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astAddColumn(this,name,type,ndim,dims,unit) astINVOKE(V,astAddColumn_(astCheckTable(this),name,type,ndim,dims,unit, STATUS_PTR))
#define astAddParameter(this,name) astINVOKE(V,astAddParameter_(astCheckTable(this),name,STATUS_PTR))
#define astRemoveColumn(this,name) astINVOKE(V,astRemoveColumn_(astCheckTable(this),name,STATUS_PTR))
#define astRemoveParameter(this,name) astINVOKE(V,astRemoveParameter_(astCheckTable(this),name,STATUS_PTR))
#define astRemoveRow(this,index) astINVOKE(V,astRemoveRow_(astCheckTable(this),index,STATUS_PTR))
#define astPurgeRows(this) astINVOKE(V,astPurgeRows_(astCheckTable(this),STATUS_PTR))
#define astColumnName(this,index) astINVOKE(V,astColumnName_(astCheckTable(this),index,STATUS_PTR))
#define astParameterName(this,index) astINVOKE(V,astParameterName_(astCheckTable(this),index,STATUS_PTR))
#define astColumnShape(this,column,mxdim,ndim,dims) astINVOKE(V,astColumnShape_(astCheckTable(this),column,mxdim,ndim,dims,STATUS_PTR))
#define astHasColumn(this,column) astINVOKE(V,astHasColumn_(astCheckTable(this),column,STATUS_PTR))
#define astHasParameter(this,param) astINVOKE(V,astHasParameter_(astCheckTable(this),param,STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astColumnProps(this) \
astINVOKE(O,astColumnProps_(astCheckTable(this),STATUS_PTR))
#define astParameterProps(this) \
astINVOKE(O,astParameterProps_(astCheckTable(this),STATUS_PTR))
#define astGetNcolumn(this) \
astINVOKE(V,astGetNcolumn_(astCheckTable(this),STATUS_PTR))
#define astGetNparameter(this) \
astINVOKE(V,astGetNparameter_(astCheckTable(this),STATUS_PTR))
#define astGetNrow(this) \
astINVOKE(V,astGetNrow_(astCheckTable(this),STATUS_PTR))
#define astSetNrow(this,value) \
astINVOKE(V,astSetNrow_(astCheckTable(this),value,STATUS_PTR))
#define astGetColumnLenC(this,column) \
astINVOKE(V,astGetColumnLenC_(astCheckTable(this),column,STATUS_PTR))
#define astGetColumnLength(this,column) \
astINVOKE(V,astGetColumnLength_(astCheckTable(this),column,STATUS_PTR))
#define astGetColumnNdim(this,column) \
astINVOKE(V,astGetColumnNdim_(astCheckTable(this),column,STATUS_PTR))
#define astGetColumnType(this,column) \
astINVOKE(V,astGetColumnType_(astCheckTable(this),column,STATUS_PTR))
#define astGetColumnUnit(this,column) \
astINVOKE(V,astGetColumnUnit_(astCheckTable(this),column,STATUS_PTR))

#endif
#endif





