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
   AstKeyMap *columns;           /* KeyMap holding column parameters */
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
   void (* AddColumn)( AstTable *, const char *, int, int, int *,
                       const char *, int * );
   void (* RemoveColumn)( AstTable *, const char *, int * );
   void (* RemoveRow)( AstTable *, int, int * );
   void (* PurgeRows)( AstTable *, int * );
   int (* GetNrow)( AstTable *, int * );
   int (* GetNcolumn)( AstTable *, int * );
   void (* SetNrow)( AstTable *, int, int * );
   const char *(* ColumnName)( AstTable *, int, int * );
   int (* ColumnType)( AstTable *, const char *, int * );
   const char *(* ColumnUnit)( AstTable *, const char *, int * );
   AstKeyMap *(* ColumnProps)( AstTable *, int * );
   int (* ColumnLenC)( AstTable *, const char *, int * );
   int (* ColumnNdim)( AstTable *, const char *, int * );
   void (* ColumnShape)( AstTable *, const char *, int, int *, int *, int * );
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
AstKeyMap *astColumnProps_( AstTable *, int * );
void astAddColumn_( AstTable *, const char *, int, int, int *, const char *, int * );
void astRemoveColumn_( AstTable *, const char *, int * );
void astRemoveRow_( AstTable *, int, int * );
void astPurgeRows_( AstTable *, int * );
int astGetNrow_( AstTable *, int * );
int astGetNcolumn_( AstTable *, int * );
void astSetNrow_( AstTable *, int, int * );
const char *astColumnName_( AstTable *, int, int * );
int astColumnType_( AstTable *, const char *, int * );
const char *astColumnUnit_( AstTable *, const char *, int * );
int astColumnLenC_( AstTable *, const char *, int * );
int astColumnNdim_( AstTable *, const char *, int * );
void astColumnShape_( AstTable *, const char *, int, int *, int *, int * );

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
#define astRemoveColumn(this,name) astINVOKE(V,astRemoveColumn_(astCheckTable(this),name,STATUS_PTR))
#define astRemoveRow(this,index) astINVOKE(V,astRemoveRow_(astCheckTable(this),index,STATUS_PTR))
#define astPurgeRows(this) astINVOKE(V,astPurgeRows_(astCheckTable(this),STATUS_PTR))
#define astColumnName(this,index) astINVOKE(V,astColumnName_(astCheckTable(this),index,STATUS_PTR))
#define astColumnType(this,column) astINVOKE(V,astColumnType_(astCheckTable(this),column,STATUS_PTR))
#define astColumnUnit(this,column) astINVOKE(V,astColumnUnit_(astCheckTable(this),column,STATUS_PTR))
#define astColumnLenC(this,column) astINVOKE(V,astColumnLenC_(astCheckTable(this),column,STATUS_PTR))
#define astColumnNdim(this,column) astINVOKE(V,astColumnNdim_(astCheckTable(this),column,STATUS_PTR))
#define astColumnShape(this,column,mxdim,ndim,dims) astINVOKE(V,astColumnShape_(astCheckTable(this),column,mxdim,ndim,dims,STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astColumnProps(this) astINVOKE(O,astColumnProps_(astCheckTable(this),STATUS_PTR))
#define astGetNcolumn(this) \
astINVOKE(V,astGetNcolumn_(astCheckTable(this),STATUS_PTR))
#define astGetNrow(this) \
astINVOKE(V,astGetNrow_(astCheckTable(this),STATUS_PTR))
#define astSetNrow(this,value) \
astINVOKE(V,astSetNrow_(astCheckTable(this),value,STATUS_PTR))

#endif
#endif





