#if !defined( STCSCHAN_INCLUDED ) /* Include this file only once */
#define STCSCHAN_INCLUDED
/*
*+
*  Name:
*     stcschan.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the StcsChan class.

*  Invocation:
*     #include "stcschan.h"

*  Description:
*     This include file defines the interface to the StcsChan class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The StcsChan class provides facilities for reading and writing AST
*     Objects in the form of STC-S text.

*  Inheritance:
*     The StcsChan class inherits from the Channel class.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: D.S. Berry (JAC, UCLan)

*  History:
*     18-DEC-2008 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "channel.h"             /* I/O channels (parent class) */

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

/* Define constants used to size global arrays in this module. */
/* Define other numerical constants for use in this module. */
#define AST__STCSCHAN_GETATTRIB_BUFF_LEN 200

/* Type Definitions. */
/* ================= */

/* StcsChan structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstStcsChan {

/* Attributes inherited from the parent class. */
   AstChannel channel;          /* Parent class structure */

/* Attributes specific to objects in this class. */
   int stcsarea;                /* Read the STC CoordinatesArea? */
   int stcscoords;              /* Read the STC Coordinates? */
   int stcsprops;               /* Read the STC-S properties? */
   int stcslength;              /* Line length */
} AstStcsChan;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstStcsChanVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstChannelVtab channel_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* GetStcsArea)( AstStcsChan *, int * );
   int (* TestStcsArea)( AstStcsChan *, int * );
   void (* ClearStcsArea)( AstStcsChan *, int * );
   void (* SetStcsArea)( AstStcsChan *, int, int * );

   int (* GetStcsCoords)( AstStcsChan *, int * );
   int (* TestStcsCoords)( AstStcsChan *, int * );
   void (* ClearStcsCoords)( AstStcsChan *, int * );
   void (* SetStcsCoords)( AstStcsChan *, int, int * );

   int (* GetStcsProps)( AstStcsChan *, int * );
   int (* TestStcsProps)( AstStcsChan *, int * );
   void (* ClearStcsProps)( AstStcsChan *, int * );
   void (* SetStcsProps)( AstStcsChan *, int, int * );

   int (* GetStcsLength)( AstStcsChan *, int * );
   int (* TestStcsLength)( AstStcsChan *, int * );
   void (* ClearStcsLength)( AstStcsChan *, int * );
   void (* SetStcsLength)( AstStcsChan *, int, int * );

} AstStcsChanVtab;

#if defined(THREAD_SAFE)
typedef struct AstStcsChanGlobals {
   AstStcsChanVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__STCSCHAN_GETATTRIB_BUFF_LEN + 1 ];
} AstStcsChanGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(StcsChan)          /* Check class membership */
astPROTO_ISA(StcsChan)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstStcsChan *astStcsChan_( const char *(*)( void ), void (*)( const char * ),
                          const char *, int *, ...);
#else
AstStcsChan *astStcsChanId_( const char *(*)( void ), void (*)( const char * ),
                            const char *, ... );
AstStcsChan *astStcsChanForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ), int * ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char *, int * ),
                              const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstStcsChan *astInitStcsChan_( void *, size_t, int, AstStcsChanVtab *,
                             const char *, const char *(*)( void ),
                             char *(*)( const char *(*)( void ), int * ),
                             void (*)( const char * ),
                             void (*)( void (*)( const char * ),
                             const char *, int * ), int * );

/* Vtab initialiser. */
void astInitStcsChanVtab_( AstStcsChanVtab *, const char *, int * );



/* Loader. */
AstStcsChan *astLoadStcsChan_( void *, size_t, AstStcsChanVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitStcsChanGlobals_( AstStcsChanGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

# if defined(astCLASS)           /* Protected */
int astGetStcsArea_( AstStcsChan *, int * );
int astTestStcsArea_( AstStcsChan *, int * );
void astClearStcsArea_( AstStcsChan *, int * );
void astSetStcsArea_( AstStcsChan *, int, int * );

int astGetStcsCoords_( AstStcsChan *, int * );
int astTestStcsCoords_( AstStcsChan *, int * );
void astClearStcsCoords_( AstStcsChan *, int * );
void astSetStcsCoords_( AstStcsChan *, int, int * );

int astGetStcsProps_( AstStcsChan *, int * );
int astTestStcsProps_( AstStcsChan *, int * );
void astClearStcsProps_( AstStcsChan *, int * );
void astSetStcsProps_( AstStcsChan *, int, int * );

int astGetStcsLength_( AstStcsChan *, int * );
int astTestStcsLength_( AstStcsChan *, int * );
void astClearStcsLength_( AstStcsChan *, int * );
void astSetStcsLength_( AstStcsChan *, int, int * );

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
#define astCheckStcsChan(this) astINVOKE_CHECK(StcsChan,this,0)
#define astVerifyStcsChan(this) astINVOKE_CHECK(StcsChan,this,1)

/* Test class membership. */
#define astIsAStcsChan(this) astINVOKE_ISA(StcsChan,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astStcsChan astINVOKE(F,astStcsChan_)
#else
#define astStcsChan astINVOKE(F,astStcsChanId_)
#define astStcsChanFor astINVOKE(F,astStcsChanForId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitStcsChan(mem,size,init,vtab,name,source,source_wrap,sink,sink_wrap) \
astINVOKE(O,astInitStcsChan_(mem,size,init,vtab,name,source,source_wrap,sink,sink_wrap,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitStcsChanVtab(vtab,name) astINVOKE(V,astInitStcsChanVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadStcsChan(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadStcsChan_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckStcsChan to validate StcsChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */


#if defined(astCLASS)            /* Protected */

#define astClearStcsArea(this) \
astINVOKE(V,astClearStcsArea_(astCheckStcsChan(this),STATUS_PTR))
#define astGetStcsArea(this) \
astINVOKE(V,astGetStcsArea_(astCheckStcsChan(this),STATUS_PTR))
#define astSetStcsArea(this,value) \
astINVOKE(V,astSetStcsArea_(astCheckStcsChan(this),value,STATUS_PTR))
#define astTestStcsArea(this) \
astINVOKE(V,astTestStcsArea_(astCheckStcsChan(this),STATUS_PTR))

#define astClearStcsCoords(this) \
astINVOKE(V,astClearStcsCoords_(astCheckStcsChan(this),STATUS_PTR))
#define astGetStcsCoords(this) \
astINVOKE(V,astGetStcsCoords_(astCheckStcsChan(this),STATUS_PTR))
#define astSetStcsCoords(this,value) \
astINVOKE(V,astSetStcsCoords_(astCheckStcsChan(this),value,STATUS_PTR))
#define astTestStcsCoords(this) \
astINVOKE(V,astTestStcsCoords_(astCheckStcsChan(this),STATUS_PTR))

#define astClearStcsProps(this) \
astINVOKE(V,astClearStcsProps_(astCheckStcsChan(this),STATUS_PTR))
#define astGetStcsProps(this) \
astINVOKE(V,astGetStcsProps_(astCheckStcsChan(this),STATUS_PTR))
#define astSetStcsProps(this,value) \
astINVOKE(V,astSetStcsProps_(astCheckStcsChan(this),value,STATUS_PTR))
#define astTestStcsProps(this) \
astINVOKE(V,astTestStcsProps_(astCheckStcsChan(this),STATUS_PTR))

#define astClearStcsLength(this) astINVOKE(V,astClearStcsLength_(astCheckStcsChan(this),STATUS_PTR))
#define astGetStcsLength(this) astINVOKE(V,astGetStcsLength_(astCheckStcsChan(this),STATUS_PTR))
#define astSetStcsLength(this,stcslength) astINVOKE(V,astSetStcsLength_(astCheckStcsChan(this),stcslength,STATUS_PTR))
#define astTestStcsLength(this) astINVOKE(V,astTestStcsLength_(astCheckStcsChan(this),STATUS_PTR))

#endif
#endif




