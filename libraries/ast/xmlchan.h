#if !defined( XMLCHAN_INCLUDED ) /* Include this file only once */
#define XMLCHAN_INCLUDED
/*
*+
*  Name:
*     xmlchan.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the XmlChan class.

*  Invocation:
*     #include "xmlchan.h"

*  Description:
*     This include file defines the interface to the XmlChan class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The XmlChan class provides facilities for reading and writing AST 
*     Objects in the form of XML.

*  Inheritance:
*     The XmlChan class inherits from the Channel class.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

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
*     DSB: D.S. Berry (Starlink)

*  History:
*     10-OCT-2003 (DSB):
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "channel.h"             /* I/O channels (parent class) */
#include "keymap.h"              /* Mappings of keys to values */
#include "xml.h"                 /* AST XML facilities */

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

/* URI defining the starlink AST XML namespace */
#define AST__XMLNS  "http://www.starlink.ac.uk/ast/xml/"

#if defined(astCLASS)            /* Protected */
#endif

/* Type Definitions. */
/* ================= */

/* XmlChan structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstXmlChan {

/* Attributes inherited from the parent class. */
   AstChannel channel;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   const char *objectname;     /* Name of Object currently being written. */
   const char *objectcomment;  /* Comment for Object currently being written. */
   int objectset;              /* Is the Object currently being written set? */
   AstXmlParent *container;    /* XmlParent to which content will be added */
   AstXmlDocument *readcontext;/* XmlDocument giving context for current read */
   int write_isa;              /* Is the next "isA" really needed? */
   int xmlindent;              /* Indentat output? */
   int xmlstrict;              /* Abort read on warning? */
   int xmllength;              /* Buffer length */
   int xmlformat;              /* Output format to use when writing */
   int formatdef;              /* Default format */
   char *xmlprefix;            /* Namespace prefix */
   int reset_source;           /* Read a new line from the source ? */
   const char *isa_class;      /* Class being loaded */
   AstKeyMap *warnings;        /* A list of warning messages */
} AstXmlChan;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstXmlChanVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstChannelVtab channel_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   AstKeyMap *(* XmlWarnings)( AstXmlChan *, int * );

   int (* GetXmlIndent)( AstXmlChan *, int * );
   int (* TestXmlIndent)( AstXmlChan *, int * );
   void (* ClearXmlIndent)( AstXmlChan *, int * );
   void (* SetXmlIndent)( AstXmlChan *, int, int * );

   int (* GetXmlStrict)( AstXmlChan *, int * );
   int (* TestXmlStrict)( AstXmlChan *, int * );
   void (* ClearXmlStrict)( AstXmlChan *, int * );
   void (* SetXmlStrict)( AstXmlChan *, int, int * );

   int (* GetXmlLength)( AstXmlChan *, int * );
   int (* TestXmlLength)( AstXmlChan *, int * );
   void (* ClearXmlLength)( AstXmlChan *, int * );
   void (* SetXmlLength)( AstXmlChan *, int, int * );

   int (* GetXmlFormat)( AstXmlChan *, int * );
   int (* TestXmlFormat)( AstXmlChan *, int * );
   void (* ClearXmlFormat)( AstXmlChan *, int * );
   void (* SetXmlFormat)( AstXmlChan *, int, int * );

   const char * (* GetXmlPrefix)( AstXmlChan *, int * );
   int (* TestXmlPrefix)( AstXmlChan *, int * );
   void (* ClearXmlPrefix)( AstXmlChan *, int * );
   void (* SetXmlPrefix)( AstXmlChan *, const char *, int * );

} AstXmlChanVtab;

#if defined(THREAD_SAFE) 
typedef struct AstXmlChanGlobals {
   AstXmlChanVtab Class_Vtab;
   int Class_Init;
   AstXmlChan *IsUsable_This;
   char GetAttrib_Buff[ 51 ];
   char *GetNextChar_C;    
   char *GetNextChar_Buf;  
   int Report_NWarn;
} AstXmlChanGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(XmlChan)          /* Check class membership */
astPROTO_ISA(XmlChan)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstXmlChan *astXmlChan_( const char *(*)( void ), void (*)( const char * ),
                          const char *, int *, ...);
#else
AstXmlChan *astXmlChanId_( const char *(*)( void ), void (*)( const char * ),
                            const char *, ... )
#ifdef __GNUC__ /* Check the variable argument list if using GNU compiler */
__attribute__((format(printf,3,4)))
#endif
;
AstXmlChan *astXmlChanForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ), int * ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char *, int * ),
                              const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstXmlChan *astInitXmlChan_( void *, size_t, int, AstXmlChanVtab *,
                             const char *, const char *(*)( void ), 
                             char *(*)( const char *(*)( void ), int * ), 
                             void (*)( const char * ), 
                             void (*)( void (*)( const char * ), 
                             const char *, int * ), int * );

/* Vtab initialiser. */
void astInitXmlChanVtab_( AstXmlChanVtab *, const char *, int * );



/* Loader. */
AstXmlChan *astLoadXmlChan_( void *, size_t, AstXmlChanVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE) 
void astInitXmlChanGlobals_( AstXmlChanGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstKeyMap *astXmlWarnings_( AstXmlChan *, int * );

# if defined(astCLASS)           /* Protected */
int astGetXmlIndent_( AstXmlChan *, int * );
int astTestXmlIndent_( AstXmlChan *, int * );
void astClearXmlIndent_( AstXmlChan *, int * );
void astSetXmlIndent_( AstXmlChan *, int, int * );

int astGetXmlStrict_( AstXmlChan *, int * );
int astTestXmlStrict_( AstXmlChan *, int * );
void astClearXmlStrict_( AstXmlChan *, int * );
void astSetXmlStrict_( AstXmlChan *, int, int * );

int astGetXmlLength_( AstXmlChan *, int * );
int astTestXmlLength_( AstXmlChan *, int * );
void astClearXmlLength_( AstXmlChan *, int * );
void astSetXmlLength_( AstXmlChan *, int, int * );

int astGetXmlFormat_( AstXmlChan *, int * );
int astTestXmlFormat_( AstXmlChan *, int * );
void astClearXmlFormat_( AstXmlChan *, int * );
void astSetXmlFormat_( AstXmlChan *, int, int * );

const char * astGetXmlPrefix_( AstXmlChan *, int * );
int astTestXmlPrefix_( AstXmlChan *, int * );
void astClearXmlPrefix_( AstXmlChan *, int * );
void astSetXmlPrefix_( AstXmlChan *, const char *, int * );


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
#define astCheckXmlChan(this) astINVOKE_CHECK(XmlChan,this)

/* Test class membership. */
#define astIsAXmlChan(this) astINVOKE_ISA(XmlChan,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astXmlChan astINVOKE(F,astXmlChan_)
#else
#define astXmlChan astINVOKE(F,astXmlChanId_)
#define astXmlChanFor astINVOKE(F,astXmlChanForId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitXmlChan(mem,size,init,vtab,name,source,source_wrap,sink,sink_wrap) \
astINVOKE(O,astInitXmlChan_(mem,size,init,vtab,name,source,source_wrap,sink,sink_wrap,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitXmlChanVtab(vtab,name) astINVOKE(V,astInitXmlChanVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadXmlChan(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadXmlChan_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckXmlChan to validate XmlChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astXmlWarnings(this) astINVOKE(O,astXmlWarnings_(astCheckXmlChan(this),STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astClearXmlIndent(this) astINVOKE(V,astClearXmlIndent_(astCheckXmlChan(this),STATUS_PTR))
#define astGetXmlIndent(this) astINVOKE(V,astGetXmlIndent_(astCheckXmlChan(this),STATUS_PTR))
#define astSetXmlIndent(this,xmlindent) astINVOKE(V,astSetXmlIndent_(astCheckXmlChan(this),xmlindent,STATUS_PTR))
#define astTestXmlIndent(this) astINVOKE(V,astTestXmlIndent_(astCheckXmlChan(this),STATUS_PTR))

#define astClearXmlStrict(this) astINVOKE(V,astClearXmlStrict_(astCheckXmlChan(this),STATUS_PTR))
#define astGetXmlStrict(this) astINVOKE(V,astGetXmlStrict_(astCheckXmlChan(this),STATUS_PTR))
#define astSetXmlStrict(this,xmlstrict) astINVOKE(V,astSetXmlStrict_(astCheckXmlChan(this),xmlstrict,STATUS_PTR))
#define astTestXmlStrict(this) astINVOKE(V,astTestXmlStrict_(astCheckXmlChan(this),STATUS_PTR))

#define astClearXmlLength(this) astINVOKE(V,astClearXmlLength_(astCheckXmlChan(this),STATUS_PTR))
#define astGetXmlLength(this) astINVOKE(V,astGetXmlLength_(astCheckXmlChan(this),STATUS_PTR))
#define astSetXmlLength(this,xmllength) astINVOKE(V,astSetXmlLength_(astCheckXmlChan(this),xmllength,STATUS_PTR))
#define astTestXmlLength(this) astINVOKE(V,astTestXmlLength_(astCheckXmlChan(this),STATUS_PTR))

#define astClearXmlFormat(this) astINVOKE(V,astClearXmlFormat_(astCheckXmlChan(this),STATUS_PTR))
#define astGetXmlFormat(this) astINVOKE(V,astGetXmlFormat_(astCheckXmlChan(this),STATUS_PTR))
#define astSetXmlFormat(this,xmlformat) astINVOKE(V,astSetXmlFormat_(astCheckXmlChan(this),xmlformat,STATUS_PTR))
#define astTestXmlFormat(this) astINVOKE(V,astTestXmlFormat_(astCheckXmlChan(this),STATUS_PTR))

#define astClearXmlPrefix(this) astINVOKE(V,astClearXmlPrefix_(astCheckXmlChan(this),STATUS_PTR))
#define astGetXmlPrefix(this) astINVOKE(V,astGetXmlPrefix_(astCheckXmlChan(this),STATUS_PTR))
#define astSetXmlPrefix(this,xmlpref) astINVOKE(V,astSetXmlPrefix_(astCheckXmlChan(this),xmlpref,STATUS_PTR))
#define astTestXmlPrefix(this) astINVOKE(V,astTestXmlPrefix_(astCheckXmlChan(this),STATUS_PTR))

#endif

#endif





