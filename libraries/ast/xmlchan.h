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
*     <COPYRIGHT_STATEMENT>

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
#include "xml.h"                 /* AST XML facilities */

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */

/* URI defining the starlink AST XML namespace */
#define AST__XMLNS  "http://www.starlink.ac.uk/"

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
   AstXmlElement *container;   /* XmlElement to which content will be added */
   int write_isa;              /* Is the next "isA" really needed? */
   int xmlindent;              /* Indentat output? */
   int xmllength;              /* Buffer length */
   int reset_source;           /* Read a new line from the source ? */
   const char *isa_class;      /* Class being loaded */
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
   int (* GetXmlIndent)( AstXmlChan * );
   int (* TestXmlIndent)( AstXmlChan * );
   void (* ClearXmlIndent)( AstXmlChan * );
   void (* SetXmlIndent)( AstXmlChan *, int );

   int (* GetXmlLength)( AstXmlChan * );
   int (* TestXmlLength)( AstXmlChan * );
   void (* ClearXmlLength)( AstXmlChan * );
   void (* SetXmlLength)( AstXmlChan *, int );

} AstXmlChanVtab;
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
                          const char *, ... );
#else
AstXmlChan *astXmlChanId_( const char *(*)( void ), void (*)( const char * ),
                            const char *, ... );
AstXmlChan *astXmlChanForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ) ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char * ),
                              const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstXmlChan *astInitXmlChan_( void *, size_t, int, AstXmlChanVtab *,
                             const char *, const char *(*)( void ), 
                             char *(*)( const char *(*)( void ) ), 
                             void (*)( const char * ), 
                             void (*)( void (*)( const char * ), 
                             const char * ) );

/* Vtab initialiser. */
void astInitXmlChanVtab_( AstXmlChanVtab *, const char * );



/* Loader. */
AstXmlChan *astLoadXmlChan_( void *, size_t, AstXmlChanVtab *,
                               const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */

# if defined(astCLASS)           /* Protected */
int astGetXmlIndent_( AstXmlChan * );
int astTestXmlIndent_( AstXmlChan * );
void astClearXmlIndent_( AstXmlChan * );
void astSetXmlIndent_( AstXmlChan *, int );

int astGetXmlLength_( AstXmlChan * );
int astTestXmlLength_( AstXmlChan * );
void astClearXmlLength_( AstXmlChan * );
void astSetXmlLength_( AstXmlChan *, int );
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
astINVOKE(O,astInitXmlChan_(mem,size,init,vtab,name,source,source_wrap,sink,sink_wrap))

/* Vtab Initialiser. */
#define astInitXmlChanVtab(vtab,name) astINVOKE(V,astInitXmlChanVtab_(vtab,name))
/* Loader. */
#define astLoadXmlChan(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadXmlChan_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckXmlChan to validate XmlChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astClearXmlIndent(this) astINVOKE(V,astClearXmlIndent_(astCheckXmlChan(this)))
#define astGetXmlIndent(this) astINVOKE(V,astGetXmlIndent_(astCheckXmlChan(this)))
#define astSetXmlIndent(this,xmlindent) astINVOKE(V,astSetXmlIndent_(astCheckXmlChan(this),xmlindent))
#define astTestXmlIndent(this) astINVOKE(V,astTestXmlIndent_(astCheckXmlChan(this)))

#define astClearXmlLength(this) astINVOKE(V,astClearXmlLength_(astCheckXmlChan(this)))
#define astGetXmlLength(this) astINVOKE(V,astGetXmlLength_(astCheckXmlChan(this)))
#define astSetXmlLength(this,xmllength) astINVOKE(V,astSetXmlLength_(astCheckXmlChan(this),xmllength))
#define astTestXmlLength(this) astINVOKE(V,astTestXmlLength_(astCheckXmlChan(this)))

#endif

#endif
