#if !defined( FITSCHAN_INCLUDED ) /* Include this file only once */
#define FITSCHAN_INCLUDED
/*
*+
*  Name:
*     fitschan.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the FitsChan class.

*  Invocation:
*     #include "fitschan.h"

*  Description:
*     This include file defines the interface to the FitsChan class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The FitsChan class provides facilities for reading and writing AST 
*     Objects in the form of FITS header cards.

*  Inheritance:
*     The FitsChan class inherits from the Channel class.

*  Attributes Over-Ridden:
*     Full (integer)
*        The FitsChan class modifies the default value of the Full
*        attribute to become -1. This prevents the output of
*        non-essential information to a FitsChan by astWrite, unless
*        it is explicitly requested by setting a Full value.
*     Skip (integer)
*        The FitsChan class modifies the default value of the Skip
*        attribute to become 1. This default value allows the astRead
*        function to skip over unrelated FITS data when searching for
*        the next Object to read.

*  New Attributes Defined:
*     Card
*        This attribute gives the index of the current card in a FitsChan
*        (i.e. the next card to be read). The first card in the FitsChan has 
*        index 1. This attribute can be set to a value greater than the number 
*        of cards in the FitsChan in order to position the current card at
*        the "end-of-file". In this case no more cards can be read from the
*        FitsChan, and any new cards stored in the FitsChan will be appended
*        to the end. In this situation, the value of the attribute will
*        always be returned as one greater than the number of cards in the 
*        FitsChan. Clearing this attribute is equivelent to setting its value 
*        to 1 (this is like performing a "rewind" on the FitsChan). Testing the
*        attribute returns 1 if the current card is at the "start-of-file"
*        and zero otherwise.
*     Encoding
*        This attribute specifies the system to use when encoding AST
*        objects into FITS headers. A value of "Native" causes AST objects 
*        to be described using FITS keywords recognised only by the AST
*        library. Such FITS headers may only be read using other AST
*        applications. A value of "FitsWcs" causes AST objects to be
*        described using the keywords reserved for the FITS World Coordinate
*        System (CRVAL1, CRPIX1, etc). Such FITS headers can be read using
*        any FITS WCS application. Using encoding systems other than
*        "Native" may impose restrictions on the number and class of objects 
*        which can be stored in a FITS header.
*     FitsDigits
*        This attribute gives the number of digits to display after the
*        decimal point when formatting floating point FITS keyword values. 
*        By default, a value is used which results in no loss of information.
*        This may need to be set to a smaller value such as 15 to satisfy 
*        the recommendations of the FITS standard, which says that floating
*        point values should be formatted in no more than 20 columns.
*     Ncard
*        This read-only attribute gives the number of cards in the FitsChan.

*  Methods Over-Ridden:
*     Public:
*        astRead
*           Read an Object from a FitsChan.
*        astWrite
*           Write an Object to a FitsChan.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a FitsChan.
*        astGetAttrib
*           Get an attribute value for a FitsChan.
*        astGetFull
*           Get the value of the Full attribute for a FitsChan.
*        astGetNextData
*           Read the next item of data from a data source.
*        astGetSkip
*           Get the value of the Skip attribute for a FitsChan.
*        astSetAttrib
*           Set an attribute value for a FitsChan.
*        astTestAttrib
*           Test if an attribute value has been set for a FitsChan.
*        astWriteBegin
*           Write a "Begin" data item to a data sink.
*        astWriteDouble
*           Write a double value to a data sink.
*        astWriteEnd
*           Write an "End" data item to a data sink.
*        astWriteInt
*           Write an int value to a data sink.
*        astWriteIsA
*           Write an "IsA" data item to a data sink.
*        astWriteObject
*           Write an Object value to a data sink.
*        astWriteString
*           Write a string value to a data sink.

*  New Methods Defined:
*     Public:
*        astDelFits
*           Delete a FITS header card from a FitsChan.
*        astFindFits
*           Find the next FITS header card for a given keyword.
*        astPutFits
*           Store a FITS header card in a FitsChan.
*
*     Protected:
*        astFitsEof
*           See if the FitsChan is at "end-of-file".
*        astFitsGet<X>
*           Get a value for a keyword from a FitsChan.
*        astFitsGetCom
*           Get a keyword comment from a FitsChan.
*        astFitsSet<X>
*           Store a keyword value in a FitsChan.
*        astSplit
*           Splits a header card up into its component parts.
*        astClearCard
*           Clear the Card attribute value for a FitsChan.
*        astClearEncoding  
*           Clear the Encoding attribute value for a FitsChan.
*        astClearFitsDigits  
*           Clear the FitsDigits attribute value for a FitsChan.
*        astClearCDMatrix
*           Clear the CDMatrix attribute value for a FitsChan.
*        astEmpty
*           Remove all cards and related data from a FitsChan.
*        astKeyFields
*           Find the ranges taken by fields within keyword names.
*        astGetCard  
*           Get the value of the Card attribute for a FitsChan.
*        astGetEncoding  
*           Get the value of the Encoding attribute for a FitsChan.
*        astGetFitsDigits  
*           Get the value of the FitsDigits attribute for a FitsChan.
*        astGetCDMatrix
*           Get the value of the CDMatrix attribute for a FitsChan.
*        astGetNcard
*           Get the value of the Ncard attribute for a FitsChan.
*        astSetCard  
*           Set the value of the Card attribute for a FitsChan.
*        astSetEncoding  
*           Set the value of the Encoding attribute for a FitsChan.
*        astSetCDMatrix
*           Set the value of the CDMatrix attribute for a FitsChan.
*        astSetFitsDigits  
*           Set the value of the FitsDigits attribute for a FitsChan.
*        astTestCard  
*           Test the Card attribute value for a FitsChan.
*        astTestEncoding  
*           Test the Encoding attribute value for a FitsChan.
*        astTestFitsDigits  
*           Test the FitsDigits attribute value for a FitsChan.
*        astTestCDMatrix
*           Test the CDMatrix attribute value for a FitsChan.
           
*  Other Class Functions:
*     Public:
*        astIsAFitsChan
*           Test class membership.
*        astFitsChan
*           Create a FitsChan.
*
*     Protected:
*        astCheckFitsChan
*           Validate class membership.
*        astInitFitsChan
*           Initialise a FitsChan.
*        astLoadFitsChan
*           Load a FitsChan.

*  Macros:
*     Public:
*        None.
*
*     Protected:
*        AST__NOTYPE
*           Integer dentifier for an illegal FITS data type.
*        AST__COMMENT
*           Integer dentifier for a FITS comment keyword.
*        AST__INT    
*           Integer dentifier for the integer FITS data type.
*        AST__FLOAT  
*           Integer dentifier for the floating point FITS data type.
*        AST__STRING 
*           Integer dentifier for the string FITS data type.
*        AST__COMPLEXF
*           Integer dentifier for the complex floating point FITS data type.
*        AST__COMPLEXI
*           Integer dentifier for the complex integer FITS data type.
*        AST__LOGICAL 
*           Integer dentifier for the logical FITS data type.

*  Type Definitions:
*     Public:
*        AstFitsChan
*           FitsChan object type.
*
*     Protected:
*        AstFitsChanVtab
*           FitsChan virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     11-DEC-1996 (DSB):
*        Original version.
*     30-Jun-1997 (DSB):
*        Changed character pointer argument to character array in PutFits.
*     26-SEP-1997 (DSB):
*        Added CDMatrix attribute.
*     21-OCT-1997 (DSB):
*        o  Renamed astFields as astKeyFields.
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
#if defined(astCLASS)            /* Protected */
#define AST__NOTYPE       -1
#define AST__COMMENT       0
#define AST__INT           1
#define AST__FLOAT         2  
#define AST__STRING        3
#define AST__COMPLEXF      4
#define AST__COMPLEXI      5
#define AST__LOGICAL       6
#endif

/* Type Definitions. */
/* ================= */

/* FitsChan structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstFitsChan {

/* Attributes inherited from the parent class. */
   AstChannel channel;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   int encoding;    /* System for encoding AST objects ito FITS headers */
   int fitsdigits;  /* No. of decmial places in formatted floating point keyword values */
   int cdmatrix;    /* Should CD matrices be written? */
   void *card;      /* Pointer to next FitsCard to be read */
   void *head;      /* Pointer to first FitsCard in the circular linked list */
   void *keyseq;    /* List of keyword sequence numbers used */
   const char *(* source)( void ); /* Pointer to source function */
   char *(* source_wrap)( const char *(*)( void ) );
                                   /* Source wrapper function pointer */
   void (* sink)( const char * );  /* Pointer to sink function */
   void (* sink_wrap)( void (*)( const char * ), const char * );
                                   /* Sink wrapper function pointer */

} AstFitsChan;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstFitsChanVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstChannelVtab channel_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   void (* Empty)( AstFitsChan * );
   void (* DelFits)( AstFitsChan * );
   int (* FindFits)( AstFitsChan *, const char *, char [81], int );
   void (* PutFits)( AstFitsChan *, const char [81], int );
   int (* KeyFields)( AstFitsChan *, const char *, int, int *, int *);
   int (* FitsEof)( AstFitsChan * );
   int (* FitsGetCF)( AstFitsChan *, const char *, double * );
   int (* FitsGetCI)( AstFitsChan *, const char *, int * );
   int (* FitsGetF)( AstFitsChan *, const char *, double * );
   int (* FitsGetI)( AstFitsChan *, const char *, int * );
   int (* FitsGetL)( AstFitsChan *, const char *, int * );
   int (* FitsGetS)( AstFitsChan *, const char *, char ** );
   int (* FitsGetCom)( AstFitsChan *, const char *, char ** );
   void (* FitsSetCom)( AstFitsChan *, const char *, const char *, int  );
   void (* FitsSetCF)( AstFitsChan *, const char *, double *, const char *, int  );
   void (* FitsSetCI)( AstFitsChan *, const char *, int *, const char *, int  );
   void (* FitsSetF)( AstFitsChan *, const char *, double, const char *, int  );
   void (* FitsSetI)( AstFitsChan *, const char *, int, const char *, int  );
   void (* FitsSetL)( AstFitsChan *, const char *, int, const char *, int  );
   void (* FitsSetS)( AstFitsChan *, const char *, const char *, const char *, int  );
   int (* GetCard)( AstFitsChan * );
   int (* TestCard)( AstFitsChan * );
   void (* SetCard)( AstFitsChan *, int );
   void (* ClearCard)( AstFitsChan * );
   int (* GetFitsDigits)( AstFitsChan * );
   int (* TestFitsDigits)( AstFitsChan * );
   void (* SetFitsDigits)( AstFitsChan *, int );
   void (* ClearFitsDigits)( AstFitsChan * );
   int (* GetCDMatrix)( AstFitsChan * );
   int (* TestCDMatrix)( AstFitsChan * );
   void (* SetCDMatrix)( AstFitsChan *, int );
   void (* ClearCDMatrix)( AstFitsChan * );
   int (* GetNcard)( AstFitsChan * );
   int (* GetEncoding)( AstFitsChan * );
   int (* TestEncoding)( AstFitsChan * );
   void (* SetEncoding)( AstFitsChan *, int );
   void (* ClearEncoding)( AstFitsChan * );

} AstFitsChanVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(FitsChan)          /* Check class membership */
astPROTO_ISA(FitsChan)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstFitsChan *astFitsChan_( const char *(*)( void ), void (*)( const char * ),
                          const char *, ... );
#else
AstFitsChan *astFitsChanId_( const char *(*)( void ), void (*)( const char * ),
                            const char *, ... );
AstFitsChan *astFitsChanForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ) ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char * ),
                              const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFitsChan *astInitFitsChan_( void *, size_t, int, AstFitsChanVtab *, 
                               const char *, 
                               const char *(*)( void ),
                               char *(*)( const char *(*)( void ) ),
                               void (*)( const char * ),
                               void (*)( void (*)( const char * ), const char * ) );



/* Loader. */
AstFitsChan *astLoadFitsChan_( void *, size_t, int, AstFitsChanVtab *,
                               const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   void astPutFits_( AstFitsChan *, const char [81], int );
   int  astFindFits_( AstFitsChan *, const char *, char [81], int );
   void astDelFits_( AstFitsChan * );

# if defined(astCLASS)           /* Protected */

   int  astFitsEof_( AstFitsChan * );
   int  astFitsGetCF_( AstFitsChan *, const char *, double * );
   int  astFitsGetCI_( AstFitsChan *, const char *, int * );
   int  astFitsGetF_( AstFitsChan *, const char *, double * );
   int  astFitsGetI_( AstFitsChan *, const char *, int * );
   int  astFitsGetL_( AstFitsChan *, const char *, int * );
   int  astFitsGetS_( AstFitsChan *, const char *, char ** );
   int  astFitsGetCom_( AstFitsChan *, const char *, char ** );
   void astFitsSetCom_( AstFitsChan *, const char *, const char *, int  );
   void astFitsSetCF_( AstFitsChan *, const char *, double *, const char *, int  );
   void astFitsSetCI_( AstFitsChan *, const char *, int *, const char *, int  );
   void astFitsSetF_( AstFitsChan *, const char *, double, const char *, int  );
   void astFitsSetI_( AstFitsChan *, const char *, int, const char *, int  );
   void astFitsSetL_( AstFitsChan *, const char *, int, const char *, int  );
   void astFitsSetS_( AstFitsChan *, const char *, const char *, const char *, int  );

   void astEmpty_( AstFitsChan * );
   int  astKeyFields_( AstFitsChan *, const char *, int, int *, int *);

   int astGetCard_( AstFitsChan * );
   int astTestCard_( AstFitsChan * );
   void astSetCard_( AstFitsChan *, int );
   void astClearCard_( AstFitsChan * );

   int astGetFitsDigits_( AstFitsChan * );
   int astTestFitsDigits_( AstFitsChan * );
   void astSetFitsDigits_( AstFitsChan *, int );
   void astClearFitsDigits_( AstFitsChan * );

   int astGetCDMatrix_( AstFitsChan * );
   int astTestCDMatrix_( AstFitsChan * );
   void astSetCDMatrix_( AstFitsChan *, int );
   void astClearCDMatrix_( AstFitsChan * );

   int astGetNcard_( AstFitsChan * );

   int astGetEncoding_( AstFitsChan * );
   int astTestEncoding_( AstFitsChan * );
   void astSetEncoding_( AstFitsChan *, int );
   void astClearEncoding_( AstFitsChan * );

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
#define astCheckFitsChan(this) astINVOKE_CHECK(FitsChan,this)

/* Test class membership. */
#define astIsAFitsChan(this) astINVOKE_ISA(FitsChan,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astFitsChan astINVOKE(F,astFitsChan_)
#else
#define astFitsChan astINVOKE(F,astFitsChanId_)
#define astFitsChanFor astINVOKE(F,astFitsChanForId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitFitsChan(mem,size,init,vtab,name,source,sourcewrap,sink,sinkwrap) \
astINVOKE(O,astInitFitsChan_(mem,size,init,vtab,name,source,sourcewrap,sink,sinkwrap))

/* Loader. */
#define astLoadFitsChan(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadFitsChan_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckFitsChan to validate FitsChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astPutFits(this,card,overwrite) \
astINVOKE(V,astPutFits_(astCheckFitsChan(this),card,overwrite))

#define astDelFits(this) \
astINVOKE(V,astDelFits_(astCheckFitsChan(this)))

#define astFindFits( this, name, card, inc ) \
astINVOKE(V,astFindFits_(astCheckFitsChan(this),name,card,inc))


#if defined(astCLASS)            /* Protected */

#define astFitsEof(this) \
astINVOKE(V,astFitsEof_(astCheckFitsChan(this)))

#define astFitsGetCF(this,name,value) \
astINVOKE(V,astFitsGetCF_(astCheckFitsChan(this),name,value))

#define astFitsGetCI(this,name,value) \
astINVOKE(V,astFitsGetCI_(astCheckFitsChan(this),name,value))

#define astFitsGetF(this,name,value) \
astINVOKE(V,astFitsGetF_(astCheckFitsChan(this),name,value))

#define astFitsGetI(this,name,value) \
astINVOKE(V,astFitsGetI_(astCheckFitsChan(this),name,value))

#define astFitsGetL(this,name,value) \
astINVOKE(V,astFitsGetL_(astCheckFitsChan(this),name,value))

#define astFitsGetS(this,name,value) \
astINVOKE(V,astFitsGetS_(astCheckFitsChan(this),name,value))

#define astFitsGetCom(this,name,comment) \
astINVOKE(V,astFitsGetCom_(astCheckFitsChan(this),name,comment))

#define astFitsSetCom(this,name,comment,overwrite) \
astINVOKE(V,astFitsSetCom_(astCheckFitsChan(this),name,comment,overwrite))

#define astFitsSetI(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetI_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astFitsSetF(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetF_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astFitsSetS(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetS_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astFitsSetCI(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetCI_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astFitsSetCF(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetCF_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astFitsSetL(this,name,value,comment,overwrite) \
astINVOKE(V,astFitsSetL_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSplit astSplit_

#define astEmpty(this) \
astINVOKE(V,astEmpty_(astCheckFitsChan(this)))

#define astKeyFields(this,filter,maxfld,ubnd,lbnd) \
astINVOKE(V,astKeyFields_(astCheckFitsChan(this),filter,maxfld,ubnd,lbnd))

#define astClearCard(this) \
astINVOKE(V,astClearCard_(astCheckFitsChan(this)))
#define astGetCard(this) \
astINVOKE(V,astGetCard_(astCheckFitsChan(this)))
#define astSetCard(this,card) \
astINVOKE(V,astSetCard_(astCheckFitsChan(this),card))
#define astTestCard(this) \
astINVOKE(V,astTestCard_(astCheckFitsChan(this)))

#define astClearFitsDigits(this) \
astINVOKE(V,astClearFitsDigits_(astCheckFitsChan(this)))
#define astGetFitsDigits(this) \
astINVOKE(V,astGetFitsDigits_(astCheckFitsChan(this)))
#define astSetFitsDigits(this,fitsdigits) \
astINVOKE(V,astSetFitsDigits_(astCheckFitsChan(this),fitsdigits))
#define astTestFitsDigits(this) \
astINVOKE(V,astTestFitsDigits_(astCheckFitsChan(this)))

#define astClearCDMatrix(this) \
astINVOKE(V,astClearCDMatrix_(astCheckFitsChan(this)))
#define astGetCDMatrix(this) \
astINVOKE(V,astGetCDMatrix_(astCheckFitsChan(this)))
#define astSetCDMatrix(this,cdmatrix) \
astINVOKE(V,astSetCDMatrix_(astCheckFitsChan(this),cdmatrix))
#define astTestCDMatrix(this) \
astINVOKE(V,astTestCDMatrix_(astCheckFitsChan(this)))

#define astGetNcard(this) \
astINVOKE(V,astGetNcard_(astCheckFitsChan(this)))

#define astClearEncoding(this) \
astINVOKE(V,astClearEncoding_(astCheckFitsChan(this)))
#define astGetEncoding(this) \
astINVOKE(V,astGetEncoding_(astCheckFitsChan(this)))
#define astSetEncoding(this,encoding) \
astINVOKE(V,astSetEncoding_(astCheckFitsChan(this),encoding))
#define astTestEncoding(this) \
astINVOKE(V,astTestEncoding_(astCheckFitsChan(this)))

#endif

#endif
