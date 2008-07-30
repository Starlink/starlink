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

*  Macros:
*     Public:
*        AST__UNDEFF
*           Floating point value marking an undefined keyword value.
*        AST__UNDEFS
*           A string used to mark an undefined keyword value.
*        AST__UNDEFI
*           Integer value marking an undefined keyword value.
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
*        AST__CONTINUE 
*           Integer dentifier for the continuation string FITS data type.
*        AST__COMPLEXF
*           Integer dentifier for the complex floating point FITS data type.
*        AST__COMPLEXI
*           Integer dentifier for the complex integer FITS data type.
*        AST__LOGICAL 
*           Integer dentifier for the logical FITS data type.
*        AST__UNDEF
*           Integer dentifier for undefined FITS data type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

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
*     11-DEC-1996 (DSB):
*        Original version.
*     30-Jun-1997 (DSB):
*        Changed character pointer argument to character array in PutFits.
*     26-SEP-1997 (DSB):
*        Added CDMatrix attribute.
*     21-OCT-1997 (DSB):
*        o  Renamed astFields as astKeyFields.
*     1-APR-2000 (DSB):
*        Changes for CDi_j based FITS-WCS standard.
*     18-MAY-2000 (DSB):
*        Added Warnings attribute.
*     4-APR-2001 (DSB):
*        Added AllWarnings attribute.
*     20-FEB-2002 (DSB):
*        Added CarLin attribute.
*     8-JAN-2003 (DSB):
*        Added protected astInitFitsChanVtab method.
*     13-FEB-2003 (DSB):
*        Added Clean attribute.
*     19-MAR-2004 (DSB):
*        Added astPutCards function.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "channel.h"             /* I/O channels (parent class) */
#include "pointset.h"            /* Defines AST__BAD */
#include "keymap.h"              /* Defines the AstKeyMap type */

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */

#define AST__UNDEFF  -123456789.0
#define AST__UNDEFS  "<undefined>"
#define AST__UNDEFI -123456789

/* Support macros to simplify defined-ness checks */
/* These are experimental. Pending approval by David and the fortran
   interface may require a subroutine interface */
#define astIsUndefF(val)  ( val == AST__UNDEFF )
#define astIsUndefI(val)  ( val == AST__UNDEFI )
#define astIsUndefS(val)  ( strcmp(val, AST__UNDEFS) == 0 )

#if defined(astCLASS)            /* Protected */
#define AST__NOTYPE       -1
#define AST__COMMENT       0
#define AST__INT           1
#define AST__FLOAT         2  
#define AST__STRING        3
#define AST__COMPLEXF      4
#define AST__COMPLEXI      5
#define AST__LOGICAL       6
#define AST__CONTINUE      7
#define AST__UNDEF         8
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
   int defb1950;    /* Use FK4 B1950 as defaults? */
   int cdmatrix;    /* Use a CD matrix in FITS-WCS Encoding? */
   int carlin;      /* Use linear CAR mappings? */
   int iwc;         /* Include an IWC Frame? */
   int clean;       /* Remove used cards even if an error occurs? */
   int fitsdigits;  /* No. of decmial places in formatted floating point keyword values */
   char *warnings;  /* Pointer to a string containing warning conditions */
   void *card;      /* Pointer to next FitsCard to be read */
   void *head;      /* Pointer to first FitsCard in the circular linked list */
   AstKeyMap *keyseq;   /* List of keyword sequence numbers used */
   AstKeyMap *keywords; /* A KeyMap holding the keywords in the FitsChan */
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
   void (* PurgeWCS)( AstFitsChan * );
   void (* RetainFits)( AstFitsChan * );
   int (* FindFits)( AstFitsChan *, const char *, char [81], int );
   void (* PutFits)( AstFitsChan *, const char [81], int );
   void (* PutCards)( AstFitsChan *, const char * );
   int (* KeyFields)( AstFitsChan *, const char *, int, int *, int *);
   int (* FitsEof)( AstFitsChan * );
   int (* GetFitsCF)( AstFitsChan *, const char *, double * );
   int (* GetFitsCI)( AstFitsChan *, const char *, int * );
   int (* GetFitsF)( AstFitsChan *, const char *, double * );
   int (* GetFitsI)( AstFitsChan *, const char *, int * );
   int (* GetFitsL)( AstFitsChan *, const char *, int * );
   int (* GetFitsU)( AstFitsChan *, const char *, int * );
   int (* GetFitsS)( AstFitsChan *, const char *, char ** );
   int (* GetFitsCN)( AstFitsChan *, const char *, char ** );
   int (* FitsGetCom)( AstFitsChan *, const char *, char ** );
   void (* SetFitsCom)( AstFitsChan *, const char *, const char *, int  );
   void (* SetFitsCF)( AstFitsChan *, const char *, double *, const char *, int  );
   void (* SetFitsCI)( AstFitsChan *, const char *, int *, const char *, int  );
   void (* SetFitsF)( AstFitsChan *, const char *, double, const char *, int  );
   void (* SetFitsI)( AstFitsChan *, const char *, int, const char *, int  );
   void (* SetFitsL)( AstFitsChan *, const char *, int, const char *, int  );
   void (* SetFitsU)( AstFitsChan *, const char *, int, const char *, int  );
   void (* SetFitsS)( AstFitsChan *, const char *, const char *, const char *, int  );
   void (* SetFitsCN)( AstFitsChan *, const char *, const char *, const char *, int  );
   int (* GetCard)( AstFitsChan * );
   int (* TestCard)( AstFitsChan * );
   void (* SetCard)( AstFitsChan *, int );
   void (* ClearCard)( AstFitsChan * );
   int (* GetFitsDigits)( AstFitsChan * );
   int (* TestFitsDigits)( AstFitsChan * );
   void (* SetFitsDigits)( AstFitsChan *, int );
   void (* ClearFitsDigits)( AstFitsChan * );
   int (* GetDefB1950)( AstFitsChan * );
   int (* TestDefB1950)( AstFitsChan * );
   void (* SetDefB1950)( AstFitsChan *, int );
   void (* ClearDefB1950)( AstFitsChan * );
   int (* GetCarLin)( AstFitsChan * );
   int (* TestCarLin)( AstFitsChan * );
   void (* SetCarLin)( AstFitsChan *, int );
   void (* ClearCarLin)( AstFitsChan * );
   int (* GetNcard)( AstFitsChan * );
   int (* GetEncoding)( AstFitsChan * );
   int (* TestEncoding)( AstFitsChan * );
   void (* SetEncoding)( AstFitsChan *, int );
   void (* ClearEncoding)( AstFitsChan * );
   const char *(* GetWarnings)( AstFitsChan * );
   const char *(* GetAllWarnings)( AstFitsChan * );
   int (* TestWarnings)( AstFitsChan * );
   void (* ClearWarnings)( AstFitsChan * );
   void (* SetWarnings)( AstFitsChan *, const char * );
   int (* GetClean)( AstFitsChan * );
   int (* TestClean)( AstFitsChan * );
   void (* SetClean)( AstFitsChan *, int );
   void (* ClearClean)( AstFitsChan * );
   int (* GetCDMatrix)( AstFitsChan * );
   int (* TestCDMatrix)( AstFitsChan * );
   void (* SetCDMatrix)( AstFitsChan *, int );
   void (* ClearCDMatrix)( AstFitsChan * );

   int (* GetIwc)( AstFitsChan * );
   int (* TestIwc)( AstFitsChan * );
   void (* SetIwc)( AstFitsChan *, int );
   void (* ClearIwc)( AstFitsChan * );

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

/* Vtab initialiser. */
void astInitFitsChanVtab_( AstFitsChanVtab *, const char * );



/* Loader. */
AstFitsChan *astLoadFitsChan_( void *, size_t, AstFitsChanVtab *,
                               const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   void astPutFits_( AstFitsChan *, const char [81], int );
   void astPutCards_( AstFitsChan *, const char * );
   int  astFindFits_( AstFitsChan *, const char *, char [81], int );
   void astDelFits_( AstFitsChan * );
   void astPurgeWCS_( AstFitsChan * );
   void astRetainFits_( AstFitsChan * );
   void astSetFitsCF_( AstFitsChan *, const char *, double *, const char *, int  );
   void astSetFitsCI_( AstFitsChan *, const char *, int *, const char *, int  );
   void astSetFitsF_( AstFitsChan *, const char *, double, const char *, int  );
   void astSetFitsI_( AstFitsChan *, const char *, int, const char *, int  );
   void astSetFitsL_( AstFitsChan *, const char *, int, const char *, int  );
   void astSetFitsU_( AstFitsChan *, const char *, int, const char *, int  );
   void astSetFitsS_( AstFitsChan *, const char *, const char *, const char *, int  );
   void astSetFitsCN_( AstFitsChan *, const char *, const char *, const char *, int  );
   int  astGetFitsCF_( AstFitsChan *, const char *, double * );
   int  astGetFitsCI_( AstFitsChan *, const char *, int * );
   int  astGetFitsF_( AstFitsChan *, const char *, double * );
   int  astGetFitsI_( AstFitsChan *, const char *, int * );
   int  astGetFitsL_( AstFitsChan *, const char *, int * );
   int  astGetFitsU_( AstFitsChan *, const char *, int * );
   int  astGetFitsS_( AstFitsChan *, const char *, char ** );
   int  astGetFitsCN_( AstFitsChan *, const char *, char ** );

# if defined(astCLASS)           /* Protected */

   int  astFitsEof_( AstFitsChan * );
   int  astFitsGetCom_( AstFitsChan *, const char *, char ** );
   void astSetFitsCom_( AstFitsChan *, const char *, const char *, int  );

   void astEmpty_( AstFitsChan * );
   int  astKeyFields_( AstFitsChan *, const char *, int, int *, int *);

   int astGetCard_( AstFitsChan * );
   int astTestCard_( AstFitsChan * );
   void astSetCard_( AstFitsChan *, int );
   void astClearCard_( AstFitsChan * );

   int astGetDefB1950_( AstFitsChan * );
   int astTestDefB1950_( AstFitsChan * );
   void astSetDefB1950_( AstFitsChan *, int );
   void astClearDefB1950_( AstFitsChan * );

   int astGetCDMatrix_( AstFitsChan * );
   int astTestCDMatrix_( AstFitsChan * );
   void astSetCDMatrix_( AstFitsChan *, int );
   void astClearCDMatrix_( AstFitsChan * );

   int astGetCarLin_( AstFitsChan * );
   int astTestCarLin_( AstFitsChan * );
   void astSetCarLin_( AstFitsChan *, int );
   void astClearCarLin_( AstFitsChan * );

   int astGetIwc_( AstFitsChan * );
   int astTestIwc_( AstFitsChan * );
   void astSetIwc_( AstFitsChan *, int );
   void astClearIwc_( AstFitsChan * );

   int astGetClean_( AstFitsChan * );
   int astTestClean_( AstFitsChan * );
   void astSetClean_( AstFitsChan *, int );
   void astClearClean_( AstFitsChan * );

   int astGetFitsDigits_( AstFitsChan * );
   int astTestFitsDigits_( AstFitsChan * );
   void astSetFitsDigits_( AstFitsChan *, int );
   void astClearFitsDigits_( AstFitsChan * );

   const char *astGetAllWarnings_( AstFitsChan * );

   const char *astGetWarnings_( AstFitsChan * );
   int astTestWarnings_( AstFitsChan * );
   void astClearWarnings_( AstFitsChan * );
   void astSetWarnings_( AstFitsChan *, const char * );

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

/* Vtab Initialiser. */
#define astInitFitsChanVtab(vtab,name) astINVOKE(V,astInitFitsChanVtab_(vtab,name))
/* Loader. */
#define astLoadFitsChan(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFitsChan_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckFitsChan to validate FitsChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astPutFits(this,card,overwrite) \
astINVOKE(V,astPutFits_(astCheckFitsChan(this),card,overwrite))

#define astPutCards(this,cards) \
astINVOKE(V,astPutCards_(astCheckFitsChan(this),cards))

#define astDelFits(this) \
astINVOKE(V,astDelFits_(astCheckFitsChan(this)))

#define astPurgeWCS(this) \
astINVOKE(V,astPurgeWCS_(astCheckFitsChan(this)))

#define astRetainFits(this) \
astINVOKE(V,astRetainFits_(astCheckFitsChan(this)))

#define astFindFits( this, name, card, inc ) \
astINVOKE(V,astFindFits_(astCheckFitsChan(this),name,card,inc))

#define astSetFitsI(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsI_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsF(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsF_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsS(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsS_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsCN(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCN_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsCI(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCI_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsCF(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCF_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsL(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsL_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astSetFitsU(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsU_(astCheckFitsChan(this),name,value,comment,overwrite))

#define astGetFitsCF(this,name,value) \
astINVOKE(V,astGetFitsCF_(astCheckFitsChan(this),name,value))

#define astGetFitsCI(this,name,value) \
astINVOKE(V,astGetFitsCI_(astCheckFitsChan(this),name,value))

#define astGetFitsF(this,name,value) \
astINVOKE(V,astGetFitsF_(astCheckFitsChan(this),name,value))

#define astGetFitsI(this,name,value) \
astINVOKE(V,astGetFitsI_(astCheckFitsChan(this),name,value))

#define astGetFitsL(this,name,value) \
astINVOKE(V,astGetFitsL_(astCheckFitsChan(this),name,value))

#define astGetFitsU(this,name,value) \
astINVOKE(V,astGetFitsU_(astCheckFitsChan(this),name,value))

#define astGetFitsS(this,name,value) \
astINVOKE(V,astGetFitsS_(astCheckFitsChan(this),name,value))

#define astGetFitsCN(this,name,value) \
astINVOKE(V,astGetFitsCN_(astCheckFitsChan(this),name,value))


#if defined(astCLASS)            /* Protected */

#define astFitsEof(this) \
astINVOKE(V,astFitsEof_(astCheckFitsChan(this)))

#define astFitsGetCom(this,name,comment) \
astINVOKE(V,astFitsGetCom_(astCheckFitsChan(this),name,comment))

#define astSetFitsCom(this,name,comment,overwrite) \
astINVOKE(V,astSetFitsCom_(astCheckFitsChan(this),name,comment,overwrite))

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

#define astClearDefB1950(this) \
astINVOKE(V,astClearDefB1950_(astCheckFitsChan(this)))
#define astGetDefB1950(this) \
astINVOKE(V,astGetDefB1950_(astCheckFitsChan(this)))
#define astSetDefB1950(this,defb950) \
astINVOKE(V,astSetDefB1950_(astCheckFitsChan(this),defb950))
#define astTestDefB1950(this) \
astINVOKE(V,astTestDefB1950_(astCheckFitsChan(this)))

#define astClearCDMatrix(this) \
astINVOKE(V,astClearCDMatrix_(astCheckFitsChan(this)))
#define astGetCDMatrix(this) \
astINVOKE(V,astGetCDMatrix_(astCheckFitsChan(this)))
#define astSetCDMatrix(this,cdmatrix) \
astINVOKE(V,astSetCDMatrix_(astCheckFitsChan(this),cdmatrix))
#define astTestCDMatrix(this) \
astINVOKE(V,astTestCDMatrix_(astCheckFitsChan(this)))

#define astClearCarLin(this) \
astINVOKE(V,astClearCarLin_(astCheckFitsChan(this)))
#define astGetCarLin(this) \
astINVOKE(V,astGetCarLin_(astCheckFitsChan(this)))
#define astSetCarLin(this,carln) \
astINVOKE(V,astSetCarLin_(astCheckFitsChan(this),carln))
#define astTestCarLin(this) \
astINVOKE(V,astTestCarLin_(astCheckFitsChan(this)))

#define astClearClean(this) \
astINVOKE(V,astClearClean_(astCheckFitsChan(this)))
#define astGetClean(this) \
astINVOKE(V,astGetClean_(astCheckFitsChan(this)))
#define astSetClean(this,value) \
astINVOKE(V,astSetClean_(astCheckFitsChan(this),value))
#define astTestClean(this) \
astINVOKE(V,astTestClean_(astCheckFitsChan(this)))

#define astClearFitsDigits(this) \
astINVOKE(V,astClearFitsDigits_(astCheckFitsChan(this)))
#define astGetFitsDigits(this) \
astINVOKE(V,astGetFitsDigits_(astCheckFitsChan(this)))
#define astSetFitsDigits(this,fitsdigits) \
astINVOKE(V,astSetFitsDigits_(astCheckFitsChan(this),fitsdigits))
#define astTestFitsDigits(this) \
astINVOKE(V,astTestFitsDigits_(astCheckFitsChan(this)))

#define astClearWarnings(this) \
astINVOKE(V,astClearWarnings_(astCheckFitsChan(this)))
#define astGetWarnings(this) \
astINVOKE(V,astGetWarnings_(astCheckFitsChan(this)))
#define astSetWarnings(this,warnings) \
astINVOKE(V,astSetWarnings_(astCheckFitsChan(this),warnings))
#define astTestWarnings(this) \
astINVOKE(V,astTestWarnings_(astCheckFitsChan(this)))

#define astGetAllWarnings(this) \
astINVOKE(V,astGetAllWarnings_(astCheckFitsChan(this)))

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

#define astClearIwc(this) \
astINVOKE(V,astClearIwc_(astCheckFitsChan(this)))
#define astGetIwc(this) \
astINVOKE(V,astGetIwc_(astCheckFitsChan(this)))
#define astSetIwc(this,iwc) \
astINVOKE(V,astSetIwc_(astCheckFitsChan(this),iwc))
#define astTestIwc(this) \
astINVOKE(V,astTestIwc_(astCheckFitsChan(this)))

#endif

#endif
