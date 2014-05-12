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

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

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

#if defined(astCLASS)            /* Protected */

/* Define constants used to size global arrays in this module. */
#define AST__FITSCHAN_FITSCARDLEN        80
#define AST__FITSCHAN_GETATTRIB_BUFF_LEN 50

#endif

/* The EXTNAME value for FITS binary tables used to store coordinate arrays for
   the -TAB algorithm. */
#define AST_TABEXTNAME "WCS-TAB"

/* Type Definitions. */
/* ================= */

/* FitsChan structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
struct AstFitsChan;
typedef struct AstFitsChan {

/* Attributes inherited from the parent class. */
   AstChannel channel;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   int encoding;    /* System for encoding AST objects ito FITS headers */
   int defb1950;    /* Use FK4 B1950 as defaults? */
   int tabok;       /* Support -TAB algorithm? */
   int cdmatrix;    /* Use a CD matrix in FITS-WCS Encoding? */
   int polytan;     /* Use distorted TAN convention? */
   int carlin;      /* Use linear CAR mappings? */
   int iwc;         /* Include an IWC Frame? */
   int clean;       /* Remove used cards even if an error occurs? */
   int fitsdigits;  /* No. of decmial places in formatted floating point keyword values */
   char *warnings;  /* Pointer to a string containing warning conditions */
   void *card;      /* Pointer to next FitsCard to be read */
   void *head;      /* Pointer to first FitsCard in the circular linked list */
   AstKeyMap *keyseq;   /* List of keyword sequence numbers used */
   AstKeyMap *keywords; /* A KeyMap holding the keywords in the FitsChan */
   AstKeyMap *tables;   /* A KeyMap holding the binary tables in the FitsChan */

   const char *(* source)( void ); /* Pointer to source function */
   const char *(* saved_source)( void ); /* Pointer to saved source function */
   char *(* source_wrap)( const char *(*)( void ), int * );
                                   /* Source wrapper function pointer */

   void (* sink)( const char * );  /* Pointer to sink function */
   void (* sink_wrap)( void (*)( const char * ), const char *, int * );
                                   /* Sink wrapper function pointer */

   void (* tabsource)( void );     /* Pointer to table source function */
   void (* tabsource_wrap)( void (*)( void ), struct AstFitsChan *, const char *, int, int, int * );
                                   /* Table source wrapper function pointer */

} AstFitsChan;

/* Virtual function table. */
/* ----------------------- */
/* The virtual function table makes a forward reference to the
   AstFitsTable structure which is not defined until "fitstable.h" is
   included (below). Hence make a preliminary definition available
   now. */
struct AstFitsTable;

/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstFitsChanVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstChannelVtab channel_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstKeyMap *(* GetTables)( AstFitsChan *, int * );
   int (* FindFits)( AstFitsChan *, const char *, char [81], int, int * );
   int (* FitsEof)( AstFitsChan *, int * );
   int (* FitsGetCom)( AstFitsChan *, const char *, char **, int * );
   int (* GetFitsCF)( AstFitsChan *, const char *, double *, int * );
   int (* GetFitsCI)( AstFitsChan *, const char *, int *, int * );
   int (* GetFitsCN)( AstFitsChan *, const char *, char **, int * );
   int (* GetFitsF)( AstFitsChan *, const char *, double *, int * );
   int (* GetFitsI)( AstFitsChan *, const char *, int *, int * );
   int (* GetFitsL)( AstFitsChan *, const char *, int *, int * );
   int (* GetFitsS)( AstFitsChan *, const char *, char **, int * );
   int (* KeyFields)( AstFitsChan *, const char *, int, int *, int *, int * );
   int (* TestFits)( AstFitsChan *, const char *, int *, int * );
   void (* DelFits)( AstFitsChan *, int * );
   void (* Empty)( AstFitsChan *, int * );
   void (* ReadFits)( AstFitsChan *, int * );
   void (* WriteFits)( AstFitsChan *, int * );
   void (* EmptyFits)( AstFitsChan *, int * );
   void (* ShowFits)( AstFitsChan *, int * );
   void (* PurgeWCS)( AstFitsChan *, int * );
   void (* PutCards)( AstFitsChan *, const char *, int * );
   void (* PutFits)( AstFitsChan *, const char [81], int, int * );
   void (* PutTable)( AstFitsChan *, struct AstFitsTable *, const char *, int * );
   void (* PutTables)( AstFitsChan *, AstKeyMap *, int * );
   void (* RemoveTables)( AstFitsChan *, const char *, int * );
   void (* RetainFits)( AstFitsChan *, int * );
   void (* SetFitsCF)( AstFitsChan *, const char *, double *, const char *, int, int * );
   void (* SetFitsCI)( AstFitsChan *, const char *, int *, const char *, int, int * );
   void (* SetFitsCM)( AstFitsChan *, const char *, int, int * );
   void (* SetFitsCN)( AstFitsChan *, const char *, const char *, const char *, int, int * );
   void (* SetFitsCom)( AstFitsChan *, const char *, const char *, int, int * );
   void (* SetFitsF)( AstFitsChan *, const char *, double, const char *, int, int * );
   void (* SetFitsI)( AstFitsChan *, const char *, int, const char *, int, int * );
   void (* SetFitsL)( AstFitsChan *, const char *, int, const char *, int, int * );
   void (* SetFitsS)( AstFitsChan *, const char *, const char *, const char *, int, int * );
   void (* SetFitsU)( AstFitsChan *, const char *, const char *, int, int * );

   int (* GetCard)( AstFitsChan *, int * );
   int (* TestCard)( AstFitsChan *, int * );
   void (* SetCard)( AstFitsChan *, int, int * );
   void (* ClearCard)( AstFitsChan *, int * );

   int (* GetFitsDigits)( AstFitsChan *, int * );
   int (* TestFitsDigits)( AstFitsChan *, int * );
   void (* SetFitsDigits)( AstFitsChan *, int, int * );
   void (* ClearFitsDigits)( AstFitsChan *, int * );

   int (* GetDefB1950)( AstFitsChan *, int * );
   int (* TestDefB1950)( AstFitsChan *, int * );
   void (* SetDefB1950)( AstFitsChan *, int, int * );
   void (* ClearDefB1950)( AstFitsChan *, int * );

   int (* GetTabOK)( AstFitsChan *, int * );
   int (* TestTabOK)( AstFitsChan *, int * );
   void (* SetTabOK)( AstFitsChan *, int, int * );
   void (* ClearTabOK)( AstFitsChan *, int * );

   int (* GetCarLin)( AstFitsChan *, int * );
   int (* TestCarLin)( AstFitsChan *, int * );
   void (* SetCarLin)( AstFitsChan *, int, int * );
   void (* ClearCarLin)( AstFitsChan *, int * );

   int (* GetNcard)( AstFitsChan *, int * );

   int (* GetCardType)( AstFitsChan *, int * );
   const char *(* GetCardName)( AstFitsChan *, int * );
   const char *(* GetCardComm)( AstFitsChan *, int * );

   int (* GetNkey)( AstFitsChan *, int * );

   int (* GetEncoding)( AstFitsChan *, int * );
   int (* TestEncoding)( AstFitsChan *, int * );
   void (* SetEncoding)( AstFitsChan *, int, int * );
   void (* ClearEncoding)( AstFitsChan *, int * );

   const char *(* GetAllWarnings)( AstFitsChan *, int * );

   const char *(* GetWarnings)( AstFitsChan *, int * );
   int (* TestWarnings)( AstFitsChan *, int * );
   void (* ClearWarnings)( AstFitsChan *, int * );
   void (* SetWarnings)( AstFitsChan *, const char *, int * );

   int (* GetClean)( AstFitsChan *, int * );
   int (* TestClean)( AstFitsChan *, int * );
   void (* SetClean)( AstFitsChan *, int, int * );
   void (* ClearClean)( AstFitsChan *, int * );

   int (* GetCDMatrix)( AstFitsChan *, int * );
   int (* TestCDMatrix)( AstFitsChan *, int * );
   void (* SetCDMatrix)( AstFitsChan *, int, int * );
   void (* ClearCDMatrix)( AstFitsChan *, int * );

   int (* GetPolyTan)( AstFitsChan *, int * );
   int (* TestPolyTan)( AstFitsChan *, int * );
   void (* SetPolyTan)( AstFitsChan *, int, int * );
   void (* ClearPolyTan)( AstFitsChan *, int * );

   int (* GetIwc)( AstFitsChan *, int * );
   int (* TestIwc)( AstFitsChan *, int * );
   void (* SetIwc)( AstFitsChan *, int, int * );
   void (* ClearIwc)( AstFitsChan *, int * );

   void (* SetTableSource)( AstFitsChan *,
                            void (*)( void ),
                            void (*)( void (*)( void ),
                                      AstFitsChan *, const char *, int,
                                      int, int * ),
                            int * );

   void (* TableSource)( AstFitsChan *,
                         void (*)( AstFitsChan *, const char *, int, int,
                                   int * ),
                         int * );

} AstFitsChanVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstFitsChanGlobals {
   AstFitsChanVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__FITSCHAN_GETATTRIB_BUFF_LEN + 1 ];
   char CnvType_Text[ AST__FITSCHAN_FITSCARDLEN + 1 ];
   char CnvType_Text0[ AST__FITSCHAN_FITSCARDLEN + 1 ];
   char CnvType_Text1[ AST__FITSCHAN_FITSCARDLEN + 1 ];
   int Items_Written;
   int Write_Nest;
   int Current_Indent;
   int Ignore_Used;
   int Mark_New;
   int CreateKeyword_Seq_Nchars;
   char FormatKey_Buff[ 10 ];
   char FitsGetCom_Sval[ AST__FITSCHAN_FITSCARDLEN + 1 ];
   const char *IsSpectral_Ret;
   char Match_Fmt[ 10 ];
   const char *Match_Template;
   int *Match_PA;
   int *Match_PB;
   int Match_NA;
   int Match_NB;
   int Match_Nentry;
   char WcsCelestial_Type[ 4 ];
} AstFitsChanGlobals;

#endif

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
                          const char *, int *, ...);
#else
AstFitsChan *astFitsChanId_( const char *(*)( void ), void (*)( const char * ),
                            const char *, ... )__attribute__((format(printf,3,4)));
AstFitsChan *astFitsChanForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ), int * ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char *, int * ),
                              const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFitsChan *astInitFitsChan_( void *, size_t, int, AstFitsChanVtab *,
                               const char *,
                               const char *(*)( void ),
                               char *(*)( const char *(*)( void ), int * ),
                               void (*)( const char * ),
                               void (*)( void (*)( const char * ), const char *, int * ), int * );

/* Vtab initialiser. */
void astInitFitsChanVtab_( AstFitsChanVtab *, const char *, int * );



/* Loader. */
AstFitsChan *astLoadFitsChan_( void *, size_t, AstFitsChanVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitFitsChanGlobals_( AstFitsChanGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   AstKeyMap *astGetTables_( AstFitsChan *, int * );
   int  astFindFits_( AstFitsChan *, const char *, char [81], int, int * );
   int  astGetFitsCF_( AstFitsChan *, const char *, double *, int * );
   int  astGetFitsCI_( AstFitsChan *, const char *, int *, int * );
   int  astGetFitsCN_( AstFitsChan *, const char *, char **, int * );
   int  astGetFitsF_( AstFitsChan *, const char *, double *, int * );
   int  astGetFitsI_( AstFitsChan *, const char *, int *, int * );
   int  astGetFitsL_( AstFitsChan *, const char *, int *, int * );
   int  astGetFitsS_( AstFitsChan *, const char *, char **, int * );
   int  astTestFits_( AstFitsChan *, const char *, int *, int * );
   void astDelFits_( AstFitsChan *, int * );
   void astReadFits_( AstFitsChan *, int * );
   void astWriteFits_( AstFitsChan *, int * );
   void astEmptyFits_( AstFitsChan *, int * );
   void astShowFits_( AstFitsChan *, int * );
   void astPurgeWCS_( AstFitsChan *, int * );
   void astPutCards_( AstFitsChan *, const char *, int * );
   void astPutFits_( AstFitsChan *, const char [81], int, int * );
   void astPutTable_( AstFitsChan *, struct AstFitsTable *, const char *, int * );
   void astPutTables_( AstFitsChan *, AstKeyMap *, int * );
   void astRemoveTables_( AstFitsChan *, const char *, int * );
   void astRetainFits_( AstFitsChan *, int * );
   void astSetFitsCF_( AstFitsChan *, const char *, double *, const char *, int, int * );
   void astSetFitsCI_( AstFitsChan *, const char *, int *, const char *, int, int * );
   void astSetFitsCM_( AstFitsChan *, const char *, int, int * );
   void astSetFitsCN_( AstFitsChan *, const char *, const char *, const char *, int, int * );
   void astSetFitsF_( AstFitsChan *, const char *, double, const char *, int, int * );
   void astSetFitsI_( AstFitsChan *, const char *, int, const char *, int, int * );
   void astSetFitsL_( AstFitsChan *, const char *, int, const char *, int, int * );
   void astSetFitsS_( AstFitsChan *, const char *, const char *, const char *, int, int * );
   void astSetFitsU_( AstFitsChan *, const char *, const char *, int, int * );

   void astTableSource_( AstFitsChan *,
                         void (*)( AstFitsChan *, const char *, int, int, int * ),
                         int * );



# if defined(astCLASS)  || defined(astFORTRAN77)         /* Protected or F77 interface */
   void astSetTableSource_( AstFitsChan *,
                            void (*)( void ),
                            void (*)( void (*)( void ),
                                      AstFitsChan *, const char *, int,
                                      int, int * ),
                            int * );

#endif

# if defined(astCLASS)           /* Protected */

   int  astFitsEof_( AstFitsChan *, int * );
   int  astFitsGetCom_( AstFitsChan *, const char *, char **, int * );
   void astSetFitsCom_( AstFitsChan *, const char *, const char *, int, int * );

   int  astKeyFields_( AstFitsChan *, const char *, int, int *, int *, int * );

   int astGetCard_( AstFitsChan *, int * );
   int astTestCard_( AstFitsChan *, int * );
   void astSetCard_( AstFitsChan *, int, int * );
   void astClearCard_( AstFitsChan *, int * );

   int astGetDefB1950_( AstFitsChan *, int * );
   int astTestDefB1950_( AstFitsChan *, int * );
   void astSetDefB1950_( AstFitsChan *, int, int * );
   void astClearDefB1950_( AstFitsChan *, int * );

   int astGetTabOK_( AstFitsChan *, int * );
   int astTestTabOK_( AstFitsChan *, int * );
   void astSetTabOK_( AstFitsChan *, int, int * );
   void astClearTabOK_( AstFitsChan *, int * );

   int astGetCDMatrix_( AstFitsChan *, int * );
   int astTestCDMatrix_( AstFitsChan *, int * );
   void astSetCDMatrix_( AstFitsChan *, int, int * );
   void astClearCDMatrix_( AstFitsChan *, int * );

   int astGetPolyTan_( AstFitsChan *, int * );
   int astTestPolyTan_( AstFitsChan *, int * );
   void astSetPolyTan_( AstFitsChan *, int, int * );
   void astClearPolyTan_( AstFitsChan *, int * );

   int astGetCarLin_( AstFitsChan *, int * );
   int astTestCarLin_( AstFitsChan *, int * );
   void astSetCarLin_( AstFitsChan *, int, int * );
   void astClearCarLin_( AstFitsChan *, int * );

   int astGetIwc_( AstFitsChan *, int * );
   int astTestIwc_( AstFitsChan *, int * );
   void astSetIwc_( AstFitsChan *, int, int * );
   void astClearIwc_( AstFitsChan *, int * );

   int astGetClean_( AstFitsChan *, int * );
   int astTestClean_( AstFitsChan *, int * );
   void astSetClean_( AstFitsChan *, int, int * );
   void astClearClean_( AstFitsChan *, int * );

   int astGetFitsDigits_( AstFitsChan *, int * );
   int astTestFitsDigits_( AstFitsChan *, int * );
   void astSetFitsDigits_( AstFitsChan *, int, int * );
   void astClearFitsDigits_( AstFitsChan *, int * );

   const char *astGetAllWarnings_( AstFitsChan *, int * );

   const char *astGetWarnings_( AstFitsChan *, int * );
   int astTestWarnings_( AstFitsChan *, int * );
   void astClearWarnings_( AstFitsChan *, int * );
   void astSetWarnings_( AstFitsChan *, const char *, int * );

   int astGetNcard_( AstFitsChan *, int * );

   int astGetCardType_( AstFitsChan *, int * );
   const char *astGetCardName_( AstFitsChan *, int * );
   const char *astGetCardComm_( AstFitsChan *, int * );

   int astGetNkey_( AstFitsChan *, int * );

   int astGetEncoding_( AstFitsChan *, int * );
   int astTestEncoding_( AstFitsChan *, int * );
   void astSetEncoding_( AstFitsChan *, int, int * );
   void astClearEncoding_( AstFitsChan *, int * );

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
#define astCheckFitsChan(this) astINVOKE_CHECK(FitsChan,this,0)
#define astVerifyFitsChan(this) astINVOKE_CHECK(FitsChan,this,1)

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
astINVOKE(O,astInitFitsChan_(mem,size,init,vtab,name,source,sourcewrap,sink,sinkwrap,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitFitsChanVtab(vtab,name) astINVOKE(V,astInitFitsChanVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadFitsChan(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFitsChan_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif


/* More include files. */
/* =================== */
/* The interface to the FitsTable class must be included here (after the
   type definitions for the FitsChan class) because "fitstable.h" itself
   includes this file ("fitschan.h"), although "fitschan.h" refers to the
   AstFitsTable structure above. This seems a little strange at first,
   but is simply analogous to making a forward reference to a
   structure type when recursively defining a normal C structure
   (except that here the definitions happen to be in separate include
   files). */
#include "fitstable.h"

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckFitsChan to validate FitsChan pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astPutFits(this,card,overwrite) \
astINVOKE(V,astPutFits_(astCheckFitsChan(this),card,overwrite,STATUS_PTR))

#define astPutCards(this,cards) \
astINVOKE(V,astPutCards_(astCheckFitsChan(this),cards,STATUS_PTR))

#define astDelFits(this) \
astINVOKE(V,astDelFits_(astCheckFitsChan(this),STATUS_PTR))

#define astPurgeWCS(this) \
astINVOKE(V,astPurgeWCS_(astCheckFitsChan(this),STATUS_PTR))

#define astGetTables(this) \
astINVOKE(O,astGetTables_(astCheckFitsChan(this),STATUS_PTR))

#define astPutTable(this,table,extnam) \
astINVOKE(V,astPutTable_(astCheckFitsChan(this),astCheckFitsTable(table),extnam,STATUS_PTR))

#define astPutTables(this,tables) \
astINVOKE(V,astPutTables_(astCheckFitsChan(this),astCheckKeyMap(tables),STATUS_PTR))

#define astRemoveTables(this,key) \
astINVOKE(V,astRemoveTables_(astCheckFitsChan(this),key,STATUS_PTR))

#define astRetainFits(this) \
astINVOKE(V,astRetainFits_(astCheckFitsChan(this),STATUS_PTR))

#define astFindFits( this, name, card, inc ) \
astINVOKE(V,astFindFits_(astCheckFitsChan(this),name,card,inc,STATUS_PTR))

#define astSetFitsI(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsI_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsF(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsF_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsS(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsS_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsCN(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCN_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsCI(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCI_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsCF(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsCF_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsL(this,name,value,comment,overwrite) \
astINVOKE(V,astSetFitsL_(astCheckFitsChan(this),name,value,comment,overwrite,STATUS_PTR))

#define astSetFitsU(this,name,comment,overwrite) \
astINVOKE(V,astSetFitsU_(astCheckFitsChan(this),name,comment,overwrite,STATUS_PTR))

#define astSetFitsCM(this,comment,overwrite) \
astINVOKE(V,astSetFitsCM_(astCheckFitsChan(this),comment,overwrite,STATUS_PTR))

#define astGetFitsCF(this,name,value) \
astINVOKE(V,astGetFitsCF_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astGetFitsCI(this,name,value) \
astINVOKE(V,astGetFitsCI_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astGetFitsF(this,name,value) \
astINVOKE(V,astGetFitsF_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astGetFitsI(this,name,value) \
astINVOKE(V,astGetFitsI_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astGetFitsL(this,name,value) \
astINVOKE(V,astGetFitsL_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astTestFits(this,name,there) \
astINVOKE(V,astTestFits_(astCheckFitsChan(this),name,there,STATUS_PTR))

#define astGetFitsS(this,name,value) \
astINVOKE(V,astGetFitsS_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astGetFitsCN(this,name,value) \
astINVOKE(V,astGetFitsCN_(astCheckFitsChan(this),name,value,STATUS_PTR))

#define astReadFits(this) \
astINVOKE(V,astReadFits_(astCheckFitsChan(this),STATUS_PTR))

#define astWriteFits(this) \
astINVOKE(V,astWriteFits_(astCheckFitsChan(this),STATUS_PTR))

#define astEmptyFits(this) \
astINVOKE(V,astEmptyFits_(astCheckFitsChan(this),STATUS_PTR))

#define astShowFits(this) \
astINVOKE(V,astShowFits_(astCheckFitsChan(this),STATUS_PTR))

#define astTableSource(this,tabsource) \
astINVOKE(V,astTableSource_(astCheckFitsChan(this),tabsource,STATUS_PTR))


#if defined(astCLASS) || defined(astFORTRAN77) /* Protected or F77 interface */

#define astSetTableSource(this,tabsource,tabsource_wrap) \
astINVOKE(V,astSetTableSource_(astCheckFitsChan(this),tabsource,tabsource_wrap,STATUS_PTR))

#endif


#if defined(astCLASS)            /* Protected */

#define astFitsEof(this) \
astINVOKE(V,astFitsEof_(astCheckFitsChan(this),STATUS_PTR))

#define astFitsGetCom(this,name,comment) \
astINVOKE(V,astFitsGetCom_(astCheckFitsChan(this),name,comment,STATUS_PTR))

#define astSetFitsCom(this,name,comment,overwrite) \
astINVOKE(V,astSetFitsCom_(astCheckFitsChan(this),name,comment,overwrite,STATUS_PTR))

#define astKeyFields(this,filter,maxfld,ubnd,lbnd) \
astINVOKE(V,astKeyFields_(astCheckFitsChan(this),filter,maxfld,ubnd,lbnd,STATUS_PTR))

#define astClearCard(this) \
astINVOKE(V,astClearCard_(astCheckFitsChan(this),STATUS_PTR))
#define astGetCard(this) \
astINVOKE(V,astGetCard_(astCheckFitsChan(this),STATUS_PTR))
#define astSetCard(this,card) \
astINVOKE(V,astSetCard_(astCheckFitsChan(this),card,STATUS_PTR))
#define astTestCard(this) \
astINVOKE(V,astTestCard_(astCheckFitsChan(this),STATUS_PTR))

#define astClearDefB1950(this) \
astINVOKE(V,astClearDefB1950_(astCheckFitsChan(this),STATUS_PTR))
#define astGetDefB1950(this) \
astINVOKE(V,astGetDefB1950_(astCheckFitsChan(this),STATUS_PTR))
#define astSetDefB1950(this,defb950) \
astINVOKE(V,astSetDefB1950_(astCheckFitsChan(this),defb950,STATUS_PTR))
#define astTestDefB1950(this) \
astINVOKE(V,astTestDefB1950_(astCheckFitsChan(this),STATUS_PTR))

#define astClearTabOK(this) \
astINVOKE(V,astClearTabOK_(astCheckFitsChan(this),STATUS_PTR))
#define astGetTabOK(this) \
astINVOKE(V,astGetTabOK_(astCheckFitsChan(this),STATUS_PTR))
#define astSetTabOK(this,tabok) \
astINVOKE(V,astSetTabOK_(astCheckFitsChan(this),tabok,STATUS_PTR))
#define astTestTabOK(this) \
astINVOKE(V,astTestTabOK_(astCheckFitsChan(this),STATUS_PTR))

#define astClearCDMatrix(this) \
astINVOKE(V,astClearCDMatrix_(astCheckFitsChan(this),STATUS_PTR))
#define astGetCDMatrix(this) \
astINVOKE(V,astGetCDMatrix_(astCheckFitsChan(this),STATUS_PTR))
#define astSetCDMatrix(this,cdmatrix) \
astINVOKE(V,astSetCDMatrix_(astCheckFitsChan(this),cdmatrix,STATUS_PTR))
#define astTestCDMatrix(this) \
astINVOKE(V,astTestCDMatrix_(astCheckFitsChan(this),STATUS_PTR))

#define astClearPolyTan(this) \
astINVOKE(V,astClearPolyTan_(astCheckFitsChan(this),STATUS_PTR))
#define astGetPolyTan(this) \
astINVOKE(V,astGetPolyTan_(astCheckFitsChan(this),STATUS_PTR))
#define astSetPolyTan(this,value) \
astINVOKE(V,astSetPolyTan_(astCheckFitsChan(this),value,STATUS_PTR))
#define astTestPolyTan(this) \
astINVOKE(V,astTestPolyTan_(astCheckFitsChan(this),STATUS_PTR))

#define astClearCarLin(this) \
astINVOKE(V,astClearCarLin_(astCheckFitsChan(this),STATUS_PTR))
#define astGetCarLin(this) \
astINVOKE(V,astGetCarLin_(astCheckFitsChan(this),STATUS_PTR))
#define astSetCarLin(this,carln) \
astINVOKE(V,astSetCarLin_(astCheckFitsChan(this),carln,STATUS_PTR))
#define astTestCarLin(this) \
astINVOKE(V,astTestCarLin_(astCheckFitsChan(this),STATUS_PTR))

#define astClearClean(this) \
astINVOKE(V,astClearClean_(astCheckFitsChan(this),STATUS_PTR))
#define astGetClean(this) \
astINVOKE(V,astGetClean_(astCheckFitsChan(this),STATUS_PTR))
#define astSetClean(this,value) \
astINVOKE(V,astSetClean_(astCheckFitsChan(this),value,STATUS_PTR))
#define astTestClean(this) \
astINVOKE(V,astTestClean_(astCheckFitsChan(this),STATUS_PTR))

#define astClearFitsDigits(this) \
astINVOKE(V,astClearFitsDigits_(astCheckFitsChan(this),STATUS_PTR))
#define astGetFitsDigits(this) \
astINVOKE(V,astGetFitsDigits_(astCheckFitsChan(this),STATUS_PTR))
#define astSetFitsDigits(this,fitsdigits) \
astINVOKE(V,astSetFitsDigits_(astCheckFitsChan(this),fitsdigits,STATUS_PTR))
#define astTestFitsDigits(this) \
astINVOKE(V,astTestFitsDigits_(astCheckFitsChan(this),STATUS_PTR))

#define astClearWarnings(this) \
astINVOKE(V,astClearWarnings_(astCheckFitsChan(this),STATUS_PTR))
#define astGetWarnings(this) \
astINVOKE(V,astGetWarnings_(astCheckFitsChan(this),STATUS_PTR))
#define astSetWarnings(this,warnings) \
astINVOKE(V,astSetWarnings_(astCheckFitsChan(this),warnings,STATUS_PTR))
#define astTestWarnings(this) \
astINVOKE(V,astTestWarnings_(astCheckFitsChan(this),STATUS_PTR))

#define astGetAllWarnings(this) \
astINVOKE(V,astGetAllWarnings_(astCheckFitsChan(this),STATUS_PTR))

#define astGetCardType(this) \
astINVOKE(V,astGetCardType_(astCheckFitsChan(this),STATUS_PTR))

#define astGetCardName(this) \
astINVOKE(V,astGetCardName_(astCheckFitsChan(this),STATUS_PTR))

#define astGetCardComm(this) \
astINVOKE(V,astGetCardComm_(astCheckFitsChan(this),STATUS_PTR))

#define astGetNcard(this) \
astINVOKE(V,astGetNcard_(astCheckFitsChan(this),STATUS_PTR))

#define astGetNkey(this) \
astINVOKE(V,astGetNkey_(astCheckFitsChan(this),STATUS_PTR))

#define astClearEncoding(this) \
astINVOKE(V,astClearEncoding_(astCheckFitsChan(this),STATUS_PTR))
#define astGetEncoding(this) \
astINVOKE(V,astGetEncoding_(astCheckFitsChan(this),STATUS_PTR))
#define astSetEncoding(this,encoding) \
astINVOKE(V,astSetEncoding_(astCheckFitsChan(this),encoding,STATUS_PTR))
#define astTestEncoding(this) \
astINVOKE(V,astTestEncoding_(astCheckFitsChan(this),STATUS_PTR))

#define astClearIwc(this) \
astINVOKE(V,astClearIwc_(astCheckFitsChan(this),STATUS_PTR))
#define astGetIwc(this) \
astINVOKE(V,astGetIwc_(astCheckFitsChan(this),STATUS_PTR))
#define astSetIwc(this,iwc) \
astINVOKE(V,astSetIwc_(astCheckFitsChan(this),iwc,STATUS_PTR))
#define astTestIwc(this) \
astINVOKE(V,astTestIwc_(astCheckFitsChan(this),STATUS_PTR))

#endif

#endif





