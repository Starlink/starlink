1 - Replace TemplateClass with capitalised class name
2 - Replace templateclass with lower case class name
3 - Replace TEMPLATECLASS with upper case class name
4 - Replace TemplateParent with capitalised parent class name
5 - Replace templateparent with lower case parent class name
6 - Replace all occurrences of >>> with suitable text

#if !defined( TEMPLATECLASS_INCLUDED ) /* Include this file only once */
#define TEMPLATECLASS_INCLUDED
/*
*+
*  Name:
*     templateclass.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the TemplateClass class.

*  Invocation:
*     #include "templateclass.h"

*  Description:
*     This include file defines the interface to the TemplateClass class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     >>> 12-NOV-2002 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "templateparent.h"               /* Parent TemplateParent class */

/* Macros. */
/* ======= */

#if defined(astCLASS)            /* Protected */

#endif

/* Type Definitions. */
/* ================= */

/* TemplateClass structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstTemplateClass {

/* Attributes inherited from the parent class. */
   AstTemplateParent templateparent;               /* Parent class structure */

/* Attributes specific to objects in this class. */
>>>

} AstTemplateClass;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstTemplateClassVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstTemplateParentVtab templateparent_vtab;      /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
>>>
   double (* GetNewAttr)( AstTemplateClass * );
   int (* TestNewAttr)( AstTemplateClass * );
   void (* ClearNewAttr)( AstTemplateClass * );
   void (* SetNewAttr)( AstTemplateClass *, double );


} AstTemplateClassVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TemplateClass)         /* Check class membership */
astPROTO_ISA(TemplateClass)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstTemplateClass *astTemplateClass_( >>> const char *, ... );
#else
AstTemplateClass *astTemplateClassId_( >>> const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTemplateClass *astInitTemplateClass_( void *, size_t, int,
                                         AstTemplateClassVtab *,
                                         const char * >>> );

/* Vtab initialiser. */
void astInitTemplateClassVtab_( AstTemplateClassVtab *, const char * );

/* Loader. */
AstTemplateClass *astLoadTemplateClass_( void *, size_t,
                                         AstTemplateClassVtab *,
                                         const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */


>>>
double astGetNewAttr_( AstTemplateClass * );
int astTestNewAttr_( AstTemplateClass * );
void astClearNewAttr_( AstTemplateClass * );
void astSetNewAttr_( AstTemplateClass *, double );


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
#define astCheckTemplateClass(this) astINVOKE_CHECK(TemplateClass,this)

/* Test class membership. */
#define astIsATemplateClass(this) astINVOKE_ISA(TemplateClass,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astTemplateClass astINVOKE(F,astTemplateClass_)
#else
#define astTemplateClass astINVOKE(F,astTemplateClassId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitTemplateClass(mem,size,init,vtab,name>>>) \
astINVOKE(O,astInitTemplateClass_(mem,size,init,vtab,name>>>))

/* Vtab Initialiser. */
#define astInitTemplateClassVtab(vtab,name) astINVOKE(V,astInitTemplateClassVtab_(vtab,name))
/* Loader. */
#define astLoadTemplateClass(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTemplateClass_(mem,size,vtab,name,astCheckChannel(channel)))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
>>>

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckTemplateClass to validate TemplateClass pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetNewAttr(this) astINVOKE(V,astGetNewAttr_(astCheckTemplateClass(this)))
#define astTestNewAttr(this) astINVOKE(V,astTestNewAttr_(astCheckTemplateClass(this)))
#define astClearNewAttr(this) astINVOKE(V,astClearNewAttr_(astCheckTemplateClass(this)))
#define astSetNewAttr(this,value) astINVOKE(V,astSetNewAttr_(astCheckTemplateClass(this),value))

#endif
#endif
