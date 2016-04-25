1 - Replace TemplateClass with capitalised class name
2 - Replace templateclass with lower case class name
3 - Replace TEMPLATECLASS with upper case class name
4 - Replace TemplateParent with capitalised parent class name
5 - Replace templateparent with lower case parent class name
6 - Replace all occurrences of >>> with suitable text

/*
*class++
*  Name:
*     TemplateClass

*  Purpose:
*     >>> purpose

*  Constructor Function:
c     astTemplateClass
f     AST_TEMPLATECLASS

*  Description:
*     A TemplateClass is a >>>

*  Inheritance:
*     The TemplateClass class inherits from the TemplateParent class.

*  Attributes:
*     In addition to those attributes common to all TemplateParents, every
*     TemplateClass also has the following attributes:
*
*     >>> Describe new attributes
*     - AlignStdOfRest: Standard of rest in which to align TemplateClasss

*  Functions:
c     In addition to those functions applicable to all TemplateParents, the
c     following functions may also be applied to all TemplateClasss:
f     In addition to those routines applicable to all TemplateParents, the
f     following routines may also be applied to all TemplateClasss:
*
*     >>> Describe new functions
c     - astSetRefPos: Set reference position in any celestial system
f     - AST_SETREFPOS: Set reference position in any celestial system

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     >>> 4-NOV-2002 (DSB):
*        Original version.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS TemplateClass

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "templateclass.h"           /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <limits.h>

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag as
   static variables. */
static AstTemplateClassVtab class_vtab; /* Virtual function table */
static int class_init = 0;          /* Virtual function table initialised? */

/* Pointers to parent class methods which are used or extended by this
   class. */
static int (* parent_getobjsize)( AstObject * );
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );
static int (* parent_equal)( AstObject *, AstObject * );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static int Equal( AstObject *, AstObject * );
static int GetObjSize( AstObject * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );

static const char *GetAttrib( AstObject *, const char * );
static int TestAttrib( AstObject *, const char * );
static void ClearAttrib( AstObject *, const char * );
static void SetAttrib( AstObject *, const char * );


/* Member functions. */
/* ================= */

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a TemplateClass.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     TemplateClass member function (over-rides the astClearAttrib protected
*     method inherited from the TemplateParent class).

*  Description:
*     This function clears the value of a specified attribute for a
*     TemplateClass, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the TemplateClass.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Variables: */
   AstTemplateClass *this;           /* Pointer to the TemplateClass structure */
   char *new_attrib;             /* Pointer value to new attribute name */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

>>> rEPLACE WITH NEW ATTRIBUTES

/* AlignStdOfRest. */
/* --------------- */
   if ( !strcmp( attrib, "alignstdofrest" ) ) {
      astClearAlignStdOfRest( this );

/* GeoLat. */
/* ------- */
/* Retained for backward compatibility with older versions of AST in which
   TemplateClass had GeoLon/Lat attributes (now ObsLon/Lat are used instead). */
   } else if ( !strcmp( attrib, "geolat" ) ) {
      astClearAttrib( this, "obslat" );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static int Equal( AstObject *this_object, AstObject *that_object ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two TemplateClasss are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     int Equal( AstObject *this, AstObject *that )

*  Class Membership:
*     TemplateClass member function (over-rides the astEqual protected
*     method inherited from the TemplateParent Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two TemplateClasss are equivalent.

*  Parameters:
*     this
*        Pointer to the first TemplateClass.
*     that
*        Pointer to the second TemplateClass.

*  Returned Value:
*     One if the TemplateClasss are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTemplateClass *that;         /* Pointer to the second TemplateClass structure */
   AstTemplateClass *this;         /* Pointer to the first TemplateClass structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the Equal method inherited from the parent TemplateParent class. This checks
   that the TemplateParents are both of the same class (amongst other things). */
   if( (*parent_equal)( this_object, that_object ) ) {

/* Obtain pointers to the two TemplateClass structures. */
      this = (AstTemplateClass *) this_object;
      that = (AstTemplateClass *) that_object;
>>>

   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int GetObjSize( AstObject *this_object ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     int GetObjSize( AstObject *this )

*  Class Membership:
*     TemplateClass member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied TemplateClass,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the TemplateClass.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTemplateClass *this;         /* Pointer to TemplateClass structure */
   int result;                /* Result value to return */
   int i;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object );

>>>

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a TemplateClass.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     TemplateClass member function (over-rides the protected astGetAttrib
*     method inherited from the TemplateParent class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a TemplateClass, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the TemplateClass.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - The returned string pointer may point at memory allocated
*     within the TemplateClass, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the TemplateClass. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstTemplateClass *this;           /* Pointer to the TemplateClass structure */
   char *new_attrib;             /* Pointer value to new attribute name */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Attribute value */
   int ival;                     /* Attribute value */
   int len;                      /* Length of attrib string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

>>>

/* AlignStdOfRest. */
/* --------------- */
/* Obtain the AlignStdOfRest code and convert to a string. */
   if ( !strcmp( attrib, "alignstdofrest" ) ) {
      sor = astGetAlignStdOfRest( this );
      if ( astOK ) {
         result = StdOfRestString( sor );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid AlignStdOfRest "
                     "identification code (%d).", astGetClass( this ),
                     astGetClass( this ), (int) sor );
         }
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib );
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

void astInitTemplateClassVtab_(  AstTemplateClassVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitTemplateClassVtab

*  Purpose:
*     Initialise a virtual function table for a TemplateClass.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "templateclass.h"
*     void astInitTemplateClassVtab( AstTemplateClassVtab *vtab, const char *name )

*  Class Membership:
*     TemplateClass vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the TemplateClass class.

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
   AstTemplateParentVtab *templateparent;          /* Pointer to TemplateParent component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitTemplateParentVtab( (AstTemplateParentVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsATemplateClass) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
>>>
   vtab->GetRefPos = GetRefPos;
   vtab->SetRefPos = SetRefPos;

   vtab->ClearAlignStdOfRest = ClearAlignStdOfRest;
   vtab->TestAlignStdOfRest = TestAlignStdOfRest;
   vtab->GetAlignStdOfRest = GetAlignStdOfRest;
   vtab->SetAlignStdOfRest = SetAlignStdOfRest;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   templateparent = (AstTemplateParentVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

>>>

/* Store replacement pointers for methods which will be over-ridden by new
   member functions implemented here. */
>>>
   templateparent->GetActiveUnit = GetActiveUnit;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "TemplateClass",
               ">>>Description of class" );

}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a TemplateClass.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     TemplateClass member function (extends the astSetAttrib method inherited from
*     the Mapping class).

*  Description:
*     This function assigns an attribute value for a TemplateClass, the attribute
*     and its value being specified by means of a string of the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in lower
*     case with no white space present. The value to the right of the "="
*     should be a suitable textual representation of the value to be assigned
*     and this will be interpreted according to the attribute's data type.
*     White space surrounding the value is only significant for string
*     attributes.

*  Parameters:
*     this
*        Pointer to the TemplateClass.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.

*  Returned Value:
*     void

*  Notes:
*     This protected method is intended to be invoked by the Object astSet
*     method and makes additional attributes accessible to it.
*/

/* Local Vaiables: */
   AstTemplateClass *this;           /* Pointer to the TemplateClass structure */
   char *a;                      /* Pointer to next character */
   char *new_setting;            /* Pointer value to new attribute setting */
   double dval;                  /* Double atribute value */
   double dtemp;                 /* Temporary double atribute value */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int ulen;                     /* Used length of setting string */
   int namelen;                  /* Length of attribute name in setting */
   int nc;                       /* Number of characters read by astSscanf */
   int off;                      /* Offset of attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Obtain the used length of the setting string. */
   ulen = astChrLen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

>>>

/* AlignStdOfRest. */
/* --------------- */
   if ( nc = 0,
        ( 0 == astSscanf( setting, "alignstdofrest=%n%*s %n", &off, &nc ) )
        && ( nc >= len ) ) {

/* Convert the string to a StdOfRest code before use. */
      sor = StdOfRestCode( setting + off );
      if ( sor != AST__BADSOR ) {
         astSetAlignStdOfRest( this, sor );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid standard of rest "
                   "description \"%s\".", astGetClass( this ), setting+off );
      }

/* GeoLat. */
/* ------- */
/* Retained for backward compatibility with older versions of AST in which
   TemplateClass had GeoLon/Lat attributes (now ObsLon/Lat are used instead). */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "geolat=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {
      new_setting = astStore( NULL, setting, len + 1 );
      new_setting[ 0 ] = 'o';
      new_setting[ 1 ] = 'b';
      new_setting[ 2 ] = 's';
      astSetAttrib( this, new_setting );
      new_setting = astFree( new_setting );

/* Pass any unrecognised setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a TemplateClass.

*  Type:
*     Private function.

*  Synopsis:
*     #include "templateclass.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     TemplateClass member function (over-rides the astTestAttrib protected
*     method inherited from the TemplateParent class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a TemplateClass's attributes.

*  Parameters:
*     this
*        Pointer to the TemplateClass.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstTemplateClass *this;           /* Pointer to the TemplateClass structure */
   char *new_attrib;             /* Pointer value to new attribute name */
   int len;                      /* Length of attrib string */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

>>>

/* AlignStdOfRest. */
/* --------------- */
   if ( !strcmp( attrib, "alignstdofrest" ) ) {
      result = astTestAlignStdOfRest( this );

/* GeoLat. */
/* ------- */
/* Retained for backward compatibility with older versions of AST in which
   TemplateClass had GeoLon/Lat attributes (now ObsLon/Lat are used instead). */
   } else if ( !strcmp( attrib, "geolat" ) ) {
      result = astTestAttrib( this, "obslat" );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}


/* Functions which access class attributes. */
/* ---------------------------------------- */
/*

>>> Add descriptions and accessors for all new attributes

*att++
*  Name:
*     AlignSpecOffset

*  Purpose:
*     Align TemplateClasss using the offset coordinate system?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which controls how a TemplateClass
*     behaves when it is used (by
c     astFindTemplateParent or astConvert) as a template to match another (target)
f     AST_FINDFRAME or AST_CONVERT) as a template to match another (target)
*     TemplateClass. It determines whether alignment occurs between the offset
*     values defined by the current value of the SpecOffset attribute, or
*     between the corresponding absolute spectral values.
*
*     The default value of zero results in the two TemplateClasss being aligned
*     so that a given absolute spectral value in one is mapped to the same
*     absolute value in the other. A non-zero value results in the TemplateClasss
*     being aligned so that a given offset value in one is mapped to the same
*     offset value in the other.

*  Applicability:
*     TemplateClass
*        All TemplateClasss have this attribute.
*att--
*/
astMAKE_CLEAR(TemplateClass,AlignSpecOffset,alignspecoffset,-INT_MAX)
astMAKE_GET(TemplateClass,AlignSpecOffset,int,0,( ( this->alignspecoffset != -INT_MAX ) ?
                                   this->alignspecoffset : 0 ))
astMAKE_SET(TemplateClass,AlignSpecOffset,int,alignspecoffset,( value != 0 ))
astMAKE_TEST(TemplateClass,AlignSpecOffset,( this->alignspecoffset != -INT_MAX ))


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for TemplateClass objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for TemplateClass objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstTemplateClass *in;             /* Pointer to input TemplateClass */
   AstTemplateClass *out;            /* Pointer to output TemplateClass */
   char *usedunit;               /* Pointer to an element of usedunits array */
   int i;                        /* Loop count */
   int nused;                    /* Size of "usedunits" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output TemplateClasss. */
   in = (AstTemplateClass *) objin;
   out = (AstTemplateClass *) objout;

/* Nullify the pointers stored in the output object since these will
   currently be pointing at the input data (since the output is a simple
   byte-for-byte copy of the input). Otherwise, the input data could be
   freed by accidient if the output object is deleted due to an error
   occuring in this function. */
   out->usedunits = NULL;

>>>

/* If an error has occurred, free the output resources. */
   if( !astOK ) Delete( (AstObject *) out );

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for TemplateClass objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for TemplateClass objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstTemplateClass *this;
   int i;

/* Release the memory referred to in the TemplateClass structure. */
   this = (AstTemplateClass *) obj;
   if( this ) {
>>>
   }
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for TemplateClass objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the TemplateClass class to an output Channel.

*  Parameters:
*     this
*        Pointer to the TemplateClass whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstTemplateClass *this;           /* Pointer to the TemplateClass structure */
   char buff[ 20 ];              /* Buffer for item name */
   char comm[ 50 ];              /* Buffer for comment */
   const char *sval;             /* Pointer to string value */
   double dval;                  /* Double value */
   int i;                        /* Loop count */
   int ival;                     /* int value */
   int j;                        /* Loop count */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the TemplateClass structure. */
   this = (AstTemplateClass *) this_object;

/* Write out values representing the instance variables for the
   TemplateClass class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

>>>

/* StdOfRest. */
/* ---------- */
   set = TestStdOfRest( this );
   sor = set ? GetStdOfRest( this ) : astGetStdOfRest( this );

/* If set, convert explicitly to a string for the external
   representation. */
   sval = "";
   if ( set ) {
      if ( astOK ) {
         sval = StdOfRestString( sor );

/* Report an error if the StdOfRest value was not recognised. */
         if ( !sval ) {
            astError( AST__SCSIN,
                     "%s(%s): Corrupt %s contains invalid standard of rest "
                     "identification code (%d).", "astWrite",
                     astGetClass( channel ), astGetClass( this ), (int) sor );
         }
      }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
   } else {
      sval = astGetAttrib( this_object, "stdofrest" );
   }

/* Write out the value. */
   astWriteString( channel, "SoR", set, 1, sval, "Standard of rest" );


}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsATemplateClass and astCheckTemplateClass functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(TemplateClass,TemplateParent,check,&class_init)
astMAKE_CHECK(TemplateClass)


>>> Change constructor argument list
AstTemplateClass *astTemplateClass_( >>> const char *options, ... ) {
/*
*+
*  Name:
*     astTemplateClass

*  Purpose:
*     Create a TemplateClass.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "templateclass.h"
*     AstTemplateClass *astTemplateClass( >>> const char *options, ... )

*  Class Membership:
*     TemplateClass constructor.

*  Description:
*     This function creates a new TemplateClass and optionally initialises its
*     attributes.

*  Parameters:
>>>
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new TemplateClass. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new TemplateClass.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic TemplateClass constructor which
*     is available via the protected interface to the TemplateClass class.
*     A public interface is provided by the astTemplateClassId_ function.
*/

/* Local Variables: */
   AstTemplateClass *new;            /* Pointer to new TemplateClass */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the TemplateClass, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitTemplateClass( NULL, sizeof( AstTemplateClass ), !class_init,
                           &class_vtab, "TemplateClass" );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new TemplateClass's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new TemplateClass. */
   return new;
}

AstTemplateClass *astInitTemplateClass_( void *mem, size_t size, int init,
                                 AstTemplateClassVtab *vtab, const char *name,
                                 >>>) {
/*
*+
*  Name:
*     astInitTemplateClass

*  Purpose:
*     Initialise a TemplateClass.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "templateclass.h"
*     AstTemplateClass *astInitTemplateClass( void *mem, size_t size, int init,
*                                     AstTemplateParentVtab *vtab, const char *name,
*                                     >>> )

*  Class Membership:
*     TemplateClass initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new TemplateClass object. It allocates memory (if
*     necessary) to accommodate the TemplateClass plus any additional data
*     associated with the derived class. It then initialises a
*     TemplateClass structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual function
*     table for a TemplateClass at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the TemplateClass is to be
*	created. This must be of sufficient size to accommodate the
*	TemplateClass data (sizeof(TemplateClass)) plus any data used by
*	the derived class. If a value of NULL is given, this function
*	will allocate the memory itself using the "size" parameter to
*	determine its size.
*     size
*        The amount of memory used by the TemplateClass (plus derived
*	class data). This will be used to allocate memory if a value of
*	NULL is given for the "mem" parameter. This value is also stored
*	in the TemplateClass structure, so a valid value must be supplied
*	even if not required for allocating memory.
*     init
*        A logical flag indicating if the TemplateClass's virtual function
*	table is to be initialised. If this value is non-zero, the
*	virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*	associated with the new TemplateClass.
*     name
*        Pointer to a constant null-terminated character string which
*	contains the name of the class to which the new object belongs
*	(it is this pointer value that will subsequently be returned by
*	the astGetClass method).
>>>

*  Returned Value:
*     A pointer to the new TemplateClass.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstTemplateClass *new;        /* Pointer to the new TemplateClass */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitTemplateClassVtab( vtab, name );

/* Initialise a 1D TemplateParent structure (the parent class) as the first component
   within the TemplateClass structure, allocating memory if necessary. */
   new = (AstTemplateClass *) astInitTemplateParent( mem, size, 0,
                                        (AstTemplateParentVtab *) vtab,
                                         name, >>> 1 );

   if ( astOK ) {

/* Initialise the TemplateClass data. */
/* ----------------------------- */
/* Initialise all attributes to their "undefined" values. */
>>>
      new->alignstdofrest = AST__BADSOR;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );

   }

/* Return a pointer to the new object. */
   return new;
}

AstTemplateClass *astLoadTemplateClass_( void *mem, size_t size,
                                 AstTemplateClassVtab *vtab,
                                 const char *name, AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadTemplateClass

*  Purpose:
*     Load a TemplateClass.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "templateclass.h"
*     AstTemplateClass *astLoadTemplateClass( void *mem, size_t size,
*                                      AstTemplateClassVtab *vtab,
*                                      const char *name, AstChannel *channel )

*  Class Membership:
*     TemplateClass loader.

*  Description:
*     This function is provided to load a new TemplateClass using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     TemplateClass structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the TemplateClass is to be
*        loaded.  This must be of sufficient size to accommodate the
*        TemplateClass data (sizeof(TemplateClass)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the TemplateClass (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the TemplateClass structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstTemplateClass) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new TemplateClass. If this is NULL, a pointer
*        to the (static) virtual function table for the TemplateClass class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "TemplateClass" is used instead.

*  Returned Value:
*     A pointer to the new TemplateClass.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstTemplateClass *new;            /* Pointer to the new TemplateClass */
   char buff[ 20 ];              /* Buffer for item name */
   char *sval;                   /* Pointer to string value */
   int i;                        /* Loop count */
   int j;                        /* Loop count */
   int nc;                       /* String length */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this TemplateClass. In this case the
   TemplateClass belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstTemplateClass );
      vtab = &class_vtab;
      name = "TemplateClass";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitTemplateClassVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built TemplateClass. */
   new = astLoadTemplateParent( mem, size, (AstTemplateParentVtab *) vtab, name,
                       channel );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
       astReadClassData( channel, "TemplateClass" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

>>>

/* StdOfRest. */
/* ---------- */
/* Set the default and read the external representation as a string. */
       new->stdofrest = AST__BADSOR;
       sval = astReadString( channel, "sor", NULL );

/* If a value was read, convert from a string to a StdOfRest code. */
       if ( sval ) {
          if ( astOK ) {
             new->stdofrest = StdOfRestCode( sval );

/* Report an error if the value wasn't recognised. */
             if ( new->stdofrest == AST__BADSOR ) {
                astError( AST__ATTIN,
                          "astRead(%s): Invalid standard of rest description "
                          "\"%s\".", astGetClass( channel ), sval );
             }
          }

/* Free the string value. */
          sval = astFree( sval );
       }

/* If an error occurred, clean up by deleting the new TemplateClass. */
       if ( !astOK ) new = astDelete( new );
   }

/* Return the new TemplateClass pointer. */
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
>>>
void astGetRefPos_( AstTemplateClass *this, AstSkyTemplateParent *frm, double *lon,
                    double *lat ){
   if ( !astOK ) return;
   (**astMEMBER(this,TemplateClass,GetRefPos))(this,frm,lon,lat);
}


/* Special public interface functions. */
/* =================================== */
/* These provide the public interface to certain special functions
   whose public interface cannot be handled using macros (such as
   astINVOKE) alone. In general, they are named after the
   corresponding protected version of the function, but with "Id"
   appended to the name. */

/* Public Interface Function Prototypes. */
/* ------------------------------------- */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstTemplateClass *astTemplateClassId_( >>> const char *, ... );

/* Special interface function implementations. */
/* ------------------------------------------- */
AstTemplateClass *astTemplateClassId_( >>> const char *options, ... ) {
/*
*++
*  Name:
c     astTemplateClass
f     AST_TEMPLATECLASS

*  Purpose:
*     Create a TemplateClass.

*  Type:
*     Public function.

*  Synopsis:
c     #include "templateclass.h"
c     AstTemplateClass *astTemplateClass( >>> const char *options, ... )
f     RESULT = AST_TEMPLATECLASS( >>> OPTIONS, STATUS )

*  Class Membership:
*     TemplateClass constructor.

*  Description:
*     This function creates a new TemplateClass and optionally initialises
*     its attributes.
*
*     A TemplateClass is a >>> (copy from class prologue at top of file)

*  Parameters:
>>>
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new TemplateClass. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new TemplateClass. The syntax used is identical to that for the
f        AST_SET routine. If no initialisation is required, a blank
f        value may be supplied.
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
c     astTemplateClass()
f     AST_TEMPLATECLASS = INTEGER
*        A pointer to the new TemplateClass.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astTemplateClass constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astTemplateClass_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astTemplateClass_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.
*/

/* Local Variables: */
   AstTemplateClass *new;            /* Pointer to new TemplateClass */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the TemplateClass, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitTemplateClass( NULL, sizeof( AstTemplateClass ), !class_init,
                           &class_vtab, "TemplateClass" >>> );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new TemplateClass's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new TemplateClass. */
   return astMakeId( new );
}




