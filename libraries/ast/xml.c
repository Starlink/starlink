/*
*  Name:
*     xml.c

*  Purpose:
*     Implement XML functions for AST.

*  Description:
*     This file implements the Xml module which provides generic XML
*     reading and writing functions for the XmlChan class.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     22-OCT-2003 (DSB):
*        Original version.
*     12-JAN-2004 (DSB):
*        Major revisions.
*     10-FEB-2004 (DSB):
*        - Added debug conditional code to keep track of memory leaks.
*        - Other minor bug fixes.
*     6-FEB-2004 (DSB):
*        DefaultURI and astXmlAddURI modified to allow a blank URI to be
*        used to ignore a default namespace URI provided by an enclosing
*        element.
*     29-NOV-2004 (DSB):
*        Added astXmlGetType method.
*     27-JAN-2005 (DSB):
*        - Move astXmlTrace and associated code into conditional
*        compilation blokc (included if DEBUG macro is defined). This
*        speeds up the create and destruction of XmlObjects in non-DEBUG code.
*        - Renamed the private Delete function as astXmlDelete and gave
*        it protected access.
*        - Modify astXmlDelete so that it can succesfully annul objects
*        which have no parent.
*        - Include extra info in some error messages.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     10-DEC-2008 (DSB):
*        Allow a prefix to be included with the attribute name in
*        astXmlGetAttributeValue.
*/


/* Module Constants. */
/* ----------------- */
/* Set the name of the module we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. NB, this module is not a proper AST
   class, but it defines this macro sanyway in order to get the protected
   symbols defined in memory.h */
#define astCLASS Xml

#define IND_INC 3


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "memory.h"            /* Interface to the memory management module */
#include "error.h"             /* Interface to the error module */
#include "xml.h"               /* Interface to this module */
#include "globals.h"           /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"           /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


/*
*  Name:
*     MAKE_CHECK

*  Type:
*     Private macro.

*  Purpose:
*     Implement the astXmlCheck<type>_ function for XML structures.

*  Synopsis:
*     #include "xml.h"
*     MAKE_CHECK(type,id)

*  Class Membership:
*     Defined by the xml module.

*  Description:
*     This macro expands to an implementation of the protected
*     astXmlCheck<type>_ function (q.v.) which validates membership of
*     a specified XML data type.

*  Parameters:
*     type
*        The type whose membership is to be validated (e.g. "Element" not
*        "XmlElement").
*     id
*        The constant (e.g. "AST__XMLELEM") defining the data type.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_CHECK(type,id) \
\
/* Declare the function */ \
AstXml##type *astXmlCheck##type##_( void *this, int nullok, int *status ) { \
\
/* Local Variables: */\
   AstXml##type *result;    /* The returned pointer */\
\
/* Check the global error status. If an error has already occurred just\
   return the supplied pointer. This is so that functions such as\
   astXmlAnnul which do not check the inherited status receive the\
   supplied pointer. */\
   if( !astOK ) return this;\
\
/* Initialise */\
   result = NULL;\
\
/* If the pointer is NULL issue an error if nullok is zero. */\
   if( !this ) {\
      if( !nullok ) astError( AST__PTRIN, "astXmlCheck"#type": Invalid "\
                              "NULL pointer supplied." , status);\
\
/* Otherwise get the "type" component which holds a magic value for each\
   different class of structure. Compare this value against all valid \
   classes of structure. If no match is found, the pointer does not \
   identify an suitable structure, and so report an error and return \
   NULL. */\
   } else {\
      if( !astXmlCheckType( ( AstXmlObject * ) this, id ) ) {\
         astError( AST__PTRIN, "astXmlCheck"#type": Invalid pointer "\
                   "supplied; pointer to AstXml"#type" required." , status);\
      } else {\
         result = (AstXml##type *) this;\
      }\
   }\
\
/* Return the result. */\
   return result;\
}


/* Module variables. */
/* ================= */

/* Define macros for accessing all items of thread-safe global data
   used by this module. */
#ifdef THREAD_SAFE

#define next_id astGLOBAL(Xml,Next_ID)
#define gettag_buff astGLOBAL(Xml,GetTag_Buff)
#define GLOBAL_inits globals->Next_ID = 0;
astMAKE_INITGLOBALS(Xml)

/* Set up mutexes */
static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX1 pthread_mutex_lock( &mutex1 );
#define UNLOCK_MUTEX1 pthread_mutex_unlock( &mutex1 );

/* If thread safety is not needed, declare globals at static variables. */
#else

static int next_id = 0;
static char gettag_buff[ AST__XML_GETTAG_BUFF_LEN + 1 ];

#define LOCK_MUTEX1
#define UNLOCK_MUTEX1

#ifdef DEBUG   /* Not available in thread-safe compilations */
static int nobj = 0;
static AstXmlObject **existing_objects = NULL;
#endif

#endif


/* Function prototypes. */
/* ==================== */

/* Private member functions. */
/* ------------------------- */
static AstXmlAttribute *FindAttribute( AstXmlElement *, const char *, int * );
static AstXmlAttribute *NewAttribute( const char *, const char *, const char *, int * );
static AstXmlDocument *NewDocument( int * );
static AstXmlPrologue *NewPrologue( AstXmlDocument *, int * );
static AstXmlNamespace *NewNamespace( const char *, const char *, int * );
static char *AppendChar( char *, int *, char, int * );
static char *AppendLine( char *, int *, const char *, int, int * );
static char *RemoveEscapes( const char *, int * );
static char *CleanText( const char *, int * );
static const char *AddEscapes( const char *, int * );
static const char *DefaultURI( AstXmlElement *, int * );
static const char *Format( AstXmlObject *, int, int * );
static char *FormatTag( AstXmlObject *, int, int * );
static const char *ResolvePrefix( const char *, AstXmlElement *, int * );
static int CheckType( long int, long int, int * );
static int MatchName( AstXmlElement *, const char *, int * );
static int Ustrcmp( const char *, const char *, int * );
static void AddContent( AstXmlParent *, int, AstXmlContentItem *, int * );
static void CheckName( const char *, const char *, const char *, int, int * );
static void CheckPrefName( char *, const char *, const char *, int * );
static void CleanXml( AstXmlObject *, long int, int * );
static void InitXmlAttribute( AstXmlAttribute *, int, const char *, const char *, const char *, int * );
static void InitXmlCDataSection( AstXmlCDataSection *, int, const char *, int * );
static void InitXmlWhite( AstXmlWhite *, int, const char *, int * );
static void InitXmlBlack( AstXmlBlack *, int, const char *, int * );
static void InitXmlComment( AstXmlComment *, int, const char *, int * );
static void InitXmlDocument( AstXmlDocument *, int, int * );
static void InitXmlPrologue( AstXmlPrologue *, int, int * );
static void InitXmlDeclPI( AstXmlDeclPI *, int, const char *, int * );
static void InitXmlDTDec( AstXmlDTDec *, int, const char *, const char *, const char *, int * );
static void InitXmlElement( AstXmlElement *, int, const char *, const char *, int * );
static void InitXmlNamespace( AstXmlNamespace *, int, const char *, const char *, int * );
static void InitXmlObject( AstXmlObject *, long int, int * );
static void InitXmlPI( AstXmlPI *, int, const char *, const char *, int * );
static AstXmlElement *ReadContent( AstXmlDocument **, int, int (*)( AstXmlElement *, int * ), int, char (*)( void *, int * ), void *, int, int * );

#ifdef DEBUG
static void AddObjectToList( AstXmlObject * );
static void RemoveObjectFromList( AstXmlObject * );
#endif

/* Function implementations. */
/* ========================= */

/* Create the astXmlCheck... functiosn which check a pointer identifies
   an XML structure of a given type. */

MAKE_CHECK(Document,AST__XMLDOC)
MAKE_CHECK(Object,AST__XMLOBJECT)
MAKE_CHECK(Element,AST__XMLELEM)
MAKE_CHECK(Attribute,AST__XMLATTR)
MAKE_CHECK(CDataSection,AST__XMLCDATA)
MAKE_CHECK(Comment,AST__XMLCOM)
MAKE_CHECK(PI,AST__XMLPI)
MAKE_CHECK(Namespace,AST__XMLNAME)
MAKE_CHECK(Prologue,AST__XMLPRO)
MAKE_CHECK(DeclPI,AST__XMLDEC)
MAKE_CHECK(DTDec,AST__XMLDTD)
MAKE_CHECK(White,AST__XMLWHITE)
MAKE_CHECK(Black,AST__XMLBLACK)
MAKE_CHECK(CharData,AST__XMLCHAR)
MAKE_CHECK(ContentItem,AST__XMLCONT)
MAKE_CHECK(MiscItem,AST__XMLMISC)
MAKE_CHECK(Parent,AST__XMLPAR)


static void AddContent( AstXmlParent *this, int where, AstXmlContentItem *item, int *status ){
/*
*  Name:
*     AddContent

*  Purpose:
*     Add a content item to an XmlElement.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void AddContent( AstXmlParent *this, int where, AstXmlContentItem *item, int *status )

*  Description:
*     This function adds a supplied item to a specified XmlElement or
*     XmlDocument. An error is reported if the item is not appropriate.

*  Parameters:
*     this
*        The pointer to the element or document to be modified.
*     where
*        Ignored if "this" is an XmlElement pointer. Otherwise, "where"
*        indicates where the item should be added to the document:
*          1 - In the prologue, after the XML declaration but before the DTD.
*          2 - In the prologue, after the DTD but before the root element.
*          3 - In the epilogue, after the root element.
*     item
*        Pointer to the content item to be added to the element. If
*        "this" is an XmlElement, this can be a pointer to any of the
*        following types: AstXmlElement, AstXmlWhite, AstXmlBlack,
*        AstXmlCDataSection, AstXmlComment, AstXmlPI. If "this" is a
*        document, the list is restricted to: AstXmlWhite, AstXmlComment,
*        AstXmlPI.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstXmlDocument *doc;   /* Document pointer */
   AstXmlElement *elem;   /* Element pointer */
   AstXmlPrologue *pro;   /* Prologue pointer */
   int nitem;             /* Number of items in the parent */

/* Check the global error status and the supplied pointers. */
   if( !astOK || !this || !item ) return;

/* Split for the two forms of parent. */
   if( astXmlCheckType( this, AST__XMLELEM ) ) {
      elem = (AstXmlElement *) this;

/* Save the number of content items currently stored in the element. */
      nitem = ( elem->items ) ? elem->nitem : 0;

/* Attempt to extend the array to hold an extra item. */
      elem->items = astGrow( elem->items, nitem + 1,
                             sizeof( AstXmlContentItem * ) );

/* Check the memory was allocated succesfully. */
      if( astOK ) {

/* Store the supplied pointer in the array of content items. */
         elem->items[ nitem ] = item;

/* Increment the number of content items in this element */
         elem->nitem = nitem + 1;

/* Indicate that the item is owned by the element. */
         ( (AstXmlObject *) item )->parent = this;
      }

/* Now deal with cases where we are adding an item to the prologue or
   epilogue of the document. */
   } else {
      if( !astXmlCheckType( item, AST__XMLMISC ) ){
         astError( AST__INTER, "AddContent(xml): Inappropriate attempt to "
                   "add an item of type %ld to an XML document (internal "
                   "AST programming error).", status, ( (AstXmlObject *) item)->type );

      } else if( !astXmlCheckType( this, AST__XMLDOC ) ){
         astError( AST__INTER, "AddContent(xml): Inappropriate attempt to "
                   "add an item of type %ld to an XML object of type %ld "
                   "(internal AST programming error).", status,
                   ( (AstXmlObject *) item)->type,
                   ( (AstXmlObject *) this)->type );

      } else {
         doc = (AstXmlDocument *) this;

/* Create a prologue if necessary. */
         if( where < 3 && !doc->prolog ) doc->prolog = NewPrologue( doc, status );
         pro = doc->prolog;

         if( where < 2 ) {
            nitem = ( pro->misc1 ) ? pro->nmisc1 : 0;
            pro->misc1 = astGrow( pro->misc1, nitem + 1, sizeof( AstXmlMiscItem * ) );
            if( astOK ) {
               pro->misc1[ nitem ] = item;
               pro->nmisc1 = nitem + 1;
               ( (AstXmlObject *) item )->parent = (AstXmlParent *) pro;
            }

         } else if( where == 2 ) {
            nitem = ( pro->misc2 ) ? pro->nmisc2 : 0;
            pro->misc2 = astGrow( pro->misc2, nitem + 1, sizeof( AstXmlMiscItem * ) );
            if( astOK ) {
               pro->misc2[ nitem ] = item;
               pro->nmisc2 = nitem + 1;
               ( (AstXmlObject *) item )->parent = (AstXmlParent *) pro;
            }

         } else {
            nitem = ( doc->epilog ) ? doc->nepi : 0;
            doc->epilog = astGrow( doc->epilog, nitem + 1, sizeof( AstXmlMiscItem * ) );
            if( astOK ) {
               doc->epilog[ nitem ] = item;
               doc->nepi = nitem + 1;
               ( (AstXmlObject *) item )->parent = this;
            }
         }
      }
   }
}

static const char *AddEscapes( const char *text, int *status ){
/*
*  Name:
*     AddEscapes

*  Purpose:
*     Replaces characters by corresponding entity references.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *AddEscapes( const char *text, int *status )

*  Description:
*     This function produces a dynamic copy of the supplied text in which
*     occurrences of "&", "<", ">", and "\"" are replaced by the corresponding
*     XML entity reference.
*
*     The "&" character is only replaced by an entity reference if it is
*     followed by a non-name character (i.e. anything except a letter
*     underscore or colon). If it is followed by a name character, it is
*     assumed to mark the start of an entity reference.

*  Parameters:
*     text
*        A pointer to a text string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated string containing the required
*     copy.

*  Notes:
*     - NULL is returned if this function is called with the global error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   char *result;             /* Returned pointer */
   const char *c;            /* Pointer to next supplied character */
   char *d;                  /* Pointer to next returned character */

/* Initialise */
   result = NULL;

/* Return if the pointer is NULL or if an error has occurred. */
   if( !astOK || !text ) return result;

/* Allocate the maximum possible amount of memory that may be needed to
   store the returned string. */
   result = astMalloc( 6*strlen( text ) + 1 );

/* Check the pointer can be used safely. */
   if( astOK ) {

/* Loop round every character in the supplied text. */
      c = text - 1;
      d = result;
      while( *(++c) ) {

/* We replace this character if it is a <, >, ', &, or ". */
         if( *c == '<' ) {
            strcpy( d, "&lt;" );
            d += 4;

         } else if( *c == '>' ) {
            strcpy( d, "&gt;" );
            d += 4;

         } else if( *c == '"' ) {
            strcpy( d, "&quot;" );
            d += 6;

         } else if( *c == '\'' ) {
            strcpy( d, "&apos;" );
            d += 6;

         } else if( *c == '&' ) {
            strcpy( d, "&amp;" );
            d += 5;

/* Otherwise just append the supplied character. */
         } else {
            *(d++) = *c;
         }
      }

/* Terminate the returned string. */
      *d = 0;

/* Reallocate the string to free up any unused space. */
      result = astRealloc( result, d - result + 1 );
   }

/* Return the result. */
   return (const char *) result;
}


#ifdef DEBUG
static void AddObjectToList( AstXmlObject *obj ){
/*
*  Name:
*     AddObjectToList

*  Purpose:
*     Adds an XmlObject to a static list of all currently active XmlObjects.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void AddObjectToList( AstXmlObject *obj )

*  Description:
*     This function adds the supplied pointer to a static list of pointers,
*     and increments the number of elements in the list. This list holds
*     pointers to all the XmlObjects which currently exist.

*  Parameters:
*     this
*        A pointer to a new XmlObject.
*/

/* Return if the pointer is NULL or if an error has occurred. */
   if( !astOK || !obj ) return;

/* Increment the number of objects in the list and increase the size of
   the list. */
   astBeginPM;
   existing_objects = astGrow( existing_objects, ++nobj, sizeof( AstXmlObject *) );
   astEndPM;

/* Add the new pointer to the end of the list. */
   existing_objects[ nobj - 1 ] = obj;
}
#endif

static char *AppendChar( char *str1, int *nc, char ch, int *status ) {
/*
*  Name:
*     AppendChar

*  Purpose:
*     Append a character to a string which grows dynamically.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *AppendChar( char *str1, int *nc, char ch, int *status )

*  Description:
*     This function appends a character to a dynamically
*     allocated string, extending the dynamic string as necessary to
*     accommodate the new character (plus the final null).

*  Parameters:
*     str1
*        Pointer to the null-terminated dynamic string, whose memory
*        has been allocated using the AST memory allocation functions
*        defined in "memory.h". If no space has yet been allocated for
*        this string, a NULL pointer may be given and fresh space will
*        be allocated by this function.
*     nc
*        Pointer to an integer containing the number of characters in
*        the dynamic string (excluding the final null). This is used
*        to save repeated searching of this string to determine its
*        length and it defines the point where the new string will be
*        appended. Its value is updated by this function to include
*        the extra characters appended.
*
*        If "str1" is NULL, the initial value supplied for "*nc" will
*        be ignored and zero will be used.
*     ch
*        The character which is to be appended to "str1".
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A possibly new pointer to the dynamic string with the new character
*     appended (its location in memory may have to change if it has to
*     be extended, in which case the original memory is automatically
*     freed by this function). When the string is no longer required,
*     its memory should be freed using astFree.

*  Notes:
*     - If this function is invoked with the global error status set
*     or if it should fail for any reason, then the returned pointer
*     will be equal to "str1" and the dynamic string contents will be
*     unchanged.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   int len;                      /* Length of new string */

/* Initialise. */
   result = str1;

/* If the first string pointer is NULL, also initialise the character
   count to zero. */
   if ( !str1 ) *nc = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Calculate the total string length once the character has been added. */
   len = *nc + 1;

/* Extend the dynamic string to the required length, including
   a final null. Save the resulting pointer, which will be
   returned. */
   result = astGrow( str1, len + 1, sizeof( char ) );

/* If OK, append the second string and update the total character
   count. */
   if ( astOK ) {
      result[ *nc ] = ch;
      *nc = len;
      result[ *nc ] = 0;
   }

/* Return the result pointer. */
   return result;
}

static char *AppendLine( char *str1, int *nc, const char *str2, int ind, int *status ) {
/*
*  Name:
*     AppendLine

*  Purpose:
*     Append an indented new line to another string which grows dynamically.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *AppendLine( char *str1, int *nc, const char *str2, int ind, int *status )

*  Description:
*     This function appends one string to another dynamically
*     allocated string, extending the dynamic string as necessary to
*     accommodate the new characters (plus the final null).
*
*     A newline character is inserted if necessary to ensure that the "str2"
*     string starts on a newline. If "ind" is positive, spaces are added
*     as necessary to ensure that "str2" begins with the specified number of
*     spaces.

*  Parameters:
*     str1
*        Pointer to the null-terminated dynamic string, whose memory
*        has been allocated using the AST memory allocation functions
*        defined in "memory.h". If no space has yet been allocated for
*        this string, a NULL pointer may be given and fresh space will
*        be allocated by this function.
*     nc
*        Pointer to an integer containing the number of characters in
*        the dynamic string (excluding the final null). This is used
*        to save repeated searching of this string to determine its
*        length and it defines the point where the new string will be
*        appended. Its value is updated by this function to include
*        the extra characters appended.
*
*        If "str1" is NULL, the initial value supplied for "*nc" will
*        be ignored and zero will be used.
*     str2
*        Pointer to a constant null-terminated string, a copy of which
*        is to be appended to "str1".
*     ind
*        The number of spaces to use as the indentation string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A possibly new pointer to the dynamic string with the new string
*     appended (its location in memory may have to change if it has to
*     be extended, in which case the original memory is automatically
*     freed by this function). When the string is no longer required,
*     its memory should be freed using astFree.

*  Notes:
*     - If this function is invoked with the global error status set
*     or if it should fail for any reason, then the returned pointer
*     will be equal to "str1" and the dynamic string contents will be
*     unchanged.
*/

/* Local Variables: */
   char *c;                      /* Point to next character */
   char *result;                 /* Pointer value to return */
   char *temp;                   /* Pointer to modified string */
   int j;                        /* Loop count */

/* Initialise. */
   result = str1;

/* If the first string pointer is NULL, also initialise the character
   count to zero. */
   if ( !str1 ) *nc = 0;

/* Check the global error status. */
   if ( !astOK || !str2 ) return result;

/* Remove any trailing white space (except for newlines) from the supplied
   string. */
   if( *nc > 0 ) {
      c = str1 + *nc - 1;
      while( isspace( *c ) && *c != '\n' ) {
         *(c--) = 0;
         (*nc)--;
      }

/* If the last character in the returned string is not now a newline,
   append a newline, so long as the new item does not start with a newline. */
      if( str1[ *nc - 1 ] != '\n' ) {
         temp = AppendChar( str1, nc, '\n', status );
      } else {
         temp = str1;
      }

   } else {
      temp = str1;
   }

/* If a fixed indentation is specified, skip over any leading spaces in
   the second string. */
   if( str2 ) {
      if( ind > 0 ) {
         while( isspace( *str2 ) ) str2++;
      }

/* If the first character of the second string is a newline, ignore it. */
      if( str2[ 0 ] == '\n' ) str2++;
   }

/* Append the indentation string. */
   for( j = 0; j < ind; j++ ) temp = AppendChar( temp, nc, ' ', status );

/* Append the supplied string. */
   return astAppendString( temp, nc, str2 );
}

void astXmlAddAttr_( AstXmlElement *this, const char *name, const char *value,
                     const char *prefix, int *status ){
/*
*+
*  Name:
*     astXmlAddAttr

*  Purpose:
*     Add an attribute to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddAttr( AstXmlElement *this, const char *name,
*                         const char *value, const char *prefix )

*  Description:
*     This function adds an attribute to a specified XmlElement. If the
*     element already contains an attribute with the given name amd prefix,
*     then the value of the attribute is changed to be the supplied value.

*  Parameters:
*     this
*        The pointer to the element to be modified.
*     name
*        Pointer to a null terminated string containing the attribute name.
*     value
*        Pointer to a null terminated string containing the attribute value.
*     prefix
*        The namespace prefix for the attribute. May be NULL or blank, in
*        which case any prefix at the start of "name" is used.
*-
*/

/* Local Variables: */
   AstXmlAttribute *attr;    /* The new attribute. */
   AstXmlAttribute *oldattr; /* Pointer to existing attribute */
   int i;                    /* Loop index */
   int nattr;                /* Number of attributes in the element */
   int oldi;                 /* Index of existing attribute */
   char *my_value;           /* Cleaned value text */

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise */
   oldattr = NULL;

/* Clean the value text. */
   my_value = CleanText( value, status );

/* Create a new XmlAttribute. */
   attr = NewAttribute( name, my_value, prefix, status );

/* Free the memory */
   my_value = astFree( my_value );

/* If OK, indicate that the attribute is owned by the element. */
   if( astOK ) {
      ( (AstXmlObject *) attr )->parent = (AstXmlParent *) this;

/* Save the number of attributes currently stored in the element. */
      nattr = ( this->attrs ) ? this->nattr : 0;

/* Search the existing attributes to see if an attribute with the given
   name and prefix already exists. */
      oldi = -1;
      for( i = 0; i < nattr; i++ ) {
         oldattr = this->attrs[ i ];
         if( !strcmp( oldattr->name, attr->name ) ) {
            if( !oldattr->prefix && !attr->prefix ) {
               oldi = i;
               break;
            } else if( oldattr->prefix && attr->prefix &&
                       !strcmp( oldattr->prefix, attr->prefix ) ){
               oldi = i;
               break;
            }
         }
      }

/* If there is an existing attribute with the same name and prefix,
   replace the old attribute with the new one created above. */
      if( oldi > -1 ){
         ((AstXmlObject *)oldattr)->parent = NULL;
         oldattr = astXmlAnnul( oldattr );
         this->attrs[ oldi ] = attr;

/* Otherwise, attempt to extend the array to hold an extra attribute. */
      } else {
         this->attrs = astGrow( this->attrs, nattr + 1,
                                sizeof( AstXmlAttribute * ) );

/* Check all has gone OK. */
         if( astOK ) {

/* Store the attribute pointer in the array of attribute pointers. */
            this->attrs[ nattr ] = attr;

/* Increment the number of content items in this element */
            this->nattr = nattr + 1;

         }
      }
   }
}

void astXmlAddCDataSection_( AstXmlElement *this, const char *text, int *status ){
/*
*+
*  Name:
*     astXmlAddCDataSection

*  Purpose:
*     Create a new XmlCDataSection and add it to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddCDataSection( AstXmlElement *this, const char *text )

*  Description:
*     This function creates a new XmlCDataSection structure representing
*     an unparsed character data (CDATA) section, and adds it into an
*     existing element.

*  Parameters:
*     this
*        A pointer to the element to be modified.
*     text
*        Pointer to a null terminated string containing the character data.

*-
*/

/* Local Variables: */
   AstXmlCDataSection *new;  /* Pointer to new structure */
   char *my_text;            /* Cleaned text */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlCDataSection *) astMalloc( sizeof( AstXmlCDataSection ) );

/* Clean the text. */
   my_text = CleanText( text, status );

/* Initialise it. */
   InitXmlCDataSection( new, AST__XMLCDATA, my_text, status );

/* Free the memory */
   my_text = astFree( my_text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( (AstXmlParent *) this, 0, (AstXmlContentItem *) new, status );
   }
}

void astXmlAddCharData_( AstXmlParent *this, int where, const char *text, int *status ){
/*
*+
*  Name:
*     astXmlAddCharData

*  Purpose:
*     Create a new XmlCharData and add it to an XmlElement or XmlDocument.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddCharData( AstXmlParent *this, int where, const char *text )

*  Description:
*     This function creates a new XmlCharData structure representing
*     parsed character data, and adds it into an existing element or
*     document.

*  Parameters:
*     this
*        Pointer to the element or document to be modified.
*     where
*        Ignored if "this" is an XmlElement pointer. Otherwise, "where"
*        indicates where the item should be added to the document:
*          1 - In the prologue, after the XML declaration but before the DTD.
*          2 - In the prologue, after the DTD but before the root element.
*          3 - In the epilogue, after the root element.
*     text
*        Pointer to a null terminated string containing the character data.
*        If "this" is a document, the text must consist entirely of white
*        space.

*-
*/

/* Local Variables: */
   AstXmlCharData *new;        /* Pointer to the new structure */
   char *my_text;              /* Pointer to cleaned text */
   char *c;                    /* Pointer to next character */

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise */
   new = NULL;

/* Clean the text by replacing "\r\n" by "\n". */
   my_text = CleanText( text, status );

/* See if the text is all white. */
   c = my_text - 1;
   while( *(++c) && isspace( *c ) );

/* If the string contains a non-white character, allocate memory for
   a XmlBlack structure, and initialise it to hold the supplied text.
   Otherwise, allocate memory for a XmlWhite structure, and initialise it
   to hold the supplied text. */
   if( *c ) {
      if( astXmlCheckType( this, AST__XMLDOC ) ) {
         astError( AST__XMLCM, "astXmlAddCharData(xml): Illegal attempt "
                   "to add non-white character data to the prologue or "
                   "epilogue of an XML document: \"%s\".", status, my_text );
      } else {
         new = (AstXmlCharData *) astMalloc( sizeof( AstXmlBlack ) );
         InitXmlBlack( (AstXmlBlack *) new, AST__XMLBLACK, my_text, status );
      }

   } else {
      new = (AstXmlCharData *) astMalloc( sizeof( AstXmlWhite ) );
      InitXmlWhite( (AstXmlWhite *) new, AST__XMLWHITE, my_text, status );
   }

/* Free the memory holding the cleaned text */
   my_text = astFree( my_text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, where, (AstXmlContentItem *) new, status );
   }
}

void astXmlAddComment_( AstXmlParent *this, int where, const char *text, int *status ){
/*
*+
*  Name:
*     astXmlAddComment

*  Purpose:
*     Create a new XmlComment and add it to an XmlElement or XmlDocument.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddComment( AstXmlParent *this, int where, const char *text )

*  Description:
*     This function creates a new XmlComment structure representing
*     an XML comment, and adds it into an existing element or document.

*  Parameters:
*     this
*        Pointer to the element or document to be modified.
*     where
*        Ignored if "this" is an XmlElement pointer. Otherwise, "where"
*        indicates where the item should be added to the document:
*          1 - In the prologue, after the XML declaration but before the DTD.
*          2 - In the prologue, after the DTD but before the root element.
*          3 - In the epilogue, after the root element.
*     text
*        Pointer to a null terminated string containing the comment text.

*-
*/

/* Local Variables: */
   AstXmlComment *new;        /* Pointer to the new structure */
   char *my_text;            /* Cleaned text */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlComment *) astMalloc( sizeof( AstXmlComment ) );

/* Clean the text. */
   my_text = CleanText( text, status );

/* Initialise it. */
   InitXmlComment( new, AST__XMLCOM, my_text, status );

/* Free the memory */
   my_text = astFree( my_text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, where, (AstXmlContentItem *) new, status );
   }

}

AstXmlElement *astXmlAddElement_( AstXmlElement *this, const char *name,
                                  const char *prefix, int *status ){
/*
*+
*  Name:
*     astXmlAddElement

*  Purpose:
*     Create a new empty XmlElement and adds it to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *astXmlAddElement( AstXmlElement *this, const char *name,
*                                      const char *prefix )

*  Description:
*     This function creates a new XmlElement structure representing an
*     empty XML element with the given name and namespace prefix, and
*     adds it into an existing element.

*  Parameters:
*     this
*        A pointer to the element to be modified. This may be NULL.
*     name
*        The name for the element.
*     prefix
*        The namespace prefix for the element. May be NULL or blank, in
*        which case any prefix at the start of "name" is used.

*  Returned Value:
*     A pointer to the new structure is returned. This pointer should be
*     freed using astXmlAnnul when no longer needed.

*  Notes:
*     - A NULL pointer is returned if the inherited status value
*     indicates an error has occurred on entry, or if this function
*     should fail for any reason.
*-
*/

/* Local Variables: */
   AstXmlElement *new;        /* The returned pointer */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Allocate space for the new structure. */
   new = (AstXmlElement *) astMalloc( sizeof( AstXmlElement ) );

/* Initialise it. */
   InitXmlElement( new, AST__XMLELEM, name, prefix, status );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( (AstXmlParent *) this, 0, (AstXmlContentItem *) new, status );
   }

/* Return the result. */
   return new;

}

void astXmlAddPI_( AstXmlParent *this, int where, const char *target, const char *text, int *status ){
/*
*+
*  Name:
*     astXmlAddPI

*  Purpose:
*     Create a new XmlPI and add it to an element or document.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddPI( AstXmlParent *this, int where, const char *target,
*                       const char *text )

*  Description:
*     This function creates a new XmlPI structure representing an
*     XML "programming instruction", and adds it into an existing element
*     or document.

*  Parameters:
*     this
*        Pointer to the element or document to be modified. This should
*        be a pointer to an XmlElement or an XmlDocument.
*     where
*        Ignored if "this" is an XmlElement pointer. Otherwise, "where"
*        indicates where the PI should be added to the document:
*          1 - In the prologue, after the XML declaration but before the DTD.
*          2 - In the prologue, after the DTD but before the root element.
*          3 - In the epilogue, after the root element.
*     target
*        Pointer to a null terminated string containing the PI target.
*     text
*        Pointer to a null terminated string containing the PI text.

*-
*/

/* Local Variables: */
   AstXmlPI *new;        /* Pointer to the new structure */
   char *my_text;            /* Cleaned text */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlPI *) astMalloc( sizeof( AstXmlPI ) );

/* Clean the text. */
   my_text = CleanText( text, status );

/* Initialise it. */
   InitXmlPI( new, AST__XMLPI, target, my_text, status );

/* Free the memory */
   my_text = astFree( my_text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, where, (AstXmlContentItem *) new, status );
   }
}

void astXmlAddURI_( AstXmlElement *this, const char *prefix, const char *uri, int *status ){
/*
*+
*  Name:
*     astXmlAddURI

*  Purpose:
*     Add a namespace prefix definition to an XmlElement, or change the
*     default namespace.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddURI( AstXmlElement *this, const char *prefix,
*                        const char *uri )

*  Description:
*     This function adds a namespace prefix definition to a specified
*     XmlElement, or changes the default namespace. If the suppliedprefix
*     is already defined in the element, the associated URI is changed to
*     the supplied URI.

*  Parameters:
*     this
*        The pointer to the element to be modified.
*     prefix
*        Pointer to a null terminated string containing the namespace
*        prefix. If this is NULL or blank, then the supplied URI is used
*        as the default namespace for this element and all child elements
*        (except for child elements which define their own default
*        namespace).
*     uri
*        Pointer to a null terminated string containing the namespace URI.
*        If this is NULL or blank, and "prefix" is also NULL or blank, then
*        this has the same effect of there being no default namespace within
*        the supplied element.
*-
*/

/* Local Variables: */
   AstXmlNamespace *ns;    /* The new namespace definition */
   AstXmlNamespace *oldns; /* The existing namespace definition */
   int i;                  /* Loop index */
   int nc;                 /* Length of namespace prefix */
   int nnspref;            /* Number of namespace defintions in the element */
   int oldi;               /* Index of existing attribute */

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise */
   oldns = NULL;

/* Store the used length of the namespace prefix. */
   nc = prefix ? astChrLen( prefix ) : 0;

/* If no namespace prefix has been supplied, just change the default
   namespace URI. */
   if( !nc ) {
      if( uri ) {
         this->defns = astStore( this->defns, uri, strlen( uri ) + 1 );
      } else {
         this->defns = astStore( this->defns, "", 1 );
      }

/* Otherwise, add the namespace definition to the element. */
   } else {

/* Create a new XmlNamespace. */
      ns = NewNamespace( prefix, uri, status );

/* If OK, indicate that the namespace is owned by the element. */
      if( astOK ) {
         ( (AstXmlObject *) ns )->parent = (AstXmlParent *) this;

/* Save the number of namespace definitions currently stored in the element. */
         nnspref = ( this->nsprefs ) ? this->nnspref : 0;

/* Search the existing prefixes to see if a namespace with the given
   prefix already exists. */
         oldi = -1;
         for( i = 0; i < nnspref; i++ ) {
            oldns = this->nsprefs[ i ];
            if( !strcmp( oldns->prefix, ns->prefix ) ) {
               oldi = i;
               break;
            }
         }

/* If there is an existing namespace with the same prefix, replace the old
   namespace with the new one created above. */
         if( oldi > -1 ){
            ((AstXmlObject *)oldns)->parent = NULL;
            oldns = astXmlAnnul( oldns );
            this->nsprefs[ oldi ] = ns;

/* Otherwise, attempt to extend the array to hold an extra namespace definition. */
         } else {
            this->nsprefs = astGrow( this->nsprefs, nnspref + 1,
                                     sizeof( AstXmlNamespace * ) );

/* Check all has gone OK. */
            if( astOK ) {

/* Store the Namespace pointer in the array of Namespace pointers. */
               this->nsprefs[ nnspref ] = ns;

/* Increment the number of namespaces in this element */
               this->nnspref = nnspref + 1;
            }
         }
      }
   }
}

void *astXmlAnnul_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlAnnul

*  Purpose:
*     Free the resources used by an XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void *astXmlAnnul( AstXmlObject *this )

*  Description:
*     This function frees the resources used to hold the XmlObject, together
*     with any child objects contained within the supplied XmlObject. A NULL
*     pointer is always returned. If the supplied object is still in use
*     (that is, if its parent XmlElement still exists) then the resources
*     are not freed, and a copy of the supplied pointer is returned.

*  Parameters:
*     this
*        pointer to the XmlObject to be freed.

*  Returned Value:
*     A NULL pointer, or the supplied pointer if the XmlObject is still
*     in use.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Return if a NULL pointer has been suppplied. */
   if( !this ) return NULL;

/* Return the supplied pointer if the objects parent still exists. */
   if( this->parent &&
       astXmlCheckType( this->parent, AST__XMLPAR ) ) return this;

#ifdef DEBUG
/* Remove the supplied object from the list of currently active XmlObjects. */
   RemoveObjectFromList( this );
#endif

/* Clean the objects contents, and free the memory holding the XmlObject. */
   CleanXml( this, this->type, status );
   astFree( this );

/* Return a NULL pointer. */
   return NULL;
}

void *astXmlAnnulTree_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlAnnulTree

*  Purpose:
*     Free the resources used by a tree of XmlObjects.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void *astXmlAnnulTree( AstXmlObject *this )

*  Description:
*     This function finds the head of the tree containing the supplied
*     XmlObject (either an XmlElement or an XmlDocument), and frees the
*     resources associated with all members of the tree. A NULL pointer
*     is always returned.

*  Parameters:
*     this
*        Pointer to a member of the tree of XmlObjects to be freed.

*  Returned Value:
*     A NULL pointer.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Return if a NULL pointer has been suppplied. */
   if( !this ) return NULL;

/* Find the root and annull it. This will free all children (i.e.
   the entire tree). */
   return astXmlAnnul( astXmlGetRoot( this ) );
}

AstXmlObject *astXmlCopy_( AstXmlObject *this, int *status ) {
/*
*+
*  Name:
*     astXmlCopy

*  Purpose:
*     Produce a deep copy of a supplied XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlObject *astXmlCopy( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a deep copy of the supplied
*     XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject to copy.

*  Returned Value:
*     Pointer to the new copy.

*  Notes:
*     - NULL is returned if NULL pointer is supplied.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/


/* Local Variables: */
   AstXmlAttribute *attr;
   AstXmlBlack *black;
   AstXmlCDataSection *cdata;
   AstXmlComment *comm;
   AstXmlDTDec *dtd;
   AstXmlDeclPI *dec;
   AstXmlDocument *doc, *newdoc;
   AstXmlElement *elem, *newelem;
   AstXmlNamespace *ns;
   AstXmlObject *new;
   AstXmlPI *pi;
   AstXmlPrologue *pro, *newpro;
   AstXmlWhite *white;
   int i, type;

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK || !this ) return new;

/* Initialise a new XmlObject of the required class, and copy any
   sub-objects. */
   type = this->type;
   if( type == AST__XMLELEM  ){
      elem = (AstXmlElement *) this;
      new = astMalloc( sizeof( AstXmlElement ) );
      InitXmlElement( (AstXmlElement *) new, AST__XMLELEM,
                      elem->name, elem->prefix, status );

      newelem = (AstXmlElement *) new;

      newelem->attrs = astMalloc( sizeof( AstXmlAttribute *) * (size_t)elem->nattr );
      newelem->nattr = elem->nattr;
      for( i = 0; i < elem->nattr; i++ ) {
         newelem->attrs[ i ] = (AstXmlAttribute *) astXmlCopy( elem->attrs[ i ] );
         ((AstXmlObject *) newelem->attrs[ i ])->parent = (AstXmlParent *) newelem;
      }

      newelem->items = astMalloc( sizeof( AstXmlContentItem *) * (size_t)elem->nitem );
      newelem->nitem = elem->nitem;
      for( i = 0; i < elem->nitem; i++ ) {
         newelem->items[ i ] = (AstXmlContentItem *) astXmlCopy( elem->items[ i ] );
         ((AstXmlObject *) newelem->items[ i ])->parent = (AstXmlParent *) newelem;
      }

      newelem->nsprefs = astMalloc( sizeof( AstXmlNamespace *) * (size_t)elem->nnspref );
      newelem->nnspref = elem->nnspref;
      for( i = 0; i < elem->nnspref; i++ ) {
         newelem->nsprefs[ i ] = (AstXmlNamespace *) astXmlCopy( elem->nsprefs[ i ] );
         ((AstXmlObject *) newelem->nsprefs[ i ])->parent = (AstXmlParent *) newelem;
      }

      if( elem->defns ) {
         newelem->defns = astStore( NULL, elem->defns,
                                    strlen( elem->defns ) + 1 );
      }

      newelem->complete = elem->complete;


   } else if( type == AST__XMLATTR ){
      attr = (AstXmlAttribute *) this;
      new = astMalloc( sizeof( AstXmlAttribute ) );
      InitXmlAttribute( (AstXmlAttribute *) new, AST__XMLATTR,
                        attr->name, attr->value, attr->prefix, status );

   } else if( type == AST__XMLBLACK ){
      black = (AstXmlBlack *) this;
      new = astMalloc( sizeof( AstXmlBlack ) );
      InitXmlBlack( (AstXmlBlack *) new, AST__XMLBLACK,
                    black->text, status );

   } else if( type == AST__XMLWHITE ){
      white = (AstXmlWhite *) this;
      new = astMalloc( sizeof( AstXmlWhite ) );
      InitXmlWhite( (AstXmlWhite *) new, AST__XMLWHITE,
                    white->text, status );

   } else if( type == AST__XMLCDATA ){
      cdata = (AstXmlCDataSection *) this;
      new = astMalloc( sizeof( AstXmlCDataSection ) );
      InitXmlCDataSection( (AstXmlCDataSection *) new, AST__XMLCDATA,
                           cdata->text, status );

   } else if( type == AST__XMLCOM ){
      comm = (AstXmlComment *) this;
      new = astMalloc( sizeof( AstXmlComment ) );
      InitXmlComment( (AstXmlComment *) new, AST__XMLCOM,
                      comm->text, status );

   } else if( type == AST__XMLPI ){
      pi = (AstXmlPI *) this;
      new = astMalloc( sizeof( AstXmlPI ) );
      InitXmlPI( (AstXmlPI *) new, AST__XMLPI, pi->target, pi->text, status );

   } else if( type == AST__XMLNAME ){
      ns = (AstXmlNamespace *) this;
      new = astMalloc( sizeof( AstXmlNamespace ) );
      InitXmlNamespace( (AstXmlNamespace *) new, AST__XMLNAME, ns->prefix,
                         ns->uri, status );

   } else if( type == AST__XMLDOC ){
      doc = (AstXmlDocument *) this;
      new = astMalloc( sizeof( AstXmlDocument ) );
      InitXmlDocument( (AstXmlDocument *) new, AST__XMLDOC, status );

      newdoc = (AstXmlDocument *) new;

      if( doc->prolog ) {
         newdoc->prolog = (AstXmlPrologue *) astXmlCopy( doc->prolog );
         ((AstXmlObject *) newdoc->prolog)->parent = (AstXmlParent *) newdoc;
      }

      if( doc->root ) {
         newdoc->root = (AstXmlElement *) astXmlCopy( doc->root );
         ((AstXmlObject *) newdoc->root)->parent = (AstXmlParent *) newdoc;
      }

      newdoc->epilog = astMalloc( sizeof( AstXmlMiscItem *) * (size_t)doc->nepi );
      newdoc->nepi = doc->nepi;
      for( i = 0; i < doc->nepi; i++ ) {
         newdoc->epilog[ i ] = (AstXmlMiscItem *) astXmlCopy( doc->epilog[ i ] );
         ((AstXmlObject *) newdoc->epilog[ i ])->parent = (AstXmlParent *) newdoc;
      }

      newdoc->current = NULL;

   } else if( type == AST__XMLPRO ){
      pro = (AstXmlPrologue *) this;
      new = astMalloc( sizeof( AstXmlPrologue ) );
      InitXmlPrologue( (AstXmlPrologue *) new, AST__XMLPRO, status );

      newpro = (AstXmlPrologue *) new;

      if( pro->xmldecl ) {
         newpro->xmldecl = (AstXmlDeclPI *) astXmlCopy( pro->xmldecl );
         ((AstXmlObject *) newpro->xmldecl)->parent = (AstXmlParent *) newpro;
      }

      if( pro->dtdec ) {
         newpro->dtdec = (AstXmlDTDec *) astXmlCopy( pro->dtdec );
         ((AstXmlObject *) newpro->dtdec)->parent = (AstXmlParent *) newpro;
      }

      newpro->misc1 = astMalloc( sizeof( AstXmlMiscItem *) * (size_t)pro->nmisc1 );
      newpro->nmisc1 = pro->nmisc1;
      for( i = 0; i < pro->nmisc1; i++ ) {
         newpro->misc1[ i ] = (AstXmlMiscItem *) astXmlCopy( pro->misc1[ i ] );
         ((AstXmlObject *) newpro->misc1[ i ])->parent = (AstXmlParent *) newpro;
      }

      newpro->misc2 = astMalloc( sizeof( AstXmlMiscItem *) * (size_t)pro->nmisc2 );
      newpro->nmisc2 = pro->nmisc2;
      for( i = 0; i < pro->nmisc2; i++ ) {
         newpro->misc2[ i ] = (AstXmlMiscItem *) astXmlCopy( pro->misc2[ i ] );
         ((AstXmlObject *) newpro->misc2[ i ])->parent = (AstXmlParent *) newpro;
      }

   } else if( type == AST__XMLDEC ){
      dec = (AstXmlDeclPI *) this;
      new = astMalloc( sizeof( AstXmlDeclPI ) );
      InitXmlDeclPI( (AstXmlDeclPI *) new, AST__XMLDEC, dec->text, status );

   } else if( type == AST__XMLDTD ){
      dtd = (AstXmlDTDec *) this;
      new = astMalloc( sizeof( AstXmlDTDec ) );
      InitXmlDTDec( (AstXmlDTDec *) new, AST__XMLDTD, dtd->name,
                    dtd->external, dtd->internal, status );

   } else if( astOK ) {
      astError( AST__INTER, "CopyXml: Invalid object type (%d) supplied "
                "(internal AST programming error).", status, type );
   }

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlDelete( new );

/* Return the result. */
   return new;
}

const char *astXmlFormat_( AstXmlObject *this, int *status ) {
/*
*+
*  Name:
*     astXmlFormat

*  Purpose:
*     Converts an XmlObject into a character string.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlFormat( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a dynamically allocated string
*     containing a textual representation of the supplied XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject to format.

*  Returned Value:
*     Pointer to a null terminated string holding the formated XmlObject.
*     This string should be freed when no longer needed using astFree.

*  Notes:
*     - No newlines or indentation strings are added to the returned string.
*     - NULL is returned if NULL pointer is supplied.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/
   return Format( this, -1, status );
}

const char *astXmlGetAttributeValue_( AstXmlElement *this, const char *name, int *status ){
/*
*+
*  Name:
*     astXmlGetAttributeValue

*  Purpose:
*     Return a pointer to a string holding the value of a named attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetAttributeValue( AstXmlElement *this, const char *name )

*  Description:
*     This function returns a pointer to a constant string holding the
*     value of a named attribute of a supplied element. If the element
*     does not have the named attribute, a NULL pointer is returned but
*     no error is reported.

*  Parameters:
*     this
*        The pointer to the XmlElement.
*     name
*        Pointer to a string holding the name of the attribute. The name
*        may be preceeded with a "prefix:" string, in which case the
*        prefix will also be matched. If no prefix is included, the first
*        attribute with the specified name is returned, regardless of
*        its prefix.

*  Returned Value:
*     Pointer to a string holding the value of the attribute within the
*     supplied element, or NULL if the attribute was not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   const char *result;     /* Returned pointer */
   AstXmlAttribute *attr;  /* Pointer to the attribute */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Find the attribute. */
   attr = FindAttribute( this, name, status );

/* Get its value. */
   if( attr ) result = attr->value;

/* Return the result. */
   return result;
}

AstXmlContentItem *astXmlGetItem_( AstXmlElement *this, int item, int *status ){
/*
*+
*  Name:
*     astXmlGetItem

*  Purpose:
*     Return a specified item of the content of an element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlContentItem *astXmlGetItem( AstXmlElement *this, int item )

*  Description:
*     This function returns a pointer to an item of the content of the
*     specified element.

*  Parameters:
*     this
*        The pointer to the XmlElement.
*     item
*        The index of the required item, in the range zero to "nitem-1",
*        where "nitem" is the number of items in the element as returned
*        by astXmlGetNitem. An error is reported if the specified index
*        is out of bounds.

*  Returned Value:
*     A pointer to the requested item.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   AstXmlContentItem *result;     /* The returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Report an error if the supplie dindex is bad. */
   if( this->nitem == 0 ) {
      astError( AST__XMLIT, "astXmlGetItem(xml): The supplied item index (%d) "
                "is out of bounds. The supplied XmlObject has no content.", status,
                item );

   } else if( item < 0 || item >= this->nitem ) {
      astError( AST__XMLIT, "astXmlGetItem(xml): The supplied item index (%d) "
                "is out of bounds. Should be in the range 0 to %d.", status,
                item, this->nitem-1 );
   } else {
      result = this->items[ item ];
   }

/* Return the result. */
   return result;
}

const char *astXmlGetName_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlGetName

*  Purpose:
*     Return a pointer to a string holding the name of an XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetName( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a constant string holding the
*     name associated with an XmlObject. For elements and attributes, the
*     "name" value is returned. For PI elements, the "target" value is
*     returned. For namespace definitions, the "prefix" value is returned.
*     An error is reported if the supplied XmlObject is of any other class.

*  Parameters:
*     this
*        The pointer to the XmlObject.

*  Returned Value:
*     Pointer to the name string within the XML object.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   const char *result;     /* Returned pointer */
   int type;               /* Object type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Return the relevant component of the structure, depending on its type. */
   type = this->type;
   if( type == AST__XMLELEM  ){
      result = ( (AstXmlElement *) this )->name;

   } else if( type == AST__XMLATTR ){
      result = ( (AstXmlAttribute *) this )->name;

   } else if( type == AST__XMLPI ){
      result = ( (AstXmlPI *) this )->target;

   } else if( type == AST__XMLNAME ){
      result = ( (AstXmlNamespace *) this )->prefix;

   } else {
      astError( AST__INTER, "astXmlGetName: Inappropriate object type (%d) supplied "
                "(internal AST programming error).", status, type );
   }

/* Return the result. */
   return result;
}

int astXmlGetNattr_( AstXmlElement *this, int *status ){
/*
*+
*  Name:
*     astXmlGetNattr

*  Purpose:
*     Return the number of attributes held by an element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     int astXmlGetNattr( AstXmlElement *this )

*  Description:
*     This function returns the number of  attributes held by an element.

*  Parameters:
*     this
*        The pointer to the XmlElement.

*  Returned Value:
*     The number of  attributes held by the supplied element.

*  Notes:
*     - Zero is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Check the global error status. */
   if( !astOK ) return 0;

/* Return the result. */
   return ( this->attrs ) ? this->nattr : 0;
}

int astXmlGetNitem_( AstXmlElement *this, int *status ){
/*
*+
*  Name:
*     astXmlGetNitem

*  Purpose:
*     Return the number of items within the content of an element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     int astXmlGetNitem( AstXmlElement *this )

*  Description:
*     This function returns the number of items within the content of an
*     XmlElement.

*  Parameters:
*     this
*        The pointer to the XmlElement.

*  Returned Value:
*     The number of items in the content of the supplied element.

*  Notes:
*     - Zero is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Check the global error status. */
   if( !astOK ) return 0;

/* Return the result. */
   return this->nitem;
}

AstXmlParent *astXmlGetParent_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlGetParent

*  Purpose:
*     Return a pointer to the object which contains the supplied XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlParent *astXmlGetParent( AstXmlObject *this )

*  Description:
*     This function returns a pointer to the XmlParent object (either an
*     XmlElement or an XmlDocument) which contains the specified XmlObject.
*     The object can be a content item (an element, a comment, a CDATA
*     section, a PI, or character data) in which case the enclosing
*     XmlElement is returned, or an attribute or namespace definition in
*     which case the XmlElement to which object refers is returned.
*     If "this" is the root element of a document, a pointer to the
*     XmlDocument is returned.


*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     Pointer to the parent, or NULL if the object does not have a parent.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Check the global error status. */
   if( !astOK ) return NULL;

/* Return the result. */
   return this->parent;
}

AstXmlObject *astXmlGetRoot_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlGetRoot

*  Purpose:
*     Return a pointer to the root XmlObject which contains the supplied
*     XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlObject *astXmlGetRoot( AstXmlObject *this )

*  Description:
*     This function returns a pointer to the XmlObject which is the root of
*     the tree containing the specified XmlObject. A pointer to the
*     supplied XmlObject is returned if it has no parent.

*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     Pointer to the root XmlObject, or a copy of the supplied pointer if
*     the supplied XmlObject is the root.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables:  */
   AstXmlObject *result;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If "this" is a document, check it has no parent. If not, return a
   pointer ot it. */
   if( astXmlCheckType( this, AST__XMLDOC ) ) {
      if( this->parent ) {
         astError( AST__INTER, "astXmlGetRoot(xml): An XmlDocument has a "
                   "non-null parent of type %ld (internal AST programming "
                   "error).", status, this->type );
      } else {
         result = (AstXmlObject *) this;
      }

/* Otherwise... */
   } else if( this->parent ) {
      result = astXmlGetRoot( this->parent );

   } else {
      result = this;
   }

/* Return the result. */
   return result;
}

const char *astXmlGetTag_( AstXmlObject *this, int opening, int *status ){
/*
*+
*  Name:
*     astXmlGetTag

*  Purpose:
*     Returns a string holding an XML tag describing the given XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetTag( AstXmlObject *this, int opening )

*  Description:
*     This function returns a pointer to a static string containing an
*     XML tag describing the given XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject.
*     opening
*        Indicates which tag is to be returned; the start tag or the end
*        tag. If non-zero the start tag is returned. Otherwise, the
*        end tag is returned. If the supplied XmlObject has no end
*        tag (i.e. if it is an empty element, or if it is not an element),
*        then NULL is returned but no error is reported.

*  Returned Value:
*     Pointer to a null terminated string holding the tag. If the tag
*     exceeds 200 characters, only the first 197 characters are returned
*     and "..." is appended to the end.

*  Notes:
*     - Subsequent invocations of this function will over-write the
*     buffer which used to hold the returned string.
*     - Empty elements are represented as an start tag of the form <.../>,
*     with no corresponding end tag.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */
   char *result;             /* The returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Get a dynamic string holding the formatted tag. */
   result = FormatTag( this, opening, status );

/* If OK, copy the result into the static buffer. */
   gettag_buff[ 0 ] = 0;
   if( result ) {
      if( astOK ) {

         if( strlen( result ) > AST__XML_GETTAG_BUFF_LEN ) {
            strncpy( gettag_buff, result, AST__XML_GETTAG_BUFF_LEN -3 );
            strcpy( gettag_buff + AST__XML_GETTAG_BUFF_LEN - 3, "..." );
         } else {
            strncpy( gettag_buff, result, AST__XML_GETTAG_BUFF_LEN );
         }

         gettag_buff[ AST__XML_GETTAG_BUFF_LEN ] = 0;
         astFree( result );
         result = gettag_buff;
      } else {
         result = astFree( result );
      }
   }

/* Return the result. */
   return result;
}

const char *astXmlGetType_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlGetType

*  Purpose:
*     Returns a string holding the type of the given XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetType( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a static string containing the
*     type of the given XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject.

*  Returned Value:
*     Pointer to a null terminated string holding the type string.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   const char *result;             /* The returned pointer */
   int type;                       /* Element type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

   type = this->type;
   if( type == AST__XMLELEM ) {
      result = "element";

   } else if( type == AST__XMLATTR ) {
      result = "attribute";

   } else if( type == AST__XMLCDATA ) {
      result = "CDATA section";

   } else if( type == AST__XMLCOM ) {
      result = "comment";

   } else if( type == AST__XMLPI ) {
      result = "processing instruction";

   } else if( type == AST__XMLNAME ) {
      result = "namespace";

   } else if( type == AST__XMLDOC ) {
      result = "document";

   } else if( type == AST__XMLPRO ) {
      result = "prologue";

   } else if( type == AST__XMLDEC ) {
      result = "XML delaration PI";

   } else if( type == AST__XMLDTD ) {
      result = "DTD";

   } else if( type == AST__XMLWHITE ) {
      result = "white-space character data ";

   } else if( type == AST__XMLBLACK ) {
      result = "non-blank character data";

   } else {
      result = "unknown XML object";
   }

/* Return the result. */
   return result;
}

const char *astXmlGetURI_( AstXmlObject *this, int *status ){
/*
*+
*  Name:
*     astXmlGetURI

*  Purpose:
*     Return a pointer to a string holding the namespace URI of an XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetURI( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a constant string holding the
*     namespace URI associated with an XmlObject. Only attributes,
*     elements and namespaces have associated URIs, so a NULL pointer is
*     returned for any other class of XmlObject. A NULL pointer is also
*     returned if XmlObject does not belong to any namespace, or if it
*     belongs to a unknown namespace (i.e. one for which no URI is
*     available). Any namespace prefix attached to the supplied object is
*     resolved first using any "xmlns" attributes contained in the same
*     element, then using any "xmlns" attributes contained in the parent
*     element, etc.

*  Parameters:
*     this
*        The pointer to the XmlObject.

*  Returned Value:
*     Pointer to a string holding the namespace URI, or NULL.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   const char *prefix;     /* Namespace prefix */
   const char *result;     /* Returned pointer */
   int type;               /* Object type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Do each type of object separately. */
   type = this->type;
   if( type == AST__XMLATTR ){
      prefix = ( (AstXmlAttribute *) this )->prefix;

/* Attributes have no default name space. Therefore if there is no prefix,
   return NULL. If there is a prefix, resolve it within the context of
   the attributes parent element. */
      if( prefix ) {
         result = ResolvePrefix( prefix, (AstXmlElement *) this->parent, status );
      }

   } else if( type == AST__XMLELEM ){
      prefix = ( (AstXmlElement *) this )->prefix;

/* If there is a prefix, resolve it within the context of this element. */
      if( prefix ) {
         result = ResolvePrefix( prefix, (AstXmlElement *) this, status );

/* Elements do have a default name space. Therefore if there is no prefix,
   return the default name space within the context of this element. */
      } else {
         result = DefaultURI( (AstXmlElement *) this, status );
      }

/* If the supplied object is a namespace, just return the associated URI. */
   } else if( type == AST__XMLNAME ){
      result = ( (AstXmlNamespace *) this )->uri;

   }

/* Return the result. */
   return result;
}

const char *astXmlGetValue_( AstXmlObject *this, int report, int *status ){
/*
*+
*  Name:
*     astXmlGetValue

*  Purpose:
*     Return a pointer to a string holding the value of an XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlGetValue( AstXmlObject *this, int report )

*  Description:
*     This function returns a pointer to a constant string holding the
*     value associated with an XmlObject. For attributes, the attribute value
*     is returned. For PI elements, the "text" value is returned. For
*     namespace definitions, the "URI" value is returned. For character
*     data, the character data is returned. For CDATA sections the "text"
*     value is returned. For comments, the "text" value is returned.
*     If the XmlObject is an element, then a non-NULL value is returned
*     only if the element contains a single content item holding character
*     data. In this case a pointer to the character data is returned.
*     A null value is returned in all other cases (but no error is
*     reported unless "report" is non-zero).

*  Parameters:
*     this
*        The pointer to the XmlObject.
*     report
*        Report an error if the supplied XmlObject does not have a value?

*  Returned Value:
*     Pointer to a string holding the value of the XML object.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   AstXmlContentItem *item;/* Element content */
   const char *result;     /* Returned pointer */
   int type;               /* Object type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Return the relevant component of the structure, depending on its type. */
   type = this->type;
   if( type == AST__XMLATTR ){
      result = ( (AstXmlAttribute *) this )->value;

   } else if( type == AST__XMLBLACK ){
      result = ( (AstXmlBlack *) this )->text;

   } else if( type == AST__XMLWHITE ){
      result = ( (AstXmlWhite *) this )->text;

   } else if( type == AST__XMLCDATA ){
      result = ( (AstXmlCDataSection *) this )->text;

   } else if( type == AST__XMLCOM ){
      result = ( (AstXmlComment *) this )->text;

   } else if( type == AST__XMLPI ){
      result = ( (AstXmlPI *) this )->text;

   } else if( type == AST__XMLNAME ){
      result = ( (AstXmlNamespace *) this )->uri;

   } else if( type == AST__XMLELEM ){
      if( astXmlGetNitem( (AstXmlElement *) this ) == 1 ) {
         item = astXmlGetItem( (AstXmlElement *) this, 0 );
         if( astXmlCheckType( item, AST__XMLCHAR ) ) {
            result = astXmlGetValue( item, report );
         }
      }

      if( !result && astOK && report ) {
         astError( AST__BADIN, "astRead(xml): Cannot get the value of "
                   "element \"<%s>\": its contents are not pure character "
                   "data.", status, astXmlGetName( this ) );
      }

   } else if( report ) {
      astError( AST__INTER, "astXmlGetValue(xml): Cannot get the value of "
                "an XmlObject of type %d (internal AST programming "
                "error).", status, type );
   }

/* Return the result. */
   return result;
}

void astXmlInsertElement_( AstXmlElement *this, AstXmlElement *elem, int *status ){
/*
*+
*  Name:
*     astXmlInsertElement

*  Purpose:
*     Inserts an existing XmlElement into another XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlInsertElement( AstXmlElement *this, AstXmlElement *elem )

*  Description:
*     This function inserts a given XmlElement "elem" into another given
*     XmlElement "this". An error is reported if "elem" already has a
*     parent.

*  Parameters:
*     this
*        A pointer to the element to be modified.
*     elem
*        The element to be inserted into "this".

*-
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Report AN error if "elem" has already been inserted into
   another element. */
   if( ((AstXmlObject *) elem)->parent ) {
      astError( AST__INTER, "astXmlInsertElement(xml): Cannot insert \"%s\" "
                "into \"%s\" because it already has a parent (\"%s\") "
                "(internal AST programming error).", status,
                astXmlGetTag( elem, 1 ), astXmlGetTag( this, 1 ),
                astXmlGetTag( ((AstXmlObject *) elem)->parent, 1 ) );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( (AstXmlParent *) this, 0, (AstXmlContentItem *) elem, status );
   }
}

void astXmlPurge_( AstXmlParent *this, int *status ) {
/*
*+
*  Name:
*     astXmlPurge

*  Purpose:
*     Remove blank content from a parent object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlPurge( AstXmlParent *this )

*  Description:
*     This function removes all character data containing only whitespace
*     from the supplied document or element. It is recursive, in that it also
*     removes white space from all children elements.

*  Parameters:
*     this
*        Pointer to the document or element.

*-
*/

/* Local Variables: */
   int i;                    /* Content item index */
   AstXmlContentItem *item;  /* Next content item */
   AstXmlMiscItem *misc;     /* Nest miscalleneous item */
   AstXmlDocument *doc;      /* This document */
   AstXmlPrologue *pro;      /* This document prologue */
   AstXmlElement *elem;      /* This element */

/* Check the global error status. */
   if( !astOK || !this ) return;

/* If this is a a document.. */
   if( astXmlCheckType( this, AST__XMLDOC ) ) {
      doc = (AstXmlDocument *) this;
      astXmlPurge( doc->prolog );
      astXmlPurge( doc->root );

      i = -1;
      while( ++i < doc->nepi ) {
         misc = doc->epilog[ i ];
         if( astXmlCheckType( misc, AST__XMLWHITE ) ) {
            misc = astXmlDelete( misc );
            i--;
         }
      }

/* If this is a prologue.. */
   } else if( astXmlCheckType( this, AST__XMLPRO ) ) {
      pro = (AstXmlPrologue *) this;

      i = -1;
      while( ++i < pro->nmisc1 ) {
         misc = pro->misc1[ i ];
         if( astXmlCheckType( misc, AST__XMLWHITE ) ) {
            misc = astXmlDelete( misc );
            i--;
         }
      }

      i = -1;
      while( ++i < pro->nmisc2 ) {
         misc = pro->misc2[ i ];
         if( astXmlCheckType( misc, AST__XMLWHITE ) ) {
            misc = astXmlDelete( misc );
            i--;
         }
      }


/* If this is an element */
   } else if( astXmlCheckType( this, AST__XMLELEM ) ) {
      elem = (AstXmlElement *) this;

      i = -1;
      while( ++i < elem->nitem ) {
         item = elem->items[ i ];

         if( astXmlCheckType( item, AST__XMLWHITE ) ) {
            item = astXmlDelete( item );
            i--;

         } else if( astXmlCheckType( item, AST__XMLELEM ) ) {
            astXmlPurge( (AstXmlParent *) item );
         }
      }
   }
}

void astXmlRemoveAttr_( AstXmlElement *this, const char *name,
                        const char *prefix, int *status ){
/*
*+
*  Name:
*     astXmlRemoveAttr

*  Purpose:
*     Removes an attribute from its parent element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlRemoveAttr( AstXmlElement *this, const char *name,
*                            const char *prefix )

*  Description:
*     This function removes a named attribute from its parent element.

*  Parameters:
*     this
*        The pointer to the element containing the attribute to be removed.
*     name
*        Pointer to a null terminated string containing the attribute name.
*     prefix
*        The namespace prefix for the attribute. May be NULL or blank, in
*        which case any prefix at the start of "name" is used.
*-
*/

/* Local Variables: */
   AstXmlAttribute *attr;           /* Pointer to temporary attribute structure */
   AstXmlAttribute *oldattr;        /* Pointer to existing attribute */
   int i;                           /* Attribute index */
   int nattr;                       /* Number of attributes in parent */
   int oldi;                        /* Indexof existing attribute */

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise */
   oldattr = NULL;

/* Create a new XmlAttribute with blank value. */
   attr = NewAttribute( name, "", prefix, status );
   if( astOK ) {

/* Get the number of attributes currently stored in the element. */
      nattr = ( this->attrs ) ? this->nattr : 0;

/* Search the existing attributes to see if an attribute with the given
   name and prefix already exists. */
      oldi = -1;
      for( i = 0; i < nattr; i++ ) {
         oldattr = this->attrs[ i ];
         if( !strcmp( oldattr->name, attr->name ) ) {
            if( !oldattr->prefix && !attr->prefix ) {
               oldi = i;
               break;
            } else if( oldattr->prefix && attr->prefix &&
                       !strcmp( oldattr->prefix, attr->prefix ) ){
               oldi = i;
               break;
            }
         }
      }

/* If there is an existing attribute with the same name and prefix,
   delete it. */
      if( oldi > -1 ) astXmlDelete( oldattr );

/* Delete the temporary attribute structure. */
      attr = astXmlDelete( attr );

   }
}

void astXmlRemoveItem_( AstXmlContentItem *this, int *status ){
/*
*+
*  Name:
*     astXmlRemoveItem

*  Purpose:
*     Removes an item of content from its parent element or document.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlRemoveItem( AstXmlContentItem *this )

*  Description:
*     This function removes an item of content from its parent element,
*     or removes the root element from a document. The removed item is not
*     annulled and may be subsequently added into another element.

*  Parameters:
*     this
*        The pointer to the item to be removed form its parent.
*-
*/

/* Local Variables: */
   AstXmlDocument *doc;             /* Pointer to parent document */
   AstXmlElement *elem;             /* Pointer to parent element */
   AstXmlParent *parent;            /* Pointer to parent */
   int found;                       /* Was the item found within its parent? */
   int i;                           /* Item index */
   int j;                           /* Item index */

/* Check the global error status. */
   if( !astOK ) return;

/* Get a pointer to the items parent element, and check it is not null. */
   parent = ( (AstXmlObject *) this )->parent;
   if( parent && astXmlCheckType( parent, AST__XMLELEM ) ) {
      elem = (AstXmlElement *) parent;

/* Search through all the items within the parent element looking for the
   supplied item. */
      found = 0;
      for( i = 0; i < elem->nitem; i++ ) {
         if( elem->items[ i ] == this ) {

/* When found, decrement the number of items in the element, and shuffle
   all the remaining item pointers down one slot to over-write it, then
   nullify the parent pointer in the supplied object and leave the loop. */
            (elem->nitem)--;
            for( j = i; j < elem->nitem; j++ ) {
               elem->items[ j ] = elem->items[ j + 1 ];
            }
            ( (AstXmlObject *) this )->parent = NULL;
            found = 1;
            break;
         }
      }

/* Report an error if the item was not found. */
      if( !found ) {
         astError( AST__INTER, "astXmlRemoveItem: The parent of the supplied "
                   "item does not contain the item (internal AST programming "
                   "error)." , status);
      }

/* If the parent is an XmlDocument, check the item being removed is the
   root element. */
   } else if( parent && astXmlCheckType( parent, AST__XMLDOC ) ) {
      doc = (AstXmlDocument *) parent;
      if( (AstXmlElement *) this == doc->root ) {
         ( (AstXmlObject *) this )->parent = NULL;
         doc->root = NULL;
      }
   }
}

void astXmlRemoveURI_( AstXmlElement *this, const char *prefix, int *status ){
/*
*+
*  Name:
*     astXmlRemoveURI

*  Purpose:
*     Removes an namespace prefix from its parent element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlRemoveURI( AstXmlElement *this, const char *prefix )

*  Description:
*     This function removes a named namespace prefix from its parent element.

*  Parameters:
*     this
*        The pointer to the element containing the namespace prefix to be
*        removed.
*     prefix
*        The namespace prefix to remove.
*-
*/

/* Local Variables: */
   AstXmlNamespace *ns;             /* Temporary namespace structure */
   AstXmlNamespace *oldns;          /* Pointer to existing namespace */
   int oldi;                        /* Index of namespace within its parent */
   int i;                           /* Namespace index */
   int nns;                         /* Number of existing namespaces */

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise */
    oldns = NULL;

/* Create a new XmlNamespace with blank URI. */
   ns = NewNamespace( prefix, "", status );
   if( astOK ) {

/* Get the number of namespace prefixes currently stored in the element. */
      nns = ( this->nsprefs ) ? this->nnspref : 0;

/* Search the list of existing namespace prefixes to see if the given prefix
   is included. */
      oldi = -1;
      for( i = 0; i < nns; i++ ) {
         oldns = this->nsprefs[ i ];
         if( !strcmp( oldns->prefix, ns->prefix ) ){
             oldi = i;
             break;
         }
      }

/* If the supplied namespace prefix was found in the list, delete it. */
      if( oldi > -1 ) astXmlDelete( oldns );

/* Delete the temporary namespace structure. */
      ns = astXmlDelete( ns );

   }
}

void astXmlSetDTDec_( AstXmlDocument *this, const char *text1,
                      const char *text2, const char *text3, int *status ){
/*
*+
*  Name:
*     astXmlSetDTDec

*  Purpose:
*     Set the Document Type declaration for a document.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlSetDTDEC( AstXmlDocument *this, const char *text1,
*                          const char *text2, const char *text3 )

*  Description:
*     This function stores an Document Type declaration of the form
*
*        <!DOCTYPE text1 text2 [text3]>
*
*     in the supplied document. Any previous DTD is removed.

*  Parameters:
*     this
*        The pointer to the document.
*     text1
*        The document type name.
*     text2
*        The text defining the external elements of the document type
*        (may be NULL).
*     text3
*        The text defining the internal elements of the document type
*        (may be NULL). Do not include delimiting "[" and "]" characters.
*-
*/

/* Local Variables: */
   AstXmlDTDec *new;             /* Pointer to new DT declaration */
   AstXmlPrologue *pro;          /* Pointer to prologue */
   char *my_text2;               /* Cleaned text2 */
   char *my_text3;               /* Cleaned text3 */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlDTDec *) astMalloc( sizeof( AstXmlDTDec ) );

/* Clean the text. */
   my_text2 = CleanText( text2, status );
   my_text3 = CleanText( text3, status );

/* Initialise it. */
   InitXmlDTDec( new, AST__XMLDTD, text1, my_text2, my_text3, status );

/* Free the memory */
   my_text2 = astFree( my_text2 );
   my_text3 = astFree( my_text3 );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, store it in the document, deleting any existing declaration
   first. */
   } else {

/* Create a prologue if necessary. */
      if( !this->prolog ) this->prolog = NewPrologue( this, status );

      pro = this->prolog;
      if( pro->dtdec ) astXmlDelete( pro->dtdec );
      pro->dtdec = new;
   }
}

void astXmlSetXmlDec_( AstXmlDocument *this, const char *text, int *status ){
/*
*+
*  Name:
*     astXmlSetXmlDec

*  Purpose:
*     Set the XML declaration for a document.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlSetXmlDec( AstXmlDocument *this, const char *text )

*  Description:
*     This function stores an XML declaration of the form
*
*        <?xml [text]?>
*
*     in the supplied document. Any previous XML declaration is removed.

*  Parameters:
*     this
*        The pointer to the document.
*     text
*        The text to include in the XML declaration tag.
*-
*/

/* Local Variables: */
   AstXmlDeclPI *new;             /* Pointer to new XML delcaration */
   AstXmlPrologue *pro;           /* Pointer to prologue */
   char *my_text;                 /* Cleaned text */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlDeclPI *) astMalloc( sizeof( AstXmlDeclPI ) );

/* Clean the text. */
   my_text = CleanText( text, status );

/* Initialise it. */
   InitXmlDeclPI( new, AST__XMLDEC, my_text, status );

/* Free the memory */
   my_text = astFree( my_text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlDelete( new );

/* Otherwise, store it in the document, deleting any existing declaration
   first. */
   } else {

/* Create a prologue if necessary. */
      if( !this->prolog ) this->prolog = NewPrologue( this, status );

      pro = this->prolog;
      if( pro->xmldecl ) astXmlDelete( pro->xmldecl );
      pro->xmldecl = new;
   }
}

const char *astXmlShow_( AstXmlObject *this, int *status ) {
/*
*+
*  Name:
*     astXmlShow

*  Purpose:
*     Converts an XmlObject into a character string with indentation.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     const char *astXmlShow( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a dynamically allocated string
*     containing a textual representation of the supplied XmlObject.
*     Newline characters are added to the string if needed to ensure that
*     each item of content within an element starts on a new line, and all
*     tags are preceeded by an indentation string consisting of a number
*     of spaces.

*  Parameters:
*     this
*        Pointer to the XmlObject to format.

*  Returned Value:
*     Pointer to a null terminated string holding the formated XmlObject.
*     This string should be freed when no longer needed using astFree.

*  Notes:
*     - NULL is returned if a NULL pointer is supplied.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/
   return Format( this, 0, status );
}

static void CheckName( const char *name, const char *noun, const char *method,
                       int nullok, int *status ){
/*
*  Name:
*     CheckName

*  Purpose:
*     Checks the supplied string is a valid XML name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void CheckName( const char *name, const char *noun, const char *method,
*                     int nullok, int *status )

*  Description:
*     This function checks that the supplied string is a valid XML name,
*     and reports an error otherwise.

*  Parameters:
*     name
*        The name string to check
*     noun
*        A word to describe the object which the name applies to - for use in
*        error messages only.
*     method
*        The name of the calling method - for use in error messages only.
*     nullok
*        If non-zero, then a null or empty name is assumed to be
*        acceptable.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   const char *c;       /* Pointer to next character to check */

/* Check the global error status. */
   if( !astOK ) return;

/* Check the string is not null. */
   if( !name ) {
      if( !nullok ) astError( AST__XMLNM, "%s: A NULL pointer was supplied "
                              "instead of an XML %s name.", status, method, noun );
   } else {

      c = name;
      if( *c == 0 ) {
         if( !nullok ) astError( AST__XMLNM, "%s: An empty string was supplied "
                                 "instead of an XML %s name.", status, method, noun );
      } else {

         if( !isalpha( *c ) && *c != '_' ) {
            astError( AST__XMLNM, "%s: The illegal XML %s name \"%s\" was "
                      "encountered.", status, method, noun, name );

         } else {
            while( *(++c) ) {
               if( !isalnum( *c ) && *c != '_' && *c != '-' && *c != '.' ){
                  astError( AST__XMLNM, "%s: The illegal XML %s name \"%s\" was "
                            "encountered.", status, method, noun, name );
                  break;
               }
            }
         }
      }
   }
}

static void CheckPrefName( char *name, const char *noun, const char *method, int *status ){
/*
*  Name:
*     CheckPrefName

*  Purpose:
*     Checks the supplied string is a valid XML (prefix:)name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void CheckPrefName( char *name, const char *noun, const char *method, int *status )

*  Description:
*     This function checks that the supplied string is a valid XML
*     (prefix:)name combination and reports an error otherwise.

*  Parameters:
*     name
*        The string to check
*     noun
*        A word to describe the object which the name applies to - for use in
*        error messages only.
*     method
*        The name of the calling method - for use in error messages only.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   char *colon;             /* Pointer to first colon */
   char *temp;              /* Pointer to temporary string */
   int nc;                  /* Length of temporary string */

/* Check the global error status. */
   if( !astOK ) return;

/* Search for a ":" character. */
   colon = strchr( name, ':' );

/* If found, temporarily convert the colon into a null so that it
   terminates the prefix string. */
   if( colon ) {
      *colon = 0;

/* Check the string before the colon is a valid name. */
      temp = NULL;
      temp = astAppendString( temp, &nc, noun );
      temp = astAppendString( temp, &nc, " prefix" );
      CheckName( name, temp, method, 0, status );
      temp = astFree( temp );

/* Restore the colon. */
      *colon = ':';

/* Check the string following the colon is a valid name. */
      CheckName( colon + 1, noun, method, 0, status );

/* If not found, the whole supplied string must be a name. */
   } else {
      CheckName( name, noun, method, 0, status );
   }
}

static int CheckType( long int given, long int want, int *status ){
/*
*  Name:
*     CheckType

*  Purpose:
*     Check that the supplied type identifies an object of a given class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     int CheckType( long int given, long int want, int *status )

*  Description:
*     This function checks that the supplied type identifier identifies
*     a specified class of XML object, or a derived class. A flag is
*     returned indicating if the check succeeds. No error is reported if
*     the check fails.

*  Parameters:
*     given
*        The type value to be checked.
*     want
*        The type of the required class.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the check is passed, zero if not of if an error has
*     already occurred.

*  Notes:
*     - This function attempts to execute even if the error status is set.
*/

/* Local Variables: */
   int result;              /* Returned value */

/* Initialise */
   result = 0;

/* Check the wanted type is recognised. Report an error if not. */
   if( want != AST__XMLOBJECT &&
       want != AST__XMLELEM  &&
       want != AST__XMLATTR  &&
       want != AST__XMLCHAR  &&
       want != AST__XMLCDATA &&
       want != AST__XMLCOM   &&
       want != AST__XMLPI    &&
       want != AST__XMLNAME  &&
       want != AST__XMLCONT  &&
       want != AST__XMLPRO   &&
       want != AST__XMLDEC   &&
       want != AST__XMLDTD   &&
       want != AST__XMLMISC  &&
       want != AST__XMLBLACK &&
       want != AST__XMLWHITE &&
       want != AST__XMLPAR   &&
       want != AST__XMLDOC ) {
      if( astOK ) {
         astError( AST__INTER, "CheckType(Xml): Unsupported XML object "
                   "type (%ld) supplied for parameter \"want\" (internal "
                   "AST programming error). ", status, want );
      }

/* You should never be given a generic "interface" type since the
   "wanted" value comes from the "type" component of an XmlObject (an explicit
   class type should always be given). */
   } else if( given == AST__XMLPAR ||
              given == AST__XMLMISC ||
              given == AST__XMLCONT ||
              given == AST__XMLCHAR ) {
      if( astOK ) {
         astError( AST__INTER, "CheckType(Xml): Generic type (%ld) supplied for "
                   "parameter \"given\" (internal AST programming error).", status,
                   given );
      }

/* If the above is OK, return a non-zero value if the type to be tested
   equals the wanted type. */
   } else if( want == given ) {
      result = 1;

/* If any class of XmlObject is acceptable, check that he given class
   type is a valid XML class type. */
   } else if( want == AST__XMLOBJECT ) {
      result = ( given == AST__XMLELEM  ||
                 given == AST__XMLATTR  ||
                 given == AST__XMLCDATA ||
                 given == AST__XMLCOM   ||
                 given == AST__XMLPI    ||
                 given == AST__XMLNAME  ||
                 given == AST__XMLPRO   ||
                 given == AST__XMLDEC   ||
                 given == AST__XMLDTD   ||
                 given == AST__XMLWHITE ||
                 given == AST__XMLBLACK ||
                 given == AST__XMLDOC );

/* Otherwise, for "interface" types, check if the given class "implements
   the interface". */
   } else if( want == AST__XMLCONT ) {
      result = ( given == AST__XMLELEM  ||
                 given == AST__XMLBLACK ||
                 given == AST__XMLWHITE ||
                 given == AST__XMLCDATA ||
                 given == AST__XMLCOM   ||
                 given == AST__XMLPI    );

   } else if( want == AST__XMLMISC ) {
      result = ( given == AST__XMLWHITE ||
                 given == AST__XMLCOM   ||
                 given == AST__XMLPI    );

   } else if( want == AST__XMLCHAR ) {
      result = ( given == AST__XMLWHITE ||
                 given == AST__XMLBLACK );

   } else if( want == AST__XMLPAR ) {
      result = ( given == AST__XMLDOC ||
                 given == AST__XMLPRO ||
                 given == AST__XMLELEM );
   }

/* Return the result. */
   return result;
}

int astXmlCheckType_( void *this, long int want, int *status ){
/*
*+
*  Name:
*     astXmlCheckType

*  Purpose:
*     Check that the supplied object is of a given class.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     int astXmlCheckType( void *this, long int want )

*  Description:
*     This function checks that the supplied XmlObject is of a specified
*     class of XML object, or a derived class. A flag is returned indicating
*     if the check succeeds. No error is reported if the check fails.

*  Parameters:
*     this
*        The object to check.
*     want
*        The type of the required class.

*  Returned Value:
*     Non-zero if the check is passed, zero if not of if an error has
*     already occurred.

*  Notes:
*     - This function attempts to execute even if the error status is set.
*-
*/

   if( this ) {
      return CheckType( ((AstXmlObject *) this)->type, want, status );
   } else {
      return 0;
   }
}

static char *CleanText( const char *text, int *status ){
/*
*  Name:
*     CleanText

*  Purpose:
*     Normalise end-of-lines in the supplied text.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *CleanText( const char *text, int *status )

*  Description:
*     This function returns a copy of "text in which "\r\n" has been
*     replaced by "\n" and any remaining "\r" characters have been
*     replaced by "\n".

*  Parameters:
*     text
*        A pointer to a text string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated string containing the required
*     copy.

*  Notes:
*     - NULL is returned if this function is called with the global error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   char *d;                  /* Pointer to next returned character */
   char *result;             /* Returned pointer */
   char *c;                  /* Pointer to next supplied character */
   char lc;                  /* Previous character */

/* Initialise */
   result = NULL;

/* Return if the pointer is NULL or if an error has occurred. */
   if( !astOK || !text ) return result;

/* Take a copy of the supplied text */
   result = astStore( NULL, text, strlen( text ) + 1 );

/* Clean the text by replacing "\r\n" by "\n". */
   c = result - 1;
   d = c;
   lc = 0;
   while( *(++c) ) {
      if( *c != '\n' || lc != '\r' ) d++;
      *d = ( lc = *c );
   }
   *(++d) = 0;

/* Now further clean it by replacing "\r" by "\n". */
   c = result - 1;
   while( *(++c) ) {
      if( *c == '\r' ) *c = '\n';
   }

/* Return the result. */
   return result;
}

static void CleanXml( AstXmlObject *this, long int type, int *status ){
/*
*  Name:
*     CleanXml

*  Purpose:
*     Free the resources used within an XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void CleanXml( AstXmlObject *this, long int type, int *status )

*  Description:
*     This function frees the resources used internally within the
*     supplied XmlObject.

*  Parameters:
*     this
*        pointer to the XmlObject to be cleaned.
*     type
*        The type of XmlObject being cleaned.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Local Variables: */
   AstXmlAttribute *attr;
   AstXmlBlack *black;
   AstXmlCDataSection *cdatasec;
   AstXmlComment *comm;
   AstXmlDTDec *dtd;
   AstXmlDeclPI *dec;
   AstXmlDocument *doc;
   AstXmlElement *elem;
   AstXmlNamespace *ns;
   AstXmlPI *pi;
   AstXmlPrologue *pro;
   AstXmlWhite *white;

/* Return if a NULL pointer has been suppplied. */
   if( !this ) return;

/* For the base XmlObject class, clear the object type, etc. */
   if( type == AST__XMLOBJECT  ){
      this->type = AST__XMLBAD;
      this->parent = NULL;

/* For each derived class of XmlObject, first clean the parent component,
   then clean any further resources. */
   } else if( type == AST__XMLELEM  ){

      elem = (AstXmlElement *) this;

      elem->name = astFree( elem->name );
      elem->defns = astFree( elem->defns );
      elem->prefix = astFree( elem->prefix );

      while( elem->nattr > 0 ) astXmlDelete( elem->attrs[ 0 ] );
      elem->attrs = astFree( elem->attrs );

      while( elem->nitem > 0 ) astXmlDelete( elem->items[ 0 ] );
      elem->items = astFree( elem->items );

      while( elem->nnspref > 0 ) astXmlDelete( elem->nsprefs[ 0 ] );
      elem->nsprefs = astFree( elem->nsprefs );

      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLATTR ){
      attr = (AstXmlAttribute *) this;
      attr->name = astFree( attr->name );
      attr->value = astFree( attr->value );
      attr->prefix = astFree( attr->prefix );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLBLACK ){
      black = (AstXmlBlack *) this;
      black->text = astFree( black->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLWHITE ){
      white = (AstXmlWhite *) this;
      white->text = astFree( white->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLCDATA ){
      cdatasec = (AstXmlCDataSection *) this;
      cdatasec->text = astFree( cdatasec->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLCOM ){
      comm = (AstXmlComment *) this;
      comm->text = astFree( comm->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLPI ){
      pi = (AstXmlPI *) this;
      pi->target = astFree( pi->target );
      pi->text = astFree( pi->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLNAME ){
      ns = (AstXmlNamespace *) this;
      ns->prefix = astFree( ns->prefix );
      ns->uri = astFree( ns->uri );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLDOC ){
      doc = (AstXmlDocument *) this;
      doc->prolog = astXmlDelete( doc->prolog );
      doc->root = astXmlDelete( doc->root );
      while( doc->nepi > 0 ) astXmlDelete( doc->epilog[ 0 ] );
      doc->epilog = astFree( doc->epilog );
      doc->current = NULL;
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLPRO ){
      pro = (AstXmlPrologue *) this;
      pro->xmldecl = astXmlDelete( pro->xmldecl );
      while( pro->nmisc1 > 0 ) astXmlDelete( pro->misc1[ 0 ] );
      pro->misc1 = astFree( pro->misc1 );
      pro->dtdec = astXmlDelete( pro->dtdec );
      while( pro->nmisc2 > 0 ) astXmlDelete( pro->misc2[ 0 ] );
      pro->misc2 = astFree( pro->misc2 );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLDEC ){
      dec = (AstXmlDeclPI *) this;
      dec->text = astFree( dec->text );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( type == AST__XMLDTD ){
      dtd = (AstXmlDTDec *) this;
      dtd->name = astFree( dtd->name );
      dtd->external = astFree( dtd->external );
      dtd->internal = astFree( dtd->internal );
      CleanXml( this, AST__XMLOBJECT, status );

   } else if( astOK ) {
      astError( AST__INTER, "CleanXml: Invalid object type (%ld) supplied "
                "(internal AST programming error).", status, type );
   }

}

static const char *DefaultURI( AstXmlElement *elem, int *status ){
/*
*  Name:
*     DefaultURI

*  Purpose:
*     Find the URI associated with the default namespace.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *DefaultURI( AstXmlElement *elem, int *status )

*  Description:
*     This function returns the default namespace URI defined within the
*     given element. If the element does not define a default namespace URI,
*     then this function is called recursively on the parent element. If
*     there is no parent element, NULL is returned.

*  Parameters:
*     elem
*        The pointer to the XmlElement.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a string holding the URI, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlParent *parent;        /* Parent of "this" */
   const char *result;          /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status, and the supplied element. */
   if( !astOK || !elem ) return result;

/* If the supplied element defines a default namespace URI, return it.
   Otherwise, call this function to get the default namespace URI from the
   parent element. */
   result = elem->defns;
   if( !result ) {
      parent = ( (AstXmlObject *) elem )->parent;
      if( astXmlCheckType( parent, AST__XMLELEM ) ) {
         result = DefaultURI( (AstXmlElement *) parent, status );
      }
   }

/* If the element has a blank default namespace URI, then return NULL
   since the XML namespaces specification says that "The default
   namespace can be set to the empty string. This has the same effect,
   within the scope of the declaration, of there being no default
   namespace". */
   if( result && astChrLen( result ) == 0 ) result = NULL;

/* Return the result. */
   return result;
}

void *astXmlDelete_( void *obj_ptr, int *status ){
/*
*+
*  Name:
*     astXmlDelete

*  Purpose:
*     Remove the supplied XmlObject from its parent and delete it.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void *astXmlDelete( void *obj )

*  Description:
*     This function removes the supplied XmlObject from its parent and
*     deletes it using astXmlAnnul.

*  Parameters:
*     obj
*        The pointer to the XmlObject to be deleted.

*  Returned Value:
*     NULL

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Local Variables: */
   AstXmlDocument *doc;         /* Pointer to XM document */
   AstXmlElement *elem;         /* Pointer to XML element */
   AstXmlObject *obj;           /* Pointer to XmlObject */
   AstXmlParent *parent;        /* Pointer to parent */
   AstXmlPrologue *pro;         /* Pointer to XML prologue */
   int i;                       /* Loop counter */
   int j;                       /* Loop counter */
   int n;                       /* Number of values in list */
   int ok;                      /* Is obj a child of its parent? */
   void *result;                /* Returned pointer */

/* Initialise */
   result = NULL;
   ok = 0;

/* Check we have an XmlObject. */
   if( !astXmlCheckType( obj_ptr, AST__XMLOBJECT ) ) return result;

/* Get the parent of the supplied object. */
   obj = (AstXmlObject *) obj_ptr;
   parent = obj->parent;
   if( parent ) {

/* First deal with cases where we are deleting items from a document. */
      if( astXmlCheckType( parent, AST__XMLDOC ) ) {
         doc = (AstXmlDocument *) parent;

         if( astXmlCheckType( obj, AST__XMLPRO ) ) {
            if( (AstXmlPrologue *) obj == doc->prolog ) {
               doc->prolog = NULL;
               ok = 1;
            }

         } else if( astXmlCheckType( obj, AST__XMLELEM ) ) {
            if( (AstXmlElement *) obj == doc->root ) {
               doc->root = NULL;
               ok = 1;
            }

         } else if( astXmlCheckType( obj, AST__XMLMISC ) ) {
            n = doc->nepi;
            for( i = 0; i < n; i++ ) {
               if( doc->epilog[ i ] == (AstXmlMiscItem *) obj ) {
                  for( j = i + 1; j < n; j++ ) {
                     doc->epilog[ j - 1 ] =  doc->epilog[ j ];
                  }
                  doc->epilog[ --doc->nepi ] = NULL;
                  ok = 1;
                  break;
               }
            }

         } else if( astOK ) {
            astError( AST__INTER, "astXmlDelete(xml): XmlObject of type %ld has "
                      "inappropriate parent of type %ld (internal AST "
                      "programming error).", status, obj->type, parent->type );
         }

/* Now deal with cases where we are deleting items from a prologue. */
      } else if( astXmlCheckType( parent, AST__XMLPRO ) ) {
         pro = (AstXmlPrologue *) parent;

         if( astXmlCheckType( obj, AST__XMLDEC ) ) {
            if( (AstXmlDeclPI *) obj == pro->xmldecl ) {
               pro->xmldecl = NULL;
               ok = 1;
            }

         } else if( astXmlCheckType( obj, AST__XMLDTD ) ) {
            if( (AstXmlDTDec *) obj == pro->dtdec ) {
               pro->dtdec = NULL;
               ok = 1;
            }

         } else if( astXmlCheckType( obj, AST__XMLMISC ) ) {
            n = pro->nmisc1;
            for( i = 0; i < n; i++ ) {
               if( pro->misc1[ i ] == (AstXmlMiscItem *) obj ) {
                  for( j = i + 1; j < n; j++ ) {
                     pro->misc1[ j - 1 ] =  pro->misc1[ j ];
                  }
                  pro->misc1[ --pro->nmisc1 ] = NULL;
                  ok = 1;
                  break;
               }
            }

            if( !ok ) {
               n = pro->nmisc2;
               for( i = 0; i < n; i++ ) {
                  if( pro->misc2[ i ] == (AstXmlMiscItem *) obj ) {
                     for( j = i + 1; j < n; j++ ) {
                        pro->misc2[ j - 1 ] =  pro->misc2[ j ];
                     }
                     pro->misc2[ --pro->nmisc2 ] = NULL;
                     ok = 1;
                     break;
                  }
               }
            }

         } else if( astOK ) {
            astError( AST__INTER, "astXmlDelete(xml): XmlObject of type %ld has "
                      "inappropriate parent of type %ld (internal AST "
                      "programming error).", status, obj->type, parent->type );
         }

/* Now deal with cases where we are deleting items from an element. */
      } else if( astXmlCheckType( parent, AST__XMLELEM ) ) {
         elem = (AstXmlElement *) parent;

/* Remove the object form the appropriate list in the parent, and
   then shuffle down the remaining entries in the list and decrement the
   size of the list. */
         if( astXmlCheckType( obj, AST__XMLATTR ) ) {
            n = elem->nattr;
            for( i = 0; i < n; i++ ) {
               if( elem->attrs[ i ] == (AstXmlAttribute *) obj ) {
                  for( j = i + 1; j < n; j++ ) {
                     elem->attrs[ j - 1 ] =  elem->attrs[ j ];
                  }
                  elem->attrs[ --elem->nattr ] = NULL;
                  ok = 1;
                  break;
               }
            }

         } else if( astXmlCheckType( obj, AST__XMLNAME ) ) {
            n = elem->nnspref;
            for( i = 0; i < n; i++ ) {
               if( elem->nsprefs[ i ] == (AstXmlNamespace *) obj ) {
                  for( j = i + 1; j < n; j++ ) {
                     elem->nsprefs[ j - 1 ] =  elem->nsprefs[ j ];
                  }
                  elem->nsprefs[ --elem->nnspref ] = NULL;
                  ok = 1;
                  break;
               }
            }

         } else if( astXmlCheckType( obj, AST__XMLCONT ) ) {
            n = elem->nitem;
            for( i = 0; i < n; i++ ) {
               if( elem->items[ i ] == (AstXmlContentItem *) obj ) {
                  for( j = i + 1; j < n; j++ ) {
                     elem->items[ j - 1 ] =  elem->items[ j ];
                  }
                  elem->items[ --elem->nitem ] = NULL;
                  ok = 1;
                  break;
               }
            }
         }

      } else if( astOK ) {
         astError( AST__INTER, "astXmlDelete(xml): XmlObject of type %ld has "
                   "inappropriate parent of type %ld (internal AST "
                   "programming error).", status, obj->type, parent->type );
      }

/* Nullify the parent pointer so that astXmlAnnul will delete the object. */
      obj->parent = NULL;

/* If the supplied object has no parent, we can continue to annul it. */
   } else {
      ok = 1;
   }

/* Report an error if required. */
   if( !ok && astOK ) {
      astError( AST__INTER, "astXmlDelete(xml): Supplied XmlObject (type %ld) "
                "is not owned by its own parent (internal AST "
                "programming error).", status, obj->type );
   }

/* Delete the object. */
   result = astXmlAnnul( obj );

/* Annul the object and return the resulting NULL pointer. */
   return result;
}

static AstXmlAttribute *FindAttribute( AstXmlElement *this, const char *name0,
                                       int *status ){
/*
*  Name:
*     FindAttribute

*  Purpose:
*     Search an XmlElement for a named attribute

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlAttribute *FindAttribute( AstXmlElement *this, const char *name0,
*                                     int *status )

*  Description:
*     This function searches the supplied XmlElement for an attribute
*     with the given name. If found, a pointer to the XmlAttribute is
*     returned. Otherwise NULL is returned.

*  Parameters:
*     this
*        The pointer to the XmlElement.
*     name0
*        Pointer to a string holding the name of the attribute. The name
*        may be preceeded with a "prefix:" string, in which case the
*        prefix will also be matched. If no prefix is included, the first
*        attribute with the specified name is returned, regardless of
*        its prefix.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the XmlAttribute, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlAttribute *result;     /* Returned pointer */
   char name_buffer[ 50 ];      /* Buffer for name */
   char prefix_buffer[ 50 ];    /* Buffer for prefix */
   const char *colon;           /* Pointer to colon in supplied string */
   const char *name1;           /* Pointer to name to be checked */
   const char *name;            /* Pointer to name to be searched for */
   const char *prefix1;         /* Pointer to prefix to be checked */
   const char *prefix;          /* Pointer to prefix to be searched for */
   int i;                       /* Loop count */
   size_t len;                  /* Length of string */

/* Initialise */
   result = NULL;
   name = name0;
   prefix = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If the supplied name string contains a colon, split it up into prefix
   and name. */
   if( ( colon = strchr( name0, ':' ) ) ) {
      len = colon - name0;

      if( len > 49 ) {
         astError( AST__XMLNM, "FindAttribute: The XML prefix in \"%s\" "
                   "is too long (> 49 characters).", status, name0 );
      } else {
         strncpy( prefix_buffer, name0, len );
         prefix_buffer[ len ] = 0;
         prefix = prefix_buffer;
         len = strlen( colon + 1 );

         if( len > 49 ) {
            astError( AST__XMLNM, "FindAttribute: The XML attribute name "
                      "in \"%s\" is too long (> 49 characters).", status, name0 );
         } else {
            strcpy( name_buffer, colon + 1 );
            name = name_buffer;
         }

      }

   }

/* Loop round all the attributes in the element. */
   for( i = 0; i < this->nattr; i++ ) {
      name1 = this->attrs[ i ]->name;
      prefix1 = this->attrs[ i ]->prefix;

/* Compare the attribute name (and prefix) with the supplied name (and
   prefix). Leave the loop if they match. */
      if( !strcmp( name1, name ) &&
          ( !prefix || ( prefix1 && !strcmp( prefix1, prefix ) ) ) ) {
         result = this->attrs[ i ];
         break;
      }
   }

/* Return the result. */
   return result;
}

static const char *Format( AstXmlObject *this, int ind, int *status ){
/*
*  Name:
*     Format

*  Purpose:
*     Converts an XmlObject into a character string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *Format( AstXmlObject *this, int ind, int *status )

*  Description:
*     This function returns a pointer to a dynamically allocated string
*     containing a textual representation of the supplied XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject to format.
*     ind
*        If the XmlObject is an element, then each content item within
*        the element will be prefixed by a string containing "ind" spaces
*        (indenting the returned element itself is the responsibility of
*        the caller and so "this" is not itself indented within this function).
*        In addition, a newline character will be included at the start
*        of the prefix if required, to ensure that each new item starts
*        on a new line. If "ind" is less than zero, then no prefixes are
*        added.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a null terminated string holding the formated XmlObject.
*     This string should be freed when no longer needed using astFree.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlPrologue *pro;      /* Pointer to XML prologue */
   AstXmlDocument *doc;      /* Pointer to XML document */
   AstXmlAttribute *attrib;  /* Pointer to XML attribute */
   AstXmlWhite *white;       /* Pointer to character data */
   AstXmlBlack *black;       /* Pointer to character data */
   AstXmlElement *elem;      /* Pointer to XML element */
   AstXmlNamespace *ns;      /* Pointer to XML namespace instruction */
   char *result;             /* The returned pointer */
   const char *temp;         /* A temporary string pointer */
   int i;                    /* Loop count */
   int nc;                   /* Length of returned string */
   int type;                 /* Object type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK || !this ) return result;

/* Get the object type */
   type = this->type;

/* If this is an element... */
   if( this->type == AST__XMLELEM ) {
      temp = FormatTag( this, 1, status );
      result = astAppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

      elem = (AstXmlElement *) this;
      if( elem->nitem > 0 ) {

/* Go round all the items of content. */
         for( i = 0; i < elem->nitem; i++ ) {

/* Ignore whitespace elements unless we are not producing indentation. */
            if( !astXmlCheckType( elem->items[ i ], AST__XMLWHITE ) ||
                ind < 0 ) {

/* Format the item */
               temp = Format( (AstXmlObject *) elem->items[ i ], ( ( ind > -1 ) ? ind + IND_INC : -1 ), status );
               if( temp ) {

/* Now append the next item of content, and free its memory. */
                  if( ind > -1 ) {
                     result = AppendLine( result, &nc, temp,
                                          ( (ind > -1) ? ind + IND_INC : -1 ), status );
                  } else {
                     result = astAppendString( result, &nc, temp );
                  }
                  temp = astFree( (void *) temp );
               }
            }
         }

/* Finally append the end tag. */
         temp = FormatTag( this, 0, status );
         if( ind > -1 ) {
            result = AppendLine( result, &nc, temp, ind, status );
         } else {
            result = astAppendString( result, &nc, temp );
         }
         temp = astFree( (void *) temp );

      }

/* If this is an attribute... */
   } else if( type == AST__XMLATTR ){
      attrib = (AstXmlAttribute *) this;

      if( attrib->prefix ) {
         result = astAppendString( result, &nc, attrib->prefix );
         result = astAppendString( result, &nc, ":" );
      }

      temp = AddEscapes( attrib->value, status );
      result = astAppendString( result, &nc, attrib->name );
      result = astAppendString( result, &nc, "=\"" );
      result = astAppendString( result, &nc, temp );
      result = astAppendString( result, &nc, "\"" );
      temp = astFree( (void *) temp );

   } else if( type == AST__XMLWHITE ){
      white = (AstXmlWhite *) this;
      temp = AddEscapes( white->text, status );
      result = astAppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

   } else if( type == AST__XMLBLACK ){
      black = (AstXmlBlack *) this;
      temp = AddEscapes( black->text, status );
      result = astAppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

   } else if( type == AST__XMLCDATA ||
              type == AST__XMLCOM ||
              type == AST__XMLPI ||
              type == AST__XMLDEC ||
              type == AST__XMLDTD ){

      temp = FormatTag( this, 1, status );
      result = astAppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

   } else if( type == AST__XMLNAME ){
      ns = (AstXmlNamespace *) this;
      result = astAppendString( result, &nc, "xmlns:" );
      result = astAppendString( result, &nc, ns->prefix );
      result = astAppendString( result, &nc, "=\"" );
      result = astAppendString( result, &nc, ns->uri );
      result = astAppendString( result, &nc, "\"" );

   } else if( type == AST__XMLPRO ){
      pro = (AstXmlPrologue *) this;
      result = astAppendString( result, &nc,
                             Format( (AstXmlObject *) pro->xmldecl, ind, status ) );

/* Append all the miscalleneous items before the DTD. */
      for( i = 0; i < pro->nmisc1; i++ ) {
         temp = Format( (AstXmlObject *) pro->misc1[ i ], ind, status );
         if( temp ) {
            if( ind > -1 ) {
               result = AppendLine( result, &nc, temp, ind, status );
            } else {
               result = astAppendString( result, &nc, temp );
            }
            temp = astFree( (void *) temp );
         }
      }

/* Append the DTD. */
      temp = Format( (AstXmlObject *) pro->dtdec, ind, status );
      if( temp ) {
         if( ind > -1 ) {
            result = AppendLine( result, &nc, temp, ind, status );
         } else {
            result = astAppendString( result, &nc, temp );
         }
         temp = astFree( (void *) temp );
      }

/* Append all the miscalleneous items after the DTD. */
      for( i = 0; i < pro->nmisc2; i++ ) {
         temp = Format( (AstXmlObject *) pro->misc2[ i ], ind, status );
         if( temp ) {
            if( ind > -1 ) {
               result = AppendLine( result, &nc, temp, ind, status );
            } else {
               result = astAppendString( result, &nc, temp );
            }
            temp = astFree( (void *) temp );
         }
      }

   } else if( type == AST__XMLDOC ){
      doc = (AstXmlDocument *) this;

/* Format the prologue. */
      result = astAppendString( result, &nc,
                             Format( (AstXmlObject *) doc->prolog, ind, status ) );

/* Append the root element. */
      temp = Format( (AstXmlObject *) doc->root, ind, status );
      if( temp ) {
         if( ind > -1 ) {
            result = AppendLine( result, &nc, temp, ind, status );
         } else {
            result = astAppendString( result, &nc, temp );
         }
         temp = astFree( (void *) temp );
      }

/* Append all the miscalleneous items in the epilogue. */
      for( i = 0; i < doc->nepi; i++ ) {
         temp = Format( (AstXmlObject *) doc->epilog[ i ], ind, status );
         if( temp ) {
            if( ind > -1 ) {
               result = AppendLine( result, &nc, temp, ind, status );
            } else {
               result = astAppendString( result, &nc, temp );
            }
            temp = astFree( (void *) temp );
         }
      }

   } else if( astOK ) {
      astError( AST__INTER, "Format(xml): Invalid object type (%d) supplied "
                "(internal AST programming error).", status, type );
   }

/* Free the returned string if an error has occurred. */
   if( !astOK ) result = astFree( result );

/* Return the result. */
   return result;
}

static char *FormatTag( AstXmlObject *this, int opening, int *status ){
/*
*  Name:
*     FormatTag

*  Purpose:
*     Returns a string holding an XML tag describing the given XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *FormatTag( AstXmlObject *this, int opening, int *status )

*  Description:
*     This function returns a pointer to a dynamic string containing an
*     XML tag describing the given XmlObject.

*  Parameters:
*     this
*        Pointer to the XmlObject.
*     opening
*        Indicates which tag is to be returned; the start tag or the end
*        tag. If non-zero the start tag is returned. Otherwise, the
*        end tag is returned. If the supplied XmlObject has no end
*        tag (i.e. if it is an empty element, or if it is not an element),
*        then NULL is returned but no error is reported.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a dynamically allocated string holding the tag.

*  Notes:
*     - Empty elements are represented as an start tag of the form <.../>,
*     with no corresponding end tag.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/


/* Local Variables: */
   AstXmlCDataSection *cdata;/* Pointer to XML CDATA section */
   AstXmlElement *elem;      /* Pointer to XML element */
   AstXmlComment *com;       /* Pointer to XML comment */
   AstXmlPI *pi;             /* Pointer to XML processing instruction */
   AstXmlDTDec *dtd;         /* Pointer to XML data type declaration */
   AstXmlDeclPI *xmlpi;      /* XML version declaration */
   char *result;             /* The returned pointer */
   const char *temp;         /* A temporary string pointer */
   int i;                    /* Loop count */
   int nc;                   /* Length of returned string */
   int type;                 /* Object type */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Get the object type */
   type = this->type;

/* If this is an element... */
   if( this->type == AST__XMLELEM ) {
      elem = (AstXmlElement *) this;

      if( opening ) {
         result = astAppendString( result, &nc, "<" );
         if( elem->prefix ) {
            result = astAppendString( result, &nc, elem->prefix );
            result = astAppendString( result, &nc, ":" );
         }
         result = astAppendString( result, &nc, elem->name );

         if( elem->defns ) {
            result = astAppendString( result, &nc, " xmlns=\"" );
            result = astAppendString( result, &nc, elem->defns );
            result = astAppendString( result, &nc, "\"" );
         }

         for( i = 0; i < elem->nnspref; i++ ) {
            temp = Format( (AstXmlObject *) elem->nsprefs[ i ], -1, status );
            if( temp ) {
               result = AppendChar( result, &nc, ' ', status );
               result = astAppendString( result, &nc, temp );
               temp = astFree( (void *) temp );
            }
         }

         for( i = 0; i < elem->nattr; i++ ) {
            temp = Format( (AstXmlObject *) elem->attrs[ i ], -1, status );
            if( temp ){
               result = AppendChar( result, &nc, ' ', status );
               result = astAppendString( result, &nc, temp );
               temp = astFree( (void *) temp );
            }
         }

         if( elem->nitem == 0 ) result = astAppendString( result, &nc, "/" );
         result = astAppendString( result, &nc, ">" );

      } else if( elem->nitem > 0 ) {
         result = astAppendString( result, &nc, "</" );
         if( elem->prefix ) {
            result = astAppendString( result, &nc, elem->prefix );
            result = astAppendString( result, &nc, ":" );
         }
         result = astAppendString( result, &nc, elem->name );
         result = astAppendString( result, &nc, ">" );
      }

   } else if( type == AST__XMLDTD ){
      dtd = (AstXmlDTDec *) this;
      if( opening && dtd->name && dtd->name[0] ) {
         result = astAppendString( result, &nc, "<!DOCTYPE " );
         result = astAppendString( result, &nc, dtd->name );
         if( dtd->external && dtd->external[ 0 ] ) {
            result = astAppendString( result, &nc, " " );
            result = astAppendString( result, &nc, dtd->external );
         }
         if( dtd->internal && dtd->internal[ 0 ] ) {
            result = astAppendString( result, &nc, " [" );
            result = astAppendString( result, &nc, dtd->internal );
            result = astAppendString( result, &nc, "]" );
         }
         result = astAppendString( result, &nc, ">" );
      }

   } else if( type == AST__XMLCDATA ){
      if( opening ) {
         cdata = (AstXmlCDataSection *) this;
         result = astAppendString( result, &nc, "<![CDATA[" );
         result = astAppendString( result, &nc, cdata->text );
         result = astAppendString( result, &nc, "]]>" );
      }

   } else if( type == AST__XMLCOM ){
      if( opening ) {
         com = (AstXmlComment *) this;
         result = astAppendString( result, &nc, "<!--" );
         result = astAppendString( result, &nc, com->text );
         result = astAppendString( result, &nc, "-->" );
      }

   } else if( type == AST__XMLPI ){
      pi = (AstXmlPI *) this;
      if( opening ) {
         result = astAppendString( result, &nc, "<?" );
         result = astAppendString( result, &nc, pi->target );
         if( pi->text && pi->text[0] ) {
            result = astAppendString( result, &nc, " " );
            result = astAppendString( result, &nc, pi->text );
         }
         result = astAppendString( result, &nc, "?>" );
      }

   } else if( type == AST__XMLDEC ){
      xmlpi = (AstXmlDeclPI *) this;
      if( opening && xmlpi->text && xmlpi->text[0] ) {
         result = astAppendString( result, &nc, "<?xml" );
         if( xmlpi->text && xmlpi->text[0] ) {
            result = astAppendString( result, &nc, " " );
            result = astAppendString( result, &nc, xmlpi->text );
         }
         result = astAppendString( result, &nc, "?>" );
      }
   }

/* If notOK, free the rteurned string. */
   if( !astOK ) result = astFree( result );

/* Return the result. */
   return result;
}

static void InitXmlAttribute( AstXmlAttribute *new, int type, const char *name,
                              const char *value, const char *prefix, int *status ){
/*
*  Name:
*     InitXmlAttribute

*  Purpose:
*     Initialise a new XmlAttribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlAttribute( AstXmlAttribute *new, int type, const char *name,
*                       const char *value, const char *prefix, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlAttribute
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     name
*        The name for the attribute.
*     value
*        The value for the attribute
*     prefix
*        The namespace prefix for the attribute. May be NULL or blank, in
*        which case any prefix at the start of "name" is used.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   const char *colon;         /* Pointer to colon within supplied name */
   char *newname;             /* Pointer to name string (no prefix) */
   char *newpref;             /* Pointer to name string */
   int nc;                    /* Length of prefix string */

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLATTR, status ) ){
      astError( AST__INTER, "InitXmlAttribute: Supplied object type (%d) "
                "does not represent an XmlAttribute", status, type );
   }

/* Ensure we have non-NULL pointers. */
   if( !name ) name = "";
   if( !value ) value = "";

/* If no prefix was supplied, extract any prefix from the start of the
   supplied name. */
   newname = (char *) name;
   newpref = (char *) prefix;
   colon = NULL;

   if( !prefix || astChrLen( prefix ) == 0 ){
      colon = strchr( name, ':' );
      if( colon ) {
         nc = colon - name;
         newpref = astStore( NULL, name, nc + 1 );
         newpref[ nc ] = 0;

         nc = strlen( name ) - ( colon - name ) - 1;
         newname = astStore( NULL, colon + 1, nc + 1 );
         newname[ nc ] = 0;
      }
   }

/* Check the supplied name and prefix are valid XML 'names'. */
   CheckName( newname, "attribute", "InitXmlAttribute", 0, status );
   CheckName( newpref, "attribute", "InitXmlAttribute", 1, status );

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Initialise the items specific to this class of structure. */
   new->name = astStore( NULL, newname, strlen( newname ) + 1 );
   new->value = astStore( NULL, value, strlen( value ) + 1 );
   new->prefix = NULL;
   if( newpref ) {
      nc = strlen( newpref );
      if( nc > 0 ) new->prefix = astStore( NULL, newpref, nc + 1 );
   }

/* Free any name and prefix extracted from the supplied name string */
   if( colon ) {
      newname = astFree( newname );
      newpref = astFree( newpref );
   }
}

static void InitXmlCDataSection( AstXmlCDataSection *new, int type,
                                 const char *text, int *status ){
/*
*  Name:
*     InitXmlCDataSection

*  Purpose:
*     Initialise a new XmlCDataSection.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlCDataSection( AstXmlCDataSection *new, int type,
*                          const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlCDataSection
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCDATA, status ) ){
      astError( AST__INTER, "InitXmlCDataSection: Supplied object type (%d) "
                "does not represent an XmlCDataSection", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. */
   new->text = astStore( NULL, text, strlen( text ) + 1 );
}

static void InitXmlWhite( AstXmlWhite *new, int type, const char *text, int *status ){
/*
*  Name:
*     InitXmlWhite

*  Purpose:
*     Initialise a new XmlWhite.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlWhite( AstXmlWhite *new, int type, const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlWhite
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables:  */
   const char *c;          /* Pointer to next character */

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLWHITE, status ) ){
      astError( AST__INTER, "InitXmlWhite: Supplied object type (%d) "
                "does not represent an XmlWhite", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Report an error if the text is not white. */
   c = text - 1;
   while( *(++c) ) {
      if( !isspace( *c ) ) {
         astError( AST__XMLCM, "InitXmlWhite(xml): Illegal XML whitespace "
                   "string supplied \"%s\" - not all characters are white.", status,
                    text );
         break;
      }
   }

/* Initialise the items specific to this class of structure. */
   new->text = astStore( NULL, text, strlen( text ) + 1 );
}

static void InitXmlBlack( AstXmlBlack *new, int type, const char *text, int *status ){
/*
*  Name:
*     InitXmlBlack

*  Purpose:
*     Initialise a new XmlBlack.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlBlack( AstXmlBlack *new, int type, const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlBlack
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLBLACK, status ) ){
      astError( AST__INTER, "InitXmlBlack: Supplied object type (%d) "
                "does not represent an XmlBlack", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. */
   new->text = astStore( NULL, text, strlen( text ) + 1 );
}

static void InitXmlComment( AstXmlComment *new, int type, const char *text, int *status ){
/*
*  Name:
*     InitXmlComment

*  Purpose:
*     Initialise a new XmlComment.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlComment( AstXmlComment *new, int type, const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlComment
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCOM, status ) ){
      astError( AST__INTER, "InitXmlComment: Supplied object type (%d) "
                "does not represent an XmlComment", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. Report an error
   if the comment is illegal. */
   if( strstr( text, "--" ) && astOK ) {
      astError( AST__XMLCM, "InitXmlCom(xml): Illegal XML comment "
                "supplied \"%s\" - comments may not contain the "
                "string \"--\".", status, text );
      new->text = NULL;
   } else {
      new->text = astStore( NULL, text, strlen( text ) + 1 );
   }
}

static void InitXmlDeclPI( AstXmlDeclPI *new, int type, const char *text, int *status ){
/*
*  Name:
*     InitXmlDeclPI

*  Purpose:
*     Initialise a new XmlDeclPI.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlDeclPI( AstXmlDeclPI *new, int type, const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlDeclPI
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLDEC, status ) ){
      astError( AST__INTER, "InitXmlDeclPI: Supplied object type (%d) "
                "does not represent an XmlDeclPI", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. */
   new->text = astStore( NULL, text, strlen( text ) + 1 );
}

static void InitXmlDocument( AstXmlDocument *new, int type, int *status ){
/*
*  Name:
*     InitXmlDocument

*  Purpose:
*     Initialise a new XmlDocument.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlDocument( AstXmlDocument *new, int type, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlDocument
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLDOC, status ) ){
      astError( AST__INTER, "InitXmlDocument: Supplied object type (%d) "
                "does not represent an XmlDocument", status, type );
   }

/* Initialise the parent XmlObject */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Initialise the items specific to this class of structure. */
   new->prolog = NULL;
   new->root = NULL;
   new->epilog = NULL;
   new->nepi = 0;
   new->current = NULL;
}

static void InitXmlDTDec( AstXmlDTDec *new, int type, const char *name,
                          const char *external, const char *internal, int *status ){
/*
*  Name:
*     InitXmlDTDec

*  Purpose:
*     Initialise a new XmlDTDec.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void InitXmlDTDec( AstXmlDTDec *new, int type, const char *name,
*                        const char *external, const char *internal, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlDTDec
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     name
*        The document type name
*     external
*        The external SYSTEM id.
*     internal
*        The internal declaration markup text (this is not checked or
*        parsed).
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLDTD, status ) ){
      astError( AST__INTER, "InitXmlDTDec: Supplied object type (%d) "
                "does not represent an XmlDTDec", status, type );
   }

/* Initialise the parent XmlObject */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !name ) name = "";
   if( !external ) external = "";
   if( !internal ) internal = "";

/* Initialise the items specific to this class of structure. */
   new->name = astStore( NULL, name, strlen( name ) + 1 );
   new->external = astStore( NULL, external, strlen( external ) + 1 );
   new->internal = astStore( NULL, internal, strlen( internal ) + 1 );
}

static void InitXmlElement( AstXmlElement *new, int type, const char *name,
                            const char *prefix, int *status ){
/*
*  Name:
*     InitXmlElement

*  Purpose:
*     Initialise a new XmlElement.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlElement( AstXmlElement *new, int type, const char *name,
*                     const char *prefix, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlElement
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     name
*        The name for the element.
*     prefix
*        The namespace prefix for the element. May be NULL or blank, in
*        which case any prefix at the start of "name" is used.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   const char *colon;         /* Pointer to colon within supplied name */
   char *newname;             /* Pointer to name string (no prefix) */
   char *newpref;             /* Pointer to name string */
   int nc;                    /* Length of prefix string */

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLELEM, status ) ){
      astError( AST__INTER, "InitXmlElement: Supplied object type (%d) "
                "does not represent an XmlElement", status, type );
   }

/* Ensure we have non-NULL pointers. */
   if( !name ) name = "";

/* If no prefix was supplied, extract any prefix from the start of the
   supplied name. */
   newname = (char *) name;
   newpref = (char *) prefix;
   colon = NULL;

   if( !prefix || astChrLen( prefix ) == 0 ){
      colon = strchr( name, ':' );
      if( colon ) {
         nc = colon - name;
         newpref = astStore( NULL, name, nc + 1 );
         newpref[ nc ] = 0;

         nc = strlen( name ) - ( colon - name ) - 1;
         newname = astStore( NULL, colon + 1, nc + 1 );
         newname[ nc ] = 0;
      }
   }

/* Check the supplied name and prefix are valid XML 'names'. */
   CheckName( newname, "element", "InitXmlElement", 0, status );
   CheckName( newpref, "element", "InitXmlElement", 1, status );

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Initialise the items specific to this class of structure. */
   new->name = astStore( NULL, newname, strlen( newname ) + 1 );
   new->attrs = NULL;
   new->nattr = 0;
   new->items = NULL;
   new->nitem = 0;
   new->defns = NULL;
   new->nsprefs = NULL;
   new->nnspref = 0;
   new->complete = 0;

   new->prefix = NULL;
   if( newpref ) {
      nc = strlen( newpref );
      if( nc > 0 ) new->prefix = astStore( NULL, newpref, nc + 1 );
   }

/* Free any name and prefix extracted from the supplied name string */
   if( colon ) {
      newname = astFree( newname );
      newpref = astFree( newpref );
   }
}

static void InitXmlNamespace( AstXmlNamespace *new, int type, const char *prefix,
                              const char *uri, int *status ){
/*
*  Name:
*     InitXmlNamespace

*  Purpose:
*     Initialise a new XmlNamespace.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlNamespace( AstXmlNamespace *new, int type, const char *prefix,
*                       const char *uri, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlNamespace
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     prefix
*        Pointer to a null terminated string holding the namespace prefix.
*     uri
*        Pointer to a null terminated string holding the namespace URI.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLNAME, status ) ){
      astError( AST__INTER, "InitXmlNamespace: Supplied object type (%d) "
                "does not represent an XmlNamespace", status, type );
   }

/* Ensure we have non-NULL pointers. */
   if( !prefix ) prefix = "";
   if( !uri ) uri = "";

/* Check the supplied prefix is a valid XML 'name'. */
   CheckName( prefix, "namespace prefix", "InitXmlNamespace", 0, status );

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Initialise the items specific to this class of structure. */
   new->prefix = astStore( NULL, prefix, strlen( prefix ) + 1 );
   new->uri = astStore( NULL, uri, strlen( uri ) + 1 );
}

static void InitXmlObject( AstXmlObject *new, long int type, int *status ){
/*
*  Name:
*     InitXmlObject

*  Purpose:
*     Initialise a new XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlObject( AstXmlObject *new, long int type, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlObject
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */

/* Check the global error status. */
   if( !astOK ) return;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the supplied object type is OK. Report an error if not. */
   if( !CheckType( type, AST__XMLOBJECT, status ) ){
      astError( AST__INTER, "InitXmlObject: Supplied object type (%ld) "
                "is not appropriate for an XmlObject", status, type );
   }

/* This class of structure is the base class for XML objects so it has no
   parent class to be initialised. So just initialise the items specific to
   this class of structure. */
   new->parent = NULL;
   new->type = type;
   new->id = next_id++;

#ifdef DEBUG
/* Add the new XmlObject to the list of all XmlObjects. */
   AddObjectToList( new );
#endif

}

static void InitXmlPI( AstXmlPI *new, int type, const char *target,
                       const char *text, int *status ){
/*
*  Name:
*     InitXmlPI

*  Purpose:
*     Initialise a new XmlPI.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlPI( AstXmlPI *new, int type, const char *target,
*                const char *text, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlPI
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     target
*        Pointer to a null terminated string holding the PI target.
*     text
*        Pointer to a null terminated string holding the PI text.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLPI, status ) ){
      astError( AST__INTER, "InitXmlPI: Supplied object type (%d) "
                "does not represent an XmlPI", status, type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Ensure we have non-NULL pointers. */
   if( !target ) target = "";
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. Report an error
   if anything is illegal. */
   new->target = NULL;
   new->text = NULL;

   if( !Ustrcmp( target, "XML", status ) && astOK ) {
      astError( AST__XMLPT, "InitXmlPI(xml): Illegal XML PI target \"%s\""
                " supplied.", status, target );
   } else {
      new->target = astStore( NULL, target, strlen( target ) + 1 );
      new->text = astStore( NULL, text, strlen( text ) + 1 );
   }
}

static void InitXmlPrologue( AstXmlPrologue *new, int type, int *status ){
/*
*  Name:
*     InitXmlPrologue

*  Purpose:
*     Initialise a new XmlPrologue.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlPrologue( AstXmlPrologue *new, int type, int *status )

*  Description:
*     This function initialises supplied memory to hold an XmlPrologue
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLPRO, status ) ){
      astError( AST__INTER, "InitXmlPrologue: Supplied object type (%d) "
                "does not represent an XmlPrologue", status, type );
   }

/* Initialise the parent XmlObject */
   InitXmlObject( (AstXmlObject *) new, type, status );

/* Initialise the items specific to this class of structure. */
   new->xmldecl = NULL;
   new->misc1 = NULL;
   new->nmisc1 = 0;
   new->dtdec = NULL;
   new->misc2 = NULL;
   new->nmisc2 = 0;
}

static int MatchName( AstXmlElement *this, const char *name, int *status ){
/*
*  Name:
*     MatchName

*  Purpose:
*     Check that an element has a specified name and/or prefix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     int MatchName( AstXmlElement *this, const char *name, int *status )

*  Description:
*     This function checks that an element has a specified name and/or prefix.

*  Parameters:
*     this
*        The XmlElement to check.
*     name
*        The name for the element (may include a namespace prefix).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the supplied element has the supplie dname/prefix. Zero
*     otherwise.

*/


/* Local Variables: */
   const char *colon;         /* Pointer to colon within supplied name */
   char *newname;             /* Pointer to name string (no prefix) */
   char *newpref;             /* Pointer to name string */
   int nc;                    /* Length of prefix string */
   int result;                /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if( !astOK ) return result;

/* Extract any prefix from the start of the supplied name. */
   newpref = NULL;
   newname = (char *) name;
   colon = strchr( name, ':' );
   if( colon ) {
      nc = colon - name;
      newpref = astStore( NULL, name, nc + 1 );
      newpref[ nc ] = 0;

      nc = strlen( name ) - ( colon - name ) - 1;
      newname = astStore( NULL, colon + 1, nc + 1 );
      newname[ nc ] = 0;
   }

/* Compare the prefix. */
   if( newpref && this->prefix ) {
      result = !strcmp( newpref, this->prefix );

   } else if( !newpref && !this->prefix ) {
      result = 1;

   } else {
      result = 0;
   }

/* If the prefixes matches, compare the names */
   if( result ) {
      if( newname && this->name ) {
         result = !strcmp( newname, this->name );

      } else if( !newname && !this->name ) {
         result = 1;

      } else {
         result = 0;
      }
   }

/* Free any name and prefix extracted from the supplied name string */
   if( colon ) {
      newname = astFree( newname );
      newpref = astFree( newpref );
   }

/* Return the result. */
   return result;
}

static AstXmlAttribute *NewAttribute( const char *name, const char *value,
                                      const char *prefix, int *status ){
/*
*  Name:
*     NewAttribute

*  Purpose:
*     Create a new XmlAttribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlAttribute *NewAttribute( const char *name, const char *value,
*                                    const char *prefix, int *status )

*  Description:
*     This function creates a new XmlAttribute structure representing an
*     XML attribute with the given name, value and namespace prefix.

*  Parameters:
*     name
*        Pointer to a null terminated string containing the attribute name.
*     value
*        Pointer to a null terminated string containing the attribute value.
*     prefix
*        Pointer to a null terminated string containing the attribute
*        namespace prefix (may be NULL or blank).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new structure is returned.

*  Notes:
*     - A NULL pointer is returned if the inherited status value
*     indicates an error has occurred on entry, or if this function
*     should fail for any reason.
*/

/* Local Variables: */
   AstXmlAttribute *new;        /* The returned pointer */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Allocate space for the new structure. */
   new = (AstXmlAttribute *) astMalloc( sizeof( AstXmlAttribute ) );

/* Initialise it. */
   InitXmlAttribute( new, AST__XMLATTR, name, value, prefix, status );

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlDelete( new );

/* Return the result. */
   return new;

}

static AstXmlDocument *NewDocument( int *status ){
/*
*  Name:
*     NewDocument

*  Purpose:
*     Create a new empty XmlDocument.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlDocument *NewDocument( int *status )

*  Description:
*     This function creates a new empty XmlDocument structure representing
*     an entire XML Document.

*  Parameters:
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new structure is returned.

*  Notes:
*     - A NULL pointer is returned if the inherited status value
*     indicates an error has occurred on entry, or if this function
*     should fail for any reason.
*/

/* Local Variables: */
   AstXmlDocument *new;        /* The returned pointer */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Allocate space for the new structure. */
   new = (AstXmlDocument *) astMalloc( sizeof( AstXmlDocument ) );

/* Initialise it. */
   InitXmlDocument( new, AST__XMLDOC, status );

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlDelete( new );

/* Return the result. */
   return new;

}

static AstXmlNamespace *NewNamespace( const char *prefix, const char *uri, int *status ){
/*
*  Name:
*     NewNamespace

*  Purpose:
*     Create a new XmlNamespace.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlNamespace *NewNamespace( const char *prefix,
*                                    const char *uri, int *status )

*  Description:
*     This function creates a new XmlNamespace structure representing an
*     XML namespace with the given prefix and uri.

*  Parameters:
*     prefix
*        Pointer to a null terminated string containing the namespace prefix.
*     uri
*        Pointer to a null terminated string containing the associated URI.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new structure is returned.

*  Notes:
*     - A NULL pointer is returned if the inherited status value
*     indicates an error has occurred on entry, or if this function
*     should fail for any reason.
*/

/* Local Variables: */
   AstXmlNamespace *new;        /* The returned pointer */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Allocate space for the new structure. */
   new = (AstXmlNamespace *) astMalloc( sizeof( AstXmlNamespace ) );

/* Initialise it. */
   InitXmlNamespace( new, AST__XMLNAME, prefix, uri, status );

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlDelete( new );

/* Return the result. */
   return new;

}

static AstXmlPrologue *NewPrologue( AstXmlDocument *doc, int *status ){
/*
*  Name:
*     NewPrologue

*  Purpose:
*     Create a new empty XmlPrologue.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlPrologue *NewPrologue( AstXmlDocument *doc, int *status )

*  Description:
*     This function creates a new empty XmlPrologue structure representing
*     an entire prologue.

*  Parameters:
*     doc
*        A pointer to the XmlDocument to add the XmlPrologue to, or NULL.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new structure is returned.

*  Notes:
*     - A NULL pointer is returned if the inherited status value
*     indicates an error has occurred on entry, or if this function
*     should fail for any reason.
*/

/* Local Variables: */
   AstXmlPrologue *new;        /* The returned pointer */

/* Initialise */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Allocate space for the new structure. */
   new = (AstXmlPrologue *) astMalloc( sizeof( AstXmlPrologue ) );

/* Initialise it. */
   InitXmlPrologue( new, AST__XMLPRO, status );

/* Set its parent. */
   ((AstXmlObject *) new )->parent = (AstXmlParent *) doc;

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlDelete( new );

/* Return the result. */
   return new;

}

static char *RemoveEscapes( const char *text, int *status ){
/*
*  Name:
*     RemoveEscapes

*  Purpose:
*     Replaces entity references by corresponding ascii characters.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *RemoveEscapes( const char *text, int *status )

*  Description:
*     This function produces a dynamic copy of the supplied text in which
*     occurrences of XML entity references are replaced by the corresponding
*     ASCII text.

*  Parameters:
*     text
*        A pointer to a text string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated string containing the required
*     copy.

*  Notes:
*     - NULL is returned if this function is called with the global error
*     status set, or if it should fail for any reason.
*/

/* Local Variables: */
   char *d;                  /* Pointer to next returned character */
   char *result;             /* Returned pointer */
   char rc;                  /* Replacement character */
   const char *c;            /* Pointer to next supplied character */
   int nc;                   /* Number of characters to skip */

/* Initialise */
   result = NULL;
   nc = 0;

/* Return if the pointer is NULL or if an error has occurred. */
   if( !astOK || !text ) return result;

/* Allocate memory to hold a copy of the supplied text. */
   result = astMalloc( strlen( text ) + 1 );

/* Check the pointer can be used safely. */
   if( astOK ) {

/* Loop round every character in the supplied text. */
      c = text - 1;
      d = result;
      while( *(++c) ) {

/* If this character marks the start of a entity reference, replace it by
   the corresponding ascii character and shuffle the remaining text down. */
         if( !strncmp( c, "&amp;", 5 ) ) {
            rc = '&';
            nc= 4;

         } else if( !strncmp( c, "&lt;", 4 ) ) {
            rc = '<';
            nc= 3;

         } else if( !strncmp( c, "&gt;", 4 ) ) {
            rc = '>';
            nc= 3;

         } else if( !strncmp( c, "&apos;", 6 ) ) {
            rc = '\'';
            nc= 5;

         } else if( !strncmp( c, "&quot;", 6 ) ) {
            rc = '"';
            nc= 5;

         } else {
            rc = 0;
         }

         if( rc ) {
            *(d++) = rc;
            c += nc;
         } else {
            *(d++) = *c;
         }

      }

/* Terminate the returned string. */
      *d = 0;

/* Reallocate the string to free up any unused space. */
      result = astRealloc( result, d - result + 1 );
   }

/* Return the result. */
   return result;
}

#ifdef DEBUG
static void RemoveObjectFromList( AstXmlObject *obj ){
/*
*  Name:
*     RemoveObjectFromList

*  Purpose:
*     Removes an XmlObject from a static list of all currently active XmlObjects.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void RemoveObjectFromList( AstXmlObject *obj )

*  Description:
*     This function removes the supplied pointer from a static list of
*     pointers, and decrements the number of elements in the list. This list
*     holds pointers to all the XmlObjects which currently exist. If the
*     supplied pointer is not found in the list, this function returns
*     without action.

*  Parameters:
*     this
*        A pointer to the XmlObject.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variavles: */
   int i;
   int ii;

/* Locate the supplied pointer within the list of pointers to all
   currently active XmlObjects. */
   ii = -1;
   for( i = 0; i < nobj; i++ ){
      if( existing_objects[ i ]->id == obj->id ) {
         ii = i;
         break;
      }
   }

/* Check the pointer was found. */
   if( ii != -1 ) {

/* Shuffle all higher index pointers down one in the list in order to fill
   the gap left by removing the supplied pointer. */
      for( ii++; ii < nobj; ii++ ){
         existing_objects[ ii - 1 ] = existing_objects[ ii ];
      }

/* Decrement the number of pointers in the list. */
      nobj--;

/* Nullify the pointer at the end of the list which is no longer used. */
      existing_objects[ nobj ] = NULL;
   }
}
#endif

static const char *ResolvePrefix( const char *prefix, AstXmlElement *elem, int *status ){
/*
*  Name:
*     ResolvePrefix

*  Purpose:
*     Find the URI associated with a namespace prefix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *ResolvePrefix( const char *prefix, AstXmlElement *elem, int *status)

*  Description:
*     This function searches the namespaces defined within the supplied
*     element for a prefix which matches the supplied prefix. If found,
*     it returns a pointer to the URI string associated with the prefix.
*     If not found, it calls this function recursively on the parent
*     element. If there is no parent element, NULL is returned.

*  Parameters:
*     prefix
*        Pointer to a string holding the namespace prefix.
*     elem
*        The pointer to the XmlElement.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a string holding the URI, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlParent *parent;        /* Parent object */
   AstXmlNamespace *ns;         /* Next namespace */
   const char *result;          /* Returned pointer */
   int i;                       /* Loop count */

/* Initialise */
   result = NULL;

/* Check the global error status, and the supplied element. */
   if( !astOK || !elem ) return result;

/* Loop round all the namespace definitions in the element. */
   for( i = 0; i < elem->nnspref; i++ ) {
      ns = elem->nsprefs[ i ];

/* Compare the namespace prefix with the supplied prefix (case sensitive).
   Store a pointer to the associated URI if they match, and leave the
   loop. */
      if( !strcmp( ns->prefix, prefix ) ) {
         result = ns->uri;
         break;
      }
   }

/* If no matching namespace was found, attempt to resolve the prefix
   within the context of the parent element. */
   if( !result ) {
      parent = ((AstXmlObject *) elem )->parent;
      if( astXmlCheckType( parent, AST__XMLELEM ) ) {
         result = ResolvePrefix( prefix, (AstXmlElement *) parent, status );
      }
   }

/* Return the result. */
   return result;
}

static int Ustrcmp( const char *a, const char *b, int *status ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     int Ustrcmp( const char *a, const char *b )

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strcmp" does.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Loop round each character. */
   while( 1 ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

#ifdef DEBUG
int astXmlTrace( int show ){
/*
*+
*  Name:
*     astXmlTrace

*  Purpose:
*     List details of XML objects currently in existence.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     int astXmlTrace( int show )

*  Description:
*     Lists details of XML objects currently in existence. Details are
*     written to standard output.

*  Parameters:
*     show
*        - 0, the ID values of all currently active XmlObjects are
*        listed. The objects themselves are unchanged.
*        - 1, each object is displayed using astXmlShow unless it has
*        already been included in the display of a previous object, and then
*        annulled. Consequently, this mode is destructive, and none of the
*        displayed XmlObjects will be acessable afterwards.
*        - 2, each object is displayed using astXmlShow whether or not it
*        has already been included in the display of a previous object.
*        The objects are left unchanged.
*        - 3, nothing is written to standard output, but the number of
*        active XmlObjects is still returned.

*  Returned Value:
*     The number of XMLObjects which are in existence on entry to this
*     function.

*-
*/

/* Local Variables: */
   AstXmlObject *root;
   int i, result, old_status;

   old_status = astStatus;
   astClearStatus;

   result = nobj;

   if( show == 0 ) {
      printf( "Current list of active XmlObject identifiers: " );
      for( i = 0; i < nobj; i++ ) printf( "%d ", existing_objects[ i ]->id );
      printf("\n");

   } else if( show ==1 ){
      while( nobj > 0 ) {
         root = astXmlGetRoot( existing_objects[0] );
         printf( "ID = %d (type %ld)\n%s\n------------\n",
                 root->id, root->type, astXmlShow( root ) );
         root = astXmlAnnulTree( root );
      }

   } else if( show == 2 ) {
      for( i = 0; i < nobj; i++ ) printf( "%d\n%s\n------------\n",
                                          existing_objects[ i ]->id,
                                          astXmlShow(existing_objects[ i ]) );
      printf("\n");
   }

   astSetStatus( old_status );

   return result;
}
#endif

AstXmlElement *astXmlReadDocument_( AstXmlDocument **doc,
                                   int (*is_wanted)( AstXmlElement *, int * ),
                                   int skip, char (*source)( void *, int * ),
                                   void *data, int *status ){
/*
*+
*  Name:
*     astXmlReadDocument

*  Purpose:
*     Read and parse an XML document.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *astXmlReadDocument( AstXmlDocument **doc,
*                                       int (*is_wanted)( AstXmlElement *, int * ),
*                                       int skip, char (*source)( void *, int * ),
*                                       void *data )

*  Description:
*     This function reads and parses text from an XML source. The text is
*     obtained by calling the supplied "source" function, which returns
*     the next character read from the external source on each invocation.
*
*     The reading scheme combines elements of the SAX and DOM schemes in
*     an attempt to minimise memory requirements (a potential problem with
*     DOM) whilst retaining a simple interface for accessing the XML
*     elements of interest to the client (a potential problem for SAX).
*
*     When an element start tag is encountered in the source, the client
*     is asked to indicate whether the element is of interest. This is
*     done by calling the supplied "is_wanted" function. If the client
*     indicates that the element is of interest, its contents are read
*     and a pointer to a corresponding XmlElement structure is returned.
*     Reading stops when the element has been read. If the client
*     indicates that the element is not of interest, then (if "skip" is
*     non-zero) the contents of the element are skipped over, and reading
*     continues following the element end tag. When the next element is
*     encountered the client will again be asked to indicate its interest
*     in the element. This continues until either the client indicates that
*     an element is of interest, or the end of the source is reached. If
*     "skip" is zero, then an error is reported if the first element in
*     the document is not of interest.
*
*     The client has an option to reply that an element is not itself of
*     interest, but may possibly contain interesting elements. In this case,
*     the sub-elements within the element are read and checked in the same
*     way.
*
*     This function returns, and no more characters are read from the
*     source, once the contents of the first "interesting" element has been
*     read.
*
*     The function thus returns a pointer to an XmlElement containing the
*     entire contents of the first interesting element encountered in the
*     source. This function can then be invoked again to read further
*     interesting elements from the source. In this case, the XmlDocument
*     structure created by the initial invocation (see parameter "doc")
*     must be supplied, as this indicates the point in the total document
*     structure at which the previous "interesting" element was located.

*  Parameters:
*     doc
*        Address of a location holding a pointer to an AstXmlDocument
*        structure. The AstXmlDocument pointer should be supplied as NULL
*        when invoking this function for the first time on a document
*        source (i.e. when reading from the beginning of the document).
*        In this case a new AstXmlDocument structure will be created and a
*        pointer to it stored at the supplied address. This structure
*        holds the context which enables subsequent invocations of this
*        function to determine the point in the document structure at
*        which to store any further text read from the source. It also
*        holds the document prologue, root element, and epilogue.
*     is_wanted
*        Pointer to a function which is called to decide if the client is
*        interested in each element start tag which has just been read.
*        It has a single argument which is a pointer to the (empty) XmlElement
*        corresponding to the element start tag which has just been read.
*        It returns an integer:
*           -1 : the element is not itself of interest but it may contain
*                an interesting element, so look through the content and
*                ask the client again about any elements found inside it.
*            0 : the element definately contains nothing of interest to
*                the client. kip its content and continue looking for new
*                elements.
*            1 : the element is definately of interest to the client so
*                read its contents and return a pointer to it.
*        If NULL is supplied, a value of "+1" is assumed.
*     skip
*        Indicates if any uninteresting elements may proceed the first
*        element of interest. If zero, then an error is reported if the
*        first element read from the source is not of interest to the client.
*        If non-zero, then any uninteresting elements are simply skipped
*        over until an interesting element is found or the document ends.
*     source
*        Pointer to a function which is called to return the next
*        character from the source. It has a single argument which is
*        used to pass any supplied data to it. It should return zero when
*        the end of the source is reached.
*     data
*        Pointer to a structure to pass to the source function. This
*        structure may contain any data needed by the source function.

*  Returned Value:
*     A pointer to the first element of interest, or NULL if there are no
*     interesting elements in the source. The returned element will be a
*     descendant of "*doc". For this reason, the returned pointer need not
*     be annulled explicitly since it will be freed when the XmlDocument
*     is annulled. However, if required (e.g. to save memory) it may be
*     annulled before the document is annulled by using astXmlRemoveItem to
*     remove it from its parent, and then using astXmlAnnul to annul it.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*     - It is assumed that the read commences outside any tag (i.e.
*     in between tags or within character data).
*-
*/

/* Local Variables: */
   AstXmlElement *result;

/* Check any supplied pointer is for an XmlDocument. */
   astXmlCheckDocument( *doc, 1 );

/* Read and parse the source text. Indicate that the element being read
   *may* contain items of interest to the client. Surround with a mutex
   since the supplied functions may not be thread-safe. */
   LOCK_MUTEX1;
   result = ReadContent( doc, -1, is_wanted, skip, source, data, 0, status );
   UNLOCK_MUTEX1;

/* Return the result. */
   return result;
}


static AstXmlElement *ReadContent( AstXmlDocument **doc, int wanted,
                                   int (*is_wanted)( AstXmlElement *, int * ),
                                   int skip, char (*source)( void *, int * ),
                                   void *data, int depth, int *status ){
/*
*  Name:
*     ReadContent

*  Purpose:
*     Read and parse an XML document.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *ReadContent( AstXmlDocument **doc, int wanted,
*                                 int (*is_wanted)( AstXmlElement *, int * ),
*                                 int skip, char (*source)( void *, int * ),
*                                 void *data, int depth, int *status )

*  Description:
*     This function reads and parses text from an XML source. The text is
*     obtained by calling the supplied "source" function, which returns
*     the next character read from the external source on each invocation.
*
*     See astXmlReadDocument for more details.

*  Parameters:
*     doc
*        Address of a location holding a pointer to an AstXmlDocument
*        structure. The AstXmlDocument pointer should be supplied as NULL
*        when invoking this function for the first time on a document
*        source (i.e. when reading from the beginning of the document).
*        In this case a new AstXmlDocument structure will be created and a
*        pointer to it stored at the supplied address. This structure
*        holds the context which enables subsequent invocations of this
*        function to determine the point in the document structure at
*        which to store any further text read from the source. It also
*        holds the document prologue, root element, and epilogue.
*     wanted
*        Indicates if the content read from the XML source is of interest
*        to the client. If a positive value is supplied, all content read
*        from the source (up to the end tag which corresponds to the
*        supplied "parent") is added to the "parent" element (if supplied).
*        If zero is supplied, then all content read from the source is
*        discarded. If a negative value is supplied, then all content up
*        to the first element start tag is discarded. When the first
*        element start tag is encountered, it is passed back to the client
*        by invoking the supplied "is_wanted" function. If this function
*        returns a non-zero value, then the contents of the new element
*        is read (by calling this function recursively) and a pointer to
*        the new element is returned as the function value (reading then
*        stops and the function returns). If the "is_wanted" function returns
*        zero, then the contents of the new element is skipped over, and
*        reading continues until the next element start tag is encountered,
*        when the "is_wanted" function is again invoked.
*     is_wanted
*        Pointer to a function which is called to decide if the client is
*        interested in the element start tag which has just been read.
*        It has a single argument which is a pointer to the (empty) XmlElement
*        corresponding to the element start tag which has just been read.
*        It returns an integer:
*           -1 : the element is not itself of interest but it may contain
*                an interesting element, so look through the content and
*                ask the client again about any elements found inside it.
*            0 : the element definately contains nothing of interest to
*                the client. kip its content and continue looking for new
*                elements.
*            1 : the element is definately of interest to the client so
*                read its contents and return a pointer to it.
*        If NULL is supplied, a value of "+1" is assumed.
*     skip
*        Indicates if any uninteresting elements may proceed the first
*        element of interest. If zero, then an error is reported if the
*        first element read from the source is not of interest to the client.
*        If non-zero, then any uninteresting elements are simply skipped
*        over until an interesting element is found or the document ends.
*     source
*        Pointer to a function which is called to return the next
*        character from the source. It has a single argument which is
*        used to pass any supplied data to it. It should return zero when
*        the end of the source is reached.
*     data
*        Pointer to a structure to pass to the source function. This
*        structure may contain any data needed by the source function.
*     depth
*        Depth of nesting (i.e. zero if this function was invoked from
*        astXmlReadDocument, and a positive value if it was invoked
*        recursively from within itself).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the first element of interest, or NULL if there are no
*     interesting elements in the source. If the first element of
*     interest has already been found (as indicated by "wanted" being +1)
*     then NULL is returned. The returned element may be a child of a
*     parent element containing namespace definitions (which may itself
*     have a parent, etc). For this reason, the returned pointer should be
*     freed using astXmlAnnulTree rather than astXmlAnnul.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*     - It is assumed that the read commences outside any tag (i.e.
*     in between tags or within character data).
*/

/* Local Variables; */
   AstXmlElement *answer;       /* Result of reading a sub-element */
   AstXmlElement *parent;       /* Pointer to current parent element */
   AstXmlElement *elem;         /* A new element to be read */
   AstXmlElement *result;       /* The returned pointer */
   char *cc;                    /* Pointer to next character */
   char *msg;                   /* Pointer to message buffer */
   char *text1;                 /* Pointer to dynamic string */
   char *text2;                 /* Pointer to another dynamic string */
   char *text3;                 /* Pointer to another dynamic string */
   char *text4;                 /* Pointer to another dynamic string */
   char c;                      /* Current character read from source */
   char lc2;                    /* Last but one character read */
   char lc;                     /* Last character read */
   char quoted;                 /* Character which opened current quote */
   int nc1;                     /* No. of characters stored in text1 */
   int nc2;                     /* No. of characters stored in text2 */
   int nc3;                     /* No. of characters stored in text2 */
   int ncmsg;                   /* Length of "msg" */
   int newwanted;               /* Is the new element wanted? */
   int state;                   /* Current action being performed */
   int prolog_ok;               /* OK for source to start with a prolog? */
   int where;                   /* Where to add the item within the document */

/* Initialise */
   result = NULL;
   elem = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If no XmlDocument was supplied, assume we are commencing to read a new
   document from the begining. Create a new XmlDocument to store the
   prologue, root element and epilogue, together with a pointer to the
   "current" element, i.e. the element whose content is currently being
   read. Also, since we have not yet asked the client if it is interested
   in anything, ignore the supplied "wanted" value and use -1 to ensure
   that we ask the client when the first element start tag is encountered. */
   if( !*doc ){
      prolog_ok = 1;
      *doc = NewDocument( status );
      wanted = -1;
   } else {
      prolog_ok = 0;
   }

/* Any content read from the source (except for prologue and epilogue)
   will be placed into the "parent" element. A pointer to this element is
   stored in the XmlDocument structure. The parent element will always be
   a descendant of the root element, or the root element itself. */
   parent = (*doc)->current;

/* If the supplied parent has already been completed (typically because
   it was read from an empty element tag), then just return without
   action. */
   if( parent && parent->complete ) {

/* If an error has occurred, or if this invocation of ReadContent was
   made recursively (rather than by the client), or if we have something
   to return, return. */
      if( !astOK || depth > 0 || result ) {
         return result;

/* Otherwise, returning would result in returning a null pointer to the
   client even though the end of the document may not have been reached.
   Revert to state 0 and search for further interesting elements. */
      } else {
         if( parent != (*doc)->root ) {
            (*doc)->current = (AstXmlElement *) ( (AstXmlObject *) parent )->parent;
         } else {
            (*doc)->current = NULL;
         }
         parent = (*doc)->current;
         state = 0;
      }
   }

/* Initialise the previous two characters read. */
   lc = 0;
   lc2 = 0;

/* Initialise pointer to dynamically allocated strings. */
   text1 = NULL;
   text2 = NULL;
   text3 = NULL;
   msg = NULL;

/* We are not in a quote. */
   quoted = 0;

/* Initialise the "state" variable which indicates what we are currently
   looking for. */
   state = 0;

/* Loop round reading characters from the source. */
   while( 1 ) {
      c = (*source)( data, status );

/* Leave the loop if an error occurred whilst reading the character. */
      if( !astOK ) break;

/* If a parent element has been supplied, (i.e. if we are currently
   reading the content of an element), or if we are not in state zero,
   report an error and leave the loop if the end of the text has been
   reached. If no parent was supplied, just leave the loop. */
      if( !c ) {
         if( parent ) {
            astError( AST__XMLWF, "astRead(XmlChan): End of XML input text "
                      "reached whilst reading the content of element %s.", status,
                       astXmlGetTag( parent, 1 ) );

         } else if( state > 1 ) {
            if( msg ) {
               astError( AST__XMLWF, "astRead(XmlChan): End of XML input text "
                         "reached whilst reading the document epilogue "
                         "(\"%s\").", status, msg );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): End of XML input text "
                         "reached whilst reading the document epilogue." , status);
            }
         }
         break;
      }

/* Save text which is not character data for use in error messages. */
      if( state < 2 ) {
         if( msg ) msg = astFree( msg );
      } else {
         msg = AppendChar( msg, &ncmsg, c, status );
      }

/* State 0: Use the first character to decide what sort of content item
   follows (character data or a tag of some form). */
      if( state == 0 ) {
         if( c != '<' ) {
            state = 1;
            text1 = AppendChar( text1, &nc1, c, status );
         } else {
            msg = AppendChar( msg, &ncmsg, '<', status );
            state = 2;
         }

/* State 1: We are reading character data. The character data ends at the
   first occurrence of "<", at which point the character data is added to
   the parent if required and we continue to state 2.*/
      } else if( state == 1 ) {
         if( c != '<' ) {
            text1 = AppendChar( text1, &nc1, c, status );
         } else {
            msg = AppendChar( msg, &ncmsg, '<', status );
            if( text1 ){

/* If we have a parent element, just add it to the element. */
               if( parent ) {
                  if( wanted > 0 ) {
                     text4 = RemoveEscapes( text1, status );
                     astXmlAddCharData( (AstXmlParent *) parent, 0, text4 );
                     text4 = astFree( text4 );


/* If we are not allowed to skip over non-blank content, report an
   error if the text is not blank. */
                  } else if( !skip ) {
                     cc = text1 - 1;
                     while( *(++cc) ) {
                        if( !isspace( *cc ) ) {
                           if( parent ) {
                              astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                                        "the input data \"%s\" within element %s.", status,
                                        text1, astXmlGetTag( parent, 1 ) );
                           } else {
                              astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                                        "the input data: \"%s\".", status, text1 );
                           }
                           break;
                        }
                     }
                  }

/* Otherwise, add it to the document prologue or epilogue. */
               } else {
                  if( (*doc)->root ) {
                     where = 3;
                  } else if( (*doc)->prolog && (*doc)->prolog->dtdec ){
                     where = 2;
                  } else {
                     where = 1;
                  }

                  text4 = RemoveEscapes( text1, status );
                  astXmlAddCharData( (AstXmlParent *) *doc, where, text4 );
                  text4 = astFree( text4 );
               }

               text1 = astFree( text1 );
            }
            state = 2;
         }

/* State 2: We are using the character following a "<" to determine what
   type of tag is commencing. */
      } else if( state == 2 ) {

/* If the character is a ">", report an error. */
         if( c == '>' ) {
            if( parent ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"<>\" "
                         "encountered within element %s.", status, astXmlGetTag( parent, 1 ) );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"<>\" "
                         "encountered." , status);
            }
            break;

/* If the character is a "?", this must be a PI tag. */
         } else if( c == '?' ) {
            state = 3;

/* If the character is a "!", it must be a comment or a CDATA section
   or a DTD. */
         } else if( c == '!' ) {
            state = 4;

/* If the character is a "/", it must be an element end tag. */
         } else if( c == '/' ) {
            state = 5;

/* Otherwise, this must be an element start tag. Append the character
   to "text1". */
         } else {
            state = 6;
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 3: We are reading the initial text following the opening "<?" string
   of a PI tag. The characters between the initial "<?" string and the first
   space or closing "?>" string is the target text. */
      } else if( state == 3 ) {
         if( c == '>' && lc == '?' ) {
            if( text1 ) text1[ --nc1 ] = 0;
            state = 100;
         } else if( isspace( c ) ) {
            state = 7;
         } else {
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 4: We are using the characters following the opening "<!" text to
   determine if the tag is a comment, DTD or CDATA section. */
      } else if( state == 4 ) {
         if( c == '-' ) {
            state = 8;
         } else if( c == 'D' ){
            state = 16;
            text1 = astAppendString( text1, &nc1, "<!D" );
         } else if( c == '[' ){
            state = 9;
            text1 = astAppendString( text1, &nc1, "<![" );
         } else {
            if( parent ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"<!%c...\" encountered within "
                         "element %s.", status, c, astXmlGetTag( parent, 1 ) );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"<!%c...\" encountered.", status, c );
            }
            break;
         }

/* State 5: We are looking for the end of an element end tag. */
      } else if( state == 5 ) {
         if( c == '>' ) {
            state = 101;
         } else {
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 6: We are looking for the (prefix:)name combination at the start of
   an element start tag. */
      } else if( state == 6 ) {
         if( c == '>' ) {
            state = ( lc != '/' ) ? 102 : 103;
         } else if( isspace( c ) ) {
            state = 104;
         } else if( c != '/' ){
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 7: We are reading the remaining text in a PI tag following the target
   text. */
      } else if( state == 7 ) {
         if( c == '>' && lc == '?' ) {
            if( text2 ) text2[ --nc2 ] = 0;
            state = 100;
         } else if( text2 || !isspace( c ) ) {
            text2 = AppendChar( text2, &nc2, c, status );
         }

/* State 8: We are looking for the start of the text within a comment tag. */
      } else if( state == 8 ) {
         if( c == '-' ) {
            state = 10;
         } else {
            if( parent ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"<!-%c...\" encountered within "
                         "element %s.", status, c, astXmlGetTag( parent, 1 ) );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"<!-%c...\" encountered.", status, c );
            }
            break;
         }

/* State 9: We are looking for the start of the text within a CDATA tag. */
      } else if( state == 9 ) {
         if( c == '[' ) {
            if( !strcmp( text1, "<![CDATA" ) ) {
               state = 11;
               text1 = astFree( text1 );
            } else {
               if( parent ) {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                            "starting with \"%s%c...\" encountered within "
                            "element %s.", status, text1, c, astXmlGetTag( parent, 1 )  );
               } else {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                            "starting with \"%s%c...\" encountered.", status, text1, c );
               }
               text1 = astFree( text1 );
               break;
            }

         } else if( nc1 < 10 ) {
            text1 = AppendChar( text1, &nc1, c, status );

         } else {
            if( parent ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"%s%c...\" encountered within "
                         "element %s.", status, text1, c, astXmlGetTag( parent, 1 )  );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"%s%c...\" encountered.", status, text1, c );
            }
            text1 = astFree( text1 );
            break;
         }

/* State 10: We are reading the remaining text in a comment tag. When the end
   ">" is reached, check the previous 2 characters are "--" and then terminate
   the text1 string in order to remove these two characters from the comment
   text.  */
      } else if( state == 10 ) {
         if( c == '>' && lc == '-' && lc2 == '-' ) {
            text1[ nc1 - 2 ] = 0;
            state = 105;
         } else {
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 11: We are reading the remaining text in a CDATA tag. */
      } else if( state == 11 ) {
         if( c == '>' && lc == ']' && lc2 == ']' ) {
            text1[ nc1 - 2 ] = 0;
            state = 106;
         } else {
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 12: We are looking for an equals sign marking the end of an
   attribute name within an element start tag. */
      } else if( state == 12 ) {
         if( c == '=' ) {
            state = 13;

         } else if( c == '>' ) {
            if( text1 ) {
               if( parent ) {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                            " \"%s...\" encountered within element %s.", status, msg,
                            astXmlGetTag( parent, 1 )  );
               } else {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                            "encountered.", status, msg );
               }
               break;
            } else {
               if( lc == '/' ) {
                  state = 108;
               } else {
                  state = 200;
               }
            }

         } else if( text1 || !isspace( c ) ) {
            if( c != '/' ) text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 13: We are looking for a '"' or ''' marking the start of an attribute
   value within an element start tag. */
      } else if( state == 13 ) {
         if( c == '"' ) {
            state = 14;

         } else if( c == '\'' ) {
            state = 15;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", status, text1, msg );
            break;
         }

/* State 14: We are looking for a '"' marking the end of an attribute value
   within an element start tag. */
      } else if( state == 14 ) {
         if( c == '"' ) {
            state = 107;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", status, text1, msg );
            break;

         } else {
            text2 = AppendChar( text2, &nc2, c, status );
         }

/* State 15: We are looking for a ''' marking the end of an attribute value
   within an element start tag. */
      } else if( state == 15 ) {
         if( c == '\'' ) {
            state = 107;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", status, text1, msg );
            break;

         } else {
            text2 = AppendChar( text2, &nc2, c, status );
         }

/* State 16: We are looking for the end of a DOCTYPE string. */
      } else if( state == 16 ) {
         if( isspace( c ) ) {
            if( !strcmp( text1, "<!DOCTYPE" ) ) {
               state = 17;
               text1 = astFree( text1 );
            } else {
               if( parent ) {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                            "starting with \"%s%c...\" encountered within "
                            "element %s.", status, text1, c, astXmlGetTag( parent, 1 )  );
               } else {
                  astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                            "starting with \"%s%c...\" encountered.", status, text1, c );
               }
               text1 = astFree( text1 );
               break;
            }

         } else if( nc1 < 15 ) {
            text1 = AppendChar( text1, &nc1, c, status );

         } else {
            if( parent ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"%s%c...\" encountered within "
                         "element %s.", status, text1, c, astXmlGetTag( parent, 1 )  );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag "
                         "starting with \"%s%c...\" encountered.", status, text1, c );
            }
            text1 = astFree( text1 );
            break;
         }

/* State 17: We are looking for the start of a DOCTYPE name string. */
      } else if( state == 17 ) {
         if( !isspace( c ) ) {
            text1 = AppendChar( text1, &nc1, c, status );
            state = 18;
         }

/* State 18: We are looking for the end of a DOCTYPE name string. */
      } else if( state == 18 ) {
         if( isspace( c ) ) {
            state = 19;
         } else if( c == '>' ) {
            state = 109;
         } else {
            text1 = AppendChar( text1, &nc1, c, status );
         }

/* State 19: We are looking for the start of a string following a DOCTYPE
   name string. */
      } else if( state == 19 ) {
         if( !isspace( c ) ) {
            if( c == '[' ) {
               state = 20;
            } else if( c == '>' ) {
               state = 109;
            } else {
               state = 21;
               text2 = AppendChar( text2, &nc2, c, status );
            }
         }

/* State 20: We are looking for the "]" marking the end of the internal
   markup of a DOCTYPE element. Avoid the contents of quoted strings (such
   as #FIXED attribute values). */
      } else if( state == 20 ) {
         text3 = AppendChar( text3, &nc3, c, status );
         if( c == '\'' ) {
            if( quoted == '\'' ) {
               quoted = 0;
            } else if( !quoted ) {
               quoted = '\'';
            }

         } else if( c == '"' ) {
            if( quoted == '"' ) {
               quoted = 0;
            } else if( !quoted ) {
               quoted = '"';
            }

         } else if( !quoted && c == ']' ) {
            text3[ --nc3 ] = 0;
            state = 22;
         }

/* State 21: We are looking for the start of a DOCTYPE internal section. */
      } else if( state == 21 ) {
         if( c == '[' ) {
            state = 20;
         } else if( c == '>' ) {
            state = 109;
         } else {
            text2 = AppendChar( text2, &nc2, c, status );
         }

/* State 22: We are looking for the ">" at the end of a DOCTYPE. */
      } else if( state == 22 ) {
         if( !isspace( c ) ) {
            if( c == '>' ) {
               state = 109;
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Extra text found "
                         "at end of XML DOCTYPE tag \"%s\".", status, msg );
            }
         }

      } else {
         astError( AST__INTER, "ReadContent(xml): Illegal state (%d) encountered "
                   "(AST internal programming error).", status, state );
      }

/* The following states perform actions consequent on the decisons made
   above, but which must be performed before reading the next character. */

/* In most cases there will be no actions to perform. Therefore check for
   this first (to avoid the time spent doing all the following usually
   irrelevant checks). */
      if( state < 23 ) {

/* State 100: We have just reached the end of a PI tag. Create a new XmlPI and
   store it in the parent (if required). */
      } else if( state == 100 ) {
         if( text1 ){

/* First deal with XML declaration PI's. These must be the first item in
   the source. */
            if( !strcmp( text1, "xml" ) ) {
               if( (*doc)->root || (*doc)->prolog || (*doc)->nepi > 0 ) {
                  astError( AST__XMLWF, "astRead(XmlChan): An XML "
                            "declaration \"%s\" was encountered within the "
                            "body of the document.", status, msg );
               } else {
                  astXmlSetXmlDec( *doc, text2 );
               }

/* Now deal with other PI's. */
            } else {

/* If we have a parent element, just add it to the element. */
               if( parent ) {
                  if( wanted > 0 ) {
                     astXmlAddPI( (AstXmlParent *) parent, 0, text1, text2 );
                  } else if( !skip ) {
                     if( parent ) {
                        astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                                  "the input data \"%s\" within element %s.", status,
                                  msg, astXmlGetTag( parent, 1 ) );
                     } else {
                        astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                                  "the input data: \"%s\".", status, msg );
                     }
                     break;
                  }

/* Otherwise, add it to the document prologue or epilogue. */
               } else {
                  if( (*doc)->root ) {
                     where = 3;
                  } else if( (*doc)->prolog->dtdec ){
                     where = 2;
                  } else {
                     where = 1;
                  }
                  astXmlAddPI( (AstXmlParent *) *doc, where, text1, text2 );

               }
            }
            text1 = astFree( text1 );
            if( text2 ) text2 = astFree( text2 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }
         state = 0;

/* State 101: We have just reached the end of an element end tag. Check that
   the (prefix:)name is legal, and matches that of the current parent,
   re-instate the parent's parent as the current element in the document,
   and leave the loop if appropriate. */
      } else if( state == 101 ) {
         if( text1 ){
            CheckPrefName( text1, "element", "astRead(XmlChan)", status );
            if( parent ) {
               if( MatchName( parent, text1, status ) ) {
                  parent->complete = 1;
                  if( parent != (*doc)->root ) {
                     (*doc)->current = (AstXmlElement *) ( (AstXmlObject *) parent )->parent;
                  } else {
                     (*doc)->current = NULL;
                  }
               } else {
                  astError( AST__XMLWF, "astRead(XmlChan): Start tag \"%s\" "
                            "closed by end tag \"%s\".", status, astXmlGetTag( parent, 1 ),
                            msg );
               }

            } else {
               (*doc)->current = NULL;
               astError( AST__XMLWF, "astRead(XmlChan): Unmatched end tag "
                         "\"%s\" encountered.", status, msg );
            }

            text1 = astFree( text1 );

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
         }

/* If an error has occurred, or if this invocation of ReadContent was
   made recursively (rather tnan by the client), or if we have something
   to return, break out of the loop. */
         if( !astOK || depth > 0 || result ) {
            break;

/* Otherwise, breaking would result in returning a null pointer to the
   client even though the end of the document may not have been reached.
   Revert to state 0 and search for further intersting elements. */
         } else {
            parent = (*doc)->current;
            state = 0;
         }

/* State 102: We have just got the (prefix:)name for an element start tag, and
   the start tag contains no attributes, etc. Create a new XmlElement, adding
   it to the supplied parent, and then proceed to state 200 to read the
   content of the element. */
      } else if( state == 102 ) {
         if( text1 ){
            elem = astXmlAddElement( parent, text1, NULL );
            text1 = astFree( text1 );
            state = 200;

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }

/* State 103: We have just got the (prefix:)name for an empty element tag, and
   the tag does not contain further attributes, etc. Create a new XmlElement
   and store it in the container (if any). Indicate that there is no
   content to read, and then go on to state 200. */
      } else if( state == 103 ) {
         if( text1 ){
            elem = astXmlAddElement( parent, text1, NULL );
            elem->complete = 1;
            text1 = astFree( text1 );
            state = 200;

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }

/* State 104: We have just got the (prefix:)name for an element start tag, but
   the start tag may contain further attributes, etc. Create a new XmlElement
   and store it in the container (if any). Then go to state 12 in which we
   look for further attributes, etc. */
      } else if( state == 104 ) {
         if( text1 ){
            elem = astXmlAddElement( parent, text1, NULL );
            text1 = astFree( text1 );
            state = 12;

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }

/* State 105: We have just reached the end of a comment tag. Create a new
   XmlComment and  store it in the parent. */
      } else if( state == 105 ) {
         if( text1 ){

/* If we have a parent element, just add it to the element. */
            if( parent ) {
               if( wanted > 0 ) {
                  astXmlAddComment( (AstXmlParent *) parent, 0, text1 );
               } else if( !skip ) {
                  if( parent ) {
                     astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                               "the input data \"%s\" within element %s.", status,
                               msg, astXmlGetTag( parent, 1 ) );
                  } else {
                     astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                               "the input data: \"%s\".", status, msg );
                  }
                  break;
               }

/* Otherwise, add it to the document prologue or epilogue. */
            } else {
               if( (*doc)->root ) {
                  where = 3;
               } else if( (*doc)->prolog->dtdec ){
                  where = 2;
               } else {
                  where = 1;
               }
               astXmlAddComment( (AstXmlParent *) *doc, where, text1 );
            }

            text1 = astFree( text1 );

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }
         state = 0;

/* State 106: We have just reached the end of a CDATA tag. Create a new
   XmlCDATASection and store it in the container (if any). */
      } else if( state == 106 ) {
         if( text1 ){
            if( parent && wanted > 0 ) {
               astXmlAddCDataSection( parent, text1 );
            } else if( !skip ) {
               if( parent ) {
                  astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                            "the input data \"%s\" within element %s.", status,
                            msg, astXmlGetTag( parent, 1 ) );
               } else {
                  astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                            "the input data: \"%s\".", status, msg );
               }
               break;
            }
            text1 = astFree( text1 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }
         state = 0;

/* State 107: We have just reached the end of an attribute or namespace
   setting. Create a new object and store it in the element created
   earlier. */
      } else if( state == 107 ) {
         if( text1 ){
            if( !elem ) {
               astError( AST__INTER, "ReadContent(xml): Container lost at state "
                         "107 (AST internal programming error).", status );
               break;
            }

            if( !strcmp( text1, "xmlns" ) ) {
               astXmlAddURI( elem, NULL, text2 );

            } else if( !strncmp( text1, "xmlns:", 6 ) ) {
               astXmlAddURI( elem, text1+6, text2 );

            } else {
               text4 = RemoveEscapes( text2, status );
               astXmlAddAttr( elem, text1, text4, NULL );
               text4 = astFree( text4 );
            }

            text1 = astFree( text1 );
            text2 = astFree( text2 );

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", status, msg );
            break;
         }
         state = 12;

/* State 108: We have just reached the end of an empty element tag to which
   we have been adding attributes, etc. */
      } else if( state == 108 ) {
         if( elem ) {
            elem->complete = 1;
            state = 200;
         } else {
            astError( AST__INTER, "Parse(xml): No container in state 108 "
                      "(AST internal programming error).", status );
            break;
         }

/* State 109: We have just reached the end of a DOCTYPE tag. */
      } else if( state == 109 ) {

         if( (*doc)->root ){
            astError( AST__XMLWF, "astRead(XmlChan): An DOCTYPE tag "
                      "\"%s\" was encountered within the body of the "
                      "document.", status, msg );
            break;

         } else if( (*doc)->prolog->dtdec ){
            astError( AST__XMLWF, "astRead(XmlChan): Multiple DOCTYPE tags "
                      "encountered." , status);
            break;

         } else {
            astXmlSetDTDec( *doc, text1, text2, text3 );
            text1 = astFree( text1 );
            text2 = astFree( text2 );
            text3 = astFree( text3 );
            state = 0;
         }

      } else if( state != 200 ) {
         astError( AST__INTER, "ReadContent(xml): Illegal state (%d) encountered "
                   "(AST internal programming error).", status, state );
      }



/* State 200: We now have now read a complete element start tag and have
   a corresponding XmlElement ("elem"), with all attributes and namespaces,
   etc (but no content). Call the "is_wanted" function to see if the client
   is interested in the element. */
      if( state == 200 ) {

/* If this element is found at the root level of the document, store a
   pointer to it as the root element. Report an error if there is already
   a root element. */
         if( !parent ) {
            if( (*doc)->root ){
               if( astOK ) {
                  astError( AST__XMLWF, "astRead(XmlChan): Multiple root "
                            "elements encountered." , status);
                  elem = astXmlDelete( elem );
               }
               break;
            } else {
               (*doc)->root = elem;
               ((AstXmlObject *) elem )->parent = (AstXmlParent *) (*doc);
            }
         }

/* If we do not already know, ask the caller if it is interested in this new
   element. If no "is_wanted" function was supplied, assume all elements
   are interesting. */
         if( wanted == -1 ) {
            newwanted = is_wanted ? (*is_wanted)( elem, status ) : 1;
         } else {
            newwanted = wanted;
         }

/* If it is not interested, report an error if skip is zero. */
         if( newwanted != 1 && !skip ) {
            if( parent ) {
               astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                         "the input data \"%s\" within element %s.", status,
                         msg, astXmlGetTag( parent, 1 ) );
            } else {
               astError( AST__BADIN, "astRead(XmlChan): Cannot interpret "
                         "the input data: \"%s\".", status, msg );
            }
            break;
         }

/* Make the new element the "current" element in the document. */
         (*doc)->current = elem;

/* Read the contents of the new element from the source. If the client is
   interested in the element, the read contents will be added to the
   element, otherwise they will be discarded after being read. */
         answer = ReadContent( doc, newwanted, is_wanted, skip, source,
                               data, depth + 1, status );

/* If the first interesting element was found inside "elem", then
   return it. If "elem" is not interesting and did not contain anything
   of interest, delete it and return the initialised NULL pointer. */
         if( newwanted < 0 ) {
            if( answer ) {
               result = answer;
            } else {
               elem = astXmlDelete( elem );
            }

/* If the elem is of no interest, delete it and return the initialised
   NULL pointer. */
         } else if( newwanted == 0 ) {
            elem = astXmlDelete( elem );

/* Otherwise, "elem" itself is definitely of interest. If "elem" is
   the first item of interest, return it. */
         } else if( wanted < 0 ) {
            result = elem;
         }

/* If we have an answer to return, leave the loop, otherwise re-instate the
   original current element in the document and continue to read any text
   following the element. */
         if( result ) {
            break;
         } else {
            (*doc)->current = parent;
            state = 0;
         }

      } if( state > 22 ) {
         astError( AST__INTER, "ReadContent(xml): Illegal state (%d) encountered "
                   "(AST internal programming error).", status, state );
      }

/* Remember the previous two character */
      lc2 = lc;
      lc = c;
   }

/* Free any dynamic strings */
   text1 = astFree( text1 );
   text2 = astFree( text2 );
   text3 = astFree( text3 );
   if( msg ) msg = astFree( msg );

/* Delete the returned object if an error occurred. */
   if( !astOK ) result = astXmlDelete( result );

/* Return the result. */
   return result;
}



