/*
*  Name:
*     xml.c

*  Purpose:
*     Implement XML functions for AST.

*  Description:
*     This file implements the Xml module which provides generic XML 
*     reading and writing functions for the XmlChan class.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     22-OCT-2003 (DSB):
*        Original version.
*/


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "memory.h"            /* Interface to the memory management module */
#include "error.h"             /* Interface to the error module */
#include "xml.h"               /* Interface to this module */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"           /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Module Constants. */
/* ----------------- */
#define IND_INC 3

/* Module variables. */
/* ================= */
static int nobj = 0;
static int next_id = 0;
static AstXmlObject **existing_objects = NULL;


/* Function prototypes. */
/* ==================== */

/* Private member functions. */
/* ------------------------- */
static AstXmlAttribute *FindAttribute( AstXmlElement *, const char * );
static AstXmlAttribute *NewAttribute( const char *, const char *, const char * );
static AstXmlElement *Parse( AstXmlElement *, char (*)( void * ), void *, char *, int );
static AstXmlNamespace *NewNamespace( const char *, const char * );
static char *AppendChar( char *, int *, char );
static char *AppendString( char *, int *, const char * );
static char *RemoveEscapes( const char * );
static const char *AddEscapes( const char * );
static const char *DefaultURI( AstXmlElement * );
static const char *Format( AstXmlObject *, int );
static const char *ResolvePrefix( const char *, AstXmlElement * );
static int CheckType( long int, long int );
static int MatchName( AstXmlElement *, const char * );
static int Ustrcmp( const char *, const char * );
static void *Delete( void * );
static void AddContent( AstXmlElement *, AstXmlContentItem * );
static void AddObjectToList( AstXmlObject * );
static void CheckName( const char *, const char *, const char *, int );
static void CheckPrefName( char *, const char *, const char * );
static void CleanXml( AstXmlObject *, long int );
static void ClearParent( void * );
static void InitXmlAttribute( AstXmlAttribute *, int, const char *, const char *, const char * );
static void InitXmlCDataSection( AstXmlCDataSection *, int, const char * );
static void InitXmlCharData( AstXmlCharData *, int, const char * );
static void InitXmlComment( AstXmlComment *, int, const char * );
static void InitXmlContentItem( AstXmlContentItem *, int );
static void InitXmlElement( AstXmlElement *, int, const char *, const char * );
static void InitXmlNamespace( AstXmlNamespace *, int, const char *, const char * );
static void InitXmlObject( AstXmlObject *, long int );
static void InitXmlPI( AstXmlPI *, int, const char *, const char * );
static void RemoveObjectFromList( AstXmlObject * );

/* Function implementations. */
/* ========================= */

static void AddContent( AstXmlElement *this, AstXmlContentItem *item ){
/*
*  Name:
*     AddContent

*  Purpose:
*     Add a content item to an XmlElement.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void AddContent( AstXmlElement *this, AstXmlContentItem *item )

*  Description:
*     This function adds a supplied content item to a specified XmlElement.

*  Parameters:
*     this
*        The pointer to the element to be modified.
*     item
*        Pointer to the content item to be added to the element. This can
*        be a pointer to any of the following types: AstXmlElement,
*        AstXmlCharData, AstXmlCDataSection, AstXmlComment, AstXmlPI.
*        These items can be created using the functions with names of the
*        form "astXmlNew<type>".
*/

/* Local Variables: */
   int nitem;             /* Number of content items in the element */

/* Check the global error status and the supplied pointers. */
   if( !astOK || !this || !item ) return;

/* Save the number of content items currently stored in the element. */
   nitem = ( this->items ) ? this->nitem : 0;

/* Attempt to extend the array to hold an extra item. */      
   this->items = astGrow( this->items, nitem + 1, 
                          sizeof( AstXmlContentItem * ) );

/* Check the memory was allocated succesfully. */
   if( astOK ) {

/* Store the supplied pointer in the array of content items. */
      this->items[ nitem ] = item;

/* Increment the number of content items in this element */
      this->nitem = nitem + 1;

/* Indicate that the item is owned by the element. */
      ( (AstXmlObject *) item )->parent = this;

   }
}

static const char *AddEscapes( const char *text ){
/*
*  Name:
*     AddEscapes

*  Purpose:
*     Replaces characters by corresponding entity references.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *AddEscapes( const char *text )

*  Description:
*     This function produces a dynamic copy of the supplied text in which 
*     occurrences of "&", "<", ">", and "\"" are replaced by the corresponding 
*     XML entity reference.

*  Parameters:
*     text
*        A pointer to a text string.

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

/* We replace this character if it is a <, >, or ". */
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

/* We also replace this character if it is a & which does not introduce
   an entity or character reference. */
         } else if( *c == '&' && c[ 1 ] != '#' &&
                    strncmp( c, "&amp;", 5 ) &&
                    strncmp( c, "&lt;", 4 ) &&
                    strncmp( c, "&gt;", 4 ) &&
                    strncmp( c, "&apos;", 6 ) &&
                    strncmp( c, "&quot;", 6 ) ) {
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
   existing_objects = astGrow( existing_objects, ++nobj, sizeof( AstXmlObject *) );

/* Add the new pointer to the end of the list. */
   existing_objects[ nobj - 1 ] = obj;
}

static char *AppendChar( char *str1, int *nc, char ch ) {
/*
*  Name:
*     AppendChar

*  Purpose:
*     Append a character to a string which grows dynamically.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *AppendChar( char *str1, int *nc, char ch )

*  Class Membership:
*     Channel member function.

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

static char *AppendString( char *str1, int *nc, const char *str2 ) {
/*
*  Name:
*     AppendString

*  Purpose:
*     Append a string to another string which grows dynamically.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *AppendString( char *str1, int *nc, const char *str2 )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function appends one string to another dynamically
*     allocated string, extending the dynamic string as necessary to
*     accommodate the new characters (plus the final null).

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
   char *result;                 /* Pointer value to return */
   int len;                      /* Length of new string */

/* Initialise. */
   result = str1;

/* If the first string pointer is NULL, also initialise the character
   count to zero. */
   if ( !str1 ) *nc = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Calculate the total string length once the two strings have been
   concatenated. */
   len = *nc + (int) strlen( str2 );

/* Extend the first (dynamic) string to the required length, including
   a final null. Save the resulting pointer, which will be
   returned. */
   result = astGrow( str1, len + 1, sizeof( char ) );

/* If OK, append the second string and update the total character
   count. */
   if ( astOK ) {
      (void) strcpy( result + *nc, str2 );
      *nc = len;
   }

/* Return the result pointer. */
   return result;
}

AstXmlContentItem *astCheckXmlContentItem_( void *this ){
/*
*+
*  Name:
*     astCheckXmlContentItem

*  Purpose:
*     Check that the supplied pointer identifies an AstXmlContentItem structure.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlContentItem *astCheckXmlContentItem( void *this )

*  Description:
*     This function checks that the supplied pointer locates an
*     AstXmlContentItem structure, or one of the derived classes of structure.
*     If so, the supplied pointer is returned as the function value, cast
*     to a "pointer to AstXmlContentItem". If not, an error is reported an NULL
*     is returned.

*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     The supplied pointer, or NULL if an error has already occurred, or
*     if the supplied pointer is not of the correct type. 
*-
*/

/* Local Variables: */
   AstXmlContentItem *result;    /* The returned pointer */

/* Check the global error status. If an error has already occurred just
   return the supplied pointer. This is so that functions such as
   astXmlAnnul which do not check the inherited status receive the
   supplied pointer. */
   if( !astOK ) return this;

/* Initialise */
   result = NULL;

/* If the pointer is NULL issue an error. */
   if( !this ) {
      astError( AST__PTRIN, "astCheckXmlContentItem: Invalid NULL pointer "
                "supplied." );

/* Otherwise get the "id" component which holds a magic value for each
   different class of AstXmlContentItem structure. Compare this id value against
   all valid classes of AstXmlContentItem structure. If no match is found, the
   pointer does not identify an AstXmlContentItem structure, and so report an
   error and return NULL. */
   } else {
      if( !CheckType( ( ( AstXmlObject * ) this )->type, AST__XMLCONT ) ) {
         astError( AST__PTRIN, "astCheckXmlContentItem: Invalid pointer "
                   "supplied; pointer to AstXmlContentItem required." );
      } else {
         result = (AstXmlContentItem *) this;
      }
   }

/* Return the result. */
   return result;
}

AstXmlElement *astCheckXmlElement_( void *this ){
/*
*+
*  Name:
*     astCheckXmlElement

*  Purpose:
*     Check that the supplied pointer identifies an AstXmlElement structure.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *astCheckXmlElement( void *this )

*  Description:
*     This function checks that the supplied pointer locates an
*     AstXmlElement structure, or one of the derived classes of structure.
*     If so, the supplied pointer is returned as the function value, cast
*     to a "pointer to AstXmlElement". If not, an error is reported an NULL
*     is returned.

*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     The supplied pointer, or NULL if an error has already occurred, or
*     if the supplied pointer is not of the correct type. 
*-
*/

/* Local Variables: */
   AstXmlElement *result;    /* The returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If the pointer is NULL issue an error. */
   if( !this ) {
      astError( AST__PTRIN, "astCheckXmlElement: Invalid NULL pointer " 
                "supplied." );

/* Otherwise get the "id" component which holds a magic value for each
   different class of AstXmlElement structure. Compare this id value against
   all valid classes of AstXmlElement structure. If no match is found, the
   pointer does not identify an AstXmlElement structure, and so report an
   error and return NULL. */
   } else {
      if( !CheckType( ( ( AstXmlObject * ) this )->type, AST__XMLELEM ) ) {
         astError( AST__PTRIN, "astCheckXmlElement: Invalid pointer supplied; "
                   "pointer to AstXmlElement required." );
      } else {
         result = (AstXmlElement *) this;
      }
   }

/* Return the result. */
   return result;
}

AstXmlObject *astCheckXmlObject_( void *this ){
/*
*+
*  Name:
*     astCheckXmlObject

*  Purpose:
*     Check that the supplied pointer identifies an AstXmlObject structure.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlObject *astCheckXmlObject( void *this )

*  Description:
*     This function checks that the supplied pointer locates an
*     AstXmlObject structure, or one of the derived classes of structure.
*     If so, the supplied pointer is returned as the function value, cast
*     to a "pointer to AstXmlObject". If not, an error is reported an NULL
*     is returned.

*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     The supplied pointer, or NULL if an error has already occurred, or
*     if the supplied pointer is not of the correct type. 
*-
*/

/* Local Variables: */
   AstXmlObject *result;       /* The returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If the pointer is NULL issue an error. */
   if( !this ) {
      astError( AST__PTRIN, "astCheckXmlObject: Invalid NULL pointer supplied." );

/* Otherwise get the "id" component which holds a magic value for each
   different class of AstXmlObject structure. Compare this id value against
   all valid classes of AstXmlObject structure. If no match is found, the
   pointer does not identify an AstXmlObject structure, and so report an
   error and return NULL. */
   } else {
      if( !CheckType( ( ( AstXmlObject * ) this )->type, AST__XMLOBJECT ) ) {
         astError( AST__PTRIN, "astCheckXmlObject: Invalid pointer supplied; " 
                   "pointer to AstXmlObject required." );
      } else {
         result = (AstXmlObject *) this;
      }
   }

/* Return the result. */
   return result;
}

void astXmlAddAttr_( AstXmlElement *this, const char *name, const char *value, 
                     const char *prefix ){
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
*        The namespace prefix for the attribute. May be NULL or empty, in
*        which case any prefix at the start of "name" is used.
*-
*/

/* Local Variables: */
   AstXmlAttribute *attr;    /* The new attribute. */
   AstXmlAttribute *oldattr; /* Pointer to existing attribute */
   int i;                    /* Loop index */
   int nattr;                /* Number of attributes in the element */
   int oldi;                 /* Index of existing attribute */

/* Check the global error status. */
   if( !astOK ) return;

/* Create a new XmlAttribute. */
   attr = NewAttribute( name, value, prefix );

/* If OK, indicate that the attribute is owned by the element. */
   if( astOK ) {
      ( (AstXmlObject *) attr )->parent = this;

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
         this->attrs[ oldi ] = attr;
         oldattr = astXmlAnnul( oldattr );      

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

void astXmlAddCDataSection( AstXmlElement *this, const char *text ){
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
   AstXmlCDataSection *new;        /* Pointer to new structure */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlCDataSection *) astMalloc( sizeof( AstXmlCDataSection ) );

/* Initialise it. */
   InitXmlCDataSection( new, AST__XMLCDATA, text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlAnnul( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, (AstXmlContentItem *) new );
   }
}

void astXmlAddCharData( AstXmlElement *this, const char *text ){
/*
*+
*  Name:
*     astXmlAddCharData

*  Purpose:
*     Create a new XmlCharData and add it to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddCharData( AstXmlElement *this, const char *text )

*  Description:
*     This function creates a new XmlCharData structure representing
*     parsed character data, and adds it into an existing element.

*  Parameters:
*     this
*        Pointer to the element to be modified.
*     text
*        Pointer to a null terminated string containing the character data.

*-
*/

/* Local Variables: */
   AstXmlCharData *new;        /* Pointer to the new structure */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlCharData *) astMalloc( sizeof( AstXmlCharData ) );

/* Initialise it. */
   InitXmlCharData( new, AST__XMLCHAR, text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlAnnul( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, (AstXmlContentItem *) new );
   }
}

void astXmlAddComment( AstXmlElement *this, const char *text ){
/*
*+
*  Name:
*     astXmlAddComment

*  Purpose:
*     Create a new XmlComment and add it to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddComment( AstXmlElement *this, const char *text )

*  Description:
*     This function creates a new XmlComment structure representing
*     an XML comment, and adds it into an existing element.

*  Parameters:
*     this
*        Pointer to the element to be modified.
*     text
*        Pointer to a null terminated string containing the comment text.

*-
*/

/* Local Variables: */
   AstXmlComment *new;        /* Pointer to the new structure */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlComment *) astMalloc( sizeof( AstXmlComment ) );

/* Initialise it. */
   InitXmlComment( new, AST__XMLCOM, text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlAnnul( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, (AstXmlContentItem *) new );
   }

}

AstXmlElement *astXmlAddElement_( AstXmlElement *this, const char *name, 
                                  const char *prefix ){
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
*        The namespace prefix for the element. May be NULL or empty, in
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
   InitXmlElement( new, AST__XMLELEM, name, prefix );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlAnnul( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, (AstXmlContentItem *) new );
   }

/* Return the result. */
   return new;

}

void astXmlAddPI( AstXmlElement *this, const char *target, const char *text ){
/*
*+
*  Name:
*     astXmlAddPI

*  Purpose:
*     Create a new XmlPI and add it to an XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlAddPI( AstXmlElement *this, const char *target, 
*                       const char *text )

*  Description:
*     This function creates a new XmlPI structure representing an 
*     XML "programming instruction", and adds it into an existing element.

*  Parameters:
*     this
*        Pointer to the element to be modified.
*     target
*        Pointer to a null terminated string containing the PI target.
*     text
*        Pointer to a null terminated string containing the PI text.

*-
*/

/* Local Variables: */
   AstXmlPI *new;        /* Pointer to the new structure */

/* Check the global error status. */
   if( !astOK ) return;

/* Allocate space for the new structure. */
   new = (AstXmlPI *) astMalloc( sizeof( AstXmlPI ) );

/* Initialise it. */
   InitXmlPI( new, AST__XMLPI, target, text );

/* If an error occurred, delete the new structure. */
   if( !astOK ) {
      new = astXmlAnnul( new );

/* Otherwise, add the content item to the element. */
   } else {
      AddContent( this, (AstXmlContentItem *) new );
   }
}

void astXmlAddURI_( AstXmlElement *this, const char *prefix, const char *uri ){
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
*        prefix. If this is NULL or empty, then the supplied URI is used 
*        as the default namespace for this element and all child elements 
*        (except for child elements which define their own default 
*        namespace).
*     uri
*        Pointer to a null terminated string containing the namespace URI.
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

/* Store the length of the namespace prefix. */
   nc = prefix ? strlen( prefix ) : 0;   

/* If no namespace prefix has been supplied, just change the default
   namespace URI. */
   if( !nc ) {
      this->defns = astStore( this->defns, uri, strlen( uri ) + 1 );

/* Otherwise, add the namespace definition to the element. */
   } else {

/* Create a new XmlNamespace. */
      ns = NewNamespace( prefix, uri );

/* If OK, indicate that the namespace is owned by the element. */
      if( astOK ) {
         ( (AstXmlObject *) ns )->parent = this;

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
            this->nsprefs[ oldi ] = ns;
            oldns = astXmlAnnul( oldns );      

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

void *astXmlAnnul_( AstXmlObject *this ){
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
   if( this->parent ) return this;

/* Remove the supplied object from the list of currently active XmlObjects. */
   RemoveObjectFromList( this );

/* Clean the objects contents, and free the memory holding the XmlObject. */
   CleanXml( this, this->type );
   astFree( this );

/* Return a NULL pointer. */
   return NULL;
}

const char *astXmlFormat_( AstXmlObject *this ) {
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
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/
   return Format( this, -1 );
}

const char *astXmlGetAttributeValue_( AstXmlElement *this, const char *name ){
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
*        Pointer to a string holding the name of the attribute.

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
   attr = FindAttribute( this, name );

/* Get its value. */
   if( attr ) result = attr->value;

/* Return the result. */
   return result;
}

AstXmlContentItem *astXmlGetItem_( AstXmlElement *this, int item ){
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
   if( item < 0 || item >= this->nitem ) {
      astError( AST__XMLIT, "astXmlGetItem: The supplied item index (%d) "
                "is out of bounds. Should be in the range 0 to %d.", 
                item, this->nitem );
   } else {
      result = this->items[ item ];
   }

/* Return the result. */
   return result; 
}

const char *astXmlGetName_( AstXmlObject *this ){
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
                "(internal AST programming error).", type );      
   }

/* Return the result. */
   return result;
}

int astXmlGetNitem_( AstXmlElement *this ){
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

AstXmlElement *astXmlGetParent_( AstXmlObject *this ){
/*
*+
*  Name:
*     astXmlGetParent

*  Purpose:
*     Return a pointer to the XmlElement which contains the supplied XmlObject.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *astXmlGetParent( AstXmlObject *this )

*  Description:
*     This function returns a pointer to the XmlElement which contains
*     the specified XmlObject. The object can be a content item (an
*     element, a comment, a CDATA section, a PI, or character data) in
*     which case the enclosing XmlElement is returned, or an attribute or
*     namespace definition in which case the XmlElement to which object
*     refers is returned.

*  Parameters:
*     this
*        The pointer to check.

*  Returned Value:
*     Pointer to the parent XmlElement, or NULL if the object does not
*     have a parent.

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

const char *astXmlGetTag_( AstXmlObject *this, int opening ){
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
*     This function returns a pointer to a dynamically allocated string
*     containing an XML tag describing the given XmlObject.

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
*     Pointer to a null terminated string holding the tag. This string should 
*     be freed when no longer needed using astFree. NULL is returned if
*     the supplied XmlObject does not have the requested tag.

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
         result = AppendString( result, &nc, "<" );
         if( elem->prefix ) {
            result = AppendString( result, &nc, elem->prefix );
            result = AppendString( result, &nc, ":" );
         }
         result = AppendString( result, &nc, elem->name );
   
         if( elem->defns ) {
            result = AppendString( result, &nc, " xmlns=\"" );
            result = AppendString( result, &nc, elem->defns );
            result = AppendString( result, &nc, "\"" );
         }
   
         for( i = 0; i < elem->nnspref; i++ ) {
            temp = Format( (AstXmlObject *) elem->nsprefs[ i ], -1 );
            result = AppendChar( result, &nc, ' ' );
            result = AppendString( result, &nc, temp );
            temp = astFree( (void *) temp );
         }
   
         for( i = 0; i < elem->nattr; i++ ) {
            temp = Format( (AstXmlObject *) elem->attrs[ i ], -1 );
            result = AppendChar( result, &nc, ' ' );
            result = AppendString( result, &nc, temp );
            temp = astFree( (void *) temp );
         }
   
         if( elem->nitem == 0 ) result = AppendString( result, &nc, "/" );
         result = AppendString( result, &nc, ">" );

      } else if( elem->nitem > 0 ) {
         result = AppendString( result, &nc, "</" );
         if( elem->prefix ) {
            result = AppendString( result, &nc, elem->prefix );
            result = AppendString( result, &nc, ":" );
         }
         result = AppendString( result, &nc, elem->name );
         result = AppendString( result, &nc, ">" );
      }
      
   } else if( type == AST__XMLCDATA ){
      if( opening ) {
         cdata = (AstXmlCDataSection *) this; 
         result = AppendString( result, &nc, "<![CDATA[" );
         result = AppendString( result, &nc, cdata->text );
         result = AppendString( result, &nc, "]]>" );
      }
 
   } else if( type == AST__XMLCOM ){
      if( opening ) {
         com = (AstXmlComment *) this; 
         result = AppendString( result, &nc, "<!--" );
         result = AppendString( result, &nc, com->text );
         result = AppendString( result, &nc, "-->" );
      }

   } else if( type == AST__XMLPI ){
      pi = (AstXmlPI *) this; 
      if( opening ) {
         result = AppendString( result, &nc, "<?" );
         result = AppendString( result, &nc, pi->target );
         if( pi->text && pi->text[0] ) {
            result = AppendString( result, &nc, " " );
            result = AppendString( result, &nc, pi->text );
         }
         result = AppendString( result, &nc, "?>" );
      }
   }

/* Free the returned string if an error has occurred. */
   if( !astOK ) result = astFree( result );

/* Return the result. */
   return result;
}

int astXmlGetType_( AstXmlObject *this ){
/*
*+
*  Name:
*     astXmlGetType

*  Purpose:
*     Return an integer identifying the type of a supplied XML object.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     int astXmlGeType( AstXmlObject *this )

*  Description:
*     This function returns an integer which identifies the type of
*     supplied XmlObject.

*  Parameters:
*     this
*        The pointer to the XmlObject.

*  Returned Value:
*     The integer identifier:
*     - AST__XMLOBJECT for an XmlObject
*     - AST__XMLELEM   for an XmlElement  
*     - AST__XMLATTR   for an XmlAttribute  
*     - AST__XMLCHAR   for an XmlCharData  
*     - AST__XMLCDATA  for an XmlCDdataSection  
*     - AST__XMLCOM    for an XmlComment  
*     - AST__XMLPI     for an XmlPI  
*     - AST__XMLNAME   for an XmlNamespace  
*     - AST__XMLCONT   for an XmlContentItem  

*  Notes:
*     - AST__XMLBAD is returned if an error has already occurred, or if 
*     this function should fail for any reason.
*-
*/

/* Check the global error status. */
   if( !astOK ) return AST__XMLBAD;

/* Return the result. */
   return this->type;
}

const char *astXmlGetURI_( AstXmlObject *this ){
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
         result = ResolvePrefix( prefix, this->parent );
      }

   } else if( type == AST__XMLELEM ){
      prefix = ( (AstXmlElement *) this )->prefix;

/* If there is a prefix, resolve it within the context of this element. */
      if( prefix ) {
         result = ResolvePrefix( prefix, (AstXmlElement *) this );

/* Elements do have a default name space. Therefore if there is no prefix,
   return the default name space within the context of this element. */
      } else {
         result = DefaultURI( (AstXmlElement *) this );
      }

/* If the supplied object is a namespace, just return the associated URI. */
   } else if( type == AST__XMLNAME ){
      result = ( (AstXmlNamespace *) this )->uri;

   }

/* Return the result. */
   return result;
}

const char *astXmlGetValue_( AstXmlObject *this ){
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
*     const char *astXmlGetValue( AstXmlObject *this )

*  Description:
*     This function returns a pointer to a constant string holding the
*     value associated with an XmlObject. For attributes, the attribute value 
*     is returned. For PI elements, the "text" value is returned. For 
*     namespace definitions, the "URI" value is returned. For character
*     data, the character data is returned. For CDATA sections the "text"
*     value is returned. For comments, the "text" value is returned.
*     An error is reported if the supplied XmlObject is of any other class.

*  Parameters:
*     this
*        The pointer to the XmlObject.

*  Returned Value:
*     Pointer to a string holding the value of the XML object.

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
   if( type == AST__XMLATTR ){
      result = ( (AstXmlAttribute *) this )->value;

   } else if( type == AST__XMLCHAR ){
      result = ( (AstXmlCharData *) this )->text;

   } else if( type == AST__XMLCDATA ){
      result = ( (AstXmlCDataSection *) this )->text;

   } else if( type == AST__XMLCOM ){
      result = ( (AstXmlComment *) this )->text;

   } else if( type == AST__XMLPI ){
      result = ( (AstXmlPI *) this )->text;

   } else if( type == AST__XMLNAME ){
      result = ( (AstXmlNamespace *) this )->uri;

   } else {
      astError( AST__INTER, "astXmlGetValue: Inappropriate object type (%d) supplied "
                "(internal AST programming error).", type );      
   }

/* Return the result. */
   return result;
}

void astXmlReadContent_( AstXmlElement *this, char (*source)( void * ), 
                         void *data ){
/*
*+
*  Name:
*     astXmlReadContent

*  Purpose:
*     Read content from an XML source and add it to a given XmlElement.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlReadContent( AstXmlElement *this, char (*source)( void * ), 
*                             void *data )

*  Description:
*     This function reads text from a specified XML source, and adds the
*     corresponding XML objects into the specified XmlElement. Reading
*     stops when the end tag corresponding to the supplied element has
*     been read. An error is reported if the end of the source text is
*     reached before this occurs, of if the text is not well formed.

*  Parameters:
*     this
*        The XmlElement to which objects read from the XML source are to
*        be added. 
*     source
*        Pointer to a function which is called to return the next
*        character from the source. It has a single argument which is
*        used to pass any supplied data to it. It should return zero when
*        the end of the source is reached.
*     data
*        Pointer to a structure to pass to the source function. This
*        structure may contain any data needed by the source function.

*-
*/

/* Read and parse the source text. */
   Parse( this, source, data, NULL, 0 );

}

AstXmlElement *astXmlReadNextElement_( char (*source)( void * ), void *data, 
                                       char *buff, int bufflen ){
/*
*+
*  Name:
*     astXmlReadNextElement

*  Purpose:
*     Read the next element start tag from a text source.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *astXmlReadNextElement( char (*source)( void * ), void *data, 
*                                           char *buff, int bufflen )

*  Description:
*     This function reads characters from the supplied source until it
*     finds the next element start tag (including empty element tags). It 
*     then continues to read the characters contained within this tag, up to 
*     the ">" character marking the end of the tag, and then creates an 
*     empty XmlElement describing the tag. The read stops when the start tag 
*     is complete - it does not go on to read the contents of the element. 
*     All other classes of tag are skipped over. If any characters were 
*     encountered before the start of the returned element start tag, a null 
*     terminated copy of them (or as many of them as will fit) is returned in 
*     the supplied buffer. 

*  Parameters:
*     source
*        Pointer to a function which is called to return the next
*        character from the source. It has a single argument which is
*        used to pass any supplied data to it. It should return zero when
*        the end of the source is reached.
*     data
*        Pointer to a structure to pass to the source function. This
*        structure may contain any data needed by the source function.
*     buff
*        Pointer to a buffer in which to return character read prior to
*        the starting "<" character. If the "<" is the first character
*        read, then buff is returned holding a null (zero length) string.
*     bufflen
*        The maximum number of characters to store in "buff" (not
*        including the terminating null character).

*  Returned Value:
*     Pointer to a new XmlElement representing the next element start tag 
*     encountered in text read from the source. NULL is returned if no
*     complete element start tag is found within the remaining text read 
*     from the source.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*     - It is assumed that the read commences outside any tag (i.e.
*     in between tags or within character data).
*-
*/

/* Return the result. */
   return Parse( NULL, source, data, buff, bufflen );
}

void astXmlRemoveAttr_( AstXmlElement *this, const char *name, 
                        const char *prefix ){
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
*        The namespace prefix for the attribute. May be NULL or empty, in
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

/* Create a new XmlAttribute with blank value. */
   attr = NewAttribute( name, "", prefix );
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
      if( oldi > -1 ) Delete( oldattr );

/* Delete the temporary attribute structure. */
      attr = astXmlAnnul( attr );

   }
}

void astXmlRemoveItem_( AstXmlContentItem *this ){
/*
*+
*  Name:
*     astXmlRemoveItem

*  Purpose:
*     Removes an item of content from its parent element.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xml.h"
*     void astXmlRemoveItem( AstXmlContentItem *this )

*  Description:
*     This function removes an item of content from its parent element.
*     The removed item is not annulled and may be subsequently added into 
*     another element.

*  Parameters:
*     this
*        The pointer to the item to be removed form its parent.
*-
*/

/* Local Variables: */
   AstXmlElement *parent;           /* Pointer to parent element */
   int found;                       /* Was the item found within its parent? */
   int i;                           /* Item index */
   int j;                           /* Item index */

/* Check the global error status. */
   if( !astOK ) return;

/* Get a pointer to the items parent element, and check it is not null. */
   parent = ( (AstXmlObject *) this )->parent;
   if( parent ) {

/* Search through all the items within the parent element looking for the
   supplied item. */
      found = 0;
      for( i = 0; i < parent->nitem; i++ ) {
         if( parent->items[ i ] == this ) {

/* When found, decrement the number of items in the element, and shuffle all 
   the remaining item pointers down one slot to over-write it, then leave 
   the loop. */
            (parent->nitem)--;
            for( j = i; j < parent->nitem; j++ ) {
               parent->items[ j ] = parent->items[ j + 1 ];
            }
            found = 1;
            break;
         }
      }

/* Report an error if the item was not found. */
      if( !found ) {
         astError( AST__INTER, "astXmlRemoveItem: The parent of the supplied "
                   "item does not contain the item (internal AST programming "
                   "error)." );
      }
   }
}

void astXmlRemoveURI_( AstXmlElement *this, const char *prefix ){
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

/* Create a new XmlNamespace with blank URI. */
   ns = NewNamespace( prefix, "" );
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
      if( oldi > -1 ) Delete( oldns );

/* Delete the temporary namespace structure. */
      ns = astXmlAnnul( ns );

   }
}

const char *astXmlShow_( AstXmlObject *this ) {
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
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/
   return Format( this, 0 );
}

static void CheckName( const char *name, const char *noun, const char *method, 
                       int nullok ){
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
*                     int nullok )

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
*/

/* Local Variables: */
   const char *c;       /* Pointer to next character to check */
   
/* Check the global error status. */
   if( !astOK ) return;

/* Check the string is not null. */
   if( !name ) {
      if( !nullok ) astError( AST__XMLNM, "%s: A NULL pointer was supplied "
                              "instead of an XML %s name.", method, noun );
   } else {

      c = name;
      if( *c == 0 ) {
         if( !nullok ) astError( AST__XMLNM, "%s: An empty string was supplied "
                                 "instead of an XML %s name.", method, noun );
      } else {

         if( !isalpha( *c ) && *c != '_' ) {
            astError( AST__XMLNM, "%s: The illegal XML %s name \"%s\" was "
                      "encountered.", method, noun, name );
   
         } else {
            while( *(++c) ) {
               if( !isalnum( *c ) && *c != '_' && *c != '-' && *c != '.' ){
                  astError( AST__XMLNM, "%s: The illegal XML %s name \"%s\" was "
                            "encountered.", method, noun, name );
                  break;
               }
            }
         } 
      }
   }
}

static void CheckPrefName( char *name, const char *noun, const char *method ){
/*
*  Name:
*     CheckPrefName

*  Purpose:
*     Checks the supplied string is a valid XML (prefix:)name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void CheckPrefName( char *name, const char *noun, const char *method )

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
      temp = AppendString( temp, &nc, noun );
      temp = AppendString( temp, &nc, " prefix" );
      CheckName( name, temp, method, 0 );
      temp = astFree( temp );

/* Restore the colon. */
      *colon = ':';

/* Check the string following the colon is a valid name. */
      CheckName( colon + 1, noun, method, 0 );

/* If not found, the whole supplied string must be a name. */
   } else {
      CheckName( name, noun, method, 0 );
   }
}

static int CheckType( long int given, long int want ){
/*
*  Name:
*     CheckType

*  Purpose:
*     Check that the supplied type identifies an object of a given class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     int CheckType( long int given, long int want )

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

*  Returned Value:
*     Non-zero if the check is passed, zero if not of if an error has
*     already occurred.
*/

/* Local Variables: */
   int result;              /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if( !astOK ) return result;

/* Check the wanted type is recognised. Report an error if not. */
   if( want != AST__XMLOBJECT &&
       want != AST__XMLELEM  &&
       want != AST__XMLATTR  &&
       want != AST__XMLCHAR  &&
       want != AST__XMLCDATA &&
       want != AST__XMLCOM   &&
       want != AST__XMLPI    &&
       want != AST__XMLNAME  &&
       want != AST__XMLCONT ) {

      astError( AST__INTER, "CheckType(Xml): Unsupported XML object type (%d) "
                "supplied for parameter \"want\" (internal AST programming "
                "error). ", want );

/* If the wanted type is OK, return a non-zero value if the type to be tested 
   equals the wanted type. */
   } else if( want == given ) {
      result = 1;

/* Otherwise, for those classes of XML structure which have children
   classes, check if the given type is a child class of the wanted 
   class. */
   } else if( want == AST__XMLOBJECT ) {
      result = ( given == AST__XMLELEM  ||
                 given == AST__XMLATTR  ||
                 given == AST__XMLCHAR  ||
                 given == AST__XMLCDATA ||
                 given == AST__XMLCOM   ||
                 given == AST__XMLPI    ||
                 given == AST__XMLNAME  ||
                 given == AST__XMLCONT );

   } else if( want == AST__XMLCONT ) { 
      result = ( given == AST__XMLELEM  ||
                 given == AST__XMLCHAR  ||
                 given == AST__XMLCDATA ||
                 given == AST__XMLCOM   ||
                 given == AST__XMLPI    );
   }

/* Return the result. */
   return result;
}

static void CleanXml( AstXmlObject *this, long int type ){
/*
*  Name:
*     CleanXml

*  Purpose:
*     Free the resources used within an XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void CleanXml( AstXmlObject *this, long int type )

*  Description:
*     This function frees the resources used internally within the
*     supplied XmlObject.

*  Parameters:
*     this
*        pointer to the XmlObject to be cleaned.
*     type
*        The type of XmlObject being cleaned.

*  Notes:
*     This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Local Variables: */
   AstXmlElement *elem;
   AstXmlAttribute *attr;
   AstXmlNamespace *ns;
   AstXmlContentItem *cont;
   AstXmlCharData *chardata;
   AstXmlCDataSection *cdatasec;
   AstXmlComment *comm;
   AstXmlPI *pi;
   int i;

/* Return if a NULL pointer has been suppplied. */
   if( !this ) return;

/* For the base XmlObject class, clear the object type, etc. */
   if( type == AST__XMLOBJECT  ){
      this->type = AST__XMLBAD;
      this->parent = NULL;

/* For each derived class of XmlObject, first clean the parent component, 
   then clean any further resources. */
   } else if( type == AST__XMLELEM  ){
      CleanXml( this, AST__XMLCONT );

      elem = (AstXmlElement *) this;

      elem->name = astFree( elem->name );   
      elem->defns = astFree( elem->defns );   
      elem->prefix = astFree( elem->prefix );   

      if( elem->attrs ) {
         for( i = 0; i < elem->nattr; i++ ) {
            ClearParent( elem->attrs[ i ] );
            elem->attrs[ i ] = astXmlAnnul( elem->attrs[ i ] );
         }
         elem->attrs = astFree( elem->attrs );
      }
      elem->nattr = 0;

      if( elem->items ) {
         for( i = 0; i < elem->nitem; i++ ) {
            ClearParent( elem->items[ i ] );
            elem->items[ i ] = astXmlAnnul( elem->items[ i ] );
         }
         elem->items = astFree( elem->items );
      }
      elem->nitem = 0;
      
      if( elem->nsprefs ) {
         for( i = 0; i < elem->nnspref; i++ ) {
            ClearParent( elem->nsprefs[ i ] );
            elem->nsprefs[ i ] = astXmlAnnul( elem->nsprefs[ i ] );
         }
         elem->nsprefs = astFree( elem->nsprefs );
      }
      elem->nnspref = 0;

   } else if( type == AST__XMLATTR ){
      CleanXml( this, AST__XMLOBJECT );
      attr = (AstXmlAttribute *) this;
      attr->name = astFree( attr->name );     
      attr->value = astFree( attr->value );     
      attr->prefix = astFree( attr->prefix );     

   } else if( type == AST__XMLCHAR ){
      CleanXml( this, AST__XMLCONT );
      chardata = (AstXmlCharData *) this;
      chardata->text = astFree( chardata->text );     

   } else if( type == AST__XMLCDATA ){
      CleanXml( this, AST__XMLCONT );
      cdatasec = (AstXmlCDataSection *) this;
      cdatasec->text = astFree( cdatasec->text );     
 
   } else if( type == AST__XMLCOM ){
      CleanXml( this, AST__XMLCONT );
      comm = (AstXmlComment *) this;
      comm->text = astFree( comm->text );     

   } else if( type == AST__XMLPI ){
      CleanXml( this, AST__XMLCONT );
      pi = (AstXmlPI *) this;
      pi->target = astFree( pi->target );     
      pi->text = astFree( pi->text );     

   } else if( type == AST__XMLNAME ){
      CleanXml( this, AST__XMLOBJECT );
      ns = (AstXmlNamespace *) this;
      ns->prefix = astFree( ns->prefix );     
      ns->uri = astFree( ns->uri );     

   } else if( type == AST__XMLCONT ){
      CleanXml( this, AST__XMLOBJECT );
      cont = (AstXmlContentItem *) this;

   } else if( astOK ) {
      astError( AST__INTER, "CleanXml: Invalid object type (%d) supplied "
                "(internal AST programming error).", type );      
   }

}

static void ClearParent( void *obj ){
/*
*  Name:
*     ClearParent 

*  Purpose:
*     Indicate that the supplied XmlObject has no parent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void ClearParent( void *obj )

*  Description:
*     This function checks that the supplied pointer is for an XmlObject,
*     and then sets the "pareent" pointer NULL in the supplied XmlObject.
*     An XmlObject with a non-NULL parent cannot be annulled.

*  Parameters:
*     obj
*        Pointer to an XmlObecjt.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstXmlObject *this;

/* Get a pointer which can be used to access the XmlObject structure. */
   this = (AstXmlObject *) obj;  

/* Check the supplied pointer is an XmlObject. */
   if( !CheckType( this->type, AST__XMLOBJECT ) ) {
      astError( AST__INTER, "ClearParent(xml): Invalid pointer supplied " 
                "(internal AST programming error)." );

/* If so, nullify its parent pointer. */
   } else {
      this->parent = NULL;
   }
}

static const char *DefaultURI( AstXmlElement *elem ){
/*
*  Name:
*     DefaultURI

*  Purpose:
*     Find the URI associated with the default namespace.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *DefaultURI( AstXmlElement *elem )

*  Description:
*     This function returns the default namespace URI defined within the
*     given element. If the element does not define a default namespace URI,
*     then this function is called recursively on the parent element. If 
*     there is no parent element, NULL is returned.

*  Parameters:
*     elem
*        The pointer to the XmlElement.

*  Returned Value:
*     Pointer to a string holding the URI, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   const char *result;          /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status, and the supplied element. */
   if( !astOK || !elem ) return result;

/* If the supplied element defines a default namespace URI, return it.
   Otherwise, call this function to get the default namespace URI from the
   parent element. */
   result = elem->defns;
   if( !result ) result = DefaultURI( ( (AstXmlObject *) elem )->parent );   

/* Return the result. */
   return result;
}

static void *Delete( void *obj_ptr ){
/*
*  Name:
*     Delete

*  Purpose:
*     Remove the supplied XmlObject from its parent and delete it.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     void *Delete( void *obj )

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
*/

/* Local Variables: */
   AstXmlObject *obj;           /* Pointer to XmlObject */
   AstXmlElement *parent;       /* Pointer to parent */
   int i;                       /* Loop counter */
   int j;                       /* Loop counter */
   int n;                       /* Number of values in list */
   int type;                    /* XmlObject type */

/* Check we have an XmlObject. */
   if( !obj_ptr ) return NULL;
   obj = (AstXmlObject *) obj_ptr;
   if( !CheckType( obj->type, AST__XMLOBJECT ) ) return NULL;

/* Get the parent of the supplied object. */
   parent = obj->parent;
   if( parent ) {

/* Remove the object form the appropriate list in the parent element, and
   then shuffle down the remaining entries in the list and decrement the
   size of the list. */
      type = obj->type;
      if( type == AST__XMLATTR ) {      
         n = parent->nattr;
         for( i = 0; i < n; i++ ) {
            if( parent->attrs[ i ] == (AstXmlAttribute *) obj ) {
               for( j = i + 1; j < n; j++ ) {
                  parent->attrs[ j - 1 ] =  parent->attrs[ j ];
               }
               parent->attrs[ --parent->nattr ] = NULL;
               break;
            }
         }

      } else if( type == AST__XMLNAME ) {      
         n = parent->nnspref;
         for( i = 0; i < n; i++ ) {
            if( parent->nsprefs[ i ] == (AstXmlNamespace *) obj ) {
               for( j = i + 1; j < n; j++ ) {
                  parent->nsprefs[ j - 1 ] =  parent->nsprefs[ j ];
               }
               parent->nsprefs[ --parent->nnspref ] = NULL;
               break;
            }
         }

      } else if( CheckType( obj->type, AST__XMLCONT ) ) {
         n = parent->nitem;
         for( i = 0; i < n; i++ ) {
            if( parent->items[ i ] == (AstXmlContentItem *) obj ) {
               for( j = i + 1; j < n; j++ ) {
                  parent->items[ j - 1 ] =  parent->items[ j ];
               }
               parent->items[ --parent->nitem ] = NULL;
               break;
            }
         }
      }

/* Nullify the parent pointer to that astXmlAnnul will delete the object. */
      obj->parent = NULL;
   }

/* Annul the object and return the resulting NULL pointer. */
   return astXmlAnnul( obj );   
}

static AstXmlAttribute *FindAttribute( AstXmlElement *this, const char *name ){
/*
*  Name:
*     FindAttribute

*  Purpose:
*     Search an XmlElement for a named attribute 

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlAttribute *FindAttribute( AstXmlElement *this, const char *name )

*  Description:
*     This function searches the supplied XmlElement for an attribute
*     with the given name. If found, a pointer to the XmlAttribute is 
*     returned. Otherwise NULL is returned.

*  Parameters:
*     this
*        The pointer to the XmlElement.
*     name
*        Pointer to a string holding the name of the attribute.

*  Returned Value:
*     Pointer to the XmlAttribute, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlAttribute *result;     /* Returned pointer */
   int i;                       /* Loop count */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* Loop round all the attributes in the element. */
   for( i = 0; i < this->nattr; i++ ) {

/* Compare the attribute name with the supplied name (case sensitive).
   Leave the loop if they match. */
      if( !strcmp( this->attrs[ i ]->name, name ) ) {
         result = this->attrs[ i ];
         break;
      }
   }

/* Return the result. */
   return result;
}

static const char *Format( AstXmlObject *this, int ind ){
/*
*  Name:
*     Format

*  Purpose:
*     Converts an XmlObject into a character string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *Format( AstXmlObject *this, int ind )

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

*  Returned Value:
*     Pointer to a null terminated string holding the formated XmlObject.
*     This string should be freed when no longer needed using astFree.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
   AstXmlAttribute *attrib;  /* Pointer to XML attribute */
   AstXmlCharData *chdata;   /* Pointer to character data */
   AstXmlElement *elem;      /* Pointer to XML element */
   AstXmlNamespace *ns;      /* Pointer to XML namespace instruction */
   char *c;                  /* Pointer into the returned string */
   char *result;             /* The returned pointer */
   const char *temp;         /* A temporary string pointer */
   int i;                    /* Loop count */
   int j;                    /* Loop count */
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

      temp = astXmlGetTag( this, 1 );
      result = AppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

      elem = (AstXmlElement *) this; 
      if( elem->nitem > 0 ) {

         for( i = 0; i < elem->nitem; i++ ) {
            temp = Format( (AstXmlObject *) elem->items[ i ], ( ( ind > -1 ) ? ind + IND_INC : -1 ) );

/* If indentation is required.... */
            if( ind > -1 ) {

/* Remove any trailing white space (except for newlines) from the returned 
   string. */
               if( nc > 0 ) {
                  c = result + nc - 1;
                  while( isspace( *c ) && *c != '\n' ) {
                     *(c--) = 0;
                     nc--;
                  }

/* If the last character in the returned string is not now a newline,
   append a newline, so long as the new item does not start with a newline. */
                  if( result[ nc - 1 ] != '\n' && temp[ 0 ] != '\n' ) {
                     result = AppendChar( result, &nc, '\n' );
                  }
               }

/* Unless this is character data, append an indentation string. */
               if( astXmlGetType( elem->items[ i ] ) != AST__XMLCHAR ) {
                  for( j = 0; j < ind + IND_INC; j++ ) {
                     result = AppendChar( result, &nc, ' ' );
                  }
               }
            }

/* Now append the next item of content, and free its memory. */
            result = AppendString( result, &nc, temp );
            temp = astFree( (void *) temp );
         }

/* Finally append the end tag. */
         if( ind > -1 ) {
            if( nc > 0 ) {
               c = result + nc - 1;
               while( isspace( *c ) && *c != '\n' ) {
                  *(c--) = 0;
                  nc--;
               }
               if( result[ nc - 1 ] != '\n' ) {
                  result = AppendChar( result, &nc, '\n' );
               }
            }
            for( j = 0; j < ind; j++ ) {
               result = AppendChar( result, &nc, ' ' );
            }
         }
         temp = astXmlGetTag( this, 0 );
         result = AppendString( result, &nc, temp );
         temp = astFree( (void *) temp );

      }
      
/* If this is an attribute... */
   } else if( type == AST__XMLATTR ){
      attrib = (AstXmlAttribute *) this; 

      if( attrib->prefix ) {
         result = AppendString( result, &nc, attrib->prefix );
         result = AppendString( result, &nc, ":" );
      }

      result = AppendString( result, &nc, attrib->name );
      result = AppendString( result, &nc, "=\"" );
      result = AppendString( result, &nc, AddEscapes( attrib->value ) );
      result = AppendString( result, &nc, "\"" );

   } else if( type == AST__XMLCHAR ){
      chdata = (AstXmlCharData *) this; 
      result = AppendString( result, &nc, AddEscapes( chdata->text ) );

   } else if( type == AST__XMLCDATA || 
              type == AST__XMLCOM ||
              type == AST__XMLPI ){

      temp = astXmlGetTag( this, 1 );
      result = AppendString( result, &nc, temp );
      temp = astFree( (void *) temp );

   } else if( type == AST__XMLNAME ){
      ns = (AstXmlNamespace *) this;
      result = AppendString( result, &nc, "xmlns:" );
      result = AppendString( result, &nc, ns->prefix );
      result = AppendString( result, &nc, "=\"" );
      result = AppendString( result, &nc, ns->uri );
      result = AppendString( result, &nc, "\"" );

   } else if( astOK ) {
      astError( AST__INTER, "Format(xml): Invalid object type (%d) supplied "
                "(internal AST programming error).", type );      
   }

/* Free the returned string if an error has occurred. */
   if( !astOK ) result = astFree( result );

/* Return the result. */
   return result;
}

static void InitXmlAttribute( AstXmlAttribute *new, int type, const char *name,
                              const char *value, const char *prefix ){
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
*                       const char *value, const char *prefix )

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
*        The namespace prefix for the attribute. May be NULL or empty, in
*        which case any prefix at the start of "name" is used.
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
   if( !CheckType( type, AST__XMLATTR ) ){
      astError( AST__INTER, "InitXmlAttribute: Supplied object type (%d) "
                "does not represent an XmlAttribute", type );
   } 

/* Ensure we have non-NULL pointers. */
   if( !name ) name = "";
   if( !value ) value = "";

/* If no prefix was supplied, extract any prefix from the start of the 
   supplied name. */
   newname = (char *) name;
   newpref = (char *) prefix;
   colon = NULL;

   if( !prefix || !prefix[ 0 ] ){
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
   CheckName( newname, "attribute", "InitXmlAttribute", 0 );
   CheckName( newpref, "attribute", "InitXmlAttribute", 1 );

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type );

/* Initialise the items specific to this class of structure. */
   new->name = astStore( NULL, newname, strlen( newname ) + 1 );
   new->value = RemoveEscapes( value );
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
                                 const char *text ){
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
*                          const char *text )

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
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCDATA ) ){
      astError( AST__INTER, "InitXmlCDataSection: Supplied object type (%d) "
                "does not represent an XmlCDataSection", type );
   } 

/* Initialise the parent XmlContentItem component. */
   InitXmlContentItem( (AstXmlContentItem *) new, type );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. */
   new->text = astStore( NULL, text, strlen( text ) + 1 );
}

static void InitXmlCharData( AstXmlCharData *new, int type, const char *text ){
/*
*  Name:
*     InitXmlCharData

*  Purpose:
*     Initialise a new XmlCharData.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlCharData( AstXmlCharData *new, int type, const char *text )

*  Description:
*     This function initialises supplied memory to hold an XmlCharData 
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*     text
*        Pointer to a null terminated string holding the text.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCHAR ) ){
      astError( AST__INTER, "InitXmlCharData: Supplied object type (%d) "
                "does not represent an XmlCharData", type );
   } 

/* Initialise the parent XmlContentItem component. */
   InitXmlContentItem( (AstXmlContentItem *) new, type );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. */
   new->text = RemoveEscapes( text );
}

static void InitXmlComment( AstXmlComment *new, int type, const char *text ){
/*
*  Name:
*     InitXmlComment

*  Purpose:
*     Initialise a new XmlComment.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlComment( AstXmlComment *new, int type, const char *text )

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
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCOM ) ){
      astError( AST__INTER, "InitXmlComment: Supplied object type (%d) "
                "does not represent an XmlComment", type );
   } 

/* Initialise the parent XmlContentItem component. */
   InitXmlContentItem( (AstXmlContentItem *) new, type );

/* Ensure we have non-NULL pointers. */
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. Report an error 
   if the comment is illegal. */
   if( strstr( text, "--" ) && astOK ) {
      astError( AST__XMLCM, "InitXmlCom(xml): Illegal XML comment "
                "supplied \"%s\" - comments may not contain the "
                "string \"--\".", text );
      new->text = NULL;
   } else {
      new->text = astStore( NULL, text, strlen( text ) + 1 );
   }
}

static void InitXmlContentItem( AstXmlContentItem *new, int type ){
/*
*  Name:
*     InitXmlContentItem

*  Purpose:
*     Initialise a new XmlContentItem.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlContentItem( AstXmlContentItem *new, int type )

*  Description:
*     This function initialises supplied memory to hold an XmlContentItem 
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLCONT ) ){
      astError( AST__INTER, "InitXmlContentItem: Supplied object type (%d) "
                "is not appropriate for an XmlContentItem", type );
   }

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type );

/* Initialise the items specific to this class of structure. */
/*  (none) */

}

static void InitXmlElement( AstXmlElement *new, int type, const char *name, 
                            const char *prefix ){
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
*                     const char *prefix )

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
*        The namespace prefix for the element. May be NULL or empty, in
*        which case any prefix at the start of "name" is used.
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
   if( !CheckType( type, AST__XMLELEM ) ){
      astError( AST__INTER, "InitXmlElement: Supplied object type (%d) "
                "does not represent an XmlElement", type );
   } 

/* Ensure we have non-NULL pointers. */
   if( !name ) name = "";

/* If no prefix was supplied, extract any prefix from the start of the 
   supplied name. */
   newname = (char *) name;
   newpref = (char *) prefix;
   colon = NULL;

   if( !prefix || !prefix[ 0 ] ){
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
   CheckName( newname, "element", "InitXmlElement", 0 );
   CheckName( newpref, "element", "InitXmlElement", 1 );

/* Initialise the parent XmlContentItem component. */
   InitXmlContentItem( (AstXmlContentItem *) new, type );

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
                              const char *uri ){
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
*                       const char *uri )

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
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLNAME ) ){
      astError( AST__INTER, "InitXmlNamespace: Supplied object type (%d) "
                "does not represent an XmlNamespace", type );
   } 

/* Ensure we have non-NULL pointers. */
   if( !prefix ) prefix = "";
   if( !uri ) uri = "";

/* Check the supplied prefix is a valid XML 'name'. */
   CheckName( prefix, "namespace prefix", "InitXmlNamespace", 0 );

/* Initialise the parent XmlObject component. */
   InitXmlObject( (AstXmlObject *) new, type );

/* Initialise the items specific to this class of structure. */
   new->prefix = astStore( NULL, prefix, strlen( prefix ) + 1 );
   new->uri = astStore( NULL, uri, strlen( uri ) + 1 );
}

static void InitXmlObject( AstXmlObject *new, long int type ){
/*
*  Name:
*     InitXmlObject

*  Purpose:
*     Initialise a new XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     InitXmlObject( AstXmlObject *new, long int type )

*  Description:
*     This function initialises supplied memory to hold an XmlObject 
*     structure.

*  Parameters:
*     new
*        The memory in which to initialise the structure.
*     type
*        An identifier for the structure type.
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is OK. Report an error if not. */
   if( !CheckType( type, AST__XMLOBJECT ) ){
      astError( AST__INTER, "InitXmlObject: Supplied object type (%d) "
                "is not appropriate for an XmlObject", type );
   }

/* This class of structure is the base class for XML objects so it has no
   parent class to be initialised. So just initialise the items specific to 
   this class of structure. */
   new->parent = NULL;
   new->type = type;
   new->id = next_id++;

/* Add the new XmlObject to the list of all XmlObjects. */
   AddObjectToList( new );

}

static void InitXmlPI( AstXmlPI *new, int type, const char *target,
                       const char *text ){
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
*                const char *text )

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
*/

/* Check the global error status. */
   if( !astOK ) return;

/* Check the supplied object type is appropriate for the class of
   structure being initialised. If not report an error. */
   if( !CheckType( type, AST__XMLPI ) ){
      astError( AST__INTER, "InitXmlPI: Supplied object type (%d) "
                "does not represent an XmlPI", type );
   } 

/* Initialise the parent XmlContentItem component. */
   InitXmlContentItem( (AstXmlContentItem *) new, type );

/* Ensure we have non-NULL pointers. */
   if( !target ) target = "";
   if( !text ) text = "";

/* Initialise the items specific to this class of structure. Report an error 
   if anything is illegal. */
   new->target = NULL;
   new->text = NULL;

   if( !Ustrcmp( target, "XML" ) && astOK ) {
      astError( AST__XMLPT, "InitXmlPI(xml): Illegal XML PI target \"%s\""
                " supplied.", target );
   } else {
      new->target = astStore( NULL, target, strlen( target ) + 1 );
      new->text = astStore( NULL, text, strlen( text ) + 1 );
   }
}

static int MatchName( AstXmlElement *this, const char *name ){
/*
*  Name:
*     MatchName

*  Purpose:
*     Check that an element has a specified name and/or prefix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     int MatchName( AstXmlElement *this, const char *name )

*  Description:
*     This function checks that an element has a specified name and/or prefix.

*  Parameters:
*     this
*        The XmlElement to check.
*     name
*        The name for the element (may include a namespace prefix).

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
                                      const char *prefix ){
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
*                                    const char *prefix )

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
*        namespace prefix (may be NULL or empty).

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
   InitXmlAttribute( new, AST__XMLATTR, name, value, prefix );

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlAnnul( new );

/* Return the result. */
   return new;

}

static AstXmlNamespace *NewNamespace( const char *prefix, const char *uri ){
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
*                                    const char *uri )

*  Description:
*     This function creates a new XmlNamespace structure representing an
*     XML namespace with the given prefix and uri.

*  Parameters:
*     prefix
*        Pointer to a null terminated string containing the namespace prefix.
*     uri
*        Pointer to a null terminated string containing the associated URI.

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
   InitXmlNamespace( new, AST__XMLNAME, prefix, uri );

/* If an error occurred, delete the new structure. */
   if( !astOK ) new = astXmlAnnul( new );

/* Return the result. */
   return new;

}

static AstXmlElement *Parse( AstXmlElement *this, char (*source)( void * ), 
                             void *data, char *buff, int bufflen ){
/*
*  Name:
*     Parse

*  Purpose:
*     Read and parse an XML document.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     AstXmlElement *Parse( AstXmlElement *this, char (*source)( void * ),
*                           void *data, char *buff, int bufflen )

*  Description:
*     This function reads and parses text from an XML source. If a
*     container element is supplied ("this") then all elements and
*     character data read from the source are added to the supplied 
*     element, until the end tag for the supplied element is encountered
*     (an error is reported if no such end tag is encountered). If no
*     container element is supplied, all tags are skipped over until the
*     next element start tag is encountered (whther empty or not). An 
*     XmlElement is then created from this tag and returned, whereupon 
*     reading stops (i.e. in this mode the contents of the element 
*     are not read).

*  Parameters:
*     this
*        An XmlElement to which all read data is to be added. May be NULL.
*     source
*        Pointer to a function which is called to return the next
*        character from the source. It has a single argument which is
*        used to pass any supplied data to it. It should return zero when
*        the end of the source is reached.
*     data
*        Pointer to a structure to pass to the source function. This
*        structure may contain any data needed by the source function.
*     buff
*        Only used if "this" is NULL. It is a pointer to a buffer in which 
*        to return character read prior to the returned element. If the "<" 
*        is the first character read, then buff is returned holding a null 
*        (zero length) string.
*     bufflen
*        Only used if "this" is NULL. The maximum number of characters to 
*        store in "buff" (not including the terminating null character).

*  Returned Value:
*     If "this" is NULL, a pointer to a new XmlElement representing the next 
*     element start tag encountered in text read from the source is returned. 
*     NULL is returned if no complete element start tag is found within the 
*     remaining text read from the source. If "this" is not NULL, then NULL
*     is returned.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*     - It is assumed that the read commences outside any tag (i.e.
*     in between tags or within character data).
*-
*/

/* Local Variables; */
   AstXmlElement *container;    /* The returned pointer */
   AstXmlElement *result;       /* The returned pointer */
   char *msg;                   /* Pointer to message buffer */
   char *text1;                 /* Pointer to dynamic string */
   char *text2;                 /* Pointer to another dynamic string */
   char c;                      /* Current character read from source */
   char lc2;                    /* Last but one character read */
   char lc;                     /* Last character read */
   int nc1;                     /* No. of characters stored in text1 */
   int nc2;                     /* No. of characters stored in text2 */
   int ncmsg;                   /* Length of "msg" */
   int ncret;                   /* No. of characters stored in buff */
   int state;                   /* Current action being performed */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if( !astOK ) return result;

/* If the supplied element has already been completed (typically because
   it was read from an empty element tag), then just return without
   action. */
   if( this && this->complete ) return result;

/* Initialise the previous two characters read. */
   lc = 0;
   lc2 = 0;

/* Initialise the number of characters stored in the returned buffer. */
   ncret = 0;

/* Initialise pointer to dynamically allocated strings. */
   text1 = NULL;
   text2 = NULL;
   msg = NULL;

/* Initialise the current container to be the supplied element. */
   container = this;

/* In "state zero" we scan the source looking for the "<" character which
   marks the start of the next tag. */
   state = 0;

/* Loop round reading characters from the source. */
   while( 1 ) {
      c = (*source)( data );

/* Leave the loop if an error has occurred. */
      if( !astOK ) break;

/* Report an error and leave the loop if the end of the text has been 
   reached, after reading any text. */
      if( !c ) {
         if( msg ) {
            astError( AST__XMLWF, "astRead(XmlChan): End of XML input text "
                      "reached prematurely following \"%s\".", msg );
         } else if( this ){
            astError( AST__XMLWF, "astRead(XmlChan): End of XML input text "
                      "reached prematurely (unclosed element %s).",
                       astXmlGetTag( this, 1 ) );
         } 
         break;
      }

/* Save the first 20 characters following the start of the most recent tag. */
      if( state > 0 && ncmsg < 20 ) msg = AppendChar( msg, &ncmsg, c );

/* State 0:
   ========
   We are reading character data prior to the next tag. See if the character 
   just read is a "<". If so, save any preceeding text as character data 
   within the container (if any) and enter state 1. Otherwise, if we have no 
   container, store the character in the returned buffer if there is room. 
   If we have a container, store the text in "text1". */
      if( state == 0 ) {
         if( msg ) msg = astFree( msg );

         if( c == '<' ) {
            msg = AppendChar( msg, &ncmsg, c );
            state = 100;

         } else if( !container ) {
            if( ncret < bufflen ) buff[ ncret++ ] = c;

         } else {
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 1:
   ========
   Use the character following the opening "<" to determine what sort of tag 
   has just been started. */
      } else if( state == 1 ) { 

/* If the character is a ">", report an error. */
         if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;

/* If the character is a "?", this must be a PI tag. */
         } else if( c == '?' ) {
            state = 2;

/* If the character is a "!", it must be a comment or a CDATA section */
         } else if( c == '!' ) {
            state = 3;

/* If the character is a "/", it must be an element end tag. */
         } else if( c == '/' ) {
            state = 7;

/* Otherwise, this must be an element start tag. Append the character
   to "text1". */
         } else {
            state = 6;
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 2:
   ========
   We are reading the initial text following the opening "<?" string of a PI 
   tag. The characters between the initial "<?" string and the first space 
   or closing "?>" string is the target text. */
      } else if( state == 2 ) {
         if( c == '>' && lc == '?' ) {
            if( text1 ) text1[ --nc1 ] = 0;
            state = 101;            
         } else if( isspace( c ) ) {
            state = 8;            
         } else {
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 3:
   ========
   We are using the characters following the opening "<!" text to
   determine if the tag is a comment or a CDATA section. */
      } else if( state == 3 ) {
         if( c == '-' ) {
            state = 4;
         } else if( c == '[' ){
            state = 5;
            AppendString( text1, &nc1, "<![" );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                      "encountered.", msg );
            break;
         }

/* State 4:
   ========
   We are looking for the start of the text within a comment tag. */
      } else if( state == 4 ) {
         if( c == '-' ) {
            state = 9;
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                      "encountered.", msg );
            break;
         }

/* State 5:
   ========
   We are looking for the start of the text within a CDATA tag. */
      } else if( state == 5 ) {
         if( c == '[' ) {
            if( !strcmp( text1, "<![CDATA" ) ) {
               state = 10;
               text1 = astFree( text1 );
            } else {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                         "encountered.", msg );
               break;
            }

         } else if( nc1 < 10 ) {
            text1 = AppendChar( text1, &nc1, c );

         } else {  
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                      "encountered.", msg );
            break;
         }

/* State 6:
   ========
   We are looking for the (prefix:)name combination at the start of an
   element start tag. */
      } else if( state == 6 ) {
         if( c == '>' ) {
            state = ( lc != '/' ) ? 102 : 108;            
         } else if( isspace( c ) ) {
            state = 103;
         } else if( c != '/' ){
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 7:
   ========
   We are looking for the end of an element end tag. */
      } else if( state == 7 ) {
         if( c == '>' ) {
            state = 104;
         } else {
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 8:
   ========
   We are reading the remaining text in a PI tag following the target
   text. */
      } else if( state == 8 ) {
         if( c == '>' && lc == '?' ) {
            if( text2 ) text2[ --nc2 ] = 0;
            state = 101;            
         } else if( text2 || !isspace( c ) ) {
            text2 = AppendChar( text2, &nc2, c );
         }

/* State 9:
   ========
   We are reading the remaining text in a comment tag. When the end ">" is
   reached, check the previous 2 characters are "--" and then terminate
   the text1 string in order to remove these two characters from the comment
   text.  */
      } else if( state == 9 ) {
         if( c == '>' && lc == '-' && lc2 == '-' ) {
            text1[ nc1 - 2 ] = 0;
            state = 105;
         } else {
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 10:
   =========
   We are reading the remaining text in a CDATA tag. */
      } else if( state == 10 ) {
         if( c == '>' && lc == ']' && lc2 == ']' ) {
            state = 106;
         } else {
            text1 = AppendChar( text1, &nc1, c );
         }

/* State 11:
   =========
   We are looking for an equals sign marking the end of an attribute name
   within an element start tag. */
      } else if( state == 11 ) {
         if( c == '=' ) {
            state = 12;

         } else if( c == '>' ) {
            if( text1 ) {
               astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s...\" "
                         "encountered.", msg );
               break;
            } else {
               if( lc == '/' ) {
                  state = 109;
               } else {
                  if( this ) {
                     state = 0;
                  } else {
                     break;
                  }
               }
            }

         } else if( text1 || !isspace( c ) ) {
            if( c != '/' ) text1 = AppendChar( text1, &nc1, c );
         }

/* State 12:
   =========
   We are looking for a '"' or ''' marking the start of an attribute value
   within an element start tag. */
      } else if( state == 12 ) {
         if( c == '"' ) {
            state = 13;

         } else if( c == '\'' ) {
            state = 14;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", text1, msg );
            break;
         }

/* State 13:
   =========
   We are looking for a '"' marking the end of an attribute value
   within an element start tag. */
      } else if( state == 13 ) {
         if( c == '"' ) {
            state = 107;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", text1, msg );
            break;

         } else {
            text2 = AppendChar( text2, &nc2, c );
         }

/* State 14:
   =========
   We are looking for a ''' marking the end of an attribute value
   within an element start tag. */
      } else if( state == 14 ) {
         if( c == '\'' ) {
            state = 107;

         } else if( c == '>' ) {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal value for attribute "
                      "\"%s\" in XML tag \"%s...\".", text1, msg );
            break;

         } else {
            text2 = AppendChar( text2, &nc2, c );
         }


      } else {
         astError( AST__INTER, "Parse(xml): Illegal state (%d) encountered "
                   "(AST internal programming error).", state );
      }



/* The following states perform actions consequent on the decisons made
   above, but which must be performed before reading the next character. */

/* In most cases there will be no actions to perform. Therefore check for
   this first (to avoid the time spent doing all the following usually 
   irrelevant checks). */
      if( state < 15 ) {

/* State 100:
   ==========
   We have just reached the start of a new tag, Store any preceeding
   character data in the container (if any). */
      } else if( state == 100 ) {
         if( text1 ){
            if( container ) astXmlAddCharData( container, text1 );
            text1 = astFree( text1 );
         }
         state = 1;

/* State 101:
   ==========
   We have just reached the end of a PI tag. Create a new XmlPI and store 
   it in the container (if any). */
      } else if( state == 101 ) {
         if( text1 ){
            if( container ) astXmlAddPI( container, text1, text2 );
            text1 = astFree( text1 );
            if( text2 ) text2 = astFree( text2 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 0;

/* State 102:
   ==========
   We have just got the (prefix:)name for an element start tag, and the
   start tag contains no attributes, etc. Create a new XmlElement and store 
   it in the container (if any). Make the new element the new container, and 
   then go on to state 0 in which we look for the start of a new tag. If
   no container element was supplied, break out of the loop returning the
   new element as the function return value. */
      } else if( state == 102 ) {
         if( text1 ){
            container = astXmlAddElement( container, text1, NULL );
            text1 = astFree( text1 );
            if( !this ) {
               result = container;
               break;
            }
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 0;

/* State 103:
   ==========
   We have just got the (prefix:)name for an element start tag, but the
   start tag may contain further attributes, etc. Create a new XmlElement 
   and store it in the container (if any). Then make the new element the
   current container, and go to state 11 in which we look for further 
   attributes, etc. */
      } else if( state == 103 ) {
         if( text1 ){
            container = astXmlAddElement( container, text1, NULL );
            if( !this ) result = container;
            text1 = astFree( text1 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 11;

/* State 104:
   ==========
   We have just reached the end of an element end tag. Check that the
   (prefix:)name is legal, and matches that of the current container (if
   any). If the current container is the supplied element ("this") we
   have finished reading the elements contents, so leave the loop.
   Otherwise, re-instate the parent container, if appropriate. */
      } else if( state == 104 ) {
         if( text1 ){
            CheckPrefName( text1, "element", "astRead(XmlChan)" );
            if( container ) {
               if( MatchName( container, text1 ) ) {
                  container->complete = 1;
                  if( container == this ) {
                     break;
                  } else {
                     container = ((AstXmlObject *) container)->parent;
                     if( !container ) {
                        astError( AST__INTER, "Parse(xml): No container after "
                                  "parsing tag %s (AST internal programming "
                                  "error).", msg );
                        break;
                     }
                  }
               } else {
                  astError( AST__XMLWF, "astRead(XmlChan): Start tag %s "
                            "closed by end tag %s.", astXmlGetTag( container, 1 ),
                            msg );
                  break;
               }
            }
            text1 = astFree( text1 );
            state = 0;            
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }

/* State 105:
   ==========
   We have just reached the end of a comment tag. Create a new XmlComment and 
   store it in the container (if any). */
      } else if( state == 105 ) {
         if( text1 ){
            if( container ) astXmlAddComment( container, text1 );
            text1 = astFree( text1 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 0;

/* State 106:
   ==========
   We have just reached the end of a CDATA tag. Create a new
   XmlCDATASection and store it in the container (if any). */
      } else if( state == 106 ) {
         if( text1 ){
            if( container ) astXmlAddCDataSection( container, text1 );
            text1 = astFree( text1 );
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 0;


/* State 107:
   ==========
   We have just reached the end of an attribute or namespace setting. Create 
   a new object and store it in the container (if any). */
            state = 0;            
      } else if( state == 107 ) {
         if( text1 ){
            if( !container ) {
               astError( AST__INTER, "Parse(xml): Container lost at state "
                         "107 (AST internal programming error).", state );
               break;   
            } 

            if( !strcmp( text1, "xmlns" ) ) {
               astXmlAddURI( container, NULL, text2 );

            } else if( !strncmp( text1, "xmlns:", 6 ) ) {
               astXmlAddURI( container, text1+6, text2 );

            } else {
               astXmlAddAttr( container, text1, text2, NULL );
            }

            text1 = astFree( text1 );
            text2 = astFree( text2 );

         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 11;


/* State 108:
   ==========
   We have just got the (prefix:)name for an empty element tag, and the
   tag contains no attributes, etc. Create a new XmlElement and store 
   it in the container (if any), and then go on to state 0 in which we look 
   for the start of a new tag. */
      } else if( state == 108 ) {
         if( text1 ){
            if( this ){
               astXmlAddElement( container, text1, NULL );
               text1 = astFree( text1 );
            } else {
               result = astXmlAddElement( container, text1, NULL );
               text1 = astFree( text1 );
               result->complete = 1;
               break;
            }
               
         } else {
            astError( AST__XMLWF, "astRead(XmlChan): Illegal XML tag \"%s\" "
                      "encountered.", msg );
            break;
         }
         state = 0;


/* State 109:
   ==========
   We have just reached the end of an empty element tag to which we have
   been adding attributes, etc. Re-instate the parent container. */
      } else if( state == 109 ) {
         if( container ) {
            if( !this ) {
               result = container;
               result->complete = 1;
               break;
            }
            container = ((AstXmlObject *) container)->parent;
            state = 0;
         } else {
            astError( AST__INTER, "Parse(xml): No container in state 109 "
                      "(AST internal programming error).", state );
            break;
         }

      } else {
         astError( AST__INTER, "Parse(xml): Illegal state (%d) encountered "
                   "(AST internal programming error).", state );
         break;
      }


/* Remember the previous two character */
      lc2 = lc;
      lc = c;
   }

/* Free any dynamic strings */
   if( text1 ) text1 = astFree( text1 );
   if( text2 ) text1 = astFree( text2 );
   if( msg ) msg = astFree( msg );

/* Terminate the supplied buffer. */
   if( !this ) buff[ ncret ] = 0;

/* Delete the returned object if an error occurred. */
   if( !astOK ) result = astXmlAnnul( result );

/* Return the result. */
   return result;
}

static char *RemoveEscapes( const char *text ){
/*
*  Name:
*     RemoveEscapes

*  Purpose:
*     Replaces entity references by corresponding ascii characters.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     char *RemoveEscapes( const char *text )

*  Description:
*     This function produces a dynamic copy of the supplied text in which 
*     occurrences of XML entity references are replaced by the corresponding 
*     ASCII text.

*  Parameters:
*     text
*        A pointer to a text string.

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

         } else if( !strncmp( c, "&lt;", 5 ) ) {
            rc = '<';
            nc= 3;

         } else if( !strncmp( c, "&gt;", 5 ) ) {
            rc = '>';
            nc= 3;

         } else if( !strncmp( c, "&apos;", 5 ) ) {
            rc = '\'';
            nc= 5;

         } else if( !strncmp( c, "&quot;", 5 ) ) {
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

static const char *ResolvePrefix( const char *prefix, AstXmlElement *elem){
/*
*  Name:
*     ResolvePrefix

*  Purpose:
*     Find the URI associated with a namespace prefix.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     const char *ResolvePrefix( const char *prefix, AstXmlElement *elem)

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

*  Returned Value:
*     Pointer to a string holding the URI, or NULL if not found.

*  Notes:
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*/

/* Local Variables: */
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
   if( !result ) result = ResolvePrefix( prefix, ((AstXmlObject *) elem )->parent );

/* Return the result. */
   return result;
}

static int Ustrcmp( const char *a, const char *b ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xml.h"
*     static int Ustrcmp( const char *a, const char *b )

*  Class Membership:
*     FitsChan member function.

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

void astXmlTrace(){
   int i;
   printf( "Current list of active XmlObject identifiers: " );
   for( i = 0; i < nobj; i++ ) printf( "%d ", existing_objects[ i ]->id );
   printf("\n");
}

