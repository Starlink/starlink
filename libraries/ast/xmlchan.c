/*
*class++
*  Name:
*     XmlChan

*  Purpose:
*     I/O Channel using XML to represent Objects.

*  Constructor Function:
c     astXmlChan
f     AST_XMLCHAN

*  Description:
*     A XmlChan is a specialised form of Channel which supports XML I/O
*     operations. Writing an Object to an XmlChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     XML description of that Object, and reading from an XmlChan will 
*     create a new Object from its XML description.
*
*     Normally, when you use an XmlChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
c     by reading and writing the resulting XML text. These functions
f     and "sink" routines which connect it to an external data store
f     by reading and writing the resulting XML text. These routines
*     should perform any conversions needed between external character 
c     encodings and the internal ASCII encoding. If no such functions 
f     encodings and the internal ASCII encoding. If no such routines
*     are supplied, a Channel will read from standard input and write 
*     to standard output.

*  Inheritance:
*     The XmlChan class inherits from the Channel class.

*  Attributes:
*     In addition to those attributes common to all Channels, every
*     XmlChan also has the following attributes:
*
*     - XmlFormat: System for formatting Objects as XML
*     - XmlIndent: Controls output of indentation and line feeds
*     - XmlLength: Controls output buffer length
*     - XmlPrefix: The namespace prefix to use when writing

*  Functions:
c     The XmlChan class does not define any new functions beyond those
f     The XmlChan class does not define any new routines beyond those
*     which are applicable to all Channels.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David Berry (Starlink)

*  History:
*     10-OCT-2003 (DSB):
*        Original version.
*     6-FEB-2004 (DSB):
*        Added XmlPrefix and XmlFormat attributes.
*     10-FEB-2004 (DSB):
*        - Added debug conditional code to keep track of memory leaks.
*        - Fixed bug which prevented more than 1 object being read from
*        an XmlChan.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS XmlChan

/* Number of spaces indentation per level of nesting */
#define INDENT_INC 3

/* The XML element name used to store an AST attribute setting */
#define ATTR "_attribute"

/* The XML element name used for an AST "isa" element */
#define ISA "_isa"

/* The XML attribute name which holds the name of the AST class which
   defines the item contained in the element. */
#define DEFINEDBY "definedby"

/* The XML attribute name which holds the name of the AST attribute */
#define NAME "name"

/* The XML attribute name which holds the value of the AST attribute */
#define VALUE "value"

/* The XML attribute name which indicates if the AST attribute value is a
   default value. */
#define DEFAULT "default"

/* The XML attribute name which indicates if the AST attribute value was
   originally a string value. */
#define QUOTED "quoted"

/* The XML attribute name which holds a description of the AST attribute. */
#define DESC "desc"

/* The XML attribute name which holds the label associated with an AST
   Object (if any). */
#define LABEL "label"

/* A string used to indicate atrue attribute value */
#define TRUE "true"

/* Format identifiers and strings */
#define UNKNOWN_FORMAT  -1
#define NATIVE_FORMAT    0
#define QUOTED_FORMAT    1
#define MAX_FORMAT       1
#define UNKNOWN_STRING   "UNKNOWN"
#define NATIVE_STRING    "NATIVE"
#define QUOTED_STRING    "QUOTED"

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "channel.h"             /* Interface for parent class */
#include "xmlchan.h"             /* Interface definition for this class */
#include "loader.h"              /* Interface to the global loader */
#include "object.h"              /* Base Object class */
#include "xml.h"                 /* AST XML facilities */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Type Definitions */
/* ================ */

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstXmlChanVtab class_vtab; /* Virtual function table */
static int class_init = 0;        /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );
static int (* parent_getfull)( AstChannel * );
static int (* parent_getcomment)( AstChannel * );

/* Text values used to represent XmlFormat values externally. These
   should be in the order defined by the associated constants above. */
static const char *xformat[2] = { NATIVE_STRING, QUOTED_STRING };

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstXmlChan *astXmlChanForId_( const char *(*)( void ), 
                           char *(*)( const char *(*)( void ) ), 
                           void (*)( const char * ), 
                           void (*)( void (*)( const char * ), const char * ),
                           const char *, ... );
AstXmlChan *astXmlChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstObject *MakeAstFromXml( AstXmlChan *, AstXmlElement * );
static AstObject *Read( AstChannel * );
static AstObject *ReadObject( AstChannel *, const char *, AstObject * );
static AstXmlElement *FindAttribute( AstXmlChan *, const char * );
static AstXmlElement *FindObject( AstXmlChan *, const char * );
static AstXmlElement *ReadXmlText( AstXmlChan * );
static AstXmlElement *Remove( AstXmlChan *, AstXmlElement * );
static char *ReadString( AstChannel *, const char *, const char * );
static char *SourceWrap( const char *(*)( void ) );
static char *SourceWrap( const char *(*)( void ) );
static const char *GetAttrib( AstObject *, const char *);
static const char *GetTag( AstXmlObject *, int );
static const char *FindNextIsA( AstXmlElement *, int );
static double ReadDouble( AstChannel *, const char *, double );
static int FindString( int, const char *[], const char *, const char *, const char *, const char * );
static int GetComment( AstChannel * );
static int GetFull( AstChannel * );
static int IsUsable( AstXmlElement * );
static int ReadInt( AstChannel *, const char *, int );
static int TestAttrib( AstObject *, const char * );
static int Use( AstXmlChan *, int, int );
static int Ustrcmp( const char *, const char * );
static int Ustrncmp( const char *, const char *, size_t );
static void ClearAttrib( AstObject *, const char * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void OutputText( AstXmlChan *, const char *, int );
static void ReadClassData( AstChannel *, const char * );
static void SetAttrib( AstObject *, const char * );
static void SinkWrap( void (*)( const char * ), const char * );
static void WriteBegin( AstChannel *, const char *, const char * );
static void WriteDouble( AstChannel *, const char *, int, int, double, const char * );
static void WriteEnd( AstChannel *, const char * );
static void WriteInt( AstChannel *, const char *, int, int, int, const char * );
static void WriteIsA( AstChannel *, const char *, const char * );
static void WriteObject( AstChannel *, const char *, int, int, AstObject *, const char * );
static void WriteString( AstChannel *, const char *, int, int, const char *, const char * );
static char GetNextChar( void * );

static int TestXmlLength( AstXmlChan * );
static void ClearXmlLength( AstXmlChan * );
static void SetXmlLength( AstXmlChan *, int );
static int GetXmlLength( AstXmlChan * );

static int TestXmlFormat( AstXmlChan * );
static void ClearXmlFormat( AstXmlChan * );
static void SetXmlFormat( AstXmlChan *, int );
static int GetXmlFormat( AstXmlChan * );

static int TestXmlIndent( AstXmlChan * );
static void ClearXmlIndent( AstXmlChan * );
static void SetXmlIndent( AstXmlChan *, int );
static int GetXmlIndent( AstXmlChan * );

static int TestXmlPrefix( AstXmlChan * );
static void ClearXmlPrefix( AstXmlChan * );
static void SetXmlPrefix( AstXmlChan *, const char * );
static const char * GetXmlPrefix( AstXmlChan * );


/* Member functions. */
/* ================= */

void astInitXmlChanVtab_(  AstXmlChanVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitXmlChanVtab

*  Purpose:
*     Initialise a virtual function table for an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     void astInitXmlChanVtab( AstXmlChanVtab *vtab, const char *name )

*  Class Membership:
*     XmlChan vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the XmlChan class.

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
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstChannelVtab *channel;      /* Pointer to Channel component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitChannelVtab( (AstChannelVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAXmlChan) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

   vtab->SetXmlIndent = SetXmlIndent;
   vtab->ClearXmlIndent = ClearXmlIndent;
   vtab->TestXmlIndent = TestXmlIndent;
   vtab->GetXmlIndent = GetXmlIndent;

   vtab->SetXmlLength = SetXmlLength;
   vtab->ClearXmlLength = ClearXmlLength;
   vtab->TestXmlLength = TestXmlLength;
   vtab->GetXmlLength = GetXmlLength;

   vtab->SetXmlFormat = SetXmlFormat;
   vtab->ClearXmlFormat = ClearXmlFormat;
   vtab->TestXmlFormat = TestXmlFormat;
   vtab->GetXmlFormat = GetXmlFormat;

   vtab->SetXmlPrefix = SetXmlPrefix;
   vtab->ClearXmlPrefix = ClearXmlPrefix;
   vtab->TestXmlPrefix = TestXmlPrefix;
   vtab->GetXmlPrefix = GetXmlPrefix;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   channel = (AstChannelVtab *) vtab;

   channel->WriteBegin = WriteBegin;
   channel->WriteIsA = WriteIsA;
   channel->WriteEnd = WriteEnd;
   channel->WriteInt = WriteInt;
   channel->WriteDouble = WriteDouble;
   channel->WriteString = WriteString;
   channel->WriteObject = WriteObject;

   channel->Read = Read;
   channel->ReadClassData = ReadClassData;
   channel->ReadDouble = ReadDouble;
   channel->ReadInt = ReadInt;
   channel->ReadObject = ReadObject;
   channel->ReadString = ReadString;

   parent_getfull = channel->GetFull;
   channel->GetFull = GetFull;
   parent_getcomment = channel->GetComment;
   channel->GetComment = GetComment;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the class dump, copy and delete functions.*/
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "XmlChan", "XML I/O channel" );
   astSetDelete( vtab, Delete );

}

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Channel member function (over-rides the astClearAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function clears the value of a specified attribute for a
*     XmlChan so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstXmlChan *this;              /* Pointer to the XmlChan structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* XmlIndent */
/* ------ */
   if ( !strcmp( attrib, "xmlindent" ) ) {
      astClearXmlIndent( this );

/* XmlLength */
/* --------- */
   } else if ( !strcmp( attrib, "xmllength" ) ) {
      astClearXmlLength( this );

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      astClearXmlFormat( this );

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      astClearXmlPrefix( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static AstXmlElement *FindAttribute( AstXmlChan *this, const char *name ) {
/*
*  Name:
*     FindAttribute

*  Purpose:
*     Find an XML element representing a named AST attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *FindAttribute( AstXmlChan *this, const char *name )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the current container element
*     of the supplied XmlChan looking for an element which represents a
*     named AST attribute. No error is reported if the attribute is not
*     found. Attributes which represent defaul values are ignored.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     name
*        Pointer to a string holding the required AST attribute name
*        (case-insensitive).

*  Returned Value:
*     A pointer to the XmlElement if found, and NULL otherwise.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item no. "i" */
   AstXmlElement *result;   /* Returned pointer */
   const char *def;         /* Value from XML DEFAULT attribute */
   const char *definedby;   /* Name of class which defines the item */
   const char *xmlname;     /* Value from XML NAME attribute */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still in the element */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Report an error if the class being loaded has not been set. */
   if( !this->isa_class ) {
      astError( AST__INTER, "astRead(XmlChan): astReadNextData not called "
                "before reading values for a %s (internal AST programming "
                "error).", astXmlGetName( this->container ) );
   }

/* Check we have a container to search. */
   if( !this->container ) {
      astError( AST__INTER, "astRead(XmlChan): No container before reading "
                "values for a %s (internal AST programming error).", 
                astXmlGetName( this->container ) );
   }

/* Check all is OK. */
   if( astOK ) {

/* Loop round all items in the elements contents. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( this->container, i );

/* Ignore this item if it is not an element. */
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* Ignore this element if its name is not ATTR. */
            if( !astOK ) break;
            if( !strcmp( astXmlGetName( item ), ATTR ) ){

/* Ignore this element if it represents a default value. */
               def = astXmlGetAttributeValue( item, DEFAULT );
               if( !def || strcmp( def, TRUE ) ) {

/* If this ATTR element has an XML attribute called NAME with
   the required value (case-insensitive), we may have found a matching 
   element. */
                   xmlname = astXmlGetAttributeValue( item, NAME );
                   if( xmlname && !Ustrcmp( xmlname, name ) ) {

/* Ignore the attribute if it does not belong to the correct part of the 
   object's class hierarchy. If it does, we have found the required
   attribute. */
                      definedby = astXmlGetAttributeValue( item, DEFINEDBY );
                      if( definedby && !strcmp( definedby, this->isa_class ) ) {
                         result = (AstXmlElement *) item;
                         break;
                      }
                   }
               }
            }
         }
      }
   }

/* Return the pointer. */
   return result;
}

static const char *FindNextIsA( AstXmlElement *elem, int start ) {
/*
*  Name:
*     FindNextIsA

*  Purpose:
*     Find the next "isa" element within an XML element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *FindNextIsA( AstXmlElement *elem, int start )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the specified element,
*     starting at the item with the speicfied index, until it finds the
*     next "isa" element. It returns the value of the "class" attribute
*     of the found "isa" element, or the name of the supplied element 
*     if no "isa" element is found.

*  Parameters:
*     elem
*        Pointer to the XmlElement (an element describing an AST Object).
*     start
*        The index of the first content item to check.

*  Returned Value:
*     A pointer to the class string.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item no. "i" */
   const char *result;      /* Returned string */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still i nthe element */

/* Initialise */
   result = astXmlGetName( elem );

/* Check the global error status. */
   if ( !astOK ) return result;

/* Loop round all items contained in the element, starting at the given
   index. */
   nitem = astXmlGetNitem( elem );
   for( i = start; i < nitem; i++ ) {   
      item = astXmlGetItem( elem, i );

/* Check this item is an XmlElement with name ISA. */
      if( astXmlCheckType( item, AST__XMLELEM ) ) {
         if( astOK && !strcmp( astXmlGetName( item ), ISA ) ) {

/* The returned string is the value of the "class" attribute of this
   element. */ 
            result = astXmlGetAttributeValue( item, "class" );

/* Report an error if the element does not have a class attribute. */
            if( !result && astOK ) {
               astError( AST__BADIN, "astRead(XmlChan): The tag %s "
                         "does not include a \"class\" attribute.", 
                         GetTag( (AstXmlObject *) item, 1 ) );
            }

            break;

         }
      }
   }

/* Return the result. */
   return result;
}

static AstXmlElement *FindObject( AstXmlChan *this, const char *name ) {
/*
*  Name:
*     FindObject

*  Purpose:
*     Find an XML element representing a named AST Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *FindObject( AstXmlChan *this, const char *name )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function searches the content of the current container element
*     of the supplied XmlChan looking for an element which represents a
*     named AST Object. No error is reported if the object is not
*     found. Objects which represent default values are ignored.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     name
*        Pointer to a string holding the required AST object name
*        (case-insensitive).

*  Returned Value:
*     A pointer to the XmlElement if found, and NULL otherwise.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item */
   AstXmlElement *result;   /* Returned pointer */
   const char *def;         /* Value from XML DEFAULT attribute */
   const char *definedby;   /* Name of class which defines the item */
   const char *xmlname;     /* Value from XML LABEL attribute */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still i nthe element */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Report an error if the class being loaded has not been set. */
   if( !this->isa_class ) {
      astError( AST__INTER, "astRead(XmlChan): astReadNextData not called "
                "before reading values for a %s (internal AST programming "
                "error).", astXmlGetName( this->container ) );
   }

/* Check we have a container to search. */
   if( !this->container ) {
      astError( AST__INTER, "astRead(XmlChan): No container before reading "
                "values for a %s (internal AST programming error).", 
                astXmlGetName( this->container ) );
   }

/* Check all is OK. */
   if( astOK ) {

/* Loop round all items in the elements contents. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( this->container, i );

/* Ignore this item if it is not an element. */
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* Ignore this element if its name is ATTR. */
            if( astOK && strcmp( astXmlGetName( item ), ATTR ) ){

/* Ignore this element if it represents a default value. */
               def = astXmlGetAttributeValue( item, DEFAULT );
               if( !def || strcmp( def, TRUE ) ) {

/* If this non-ATTR element has an XML attribute called LABEL with
   the required value (case-insensitive), we may have found a matching element. */
                   xmlname = astXmlGetAttributeValue( item, LABEL );
                   if( xmlname && !Ustrcmp( xmlname, name ) ) {

/* Ignore the element if it does not belong to the correct part of the 
   object's class hierarchy. If it does, we have found the required
   object. */
                      definedby = astXmlGetAttributeValue( item, DEFINEDBY );
                      if( definedby && !strcmp( definedby, this->isa_class ) ) {
                         result = (AstXmlElement *) item;
                         break;
                      }
                   }
               }
            }
         }
      }
   }

/* Return the pointer. */
   return result;
}

static int FindString( int n, const char *list[], const char *test, 
                       const char *text, const char *method, 
                       const char *class ){
/*
*  Name:
*     FindString

*  Purpose:
*     Find a given string within an array of character strings.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int FindString( int n, const char *list[], const char *test, 
*                     const char *text, const char *method, const char *class )

*  Class Membership:
*     XmlChan method.

*  Description:
*     This function identifies a supplied string within a supplied
*     array of valid strings, and returns the index of the string within
*     the array. The test option may not be abbreviated, but case is
*     insignificant.

*  Parameters:
*     n
*        The number of strings in the array pointed to be "list".
*     list
*        A pointer to an array of legal character strings.
*     test
*        A candidate string.
*     text
*        A string giving a description of the object, parameter,
*        attribute, etc, to which the test value refers.
*        This is only for use in constructing error messages. It should
*        start with a lower case letter.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     The index of the identified string within the supplied array, starting
*     at zero.

*  Notes:
*     -  A value of -1 is returned if an error has already occurred, or
*     if this function should fail for any reason (for instance if the
*     supplied option is not specified in the supplied list). 

*/

/* Local Variables: */
   int ret;                /* The returned index */

/* Check global status. */
   if( !astOK ) return -1;

/* Compare the test string with each element of the supplied list. Leave
   the loop when a match is found. */
   for( ret = 0; ret < n; ret++ ) {
      if( !Ustrcmp( test, list[ ret ] ) ) break;
   }

/* Report an error if the supplied test string does not match any element
   in the supplied list. */
   if( ret >= n && astOK ) {
      astError( AST__RDERR, "%s(%s): Illegal value '%s' supplied for %s.",
                method, class, test, text );
      ret = -1;
   }

/* Return the answer. */
   return ret;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetAttrib
*     method inherited from the Channel class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a XmlChan, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the XmlChan, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the XmlChan. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Integer attribute value */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* XmlIndent */
/* --------- */
   if ( !strcmp( attrib, "xmlindent" ) ) {
      ival = astGetXmlIndent( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* XmlLength */
/* --------- */
   } else if ( !strcmp( attrib, "xmllength" ) ) {
      ival = astGetXmlLength( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      ival = astGetXmlFormat( this );
      if ( astOK ) {
         if( ival == NATIVE_FORMAT ){
            result = NATIVE_STRING;

         } else if( ival == QUOTED_FORMAT ){
            result = QUOTED_STRING;

         } else {
            result = UNKNOWN_STRING;
         }
      }

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      result = astGetXmlPrefix( this );

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


static int GetComment( AstChannel *this ) {
/*
*  Name:
*     GetComment

*  Purpose:
*     Get the value of the Comment attribute of a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int GetComment( AstChannel *this )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetComment
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Comment attribute of the XmlChan.
*     It changs the default value from 1 (provided by the parent Channel
*     class) to zero.

*  Parameters:
*     this
*        Pointer to the XmlChan.

*  Returned Value:
*     - The Comment value.
*/

   return astTestComment( this ) ? (*parent_getcomment)( this ) : 0;
}

static int GetFull( AstChannel *this ) {
/*
*  Name:
*     GetFull

*  Purpose:
*     Get the value of the Full attribute of a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int GetFull( AstChannel *this )

*  Class Membership:
*     XmlChan member function (over-rides the protected astGetFull
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Full attribute of the XmlChan.
*     It changs the default value from zero (provided by the parent Channel
*     class) to -1.

*  Parameters:
*     this
*        Pointer to the XmlChan.

*  Returned Value:
*     - The Full value.
*/

   return astTestFull( this ) ? (*parent_getfull)( this ) : -1;
}

static char GetNextChar( void *data ) {
/*
*  Name:
*     GetNextChar

*  Purpose:
*     Get the next character from the XML source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char GetNextChar( void *data )

*  Class Membership:
*     XmlChan member function 

*  Description:
*     This function returns the next character from the XML source,
*     getting a new string if necessary.

*  Parameters:
*     data 
*        Pointer to a structure holding data needed to perform the read.
*        This should be a pointer to the XmlChan being read.

*  Returned Value:
*     - The next source character.

*  Notes:
*     - Zero is returned if there is no more text to read.
*     - Zero is returned if an error has already occurred, or if this
*     function should failed for any reason.
*/

/* Local Variables: */
   AstXmlChan *this;         /* Pointer to the XmlChan */
   static char *c = NULL;    /* Pointer to next character to read */
   static char *buf = NULL;  /* Pointer to previously read text */
   char result;              /* The returned character */

/* Initiialise */
   result = 0;

/* Check the global status */
   if( !astOK ) return result;

/* Get a pointer to the XmlChan. */
   this = (AstXmlChan *) data;

/* We read a new line from the source if: 1) the reset flag is set in the
   XmlChan, 2) we have reached the terminating null in the previous line,
   or 3) we do not yet have a line of text. */
   if( this->reset_source || *c == 0 || !buf ) {
      this->reset_source = 0;

/* Free the memory used to hold any previous text. */
      if( buf ) buf = astFree( buf );

/* Read a new line of text from the source. */
      buf = astGetNextText( this );

/* Read a new line if the previous line was empty. */
      while( buf && !buf[ 0 ] ) buf = astGetNextText( this );

/* Reset the pointer to the next character to the start of the new
   string. */
      c = buf;

/* If all has gone OK, return the first character and then increment c to
   point to the next character. */
      if( c && astOK ) result = *(c++);

/* If we are reading a previously read line, return the character located
   by c and increment c. */
   } else {
      result = *(c++);
   }

/* Return the result */
   return result;

}

static const char *GetTag( AstXmlObject *this, int opening ){
/*
*  Name:
*     GetTag

*  Purpose:
*     Returns a string holding an XML tag describing the given XmlObject.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     const char *GetTag( AstXmlObject *this, int opening )

*  Description:
*     This function returns a pointer to static string
*     containing an XML tag describing the given XmlObject. It is a
*     wrapper for the astXmlGetTag function defined in xml.h, but it
*     additionally removes any "definedby" attribute before formating the
*     tag (the "definedby" attribute is added by the ReadClassData
*     function and is not part of the XML text read from the source).

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
*     Pointer to a null terminated static string holding the tag. 

*  Notes:
*     - Empty elements are represented as an start tag of the form <.../>, 
*     with no corresponding end tag.
*     - NULL is returned if an error has already occurred, or if this
*     function should fail for any reason.
*-
*/

/* Local Variables: */
   AstXmlElement *elem;      /* Pointer to XML element */
   const char *result;       /* The returned pointer */
   const char *ptr;          /* The value of the "definedby" attribute */
   const char *class;        /* Copy of the value of the "definedby" attribute */

/* Initialise */
   result = NULL;

/* If the object is an element, check for the "definedby" attribute. */
   if( astXmlCheckType( this, AST__XMLELEM ) ) {
      elem = (AstXmlElement *) this;

/* See if the element contains a "definedby" attribute. */
      ptr = astXmlGetAttributeValue( elem, DEFINEDBY );

/* If so, temporarily remove it, format the tag and then put it back. */
      if( ptr ) {
         class = astStore( NULL, ptr, strlen( ptr ) + 1 );
         astXmlRemoveAttr( elem, DEFINEDBY, NULL );
         result = astXmlGetTag( elem, opening );
         astXmlAddAttr( elem, DEFINEDBY, class, NULL );
         class = astFree( (void *) class );

/* If not, just use astXmlGetTag. */
      } else {
         result = astXmlGetTag( this, opening );   
      }

/* If the object is not an element, just use astXmlGetTag. */
   } else {
      result = astXmlGetTag( this, opening );   
   }

/* Return the result. */
   return result;
}

static int IsUsable( AstXmlElement *elem ){
/*
*  Name:
*     IsUsable

*  Purpose:
*     See if an XmlElement could describe an AST object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int IsUsable( AstXmlElement *elem )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function checks if an instance of an AST class could be
*     created from the supplied XmlElement.

*  Parameters:
*     elem
*        A pointer to the XmlElement, or NULL.

*  Returned Value:
*     If an AST Object could be created from the supplied element, +1 is 
*     returned. Otherwise, -1 is returned. Zero is returned if the supplied 
*     pointer is NULL.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   const char *class;            /* Pointer to element name */
   const char *uri;              /* Pointer to namespace URI */
   int oldrep;                   /* Original value of the Reporting flag */
   int result;                   /* Result value to be returned */

/* Check the global error status, and the supplied pointer. */
   if ( !astOK || !elem ) return 0;

/* Initialise */
   result = -1;

/* Get the namespace URI for the element. */
   uri = astXmlGetURI( elem );

/* Only proceed if the URI is not defined, or if it the AST URI. */
   if( !uri || !strcmp( uri, AST__XMLNS ) ) {

/* Get the element name. This will be an AST class name if the element
   describes an AST Object. */
      class = astXmlGetName( elem );

/* Attempt to get the loader for a class of this name. If no loader exists an
   error would normally be reported. Therefore we switch off error reporting 
   before making this call. After the class we clear any error status and
   switch error reporting back on. If no error occurs whilst getting the
   loader, then the class name must be a valid AST class name and so return
   a non-zero result value. */
      if( astOK ) {
         oldrep = astReporting( 0 );
         astGetLoader( class );
         if( astOK ) {
            result = 1;
         } else {
            astClearStatus;
         }
         astReporting( oldrep );
      }
  
/* If the element is in no namespace, use the AST URI as the default
   namespace for it and its children. */
      if( !uri ) astXmlAddURI( elem, NULL, AST__XMLNS );

   }

/* Return the result. */
   return result;
}

static AstObject *MakeAstFromXml( AstXmlChan *this, AstXmlElement *elem ) {
/*
*  Name:
*     MakeAstFromXml

*  Purpose:
*     Make an AST Object from an XML element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *MakeAstFromXml( AstXmlChan *this, AstXmlElement *elem )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function makes a new AST Object from the supplied XML element.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     elem
*        Pointer to the XML element containing a description of the AST
*        object.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstLoaderType *loader;        /* Pointer to loader for Object */
   AstObject *new;               /* Pointer to returned Object */
   AstXmlParent *old_container;  /* Element from which items are being read */
   const char *class;            /* Pointer to Object class name string */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get the AST class name. This is the name of the XML element. */
   class = astXmlGetName( elem );

/* Use the associated class name to locate the loader for that
   class. This function will then be used to build the Object. */
   loader = astGetLoader( class );

/* If OK, save the pointer to the current container element, and indicate
   that the supplied element is now to be used as the current container.   
   The "current container" is the XML element from which values are being
   read. */
   if( astOK ) {
      old_container = this->container;
      this->container = (AstXmlParent *) elem;

/* The "isa_class" item in the XmlChan structure contains a pointer to
   the name of the class whose loader is currently being invoked. It is set 
   by the loader itself as a side effect of calling the astReadClassData
   function. Initialise it to NULL to indicate that astReadClassData has
   not yet been called. */
      this->isa_class = NULL;

/* Invoke the loader, which reads the Object definition from the
   current XML container (i.e. the supplied XML element) and builds the 
   Object. Supply NULL/zero values to the loader so that it will substitute 
   values appropriate to its own class. */
      new = (*loader)( NULL, (size_t) 0, NULL, NULL, (AstChannel *) this );

/* Re-instate the original container. */
      this->container = old_container;
   }

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* Return the pointer to the new Object. */
   return new;
}

static void OutputText( AstXmlChan *this, const char *text, int mxlen ) {
/*
*  Name:
*     OutputText

*  Purpose:
*     Write a string to the sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void OutputText( AstXmlChan *this, const char *text, int mxlen )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function writes the supplied text to the output sink,
*     splitting it into lines of no more than "mxlen" characters, if 
*     required.

*  Parameters:
*     this
*        A pointer to the XmlChan.
*     text
*        Pointer to the (potentially long) null terminated string to write 
*        out to the sink.
*     mxlen
*        The maximum allowed output line length. If zero, no limit is
*        placed on the output line length and the supplied text is always 
*        written out as a single string.
*/

/* Local Variables: */
   char *breakat;                /* Pointer to terminating character */
   char *c;                      /* Pointer to start of next chunk */
   char *lastend;                /* Pointer to last closing quote */
   char *lastspace;              /* Pointer to last whitespace character */
   char *linestart;              /* Pointer to start of current line */
   char quote;                   /* Opening quote character */
   char tt;                      /* Character temporarily replaced by 0 */
   int len;                      /* Length of current line */

/* Check the global error status. */
   if ( !astOK ) return;

/* If "mxlen" is zero, just output the string as supplied. */
   if( mxlen < 1 ) {
      astPutNextText( this, text );

/* Otherwise, output the string split up into lines */
   } else {
      c = (char *) text - 1;
      quote = 0;
      lastend = NULL;
      lastspace = NULL;
      len = 0;
      linestart = (char *) text;   

/* Loop round each character in the text */
      while( *(++c ) ){

/* Note if we are currently inside a quoted string. Remember the quote
   character (" or ') so that we can look out for the corresponding
   closing quote. Note the position of the previous quote end. */
         if( !quote ) {
            if( *c == '\"' || *c == '\'' ) quote = *c;
         } else {
            if( *c == quote ) {
               quote = 0;
               lastend = c;
            }
         }             

/* Note the position of hte previous space. */
         if( isspace( *c ) ) lastspace = c;

/* If we have exceeded the maximum allowed line length, split it. If we
   are inside a quote, we split it at the last quote end (if any). If we
   are not in a quote, we split it at the last space, or the last quote
   end (which ever is closest). To cover the case where the end quote is
   first character beyoind the limit, reduce the limit a bit. */
         len++;
         if( len >= mxlen - 2 ) {
            if( !quote || !lastend ) {
               if( lastend && lastspace ){
                  breakat = ( lastend > lastspace ) ? lastend + 1: lastspace;
               } else if( lastend ){
                  breakat = lastend + 1;
               } else if( lastspace ){
                  breakat = lastspace;
               } else {
                  breakat = c;
               }
            } else {
               breakat = lastend + 1;
            }
         } else {
            breakat = NULL;
         }

/* If we have a line break, output the current line. */
         if( breakat ) {

/* Terminate the string, first saving the character which is replaced by the 
   terminating zero so that it can be re-instated later. */
            tt = *breakat;
            *breakat = 0;

/* Write out the newly terminated chunk. */
            astPutNextText( this, linestart );

/* Move on to ths start of the next chunk, decrement the number of characters 
   remaining, and re-instate the character previously over-written by
   zero. */
            c = breakat;
            linestart = c;
            *c = tt;                  
            len = 0;
            quote = 0;
         }
      }

/* Write out any remaining text (this will be less than "mxlen"
   characters long)*/
      if( linestart && *linestart ) astPutNextText( this, linestart );
   }
}

static AstObject *Read( AstChannel *this_channel ) {
/*
*  Name:
*     Read

*  Purpose:
*     Read an Object from a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstObject *Read( AstChannel *this_channel ) 

*  Class Membership:
*     XmlChan member function (over-rides the astRead method
*     inherited from the Channel class).

*  Description:
*     This function reads an Object from an XmlChan.

*  Parameters:
*     this
*        Pointer to the XmlChan.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstObject *new;               /* Pointer to returned Object */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *elem;          /* XML element holding AST Object */
   int def_fmt;                  /* Original default format */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Save the current default format, and then reset it to NATIVE */
   def_fmt = this->formatdef;
   this->formatdef = NATIVE_FORMAT;

/* First we construct an in-memory XML representation of the data source,
   by reading text up to the end of the first element encountered from
   which an AST Object could be created. If the Skip attribute is zero, then 
   an error is reported if there is any text prior to the start of the first 
   usable element. If Skip is non-zero any initial text prior to the start 
   of the first usable element is ignored. */
   elem = ReadXmlText( this );

/* Check a usable element was found. */
   if( elem ) {

/* The "current container element" is the XML element from which items
   are currently being read. Indicate that we are currently not reading
   any element. */
      this->container = NULL;

/* Next we create a new AST Object from this in-memory XML representation
   of the source. */
      new = MakeAstFromXml( this, elem );

/* Remove the element. This will cause an error to be reported if
   the element contains any items which have not been used. */
      elem = Remove( this, elem );
   }

/* If an error has occurred, annul the document. */
   if( !astOK ) this->readcontext = astXmlAnnul( this->readcontext );

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer, and re-instate original default format. */
   if ( !astOK ) {
      new = astDelete( new );
      this->formatdef = def_fmt;
   }

/* Return the pointer to the new Object. */
   return new;
}

static void ReadClassData( AstChannel *this_channel, const char *class ) {
/*
*  Name:
*     ReadClassData

*  Purpose:
*     Read values from a data source for a class loader.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void ReadClassData( AstChannel *this, const char *class )

*  Class Membership:
*     XmlChan member function (over-rides the astReadClassData method
*     inherited from the Channel class).

*  Description:
*     This function reads the data for a class from the data source
*     associated with a Channel, so as to provide values for
*     initialising the instance variables of that class as part of
*     building a complete Object. This function should be invoked by
*     the loader for each class immediately before it attempts to read
*     these values.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        A pointer to a constant null-terminated string containing the
*        name of the class whose loader is requesting the data (note
*        this is not usually the same as the class name of the Object
*        being built). This value allows the class structure of the
*        input data to be validated.
*-
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlContentItem *item;      /* Pointer to next item of content */
   const char *definedby;        /* Class defining current content items */
   int nitem;                    /* Number of items in container */
   int i;                        /* Loop counter */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Check we have a container, and then store the name of the class being
   loaded. */
   if( !this->container ){
      astError( AST__INTER, "astRead(XmlChan): Invalid attempt to read "
                "%s data - there is currently no container element "
                "(internal AST programming error).", class );

   } else {
      this->isa_class = class;

/* Go through all the content elements within the current container and
   give them an extra attribute named "definedby" the value of which is
   the name of the class which defines the associated AST attribute or 
   object. This is determined by the the "isa" elements - an element is
   "definedby" the class noted in the next following "isa" element, or by
   the class being loaded if there is no following "isa" element. */

/* Find the first "isa" element and get the value of its "class" attribute. 
   If none is found the name of the class being loaded is used. */
      definedby = FindNextIsA( (AstXmlElement *) this->container, 0 );

/* Loop round all elements within the container. */
      nitem = astXmlGetNitem( this->container );
      for( i = 0; astOK && i < nitem; i++ ) {   
         item = astXmlGetItem( this->container, i );
         if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* If this is an "ISA" element, then we have ended the scope of the
   current "isa" class. All subsequent items will be defined by the class
   mentioned in the next following "ISA" element. Find the next ISA
   element and get its class. */
            if( astOK && !strcmp( astXmlGetName( item ), ISA ) ) {
               definedby = FindNextIsA( (AstXmlElement *) this->container, i + 1 );

/* For other element types, add a "definedby" attribute holding the name
   of the class defined by the current ISA element. */
            } else {
               astXmlAddAttr( item, DEFINEDBY, definedby, NULL );
            }
         }
      }
   }
}

static double ReadDouble( AstChannel *this_channel, const char *name, double def ) {
/*
*  Name:
*     ReadDouble

*  Purpose:
*     Read a double value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     double ReadDouble( AstChannel *this, const char *name, double def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadDouble method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a double value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a double
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of 0.0 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   const char *value;            /* Pointer to attribute value */
   double result;                /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */
   
/* Initialise. */
   result = 0.0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be 
   found. */
   element = FindAttribute( this, name );

/* If an element was found, attempt to decode the string to give a double 
   value, checking that the entire string is read. If this fails, then the
   wrong name has probably been given, or the input data are corrupt,
   so report an error. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         nc = 0;
         if ( !( ( 1 == astSscanf( value, " %lf %n", &result, &nc ) )
                 && ( nc >= (int) strlen( value ) ) ) ) {
            astError( AST__BADIN, "astRead(XmlChan): The value \"%s = %s\" "
                      "cannot be read as a double precision floating point "
                      "number.", name, value );

/* If the value was succesfully read, remove the ATTR element
   from the container. */
         } else {
            element = Remove( this, element );
         }

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", name, 
                   GetTag( (AstXmlObject *) element, 1 ) );
      }

/* If no suitable element was found, then use the default value instead. */
   } else {
      result = def;
   }

/* Return the result. */
   return result;
}

static int ReadInt( AstChannel *this_channel, const char *name, int def ) {
/*
*  Name:
*     ReadInt

*  Purpose:
*     Read a int value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     int ReadInt( AstChannel *this, const char *name, int def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadInt method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a int value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   const char *value;            /* Pointer to attribute value */
   int result;                   /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */
   
/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be 
   found. */
   element = FindAttribute( this, name );

/* If an element was found, attempt to decode the string to give a int 
   value, checking that the entire string is read. If this fails, then the
   wrong name has probably been given, or the input data are corrupt,
   so report an error. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         nc = 0;
         if ( !( ( 1 == astSscanf( value, " %d %n", &result, &nc ) )
                 && ( nc >= (int) strlen( value ) ) ) ) {
            astError( AST__BADIN,
                      "astRead(XmlChan): The value \"%s = %s\" cannot "
                      "be read as an integer.", name, value );

/* If the value was succesfully read, remove the ATTR element
   from the container. */
         } else {
            element = Remove( this, element );
         }

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", name, 
                   GetTag( (AstXmlObject *) element, 1 ) );
      }

/* If no suitable element was found, then use the default value instead. */
   } else {
      result = def;
   }

/* Return the result. */
   return result;
}

static AstObject *ReadObject( AstChannel *this_channel, const char *name, 
                              AstObject *def ) {
/*
*  Name:
*     ReadObject

*  Purpose:
*     Read a (sub)Object as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char *ReadObject( AstChannel *this, const char *name, AstObject *def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadObject method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify an Object with a specified name. If such an Object
*     is found, a pointer to it is returned, otherwise a default pointer
*     is returned instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     A pointer to the Object, or a clone of the default pointer if
*     the Object was not found.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   AstObject *result;            /* Value to be returned */
   const char *isa_class;        /* Class currently being loaded */
   
/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an element with a name which
   is not ATTR and with the specified LABEL. This call ignores
   elements which represent default values. No error is reported if an
   element with the given label cannot be found. */
   element = FindObject( this, name );

/* If an element was found, make an AST object from it. First remember
   the class currently being loaded so that it can be re-instated. */
   if( element ) {
      isa_class = this->isa_class;
      result = MakeAstFromXml( this, element );
      this->isa_class = isa_class;

/* Remove the element from the container. */
      element = Remove( this, element );

/* If no suitable Value structure was found, clone the default
   pointer, if given. */
   } else if ( def ) {
      result = astClone( def );
   }

/* Return the result. */
   return result;
}

static char *ReadString( AstChannel *this_channel, const char *name, const char *def ) {
/*
*  Name:
*     ReadString

*  Purpose:
*     Read a string value as part of loading a class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     char *ReadString( AstChannel *this, const char *name, const char *def )

*  Class Membership:
*     XmlChan member function (over-rides the astReadString method
*     inherited from the Channel class).

*  Description:
*     This function searches the current container element of an XmlChan to
*     identify a string value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     A pointer to a dynamically allocated null-terminated string
*     containing the value required, or to a copy of the default
*     string if the value was not found (or NULL if the "def" pointer
*     was NULL).

*  Notes:
*     - It is the caller's responsibility to arrange for the memory
*     holding the returned string to be freed (using astFree) when it
*     is no longer required.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlElement *element;       /* Pointer to element holding required value */
   char *result;                 /* Value to be returned */
   const char *value;            /* Pointer to attribute value */
   
/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Search the current container element for an ATTR element
   describing an AST attribute of the specified name. This call ignores
   ATTR elements which represent default values. No error is
   reported if an ATTR element with the given name cannot be 
   found. */
   element = FindAttribute( this, name );

/* If an element was found, return a copy of the "value" string. */
   if( element ) {
      value = astXmlGetAttributeValue( element, VALUE );
      if( value ) {
         result = astStore( NULL, value, strlen( value ) + 1 );

/* If the new default for XmlFormat has not yet been set, note if this 
   element contained a "quoted" attribute. */
         if( this->formatdef == NATIVE_FORMAT ) {
            if( astXmlGetAttributeValue( element, QUOTED ) ) {
               this->formatdef = QUOTED_FORMAT;
            }         
         }

/* Remove the ATTR element from the container. */
         element = Remove( this, element );

/* Report an error if the attribute does not have a value. */
      } else {
         astError( AST__BADIN, "astRead(XmlChan): No value for attribute "
                   "\"%s\" within element \"%s\".", name, 
                   GetTag( (AstXmlObject *) element, 1 ) );
      }

/* If no suitable Value structure was found, then make a dynamic copy
   of the default string (if given) and return a pointer to this. */
   } else if ( def ) {
      result = astStore( NULL, def, strlen( def ) + (size_t) 1 );
   }

/* Return the result. */
   return result;
}

static AstXmlElement *ReadXmlText( AstXmlChan *this ){
/*
*  Name:
*     ReadXmlText

*  Purpose:
*     Create an in-memory XML tree from an XML text source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *ReadXmlText( AstXmlChan *this )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function constructs an in-memory XML representation of the data 
*     source by reading text up to the end of the first element encountered
*     from which an AST Object could be constructed. If the Skip attribute is 
*     zero, then an error is reported if there is any text prior to the start 
*     of the first AST Object. If Skip is non-zero any initial text prior to 
*     the start of the first usable element is ignored. 

*  Parameters:
*     this
*        Pointer to the XmlChan.

*  Returned Value:
*     A pointer to the returned XmlElement. This should be annuled using
*     astXmlAnnul when no longer needed. NULL is returned if the end of
*     the source text is reached without finding a en element from which 
*     an AST Object could be read.

*  Notes:
*     - A NULL pointer is returned if an error has already occurred, of
*     if this function should fail for any reason.

*/

/* Local Variables: */
   AstXmlElement *result;   /* Returned pointer */
   int skip;                /* Skip over initial irrelevant markup? */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the value of the Skip attribute. This indicates if we should skip
   over any irrelevant markup prior to the first element from which an
   AST object could be created. */
   skip = astGetSkip( this );

/* Read characters from the XML source and return an XmlElement structure
   containing the first usable element encountered. */
   result = astXmlReadDocument( &(this->readcontext), IsUsable, skip, 
                               GetNextChar, this );

/* If no usable element was found, annul the document. */
   if( !result ) this->readcontext = astXmlAnnul( this->readcontext );

/* Delete the returned element if an error has occurred. */
   if( !astOK ) result = astXmlAnnulTree( result );

/* Return the result. */
   return result;

}

static AstXmlElement *Remove( AstXmlChan *this, AstXmlElement *element ) {
/*
*  Name:
*     Remove

*  Purpose:
*     Remove an element from the current container element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlElement *Remove( AstXmlChan *this, AstXmlElement *element )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function removes the specified element from the current
*     container element, and then annuls the removed element. An error is 
*     reported if the element being removed contains anything other than 
*     comments, "isa" elements and blank character data (all contents should 
*     have been consumed by the process of reading the object).

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     element
*        Pointer to the XML element to be removed.

*  Returned Value:
*     A NULL pointer is returned.

*/

/* Local Variables: */
   AstXmlContentItem *item; /* Item */
   const char *def;         /* Pointer to default attribute value */
   int i;                   /* Index of current item */
   int nitem;               /* Number of items still in the element */

/* Check the global error status, and the supplied element. */
   if ( !astOK || !element ) return NULL;

/* Check we have a container from which to remove the element. If so, 
   check that the container is the elements parent. If so, remove the
   element from its parent container. */
   if( this->container ) {
      if( (AstXmlParent *) this->container != astXmlGetParent( element ) ){
         astError( AST__INTER, "Remove(XmlChan): Supplied element is not "
                   "contained within the current container element (internal "
                   "AST programming error)." );
      } else {
         astXmlRemoveItem( element );
      }
   }

/* Check that the element being removed is empty (apart from comments,
   defaulted values and "isa" elements). */
   nitem = astXmlGetNitem( element );
   for( i = 0; i < nitem; i++ ) {
      item = astXmlGetItem( element, i );
      if( astXmlCheckType( item, AST__XMLELEM ) ) {

/* See if this element represents a default value */
         def = astXmlGetAttributeValue( item, DEFAULT );

/* Default values and "isa" elements are OK. */
         if( ( !def || strcmp( def, TRUE ) ) && astOK &&
             strcmp( astXmlGetName( item ), ISA ) ) {

/* Remove any "definedby" attribute (added by ReadClassData) so that it
   does not appear in the error message. */
            astXmlRemoveAttr( item, DEFINEDBY, NULL );

/* Report the error. */
            if( astOK ) astError( AST__BADIN, "astRead(XmlChan): The following "
                                  "tag was not recognised as valid input within "
                                  "a %s: %s", astXmlGetName( element ), 
                                  GetTag( (AstXmlObject *) item, 1 ) );
            break;
         }
               
/* Character data is OK so long as it contains only white space */
      } else if( astXmlCheckType( item, AST__XMLBLACK ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following character "
                   "data was not recognised as valid input within a %s: %s",
                   astXmlGetName( element ), astXmlGetValue( item, 0 ) );
         break;

      } else if( astXmlCheckType( item, AST__XMLCDATA ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following CDATA section "
                   "data was not recognised as valid input within a %s: %s",
                   astXmlGetName( element ), astXmlGetValue( item, 0 ) );
         break;

      } else if( astXmlCheckType( item, AST__XMLPI ) ) {
         astError( AST__BADIN, "astRead(XmlChan): The following processing "
                   "instruction was not recognised as valid input within "
                   "a %s: %s", astXmlGetName( element ), GetTag( (AstXmlObject *) item, 1 ) );
         break;
      }
   }

/* Renmove the element from its parent and the annul it. */
   astXmlRemoveItem( element );
   astXmlAnnul( element );   

/* Return a NULL pointer. */
   return NULL;
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     astSetAttrib

*  Purpose:
*     Set an attribute value for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     XmlChan member function (over-rides the astSetAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function assigns an attribute value for a XmlChan, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   int ival;                     /* Integer attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by "astSscanf" */
   int pr;                       /* Offset to start of string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* XmlIndent */
/* ----------*/
   if ( nc = 0,
        ( 1 == astSscanf( setting, "xmlindent= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetXmlIndent( this, ival );

/* XmlLength */
/* ----------*/
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "xmllength= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetXmlLength( this, ival );

/* XmlFormat */
/* ----------*/
   } else if( nc = 0,
        ( 0 == astSscanf( setting, "xmlformat=%n%*[^\n]%n", &ival, &nc ) )
        && ( nc >= len ) ) {

      nc = astChrLen( setting + ival );

      if( !Ustrncmp( setting + ival, NATIVE_STRING, nc ) ){
         astSetXmlFormat( this, NATIVE_FORMAT );

      } else if( !Ustrncmp( setting + ival, QUOTED_STRING, nc ) ){
         astSetXmlFormat( this, QUOTED_FORMAT );

      } else {
         astError( AST__BADAT, "astSet(%s): Unknown XML format '%s' "
                   "requested for a %s.", astGetClass( this ), setting + ival, 
                   astGetClass( this ) );
      }

/* XmlPrefix */
/* ----------*/
   } else if ( nc = 0, ( 0 == astSscanf( setting, "xmlprefix=%n%*[^\n]%n", &pr, &nc ) )
                && ( nc >= len ) ) {
      astSetXmlPrefix( this, setting + pr );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }
}

static void SinkWrap( void (* sink)( const char * ), const char *line ) {
/*
*  Name:
*     SinkWrap

*  Purpose:
*     Wrapper function to invoke a C XmlChan sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     static void SinkWrap( void (* sink)( const char * ), const char *line )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function, whose single parameter is a
*        pointer to a const, null-terminated string containing the
*        text to be written, and which returns void. This is the form
*        of XmlChan sink function employed by the C language interface
*        to the AST library.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the sink function. */
   ( *sink )( line );
}

static char *SourceWrap( const char *(* source)( void ) ) {
/*
*  Name:
*     SourceWrap

*  Purpose:
*     Wrapper function to invoke a C XmlChan source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     static char *SourceWrap( const char *(* source)( void ) )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function, with no parameters, that
*        returns a pointer to a const, null-terminated string
*        containing the text that it read. This is the form of XmlChan
*        source function employed by the C language interface to the
*        AST library.

*  Returned Value:
*     A pointer to a dynamically allocated, null terminated string
*     containing a copy of the text that was read. This string must be
*     freed by the caller (using astFree) when no longer required.
*
*     A NULL pointer will be returned if there is no more input text
*     to read.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   const char *line;             /* Pointer to input line */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the source function to read the next input line and return a
   pointer to the resulting string. */
   line = ( *source )();

/* If a string was obtained, make a dynamic copy of it and save the
   resulting pointer. */
   if ( line ) result = astString( line, (int) strlen( line ) );

/* Return the result. */
   return result;
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     XmlChan member function (over-rides the astTestAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a XmlChan's attributes.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* XmlIndent */
/* --------- */
   if ( !strcmp( attrib, "xmlindent" ) ) {
      result = astTestXmlIndent( this );

/* XmlLength */
/* --------- */
   } else if ( !strcmp( attrib, "xmllength" ) ) {
      result = astTestXmlLength( this );

/* XmlFormat */
/* --------- */
   } else if ( !strcmp( attrib, "xmlformat" ) ) {
      result = astTestXmlFormat( this );

/* XmlPrefix */
/* --------- */
   } else if ( !strcmp( attrib, "xmlprefix" ) ) {
      result = astTestXmlPrefix( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

static int Use( AstXmlChan *this, int set, int helpful ) {
/*
*  Name:
*     Use

*  Purpose:
*     Decide whether to write a value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int Use( AstXmlChan *this, int set, int helpful )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     This function decides whether a value supplied by a class "Dump"
*     function, via a call to one of the astWrite... protected
*     methods, should actually be written to the data sink associated
*     with a XmlChan.
*
*     This decision is based on the settings of the "set" and
*     "helpful" flags supplied to the astWrite... method, plus the
*     attribute settings of the XmlChan.

*  Parameters:
*     this
*        A pointer to the XmlChan.
*     set
*        The "set" flag supplied.
*     helpful
*        The "helpful" value supplied.

*  Returned Value:
*     One if the value should be written out, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int full;                     /* Full attribute value */
   int result;                   /* Result value to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If "set" is non-zero, then so is the result ("set" values must
   always be written out). */
   result = ( set != 0 );

/* Otherwise, obtain the value of the XmlChan's Full attribute. */
   if ( !set ) {
      full = astGetFull( this );

/* If Full is positive, display all values, if zero, display only
   "helpful" values, if negative, display no (un-"set") values. */
      if ( astOK ) result = ( ( helpful && ( full > -1 ) ) || ( full > 0 ) );
   }

/* Return the result. */
   return result;
}

static int Ustrcmp( const char *a, const char *b ){
/*
*  Name:
*     Ustrcmp

*  Purpose:
*     A case blind version of strcmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     static int Ustrcmp( const char *a, const char *b )

*  Class Membership:
*     XmlChan member function.

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

static int Ustrncmp( const char *a, const char *b, size_t n ){
/*
*  Name:
*     Ustrncmp

*  Purpose:
*     A case blind version of strncmp.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     static int Ustrncmp( const char *a, const char *b, size_t n )

*  Class Membership:
*     XmlChan member function.

*  Description:
*     Returns 0 if there are no differences between the first "n"
*     characters of the two strings, and 1 otherwise. Comparisons are
*     case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     n
*        The maximum number of characters to compare.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strncmp" does.
*     -  This function attempts to execute even if an error has occurred. 

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int i;                  /* Character index */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Compare up to "n" characters. */
   for( i = 0; i < (int) n; i++ ){

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

static void WriteBegin( AstChannel *this_channel, const char *class,
                        const char *comment ) {
/*
*  Name:
*     WriteBegin

*  Purpose:
*     Write a "Begin" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteBegin( AstChannel *this_channel, const char *class,
*                      const char *comment )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteBegin method inherited from the Channel class).

*  Description:
*     This function writes a "Begin" data item to the data sink
*     associated with a Channel, so as to begin the output of a new
*     Object definition.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class to which the Object belongs.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "Begin"
*        item. Normally, this will describe the purpose of the Object.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*/

/* Local Variables: */
   AstXmlChan *this;         /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;      /* The XML element to hodl the new AST object */
   const char *pref;         /* XML namespace prefix to use */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If this is a top level object (i.e. if there is no container element),
   reset all the other values in the XmlChan for safety. */
   if( !this->container ) {
      this->objectname = NULL;   
      this->objectset = 1;       
      this->objectcomment = NULL;
   }

/* Initialise a flag to indicate that the next "IsA" item should not be
   written. This flag will be changed if and when an item is added which
   related to the class described by the "IsA" item. Save the old value
   first. */
   this->write_isa = 0;

/* Store the namespace prefix. */
   pref = astGetXmlPrefix( this );

/* Create a new XmlElement with a name equal to the AST class name of the
   object being dumped (and no namespace prefix), and add it into the
   current container (i.e. parent) element. */
   elem = astXmlAddElement( this->container, class, pref );

/* If this is a top level container, store the namespace URI for 
   the element, either default or named depending on the value of
   XmlPrefix. */
   if( !this->container ) astXmlAddURI( elem, pref, AST__XMLNS );

/* If non-blank, append a "Label" atttribute to the element holding the
   name of the object (stored in the XmlChan structure). */
   if( this->objectname ) astXmlAddAttr( elem, LABEL, this->objectname, NULL );

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
   if( !this->objectset ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Add commments if required. */
   if( astGetComment( this_channel ) ) {

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. This comment describes the class function as a whole, not
   the specific usage of this instance of the class (this is given by the
   comment in this->objectcomment). */
      if( comment && *comment ) astXmlAddComment( elem, 0, comment );

/* If the object has a usage comment, add it to the content of the
   element if required. */
      if( this->objectcomment ) astXmlAddAttr( elem, DESC, this->objectcomment, NULL );
   }

/* Make the new element the current container. */
   this->container = (AstXmlParent *) elem;

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteDouble( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         double value, const char *comment ) {
/*
*  Name:
*     WriteDouble

*  Purpose:
*     Write a double value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteDouble( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       double value, const char *comment )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteDouble method inherited from the Channel class).

*  Description:
*     This function writes a named double value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*/

/* Local Constants: */
#define BUFF_LEN 100             /* Size of local formatting buffer */

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace 
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* Format the value as a string and store it as the VALUE attribute.
   Make sure "-0" isn't produced. */
         (void) sprintf( buff, "%.*g", DBL_DIG, value );
         if ( !strcmp( buff, "-0" ) ) {
            buff[ 0 ] = '0';
            buff[ 1 ] = '\0';
         }
         astXmlAddAttr( elem, VALUE, buff, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void WriteEnd( AstChannel *this_channel, const char *class ) {
/*
*  Name:
*     WriteEnd

*  Purpose:
*     Write an "End" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteEnd( AstChannel *this, const char *class )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteEnd method inherited from the Channel class).

*  Description:
*     This function writes an "End" data item to the data sink
*     associated with a Channel. This item delimits the end of an
*     Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        class name of the Object whose definition is being terminated
*        by the "End" item.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to the XmlChan structure */
   AstXmlParent *parent;         /* Pointer to parent element */
   char *d;                      /* Pointer to end of next sub-string */
   char *c;                      /* Pointer to start of next sub-string */
   char *text;                   /* Pointer to complete string */
   int mxlen;                    /* Max allowed length of text */

#ifdef DEBUG
   int nobj;                     /* No. of XmlObjects in existence */
#endif

/* Check the global error status. */
   if ( !astOK ) return;

#ifdef DEBUG
/* Save the number of XmlObjects currently in existenece. */
   nobj = astXmlTrace(3);
#endif

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* Get the parent of the current container element. */
   if( this->container ) {
      parent = astXmlGetParent( this->container );      

/* If the current container element has no parent, we have completed the
   construction of the in-memory XML representation of the AST object being
   written out. In this case, we convert the in-memory representation
   into a set of strings and write them out using the supplied sink
   function. */
      if( !parent ) {

/* First get a single string holding the complete formatted XML
   representation of the AST object. */
         if( astGetXmlIndent( this ) ) {
            text = (char *) astXmlShow( this->container );
         } else {
            text = (char *) astXmlFormat( this->container );
         }

/* Now, if we have any text, split it into separate lines. The end of a line 
   is indicated by a "\n" character in the text returned by astXmlFormat. */
         if( text ) {

/* Get the maximum allowed line length. */
            mxlen = astGetXmlLength( this );

/* Loop round locating each '\n' character in the string. Replace the
   '\n' character by 0, so that the previous part of the string is then 
   null terminated, and write it out using the astPutNextText method
   (splitting the text up into lines no longer than "mxlen"). */
            c = text;
            d = strchr( c, '\n' );
            while( d ) {
               *d = 0;
               OutputText( this, c, mxlen );
               c = d + 1;
               d = strchr( c, '\n' );
            }

/* Write out any text following the last '\n' character. */
            if( *c ) OutputText( this, c, mxlen );

/* Free the memory holding the text and in-memory representations of the AST 
   Object. */
            text = astFree( (void *) text );
            astXmlRemoveItem( this->container );
            this->container = astXmlAnnul( this->container );

#ifdef DEBUG
/* Report an error if there is a memory leak. */
            if( astXmlTrace(3) > nobj && astOK ) {
               astError( AST__INTER, "astWriteEnd(XmlChan): %d XmlObjects "
                         "remain in existence - should be %d (internal AST "
                         "programming error).", astXmlTrace(3), nobj );
            }
#endif

         }
      }

/* Reset the current container element to be the parent found above. */
      if( !parent || astXmlCheckType( parent, AST__XMLELEM ) ) {
         this->container = parent;
      } else if( astOK ) {
         astError( AST__INTER, "astWriteEnd(XmlChan): Cannot update "
                   "container: parent is not an XmlElement (internal "
                   "AST programming error)." );
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteInt( AstChannel *this_channel, const char *name, int set, int helpful,
                      int value, const char *comment ) {
/*
*  Name:
*     WriteInt

*  Purpose:
*     Write an integer value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteInt( AstChannel *this, const char *name, int set, int helpful,
*                    int value, const char *comment ) 

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteInt method inherited from the Channel class).

*  Description:
*     This function writes a named integer value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*/

/* Local Constants: */
#define BUFF_LEN 50             /* Size of local formatting buffer */

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace 
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* Format the value as a decimal string and add it to the element as the
   VALUE attribute. */
         (void) sprintf( buff, "%d", value );
         astXmlAddAttr( elem, VALUE, buff, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void WriteIsA( AstChannel *this_channel, const char *class,
                      const char *comment ) {
/*
*  Name:
*     WriteIsA

*  Purpose:
*     Write an "IsA" data item to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteIsA( AstChannel *this, const char *class,
*                    const char *comment )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteIsA method inherited from the Channel class).

*  Description:
*     This function writes an "IsA" data item to the data sink
*     associated with a Channel. This item delimits the end of the
*     data associated with the instance variables of a class, as part
*     of an overall Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class whose data are terminated by the "IsA"
*        item.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "IsA"
*        item. Normally, this will describe the purpose of the class
*        whose data are being terminated.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*/

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Output an "IsA" item only if there has been at least one item
   written since the last "Begin" or "IsA" item, or if the Full
   attribute for the Channel is greater than zero (requesting maximum
   information). */
      if ( this->write_isa || astGetFull( this ) > 0 ) {

/* Create a new XmlElement with a name of "_isa" (and no namespace prefix), 
   and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ISA, 
                                  astGetXmlPrefix( this ) );

/* Add a "class" attribute to this element containing the class name. */
         astXmlAddAttr( elem, "class", class, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. This comment describes the class function as a whole, not
   the specific usage of this instance of the class. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }
      }
   }

/* Initialise a flag to indicate that the next "IsA" item should not be
   written. This flag will be changed if and when an item is added which
   related to the class described by the "IsA" item. */
   this->write_isa = 0;

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );
}

static void WriteObject( AstChannel *this_channel, const char *name,
                         int set, int helpful,
                         AstObject *value, const char *comment ) {
/*
*  Name:
*     WriteObject

*  Purpose:
*     Write an Object as a value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteObject( AstChannel *this_channel, const char *name,
*                       int set, int helpful,
*                       AstObject *value, const char *comment )

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteObject method inherited from the Channel class).

*  Description:
*     This function writes an Object as a named value, representing
*     the value of a class instance variable, to the data sink
*     associated with an XmlChan. It is intended for use by class
*     "Dump" functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the XmlChan.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations.
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        A Pointer to the Object to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*/

/* Local Variables: */
   AstXmlChan *this;         /* A pointer to the XmlChan structure. */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out the object. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if ( Use( this, set, helpful ) ) {

/* Save the supplied name associated with the object being written so
   that it is available for use within the following invocation of the 
   WriteBegin method. The name is stored within the XmlChan structure
   (NULL is used to indicate "no name supplied"). */
         this->objectname = ( name && strlen( name ) ) ? name : NULL;
      
/* Also save the supplied comment and a flag indicating if the object is
   set. These will be used by the WriteBegin method. They are stored within 
   the XmlChan structure. */
         this->objectset = set;
         this->objectcomment = comment;

/* Write the object to the XmlChan. */
         (void) astWrite( this, value );

/* Nullify the components of the XmlChan set above. */
         this->objectname = NULL;
         this->objectset = 1;
         this->objectcomment = NULL;

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

static void WriteString( AstChannel *this_channel, const char *name, int set, 
                         int helpful, const char *value, const char *comment ){
/*
*  Name:
*     WriteString

*  Purpose:
*     Write a string value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     void WriteString( AstChannel *this, const char *name, int set, int helpful,
*                       const char *value, const char *comment ) 

*  Class Membership:
*     XmlChan member function (over-rides the protected
*     astWriteString method inherited from the Channel class).

*  Description:
*     This function writes a named string value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        Pointer to a constant null-terminated string containing the
*        value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*/

/* Local Variables: */
   AstXmlChan *this;             /* A pointer to the XmlChan structure. */
   AstXmlElement *elem;          /* Pointer to new element */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_channel;

/* If the object to be written is a component of a default AST object (i.e.
   an object which is "not set"), then we do not write out this item. */
   if( this->objectset ) {

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
      if( Use( this, set, helpful ) ) {

/* Create a new XmlElement with a name of ATTR (and no namespace 
   prefix), and add it into the current container (i.e. parent) element. */
         elem = astXmlAddElement( this->container, ATTR,
                                  astGetXmlPrefix( this ) );

/* Add a NAME attribute to this element containing the item name. */
         astXmlAddAttr( elem, NAME, name, NULL );

/* If we are using QUOTED format, add an attribute to indicate that this is a 
   string value (mainly included for compatibility with JNIAST). */
         if( astGetXmlFormat( this ) == QUOTED_FORMAT ) {
            astXmlAddAttr( elem, QUOTED, TRUE, NULL );
         }

/* Add it the value to the element as the VALUE attribute. */
         astXmlAddAttr( elem, VALUE, value, NULL );

/* If we are adding comments, and if a comment was supplied as a
   parameter to this function, then store the commment as an attribute of
   the element. */
         if( comment && *comment && astGetComment( this_channel ) ) {
            astXmlAddAttr( elem, DESC, comment, NULL );
         }

/* If the object has all default values, store a true value for the
   DEFAULT attribute. */
         if( !set ) astXmlAddAttr( elem, DEFAULT, TRUE, NULL );

/* Initialise a flag to indicate that the next "IsA" item should be
   written. */
         this->write_isa = 1;
      }
   }

/* If an error has occurred, annul the container element in the XmlChan. */
   if( !astOK ) this->container = astXmlAnnulTree( this->container );

}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* XmlFormat */
/* ========= */
/*
*att++
*  Name:
*     XmlFormat

*  Purpose:
*     System for formatting Objects as XML.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the formatting system to use when AST
*     Objects are written out as XML through an XmlChan. It
c     affects the behaviour of the astWrite function when
f     affects the behaviour of the AST_WRITE routine  when
*     they are used to transfer any AST Object to or from an external
*     XML representation.
*
*     The XmlChan class allows AST objects to be represented in the form
*     of XML in several ways (conventions) and the XmlFormat attribute is 
*     used to specify which of these should be used. The formatting options 
*     available are outlined in the "Formats Available" section below.
*
*     By default, an XmlChan will attempt to determine which format system
*     is already in use, and will set the default XmlFormat value
*     accordingly (so that subsequent I/O operations adopt the same
*     conventions). It does this by looking for certain critical items
*     which only occur in particular formats. For details of how this
*     works, see the "Choice of Default Format" section below. If you wish
*     to ensure that a particular format system is used, independently of
*     any XML already read, you should set an explicit XmlFormat value
*     yourself.
*
*  Formats Available:
*     The XmlFormat attribute can take any of the following (case
*     insensitive) string values to select the corresponding formatting
*     system:
*
*     - "NATIVE": This is a direct conversion to XML of the heirarchical
*     format used by a standard XML channel (and also by the NATIVE
*     encoding of a FitsChan).
*
*     - "QUOTED": This is the same as NATIVE format except that extra
*     information is included which allows client code to convert the
*     XML into a form which can be read by a standard AST Channel. This
*     extra information indicates which AST attribute values should be
*     enclosed in quotes before being passed to a Channel. 

*  Choice of Default Format;
*     If the XmlFormat attribute of an XmlChan is not set, the default
*     value it takes is determined by the presence of certain critical
*     items within the document most recently read using 
c     astRead.
f     AST_READ.
*     The sequence of decision used to arrive at the default value is as 
*     follows:
*
*     - If the previous document read contained any elements in the AST
*     namespace which had an associated XML attribute called "quoted", then
*     the default value is QUOTED.
*     - Otherwise, if none of these conditions is met (as would be the
*     case if no document had yet been read), then NATIVE format is
*     used.
*
*     Setting an explicit value for the XmlFormat attribute always
*     over-rides this default behaviour.

*  Applicability:
*     XmlChan
*        All XmlChans have this attribute.
*att--
*/
astMAKE_CLEAR(XmlChan,XmlFormat,xmlformat,UNKNOWN_FORMAT)
astMAKE_SET(XmlChan,XmlFormat,int,xmlformat,( 
   value == NATIVE_FORMAT || 
   value == QUOTED_FORMAT ? value : 
   (astError( AST__BADAT, "astSetXmlFormat: Unknown XML formatting system %d "
              "supplied.", value ), UNKNOWN_FORMAT )))
astMAKE_TEST(XmlChan,XmlFormat,( this->xmlformat != UNKNOWN_FORMAT ))
astMAKE_GET(XmlChan,XmlFormat,int,0,(this->xmlformat == UNKNOWN_FORMAT ? 
                                this->formatdef : this->xmlformat))

/*
*att++
*  Name:
*     XmlIndent

*  Purpose:
*     Controls output of indentation and line feeds.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls the appearance of the XML produced when an
*     AST object is written to an XmlChan. If it is non-zero, then extra
*     linefeed characters will be inserted as necessary to ensure that each 
*     XML tag starts on a new line, and each tag will be indented to show 
*     its depth in the containment hierarchy. If XmlIndent is zero (the
*     default), then no linefeeds or indentation strings will be added to
*     output text.

*  Applicability:
*     XmlChan
*        All XmlChans have this attribute.
*att--
*/
astMAKE_CLEAR(XmlChan,XmlIndent,xmlindent,-1)
astMAKE_GET(XmlChan,XmlIndent,int,0,(this->xmlindent == -1 ? 0 : this->xmlindent))
astMAKE_SET(XmlChan,XmlIndent,int,xmlindent,( value ? 1 : 0 ))
astMAKE_TEST(XmlChan,XmlIndent,( this->xmlindent != -1 ))

/*
*att++
*  Name:
*     XmlLength

*  Purpose:
*     Controls output buffer length.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute specifies the maximum length to use when writing out 
*     text through the sink function supplied when the XmlChan was created.
*
*     The number of characters in each string written out through the sink 
*     function will not be greater than the value of this attribute (but
*     may be less). A value of zero (the default) means there is no limit - 
*     each string can be of any length.
*
f     Note, the default value of zero is unlikely to be appropriate when
f     an XmlChan is used within Fortran code. In this case, XmlLength
f     should usually be set to the size of the CHARACTER variable used to 
f     receive the text returned by AST_GETLINE within the sink function.
f     This avoids the possibility of long lines being truncated invisibly
f     within AST_GETLINE.

*  Applicability:
*     XmlChan
*        All XmlChans have this attribute.
*att--
*/
astMAKE_CLEAR(XmlChan,XmlLength,xmllength,-INT_MAX)
astMAKE_GET(XmlChan,XmlLength,int,0,( ( this->xmllength != -INT_MAX ) ? this->xmllength : 0 ))
astMAKE_SET(XmlChan,XmlLength,int,xmllength,(value<0?0:value))
astMAKE_TEST(XmlChan,XmlLength,( this->xmllength != -INT_MAX ))

/*
*att++
*  Name:
*     XmlPrefix

*  Purpose:
*     The namespace prefix to use when writing.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute is a string which is to be used as the namespace
*     prefix for all XML elements created as a result of writing an AST
*     Object out through an XmlChan. The URI associated with the namespace
*     prefix is given by the symbolic constant AST__XMLNS defined in 
f     AST_PAR.
c     ast.h.
*     A definition of the namespace prefix is included in each top-level
*     element produced by the XmlChan.
*
*     The default value is a blank string which causes no prefix to be
*     used. In this case each top-level element will set the default 
*     namespace to be the value of AST__XMLNS.

*  Applicability:
*     Object
*        All Objects have this attribute.

*att--
*/
astMAKE_CLEAR(XmlChan,XmlPrefix,xmlprefix,astFree( this->xmlprefix ))
astMAKE_GET(XmlChan,XmlPrefix,const char *,NULL,( this->xmlprefix ? this->xmlprefix : "" ))
astMAKE_SET(XmlChan,XmlPrefix,const char *,xmlprefix,astStore( this->xmlprefix, value,
                                                strlen( value ) + (size_t) 1 ))
astMAKE_TEST(XmlChan,XmlPrefix,( this->xmlprefix != NULL ))


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for XmlChan objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstXmlChan *in;               /* Pointer to input XmlChan */
   AstXmlChan *out;              /* Pointer to output XmlChan */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output XmlChans. */
   in = (AstXmlChan *) objin;
   out = (AstXmlChan *) objout;

/* Clear the non-persistent values in the new XmlChan. */
   out->objectname = NULL;   /* Name of object being written */
   out->objectset = 1;       /* Is object being written set?*/
   out->objectcomment = NULL;/* Comment for object class being written */
   out->readcontext = NULL;  /* XmlElement giving context for current read */ 
   out->container = NULL;    /* XmlElement to which content will be added */ 
   out->write_isa = 0;       /* Write out the next "IsA" item? */
   out->reset_source = 1;    /* A new line should be read from the source */
   out->isa_class = NULL;    /* Class being loaded */

/* Store a copy of the prefix string.*/
   if ( in->xmlprefix ) out->xmlprefix = astStore( NULL, in->xmlprefix,
                                           strlen( in->xmlprefix ) + (size_t) 1 );
}


/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for XmlChan objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstXmlChan *this;             /* Pointer to XmlChan */

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) obj;

/* Free any unread part of the document. */
   this->readcontext = astXmlAnnul( this->readcontext );

/* Free the memory used for the XmlPrefix string if necessary. */
   this->xmlprefix = astFree( this->xmlprefix );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for XmlChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the XmlChan class to an output Channel.

*  Parameters:
*     this
*        Pointer to the XmlChan whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstXmlChan *this;            /* Pointer to the XmlChan structure */
   const char *class;           /* Class name */
   const char *sval;            /* String attribute value */
   int ival;                    /* Integer attribute value */
   int set;                     /* Has the attribute got a set value? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the XmlChan structure. */
   this = (AstXmlChan *) this_object;

/* Store the object class. */
   class = astGetClass( this );

/* Write out values representing the instance variables for the
   XmlChan class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/


/* Now do instance variables which are not attributes. */
/* =================================================== */

/* XmlIndent */
/* --------- */
      set = TestXmlIndent( this );
      ival = set ? GetXmlIndent( this ) : astGetXmlIndent( this );
      astWriteInt( channel, "XmlInd", set, 0, ival, "XML indentation" );

/* XmlLength */
/* --------- */
      set = TestXmlLength( this );
      ival = set ? GetXmlLength( this ) : astGetXmlLength( this );
      astWriteInt( channel, "XmlLen", set, 0, ival, "XML buffer length" );

/* XmlFormat. */
/* --------- */
   set = TestXmlFormat( this );
   ival = set ? GetXmlFormat( this ) : astGetXmlFormat( this );
   if( ival > UNKNOWN_FORMAT && ival <= MAX_FORMAT ) {
      astWriteString( channel, "XmlFmt", set, 1, xformat[ival], "Formatting system" );
   } else {
      astWriteString( channel, "XmlFmt", set, 1, UNKNOWN_STRING, "Formatting system" );
   }

/* XmlPrefix */
/* --------- */
      set = TestXmlPrefix( this );
      sval = set ? GetXmlPrefix( this ) : astGetXmlPrefix( this );
      astWriteString( channel, "XmlPrf", set, 1, sval,
                      "Namespace prefix" );
}


/* Standard class functions. */
/* ========================= */
/* Implement the astIsAXmlChan and astCheckXmlChan functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(XmlChan,Channel,check,&class_init)
astMAKE_CHECK(XmlChan)

AstXmlChan *astXmlChan_( const char *(* source)( void ),
                         void (* sink)( const char * ),
                         const char *options, ... ) {
/*
*++
*  Name:
c     astXmlChan
f     AST_XMLCHAN

*  Purpose:
*     Create an XmlChan.

*  Type:
*     Public function.

*  Synopsis:
c     #include "xmlchan.h"
c     AstXmlChan *astXmlChan( const char *(* source)( void ),
c                             void (* sink)( const char * ),
c                             const char *options, ... )
f     RESULT = AST_XMLCHAN( SOURCE, SINK, OPTIONS, STATUS )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function creates a new XmlChan and optionally initialises
*     its attributes.
*
*     A XmlChan is a specialised form of Channel which supports XML I/O
*     operations. Writing an Object to an XmlChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     XML description of that Object, and reading from an XmlChan will 
*     create a new Object from its XML description.
*
*     Normally, when you use an XmlChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
f     and "sink" routines which connect it to an external data store
*     by reading and writing the resulting XML text. By default, however,
*     a Channel will read from standard input and write to standard
*     output.

*  Parameters:
c     source
f     SOURCE = SUBROUTINE (Given)
c        Pointer to a source function that takes no arguments and
c        returns a pointer to a null-terminated string.  This function
c        will be used by the XmlChan to obtain lines of input text. On
c        each invocation, it should return a pointer to the next input
c        line read from some external XML data store, and a NULL pointer
c        when there are no more lines to read.
c
c        If "source" is NULL, the Channel will read from standard
c        input instead.
f        A source routine, which is a subroutine which takes a single
f        integer error status argument.  This routine will be used by
f        the XmlChan to obtain lines of input text. On each
f        invocation, it should read the next input line from some
f        external XML data store, and then return the resulting text to
f        the AST library by calling AST_PUTLINE. It should supply a
f        negative line length when there are no more lines to read.
f        If an error occurs, it should set its own error status
f        argument to an error value before returning.
f
f        If the null routine AST_NULL is suppied as the SOURCE value,
f        the Channel will read from standard input instead.
c     sink
f     SINK = SUBROUTINE (Given)
c        Pointer to a sink function that takes a pointer to a
c        null-terminated string as an argument and returns void.  This
c        function will be used by the XmlChan to deliver lines of
c        output text. On each invocation, it should deliver the
c        contents of the string supplied to some external XML data store.
c
c        If "sink" is NULL, the XmlChan will write to standard output
c        instead.
f        A sink routine, which is a subroutine which takes a single
f        integer error status argument.  This routine will be used by
f        the XmlChan to deliver lines of output text. On each
f        invocation, it should obtain the next output line from the
f        AST library by calling AST_GETLINE, and then deliver the
f        resulting text to some external XML data store.  If an error
f        occurs, it should set its own error status argument to an
f        error value before returning.
f
f        If the null routine AST_NULL is suppied as the SINK value,
f        the Channel will write to standard output instead.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new XmlChan. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new XmlChan. The syntax used is identical to that for the
f        AST_SET routine.
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
c     astXmlChan()
f     AST_XMLCHAN = INTEGER
*        A pointer to the new XmlChan.

*  Notes:
f     - The names of the routines supplied for the SOURCE and SINK
f     arguments should appear in EXTERNAL statements in the Fortran
f     routine which invokes AST_XMLCHAN. However, this is not generally
f     necessary for the null routine AST_NULL (so long as the AST_PAR
f     include file has been used).
*     - If the external data source or sink uses a character encoding
*     other than ASCII, the supplied source and sink functions should
*     translate between the external character encoding and the internal 
*     ASCII encoding used by AST.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
f     - Note that the null routine AST_NULL (one underscore) is
f     different to AST__NULL (two underscores), which is the null Object
f     pointer.
*--
*/

/* Local Variables: */
   AstXmlChan *new;             /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by other C functions within AST, and uses the standard "wrapper"
   functions included in this class. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init, 
                          &class_vtab, "XmlChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new XmlChan. */
   return new;
}

AstXmlChan *astXmlChanId_( const char *(* source)( void ),
                           void (* sink)( const char * ),
                           const char *options, ... ) {
/*
*  Name:
*     astXmlChanId_

*  Purpose:
*     Create an XmlChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astXmlChanId_( const char *(* source)( void ),
*                                void (* sink)( const char * ),
*                                const char *options, ... )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function implements the external (public) C interface to the
*     astXmlChan constructor function. Another function (astXmlChanForId)
*     should be called to create an XmlChan for use within other languages.
*     Both functions return an ID value (instead of a true C pointer) to 
*     external users, and must be provided because astXmlChan_ has a variable 
*     argument list which cannot be encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astXmlChan_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astXmlChan_.

*  Returned Value:
*     The ID value associated with the new XmlChan.
*/

/* Local Variables: */
   AstXmlChan *new;             /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by external C functions and uses the standard "wrapper"
   functions included in this class. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init, 
                         &class_vtab, "XmlChan", source, SourceWrap, 
                         sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new XmlChan. */
   return astMakeId( new );
}

AstXmlChan *astXmlChanForId_( const char *(* source)( void ),
                              char *(* source_wrap)( const char *(*)( void ) ),
                              void (* sink)( const char * ),
                              void (* sink_wrap)( void (*)( const char * ),
                                                  const char * ),
                              const char *options, ... ) {
/*
*+
*  Name:
*     astXmlChanFor

*  Purpose:
*     Initialise an XmlChan from a foreign language interface.

*  Type:
*     Public function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astXmlChanFor( const char *(* source)( void ),
*                                char *(* source_wrap)( const char *(*)
*                                                       ( void ) ),
*                                void (* sink)( const char * ),
*                                void (* sink_wrap)( void (*)( const char * ),
*                                                    const char * ),
*                                const char *options, ... )

*  Class Membership:
*     XmlChan constructor.

*  Description:
*     This function creates a new XmlChan from a foreign language
*     interface and optionally initialises its attributes.
*
*     A XmlChan implements low-level XML input/output for the AST library.
*     Writing an Object to an XmlChan (using astWrite) will generate a
*     XML representation of that Object, and reading from a
*     XmlChan (using astRead) will create a new Object from its
*     XML representation.
*
*     Normally, when you use an XmlChan, you should provide "source"
*     and "sink" functions which connect it to an external data store
*     by reading and writing the resulting text. This function also
*     requires you to provide "wrapper" functions which will invoke
*     the source and sink functions. By default, however, an XmlChan
*     will read from standard input and write to standard output.

*  Parameters:
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of input text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the XmlChan will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the XmlChan will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of output text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the XmlChan will write to standard output
*        instead.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new XmlChan. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astXmlChanFor()
*        A pointer to the new XmlChan.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*     - This function is only available through the public interface
*     to the XmlChan class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-

*  Implememtation Notes:
*     - This function behaves exactly like astXmlChanId_, in that it
*     returns ID values and not true C pointers, but it has two
*     additional arguments. These are pointers to the "wrapper
*     functions" which are needed to accommodate foreign language
*     interfaces.
*/

/* Local Variables: */
   AstXmlChan *new;             /* Pointer to new XmlChan */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the XmlChan, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitXmlChan( NULL, sizeof( AstXmlChan ), !class_init, 
                         &class_vtab, "XmlChan", source, source_wrap,
                         sink, sink_wrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   XmlChan's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new XmlChan. */
   return astMakeId( new );
}

AstXmlChan *astInitXmlChan_( void *mem, size_t size, int init,
                             AstXmlChanVtab *vtab, const char *name,
                             const char *(* source)( void ),
                             char *(* source_wrap)( const char *(*)( void ) ),
                             void (* sink)( const char * ),
                             void (* sink_wrap)( void (*)( const char * ),
                                                 const char * ) ) {
/*
*+
*  Name:
*     astInitXmlChan

*  Purpose:
*     Initialise an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astInitXmlChan( void *mem, size_t size, int init,
*                                 AstXmlChanVtab *vtab, const char *name,
*                                 const char *(* source)( void ),
*                                 char *(* source_wrap)( const char *(*)( void ) ),
*                                 void (* sink)( const char * ),
*                                 void (* sink_wrap)( void (*)( const char * ),
*                                                     const char * ) )

*  Class Membership:
*     XmlChan initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new XmlChan object. It allocates memory (if
*     necessary) to accommodate the XmlChan plus any additional data
*     associated with the derived class.  It then initialises a
*     XmlChan structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for an XmlChan at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the XmlChan is to be
*        initialised.  This must be of sufficient size to accommodate
*        the XmlChan data (sizeof(XmlChan)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the XmlChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the XmlChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A boolean flag indicating if the XmlChan's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new XmlChan.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the contents of the XmlChan will not be
*        written out before being deleted.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.

*  Returned Value:
*     A pointer to the new XmlChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *new;              /* Pointer to new XmlChan */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitXmlChanVtab( vtab, name );

/* Initialise a Channel structure (the parent class) as the first
   component within the XmlChan structure, allocating memory if
   necessary. */
   new = (AstXmlChan *) astInitChannel( mem, size, 0,
                                       (AstChannelVtab *) vtab, name,
                                        source, source_wrap, sink,
                                        sink_wrap );

   if ( astOK ) {

/* Initialise the XmlChan data. */
/* ---------------------------- */
      new->objectname = NULL;   /* Name of object being written */
      new->objectset = 1;       /* Is object being written set?*/
      new->objectcomment = NULL;/* Comment for object class being written */
      new->container = NULL;    /* XmlElement to which content will be added */ 
      new->readcontext = NULL;  /* XmlElement giving context for current read */ 
      new->write_isa = 0;       /* Write out the next "IsA" item? */
      new->xmlindent = -1;      /* Indentat output? */
      new->xmllength = -INT_MAX;/* Buffer length */
      new->xmlprefix = NULL;    /* Xml prefix */
      new->xmlformat = UNKNOWN_FORMAT; /* Xml format */
      new->formatdef = NATIVE_FORMAT;  /* Default Xml format */
      new->reset_source = 1;    /* A new line should be read from the source */
      new->isa_class = NULL;    /* Class being loaded */

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstXmlChan *astLoadXmlChan_( void *mem, size_t size,
                             AstXmlChanVtab *vtab, const char *name,
                             AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadXmlChan

*  Purpose:
*     Load an XmlChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "xmlchan.h"
*     AstXmlChan *astLoadXmlChan( void *mem, size_t size,
*                                 AstXmlChanVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     XmlChan loader.

*  Description:
*     This function is provided to load a new XmlChan using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     XmlChan structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for an XmlChan at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the XmlChan is to be
*        loaded.  This must be of sufficient size to accommodate the
*        XmlChan data (sizeof(XmlChan)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the XmlChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the XmlChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstXmlChan) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new XmlChan. If this is NULL, a pointer
*        to the (static) virtual function table for the XmlChan class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "XmlChan" is used instead.

*  Returned Value:
*     A pointer to the new XmlChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstXmlChan *new;            /* Pointer to the new XmlChan */
   char *text;                 /* Textual version of integer value */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this XmlChan. In this case the
   XmlChan belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstXmlChan );
      vtab = &class_vtab;
      name = "XmlChan";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitXmlChanVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built XmlChan. */
   new = astLoadChannel( mem, size, (AstChannelVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "XmlChan" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* Ensure other items in the XmlChan structure are initialised properly. */
      new->objectname = NULL;   /* Name of object being written */
      new->objectset = 1;       /* Is object being written set?*/
      new->objectcomment = NULL;/* Comment for object class being written */
      new->container = NULL;    /* XmlElement to which content will be added */ 
      new->readcontext = NULL;  /* XmlElement giving context for current read */ 
      new->write_isa = 0;       /* Write out the next "IsA" item? */
      new->xmlindent = -1;      /* Indent output? */
      new->xmllength = -INT_MAX;/* Buffer length */
      new->xmlprefix = NULL;    /* Xml prefix */
      new->reset_source = 1;    /* A new line should be read from the source */
      new->isa_class = NULL;    /* Class being loaded */
      new->formatdef = NATIVE_FORMAT;  /* Default Xml format */

/* Now restore presistent values. */

/* XmlIndent */
/* --------- */
      new->xmlindent = astReadInt( channel, "xmlind", -1 );

/* XmlLength */
/* --------- */
      new->xmllength = astReadInt( channel, "xmllen", -INT_MAX );

/* XmlPrefix */
/* --------- */
      new->xmlprefix = astReadString( channel, "xmlprf", NULL );

/* XmlFormat. */
/* --------- */
      text = astReadString( channel, "xmlfmt", UNKNOWN_STRING );
      if( strcmp( text, UNKNOWN_STRING ) ) {
         new->xmlformat = FindString( MAX_FORMAT + 1, xformat, text, 
                                     "the XmlChan component 'XmlFmt'", 
                                     "astRead", astGetClass( channel ) );
      } else {
         new->xmlformat = UNKNOWN_FORMAT;
      }
      if ( TestXmlFormat( new ) ) SetXmlFormat( new, new->xmlformat );

   }

/* If an error occurred, clean up by deleting the new XmlChan. */
   if ( !astOK ) new = astDelete( new );

/* Return the new XmlChan pointer. */
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




