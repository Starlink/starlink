#if !defined( XML_INCLUDED ) /* Include this file only once */
#define XML_INCLUDED
/*
*+
*  Name:
*     xml.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the AST xml module

*  Invocation:
*     #include "xml.h"

*  Description:
*     This include file defines the interface to the internal xml module 
*     used by the AST library and provides the type definitions, function 
*     prototypes and macros, etc. needed to use this module.

*  Inheritance:
*     The xml module is not a class and does not inherit.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     23-OCT-2003 (DSB):
*        Original version.
*/

/* Constant Values. */
/* ================ */

/* These constants are used as identifiers for the different classes of
   structure defined in this file. They are purposefully obscure to reduce 
   the possibility of random integer values being incorrectly interpreted as 
    valid XML types */
#define AST__XMLBAD    0            /* Id for an uninitialised XmlObject */
#define AST__XMLOBJECT 198263577    /* Id for XmlObject structure */
#define AST__XMLELEM   182874779    /* Id for XmlElement structure */
#define AST__XMLATTR   837746634    /* Id for XmlAttribute structure */
#define AST__XMLCHAR   456739289    /* Id for XmlCharData structure */
#define AST__XMLCDATA  293854662    /* Id for XmlCDdataSection structure */
#define AST__XMLCOM    748737648    /* Id for XmlComment structure */
#define AST__XMLPI     983763553    /* Id for XmlPI structure */
#define AST__XMLNAME   236756469    /* Id for XmlNamespace structure */
#define AST__XMLCONT   673882993    /* Id for XmlContentItem structure */

/* Type Definitions. */
/* ================= */

/* XmlObject structure. */
/* -------------------- */
typedef struct AstXmlObject {
   struct AstXmlElement *parent; /* The element which contains this XmlObject */
   long int type;             /* An ID giving the type of structure */
   int id;                    /* A unique id for this object. */
} AstXmlObject;

/* XmlAttribute structure. */
/* ----------------------- */
typedef struct AstXmlAttribute {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *name;                /* The name of the attribute */
   char *value;               /* Attribute value */
   char *prefix;              /* Namespace prefix for this attribute */
} AstXmlAttribute;

/* XmlNamespace structure. */
/* ----------------------- */
typedef struct AstXmlNamespace {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *prefix;              /* Namespace prefix */
   char *uri;                 /* Namespace URI */
} AstXmlNamespace;

/* XmlContentItem structure. */
/* ------------------------- */
typedef struct AstXmlContentItem {
   AstXmlObject obj;               /* General information for this XmlObject */
} AstXmlContentItem;

/* XmlElement structure. */
/* --------------------- */
typedef struct AstXmlElement {
   AstXmlContentItem item;    /* General information for this ContentItem */
   char *name;                /* The type (name) of the element */
   AstXmlAttribute **attrs;   /* Ptr. to list of attributes of the element */
   int nattr;                 /* Number of attributes in the above list */
   AstXmlContentItem **items; /* Ptr. to list of items in the element's content */
   int nitem;                 /* Number of items in above list */
   char *defns;               /* Default Namespace URI for element content */
   char *prefix;              /* Namespace prefix for this element */
   AstXmlNamespace **nsprefs; /* Ptr. to list of new Namespaces defined by this element */
   int nnspref;               /* Number of Namespaces in above list */
   int complete;              /* Have the contents of the element been read? */
} AstXmlElement;

/* XmlCharData structure. */
/* ---------------------- */
typedef struct AstXmlCharData {
   AstXmlContentItem item;    /* General information for this ContentItem */
   char *text;                /* The character data */
} AstXmlCharData;

/* XmlCDataSection structure. */
/* ----------------------- */
typedef struct AstXmlCDataSection {
   AstXmlContentItem item;    /* General information for this ContentItem */
   char *text;                /* The text of the cdata section */
} AstXmlCDataSection;

/* XmlComment structure. */
/* --------------------- */
typedef struct AstXmlComment {
   AstXmlContentItem item;    /* General information for this ContentItem */
   char *text;                /* The text of the comment */
} AstXmlComment;

/* XmlPI structure. */
/* ---------------- */
typedef struct AstXmlPI {
   AstXmlContentItem item;    /* General information for this ContentItem */
   char *target;              /* The target of the processing instruction */
   char *text;                /* The text of the processing instruction */
} AstXmlPI;

/* Function prototypes. */
/* ==================== */
AstXmlContentItem *astCheckXmlContentItem_( void * );
AstXmlElement *astCheckXmlElement_( void * );
AstXmlElement *astXmlGetParent_( AstXmlObject * );
AstXmlElement *astXmlAddElement_( AstXmlElement *, const char *, const char * );
AstXmlObject *astCheckXmlObject_( void * );
const char *astXmlFormat_( AstXmlObject * );
const char *astXmlShow_( AstXmlObject * );
const char *astXmlGetName_( AstXmlObject * );
const char *astXmlGetValue_( AstXmlObject * );
void *astXmlAnnul_( AstXmlObject * );
void astXmlAddAttr_( AstXmlElement *, const char *, const char *, const char * );
void astXmlAddCDataSection_( AstXmlElement *, const char * );
void astXmlAddCharData_( AstXmlElement *, const char * );
void astXmlAddComment_( AstXmlElement *, const char * );
void astXmlAddPI_( AstXmlElement *, const char *, const char * );
void astXmlAddURI_( AstXmlElement *, const char *, const char * );
void astXmlTrace_();           
const char *astXmlGetAttributeValue_( AstXmlElement *, const char *);
int astXmlGetType_( AstXmlObject *);
int astXmlGetNitem_( AstXmlElement *);
AstXmlContentItem *astXmlGetItem_( AstXmlElement *, int );
AstXmlContentItem *astXmlGetItem_( AstXmlElement *, int );
const char *astXmlGetTag_( AstXmlObject *, int );
void astXmlRemoveItem_( AstXmlContentItem * );
void astXmlRemoveAttr_( AstXmlElement *, const char *, const char * );
void astXmlRemoveURI_( AstXmlElement *, const char * );
const char *astXmlGetURI_( AstXmlObject * );
AstXmlElement *astXmlReadNextElement_( char (*)( void * ), void *, char *, int );
void astXmlReadContent_( AstXmlElement *, char (*)( void * ), void * );

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */

#define astCheckXmlObject(this) astCheckXmlObject_(this)
#define astCheckXmlElement(this) astCheckXmlElement_(this)
#define astCheckXmlContentItem(this) astCheckXmlContentItem_(this)

#define astXmlAddAttr(elem,name,value,prefix) astXmlAddAttr_(astCheckXmlElement(elem),name,value,prefix)
#define astXmlAddURI(elem,prefix,uri) astXmlAddURI_(astCheckXmlElement(elem),prefix,uri)
#define astXmlAnnul(this) astXmlAnnul_(astCheckXmlObject(this))
#define astXmlAddCDataSection(this,text) astXmlAddCDataSection_(this,text)
#define astXmlAddCharData(this,text) astXmlAddCharData_(this,text)
#define astXmlAddComment(this,text) astXmlAddComment_(this,text)
#define astXmlAddElement(this,name,prefix) astXmlAddElement_(this,name,prefix)
#define astXmlAddPI(this,target,text) astXmlAddPI_(this,target,text)
#define astXmlTrace astXmlTrace_
#define astXmlGetParent(this) astXmlGetParent_(astCheckXmlObject(this))
#define astXmlGetName(this) astXmlGetName_(astCheckXmlObject(this))
#define astXmlGetValue(this) astXmlGetValue_(astCheckXmlObject(this))
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astCheckXmlElement(this),name)
#define astXmlGetType(this) astXmlGetType_(astCheckXmlObject(this))
#define astXmlGetNitem(this) astXmlGetNitem_(astCheckXmlElement(this))
#define astXmlGetItem(this,item) astXmlGetItem_(astCheckXmlElement(this),item)
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astCheckXmlElement(this),name)
#define astXmlGetTag(this,opening) astXmlGetTag_(astCheckXmlObject(this),opening)
#define astXmlGetURI(this) astXmlGetURI_(astCheckXmlObject(this))
#define astXmlReadNextElement(source,data,buff,bufflen) astXmlReadNextElement_(source,data,buff,bufflen) 
#define astXmlReadContent(this,source,data) astXmlReadContent_(astCheckXmlElement(this),source,data) 
#define astXmlFormat(this) astXmlFormat_(astCheckXmlObject(this))
#define astXmlShow(this) astXmlShow_(astCheckXmlObject(this))
#define astXmlRemoveItem(this) astXmlRemoveItem_(astCheckXmlContentItem(this))
#define astXmlRemoveAttr(this,name,prefix) astXmlRemoveAttr_(astCheckXmlElement(this),name,prefix)
#define astXmlRemoveURI(this,prefix) astXmlRemoveURI_(astCheckXmlElement(this),prefix)

#endif
