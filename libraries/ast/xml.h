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
*     12-JAN-2004 (DSB):
*        Major revisions.
*/



/* Constant Values. */
/* ================ */

/* These constants are used as identifiers for the different classes of
   XML object defined in this file. They are purposefully obscure to reduce 
   the possibility of random integer values being incorrectly interpreted
   as valid XML types */
#define AST__XMLBAD    0            /* Id for an uninitialised XmlObject */
#define AST__XMLOBJECT 198263577    /* Id for XmlObject structure */
#define AST__XMLELEM   182874779    /* Id for XmlElement structure */
#define AST__XMLATTR   837746634    /* Id for XmlAttribute structure */
#define AST__XMLCDATA  293854662    /* Id for XmlCDdataSection structure */
#define AST__XMLCOM    748737648    /* Id for XmlComment structure */
#define AST__XMLPI     983763553    /* Id for XmlPI structure */
#define AST__XMLNAME   236756469    /* Id for XmlNamespace structure */
#define AST__XMLDOC    356274395    /* Id for XmlDocument structure */
#define AST__XMLPRO    743682474    /* Id for XmlPrologue structure */
#define AST__XMLDEC    987546328    /* Id for XmlDeclPI structure */
#define AST__XMLDTD    874673747    /* Id for XmlDTDec structure */
#define AST__XMLWHITE  675849952    /* Id for XmlWhite structure */
#define AST__XMLBLACK  347657863    /* Id for XmlBlack structure */

/* The following constants refer to "interfaces", not "classes". */

#define AST__XMLCHAR   456739289    /* Id for XmlCharData structure */
#define AST__XMLCONT   673882993    /* Id for XmlContentItem structure */
#define AST__XMLMISC   358768954    /* Id for XmlMiscItem structure */
#define AST__XMLPAR    874366235    /* Id for XmlParent structure */


/* Type Definitions. */
/* ================= */

/* Pre-define types so they can be used within structure definitions. */
typedef struct AstXmlObject AstXmlObject;
typedef struct AstXmlAttribute AstXmlAttribute;
typedef struct AstXmlNamespace AstXmlNamespace;
typedef struct AstXmlElement AstXmlElement;
typedef struct AstXmlBlack AstXmlBlack;
typedef struct AstXmlWhite AstXmlWhite;
typedef struct AstXmlCDataSection AstXmlCDataSection;
typedef struct AstXmlComment AstXmlComment;
typedef struct AstXmlPI AstXmlPI;
typedef struct AstXmlDocument AstXmlDocument;
typedef struct AstXmlPrologue AstXmlPrologue;
typedef struct AstXmlDeclPI AstXmlDeclPI;
typedef struct AstXmlDTDec AstXmlDTDec;

/* The following data types define "interfaces". That is each data type
   corresponds to a subset of the above classes. */

/* Marks a class as "character data" */
typedef AstXmlObject AstXmlCharData;

/* Marks a class as a "content item" */
typedef AstXmlObject AstXmlContentItem;

/* Marks a class as a "miscalleneous item" */
typedef AstXmlObject AstXmlMiscItem;

/* Marks a class as being able to own a child */
typedef AstXmlObject AstXmlParent;

/* XmlObject structure. */
/* -------------------- */
/* Contains data common to all other structures */
struct AstXmlObject {
   AstXmlParent *parent;        /* The parent which contains this XmlObject */
   long int type;               /* An ID giving the type of structure */
   int id;                      /* A unique id for this object. */
};

/* XmlAttribute structure. */
/* ----------------------- */
/* Describes an XML attribute */
struct AstXmlAttribute {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *name;                /* The name of the attribute */
   char *value;               /* Attribute value */
   char *prefix;              /* Namespace prefix for this attribute */
};

/* XmlNamespace structure. */
/* ----------------------- */
/* Describes an XML namespace definition */
struct AstXmlNamespace {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *prefix;              /* Namespace prefix */
   char *uri;                 /* Namespace URI */
};

/* XmlElement structure. */
/* --------------------- */
/* Describes an XML element */
struct AstXmlElement {
   AstXmlObject obj;          /* General information for this XmlObject */
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
};

/* XmlBlack structure. */
/* ---------------------- */
/* Describes character data containing at least one non-blank character. */
struct AstXmlBlack {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *text;                /* The character data */
};

/* XmlWhite structure. */
/* ------------------- */
/* Describes character data containing no one non-blank characters. */
struct AstXmlWhite {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *text;                /* The white character data */
};

/* XmlCDataSection structure. */
/* ----------------------- */
/* Describes an XML CDATA section */
struct AstXmlCDataSection {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *text;                /* The text of the cdata section */
};

/* XmlComment structure. */
/* --------------------- */
/* Describes an XML CDATA section */
struct AstXmlComment {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *text;                /* The text of the comment */
};

/* XmlPI structure. */
/* ---------------- */
/* Describes an XML processing instruction */
struct AstXmlPI {
   AstXmlObject obj;          /* General information for this XmlObject */
   char *target;              /* The target of the processing instruction */
   char *text;                /* The text of the processing instruction */
};

/* XmlDocument structure. */
/* ---------------------- */
/* Describes an entire XML document */
struct AstXmlDocument {
   AstXmlObject obj;        /* General information for this XmlObject */
   AstXmlPrologue *prolog;  /* Pointer to document prologue */
   AstXmlElement *root;     /* Pointer to root element */
   AstXmlMiscItem **epilog; /* List of XmlObjects forming the document epilogue */
   int nepi;                /* No of XmlObjects pointers in "epilogue" */
   AstXmlElement *current;  /* Pointer to element being read */
};

/* XmlPrologue structure. */
/* ---------------------- */
/* Describes an XML document prologue */
struct AstXmlPrologue {
   AstXmlObject obj;        /* General information for this XmlObject */
   AstXmlDeclPI *xmldecl;   /* Pointer to XML declaration PI */
   AstXmlMiscItem **misc1;  /* Group of of miscalleneous XmlObjects pointers */
   int nmisc1;              /* No of XmlObjects pointers in "misc1" */
   AstXmlDTDec *dtdec;      /* Pointer to Document Type Declaration */
   AstXmlMiscItem **misc2;  /* Group of of miscalleneous XmlObjects pointers */
   int nmisc2;              /* No of XmlObjects pointers in "misc2" */
};

/* XmlDecPI structure. */
/* ------------------- */
/* Describes an XML declaration PI */
struct AstXmlDeclPI {
   AstXmlObject obj;        /* General information for this XmlObject */
   char *text;              /* The text of the XML declaration */
};

/* XmlDTDec structure. */
/* ------------------- */
/* Describes a data type declaration */
struct AstXmlDTDec {
   AstXmlObject obj;        /* General information for this XmlObject */
   char *name;              /* Document type name */
   char *external;          /* External ID */
   char *internal;          /* Internal declarations */
};






/* Function prototypes. */
/* ==================== */
AstXmlAttribute *astXmlCheckAttribute_( void *, int );
AstXmlBlack *astXmlCheckBlack_( void *, int );
AstXmlCDataSection *astXmlCheckCDataSection_( void *, int );
AstXmlComment *astXmlCheckComment_( void *, int );
AstXmlContentItem *astXmlGetItem_( AstXmlElement *, int );
AstXmlDTDec *astXmlCheckDTDec_( void *, int );
AstXmlDeclPI *astXmlCheckDeclPI_( void *, int );
AstXmlDocument *astXmlCheckDocument_( void *, int );
AstXmlElement *astXmlAddElement_( AstXmlElement *, const char *, const char * );
AstXmlElement *astXmlCheckElement_( void *, int );
AstXmlParent *astXmlGetParent_( AstXmlObject * );
AstXmlObject *astXmlGetRoot_( AstXmlObject * );
AstXmlElement *astXmlReadDocument_( AstXmlDocument **, int (*)( AstXmlElement * ), int, char (*)( void * ), void * );
AstXmlNamespace *astXmlCheckNamespace_( void *, int );
AstXmlObject *astXmlCopy_( AstXmlObject * );
AstXmlObject *astXmlCheckObject_( void *, int );
AstXmlPI *astXmlCheckPI_( void *, int );
AstXmlPrologue *astXmlCheckPrologue_( void *, int );
AstXmlWhite *astXmlCheckWhite_( void *, int );
AstXmlCharData *astXmlCheckCharData_( void *, int );
AstXmlContentItem *astXmlCheckContentItem_( void *, int );
AstXmlMiscItem *astXmlCheckMiscItem_( void *, int );
AstXmlParent *astXmlCheckParent_( void *, int );
const char *astXmlFormat_( AstXmlObject * );
const char *astXmlGetAttributeValue_( AstXmlElement *, const char *);
const char *astXmlGetName_( AstXmlObject * );
const char *astXmlGetTag_( AstXmlObject *, int );
const char *astXmlGetURI_( AstXmlObject * );
const char *astXmlGetValue_( AstXmlObject *, int );
const char *astXmlShow_( AstXmlObject * );
int astXmlCheckType_( void *, long int );
int astXmlGetNattr_( AstXmlElement *);
int astXmlGetNitem_( AstXmlElement *);
void *astXmlAnnulTree_( AstXmlObject * );
void *astXmlAnnul_( AstXmlObject * );
void *astXmlDelete_( void * );
void astXmlAddAttr_( AstXmlElement *, const char *, const char *, const char * );
void astXmlAddCDataSection_( AstXmlElement *, const char * );
void astXmlAddCharData_( AstXmlParent *, int, const char * );
void astXmlAddComment_( AstXmlParent *, int, const char * );
void astXmlAddPI_( AstXmlParent *, int, const char *, const char * );
void astXmlAddURI_( AstXmlElement *, const char *, const char * );
void astXmlInsertElement_( AstXmlElement *, AstXmlElement * );
void astXmlPurge_( AstXmlParent * );
void astXmlRemoveAttr_( AstXmlElement *, const char *, const char * );
void astXmlRemoveItem_( AstXmlContentItem * );
void astXmlRemoveURI_( AstXmlElement *, const char * );
void astXmlSetXmlDec_( AstXmlDocument *, const char * );
void astXmlSetDTDEC_( AstXmlDocument *, const char *, const char *, const char *);

#ifdef DEBUG
int astXmlTrace_( int );           
#endif

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */

#define astXmlCheckAttribute(this,nullok) astXmlCheckAttribute_(this,nullok)
#define astXmlCheckBlack(this,nullok) astXmlCheckBlack_(this,nullok)
#define astXmlCheckCDataSection(this,nullok) astXmlCheckCDataSection_(this,nullok)
#define astXmlCheckCharData(this,nullok) astXmlCheckCharData_(this,nullok)
#define astXmlCheckComment(this,nullok) astXmlCheckComment_(this,nullok)
#define astXmlCheckContentItem(this,nullok) astXmlCheckContentItem_(this,nullok)
#define astXmlCheckDTDec(this,nullok) astXmlCheckDTDec_(this,nullok)
#define astXmlCheckDeclPI(this,nullok) astXmlCheckDeclPI_(this,nullok)
#define astXmlCheckDocument(this,nullok) astXmlCheckDocument_(this,nullok)
#define astXmlCheckElement(this,nullok) astXmlCheckElement_(this,nullok)
#define astXmlCheckMiscItem(this,nullok) astXmlCheckMiscItem_(this,nullok)
#define astXmlCheckNamespace(this,nullok) astXmlCheckNamespace_(this,nullok)
#define astXmlCheckObject(this,nullok) astXmlCheckObject_(this,nullok)
#define astXmlCheckPI(this,nullok) astXmlCheckPI_(this,nullok)
#define astXmlCheckParent(this,nullok) astXmlCheckParent_(this,nullok)
#define astXmlCheckPrologue(this,nullok) astXmlCheckPrologue_(this,nullok)
#define astXmlCheckWhite(this,nullok) astXmlCheckWhite_(this,nullok)

#define astXmlAddAttr(elem,name,value,prefix) astXmlAddAttr_(astXmlCheckElement(elem,0),name,value,prefix)
#define astXmlAddURI(elem,prefix,uri) astXmlAddURI_(astXmlCheckElement(elem,0),prefix,uri)
#define astXmlAnnul(this) astXmlAnnul_(astXmlCheckObject(this,1))
#define astXmlDelete(this) astXmlDelete_(this)
#define astXmlAnnulTree(this) astXmlAnnulTree_(astXmlCheckObject(this,1))
#define astXmlAddCDataSection(this,text) astXmlAddCDataSection_(astXmlCheckElement(this,0),text)
#define astXmlAddCharData(this,where,text) astXmlAddCharData_(astXmlCheckParent(this,0),where,text)
#define astXmlAddComment(this,where,text) astXmlAddComment_(astXmlCheckParent(this,0),where,text)
#define astXmlAddElement(this,name,prefix) astXmlAddElement_(astXmlCheckElement(this,1),name,prefix)
#define astXmlAddPI(this,where,target,text) astXmlAddPI_(astXmlCheckParent(this,0),where,target,text)
#define astXmlGetParent(this) astXmlGetParent_(astXmlCheckObject(this,0))
#define astXmlGetRoot(this) astXmlGetRoot_(astXmlCheckObject(this,0))
#define astXmlGetName(this) astXmlGetName_(astXmlCheckObject(this,0))
#define astXmlGetValue(this,report) astXmlGetValue_(astXmlCheckObject(this,0),report)
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astXmlCheckElement(this,0),name)
#define astXmlGetNattr(this) astXmlGetNattr_(astXmlCheckElement(this,0))
#define astXmlGetNitem(this) astXmlGetNitem_(astXmlCheckElement(this,0))
#define astXmlGetItem(this,item) astXmlGetItem_(astXmlCheckElement(this,0),item)
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astXmlCheckElement(this,0),name)
#define astXmlGetTag(this,opening) astXmlGetTag_(astXmlCheckObject(this,0),opening)
#define astXmlGetURI(this) astXmlGetURI_(astXmlCheckObject(this,0))
#define astXmlFormat(this) astXmlFormat_(astXmlCheckObject(this,0))
#define astXmlShow(this) astXmlShow_(astXmlCheckObject(this,0))
#define astXmlRemoveItem(this) astXmlRemoveItem_(astXmlCheckContentItem(this,0))
#define astXmlRemoveAttr(this,name,prefix) astXmlRemoveAttr_(astXmlCheckElement(this,0),name,prefix)
#define astXmlRemoveURI(this,prefix) astXmlRemoveURI_(astXmlCheckElement(this,0),prefix)
#define astXmlReadDocument(doc,is_wanted,skip,source,data) astXmlReadDocument_(doc,is_wanted,skip,source,data) 
#define astXmlInsertElement(this,elem) astXmlInsertElement_(astXmlCheckElement(this,0),astXmlCheckElement(elem,0)) 
#define astXmlPurge(this) astXmlPurge_(astXmlCheckParent(this,1)) 
#define astXmlSetXmlDec(this,text) astXmlSetXmlDec_(astXmlCheckDocument(this,0),text) 
#define astXmlSetDTDec(this,text1,text2,text3) astXmlSetDTDec_(astXmlCheckDocument(this,0),text1,text2,text3) 
#define astXmlCheckType(this,type) astXmlCheckType_(this,type)
#define astXmlCopy(this) astXmlCopy_(astXmlCheckObject(this,1))

#ifdef DEBUG
#define astXmlTrace(show) astXmlTrace_(show)
#endif

#endif
