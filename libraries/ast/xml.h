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

/* Define constants used to size global arrays in this module. */
#define AST__XML_GETTAG_BUFF_LEN 200

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


#if defined(THREAD_SAFE) && defined(astCLASS)

/* Define a structure holding all data items that are global within the
   xml.c file. */
typedef struct AstXmlGlobals {
   int Next_ID;
   char GetTag_Buff[ AST__XML_GETTAG_BUFF_LEN + 1 ];
} AstXmlGlobals;

#endif




/* Function prototypes. */
/* ==================== */
AstXmlAttribute *astXmlCheckAttribute_( void *, int, int * );
AstXmlBlack *astXmlCheckBlack_( void *, int, int * );
AstXmlCDataSection *astXmlCheckCDataSection_( void *, int, int * );
AstXmlComment *astXmlCheckComment_( void *, int, int * );
AstXmlContentItem *astXmlGetItem_( AstXmlElement *, int, int * );
AstXmlDTDec *astXmlCheckDTDec_( void *, int, int * );
AstXmlDeclPI *astXmlCheckDeclPI_( void *, int, int * );
AstXmlDocument *astXmlCheckDocument_( void *, int, int * );
AstXmlElement *astXmlAddElement_( AstXmlElement *, const char *, const char *, int * );
AstXmlElement *astXmlCheckElement_( void *, int, int * );
AstXmlParent *astXmlGetParent_( AstXmlObject *, int * );
AstXmlObject *astXmlGetRoot_( AstXmlObject *, int * );
AstXmlElement *astXmlReadDocument_( AstXmlDocument **, int (*)( AstXmlElement *, int * ), int, char (*)( void *, int * ), void *, int * );
AstXmlNamespace *astXmlCheckNamespace_( void *, int, int * );
AstXmlObject *astXmlCopy_( AstXmlObject *, int * );
AstXmlObject *astXmlCheckObject_( void *, int, int * );
AstXmlPI *astXmlCheckPI_( void *, int, int * );
AstXmlPrologue *astXmlCheckPrologue_( void *, int, int * );
AstXmlWhite *astXmlCheckWhite_( void *, int, int * );
AstXmlCharData *astXmlCheckCharData_( void *, int, int * );
AstXmlContentItem *astXmlCheckContentItem_( void *, int, int * );
AstXmlMiscItem *astXmlCheckMiscItem_( void *, int, int * );
AstXmlParent *astXmlCheckParent_( void *, int, int * );
const char *astXmlFormat_( AstXmlObject *, int * );
const char *astXmlGetAttributeValue_( AstXmlElement *, const char *, int * );
const char *astXmlGetName_( AstXmlObject *, int * );
const char *astXmlGetTag_( AstXmlObject *, int, int * );
const char *astXmlGetType_( AstXmlObject *, int * );
const char *astXmlGetURI_( AstXmlObject *, int * );
const char *astXmlGetValue_( AstXmlObject *, int, int * );
const char *astXmlShow_( AstXmlObject *, int * );
int astXmlCheckType_( void *, long int, int * );
int astXmlGetNattr_( AstXmlElement *, int * );
int astXmlGetNitem_( AstXmlElement *, int * );
void *astXmlAnnulTree_( AstXmlObject *, int * );
void *astXmlAnnul_( AstXmlObject *, int * );
void *astXmlDelete_( void *, int * );
void astXmlAddAttr_( AstXmlElement *, const char *, const char *, const char *, int * );
void astXmlAddCDataSection_( AstXmlElement *, const char *, int * );
void astXmlAddCharData_( AstXmlParent *, int, const char *, int * );
void astXmlAddComment_( AstXmlParent *, int, const char *, int * );
void astXmlAddPI_( AstXmlParent *, int, const char *, const char *, int * );
void astXmlAddURI_( AstXmlElement *, const char *, const char *, int * );
void astXmlInsertElement_( AstXmlElement *, AstXmlElement *, int * );
void astXmlPurge_( AstXmlParent *, int * );
void astXmlRemoveAttr_( AstXmlElement *, const char *, const char *, int * );
void astXmlRemoveItem_( AstXmlContentItem *, int * );
void astXmlRemoveURI_( AstXmlElement *, const char *, int * );
void astXmlSetXmlDec_( AstXmlDocument *, const char *, int * );
void astXmlSetDTDec_( AstXmlDocument *, const char *, const char *, const char *, int * );

#if defined(THREAD_SAFE) && defined(astCLASS)
void astInitXmlGlobals_( AstXmlGlobals * );
#else

#ifdef DEBUG
int astXmlTrace_( int );
#endif

#endif

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */
#define astXmlGetType(this) astXmlGetType_(this,STATUS_PTR)
#define astXmlCheckAttribute(this,nullok) astXmlCheckAttribute_(this,nullok,STATUS_PTR)
#define astXmlCheckBlack(this,nullok) astXmlCheckBlack_(this,nullok,STATUS_PTR)
#define astXmlCheckCDataSection(this,nullok) astXmlCheckCDataSection_(this,nullok,STATUS_PTR)
#define astXmlCheckCharData(this,nullok) astXmlCheckCharData_(this,nullok,STATUS_PTR)
#define astXmlCheckComment(this,nullok) astXmlCheckComment_(this,nullok,STATUS_PTR)
#define astXmlCheckContentItem(this,nullok) astXmlCheckContentItem_(this,nullok,STATUS_PTR)
#define astXmlCheckDTDec(this,nullok) astXmlCheckDTDec_(this,nullok,STATUS_PTR)
#define astXmlCheckDeclPI(this,nullok) astXmlCheckDeclPI_(this,nullok,STATUS_PTR)
#define astXmlCheckDocument(this,nullok) astXmlCheckDocument_(this,nullok,STATUS_PTR)
#define astXmlCheckElement(this,nullok) astXmlCheckElement_(this,nullok,STATUS_PTR)
#define astXmlCheckMiscItem(this,nullok) astXmlCheckMiscItem_(this,nullok,STATUS_PTR)
#define astXmlCheckNamespace(this,nullok) astXmlCheckNamespace_(this,nullok,STATUS_PTR)
#define astXmlCheckObject(this,nullok) astXmlCheckObject_(this,nullok,STATUS_PTR)
#define astXmlCheckPI(this,nullok) astXmlCheckPI_(this,nullok,STATUS_PTR)
#define astXmlCheckParent(this,nullok) astXmlCheckParent_(this,nullok,STATUS_PTR)
#define astXmlCheckPrologue(this,nullok) astXmlCheckPrologue_(this,nullok,STATUS_PTR)
#define astXmlCheckWhite(this,nullok) astXmlCheckWhite_(this,nullok,STATUS_PTR)

#define astXmlAddAttr(elem,name,value,prefix) astXmlAddAttr_(astXmlCheckElement(elem,0),name,value,prefix,STATUS_PTR)
#define astXmlAddURI(elem,prefix,uri) astXmlAddURI_(astXmlCheckElement(elem,0),prefix,uri,STATUS_PTR)
#define astXmlAnnul(this) astXmlAnnul_(astXmlCheckObject(this,1),STATUS_PTR)
#define astXmlDelete(this) astXmlDelete_(this,STATUS_PTR)
#define astXmlAnnulTree(this) astXmlAnnulTree_(astXmlCheckObject(this,1),STATUS_PTR)
#define astXmlAddCDataSection(this,text) astXmlAddCDataSection_(astXmlCheckElement(this,0),text,STATUS_PTR)
#define astXmlAddCharData(this,where,text) astXmlAddCharData_(astXmlCheckParent(this,0),where,text,STATUS_PTR)
#define astXmlAddComment(this,where,text) astXmlAddComment_(astXmlCheckParent(this,0),where,text,STATUS_PTR)
#define astXmlAddElement(this,name,prefix) astXmlAddElement_(astXmlCheckElement(this,1),name,prefix,STATUS_PTR)
#define astXmlAddPI(this,where,target,text) astXmlAddPI_(astXmlCheckParent(this,0),where,target,text,STATUS_PTR)
#define astXmlGetParent(this) astXmlGetParent_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlGetRoot(this) astXmlGetRoot_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlGetName(this) astXmlGetName_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlGetValue(this,report) astXmlGetValue_(astXmlCheckObject(this,0),report,STATUS_PTR)
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astXmlCheckElement(this,0),name,STATUS_PTR)
#define astXmlGetNattr(this) astXmlGetNattr_(astXmlCheckElement(this,0),STATUS_PTR)
#define astXmlGetNitem(this) astXmlGetNitem_(astXmlCheckElement(this,0),STATUS_PTR)
#define astXmlGetItem(this,item) astXmlGetItem_(astXmlCheckElement(this,0),item,STATUS_PTR)
#define astXmlGetAttributeValue(this,name) astXmlGetAttributeValue_(astXmlCheckElement(this,0),name,STATUS_PTR)
#define astXmlGetTag(this,opening) astXmlGetTag_(astXmlCheckObject(this,0),opening,STATUS_PTR)
#define astXmlGetURI(this) astXmlGetURI_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlFormat(this) astXmlFormat_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlShow(this) astXmlShow_(astXmlCheckObject(this,0),STATUS_PTR)
#define astXmlRemoveItem(this) astXmlRemoveItem_(astXmlCheckContentItem(this,0),STATUS_PTR)
#define astXmlRemoveAttr(this,name,prefix) astXmlRemoveAttr_(astXmlCheckElement(this,0),name,prefix,STATUS_PTR)
#define astXmlRemoveURI(this,prefix) astXmlRemoveURI_(astXmlCheckElement(this,0),prefix,STATUS_PTR)
#define astXmlReadDocument(doc,is_wanted,skip,source,data) astXmlReadDocument_(doc,is_wanted,skip,source,data,STATUS_PTR)
#define astXmlInsertElement(this,elem) astXmlInsertElement_(astXmlCheckElement(this,0),astXmlCheckElement(elem,0),STATUS_PTR)
#define astXmlPurge(this) astXmlPurge_(astXmlCheckParent(this,1),STATUS_PTR)
#define astXmlSetXmlDec(this,text) astXmlSetXmlDec_(astXmlCheckDocument(this,0),text,STATUS_PTR)
#define astXmlSetDTDec(this,text1,text2,text3) astXmlSetDTDec_(astXmlCheckDocument(this,0),text1,text2,text3,STATUS_PTR)
#define astXmlCheckType(this,type) astXmlCheckType_(this,type,STATUS_PTR)
#define astXmlCopy(this) astXmlCopy_(astXmlCheckObject(this,1),STATUS_PTR)

#ifdef DEBUG
#define astXmlTrace(show) astXmlTrace_(show)
#endif

#endif



