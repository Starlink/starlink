

/*
*+
*  Name:
*     smf_kmmerge

*  Purpose:
*     Merge the contents of two time slices in an AST KeyMap holding 
*     time-indexed extension items in an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_kmmerge( const char *xname, AstKeyMap *keymap, int from,
*                       int into, int ndet, int *mask, int nts, 
*                       int rts_num, int *status )

*  Arguments:
*     xname = const char * (Given)
*        The name of the NDF extension to use.
*     keymap = AstKeyMap * (Given)
*        An AST keyMap holding the primitive array values copied from the
*        NDF extension (see smf_ext2km). Only time-indexed extension items 
*        are stored in this KeyMap.
*     from = int (Given)
*        The zero-based input time slice index for the values that are to 
*        be read. The contents of this time slice are unchanged.   
*     into = int (Given)
*        The zero-based index for the input time slice in to which the read 
*        values are to be merged. 
*     ndet = int (Given)
*        The number of detectors. 
*     mask = int * (Given)
*        Pointer to an array with "ndet" elements. Each element is a flag 
*        indicating if the are any good data values in the input spectrum
*        from the corresponding detector for the "from" time slice.
*     nts = int (Given)
*        The total number of input time slices described by the keyMap.
*     rts_num = int (Given)
*        The RTS_NUM value associated with both time slices.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function merges the extension values stored for an input 
*     time slice with the extension values for another time slice. 
*     If an extension item contains only a scalar value for each time 
*     slice, then an error is reported if there is any difference between
*     the scalar value in time slices "from" and "into". If an extension
*     item contains a vector (indexed by detector number) for each time 
*     slice, then any good detector values in "from" are used to over-write 
*     any corresponding bad values in "into" (an error is reported if a
*     detector has good - but different - values in both time slices). 
*     Detector are skipped if they have a zero value in the "mask" array.
*
*     It is assumed that the time slice axis is the last axis in the
*     extension array, and the detector axis (if it exists) is the last
*     but one axis in the extension array. Note, the inpuyt KeyMap only
*     contains entries for arrays that are index by time slice.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-APR-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
/*+
 *  Name:
 *     SAE_PAR.H

 *  Purpose:
 *     Define the Starlink ADAM Environment public constants.

 *  Language:
 *     Starlink ANSI C

 *  Type of module:
 *     Global constants header file.

 *  Description:
 *     This file defines the ADAM status values for non-ADAM users.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *     12-JUN-1990 (PCTR):
 *        Original version.
 *     23-SEP-2005 (TIMJ):
 *        Use proper constants rather than the C preprocessor
 *     25-SEP-2005 (TIMJ):
 *        Prevent the constants being defined multiple times if
 *        the file is included multiple times.

 *  Bugs:

 *- */
/* OK Status. */
enum { SAI__OK = 0 };
/* Warning. */
enum { SAI__WARN = 148013859 };
/* Error. */
enum { SAI__ERROR = 148013867 };
/*
*+
*  Name:
*     ast.h

*  Purpose:
*     Define the public C interface to the AST library.

*  Language:
*     ANSI C

*  Type of Module:
*     C header file.

*  Description:
*     This file defines the public C interface to the AST library. It contains
*     all the type definitions, function prototypes, macro definitions, etc.
*     needed to use the library.

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
*     DSB: D.S. Berry (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-APR-2008 (makeh):
*        Original version, generated automatically from the internal header
*        files by the "makeh" script.
*     {enter_changes_here}
*-
*/
/* xml. */
/* ==== */
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
typedef AstXmlObject AstXmlCharData;
typedef AstXmlObject AstXmlContentItem;
typedef AstXmlObject AstXmlMiscItem;
typedef AstXmlObject AstXmlParent;
struct AstXmlObject {
    AstXmlParent *parent;
    long int type;
    int id;
};
struct AstXmlAttribute {
    AstXmlObject obj;
    char *name;
    char *value;
    char *prefix;
};
struct AstXmlNamespace {
    AstXmlObject obj;
    char *prefix;
    char *uri;
};
struct AstXmlElement {
    AstXmlObject obj;
    char *name;
    AstXmlAttribute **attrs;
    int nattr;
    AstXmlContentItem **items;
    int nitem;
    char *defns;
    char *prefix;
    AstXmlNamespace **nsprefs;
    int nnspref;
    int complete;
};
struct AstXmlBlack {
    AstXmlObject obj;
    char *text;
};
struct AstXmlWhite {
    AstXmlObject obj;
    char *text;
};
struct AstXmlCDataSection {
    AstXmlObject obj;
    char *text;
};
struct AstXmlComment {
    AstXmlObject obj;
    char *text;
};
struct AstXmlPI {
    AstXmlObject obj;
    char *target;
    char *text;
};
struct AstXmlDocument {
    AstXmlObject obj;
    AstXmlPrologue *prolog;
    AstXmlElement *root;
    AstXmlMiscItem **epilog;
    int nepi;
    AstXmlElement *current;
};
struct AstXmlPrologue {
    AstXmlObject obj;
    AstXmlDeclPI *xmldecl;
    AstXmlMiscItem **misc1;
    int nmisc1;
    AstXmlDTDec *dtdec;
    AstXmlMiscItem **misc2;
    int nmisc2;
};
struct AstXmlDeclPI {
    AstXmlObject obj;
    char *text;
};
struct AstXmlDTDec {
    AstXmlObject obj;
    char *name;
    char *external;
    char *internal;
};
AstXmlAttribute *astXmlCheckAttribute_(void *, int);
AstXmlBlack *astXmlCheckBlack_(void *, int);
AstXmlCDataSection *astXmlCheckCDataSection_(void *, int);
AstXmlComment *astXmlCheckComment_(void *, int);
AstXmlContentItem *astXmlGetItem_(AstXmlElement *, int);
AstXmlDTDec *astXmlCheckDTDec_(void *, int);
AstXmlDeclPI *astXmlCheckDeclPI_(void *, int);
AstXmlDocument *astXmlCheckDocument_(void *, int);
AstXmlElement *astXmlAddElement_(AstXmlElement *, const char *,
				 const char *);
AstXmlElement *astXmlCheckElement_(void *, int);
AstXmlParent *astXmlGetParent_(AstXmlObject *);
AstXmlObject *astXmlGetRoot_(AstXmlObject *);
AstXmlElement *astXmlReadDocument_(AstXmlDocument **,
				   int (*)(AstXmlElement *), int,
				   char (*)(void *), void *);
AstXmlNamespace *astXmlCheckNamespace_(void *, int);
AstXmlObject *astXmlCopy_(AstXmlObject *);
AstXmlObject *astXmlCheckObject_(void *, int);
AstXmlPI *astXmlCheckPI_(void *, int);
AstXmlPrologue *astXmlCheckPrologue_(void *, int);
AstXmlWhite *astXmlCheckWhite_(void *, int);
AstXmlCharData *astXmlCheckCharData_(void *, int);
AstXmlContentItem *astXmlCheckContentItem_(void *, int);
AstXmlMiscItem *astXmlCheckMiscItem_(void *, int);
AstXmlParent *astXmlCheckParent_(void *, int);
const char *astXmlFormat_(AstXmlObject *);
const char *astXmlGetAttributeValue_(AstXmlElement *, const char *);
const char *astXmlGetName_(AstXmlObject *);
const char *astXmlGetTag_(AstXmlObject *, int);
const char *astXmlGetURI_(AstXmlObject *);
const char *astXmlGetValue_(AstXmlObject *, int);
const char *astXmlShow_(AstXmlObject *);
int astXmlCheckType_(void *, long int);
int astXmlGetNattr_(AstXmlElement *);
int astXmlGetNitem_(AstXmlElement *);
void *astXmlAnnulTree_(AstXmlObject *);
void *astXmlAnnul_(AstXmlObject *);
void *astXmlDelete_(void *);
void astXmlAddAttr_(AstXmlElement *, const char *, const char *,
		    const char *);
void astXmlAddCDataSection_(AstXmlElement *, const char *);
void astXmlAddCharData_(AstXmlParent *, int, const char *);
void astXmlAddComment_(AstXmlParent *, int, const char *);
void astXmlAddPI_(AstXmlParent *, int, const char *, const char *);
void astXmlAddURI_(AstXmlElement *, const char *, const char *);
void astXmlInsertElement_(AstXmlElement *, AstXmlElement *);
void astXmlPurge_(AstXmlParent *);
void astXmlRemoveAttr_(AstXmlElement *, const char *, const char *);
void astXmlRemoveItem_(AstXmlContentItem *);
void astXmlRemoveURI_(AstXmlElement *, const char *);
void astXmlSetXmlDec_(AstXmlDocument *, const char *);
void astXmlSetDTDEC_(AstXmlDocument *, const char *, const char *,
		     const char *);
/* wcstrig. */
/* ======== */
double astCosd(const double);
double astSind(const double);
double astTand(const double);
double astACosd(const double);
double astASind(const double);
double astATand(const double);
double astATan2d(const double, const double);
/* proj. */
/* ===== */
extern int npcode;
extern char pcodes[26][4];
struct AstPrjPrm {
    char code[4];
    int flag;
    double phi0, theta0;
    double r0;
    double *p;
    double *p2;
    double w[20];
    int n;
    int (*astPRJfwd) (const double, const double,
		      struct AstPrjPrm *, double *, double *);
    int (*astPRJrev) (const double, const double,
		      struct AstPrjPrm *, double *, double *);
};
int astPRJset(const char[], struct AstPrjPrm *);
int astPRJfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astPRJrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAZPset(struct AstPrjPrm *);
int astAZPfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAZPrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSZPset(struct AstPrjPrm *);
int astSZPfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSZPrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTANset(struct AstPrjPrm *);
int astTANfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTANrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSTGset(struct AstPrjPrm *);
int astSTGfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSTGrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSINset(struct AstPrjPrm *);
int astSINfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSINrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astARCset(struct AstPrjPrm *);
int astARCfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astARCrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astZPNset(struct AstPrjPrm *);
int astZPNfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astZPNrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astZEAset(struct AstPrjPrm *);
int astZEAfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astZEArev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAIRset(struct AstPrjPrm *);
int astAIRfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAIRrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCYPset(struct AstPrjPrm *);
int astCYPfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCYPrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCEAset(struct AstPrjPrm *);
int astCEAfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCEArev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCARset(struct AstPrjPrm *);
int astCARfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCARrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astMERset(struct AstPrjPrm *);
int astMERfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astMERrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSFLset(struct AstPrjPrm *);
int astSFLfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astSFLrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astPARset(struct AstPrjPrm *);
int astPARfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astPARrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astMOLset(struct AstPrjPrm *);
int astMOLfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astMOLrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAITset(struct AstPrjPrm *);
int astAITfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astAITrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOPset(struct AstPrjPrm *);
int astCOPfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOPrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOEset(struct AstPrjPrm *);
int astCOEfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOErev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCODset(struct AstPrjPrm *);
int astCODfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCODrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOOset(struct AstPrjPrm *);
int astCOOfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCOOrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astBONset(struct AstPrjPrm *);
int astBONfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astBONrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astPCOset(struct AstPrjPrm *);
int astPCOfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astPCOrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTSCset(struct AstPrjPrm *);
int astTSCfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTSCrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCSCset(struct AstPrjPrm *);
int astCSCfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astCSCrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astQSCset(struct AstPrjPrm *);
int astQSCfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astQSCrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astHPXset(struct AstPrjPrm *);
int astHPXfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astHPXrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTPNset(struct AstPrjPrm *);
int astTPNfwd(const double, const double, struct AstPrjPrm *, double *,
	      double *);
int astTPNrev(const double, const double, struct AstPrjPrm *, double *,
	      double *);
extern const char *astPRJset_errmsg[];
extern const char *astPRJfwd_errmsg[];
extern const char *astPRJrev_errmsg[];
/* memory. */
/* ======= */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* snaroff@next.com says the NeXT needs this.  */
/* Irix 5.1 needs this.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef long int ptrdiff_t;
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef long unsigned int size_t;
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
   symbols in the _FOO_T_ family, stays defined even after its
   corresponding type is defined).  If we define wchar_t, then we
   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
   we undef _WCHAR_T_, then we must also define rune_t, since 
   headers like runetype.h assume that if machine/ansi.h is included,
   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
   the same type." */
/* FreeBSD 5 can't be handled well using "traditional" logic above
   since it no longer defines _BSD_RUNE_T_ yet still desires to export
   rune_t in some cases... */
typedef int wchar_t;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
extern int *starlink_ast_status_ptr;
int *astWatch_(int *);
int astStatus_(void);
void astClearStatus_(void);
void astSetStatus_(int);
void astAt_(const char *, const char *, int, int);
void astError_(int, const char *, ...);
int astReporting_(int);
int astMemCaching_(int);
char **astChrSplit_(const char *, int *);
char **astChrSplitRE_(const char *, const char *, int *);
char **astChrSplitC_(const char *, char, int *);
int astChrMatch_(const char *, const char *);
int astChrMatchN_(const char *, const char *, size_t);
char **astStringArray_(const char *, int, int);
char *astString_(const char *, int);
int astSscanf_(const char *str, const char *format, ...);
size_t astSizeOf_(const void *);
int astIsDynamic_(const void *);
size_t astTSizeOf_(const void *);
void *astFree_(void *);
void *astGrow_(void *, int, size_t);
void *astMalloc_(size_t);
void *astRealloc_(void *, size_t);
void *astStore_(void *, const void *, size_t);
size_t astChrLen_(const char *);
void astRemoveLeadingBlanks_(char *);
char *astAppendString_(char *, int *, const char *);
char *astChrSub_(const char *, const char *, const char *[], int);
/* error. */
/* ====== */
/* unit. */
/* ===== */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */
/* Define __gnuc_va_list.  */
typedef __builtin_va_list __gnuc_va_list;
/* Define the standard macros for the user,
   if this invocation was from the user program.  */
/* Define va_list, if desired, from __gnuc_va_list. */
/* We deliberately do not define va_list when called from
   stdio.h, because ANSI C says that stdio.h is not supposed to define
   va_list.  stdio.h needs to have access to that data type, 
   but must not use that name.  It should use the name __gnuc_va_list,
   which is safe because it is reserved for the implementation.  */
/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.
   But on BSD NET2 we must not test or define or undef it.
   (Note that the comments in NET 2's ansi.h
   are incorrect for _VA_LIST_--see stdio.h!)  */
/* The macro _VA_LIST_DEFINED is used in Windows NT 3.5  */
/* The macro _VA_LIST is used in SCO Unix 3.2.  */
/* The macro _VA_LIST_T_H is used in the Bull dpx2  */
/* The macro __va_list__ is used by BeOS.  */
typedef __gnuc_va_list va_list;
/* Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  5.2.4.2.2  Characteristics of floating types <float.h>
 */
/* Radix of exponent representation, b. */
/* Number of base-FLT_RADIX digits in the significand, p.  */
/* Number of decimal digits, q, such that any floating-point number with q
   decimal digits can be rounded into a floating-point number with p radix b
   digits and back again without change to the q decimal digits,

	p * log10(b)			if b is a power of 10
	floor((p - 1) * log10(b))	otherwise
*/
/* Minimum int x such that FLT_RADIX**(x-1) is a normalized float, emin */
/* Minimum negative integer such that 10 raised to that power is in the
   range of normalized floating-point numbers,

	ceil(log10(b) * (emin - 1))
*/
/* Maximum int x such that FLT_RADIX**(x-1) is a representable float, emax.  */
/* Maximum integer such that 10 raised to that power is in the range of
   representable finite floating-point numbers,

	floor(log10((1 - b**-p) * b**emax))
*/
/* Maximum representable finite floating-point number,

	(1 - b**-p) * b**emax
*/
/* The difference between 1 and the least value greater than 1 that is
   representable in the given floating point type, b**1-p.  */
/* Minimum normalized positive floating-point number, b**(emin - 1).  */
/* Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1: unknown.  */
/* ??? This is supposed to change with calls to fesetround in <fenv.h>.  */
/* Define ISO C stdio on top of C++ iostreams.
   Copyright (C) 1991,1994-2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/*
 *	ISO C99 Standard: 7.19 Input/output	<stdio.h>
 */
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* These are defined by the user (or the compiler)
   to specify the desired environment:

   __STRICT_ANSI__	ISO Standard C.
   _ISOC99_SOURCE	Extensions to ISO C89 from ISO C99.
   _POSIX_SOURCE	IEEE Std 1003.1.
   _POSIX_C_SOURCE	If ==1, like _POSIX_SOURCE; if >=2 add IEEE Std 1003.2;
			if >=199309L, add IEEE Std 1003.1b-1993;
			if >=199506L, add IEEE Std 1003.1c-1995;
			if >=200112L, all of IEEE 1003.1-2004
   _XOPEN_SOURCE	Includes POSIX and XPG things.  Set to 500 if
			Single Unix conformance is wanted, to 600 for the
			upcoming sixth revision.
   _XOPEN_SOURCE_EXTENDED XPG things and X/Open Unix extensions.
   _LARGEFILE_SOURCE	Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE	Additional functionality from LFS for large files.
   _FILE_OFFSET_BITS=N	Select default filesystem interface.
   _BSD_SOURCE		ISO C, POSIX, and 4.3BSD things.
   _SVID_SOURCE		ISO C, POSIX, and SVID things.
   _ATFILE_SOURCE	Additional *at interfaces.
   _GNU_SOURCE		All of the above, plus GNU extensions.
   _REENTRANT		Select additionally reentrant object.
   _THREAD_SAFE		Same as _REENTRANT, often used by other systems.
   _FORTIFY_SOURCE	If set to numeric value > 0 additional security
			measures are defined, according to level.

   The `-ansi' switch to the GNU C compiler defines __STRICT_ANSI__.
   If none of these are defined, the default is to have _SVID_SOURCE,
   _BSD_SOURCE, and _POSIX_SOURCE set to one and _POSIX_C_SOURCE set to
   200112L.  If more than one of these are defined, they accumulate.
   For example __STRICT_ANSI__, _POSIX_SOURCE and _POSIX_C_SOURCE
   together give you ISO C, 1003.1, and 1003.2, but nothing else.

   These are defined by this file and are used by the
   header files to decide what to declare or define:

   __USE_ISOC99		Define ISO C99 things.
   __USE_POSIX		Define IEEE Std 1003.1 things.
   __USE_POSIX2		Define IEEE Std 1003.2 things.
   __USE_POSIX199309	Define IEEE Std 1003.1, and .1b things.
   __USE_POSIX199506	Define IEEE Std 1003.1, .1b, .1c and .1i things.
   __USE_XOPEN		Define XPG things.
   __USE_XOPEN_EXTENDED	Define X/Open Unix things.
   __USE_UNIX98		Define Single Unix V2 things.
   __USE_XOPEN2K        Define XPG6 things.
   __USE_LARGEFILE	Define correct standard I/O things.
   __USE_LARGEFILE64	Define LFS things with separate names.
   __USE_FILE_OFFSET64	Define 64bit interface as default.
   __USE_BSD		Define 4.3BSD things.
   __USE_SVID		Define SVID things.
   __USE_MISC		Define things common to BSD and System V Unix.
   __USE_ATFILE		Define *at interfaces and AT_* constants for them.
   __USE_GNU		Define GNU extensions.
   __USE_REENTRANT	Define reentrant/thread-safe *_r functions.
   __USE_FORTIFY_LEVEL	Additional security measures used, according to level.
   __FAVOR_BSD		Favor 4.3BSD things in cases of conflict.

   The macros `__GNU_LIBRARY__', `__GLIBC__', and `__GLIBC_MINOR__' are
   defined by this file unconditionally.  `__GNU_LIBRARY__' is provided
   only for compatibility.  All new code should use the other symbols
   to test for features.

   All macros listed above as possibly being defined by this file are
   explicitly undefined if they are not explicitly defined.
   Feature-test macros that are not defined by the user or compiler
   but are implied by the other feature-test macros defined (or by the
   lack of any definitions) are defined by the file.  */
/* Undefine everything, so we get a clean slate.  */
/* Suppress kernel-name space pollution unless user expressedly asks
   for it.  */
/* Always use ISO C things.  */
/* Convenience macros to test the versions of glibc and gcc.
   Use them like this:
   #if __GNUC_PREREQ (2,8)
   ... code requiring gcc 2.8 or later ...
   #endif
   Note - they won't work for gcc1 or glibc1, since the _MINOR macros
   were not defined then.  */
/* If _BSD_SOURCE was defined by the user, favor BSD over POSIX.  */
/* If _GNU_SOURCE was defined by the user, turn on all the other features.  */
/* If nothing (other than _GNU_SOURCE) is defined,
   define _BSD_SOURCE and _SVID_SOURCE.  */
/* This is to enable the ISO C99 extension.  Also recognize the old macro
   which was used prior to the standard acceptance.  This macro will
   eventually go away and the features enabled by default once the ISO C99
   standard is widely adopted.  */
/* If none of the ANSI/POSIX macros are defined, use POSIX.1 and POSIX.2
   (and IEEE Std 1003.1b-1993 unless _XOPEN_SOURCE is defined).  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO 10646-1 (2nd ed., published 2000-09-15) / Unicode 3.1.  */
/* This macro indicates that the installed library is the GNU C Library.
   For historic reasons the value now is 6 and this will stay from now
   on.  The use of this variable is deprecated.  Use __GLIBC__ and
   __GLIBC_MINOR__ now (see below) when you want to test for a specific
   GNU C library version and use the values in <gnu/lib-names.h> to get
   the sonames of the shared libraries.  */
/* Major and minor version number of the GNU C library package.  Use
   these macros to test for features in specific releases.  */
/* Decide whether a compiler supports the long long datatypes.  */
/* This is here only because every header file already includes this one.  */
/* Copyright (C) 1992-2001, 2002, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* We are almost always included from features.h. */
/* The GNU libc does not support any K&R compilers or the traditional mode
   of ISO C compilers anymore.  Check for some of the combinations not
   anymore supported.  */
/* Some user header file might have defined this before.  */
/* GCC can always grok prototypes.  For C++ programs we add throw()
   to help it optimize the function calls.  But this works only with
   gcc 2.8.x and egcs.  For gcc 3.2 and up we even mark C functions
   as non-throwing using a function attribute since programs can use
   the -fexceptions options for C code as well.  */
/* These two macros are not used in glibc anymore.  They are kept here
   only because some other projects expect the macros to be defined.  */
/* For these things, GCC behaves the ANSI way normally,
   and the non-ANSI way under -traditional.  */
/* This is not a typedef so `const __ptr_t' does the right thing.  */
/* C++ needs to know that types and declarations are C, not C++.  */
/* The standard library needs the functions from the ISO C90 standard
   in the std namespace.  At the same time we want to be safe for
   future changes and we include the ISO C99 code in the non-standard
   namespace __c99.  The C++ wrapper header take case of adding the
   definitions to the global namespace.  */
/* For compatibility we do not add the declarations into any
   namespace.  They will end up in the global namespace which is what
   old code expects.  */
/* Support for bounded pointers.  */
/* Fortify support.  */
/* Support for flexible arrays.  */
/* GCC 2.97 supports C99 flexible array members.  */
/* __asm__ ("xyz") is used throughout the headers to rename functions
   at the assembly language level.  This is wrapped by the __REDIRECT
   macro, in order to support compilers that can do this some other
   way.  When compilers don't support asm-names at all, we have to do
   preprocessor tricks instead (which don't have exactly the right
   semantics, but it's the best we can do).

   Example:
   int __REDIRECT(setpgrp, (__pid_t pid, __pid_t pgrp), setpgid); */
/*
#elif __SOME_OTHER_COMPILER__

# define __REDIRECT(name, proto, alias) name proto; 	_Pragma("let " #name " = " #alias)
)
*/
/* GCC has various useful declarations that can be made with the
   `__attribute__' syntax.  All of the ways we use this do fine if
   they are omitted for compilers that don't understand it. */
/* At some point during the gcc 2.96 development the `malloc' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */
/* At some point during the gcc 2.96 development the `pure' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */
/* At some point during the gcc 3.1 development the `used' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */
/* gcc allows marking deprecated functions.  */
/* At some point during the gcc 2.8 development the `format_arg' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.
   If several `format_arg' attributes are given for the same function, in
   gcc-3.0 and older, all but the last one are ignored.  In newer gccs,
   all designated arguments are considered.  */
/* At some point during the gcc 2.97 development the `strfmon' format
   attribute for functions was introduced.  We don't want to use it
   unconditionally (although this would be possible) since it
   generates warnings.  */
/* The nonull function attribute allows to mark pointer parameters which
   must not be NULL.  */
/* If fortification mode, we warn about unused results of certain
   function calls which can lead to problems.  */
/* Forces a function to be always inlined.  */
/* GCC 4.3 and above with -std=c99 or -std=gnu99 implements ISO C99
   inline semantics, unless -fgnu89-inline is used.  */
/* It is possible to compile containing GCC extensions even if GCC is
   run in pedantic mode if the uses are carefully marked using the
   `__extension__' keyword.  But this is not generally available before
   version 2.8.  */
/* __restrict is known in EGCS 1.2 and above. */
/* ISO C99 also allows to declare arrays as non-overlapping.  The syntax is
     array_name[restrict]
   GCC 3.1 supports this.  */
/* Determine the wordsize from the preprocessor defines.  */
/* If we don't have __REDIRECT, prototypes will be missing if
   __USE_FILE_OFFSET64 but not __USE_LARGEFILE[64]. */
/* Decide whether we can define 'extern inline' functions in headers.  */
/* This is here only because every header file already includes this one.
   Get the definitions of all the appropriate `__stub_FUNCTION' symbols.
   <gnu/stubs.h> contains `#define __stub_FUNCTION' when FUNCTION is a stub
   that will always return failure (and set errno to ENOSYS).  */
/* This file selects the right generated file of `__stub_FUNCTION' macros
   based on the architecture being compiled for.  */
/* Determine the wordsize from the preprocessor defines.  */
/* This file is automatically generated.
   It defines a symbol `__stub_FUNCTION' for each function
   in the C library which is a stub, meaning it will fail
   every time called, usually setting errno to ENOSYS.  */

/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/*
 * Never include this file directly; use <sys/types.h> instead.
 */
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* Determine the wordsize from the preprocessor defines.  */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* Convenience types.  */
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
/* Fixed-size types, underlying types depend on word size and compiler.  */
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
/* quad_t is also 64 bits.  */
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
/* The machine-dependent file <bits/typesizes.h> defines __*_T_TYPE
   macros for each of the OS types we define below.  The definitions
   of those macros must use the following macros for underlying types.
   We define __S<SIZE>_TYPE and __U<SIZE>_TYPE for the signed and unsigned
   variants of each of the following integer types on this machine.

	16		-- "natural" 16-bit type (always short)
	32		-- "natural" 32-bit type (always int)
	64		-- "natural" 64-bit type (long or long long)
	LONG32		-- 32-bit type, traditionally long
	QUAD		-- 64-bit type, always long long
	WORD		-- natural type of __WORDSIZE bits (int or long)
	LONGWORD	-- type of __WORDSIZE bits, traditionally long

   We distinguish WORD/LONGWORD, 32/LONG32, and 64/QUAD so that the
   conventional uses of `long' or `long long' type modifiers match the
   types we define, even when a less-adorned type would be the same size.
   This matters for (somewhat) portably writing printf/scanf formats for
   these types, where using the appropriate l or ll format modifiers can
   make the typedefs and the formats match up across all GNU platforms.  If
   we used `long' when it's 64 bits where `long long' is expected, then the
   compiler would warn about the formats not matching the argument types,
   and the programmer changing them to shut up the compiler would break the
   program's portability.

   Here we assume what is presently the case in all the GCC configurations
   we support: long long is always 64 bits, long is always word/address size,
   and int is always 32 bits.  */
/* No need to mark the typedef with __extension__.   */
/* bits/typesizes.h -- underlying types for *_t.  Generic version.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* See <bits/types.h> for the meaning of these macros.  This file exists so
   that <bits/types.h> need not vary across different GNU platforms.  */
/* Number of descriptors that can fit in an `fd_set'.  */
typedef unsigned long int __dev_t;	/* Type of device numbers.  */
typedef unsigned int __uid_t;	/* Type of user identifications.  */
typedef unsigned int __gid_t;	/* Type of group identifications.  */
typedef unsigned long int __ino_t;	/* Type of file serial numbers.  */
typedef unsigned long int __ino64_t;	/* Type of file serial numbers (LFS). */
typedef unsigned int __mode_t;	/* Type of file attribute bitmasks.  */
typedef unsigned long int __nlink_t;	/* Type of file link counts.  */
typedef long int __off_t;	/* Type of file sizes and offsets.  */
typedef long int __off64_t;	/* Type of file sizes and offsets (LFS).  */
typedef int __pid_t;		/* Type of process identifications.  */
typedef struct {
    int __val[2];
} __fsid_t;			/* Type of file system IDs.  */
typedef long int __clock_t;	/* Type of CPU usage counts.  */
typedef unsigned long int __rlim_t;	/* Type for resource measurement.  */
typedef unsigned long int __rlim64_t;	/* Type for resource measurement (LFS).  */
typedef unsigned int __id_t;	/* General type for IDs.  */
typedef long int __time_t;	/* Seconds since the Epoch.  */
typedef unsigned int __useconds_t;	/* Count of microseconds.  */
typedef long int __suseconds_t;	/* Signed count of microseconds.  */
typedef int __daddr_t;		/* The type of a disk address.  */
typedef long int __swblk_t;	/* Type of a swap block maybe?  */
typedef int __key_t;		/* Type of an IPC key.  */
/* Clock ID used in clock and timer functions.  */
typedef int __clockid_t;
/* Timer ID returned by `timer_create'.  */
typedef void *__timer_t;
/* Type to represent block size.  */
typedef long int __blksize_t;
/* Types from the Large File Support interface.  */
/* Type to count number of disk blocks.  */
typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;
/* Type to count file system blocks.  */
typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;
/* Type to count file system nodes.  */
typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;
typedef long int __ssize_t;	/* Type of a byte count, or error.  */
/* These few don't really vary by system, they always correspond
   to one of the other defined types.  */
typedef __off64_t __loff_t;	/* Type of file sizes and offsets (LFS).  */
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;
/* Duplicates info from stdint.h but this is used in unistd.h.  */
typedef long int __intptr_t;
/* Duplicate info from sys/socket.h.  */
typedef unsigned int __socklen_t;
/* Define outside of namespace so the C++ is happy.  */
struct _IO_FILE;

/* The opaque type of streams.  This is the definition used elsewhere.  */
typedef struct _IO_FILE FILE;


/* The opaque type of streams.  This is the definition used elsewhere.  */
typedef struct _IO_FILE __FILE;
/* Copyright (C) 1991-1995,1997-2005,2006 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Written by Per Bothner <bothner@cygnus.com>.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.

   As a special exception, if you link the code in this file with
   files compiled with a GNU compiler to produce an executable,
   that does not cause the resulting executable to be covered by
   the GNU Lesser General Public License.  This exception does not
   however invalidate any other reasons why the executable file
   might be covered by the GNU Lesser General Public License.
   This exception applies to code released by its copyright holders
   in files containing the exception.  */
/* This file is needed by libio to define various configuration parameters.
   These are always the same in the GNU C library.  */
/* Define types for libio in terms of the standard internal type names.  */
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/*
 * Never include this file directly; use <sys/types.h> instead.
 */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef unsigned int wint_t;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* Copyright (C) 1995-2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/*
 *      ISO C99 Standard: 7.24
 *	Extended multibyte and wide character utilities	<wchar.h>
 */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* wchar_t type related definitions.
   Copyright (C) 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* We try to get wint_t from <stddef.h>, but not all GCC versions define it
   there.  So define it ourselves if it remains undefined.  */
/* Work around problems with the <stddef.h> file which doesn't put
   wint_t in the std namespace.  */
/* Conversion state information.  */
typedef struct {
    int __count;
    union {
	wint_t __wch;
	char __wchb[4];
    } __value;			/* Value so far.  */
} __mbstate_t;
/* The rest of the file is only used if used if __need_mbstate_t is not
   defined.  */
typedef struct {
    __off_t __pos;
    __mbstate_t __state;
} _G_fpos_t;
typedef struct {
    __off64_t __pos;
    __mbstate_t __state;
} _G_fpos64_t;
/* Copyright (C) 1997-1999, 2000-2002 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* This header provides no interface for a user to the internals of
   the gconv implementation in the libc.  Therefore there is no use
   for these definitions beside for writing additional gconv modules.  */
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* Copyright (C) 1995-2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/*
 *      ISO C99 Standard: 7.24
 *	Extended multibyte and wide character utilities	<wchar.h>
 */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* wchar_t type related definitions.
   Copyright (C) 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* We try to get wint_t from <stddef.h>, but not all GCC versions define it
   there.  So define it ourselves if it remains undefined.  */
/* Work around problems with the <stddef.h> file which doesn't put
   wint_t in the std namespace.  */
/* The rest of the file is only used if used if __need_mbstate_t is not
   defined.  */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/* ISO 10646 value used to signal invalid value.  */
/* Error codes for gconv functions.  */
enum {
    __GCONV_OK = 0,
    __GCONV_NOCONV,
    __GCONV_NODB,
    __GCONV_NOMEM,
    __GCONV_EMPTY_INPUT,
    __GCONV_FULL_OUTPUT,
    __GCONV_ILLEGAL_INPUT,
    __GCONV_INCOMPLETE_INPUT,
    __GCONV_ILLEGAL_DESCRIPTOR,
    __GCONV_INTERNAL_ERROR
};
/* Flags the `__gconv_open' function can set.  */
enum {
    __GCONV_IS_LAST = 0x0001,
    __GCONV_IGNORE_ERRORS = 0x0002
};
/* Forward declarations.  */
struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;
/* Type of a conversion function.  */
typedef int (*__gconv_fct) (struct __gconv_step *,
			    struct __gconv_step_data *,
			    __const unsigned char **,
			    __const unsigned char *, unsigned char **,
			    size_t *, int, int);
/* Type of a specialized conversion function for a single byte to INTERNAL.  */
typedef wint_t(*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);
/* Constructor and destructor for local data for conversion step.  */
typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);
/* Type of a transliteration/transscription function.  */
typedef int (*__gconv_trans_fct) (struct __gconv_step *,
				  struct __gconv_step_data *, void *,
				  __const unsigned char *,
				  __const unsigned char **,
				  __const unsigned char *,
				  unsigned char **, size_t *);
/* Function to call to provide transliteration module with context.  */
typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
					  __const unsigned char *,
					  unsigned char *,
					  unsigned char *);
/* Function to query module about supported encoded character sets.  */
typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
					size_t *);
/* Constructor and destructor for local data for transliteration.  */
typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);
struct __gconv_trans_data {
    /* Transliteration/Transscription function.  */
    __gconv_trans_fct __trans_fct;
    __gconv_trans_context_fct __trans_context_fct;
    __gconv_trans_end_fct __trans_end_fct;
    void *__data;
    struct __gconv_trans_data *__next;
};
/* Description of a conversion step.  */
struct __gconv_step {
    struct __gconv_loaded_object *__shlib_handle;
    __const char *__modname;
    int __counter;
    char *__from_name;
    char *__to_name;
    __gconv_fct __fct;
    __gconv_btowc_fct __btowc_fct;
    __gconv_init_fct __init_fct;
    __gconv_end_fct __end_fct;
    /* Information about the number of bytes needed or produced in this
       step.  This helps optimizing the buffer sizes.  */
    int __min_needed_from;
    int __max_needed_from;
    int __min_needed_to;
    int __max_needed_to;
    /* Flag whether this is a stateful encoding or not.  */
    int __stateful;
    void *__data;		/* Pointer to step-local data.  */
};
/* Additional data for steps in use of conversion descriptor.  This is
   allocated by the `init' function.  */
struct __gconv_step_data {
    unsigned char *__outbuf;	/* Output buffer for this step.  */
    unsigned char *__outbufend;	/* Address of first byte after the output
				   buffer.  */
    /* Is this the last module in the chain.  */
    int __flags;
    /* Counter for number of invocations of the module function for this
       descriptor.  */
    int __invocation_counter;
    /* Flag whether this is an internal use of the module (in the mb*towc*
       and wc*tomb* functions) or regular with iconv(3).  */
    int __internal_use;
    __mbstate_t *__statep;
    __mbstate_t __state;	/* This element must not be used directly by
				   any module; always use STATEP!  */
    /* Transliteration information.  */
    struct __gconv_trans_data *__trans;
};
/* Combine conversion step description with data.  */
typedef struct __gconv_info {
    size_t __nsteps;
    struct __gconv_step *__steps;
    __extension__ struct __gconv_step_data __data[];
} *__gconv_t;
typedef union {
    struct __gconv_info __cd;
    struct {
	struct __gconv_info __cd;
	struct __gconv_step_data __data;
    } __combined;
} _G_iconv_t;
typedef int _G_int16_t __attribute__ ((__mode__(__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__(__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__(__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__(__SI__)));
/* These library features are always available in the GNU C library.  */
/* This is defined by <bits/stat.h> if `st_blksize' exists.  */
/* These are the vtbl details for ELF.  */
/* ALL of these should be defined in _G_config.h */
/* This define avoids name pollution if we're using GNU stdarg.h */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */
/* For backward compatibility */
/* Magic numbers and bits for the _flags field.
   The magic numbers use the high-order bits of _flags;
   the remaining bits are available for variable flags.
   Note: The magic numbers must all be negative if stdio
   emulation is desired. */
/* These are "formatting flags" matching the iostream fmtflags enum values. */
struct _IO_jump_t;
struct _IO_FILE;
/* Handle lock.  */
typedef void _IO_lock_t;
/* A streammarker remembers a position in a buffer. */
struct _IO_marker {
    struct _IO_marker *_next;
    struct _IO_FILE *_sbuf;
    /* If _pos >= 0
       it points to _buf->Gbase()+_pos. FIXME comment */
    /* if _pos < 0, it points to _buf->eBptr()+_pos. FIXME comment */
    int _pos;
};
/* This is the structure from the libstdc++ codecvt class.  */
enum __codecvt_result {
    __codecvt_ok,
    __codecvt_partial,
    __codecvt_error,
    __codecvt_noconv
};
struct _IO_FILE {
    int _flags;			/* High-order word is _IO_MAGIC; rest is flags. */
    /* The following pointers correspond to the C++ streambuf protocol. */
    /* Note:  Tk uses the _IO_read_ptr and _IO_read_end fields directly. */
    char *_IO_read_ptr;		/* Current read pointer */
    char *_IO_read_end;		/* End of get area. */
    char *_IO_read_base;	/* Start of putback+get area. */
    char *_IO_write_base;	/* Start of put area. */
    char *_IO_write_ptr;	/* Current put pointer. */
    char *_IO_write_end;	/* End of put area. */
    char *_IO_buf_base;		/* Start of reserve area. */
    char *_IO_buf_end;		/* End of reserve area. */
    /* The following fields are used to support backing up and undo. */
    char *_IO_save_base;	/* Pointer to start of non-current get area. */
    char *_IO_backup_base;	/* Pointer to first valid character of backup area */
    char *_IO_save_end;		/* Pointer to end of non-current get area. */
    struct _IO_marker *_markers;
    struct _IO_FILE *_chain;
    int _fileno;
    int _flags2;
    __off_t _old_offset;	/* This used to be _offset but it's too small.  */
    /* 1+column number of pbase(); 0 is unknown. */
    unsigned short _cur_column;
    signed char _vtable_offset;
    char _shortbuf[1];
    /*  char* _save_gptr;  char* _save_egptr; */
    _IO_lock_t *_lock;
    __off64_t _offset;
    void *__pad1;
    void *__pad2;
    void *__pad3;
    void *__pad4;
    size_t __pad5;
    int _mode;
    /* Make sure we don't get into trouble again.  */
    char _unused2[15 * sizeof(int) - 4 * sizeof(void *) - sizeof(size_t)];
};
typedef struct _IO_FILE _IO_FILE;
struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
/* Functions to do I/O and file management for a stream.  */
/* Read NBYTES bytes from COOKIE into a buffer pointed to by BUF.
   Return number of bytes read.  */
typedef __ssize_t __io_read_fn(void *__cookie, char *__buf,
			       size_t __nbytes);
/* Write N bytes pointed to by BUF to COOKIE.  Write all N bytes
   unless there is an error.  Return number of bytes written, or -1 if
   there is an error without writing anything.  If the file has been
   opened for append (__mode.__append set), then set the file pointer
   to the end of the file and then do the write; if not, just write at
   the current file pointer.  */
typedef __ssize_t __io_write_fn(void *__cookie, __const char *__buf,
				size_t __n);
/* Move COOKIE's file position to *POS bytes from the
   beginning of the file (if W is SEEK_SET),
   the current position (if W is SEEK_CUR),
   or the end of the file (if W is SEEK_END).
   Set *POS to the new file position.
   Returns zero if successful, nonzero if not.  */
typedef int __io_seek_fn(void *__cookie, __off64_t * __pos, int __w);
/* Close COOKIE.  */
typedef int __io_close_fn(void *__cookie);
extern int __underflow(_IO_FILE *);
extern int __uflow(_IO_FILE *);
extern int __overflow(_IO_FILE *, int);
extern wint_t __wunderflow(_IO_FILE *);
extern wint_t __wuflow(_IO_FILE *);
extern wint_t __woverflow(_IO_FILE *, wint_t);
extern int _IO_getc(_IO_FILE * __fp);
extern int _IO_putc(int __c, _IO_FILE * __fp);
extern int _IO_feof(_IO_FILE * __fp) __attribute__ ((__nothrow__));
extern int _IO_ferror(_IO_FILE * __fp) __attribute__ ((__nothrow__));
extern int _IO_peekc_locked(_IO_FILE * __fp);
/* This one is for Emacs. */
extern void _IO_flockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile(_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_vfscanf(_IO_FILE * __restrict, const char *__restrict,
		       __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf(_IO_FILE * __restrict, const char *__restrict,
			__gnuc_va_list);
extern __ssize_t _IO_padn(_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn(_IO_FILE *, void *, size_t);
extern __off64_t _IO_seekoff(_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos(_IO_FILE *, __off64_t, int);
extern void _IO_free_backup_area(_IO_FILE *) __attribute__ ((__nothrow__));
/* The type of the second argument to `fgetpos' and `fsetpos'.  */

typedef _G_fpos_t fpos_t;

/* The possibilities for the third argument to `setvbuf'.  */
/* Default buffer size.  */
/* End of file character.
   Some things throughout the library rely on this being -1.  */
/* The possibilities for the third argument to `fseek'.
   These values should not be changed.  */
/* Default path prefix for `tempnam' and `tmpnam'.  */
/* Get the values:
   L_tmpnam	How long an array of chars must be to be passed to `tmpnam'.
   TMP_MAX	The minimum number of unique filenames generated by tmpnam
   		(and tempnam when it uses tmpnam's name space),
		or tempnam (the two are separate).
   L_ctermid	How long an array to pass to `ctermid'.
   L_cuserid	How long an array to pass to `cuserid'.
   FOPEN_MAX	Minimum number of files that can be open at once.
   FILENAME_MAX	Maximum length of a filename.  */
/* Copyright (C) 1994, 1997, 1998, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* Standard streams.  */
extern struct _IO_FILE *stdin;	/* Standard input stream.  */
extern struct _IO_FILE *stdout;	/* Standard output stream.  */
extern struct _IO_FILE *stderr;	/* Standard error output stream.  */
/* C89/C99 say they're macros.  Make them happy.  */

/* Remove file FILENAME.  */
extern int remove(__const char *__filename) __attribute__ ((__nothrow__));
/* Rename file OLD to NEW.  */
extern int rename(__const char *__old, __const char *__new)
    __attribute__ ((__nothrow__));


/* Create a temporary file and open it read/write.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern FILE *tmpfile(void);
/* Generate a temporary filename.  */
extern char *tmpnam(char *__s) __attribute__ ((__nothrow__));

/* This is the reentrant variant of `tmpnam'.  The only difference is
   that it does not allow S to be NULL.  */
extern char *tmpnam_r(char *__s) __attribute__ ((__nothrow__));
/* Generate a unique temporary filename using up to five characters of PFX
   if it is not NULL.  The directory to put this file in is searched for
   as follows: First the environment variable "TMPDIR" is checked.
   If it contains the name of a writable directory, that directory is used.
   If not and if DIR is not NULL, that value is checked.  If that fails,
   P_tmpdir is tried and finally "/tmp".  The storage for the filename
   is allocated by `malloc'.  */
extern char *tempnam(__const char *__dir, __const char *__pfx)
    __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

/* Close STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fclose(FILE * __stream);
/* Flush STREAM, or all streams if STREAM is NULL.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fflush(FILE * __stream);

/* Faster versions when locking is not required.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fflush_unlocked(FILE * __stream);

/* Open a file and create a new stream for it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *fopen(__const char *__restrict __filename,
		   __const char *__restrict __modes);
/* Open a file, replacing an existing stream with it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *freopen(__const char *__restrict __filename,
		     __const char *__restrict __modes,
		     FILE * __restrict __stream);

/* Create a new stream that refers to an existing system file descriptor.  */
extern FILE *fdopen(int __fd, __const char *__modes)
    __attribute__ ((__nothrow__));

/* If BUF is NULL, make STREAM unbuffered.
   Else make it use buffer BUF, of size BUFSIZ.  */
extern void setbuf(FILE * __restrict __stream, char *__restrict __buf)
    __attribute__ ((__nothrow__));
/* Make STREAM use buffering mode MODE.
   If BUF is not NULL, use N bytes of it for buffering;
   else allocate an internal buffer N bytes long.  */
extern int setvbuf(FILE * __restrict __stream, char *__restrict __buf,
		   int __modes, size_t __n) __attribute__ ((__nothrow__));

/* If BUF is NULL, make STREAM unbuffered.
   Else make it use SIZE bytes of BUF for buffering.  */
extern void setbuffer(FILE * __restrict __stream, char *__restrict __buf,
		      size_t __size) __attribute__ ((__nothrow__));
/* Make STREAM line-buffered.  */
extern void setlinebuf(FILE * __stream) __attribute__ ((__nothrow__));

/* Write formatted output to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fprintf(FILE * __restrict __stream,
		   __const char *__restrict __format, ...);
/* Write formatted output to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int printf(__const char *__restrict __format, ...);
/* Write formatted output to S.  */
extern int sprintf(char *__restrict __s,
		   __const char *__restrict __format, ...)
    __attribute__ ((__nothrow__));
/* Write formatted output to S from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vfprintf(FILE * __restrict __s,
		    __const char *__restrict __format,
		    __gnuc_va_list __arg);
/* Write formatted output to stdout from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vprintf(__const char *__restrict __format,
		   __gnuc_va_list __arg);
/* Write formatted output to S from argument list ARG.  */
extern int vsprintf(char *__restrict __s,
		    __const char *__restrict __format,
		    __gnuc_va_list __arg) __attribute__ ((__nothrow__));


/* Maximum chars of output to write in MAXLEN.  */
extern int snprintf(char *__restrict __s, size_t __maxlen,
		    __const char *__restrict __format, ...)
    __attribute__ ((__nothrow__))
    __attribute__ ((__format__(__printf__, 3, 4)));
extern int vsnprintf(char *__restrict __s, size_t __maxlen,
		     __const char *__restrict __format,
		     __gnuc_va_list __arg)
    __attribute__ ((__nothrow__))
    __attribute__ ((__format__(__printf__, 3, 0)));


/* Read formatted input from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fscanf(FILE * __restrict __stream,
		  __const char *__restrict __format, ...);
/* Read formatted input from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int scanf(__const char *__restrict __format, ...);
/* Read formatted input from S.  */
extern int sscanf(__const char *__restrict __s,
		  __const char *__restrict __format, ...)
    __attribute__ ((__nothrow__));


/* Read a character from STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int fgetc(FILE * __stream);
extern int getc(FILE * __stream);
/* Read a character from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int getchar(void);

/* The C standard explicitly says this is a macro, so we always do the
   optimization for it.  */
/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int getc_unlocked(FILE * __stream);
extern int getchar_unlocked(void);
/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fgetc_unlocked(FILE * __stream);

/* Write a character to STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.

   These functions is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fputc(int __c, FILE * __stream);
extern int putc(int __c, FILE * __stream);
/* Write a character to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int putchar(int __c);

/* The C standard explicitly says this can be a macro,
   so we always do the optimization for it.  */
/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fputc_unlocked(int __c, FILE * __stream);
/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int putc_unlocked(int __c, FILE * __stream);
extern int putchar_unlocked(int __c);
/* Get a word (int) from STREAM.  */
extern int getw(FILE * __stream);
/* Write a word (int) to STREAM.  */
extern int putw(int __w, FILE * __stream);

/* Get a newline-terminated string of finite length from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern char *fgets(char *__restrict __s, int __n,
		   FILE * __restrict __stream);
/* Get a newline-terminated string from stdin, removing the newline.
   DO NOT USE THIS FUNCTION!!  There is no limit on how much it will read.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern char *gets(char *__s);


/* Write a string to STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern int fputs(__const char *__restrict __s, FILE * __restrict __stream);
/* Write a string, followed by a newline, to stdout.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern int puts(__const char *__s);
/* Push a character back onto the input buffer of STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern int ungetc(int __c, FILE * __stream);
/* Read chunks of generic data from STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern size_t fread(void *__restrict __ptr, size_t __size,
		    size_t __n, FILE * __restrict __stream);
/* Write chunks of generic data to STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */
extern size_t fwrite(__const void *__restrict __ptr, size_t __size,
		     size_t __n, FILE * __restrict __s);

/* Faster versions when locking is not necessary.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */
extern size_t fread_unlocked(void *__restrict __ptr, size_t __size,
			     size_t __n, FILE * __restrict __stream);
extern size_t fwrite_unlocked(__const void *__restrict __ptr,
			      size_t __size, size_t __n,
			      FILE * __restrict __stream);

/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fseek(FILE * __stream, long int __off, int __whence);
/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern long int ftell(FILE * __stream);
/* Rewind to the beginning of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern void rewind(FILE * __stream);

/* The Single Unix Specification, Version 2, specifies an alternative,
   more adequate interface for the two functions above which deal with
   file offset.  `long int' is not the right type.  These definitions
   are originally defined in the Large File Support API.  */
/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fseeko(FILE * __stream, __off_t __off, int __whence);
/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern __off_t ftello(FILE * __stream);

/* Get STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fgetpos(FILE * __restrict __stream, fpos_t * __restrict __pos);
/* Set STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fsetpos(FILE * __stream, __const fpos_t * __pos);


/* Clear the error and EOF indicators for STREAM.  */
extern void clearerr(FILE * __stream) __attribute__ ((__nothrow__));
/* Return the EOF indicator for STREAM.  */
extern int feof(FILE * __stream) __attribute__ ((__nothrow__));
/* Return the error indicator for STREAM.  */
extern int ferror(FILE * __stream) __attribute__ ((__nothrow__));

/* Faster versions when locking is not required.  */
extern void clearerr_unlocked(FILE * __stream)
    __attribute__ ((__nothrow__));
extern int feof_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
extern int ferror_unlocked(FILE * __stream) __attribute__ ((__nothrow__));

/* Print a message describing the meaning of the value of errno.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern void perror(__const char *__s);

/* Provide the declarations for `sys_errlist' and `sys_nerr' if they
   are available on this system.  Even if available, these variables
   should not be used directly.  The `strerror' function provides
   all the necessary functionality.  */
/* Declare sys_errlist and sys_nerr, or don't.  Compatibility (do) version.
   Copyright (C) 2002 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */
/* sys_errlist and sys_nerr are deprecated.  Use strerror instead.  */
extern int sys_nerr;
extern __const char *__const sys_errlist[];
/* Return the system file descriptor for STREAM.  */
extern int fileno(FILE * __stream) __attribute__ ((__nothrow__));
/* Faster version when locking is not required.  */
extern int fileno_unlocked(FILE * __stream) __attribute__ ((__nothrow__));
/* Create a new stream connected to a pipe running the given command.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *popen(__const char *__command, __const char *__modes);
/* Close a stream opened by popen and return the status of its child.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int pclose(FILE * __stream);
/* Return the name of the controlling terminal.  */
extern char *ctermid(char *__s) __attribute__ ((__nothrow__));
/* These are defined in POSIX.1:1996.  */
/* Acquire ownership of STREAM.  */
extern void flockfile(FILE * __stream) __attribute__ ((__nothrow__));
/* Try to acquire ownership of STREAM but do not block if it is not
   possible.  */
extern int ftrylockfile(FILE * __stream) __attribute__ ((__nothrow__));
/* Relinquish the ownership granted for STREAM.  */
extern void funlockfile(FILE * __stream) __attribute__ ((__nothrow__));
/* If we are compiling with optimizing read this file.  It contains
   several optimizing inline functions and macros.  */

typedef struct AstObject {
    unsigned long check;
    size_t size;
    struct AstObjectVtab *vtab;
    char dynamic;
    int ref_count;
    char *id;
    char *ident;
    char usedefs;
} AstObject;
struct AstChannel;
typedef struct AstChannel {
    AstObject object;
    const char *(*source) (void);
    char *(*source_wrap) (const char *(*)(void));
    void (*sink) (const char *);
    void (*sink_wrap) (void (*)(const char *), const char *);
    int comment;
    int full;
    int skip;
} AstChannel;
AstChannel *astCheckChannel_(AstChannel *);
int astIsAChannel_(const AstChannel *);
AstChannel *astChannelId_(const char *(*)(void), void (*)(const char *),
			  const char *, ...);
AstChannel *astChannelForId_(const char *(*)(void),
			     char *(*)(const char *(*)(void)),
			     void (*)(const char *),
			     void (*)(void (*)(const char *),
				      const char *), const char *, ...);
AstObject *astRead_(AstChannel *);
int astWrite_(AstChannel *, AstObject *);
char *astSourceWrap_(const char *(*)(void));
void astSinkWrap_(void (*)(const char *), const char *);
AstObject *astCheckObject_(AstObject *);
int astIsAObject_(const AstObject *);
void astBegin_(void);
void astEnd_(void);
AstObject *astI2P_(int);
AstObject *astMakeId_(AstObject *);
AstObject *astMakePointer_(AstObject *);
int astP2I_(AstObject *);
int astVersion_(void);
int astEscapes_(int);
int astTune_(const char *, int);
AstObject *astDeleteId_(AstObject *);
void astExportId_(AstObject *);
void astSetId_(void *, const char *, ...);
void astExemptId_(AstObject *);
AstObject *astAnnulId_(AstObject *);
AstObject *astClone_(AstObject *);
AstObject *astCopy_(const AstObject *);
const char *astGetC_(AstObject *, const char *);
double astGetD_(AstObject *, const char *);
float astGetF_(AstObject *, const char *);
int astGetI_(AstObject *, const char *);
int astTest_(AstObject *, const char *);
long astGetL_(AstObject *, const char *);
void astClear_(AstObject *, const char *);
void astSetC_(AstObject *, const char *, const char *);
void astSetD_(AstObject *, const char *, double);
void astSetF_(AstObject *, const char *, float);
void astSetI_(AstObject *, const char *, int);
void astSetL_(AstObject *, const char *, long);
void astShow_(AstObject *);
typedef struct AstPointSet {
    AstObject object;
    double **ptr;
    double *values;
    int ncoord;
    int npoint;
    double *acc;
} AstPointSet;
AstPointSet *astCheckPointSet_(AstPointSet *);
int astIsAPointSet_(const AstPointSet *);
AstPointSet *astPointSetId_(int, int, const char *, ...);
double **astGetPoints_(AstPointSet *);
void astPermPoints_(AstPointSet *, int, const int[]);
void astSetPoints_(AstPointSet *, double **);
void astSetNpoint_(AstPointSet *, int);
void astSetSubPoints_(AstPointSet *, int, int, AstPointSet *);
AstPointSet *astAppendPoints_(AstPointSet *, AstPointSet *);
typedef struct AstMapping {
    AstObject object;
    char invert;
    char issimple;
    int nin;
    int nout;
    char report;
    char tran_forward;
    char tran_inverse;
} AstMapping;
AstMapping *astCheckMapping_(AstMapping *);
int astIsAMapping_(const AstMapping *);
int astResampleLD_(AstMapping *, int, const int[], const int[],
		   const long double[], const long double[], int,
		   void (*)(void), const double[], int, double, int,
		   long double, int, const int[], const int[], const int[],
		   const int[], long double[], long double[]);
void astRebinLD_(AstMapping *, double, int, const int[], const int[],
		 const long double[], const long double[], int,
		 const double[], int, double, int, long double, int,
		 const int[], const int[], const int[], const int[],
		 long double[], long double[]);
void astRebinSeqLD_(AstMapping *, double, int, const int[], const int[],
		    const long double[], const long double[], int,
		    const double[], int, double, int, long double, int,
		    const int[], const int[], const int[], const int[],
		    long double[], long double[], double[], int *);
AstMapping *astSimplify_(AstMapping *);
void astRebinD_(AstMapping *, double, int, const int[], const int[],
		const double[], const double[], int, const double[], int,
		double, int, double, int, const int[], const int[],
		const int[], const int[], double[], double[]);
void astRebinF_(AstMapping *, double, int, const int[], const int[],
		const float[], const float[], int, const double[], int,
		double, int, float, int, const int[], const int[],
		const int[], const int[], float[], float[]);
void astRebinI_(AstMapping *, double, int, const int[], const int[],
		const int[], const int[], int, const double[], int, double,
		int, int, int, const int[], const int[], const int[],
		const int[], int[], int[]);
void astRebinSeqD_(AstMapping *, double, int, const int[], const int[],
		   const double[], const double[], int, const double[],
		   int, double, int, double, int, const int[], const int[],
		   const int[], const int[], double[], double[], double[],
		   int *);
void astRebinSeqF_(AstMapping *, double, int, const int[], const int[],
		   const float[], const float[], int, const double[], int,
		   double, int, float, int, const int[], const int[],
		   const int[], const int[], float[], float[], double[],
		   int *);
void astRebinSeqI_(AstMapping *, double, int, const int[], const int[],
		   const int[], const int[], int, const double[], int,
		   double, int, int, int, const int[], const int[],
		   const int[], const int[], int[], int[], double[],
		   int *);
int astResampleB_(AstMapping *, int, const int[], const int[],
		  const signed char[], const signed char[], int,
		  void (*)(void), const double[], int, double, int,
		  signed char, int, const int[], const int[], const int[],
		  const int[], signed char[], signed char[]);
int astResampleD_(AstMapping *, int, const int[], const int[],
		  const double[], const double[], int, void (*)(void),
		  const double[], int, double, int, double, int,
		  const int[], const int[], const int[], const int[],
		  double[], double[]);
int astResampleF_(AstMapping *, int, const int[], const int[],
		  const float[], const float[], int, void (*)(void),
		  const double[], int, double, int, float, int,
		  const int[], const int[], const int[], const int[],
		  float[], float[]);
int astResampleI_(AstMapping *, int, const int[], const int[], const int[],
		  const int[], int, void (*)(void), const double[], int,
		  double, int, int, int, const int[], const int[],
		  const int[], const int[], int[], int[]);
int astResampleL_(AstMapping *, int, const int[], const int[],
		  const long int[], const long int[], int, void (*)(void),
		  const double[], int, double, int, long int, int,
		  const int[], const int[], const int[], const int[],
		  long int[], long int[]);
int astResampleS_(AstMapping *, int, const int[], const int[],
		  const short int[], const short int[], int,
		  void (*)(void), const double[], int, double, int,
		  short int, int, const int[], const int[], const int[],
		  const int[], short int[], short int[]);
int astResampleUB_(AstMapping *, int, const int[], const int[],
		   const unsigned char[], const unsigned char[], int,
		   void (*)(void), const double[], int, double, int,
		   unsigned char, int, const int[], const int[],
		   const int[], const int[], unsigned char[],
		   unsigned char[]);
int astResampleUI_(AstMapping *, int, const int[], const int[],
		   const unsigned int[], const unsigned int[], int,
		   void (*)(void), const double[], int, double, int,
		   unsigned int, int, const int[], const int[],
		   const int[], const int[], unsigned int[],
		   unsigned int[]);
int astResampleUL_(AstMapping *, int, const int[], const int[],
		   const unsigned long int[], const unsigned long int[],
		   int, void (*)(void), const double[], int, double, int,
		   unsigned long int, int, const int[], const int[],
		   const int[], const int[], unsigned long int[],
		   unsigned long int[]);
int astResampleUS_(AstMapping *, int, const int[], const int[],
		   const unsigned short int[], const unsigned short int[],
		   int, void (*)(void), const double[], int, double, int,
		   unsigned short int, int, const int[], const int[],
		   const int[], const int[], unsigned short int[],
		   unsigned short int[]);
void astInvert_(AstMapping *);
int astLinearApprox_(AstMapping *, const double *, const double *, double,
		     double *);
void astTran1_(AstMapping *, int, const double[], int, double[]);
void astTran2_(AstMapping *, int, const double[], const double[], int,
	       double[], double[]);
void astTranGrid_(AstMapping *, int, const int[], const int[], double, int,
		  int, int, int, double *);
void astTranN_(AstMapping *, int, int, int, const double *, int, int, int,
	       double *);
void astTranP_(AstMapping *, int, int, const double *[], int, int,
	       double *[]);
void astDecomposeId_(AstMapping *, AstMapping **, AstMapping **, int *,
		     int *, int *);
void astMapBoxId_(AstMapping *, const double[], const double[], int, int,
		  double *, double *, double[], double[]);
double astRateId_(AstMapping *, double *, int, int);
void astMapSplitId_(AstMapping *, int, int *, int *, AstMapping **);
/* ast_err. */
/* ======== */
enum { AST__ATGER = 233933154 };
enum { AST__ATSER = 233933162 };
enum { AST__ATTIN = 233933170 };
enum { AST__AXIIN = 233933178 };
enum { AST__BADAT = 233933186 };
enum { AST__BADBX = 233933194 };
enum { AST__BADIN = 233933202 };
enum { AST__BADNI = 233933210 };
enum { AST__BADNO = 233933218 };
enum { AST__BADPW = 233933226 };
enum { AST__BADSM = 233933234 };
enum { AST__BADWM = 233933242 };
enum { AST__BDBRK = 233933250 };
enum { AST__BDFMT = 233933258 };
enum { AST__BDFTS = 233933266 };
enum { AST__BDOBJ = 233933274 };
enum { AST__CLPAX = 233933282 };
enum { AST__CORNG = 233933290 };
enum { AST__CVBRK = 233933298 };
enum { AST__DIMIN = 233933306 };
enum { AST__DTERR = 233933314 };
enum { AST__ENDIN = 233933322 };
enum { AST__EOCHN = 233933330 };
enum { AST__EXPIN = 233933338 };
enum { AST__FCRPT = 233933346 };
enum { AST__FMTER = 233933354 };
enum { AST__FRMIN = 233933362 };
enum { AST__FRSIN = 233933370 };
enum { AST__FTCNV = 233933378 };
enum { AST__GRFER = 233933386 };
enum { AST__INHAN = 233933394 };
enum { AST__INNCO = 233933402 };
enum { AST__INTER = 233933410 };
enum { AST__INTRD = 233933418 };
enum { AST__KYCIR = 233933426 };
enum { AST__LDERR = 233933434 };
enum { AST__LUTII = 233933442 };
enum { AST__LUTIN = 233933450 };
enum { AST__MEMIN = 233933458 };
enum { AST__MTR23 = 233933466 };
enum { AST__MTRAX = 233933474 };
enum { AST__MTRML = 233933482 };
enum { AST__MTRMT = 233933490 };
enum { AST__NAXIN = 233933498 };
enum { AST__NCHIN = 233933506 };
enum { AST__NCOIN = 233933514 };
enum { AST__NCPIN = 233933522 };
enum { AST__NELIN = 233933530 };
enum { AST__NOCTS = 233933538 };
enum { AST__NODEF = 233933546 };
enum { AST__NOFTS = 233933554 };
enum { AST__NOMEM = 233933562 };
enum { AST__NOPTS = 233933570 };
enum { AST__NOWRT = 233933578 };
enum { AST__NPTIN = 233933586 };
enum { AST__OBJIN = 233933594 };
enum { AST__OPT = 233933602 };
enum { AST__PDSIN = 233933610 };
enum { AST__PLFMT = 233933618 };
enum { AST__PRMIN = 233933626 };
enum { AST__PTRIN = 233933634 };
enum { AST__PTRNG = 233933642 };
enum { AST__RDERR = 233933650 };
enum { AST__REGIN = 233933658 };
enum { AST__REMIN = 233933666 };
enum { AST__SCSIN = 233933674 };
enum { AST__SELIN = 233933682 };
enum { AST__SLAIN = 233933690 };
enum { AST__TRNND = 233933698 };
enum { AST__UNMQT = 233933706 };
enum { AST__VSMAL = 233933714 };
enum { AST__WCSAX = 233933722 };
enum { AST__WCSNC = 233933730 };
enum { AST__WCSPA = 233933738 };
enum { AST__WCSTY = 233933746 };
enum { AST__XSOBJ = 233933754 };
enum { AST__ZOOMI = 233933762 };
enum { AST__BADCI = 233933770 };
enum { AST__ILOST = 233933778 };
enum { AST__ITFER = 233933786 };
enum { AST__ITFNI = 233933794 };
enum { AST__MBBNF = 233933802 };
enum { AST__MRITF = 233933810 };
enum { AST__OCLUK = 233933818 };
enum { AST__UNFER = 233933826 };
enum { AST__URITF = 233933834 };
enum { AST__GBDIN = 233933842 };
enum { AST__NGDIN = 233933850 };
enum { AST__PATIN = 233933858 };
enum { AST__SISIN = 233933866 };
enum { AST__SSPIN = 233933874 };
enum { AST__UINER = 233933882 };
enum { AST__UK1ER = 233933890 };
enum { AST__COMIN = 233933898 };
enum { AST__CONIN = 233933906 };
enum { AST__DUVAR = 233933914 };
enum { AST__INNTF = 233933922 };
enum { AST__MIOPA = 233933930 };
enum { AST__MIOPR = 233933938 };
enum { AST__MISVN = 233933946 };
enum { AST__MLPAR = 233933954 };
enum { AST__MRPAR = 233933962 };
enum { AST__NORHS = 233933970 };
enum { AST__UDVOF = 233933978 };
enum { AST__VARIN = 233933986 };
enum { AST__WRNFA = 233933994 };
enum { AST__BADUN = 233934002 };
enum { AST__NORSF = 233934010 };
enum { AST__NOSOR = 233934018 };
enum { AST__SPCIN = 233934026 };
enum { AST__XMLNM = 233934034 };
enum { AST__XMLCM = 233934042 };
enum { AST__XMLPT = 233934050 };
enum { AST__XMLIT = 233934058 };
enum { AST__XMLWF = 233934066 };
enum { AST__ZERAX = 233934074 };
enum { AST__BADOC = 233934082 };
enum { AST__MPGER = 233934090 };
enum { AST__MPIND = 233934098 };
enum { AST__REGCN = 233934106 };
enum { AST__NOVAL = 233934114 };
enum { AST__INCTS = 233934122 };
enum { AST__TIMIN = 233934130 };
enum { AST__STCKEY = 233934138 };
enum { AST__STCIND = 233934146 };
enum { AST__CNFLX = 233934154 };
enum { AST__TUNAM = 233934162 };
enum { AST__BDPAR = 233934170 };
enum { AST__3DFSET = 233934178 };
enum { AST__PXFRRM = 233934186 };
enum { AST__BADSUB = 233934194 };
enum { AST__BADFLG = 233934202 };
/* version. */
/* ======== */
/* object. */
/* ======= */
/* keymap. */
/* ======= */
typedef struct AstMapEntry {
    struct AstMapEntry *next;
    const char *key;
    unsigned long hash;
    int type;
    int nel;
    const char *comment;
} AstMapEntry;
typedef struct AstKeyMap {
    AstObject object;
    int sizeguess;
    AstMapEntry **table;
    int *nentry;
    int mapsize;
} AstKeyMap;
AstKeyMap *astCheckKeyMap_(AstKeyMap *);
int astIsAKeyMap_(const AstKeyMap *);
AstKeyMap *astKeyMapId_(const char *, ...);
int astMapGet0AId_(AstKeyMap *, const char *, AstObject **);
int astMapGet1AId_(AstKeyMap *, const char *, int, int *, AstObject **);
void astMapPut1AId_(AstKeyMap *, const char *, int, AstObject *[],
		    const char *);
const char *astMapKey_(AstKeyMap *, int);
int astMapGet0C_(AstKeyMap *, const char *, const char **);
int astMapGet0D_(AstKeyMap *, const char *, double *);
int astMapGet0F_(AstKeyMap *, const char *, float *);
int astMapGet0I_(AstKeyMap *, const char *, int *);
int astMapGet1C_(AstKeyMap *, const char *, int, int, int *, char *);
int astMapGet1D_(AstKeyMap *, const char *, int, int *, double *);
int astMapGet1F_(AstKeyMap *, const char *, int, int *, float *);
int astMapGet1I_(AstKeyMap *, const char *, int, int *, int *);
int astMapHasKey_(AstKeyMap *, const char *);
int astMapLength_(AstKeyMap *, const char *);
int astMapLenC_(AstKeyMap *, const char *);
int astMapSize_(AstKeyMap *);
int astMapType_(AstKeyMap *, const char *);
void astMapPut0A_(AstKeyMap *, const char *, AstObject *, const char *);
void astMapPut0C_(AstKeyMap *, const char *, const char *, const char *);
void astMapPut0D_(AstKeyMap *, const char *, double, const char *);
void astMapPut0F_(AstKeyMap *, const char *, float, const char *);
void astMapPut0I_(AstKeyMap *, const char *, int, const char *);
void astMapPut1C_(AstKeyMap *, const char *, int, const char *[],
		  const char *);
void astMapPut1D_(AstKeyMap *, const char *, int, double *, const char *);
void astMapPut1F_(AstKeyMap *, const char *, int, float *, const char *);
void astMapPut1I_(AstKeyMap *, const char *, int, int *, const char *);
void astMapRemove_(AstKeyMap *, const char *);
int astMapGet0P_(AstKeyMap *, const char *, void **);
int astMapGet1P_(AstKeyMap *, const char *, int, int *, void **);
void astMapPut0P_(AstKeyMap *, const char *, void *, const char *);
void astMapPut1P_(AstKeyMap *, const char *, int, void *[], const char *);
/* pointset. */
/* ========= */
/* axis. */
/* ===== */
typedef struct AstAxis {
    AstObject object;
    char *label;
    char *format;
    char *symbol;
    char *unit;
    int digits;
    int direction;
    double top;
    double bottom;
} AstAxis;
AstAxis *astCheckAxis_(AstAxis *);
int astIsAAxis_(const AstAxis *);
AstAxis *astAxisId_(const char *, ...);
const char *astAxisFormat_(AstAxis *, double);
int astAxisUnformat_(AstAxis *, const char *, double *);
void astAxisNorm_(AstAxis *, double *);
/* skyaxis. */
/* ======== */
typedef struct AstSkyAxis {
    AstAxis axis;
    char *skyformat;
    int as_time;
    int is_latitude;
    int centrezero;
} AstSkyAxis;
AstSkyAxis *astCheckSkyAxis_(AstSkyAxis *);
int astIsASkyAxis_(const AstSkyAxis *);
AstSkyAxis *astSkyAxisId_(const char *, ...);
/* mapping. */
/* ======== */
/* cmpmap. */
/* ======= */
typedef struct AstCmpMap {
    AstMapping mapping;
    AstMapping *map1;
    AstMapping *map2;
    char invert1;
    char invert2;
    char series;
} AstCmpMap;
AstCmpMap *astCheckCmpMap_(AstCmpMap *);
int astIsACmpMap_(const AstCmpMap *);
AstCmpMap *astCmpMapId_(void *, void *, int, const char *, ...);
/* dssmap. */
/* ======= */
typedef struct AstFitsChan {
    AstChannel channel;
    int encoding;
    int defb1950;
    int cdmatrix;
    int carlin;
    int iwc;
    int clean;
    int fitsdigits;
    char *warnings;
    void *card;
    void *head;
    AstKeyMap *keyseq;
    AstKeyMap *keywords;
    const char *(*source) (void);
    char *(*source_wrap) (const char *(*)(void));
    void (*sink) (const char *);
    void (*sink_wrap) (void (*)(const char *), const char *);
} AstFitsChan;
AstFitsChan *astCheckFitsChan_(AstFitsChan *);
int astIsAFitsChan_(const AstFitsChan *);
AstFitsChan *astFitsChanId_(const char *(*)(void), void (*)(const char *),
			    const char *, ...);
AstFitsChan *astFitsChanForId_(const char *(*)(void),
			       char *(*)(const char *(*)(void)),
			       void (*)(const char *),
			       void (*)(void (*)(const char *),
					const char *), const char *, ...);
void astPutFits_(AstFitsChan *, const char[81], int);
void astPutCards_(AstFitsChan *, const char *);
int astFindFits_(AstFitsChan *, const char *, char[81], int);
void astDelFits_(AstFitsChan *);
void astPurgeWCS_(AstFitsChan *);
void astRetainFits_(AstFitsChan *);
void astSetFitsCF_(AstFitsChan *, const char *, double *, const char *,
		   int);
void astSetFitsCI_(AstFitsChan *, const char *, int *, const char *, int);
void astSetFitsF_(AstFitsChan *, const char *, double, const char *, int);
void astSetFitsI_(AstFitsChan *, const char *, int, const char *, int);
void astSetFitsL_(AstFitsChan *, const char *, int, const char *, int);
void astSetFitsU_(AstFitsChan *, const char *, int, const char *, int);
void astSetFitsS_(AstFitsChan *, const char *, const char *, const char *,
		  int);
void astSetFitsCN_(AstFitsChan *, const char *, const char *, const char *,
		   int);
int astGetFitsCF_(AstFitsChan *, const char *, double *);
int astGetFitsCI_(AstFitsChan *, const char *, int *);
int astGetFitsF_(AstFitsChan *, const char *, double *);
int astGetFitsI_(AstFitsChan *, const char *, int *);
int astGetFitsL_(AstFitsChan *, const char *, int *);
int astGetFitsU_(AstFitsChan *, const char *, int *);
int astGetFitsS_(AstFitsChan *, const char *, char **);
int astGetFitsCN_(AstFitsChan *, const char *, char **);
typedef struct AstDssMap {
    AstMapping mapping;
    void *wcs;
} AstDssMap;
AstDssMap *astCheckDssMap_(AstDssMap *);
int astIsADssMap_(const AstDssMap *);
/* grismmap. */
/* ========= */
typedef struct AstGrismMap {
    AstMapping mapping;
    double nr;
    double nrp;
    double waver;
    double alpha;
    double g;
    double m;
    double eps;
    double theta;
    double k1;
    double k2;
    double k3;
} AstGrismMap;
AstGrismMap *astCheckGrismMap_(AstGrismMap *);
int astIsAGrismMap_(const AstGrismMap *);
AstGrismMap *astGrismMapId_(const char *, ...);
/* intramap. */
/* ========= */
typedef struct AstIntraMap {
    AstMapping mapping;
    char *intraflag;
    int ifun;
} AstIntraMap;
AstIntraMap *astCheckIntraMap_(AstIntraMap *);
int astIsAIntraMap_(const AstIntraMap *);
AstIntraMap *astIntraMapId_(const char *, int, int, const char *, ...);
void astIntraReg_(const char *, int, int,
		  void (*)(AstMapping *, int, int, const double *[], int,
			   int, double *[]), unsigned int, const char *,
		  const char *, const char *);
void astIntraRegFor_(const char *, int, int,
		     void (*)(AstMapping *, int, int, const double *[],
			      int, int, double *[]),
		     void (*)(void (*)
			      (AstMapping *, int, int, const double *[],
			       int, int, double *[]), AstMapping *, int,
			      int, const double *[], int, int, double *[]),
		     unsigned int, const char *, const char *,
		     const char *);
/* lutmap. */
/* ======= */
typedef struct AstLutMap {
    AstMapping mapping;
    double *lut;
    double start;
    double inc;
    double last_fwd_in;
    double last_fwd_out;
    double last_inv_in;
    double last_inv_out;
    int nlut;
    int lutinterp;
} AstLutMap;
AstLutMap *astCheckLutMap_(AstLutMap *);
int astIsALutMap_(const AstLutMap *);
AstLutMap *astLutMapId_(int, const double[], double, double, const char *,
			...);
/* mathmap. */
/* ======== */
typedef struct AstMathMapRandContext_ {
    long int rand1;
    long int rand2;
    long int random_int;
    long int table[(32)];
    int active;
    int seed;
    int seed_set;
} AstMathMapRandContext_;
typedef struct AstMathMap {
    AstMapping mapping;
    AstMathMapRandContext_ rcontext;
    char **fwdfun;
    char **invfun;
    double **fwdcon;
    double **invcon;
    int **fwdcode;
    int **invcode;
    int fwdstack;
    int invstack;
    int nfwd;
    int ninv;
    int simp_fi;
    int simp_if;
} AstMathMap;
AstMathMap *astCheckMathMap_(AstMathMap *);
int astIsAMathMap_(const AstMathMap *);
AstMathMap *astMathMapId_(int, int, int, const char *[], int,
			  const char *[], const char *, ...);
/* matrixmap. */
/* ========== */
typedef struct AstMatrixMap {
    AstMapping mapping;
    double *f_matrix;
    double *i_matrix;
    int form;
} AstMatrixMap;
AstMatrixMap *astCheckMatrixMap_(AstMatrixMap *);
int astIsAMatrixMap_(const AstMatrixMap *);
AstMatrixMap *astMatrixMapId_(int, int, int, const double[], const char *,
			      ...);
/* pcdmap. */
/* ======= */
typedef struct AstPcdMap {
    AstMapping mapping;
    double disco;
    double pcdcen[2];
} AstPcdMap;
AstPcdMap *astCheckPcdMap_(AstPcdMap *);
int astIsAPcdMap_(const AstPcdMap *);
AstPcdMap *astPcdMapId_(double, const double[2], const char *, ...);
/* permmap. */
/* ======== */
typedef struct AstPermMap {
    AstMapping mapping;
    int *inperm;
    int *outperm;
    double *constant;
} AstPermMap;
AstPermMap *astCheckPermMap_(AstPermMap *);
int astIsAPermMap_(const AstPermMap *);
AstPermMap *astPermMapId_(int, const int[], int, const int[],
			  const double[], const char *, ...);
/* polymap. */
/* ======== */
typedef struct AstPolyMap {
    AstMapping mapping;
    int *ncoeff_f;
    int *mxpow_f;
    int ***power_f;
    double **coeff_f;
    int *ncoeff_i;
    int *mxpow_i;
    int ***power_i;
    double **coeff_i;
} AstPolyMap;
AstPolyMap *astCheckPolyMap_(AstPolyMap *);
int astIsAPolyMap_(const AstPolyMap *);
AstPolyMap *astPolyMapId_(int, int, int, const double[], int,
			  const double[], const char *, ...);
/* ratemap. */
/* ======== */
typedef struct AstRateMap {
    AstMapping mapping;
    AstMapping *map;
    int invert;
    int iin;
    int iout;
} AstRateMap;
AstRateMap *astCheckRateMap_(AstRateMap *);
int astIsARateMap_(const AstRateMap *);
AstRateMap *astRateMapId_(void *, int, int, const char *, ...);
/* normmap. */
/* ======== */
typedef int AstSystemType;
typedef struct AstFrame {
    AstMapping mapping;
    AstAxis **axis;
    char *domain;
    char *title;
    double epoch;
    double obslat;
    double obslon;
    double dut1;
    int *perm;
    int digits;
    int match_end;
    int active_unit;
    int max_axes;
    int min_axes;
    int naxes;
    int permute;
    int preserve_axes;
    AstSystemType system;
    AstSystemType alignsystem;
    int flags;
} AstFrame;
typedef struct AstLineDef {
    AstFrame *frame;
    double length;
    double start[2];
    double end[2];
    double dir[2];
    double q[2];
} AstLineDef;
struct AstFrameSet;
typedef struct AstFrameSet {
    AstFrame parent;
    AstFrame **frame;
    AstMapping **map;
    int *invert;
    int *link;
    int *node;
    int base;
    int current;
    int nframe;
    int nnode;
} AstFrameSet;
AstFrameSet *astCheckFrameSet_(AstFrameSet *);
int astIsAFrameSet_(const AstFrameSet *);
AstFrameSet *astFrameSetId_(void *, const char *, ...);
AstFrame *astGetFrame_(AstFrameSet *, int);
AstMapping *astGetMapping_(AstFrameSet *, int, int);
void astAddFrame_(AstFrameSet *, int, AstMapping *, AstFrame *);
void astRemapFrame_(AstFrameSet *, int, AstMapping *);
void astRemoveFrame_(AstFrameSet *, int);
AstFrame *astCheckFrame_(AstFrame *);
int astIsAFrame_(const AstFrame *);
AstFrame *astFrameId_(int, const char *, ...);
AstFrameSet *astConvert_(AstFrame *, AstFrame *, const char *);
AstFrameSet *astFindFrame_(AstFrame *, AstFrame *, const char *);
double astAngle_(AstFrame *, const double[], const double[],
		 const double[]);
double astDistance_(AstFrame *, const double[], const double[]);
void astNorm_(AstFrame *, double[]);
double astAxDistance_(AstFrame *, int, double, double);
double astAxOffset_(AstFrame *, int, double, double);
double astAxAngle_(AstFrame *, const double[2], const double[2], int);
double astOffset2_(AstFrame *, const double[2], double, double, double[2]);
void astOffset_(AstFrame *, const double[], const double[], double,
		double[]);
void astResolve_(AstFrame *, const double[], const double[],
		 const double[], double[], double *, double *);
int astGetActiveUnit_(AstFrame *);
void astSetActiveUnit_(AstFrame *, int);
AstFrame *astPickAxesId_(AstFrame *, int, const int[], AstMapping **);
const char *astFormatId_(AstFrame *, int, double);
int astUnformatId_(AstFrame *, int, const char *, double *);
void astPermAxesId_(AstFrame *, const int[]);
typedef struct AstNormMap {
    AstMapping mapping;
    AstFrame *frame;
} AstNormMap;
AstNormMap *astCheckNormMap_(AstNormMap *);
int astIsANormMap_(const AstNormMap *);
AstNormMap *astNormMapId_(void *, const char *, ...);
/* shiftmap. */
/* ========= */
typedef struct AstShiftMap {
    AstMapping mapping;
    double *shift;
} AstShiftMap;
AstShiftMap *astCheckShiftMap_(AstShiftMap *);
int astIsAShiftMap_(const AstShiftMap *);
AstShiftMap *astShiftMapId_(int, const double[], const char *, ...);
/* slamap. */
/* ======= */
typedef struct AstSlaMap {
    AstMapping mapping;
    int *cvttype;
    double **cvtargs;
    double **cvtextra;
    int ncvt;
} AstSlaMap;
AstSlaMap *astCheckSlaMap_(AstSlaMap *);
int astIsASlaMap_(const AstSlaMap *);
AstSlaMap *astSlaMapId_(int, const char *, ...);
void astSlaAdd_(AstSlaMap *, const char *, const double[]);
/* specmap. */
/* ======== */
typedef struct AstSpecMap {
    AstMapping mapping;
    int *cvttype;
    double **cvtargs;
    int ncvt;
} AstSpecMap;
AstSpecMap *astCheckSpecMap_(AstSpecMap *);
int astIsASpecMap_(const AstSpecMap *);
AstSpecMap *astSpecMapId_(int, int, const char *, ...);
void astSpecAdd_(AstSpecMap *, const char *, const double[]);
/* sphmap. */
/* ======= */
typedef struct AstSphMap {
    AstMapping mapping;
    double polarlong;
    int unitradius;
} AstSphMap;
AstSphMap *astCheckSphMap_(AstSphMap *);
int astIsASphMap_(const AstSphMap *);
AstSphMap *astSphMapId_(const char *, ...);
/* timemap. */
/* ======== */
typedef struct AstTimeMap {
    AstMapping mapping;
    int *cvttype;
    double **cvtargs;
    int ncvt;
} AstTimeMap;
AstTimeMap *astCheckTimeMap_(AstTimeMap *);
int astIsATimeMap_(const AstTimeMap *);
AstTimeMap *astTimeMapId_(int, const char *, ...);
void astTimeAdd_(AstTimeMap *, const char *, const double[]);
/* selectormap. */
/* ============ */
typedef struct AstRegion {
    AstFrame parent;
    AstFrameSet *frameset;
    AstPointSet *points;
    struct AstRegion *unc;
    double fillfactor;
    int regionfs;
    int negated;
    int closed;
    int meshsize;
    int defunc;
    AstPointSet *basemesh;
    AstPointSet *basegrid;
    int adaptive;
    int nomap;
} AstRegion;
AstRegion *astCheckRegion_(AstRegion *);
int astIsARegion_(const AstRegion *);
AstFrame *astGetRegionFrame_(AstRegion *);
int astOverlap_(AstRegion *, AstRegion *);
void astNegate_(AstRegion *);
int astMaskLD_(AstRegion *, AstMapping *, int, int, const int[],
	       const int[], long double[], long double);
int astMaskB_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], signed char[], signed char);
int astMaskD_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], double[], double);
int astMaskF_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], float[], float);
int astMaskI_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], int[], int);
int astMaskL_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], long int[], long int);
int astMaskS_(AstRegion *, AstMapping *, int, int, const int[],
	      const int[], short int[], short int);
int astMaskUB_(AstRegion *, AstMapping *, int, int, const int[],
	       const int[], unsigned char[], unsigned char);
int astMaskUI_(AstRegion *, AstMapping *, int, int, const int[],
	       const int[], unsigned int[], unsigned int);
int astMaskUL_(AstRegion *, AstMapping *, int, int, const int[],
	       const int[], unsigned long int[], unsigned long int);
int astMaskUS_(AstRegion *, AstMapping *, int, int, const int[],
	       const int[], unsigned short int[], unsigned short int);
void astSetUnc_(AstRegion *, AstRegion *);
AstRegion *astGetUnc_(AstRegion *, int);
void astGetRegionBounds_(AstRegion *, double *, double *);
void astShowMesh_(AstRegion *, int, const char *);
AstRegion *astMapRegionId_(AstRegion *, AstMapping *, AstFrame *);
typedef struct AstSelectorMap {
    AstMapping mapping;
    int nreg;
    AstRegion **reg;
    double badval;
} AstSelectorMap;
AstSelectorMap *astCheckSelectorMap_(AstSelectorMap *);
int astIsASelectorMap_(const AstSelectorMap *);
AstSelectorMap *astSelectorMapId_(int, void **, double, const char *, ...);
/* switchmap. */
/* ========== */
typedef struct AstSwitchMap {
    AstMapping mapping;
    AstMapping *fsmap;
    AstMapping *ismap;
    int fsinv;
    int isinv;
    int nroute;
    AstMapping **routemap;
    int *routeinv;
} AstSwitchMap;
AstSwitchMap *astCheckSwitchMap_(AstSwitchMap *);
int astIsASwitchMap_(const AstSwitchMap *);
AstSwitchMap *astSwitchMapId_(void *, void *, int, void **, const char *,
			      ...);
/* tranmap. */
/* ======== */
typedef struct AstTranMap {
    AstMapping mapping;
    AstMapping *map1;
    AstMapping *map2;
    int invert1;
    int invert2;
} AstTranMap;
AstTranMap *astCheckTranMap_(AstTranMap *);
int astIsATranMap_(const AstTranMap *);
AstTranMap *astTranMapId_(void *, void *, const char *, ...);
/* unitmap. */
/* ======== */
typedef struct AstUnitMap {
    AstMapping mapping;
} AstUnitMap;
AstUnitMap *astCheckUnitMap_(AstUnitMap *);
int astIsAUnitMap_(const AstUnitMap *);
AstUnitMap *astUnitMapId_(int, const char *, ...);
/* wcsmap. */
/* ======= */
typedef struct AstWcsMap {
    AstMapping mapping;
    int type;
    int wcsaxis[2];
    double **p;
    int *np;
    struct AstPrjPrm params;
} AstWcsMap;
AstWcsMap *astCheckWcsMap_(AstWcsMap *);
int astIsAWcsMap_(const AstWcsMap *);
AstWcsMap *astWcsMapId_(int, int, int, int, const char *, ...);
/* winmap. */
/* ======= */
typedef struct AstWinMap {
    AstMapping mapping;
    double *a;
    double *b;
} AstWinMap;
AstWinMap *astCheckWinMap_(AstWinMap *);
int astIsAWinMap_(const AstWinMap *);
AstWinMap *astWinMapId_(int, const double[], const double[],
			const double[], const double[], const char *, ...);
/* zoommap. */
/* ======== */
typedef struct AstZoomMap {
    AstMapping mapping;
    double zoom;
} AstZoomMap;
AstZoomMap *astCheckZoomMap_(AstZoomMap *);
int astIsAZoomMap_(const AstZoomMap *);
AstZoomMap *astZoomMapId_(int, double, const char *, ...);
/* frame. */
/* ====== */
/* cmpframe. */
/* ========= */
typedef struct AstCmpFrame {
    AstFrame frame;
    AstFrame *frame1;
    AstFrame *frame2;
    int *perm;
} AstCmpFrame;
AstCmpFrame *astCheckCmpFrame_(AstCmpFrame *);
int astIsACmpFrame_(const AstCmpFrame *);
AstCmpFrame *astCmpFrameId_(void *, void *, const char *, ...);
/* specfluxframe. */
/* ============== */
typedef struct AstSpecFluxFrame {
    AstCmpFrame cmpframe;
} AstSpecFluxFrame;
AstSpecFluxFrame *astCheckSpecFluxFrame_(AstSpecFluxFrame *);
int astIsASpecFluxFrame_(const AstSpecFluxFrame *);
AstSpecFluxFrame *astSpecFluxFrameId_(void *, void *, const char *, ...);
/* fluxframe. */
/* ========== */
typedef struct AstSkyFrame {
    AstFrame frame;
    char *projection;
    double equinox;
    int neglon;
    int alignoffset;
    int skyrefis;
    double skyref[2];
    double skyrefp[2];
    double last;
    double eplast;
    double diurab;
} AstSkyFrame;
AstSkyFrame *astCheckSkyFrame_(AstSkyFrame *);
int astIsASkyFrame_(const AstSkyFrame *);
AstSkyFrame *astSkyFrameId_(const char *, ...);
typedef int AstStdOfRestType;
typedef struct AstSpecFrame {
    AstFrame frame;
    AstStdOfRestType alignstdofrest;
    AstStdOfRestType stdofrest;
    double refdec;
    double refra;
    double restfreq;
    double sourcevel;
    AstStdOfRestType sourcevrf;
    AstSystemType sourcesys;
    int nuunits;
    char **usedunits;
    double specorigin;
    int alignspecoffset;
} AstSpecFrame;
AstSpecFrame *astCheckSpecFrame_(AstSpecFrame *);
int astIsASpecFrame_(const AstSpecFrame *);
AstSpecFrame *astSpecFrameId_(const char *, ...);
void astGetRefPos_(AstSpecFrame *, AstSkyFrame *, double *, double *);
void astSetRefPos_(AstSpecFrame *, AstSkyFrame *, double, double);
typedef struct AstFluxFrame {
    AstFrame frame;
    double specval;
    double defspecval;
    AstSpecFrame *specframe;
    int nuunits;
    char **usedunits;
} AstFluxFrame;
AstFluxFrame *astCheckFluxFrame_(AstFluxFrame *);
int astIsAFluxFrame_(const AstFluxFrame *);
AstFluxFrame *astFluxFrameId_(double, void *, const char *, ...);
/* frameset. */
/* ========= */
/* plot. */
/* ===== */
struct AstPlot;
typedef void (*AstGrfFun) (void);
typedef int (*AstGAttrFun) (AstKeyMap *, int, double, double *, int);
typedef int (*AstGFlushFun) (AstKeyMap *);
typedef int (*AstGLineFun) (AstKeyMap *, int, const float *,
			    const float *);
typedef int (*AstGMarkFun) (AstKeyMap *, int, const float *, const float *,
			    int);
typedef int (*AstGTextFun) (AstKeyMap *, const char *, float, float,
			    const char *, float, float);
typedef int (*AstGCapFun) (AstKeyMap *, int, int);
typedef int (*AstGTxExtFun) (AstKeyMap *, const char *, float, float,
			     const char *, float, float, float *, float *);
typedef int (*AstGScalesFun) (AstKeyMap *, float *, float *);
typedef int (*AstGQchFun) (AstKeyMap *, float *, float *);
typedef void (*AstGrfWrap) (void);
typedef int (*AstGAttrWrap) (struct AstPlot *, int, double, double *, int);
typedef int (*AstGFlushWrap) (struct AstPlot *);
typedef int (*AstGLineWrap) (struct AstPlot *, int, const float *,
			     const float *);
typedef int (*AstGMarkWrap) (struct AstPlot *, int, const float *,
			     const float *, int);
typedef int (*AstGTextWrap) (struct AstPlot *, const char *, float, float,
			     const char *, float, float);
typedef int (*AstGCapWrap) (struct AstPlot *, int, int);
typedef int (*AstGTxExtWrap) (struct AstPlot *, const char *, float, float,
			      const char *, float, float, float *,
			      float *);
typedef int (*AstGScalesWrap) (struct AstPlot *, float *, float *);
typedef int (*AstGQchWrap) (struct AstPlot *, float *, float *);
typedef struct AstGrfPtrs {
    AstGrfFun grffun[9];
    AstGAttrWrap GAttr;
    AstGFlushWrap GFlush;
    AstGLineWrap GLine;
    AstGMarkWrap GMark;
    AstGTextWrap GText;
    AstGCapWrap GCap;
    AstGTxExtWrap GTxExt;
    AstGScalesWrap GScales;
    AstGQchWrap GQch;
} AstGrfPtrs;
typedef struct AstGat {
    float rise;
    double size;
    double width;
    double col;
    double font;
    double style;
} AstGat;
typedef struct AstPlot {
    AstFrameSet parent;
    double *clip_lbnd;
    double *clip_ubnd;
    double centre[3];
    double gap[3];
    double loggap[3];
    double labelat[3];
    double majticklen[3];
    double minticklen[3];
    double numlabgap[3];
    double size[20];
    double textlabgap[3];
    double titlegap;
    double tol;
    double ucentre[3];
    double ugap[3];
    double uloggap[3];
    double ulblat[3];
    double umjtkln[3];
    double width[20];
    double *majtickgx[3];
    double *majtickgy[3];
    double *mintickgx[3];
    double *mintickgy[3];
    int majtickcount[3];
    int mintickcount[3];
    int nmajtickval[3];
    double *majtickval[3];
    int nmintickval[3];
    double *mintickval[3];
    double xhi;
    double xlo;
    double yhi;
    double ylo;
    double bbox[4];
    int border;
    int clip_axes;
    int clip_frame;
    int clip;
    int clipop;
    int colour[20];
    int drawaxes[3];
    int abbrev[3];
    int escape;
    int drawtitle;
    int edge[3];
    int font[20];
    int grf;
    int grid;
    int invisible;
    int labelling;
    int labelunits[3];
    int labelup[3];
    int mintick[3];
    int numlab[3];
    int style[20];
    int textlab[3];
    int tickall;
    int forceexterior;
    int uborder;
    int uedge[3];
    int ugrid;
    int ulbling;
    int ulbunit[3];
    int ulgtk[3];
    int ulglb[3];
    int umintk[3];
    int utxtlb[3];
    int xrev;
    int yrev;
    int ink;
    int logplot[3];
    int logticks[3];
    int loglabel[3];
    AstGrfFun grffun[9];
    AstGAttrWrap GAttr;
    AstGFlushWrap GFlush;
    AstGLineWrap GLine;
    AstGMarkWrap GMark;
    AstGTextWrap GText;
    AstGCapWrap GCap;
    AstGTxExtWrap GTxExt;
    AstGScalesWrap GScales;
    AstGQchWrap GQch;
    AstGrfPtrs *grfstack;
    int grfnstack;
    AstGat **gat;
    int ngat;
    AstKeyMap *grfcontext;
    AstKeyMap *grfcontextID;
    float hmarkx;
    float hmarky;
} AstPlot;
AstPlot *astCheckPlot_(AstPlot *);
int astIsAPlot_(const AstPlot *);
AstPlot *astPlotId_(void *, const float[], const double[], const char *,
		    ...);
AstKeyMap *astGrfConID_(AstPlot *);
const char *astStripEscapes_(const char *);
int astFindEscape_(const char *, int *, int *, int *);
int astBorder_(AstPlot *);
AstKeyMap *astGetGrfContext_(AstPlot *);
void astBoundingBox_(AstPlot *, float[2], float[2]);
void astClip_(AstPlot *, int, const double[], const double[]);
void astGridLine_(AstPlot *, int, const double[], double);
void astCurve_(AstPlot *, const double[], const double[]);
void astGrid_(AstPlot *);
void astMark_(AstPlot *, int, int, int, const double *, int);
void astGrfSet_(AstPlot *, const char *, AstGrfFun);
void astGrfPush_(AstPlot *);
void astGrfPop_(AstPlot *);
void astGenCurve_(AstPlot *, AstMapping *);
void astPolyCurve_(AstPlot *, int, int, int, const double *);
void astText_(AstPlot *, const char *, const double[], const float[],
	      const char *);
void astGrfWrapper_(AstPlot *, const char *, AstGrfWrap);
int astGrfFunID_(const char *, const char *, const char *);
/* plot3d. */
/* ======= */
typedef struct AstPlot3D {
    AstPlot plot;
    AstPlot *plotxy;
    AstPlot *plotxz;
    AstPlot *plotyz;
    double gbox[6];
    int pix_frame;
    int rootcorner;
    int baseplot;
    int axis_plot1[3];
    int axis_index1[3];
    int axis_plot2[3];
    int axis_index2[3];
    double norm[3];
} AstPlot3D;
AstPlot3D *astCheckPlot3D_(AstPlot3D *);
int astIsAPlot3D_(const AstPlot3D *);
AstPlot3D *astPlot3DId_(void *, const float[], const double[],
			const char *, ...);
/* skyframe. */
/* ========= */
/* specframe. */
/* ========== */
/* dsbspecframe. */
/* ============= */
typedef struct AstDSBSpecFrame {
    AstSpecFrame specframe;
    double dsbcentre;
    double ifr;
    int sideband;
    int alignsideband;
} AstDSBSpecFrame;
AstDSBSpecFrame *astCheckDSBSpecFrame_(AstDSBSpecFrame *);
int astIsADSBSpecFrame_(const AstDSBSpecFrame *);
AstDSBSpecFrame *astDSBSpecFrameId_(const char *, ...);
/* region. */
/* ======= */
/* box. */
/* ==== */
typedef struct AstBox {
    AstRegion region;
    double *extent;
    double *shextent;
    double *centre;
    double shrink;
    double *lo;
    double *hi;
    int stale;
} AstBox;
AstBox *astCheckBox_(AstBox *);
int astIsABox_(const AstBox *);
AstBox *astBoxId_(void *, int, const double[], const double[], AstRegion *,
		  const char *, ...);
/* circle. */
/* ======= */
typedef struct AstCircle {
    AstRegion region;
    double *centre;
    double radius;
    int stale;
} AstCircle;
AstCircle *astCheckCircle_(AstCircle *);
int astIsACircle_(const AstCircle *);
AstCircle *astCircleId_(void *, int, const double[], const double[],
			AstRegion *, const char *, ...);
/* cmpregion. */
/* ========== */
typedef struct AstCmpRegion {
    AstRegion region;
    AstRegion *region1;
    AstRegion *region2;
    int oper;
} AstCmpRegion;
AstCmpRegion *astCheckCmpRegion_(AstCmpRegion *);
int astIsACmpRegion_(const AstCmpRegion *);
AstCmpRegion *astCmpRegionId_(void *, void *, int, const char *, ...);
/* ellipse. */
/* ======== */
typedef struct AstEllipse {
    AstRegion region;
    double *centre;
    double *point1;
    double angle;
    double a;
    double b;
    double lbx;
    double ubx;
    double lby;
    double uby;
    int stale;
} AstEllipse;
AstEllipse *astCheckEllipse_(AstEllipse *);
int astIsAEllipse_(const AstEllipse *);
AstEllipse *astEllipseId_(void *, int, const double[2], const double[2],
			  const double[2], AstRegion *, const char *, ...);
/* interval. */
/* ========= */
typedef struct AstInterval {
    AstRegion region;
    double *lbnd;
    double *ubnd;
    AstBox *box;
    int stale;
} AstInterval;
AstInterval *astCheckInterval_(AstInterval *);
int astIsAInterval_(const AstInterval *);
AstInterval *astIntervalId_(void *, const double[], const double[],
			    AstRegion *, const char *, ...);
/* nullregion. */
/* =========== */
typedef struct AstNullRegion {
    AstRegion region;
} AstNullRegion;
AstNullRegion *astCheckNullRegion_(AstNullRegion *);
int astIsANullRegion_(const AstNullRegion *);
AstNullRegion *astNullRegionId_(void *, AstRegion *, const char *, ...);
/* pointlist. */
/* ========== */
typedef struct AstPointList {
    AstRegion region;
    double *lbnd;
    double *ubnd;
} AstPointList;
AstPointList *astCheckPointList_(AstPointList *);
int astIsAPointList_(const AstPointList *);
AstPointList *astPointListId_(void *, int, int, int, const double *,
			      AstRegion *, const char *, ...);
/* polygon. */
/* ======== */
typedef struct AstPolygon {
    AstRegion region;
    double in[2];
    double lbnd[2];
    double ubnd[2];
    AstLineDef **edges;
    int stale;
} AstPolygon;
AstPolygon *astCheckPolygon_(AstPolygon *);
int astIsAPolygon_(const AstPolygon *);
AstPolygon *astPolygonId_(void *, int, int, const double *, AstRegion *,
			  const char *, ...);
/* prism. */
/* ====== */
typedef struct AstPrism {
    AstRegion region;
    AstRegion *region1;
    AstRegion *region2;
} AstPrism;
AstPrism *astCheckPrism_(AstPrism *);
int astIsAPrism_(const AstPrism *);
AstPrism *astPrismId_(void *, void *, const char *, ...);
/* stc. */
/* ==== */
typedef struct AstStc {
    AstRegion parent_region;
    AstRegion *region;
    AstKeyMap **coord;
    int ncoord;
} AstStc;
AstStc *astCheckStc_(AstStc *);
int astIsAStc_(const AstStc *);
AstRegion *astGetStcRegion_(AstStc *);
AstKeyMap *astGetStcCoord_(AstStc *, int);
int astGetStcNCoord_(AstStc *);
/* stcresourceprofile. */
/* =================== */
typedef struct AstStcResourceProfile {
    AstStc stc;
} AstStcResourceProfile;
AstStcResourceProfile *astCheckStcResourceProfile_(AstStcResourceProfile
						   *);
int astIsAStcResourceProfile_(const AstStcResourceProfile *);
AstStcResourceProfile *astStcResourceProfileId_(void *, int, AstKeyMap **,
						const char *, ...);
/* stcsearchlocation. */
/* ================== */
typedef struct AstStcSearchLocation {
    AstStc stc;
} AstStcSearchLocation;
AstStcSearchLocation *astCheckStcSearchLocation_(AstStcSearchLocation *);
int astIsAStcSearchLocation_(const AstStcSearchLocation *);
AstStcSearchLocation *astStcSearchLocationId_(void *, int, AstKeyMap **,
					      const char *, ...);
/* stccatalogentrylocation. */
/* ======================== */
typedef struct AstStcCatalogEntryLocation {
    AstStc stc;
} AstStcCatalogEntryLocation;
AstStcCatalogEntryLocation
    *astCheckStcCatalogEntryLocation_(AstStcCatalogEntryLocation *);
int astIsAStcCatalogEntryLocation_(const AstStcCatalogEntryLocation *);
AstStcCatalogEntryLocation *astStcCatalogEntryLocationId_(void *, int,
							  AstKeyMap **,
							  const char *,
							  ...);
/* stcobsdatalocation. */
/* =================== */
typedef struct AstStcObsDataLocation {
    AstStc stc;
    AstPointList *obs;
} AstStcObsDataLocation;
AstStcObsDataLocation *astCheckStcObsDataLocation_(AstStcObsDataLocation
						   *);
int astIsAStcObsDataLocation_(const AstStcObsDataLocation *);
AstStcObsDataLocation *astStcObsDataLocationId_(void *, int, AstKeyMap **,
						const char *, ...);
/* timeframe. */
/* ========== */
typedef int AstTimeScaleType;
typedef struct AstTimeFrame {
    AstFrame frame;
    double timeorigin;
    AstTimeScaleType timescale;
    AstTimeScaleType aligntimescale;
} AstTimeFrame;
AstTimeFrame *astCheckTimeFrame_(AstTimeFrame *);
int astIsATimeFrame_(const AstTimeFrame *);
AstTimeFrame *astTimeFrameId_(const char *, ...);
double astCurrentTime_(AstTimeFrame *);
/* channel. */
/* ======== */
/* fitschan. */
/* ========= */
/* xmlchan. */
/* ======== */
typedef struct AstXmlChan {
    AstChannel channel;
    const char *objectname;
    const char *objectcomment;
    int objectset;
    AstXmlParent *container;
    AstXmlDocument *readcontext;
    int write_isa;
    int xmlindent;
    int xmlstrict;
    int xmllength;
    int xmlformat;
    int formatdef;
    char *xmlprefix;
    int reset_source;
    const char *isa_class;
    AstKeyMap *warnings;
} AstXmlChan;
AstXmlChan *astCheckXmlChan_(AstXmlChan *);
int astIsAXmlChan_(const AstXmlChan *);
AstXmlChan *astXmlChanId_(const char *(*)(void), void (*)(const char *),
			  const char *, ...);
AstXmlChan *astXmlChanForId_(const char *(*)(void),
			     char *(*)(const char *(*)(void)),
			     void (*)(const char *),
			     void (*)(void (*)(const char *),
				      const char *), const char *, ...);
AstKeyMap *astXmlWarnings_(AstXmlChan *);
/*
*+
*  Name:
*     prm_par.h

*  Type of Module:
*     C include file.

*  Purpose:
*     Define public constants for the PRM system.

*  Description:
*     This file defines machine-dependent public constants for the
*     PRM system (used to be called PRIMDAT).

*  Machine-specific features used:
*     (platform x86_64-unknown-linux-gnu, Fortran compiler g95)
*     The machine DOES appear to have IEEE floats.
*     The machine is LITTLE-endian
*     The CPU DOES NOT throw SIGFPE on denormalized numbers.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     ./make-prm-par program

*  History:
*     9-AUG-1988 (RFWS):
*        Original version.
*     25-OCT-1991 (RFWS):
*        Adapted for SUN4 systems from the original VMS file.
*     03-Mar-2008 (./make-prm-par):
*        Generated
*     No further changes -- do not edit this file

*  Copyright:
*     Copyright 1988, 1991, 1992, 1995, 2004, 2005, Council for the Central Laboratory of the Research Councils

*-
*/
/* Bad values, used for flagging undefined data. */
/* Machine precision. */
/* Maximum (most positive) non-bad value. */
/* Maximum (most positive) number. */
/* Minimum (most negative) non-bad value. */
/* Minimum (most negative) number. */
/* Number of basic machine units (bytes) used by a value. */
/* Smallest positive value. */
/* Number of characters required to format value as decimal string. */
/*
*+
*  Name:
*     mers.h

*  Purpose:
*     Public header for using the MERS library

*  Description:
*     This include file includes all MERS related include files.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: Alan Chipperfield (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-APR-2006 (TIMJ):
*        Add prolog.

*-
*/
/*
*+
*  Name:
*     merswrap.h

*  Purpose:
*     Prototypes for public interface.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: Alan Chipperfield (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-APR-2006 (TIMJ):
*        Add prolog.

*-
*/
void errAnnul(int *status);
void errBegin(int *status);
void errClear(int *status);
void errEnd(int *status);
void errFacer(const char *token, int *status);
void errFioer(const char *token, int iostat);
void errFlbel(int *status);
void errFlush(int *status);
void errLevel(int *level);
void errLoad(char *param,
	     int param_length,
	     int *parlen,
	     char *opstr, int opstr_length, int *oplen, int *status);
void errMark(void);
void errOut(const char *param, const char *text, int *status);
void errRep(const char *param, const char *text, int *status);
void errRlse(void);
void errStart(void);
void errStat(int *status);
void errStop(int *status);
void errSyser(const char *token, int systat);
void errTune(const char *param, int value, int *status);
void msgBell(int *status);
void msgBlank(int *status);
void msgFmtc(const char *token, const char *format, const char *cvalue);
void msgFmtd(const char *token, const char *format, double dvalue);
void msgFmti(const char *token, const char *format, int ivalue);
void msgFmtl(const char *token, const char *format, int lvalue);
void msgFmtr(const char *token, const char *format, float rvalue);
void msgIfget(const char *pname, int *status);
void msgIflev(int *filter);
void msgIfset(int filter, int *status);
void msgLoad(const char *param,
	     const char *text,
	     char *opstr, int opstr_length, int *oplen, int *status);
void msgOut(const char *param, const char *text, int *status);
void msgOutif(int prior, const char *param, const char *text, int *status);
void msgRenew(void);
void msgSetc(const char *token, const char *cvalue);
void msgSetd(const char *token, double dvalue);
void msgSeti(const char *token, int ivalue);
void msgSetl(const char *token, int lvalue);
void msgSetr(const char *token, float rvalue);
void msgSync(int *status);
void msgTune(const char *param, int value, int *status);
/*
*+

*  Name:
*     ERR_PAR

*  Purpose:
*     Define the ERR_ public constants.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Global constants include file.

*  Description:
*     This file contains definitions of the public global constants used
*     by the ERR_ system.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran include file err_par by the Perl script fchead.
*     {enter_new_authors_here}

*  History:
*     22-Sep-1998 (fhead):
*        Original version
*     {enter_changes_here}

*-
*/
/*  Global Constants: */
/*   Maximum length of error message text */
/*   Maximum length of error message name */
/*. */
/*
 * C error define file for facility ERR (102)
 * Generated by the MESSGEN utility
 */
/* Error encountered during message output. */
enum { ERR__OPTER = 140936770 };	/* messid=200 */
/* Status not set in call to ERR_REP (improper use of ERR_REP). */
enum { ERR__UNSET = 140937570 };	/* messid=300 */
/* Status set to SAI__OK in call to ERR_REP (improper use of ERR_REP). */
enum { ERR__BADOK = 140937578 };	/* messid=301 */
/* Invalid tuning parameter name (improper use of ERR_TUNE). */
enum { ERR__BDPAR = 140937586 };	/* messid=302 */
/* Bad tuning value (improper use of ERR_TUNE). */
enum { ERR__BTUNE = 140937594 };	/* messid=303 */
/* Bad ERR environment variable value. */
enum { ERR__BDENV = 140937602 };	/* messid=304 */
/*
*+

*  Name:
*     MSG_PAR

*  Purpose:
*     Define the MSG_ global constants.

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Global constants include file.

*  Description:
*     This file contains definitions of the public global constants used
*     by the MSG_ system.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran include file msg_par by the Perl script fchead.
*     {enter_new_authors_here}

*  History:
*     22-Sep-1998 (fhead):
*        Original version
*     {enter_changes_here}

*-
*/
/*  Global Constants: */
/*   Normal conditional message output level */
/*   Quiet conditional message output level */
/*   Maximum length of message text */
/*   Verbose conditional message output level */
/*. */
/*
 * C error define file for facility MSG (103)
 * Generated by the MESSGEN utility
 */
/* Error encountered during message output. */
enum { MSG__OPTER = 141002306 };	/* messid=200 */
/* Error encountered during message synchronisation. */
enum { MSG__SYNER = 141002314 };	/* messid=201 */
/* Invalid conditional message filter value. */
enum { MSG__INVIF = 141002322 };	/* messid=202 */
/* Invalid tuning parameter name (improper use of MSG_TUNE). */
enum { MSG__BDPAR = 141002330 };	/* messid=203 */
/* Bad tuning value (improper use of MSG_TUNE). */
enum { MSG__BTUNE = 141002338 };	/* messid=204 */
/* Bad MSG environment variable value. */
enum { MSG__BDENV = 141002346 };	/* messid=205 */
/* Define a macro to do the work for a given data type. */
void smf_kmmerge(const char *xname, AstKeyMap * keymap, int from, int into,
		 int ndet, int *mask, int nts, int rts_num, int *status)
{
/* Local Variables */
    const char *key = ((void *) 0);
    double *pfromD = ((void *) 0);
    double *pintoD = ((void *) 0);
    double *valuesD = ((void *) 0);
    float *pfromR = ((void *) 0);
    float *pintoR = ((void *) 0);
    float *valuesR = ((void *) 0);
    int *pfromI = ((void *) 0);
    int *pintoI = ((void *) 0);
    int *valuesI = ((void *) 0);
    int changed;
    int i;
    int idet;
    int j;
    int nentry;
    int seclen;
    int type;
    int veclen;
    int vpd;
/* Check the inherited status */
    if (*status != SAI__OK)
	return;
/* Do nothing if we are merging a time slice with itself. */
    if (from != into) {
/* Loop round every entry in the KeyMap. */
	nentry =
	    (astAt_(((void *) 0), "smf_kmmerge.c", 239, 0),
	     ((astMapSize_
	       (astCheckKeyMap_
		((AstKeyMap *) ((void *)
				astMakePointer_((AstObject
						 *) (keymap))))))));
	for (i = 0; i < nentry; i++) {
	    key =
		(astAt_(((void *) 0), "smf_kmmerge.c", 241, 0),
		 ((astMapKey_
		   (astCheckKeyMap_
		    ((AstKeyMap *) ((void *)
				    astMakePointer_((AstObject
						     *) (keymap)))), i))));
/* Get the vector length of the entry. */
	    veclen =
		(astAt_(((void *) 0), "smf_kmmerge.c", 244, 0),
		 ((astMapLength_
		   (astCheckKeyMap_
		    ((AstKeyMap *) ((void *)
				    astMakePointer_((AstObject
						     *) (keymap)))),
		    key))));
/* Set a flag indicating that the contents of the KeyMap entry have not
   been changed. */
	    changed = 0;
/* Invoke a macro to handle the data type. */
	    type =
		(astAt_(((void *) 0), "smf_kmmerge.c", 251, 0),
		 ((astMapType_
		   (astCheckKeyMap_
		    ((AstKeyMap *) ((void *)
				    astMakePointer_((AstObject
						     *) (keymap)))),
		    key))));
	    if (type == 1) {
		valuesI =
		    (astAt_(((void *) 0), "smf_kmmerge.c", 253, 0),
		     (astGrow_(valuesI, veclen, sizeof(int))));
		(void) (astAt_(((void *) 0), "smf_kmmerge.c", 253, 0),
			((astMapGet1I_
			  (astCheckKeyMap_
			   ((AstKeyMap *) ((void *)
					   astMakePointer_((AstObject
							    *) (keymap)))),
			   key, veclen, &veclen, valuesI))));
		if ((*starlink_ast_status_ptr == 0)) {
		    if (veclen == nts) {
			if (valuesI[from] != valuesI[into]) {
			    *status = SAI__ERROR;
			    msgSetc("X", xname);
			    msgSetc("N", key);
			    msgSeti("R", rts_num);
			    msgSeti("V1", valuesI[from]);
			    msgSeti("V2", valuesI[into]);
			    errRep("",
				   "Differing values (^V1 and ^V2) found for "
				   "item ^X.^N when RTS_NUM=^R.", status);
			}
		    } else if (veclen % nts == 0) {
			seclen = veclen / nts;
			if (seclen % ndet == 0) {
			    vpd = seclen / ndet;
			    pfromI = valuesI + from * seclen;
			    pintoI = valuesI + into * seclen;
			    for (idet = 0;
				 idet < ndet && *status == SAI__OK;
				 idet++) {
				if (mask[idet]) {
				    for (j = 0; j < vpd; j++) {
					if (pfromI[j] != (-2147483647 - 1)) {
					    if (pintoI[j] ==
						(-2147483647 - 1)) {
						pintoI[j] = pfromI[j];
						changed = 1;
					    } else if (pintoI[j] !=
						       pfromI[j]) {
						*status = SAI__ERROR;
						msgSetc("X", xname);
						msgSetc("N", key);
						msgSeti("R", rts_num);
						msgSeti("V1",
							pintoI[from]);
						msgSeti("V2",
							pfromI[into]);
						errRep("",
						       "Differing values (^V1 and ^V2) found for "
						       "item ^X.^N when RTS_NUM=^R.",
						       status);
						break;
					    }
					}
				    }
				}
				pfromI += vpd;
				pintoI += vpd;
			    }
			}
		    }
		}
		if (changed)
		    (astAt_(((void *) 0), "smf_kmmerge.c", 253, 0),
		     ((astMapPut1I_
		       (astCheckKeyMap_
			((AstKeyMap *) ((void *)
					astMakePointer_((AstObject
							 *) (keymap)))),
			key, veclen, valuesI, ((void *) 0)))));;
	    } else if (type == 5) {
		valuesR =
		    (astAt_(((void *) 0), "smf_kmmerge.c", 256, 0),
		     (astGrow_(valuesR, veclen, sizeof(float))));
		(void) (astAt_(((void *) 0), "smf_kmmerge.c", 256, 0),
			((astMapGet1F_
			  (astCheckKeyMap_
			   ((AstKeyMap *) ((void *)
					   astMakePointer_((AstObject
							    *) (keymap)))),
			   key, veclen, &veclen, valuesR))));
		if ((*starlink_ast_status_ptr == 0)) {
		    if (veclen == nts) {
			if (valuesR[from] != valuesR[into]) {
			    *status = SAI__ERROR;
			    msgSetc("X", xname);
			    msgSetc("N", key);
			    msgSeti("R", rts_num);
			    msgSetr("V1", valuesR[from]);
			    msgSetr("V2", valuesR[into]);
			    errRep("",
				   "Differing values (^V1 and ^V2) found for "
				   "item ^X.^N when RTS_NUM=^R.", status);
			}
		    } else if (veclen % nts == 0) {
			seclen = veclen / nts;
			if (seclen % ndet == 0) {
			    vpd = seclen / ndet;
			    pfromR = valuesR + from * seclen;
			    pintoR = valuesR + into * seclen;
			    for (idet = 0;
				 idet < ndet && *status == SAI__OK;
				 idet++) {
				if (mask[idet]) {
				    for (j = 0; j < vpd; j++) {
					if (pfromR[j] != -3.40282347e+38F) {
					    if (pintoR[j] ==
						-3.40282347e+38F) {
						pintoR[j] = pfromR[j];
						changed = 1;
					    } else if (pintoR[j] !=
						       pfromR[j]) {
						*status = SAI__ERROR;
						msgSetc("X", xname);
						msgSetc("N", key);
						msgSeti("R", rts_num);
						msgSetr("V1",
							pintoR[from]);
						msgSetr("V2",
							pfromR[into]);
						errRep("",
						       "Differing values (^V1 and ^V2) found for "
						       "item ^X.^N when RTS_NUM=^R.",
						       status);
						break;
					    }
					}
				    }
				}
				pfromR += vpd;
				pintoR += vpd;
			    }
			}
		    }
		}
		if (changed)
		    (astAt_(((void *) 0), "smf_kmmerge.c", 256, 0),
		     ((astMapPut1F_
		       (astCheckKeyMap_
			((AstKeyMap *) ((void *)
					astMakePointer_((AstObject
							 *) (keymap)))),
			key, veclen, valuesR, ((void *) 0)))));;
	    } else if (type == 2) {
		valuesD =
		    (astAt_(((void *) 0), "smf_kmmerge.c", 259, 0),
		     (astGrow_(valuesD, veclen, sizeof(double))));
		(void) (astAt_(((void *) 0), "smf_kmmerge.c", 259, 0),
			((astMapGet1D_
			  (astCheckKeyMap_
			   ((AstKeyMap *) ((void *)
					   astMakePointer_((AstObject
							    *) (keymap)))),
			   key, veclen, &veclen, valuesD))));
		if ((*starlink_ast_status_ptr == 0)) {
		    if (veclen == nts) {
			if (valuesD[from] != valuesD[into]) {
			    *status = SAI__ERROR;
			    msgSetc("X", xname);
			    msgSetc("N", key);
			    msgSeti("R", rts_num);
			    msgSetd("V1", valuesD[from]);
			    msgSetd("V2", valuesD[into]);
			    errRep("",
				   "Differing values (^V1 and ^V2) found for "
				   "item ^X.^N when RTS_NUM=^R.", status);
			}
		    } else if (veclen % nts == 0) {
			seclen = veclen / nts;
			if (seclen % ndet == 0) {
			    vpd = seclen / ndet;
			    pfromD = valuesD + from * seclen;
			    pintoD = valuesD + into * seclen;
			    for (idet = 0;
				 idet < ndet && *status == SAI__OK;
				 idet++) {
				if (mask[idet]) {
				    for (j = 0; j < vpd; j++) {
					if (pfromD[j] !=
					    -1.7976931348623157e+308) {
					    if (pintoD[j] ==
						-1.7976931348623157e+308) {
						pintoD[j] = pfromD[j];
						changed = 1;
					    } else if (pintoD[j] !=
						       pfromD[j]) {
						*status = SAI__ERROR;
						msgSetc("X", xname);
						msgSetc("N", key);
						msgSeti("R", rts_num);
						msgSetd("V1",
							pintoD[from]);
						msgSetd("V2",
							pfromD[into]);
						errRep("",
						       "Differing values (^V1 and ^V2) found for "
						       "item ^X.^N when RTS_NUM=^R.",
						       status);
						break;
					    }
					}
				    }
				}
				pfromD += vpd;
				pintoD += vpd;
			    }
			}
		    }
		}
		if (changed)
		    (astAt_(((void *) 0), "smf_kmmerge.c", 259, 0),
		     ((astMapPut1D_
		       (astCheckKeyMap_
			((AstKeyMap *) ((void *)
					astMakePointer_((AstObject
							 *) (keymap)))),
			key, veclen, valuesD, ((void *) 0)))));;
	    }
	}
/* Free resources. */
	valuesD =
	    (astAt_(((void *) 0), "smf_kmmerge.c", 265, 0),
	     (astFree_(valuesD)));
	valuesR =
	    (astAt_(((void *) 0), "smf_kmmerge.c", 266, 0),
	     (astFree_(valuesR)));
	valuesI =
	    (astAt_(((void *) 0), "smf_kmmerge.c", 267, 0),
	     (astFree_(valuesI)));
    }
}
