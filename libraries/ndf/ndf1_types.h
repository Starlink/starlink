#if !defined( NDF1_TYPES_INCLUDED )   /* Protect against multiple inclusion*/
#define NDF1_TYPES_INCLUDED 1

/*
*  Name:
*     ndf1_types.h

*  Purpose:
*     Defines private data types and constants used by NDF.

*  Description:
*     This file defines all the private data types and constants
*     used within the C version of NDF.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     19-MAR-2018 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

#include <pthread.h>
#include <limits.h>
#include "star/hds.h"
#include "ary.h"
#include "ast.h"
#include "ems_par.h"
#include "ndf_types.h"





/* --- Function macros --------------------------------------- */

/* Return max or min of two values (beware of side effects!). */
#define NDF_MIN(a,b) (((a)<(b))?(a):(b))
#define NDF_MAX(a,b) (((a)>(b))?(a):(b))

/* Return nearest integer, as an int. */
#define NDF_NINT(a) ((int)round(a))


/* Get a pointer to the thread specific data. */
#define NDF_GETTSD \
   NdfTSD *tsd = pthread_getspecific( starlink_ndf_globals_key ); \
   if( !tsd ) { \
      fprintf( stderr, "NDF library not initialised correctly (programming " \
              "error)." ); \
   }

/* Get a referecne to a specific item of thread specific data */
#define NDF_TSD( name ) (tsd->name)

/* Determine if an NDF quality value and a "bad-bits" mask dictate that
   an NDF pixel be included in processing. A non-zero value is returned if
   the bit-wise "AND" of the quality and bad-bits values is zero, indicating
   that the NDF pixel is not excluded by its quality value and should be
   included in processing. Otherwise, a zero value is returned, indicating
   that the pixel should be excluded from processing. */
#define NDF_QMASK(Qual,BadBit) (((Qual)&(BadBit))==0)


/* Invoke these macros at the start and end of each public NDF function.
   The start macro performs all initialisation of the NDF library (if
   not already done), and tells AST to watch the provided status variable.
   The end macro tells AST to watch the original status variable. */
#define NDF_INIT(status) \
   int *old_ast_status_ptr = ndf1Init( status );

#define NDF_FINAL \
   (void) astWatch( old_ast_status_ptr );






/* --- CONSTANTS --------------------------------------------- */


/* Miscellaneous.
   ============= */

/* Context level that indicates that an ACB is not in any context. */
#define NDF__INLIMBO -INT_MAX

/* Unique NDF_ system facility number. */
#define NDF__FACNO 1506

/* Identifiers for block types (use a large starting value to reduce the
   chance of uninitialised integer values being interpreted as a legal
   type). */
typedef enum NdfBlockType {
  NDF__ACBTYPE = 1000000,
  NDF__DCBTYPE,
  NDF__PCBTYPE,
  NDF__FCBTYPE
} NdfBlockType;


/* String lengths.
   ============== */

/* Maximum length of a command to be used during foreign data format
   conversion operations. */
#define NDF__SZCVT 512

/* Maximum length of a string describing a data object disposal mode,
   entry in the DCB, e.g. 'DELETE'. */
#define NDF__SZDSP 6

/* Maximum length of a foreign format file identification code. Note
   that this is potentially system-dependent, since it depends on how
   file identification information is encoded. However, using a
   sufficiently large value means that it is unlikely ever to need
   changing. */
#define NDF__SZFID 255

/* Maximum length of a string containing the name of an HDS container
   file (this value is somewhat arbitrary, since the maximum length of
   a file name is often a rather ill-defined quantity). */
#define NDF__SZFIL 255

/* Maximum length of a foreign data format name. */
#define NDF__SZFMN 50

/* Maximum length of a foreign data format file extension. */
#define NDF__SZFMX 10

/* Maximum length of a list specifying NDF extension names to be
   recognised during format conversion. */
#define NDF__SZXLS 1024

/* Maximum length of a string describing an initialisation option
   appended to an NDF array component mapping mode specification, e.g.
   'ZERO'. */
#define NDF__SZIOP 4

/* Maximum length of a string describing a data object access mode
   entry in the DCB, e.g. 'UPDATE'. */
#define NDF__SZMOD 6

/* Maximum length of a parameter system character string value. */
#define NDF__SZPAR 200

/* Maximum length of a string containing the path name of an HDS data
   object (this value is somewhat arbitrary, since the maximum depth of
   nesting of HDS objects is in principle unlimited). */
#define NDF__SZPTH 255

/* Maximum length of a string describing the state of an NDF, e.g.
   'UNKNOWN'. */
#define NDF__SZSTA 7

/* Maximum length of a string describing a sub-structure within a
   foreign format file (e.g. a FITS extension), including the enclosing
   square brackets. This includes the length of the file specification.  */
#define NDF__SZFXS ( NDF__SZFIL + 100 )

/* Maximum length of a list specifying recognised foreign data formats. */
#define NDF__SZFMT 1024

/* Maximum number of foreign data formats which can be recognised. */
#define NDF__MXFMT 50




/* Access control.
   ============== */

/* Macro to convert a bit position to an integer value */
#define NDF__BIT_TO_VAL(bit) (1<<bit)

/* Number of access control flags maintained in the ACB, which
   determines the number of types of NDF access over which control can
   be exercised. */
#define NDF__ACC_MXACC 5

/* Bit masks defining each individual access control flag. */
typedef enum NdfAccessControlFlag  {
  NDF__ACC_BOUND = NDF__BIT_TO_VAL(0), /* BOUNDS access control flag. */
  NDF__ACC_DELET = NDF__BIT_TO_VAL(1), /* DELETE access control flag. */
  NDF__ACC_SHIFT = NDF__BIT_TO_VAL(2), /* SHIFT access control flag. */
  NDF__ACC_TYPE = NDF__BIT_TO_VAL(3), /* TYPE access control flag. */
  NDF__ACC_WRITE = NDF__BIT_TO_VAL(4), /* WRITE access control flag. */
} NdfAccessControlFlag;


/* NDF components.
   =============== */

/* Minimum number of characters in a component name abbreviation. */
#define NDF__MINAB 3

/* Number of character component names. */
#define NDF__MXCCN 3

/* Identifier for LABEL component. */
#define NDF__LABEL 0

/* Identifier for TITLE component */
#define NDF__TITLE 1

/* Identifier for UNITS component. */
#define NDF__UNITS 2

/* NDF axis components.
   =================== */

/* Number of axis character components. */
#define NDF__MXACN 2

/* Identifier for axis label component. */
#define NDF__ALAB 0

/* Identifier for axis units component. */
#define NDF__AUNI 1

/* Component propagation.
   ===================== */

/* Maximum number of extension names in a component propagation list
   (and also in a list of extensions to be considered during foreign
   format conversion operations). */
#define NDF__MXEXT 32

/* Number of component propagation flags. */
#define NDF__NCPF 9

/* AXIS component propagation flag. */
#define NDF__ACPF 0

/* DATA component propagation flag. */
#define NDF__DCPF 1

/* HISTORY component propagation flag. */
#define NDF__HCPF 2

/* LABEL component propagation flag. */
#define NDF__LCPF 3

/* QUALITY component propagation flag. */
#define NDF__QCPF 4

/* TITLE component propagation flag. */
#define NDF__TCPF 5

/* UNITS component propagation flag. */
#define NDF__UCPF 6

/* VARIANCE component propagation flag. */
#define NDF__VCPF 7

/* WCS component propagation flag. */
#define NDF__WCPF 8

/* Numeric types.
   ============= */

/* Number of numeric types. */
#define NDF__NTYP 8

/* Unsigned byte type code. */
#define NDF__TYPUB 0

/* Byte type code. */
#define NDF__TYPB 1

/* Unsigned word type code. */
#define NDF__TYPUW 2

/* Word type code. */
#define NDF__TYPW 3

/* int type code. */
#define NDF__TYPI 4

/* 64-bit int type code. */
#define NDF__TYPK 5

/* Real type code. */
#define NDF__TYPR 6

/* Double precision type code. */
#define NDF__TYPD 7

/* History update mode settings.
   ============================ */

/* History updating disabled. */
#define NDF__HDISA -2

/* History updating quiet. */
#define NDF__HQUIE -1

/* History updating normal. */
#define NDF__HNORM 0

/* History updating verbose. */
#define NDF__HVERB 1

/* Fortran I/O units.
   ================= */

/* First Fortran I/O unit number to search when looking for a free
   unit. */
#define NDF__UNIT1 0

/* Last Fortran I/O unit number to search when looking for a free unit. */
#define NDF__UNIT2 255

/* Error Logging.
   ============= */

/* Maximum number of pending error messages that can be logged for
   recording in NDF history records. N.B. This number should not be
   less than the maximum number of pending messages allowed by EMS (see
   SSN/4), otherwise error messages may get lost. It should be replaced
   by an appropriate EMS__ constant when this is provided. */
#define NDF__MXERR 32

/* World Coordinate Systems.
   ======================== */

/* Length of _CHAR array elements used to store AST data in HDS. */
#define NDF__SZAST 32

/* Number of _CHAR array elements initially created when storing AST
   data in HDS (i.e. an initial guess at the number of lines of data to
   be stored). The array is expanded if this proves insufficient. It is
   always truncated after the data have been written. */
#define NDF__INAST 64

/* Minimum length of _CHAR array elements accepted as valid when
   reading AST data that has been stored in HDS. */
#define NDF__MLAST 16

/* Maximum number of continuation lines (i.e. array elements of size
   NDF__SZAST) to be used when storing a single line of AST data in
   HDS.  Continuation lines are indicated by a '+' as the first
   character (normally this is a space). AST data lines which are still
   too long to be accommodated will cause an error. */
#define NDF__MXACL 64








/* --- DATA TYPES --------------------------------------------- */



/* Block header */
/* ------------ */
/* This structure is used as the first component in each of the
   block structures defined below. */
typedef struct NdfObject {

/* Usage: This value is non-zero if the remaining values in the block
   structure are in use, and zero if it is not currently being used to
   describe an object and can thus be used to describe a new object. */
   int used;

/* Slot. The index of the block structure within the arrays maintained
   by ndf1Ffs. For historical reasons, this is referred to as the slot
   number of the structure. */
   int slot;

/* Type: The type of block structure; DCB, ACB, FCB or PCB. */
   NdfBlockType type;

/* Check: When an int identifier is issued for an object, its value is
   stored in the "check" value and an identifier is only considered valid
   if it matches the "check" value in the object to which it refers. This
   allows identifiers to be rendered invalid either by the user or by the
   NDF_  system by assigning NULL to them. */
   int check;

} NdfObject;





/* Data Control Block (DCB). */
/* ------------------------- */

/* Define a structure to contain information used internally by the NDF_
   about the state and values of external data objects. There is a one-to-one
   correspondence between each such structure (DCB) and the data objects
   (NDFs) in use by the system. Note that each DCB may be referenced by more
   than one Access Control Block (ACB).

   Historical note: The word "block" is a hangover from the Fortran version
   of the NDF library, where "block" refered to a Fortran common block.
   The "DCB" was a common block containing information about all known
   data objects (each one described by a "slot" in the "DCB"). By
   contrast, in the C version of the NDF library a "DCB" is a single
   structure describing a single data object. In this new sense the word
   "block" can be thought of as refering to a block of memory containing the
   structure. The same issue also attaches to the use of the word "block"
   in the other structures described below (ACB, FCB, etc).
*/

typedef struct NdfDCB {

/* Header. This contains components that are common to all types of block
   structure. */
   NdfObject header;

/* General.
   ======= */

/* Is the NDF currently locked by a specific thread? If so, the identifier
   for the locker thread. Plus a mutex to serialise access to these values. */
   int locked;
   pthread_t locker;
   pthread_mutex_t mutex;

/* Reference count. */
   int refct;

/* Mapping count. */
   int nmap;

/* Data object locators. */
   HDSLoc *loc;

/* Access mode. */
   char mod[ NDF__SZMOD + 1 ];

/* Disposal mode. */
   char dsp[ NDF__SZDSP + 1 ];

/* Data object container file and HDS path names. */
   char file[ NDF__SZFIL + 1 ];
   char path[ NDF__SZPTH + 1 ];


/* Extensions.
   ========== */
/* Locator to extension (MORE) component and whether this information is
   available. */
   HDSLoc *xloc;
   int kx;


/* Character components.
   ==================== */
/* Locators to NDF character components, names of these components and
   whether information is available about each. */
   HDSLoc *cloc[ NDF__MXCCN ];
   int kc[ NDF__MXCCN ];


/* Data array.
   ========== */
/* Data array identifier and whether this information is up to date.
   Number of current data array mappings. Storage form. */
   Ary *did;
   int kd;
   int ndmap;
   char dfrm[ NDF__SZFRM + 1 ];


/* Default.
   ======= */
/* Derived from the data array, used to set the initial attributes of
   other components. */
   char detyp[ NDF__SZTYP + 1 ];
   int decpx;
   char defrm[ NDF__SZFRM + 1 ];


/* Variance.
   ======== */
/* Variance component ARY_ system identifier, numeric data type,
   complexity, storage form and whether this information is up to date. */
   Ary *vid;
   char vtyp[ NDF__SZTYP + 1 ];
   int vcpx;
   char vfrm[ NDF__SZFRM + 1 ];
   int nvmap;
   int kv;


/* Quality.
   ======= */
/* Whether quality information is available, the quality array
   identifier, the array's storage form and the bad bits value. */
   int kq;
   int nqmap;
   HDSLoc *qloc;
   Ary *qid;
   char qfrm[ NDF__SZFRM + 1 ];
   unsigned char qbb;
   int isqbb;
   unsigned char ovqbb;


/* Axes (general).
   ============== */
/* Whether information about the axis component is available, and the
   axis locator. */
   int ka;
   HDSLoc *aloc[NDF__MXDIM ];


/* Axes (individual).
   ================= */
/* Whether axis extension information is available, and the axis
   extension locators. NOTE - THIS IS A HANGOVER FROM FORTRAN NDF. IN FACT
   NO USE IS MADE OF THIS FACILITY IN EITHER THE F77 OR C LIBRARY. */
   int kax[ NDF__MXDIM ];
   HDSLoc *axloc[ NDF__MXDIM ];

/* Whether character axis component name information is available,
   names of the character axis components, and character component
   locators. */
   int kac[ NDF__MXDIM ][ NDF__MXACN ];
   HDSLoc *acloc[ NDF__MXDIM ][ NDF__MXACN ];

/* Whether axis data array information is available, the ARY_ system
   identifier for the data array, and the array's storage form and
   type. Also the number of current axis data array mappings. */
   int kad[ NDF__MXDIM ];
   Ary *adid[ NDF__MXDIM ];
   char adtyp[ NDF__MXDIM ][ NDF__SZTYP + 1 ];
   char adfrm[ NDF__MXDIM ][ NDF__SZFRM + 1 ];
   int nadmp[ NDF__MXDIM ];

/* Whether axis variance information is available, the ARY_ system
   identifier for the variance array, and the array's storage form and
   type. */
   int kav[ NDF__MXDIM ];
   Ary *avid[ NDF__MXDIM ];
   char avtyp[ NDF__MXDIM ][ NDF__SZTYP + 1 ];
   char avfrm[ NDF__MXDIM ][ NDF__SZFRM + 1 ];
   int navmp[ NDF__MXDIM ];

/* Whether axis width information is available, the ARY_ system
   identifier for the width array, and the array's storage form and
   type. */
   int kaw[ NDF__MXDIM ];
   Ary *awid[ NDF__MXDIM ];
   char awtyp[ NDF__MXDIM ][ NDF__SZTYP + 1 ];
   char awfrm[ NDF__MXDIM ][ NDF__SZFRM + 1 ];
   int nawmp[ NDF__MXDIM ];

/* Whether axis normalisation information is available and the
   normalisation value. */
   int kan[ NDF__MXDIM ];
   int anrm[ NDF__MXDIM ];


/* History.
   ======= */

/* Whether history component information is available in the DCB. */
   int kh;

/* History structure locator. */
   HDSLoc *hloc;

/* Locator for array of history records. */
   HDSLoc *hrloc;

/* Do the history records need sorting? */
   int hsort;

/* Number of history records present. */
   int hnrec;

/* History record array extend size. */
   int hext;

/* Whether default history information is to be written. */
   int hdef;

/* Text length of the current history record. This also acts as a
   modification flag; it is set to zero if history has not been modified
   by the current application, so that no current record exists. */
   int htlen;

/* The date/time to attach to the next history record to be created, as
   a UTC Modified Julian Date. If negative, then the current time will be
   used. */
   double htime;

/* History recording update mode. */
   int humod;


/* Data Conversion.
   =============== */
/* Pointer to the object describing the associated foreign file format (or
   NULL if there is no associated foreign format file). */
   struct NdfFCB *fcb;

/* Name of associated foreign format file (if it exists). */
   char forfl[ NDF__SZFXS + 1 ];

/* Unique file identification code for foreign format files. */
   char forid[ NDF__SZFID + 1 ];

/* Whether to keep NDFs associated with foreign format files. */
   int forkp;

/* Whether the associated foreign file existed prior to being accessed
   by the NDF library, so that it already contains data. A foreign file
   name may also be associated with an NDF even if the real foreign file
   has yet to be created by conversion of the data from a native format
   copy - in such cases FOREX will be set to .FALSE. and the foreign
   file will simply be a dummy (empty) file used as a placeholder in the
   host file system. */
   int forex;


/* World Coordinate Systems (WCS).
   =============================== */
/* AST_ pointer to FrameSet containing WCS information. */
   AstFrameSet *iwcs;

/* Whether WCS component information is available in the DCB. */
   int kw;

} NdfDCB;





/* Access Control Block (ACB). */
/* --------------------------- */

/* Define a structure to contain information about the "virtual"
   objects used by the NDF_ system to provide user access to actual
   data objects, each of which is described by a Data Control Block (DCB).
   Each active ACB object refers to an associated DCB and several ACBs may
   refer to the same DCB. A one-to-one correspondence is maintained between
   active ACBs and the NDF identifiers issued to users. */

typedef struct NdfACB {

/* Header. This contains components that are common to all types of block
   structure. */
   NdfObject header;


/* Access control flags.
   ==================== */
/* If the ACB is in use, then "acc" is a bit field in which each bit
   is a boolean access control flags that control the types of access
   permitted to the virtual object described by the ACB. Access is
   permitted if the bit corresponding to the flag is set, otherwise it
   is denied. Access control flags are propagated to new virtual objects
   when they are derived from existing ones. */
   NdfAccessControlFlag access;


/* Whether access is to a section.
   ============================== */
/* If the ACB is in use and the "cut" entry is non-zero, then the object
   it describes is a "section", which allows virtual access to a subset
   or superset of the data. If this entry is zero, then the NDF is a
   "base NDF" which gives direct access to the actual data object. */
   int cut;


/* Index to data object entry in the DCB.
   ===================================== */
/* If the ACB is in use, then "dcb" is a pointer to the Data Control Block
   (DCB) that describes the actual data object to which the virtual (ACB)
   object refers. */
   NdfDCB *dcb;


/* Data array.
   ========== */
/* If the ACB is in use, then "did" contains an ARY_ system identifier
   for the data array of the virtual NDF object which the ACB describes.
   This may be a base array, or an array section, as appropriate, and
   is derived from the corresponding DCB. This identifier should always
   be valid (although the array it describes may temporarily be in an
   undefined state) and serves to define the shape of the virtual NDF. */
   Ary *did;

/* If the ACB is in use and the data array of the NDF is mapped for
   access, then "dmap" will be set non-zero to indicate this ("dmap"
   is initially set to zero when a new ACB is issued by the routine ndf1Ffs).
   If a temporary copy of the data array values has to be created for the
   purposes of mapping, then these will be held in a temporary array, whose
   ARY_ system identifier is stored in "dmtid". Otherwise, this identifier
   will be invalid. */
   int dmap;
   Ary *dmtid;

/* If the ACB is in use and its data array is mapped for access, then
   "dmcpx" will contain a boolean value indicating whether the mapped
   values are complex (non-zero) or not (zero). If complex values are
   mapped, then "dmdpt" and "dmipt" will contain pointers to the
   non-imaginary and imaginary parts of the mapped array, respectively.
   If non-complex values are mapped (dmcpx is zero), then only the dmdpt
   value is significant. */ int dmcpx; void *dmdpt; void *dmipt;

/* If the ACB is in use and its data array is mapped for access, then
   "dmtyp" contains an upper-case numeric type string (e.g. '_REAL')
   indicating the numeric type of the mapped values. */
   char dmtyp[ NDF__SZTYP +1 ];

/* If the ACB is in use and its data array is mapped for access, then
   "dmbad" contains a boolean value indicating whether the mapped array
   may contain "bad" values. The associated "dmbmd" value contains a
   boolean flag to indicate whether the value of dmbad has been explicitly
   modified by the user (e.g. by a call to ndfSbad while the array was
   mapped. This allows the NDF system to determine whether or not it
   should update the bad pixel flag of the data array via the ARY_ system
   when it is unmapped. */
   int dmbad;
   int dmbmd;


/* Variance array.
   ============== */
/* If the ACB is in use, then "vid" contains an ARY_ system identifier
   for the variance array of the virtual NDF object which the ACB describes.
   This may be a base array, or an array section, as appropriate, and is
   derived from the corresponding DCB.  If the NDF does have a variance
   component, then this identifier should be valid; conversely, if the NDF
   does not have a variance component, then the identifier will be invalid.
   Before the identifier can be used, the routine ndf1Vimp must be called
   to ensure that NDF variance array information is available in all the
   ACBs which refer to the NDF (this routine ensures that all ACBs which
   refer to the same DCB are kept in step). Note that the validity of the
   vid identifier must be checked with the aryValid routine, since the NDF's
   variance array may be deleted without updating the "vid" value. */
   Ary *vid;

/* If the ACB is in use and the variance array of the NDF is mapped for
   access, then "vmap" will be set non-zero to indicate this (vmap
   is initially set to zero when a new ACB is created). If a temporary
   copy of the variance array values has to be created for the purposes
   of mapping, then these will be held in a temporary array, whose ARY_
   system identifier is stored in "vmtid". Otherwise, this identifier
   will be invalid. */
   int vmap;
   Ary *vmtid;


/* If the ACB is in use and its variance array is mapped for access, then
   "vmcpx" will contain a boolean value indicating whether the mapped
   values are complex (non-zero) or not (zero). If complex values are
   mapped, then "vmdpt" and "vmipt" will contain pointers to the
   non-imaginary and imaginary parts of the mapped array, respectively.
   If non-complex values are mapped (vmcpx is zero), then only the vmdpt
   value is significant. */
   int vmcpx;
   void *vmdpt;
   void *vmipt;

/* If the ACB is in use and its variance array is mapped for access,
   then "vmtyp" contains an upper-case numeric type string (e.g.
   '_REAL') indicating the numeric type of the mapped values. "vmmod"
   similarly contains an upper-case string describing the access mode
   used for mapping the values (e.g. 'UPDATE'). "vmstd" contains a
   boolean value indicating whether the NDF's variance values have
   been converted to standard deviations (i.e. error values) by taking
   the square root and (equivalently) whether they must be squared
   before being written back to the variance array. */
   char vmtyp[ NDF__SZTYP + 1 ];
   char vmmod[ NDF__SZMOD + 1 ];
   int vmstd;

/* If the ACB is in use and its variance array is mapped for access,
   then "vmbad" contains a boolean value indicating whether the
   mapped array may contain "bad" values. The associated "vmbmd"
   value contains a boolean flag to indicate whether the value of
   vmbad has been explicitly modified by the user (e.g. by a call
   to ndfSbad) while the array was mapped. This allows the NDF system
   to determine whether or not it should update the bad pixel flag of
   the variance array via the ARY_ system when it is unmapped. */
   int vmbad;
   int vmbmd;

/* Quality array identifiers. */
   Ary *qid;
   int qmap;
   Ary *qmtid;
   char qmtyp[ NDF__SZTYP + 1 ];
   char qmmod[ NDF__SZMOD + 1 ];
   int *qmptr;
   unsigned char qbb;
   int isqbb;
   HDSLoc *qmtlc;
   int qmf;

/* Identifier context for each ACB. */
   int ctx;

/* Axis data array mapping items: whether mapped, mapped array ID,
   mapped value pointer, mapped value type. */
   int admap[ NDF__MXDIM ];
   Ary *admid[ NDF__MXDIM ];
   void *admpt[ NDF__MXDIM ];
   char admtp[ NDF__MXDIM ][ NDF__SZTYP + 1];

/* Axis variance array mapping items: whether mapped, mapped array ID,
   mapped value pointer, mapped value type. */
   int avmap[ NDF__MXDIM ];
   Ary *avmid[ NDF__MXDIM ];
   void *avmpt[ NDF__MXDIM ];
   char avmtp[ NDF__MXDIM ][ NDF__SZTYP + 1 ];
   char avmmd[ NDF__MXDIM ][ NDF__SZMOD + 1 ];
   int avmst[ NDF__MXDIM ];

/* Axis width array mapping items: whether mapped, mapped array ID,
   mapped value pointer, mapped value type. */
   int awmap[ NDF__MXDIM ];
   Ary *awmid[ NDF__MXDIM ];
   void *awmpt[ NDF__MXDIM ];
   char awmtp[ NDF__MXDIM ][ NDF__SZTYP + 1 ];

} NdfACB;





/* Placeholder Control Block [PCB]. */
/* -------------------------------- */

/* Define a structure to hold information about placeholders used by
   the NDF_ system. Objects in this class are kept in one-to-one
   correspondence with placeholder identifiers issued to users. */

typedef struct NdfPCB {

/* Header. This contains components that are common to all types of block
   structure. */
   NdfObject header;

/* Placeholder locator:  If the PCB is in use, then "loc is an HDS locator
   to an NDF_ placeholder, which reserves a position in the data system. */
   HDSLoc *loc;

/* Temporary placeholder flag: If the PCB is in use, then "tmp" indicates
   whether the object which replaces the placeholder object should be
   temporary. */
   int tmp;

/* New placeholder object flag.
   =========================== */
/* If a slot is in use, then "new" contains a boolean value indicating
   whether a new placeholder object was created by the NDF_ system
   (as opposed to a pre-existing object being passed by the caller).
   This flag is used to decide whether to delete the placeholder
   object when cleaning up after any error. */
   int new;

/* Foreign format code, propagation flag, file name, ID, etc.
   ========================================================= */
/* If a slot is in use, and a foreign file may be associated with the
   new NDF described by a PCB, then "fcb" holds a pointer to the FCB
   object describing the format of the foreign file, "forfl" holds the
   file's name and "forid" holds a unique identification code for
   the file (if it already exists). If "wild carding" of the output
   format is enabled and no foreign format information has yet
   been propagated to the PCB, then "prfmt" will be set non-zero,
   indicating that uncertainty still exists about the actual format of
   any foreign file. In this case, "fmt" will be the default  format,
   which may be over-ridden by subsequent propagation of this information,
   and "forfl" will hold the full name of the NDF as originally supplied
   (it may later require re-interpretation in the light of the propagated
   format information, so "forid" will be blank). */
   struct NdfFCB *fcb;
   int prfmt;
   char forfl[ NDF__SZFIL + 1 ];
   char forid[ NDF__SZFID + 1 ];

/* If a slot is in use and there is a foreign format file associated
   with it (see above), then "forkp" indicates whether the native NDF
   copy of the foreign data is to be kept (as opposed to being held
   in a temporary object which is deleted when the NDF is released
   from the NDF_ system). */
   int forkp;

/* If a slot is in use, then "ctx" contains the identifier context
   level at which the placeholder identifier for the slot was issued.
   This allows the appropriate placeholders to be annulled when each
   NDF_ context is closed. */
   int ctx;

} NdfPCB;





/* Format Control Block [FCB]. */
/* --------------------------- */

/* Define a structure to hold information about the foreign file formats
   to be recognised. */

typedef struct NdfFCB {

/* Header. This contains components that are common to all types of block
   structure. */
   NdfObject header;

/* Format name. */
   char name[ NDF__SZFMN + 1 ];

/* File name extension. */
   char ext[ NDF__SZFMX + 1 ];

/* A boolean flag indicating if the format is an input format (if not it
   is an output format). */
   int infmt;

} NdfFCB;




/* Thread Specific Data (TSD)  */
/* --------------------------- */

/* Declare the key used for accessing thread specific data */
extern pthread_key_t starlink_ndf_globals_key;

/* Define a structure to hold data values that are speific to each
   thread. */

typedef struct NdfTSD {

/* List of pending error messages (most recently reported first). */
   char elbMsg[ NDF__MXERR ][ EMS__SZMSG + 1 ];

/* Number of pending EMS error messages (zero indicates no error). */
   int elbNerr;

/* Current EMS error status value (associated with the most recently
   reported error message). */
   int elbStat;

/* Current context level. */
   int acbIdctx;

} NdfTSD;






/* Info returned by uname system function. */
/* --------------------------------------- */

/* Define a structure to strings returned by ndf1Uname. */

typedef struct NdfUNAME {
   char sysname[ 257 ];  /* Name of the operating system. */
   char nodename[ 257 ]; /* Node name of the computer */
   char release[ 257 ];  /* Version of the operating system */
   char version[ 257 ];  /* Sub-version of the operating system. */
   char machine[ 257 ];  /* Name of the hardware of the computer */
} NdfUNAME;





/* Constant values stored in read-only global variables. These are
   declared and initialised in ndf1Init.  Code should never attempt to
   change their values.
   -------------------------------------------------------------- */

/* The highest value that can be squared without overflow for each
   generic data type. Refer to these as CGEN_FUNCTION(Ndf_SQLIM). */
extern double Ndf_SQLIMD;
extern float Ndf_SQLIMF;
extern int Ndf_SQLIMI;
extern int64_t Ndf_SQLIMK;
extern short int Ndf_SQLIMW;
extern unsigned short int Ndf_SQLIMUW;
extern char Ndf_SQLIMB;
extern unsigned char Ndf_SQLIMUB;




/* Other global variables.  All external variables referred to below are
   declared in ndf1GlobalDecs.c
   ------------------------------------------------------------ */





/* Macros to lock and unlock a mutex that serialises access to the
   global variables used by ndf1Nxtsl. These mutexes should be locked
   by any function that uses ndf1Nxtsl to search the list of ACB, DCB,
   PCB or FCB entries, or which accesses these lists directly. */

extern pthread_mutex_t Ndf_ACB_mutex;
#define NDF__ACB_LOCK_MUTEX pthread_mutex_lock( &Ndf_ACB_mutex );
#define NDF__ACB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_ACB_mutex );

extern pthread_mutex_t Ndf_DCB_mutex;
#define NDF__DCB_LOCK_MUTEX pthread_mutex_lock( &Ndf_DCB_mutex );
#define NDF__DCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_DCB_mutex );

extern pthread_mutex_t Ndf_PCB_mutex;
#define NDF__PCB_LOCK_MUTEX pthread_mutex_lock( &Ndf_PCB_mutex );
#define NDF__PCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_PCB_mutex );

extern pthread_mutex_t Ndf_FCB_mutex;
#define NDF__FCB_LOCK_MUTEX pthread_mutex_lock( &Ndf_FCB_mutex );
#define NDF__FCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_FCB_mutex );

extern NdfDCB **Ndf_DCB;  /* Pointer to array of all DCB pointers */
extern NdfACB **Ndf_ACB;  /* Pointer to array of all ACB pointers */
extern NdfFCB **Ndf_FCB;  /* Pointer to array of all FCB pointers */
extern NdfPCB **Ndf_PCB;  /* Pointer to array of all PCB pointers */

extern int Ndf_NDCB;      /* Number of DCBs in above array */
extern int Ndf_NACB;      /* Number of ACBs in above array */
extern int Ndf_NFCB;      /* Number of FCBs in above array */
extern int Ndf_NPCB;      /* Number of PCBs in above array */





/* The following variables are used for local communication between the
   NDF_ library and the "source" and "sink" functions used to read and
   write AST_ data from/to HDS objects. */

/* A mutex to serialise access to the following three values. */
extern pthread_mutex_t Ndf_DCB_astmutex;
#define NDF__DCB_LOCK_ASTMUTEX pthread_mutex_lock( &Ndf_DCB_astmutex );
#define NDF__DCB_UNLOCK_ASTMUTEX pthread_mutex_unlock( &Ndf_DCB_astmutex );

/* HDS object locator. */
extern HDSLoc *Ndf_DCB_astlc;

/* Line number of text being read/written. */
extern int Ndf_DCB_astln;

/* Pointer to mapped HDS _CHAR array data. */
extern char *Ndf_DCB_astpt;





/* The following variables hold information about the application. */

/* A mutex to serialise access to the following three values. */
extern pthread_mutex_t Ndf_DCB_appmutex;
#define NDF__DCB_LOCK_APPMUTEX pthread_mutex_lock( &Ndf_DCB_appmutex );
#define NDF__DCB_UNLOCK_APPMUTEX pthread_mutex_unlock( &Ndf_DCB_appmutex );

/* Name of the currently-executing application. */
extern char Ndf_DCB_happn[ NDF__SZAPP + 1 ];

/* The command line arguments */
extern int NDF_DCB_argc;
extern char **NDF_DCB_argv;





/* The following variables hold information about character components. */

/* The names of the character components. These are initialised in
   pthread_once initialiser, and are then never changed, so do not need a
   mutex to serialise access to them (multiple threads can read them
   simultaneously withotu problems). */
extern const char *Ndf_DCB_ccn[ NDF__MXCCN ];
extern const char *Ndf_DCB_accn[ NDF__MXACN ];






/* The following variables are used to hold information about temporary
   NDFs. */

/* A mutex to serialise access to the following three values. */
extern pthread_mutex_t Ndf_TMP_mutex;
#define NDF__TMP_LOCK_MUTEX pthread_mutex_lock( &Ndf_TMP_mutex );
#define NDF__TMP_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_TMP_mutex );

/* A flag indicating if no temporary objects have yet been created, and
   therefore that a temporary container file should be created. Also
   acts as a seed for deriving a unique object name. */
extern int Ndf_TMP_count;

/* A locator for the temporary container file. */
extern HDSLoc *Ndf_TMP_tmploc;






/* The following variables are used to record usage of ADAM parameters
   by the NDF library. */

/* A mutex to serialise access to the following value. */
extern pthread_mutex_t Ndf_APB_mutex;
#define NDF__APB_LOCK_MUTEX pthread_mutex_lock( &Ndf_APB_mutex );
#define NDF__APB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_APB_mutex );

/* An AST KeyMap in which each entry has a key that is an ADAM parameter
   name, and a value that is a boolean flag indicating if ndfCancl should
   cancel the parameter if supplied with a blank parameter name. The
   parameters stored in the KeyMap are the ones for which the NDF library
   has stored explicit locators in the parameter system using
   subparPutfloc and subparPutloc These locators are only release when
   the parameter is cancelled, or the parameter system is shut down. In
   order to avoid apparent HDS locator leaks, applications may use
   ndfCancl to cancel all active NDF parameters. */
extern AstKeyMap *Ndf_APB_pars;








/* The following variables defines global variables (tuning parameters)
   which control the internal function of the NDF system. They are
   declared and initialised in function ndf1Intcb. */


/* A mutex to serialise access to the following tuning parameters. */
extern pthread_mutex_t Ndf_TCB_mutex;
#define NDF__TCB_LOCK_MUTEX pthread_mutex_lock( &Ndf_TCB_mutex );

#define NDF__TCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ndf_TCB_mutex );

/* Do format conversion flag:

   This logical flag determines whether the NDF library attempts to
   access "foreign" data formats (i.e. files not in the native NDF
   format) and to convert them to/from the native NDF format for
   output/input.  If it is set non-zero, foreign formats will be
   accessed if the NDF_FORMATS_IN and/or NDF_FORMATS_OUT environment
   variables are also set appropriately. If set to zero, all files
   must be supplied in the native NDF format and no conversions will
   be attempted. */
extern int Ndf_TCB_docvt;

/* Error tracing flag:

   This logical flag controls whether the ndf1Trace function actually
   reports a message. This routine is called whenever another NDF_
   function exits as a result of an error occurring within itself or a
   lower level function which it calls. This results in a series of
   messages showing the sequence of functions which have exited
   prematurely as a result of the error. The TCB_ETFLG flag controls
   whether this internal diagnostic information is actually produced.  */
extern int Ndf_TCB_etflg;

/* Keep NDF objects flag:

   This flag controls whether NDF objects created as intermediate
   results of data conversion on foreign format files will be
   retained.  If it is set non-zero, they will be created in the default
   directory and not deleted. If set to zero, scratch objects will be
   created and deleted when they are no longer required. Its default
   value is established by the ndf1Intcb routine. */
extern int Ndf_TCB_keep;

/* Show data conversions flag:

   This flag controls whether information about data conversion
   performed on foreign format files will be displayed. If set non-zero,
   this information will be displayed on the standard output for use
   in debugging format conversion commands. If set zero, this
   information will not be displayed. Its default value of is
   established by the ndf1Intcb routine. */
extern int Ndf_TCB_shcvt;

/* Warning message flag:

   This flag controls whether warning messages are issued on detection
   of non-fatal errors in the structure of NDF data objects. If it is
   set non-zero, a warning message is reported and immediately flushed.
   If it is set to zero, then no message is issued and execution
   continues normally. Its default value is established by the
   ndf1Intcb routine. */
extern int Ndf_TCB_warn;

/* Propagation of extensions:

   This is a pointer to an AST KeyMap in which each entry has a key that
   is the name of an NDF extension, and an integer value that is non-zero
   if the extension is to be propagated by default (by ndfProp), or zero
   if the extension is not to be propagated by default. Any extension that
   is not specified within this KeyMap is propagated by default. */
extern AstKeyMap *Ndf_TCB_pxt;

/* Automatic history creation flag:

   If this flag is non-zero, the ndfCreat and ndfNew function will add a
   History component automatically to the output NDF. If this flag is
   zero. (the default), then ndfCreat and ndfNew do not include a
   History component in the output NDF. */
extern int Ndf_TCB_autohistory;

/* Maximum number of pixels in an NDF section:

   An error will be reported if a request is made for an NDF section
   containing more than TCB_SECMAX mega-pixels. This is to guard against
   (for instance) accidental use of incorrect WCS units when specifying a
   section on the command line. */
extern int Ndf_TCB_secmax;

/* Foreign output formats flag:

   If this flag is non-zero, one or more foreign output formats are
   defined. NB - this is not strictly a tuning parameter as it is
   determined by the NDF_FORMATS_OUT environment variable. */
extern int Ndf_TCB_forout;

/* Foreign input formats flag:

   If this flag is non-zero, one or more foreign input formats are
   defined. NB - this is not strictly a tuning parameter as it is
   determined by the NDF_FORMATS_IN environment variable. */
extern int Ndf_TCB_forin;

/* Fix the date/time strings stored in history records?: */
extern int Ndf_TCB_fixdt;

/* Blank out the path in the software strings stored in history records?: */
extern int Ndf_TCB_fixsw;

/* Round floating-point values to the nearest integer. */
extern int Ndf_TCB_round;

#endif


