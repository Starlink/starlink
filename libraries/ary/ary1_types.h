#if !defined( ARY1_TYPES_INCLUDED )   /* Protect against multiple inclusion*/
#define ARY1_TYPES_INCLUDED 1

/*
*  Name:
*     ary1_types.h

*  Purpose:
*     Defines private data types and constants used by ARY.

*  Description:
*     This file defines all the private data types used within the C
*     version of ARY.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     19-JUN-2017 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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
#include "star/hds.h"
#include "ary_types.h"

/* Macros to lock and unlock a mutex that serialises access to the
   global variables used byu ary1Nxtsl (and declared in ary1Ffs). These
   mutexes should be locked by any function that uses ary1Nxtsl to search
   the list of ACB, DCB, PCB or MCP entries, or which accesses these
   lists directly. */

extern pthread_mutex_t Ary_ACB_mutex;
#define ARY__ACB_LOCK_MUTEX pthread_mutex_lock( &Ary_ACB_mutex );
#define ARY__ACB_UNLOCK_MUTEX pthread_mutex_unlock( &Ary_ACB_mutex );

extern pthread_mutex_t Ary_DCB_mutex;
#define ARY__DCB_LOCK_MUTEX pthread_mutex_lock( &Ary_DCB_mutex );
#define ARY__DCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ary_DCB_mutex );

extern pthread_mutex_t Ary_PCB_mutex;
#define ARY__PCB_LOCK_MUTEX pthread_mutex_lock( &Ary_PCB_mutex );
#define ARY__PCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ary_PCB_mutex );

extern pthread_mutex_t Ary_MCB_mutex;
#define ARY__MCB_LOCK_MUTEX pthread_mutex_lock( &Ary_MCB_mutex );
#define ARY__MCB_UNLOCK_MUTEX pthread_mutex_unlock( &Ary_MCB_mutex );

#define THREAD_DEBUG 0
#if THREAD_DEBUG
void ary1AssertLocked(pthread_mutex_t* mutex, char* name);
#define ARY__ACB_ASSERT_MUTEX ary1AssertLocked(&Ary_ACB_mutex, "ACB");
#define ARY__DCB_ASSERT_MUTEX ary1AssertLocked(&Ary_DCB_mutex, "DCB");
#define ARY__PCB_ASSERT_MUTEX ary1AssertLocked(&Ary_PCB_mutex, "PCB");
#define ARY__MCB_ASSERT_MUTEX ary1AssertLocked(&Ary_MCB_mutex, "MCB");
#else
#define ARY__ACB_ASSERT_MUTEX
#define ARY__DCB_ASSERT_MUTEX
#define ARY__PCB_ASSERT_MUTEX
#define ARY__MCB_ASSERT_MUTEX
#endif

/* Maximum number of dimensions for which the data system (HDS) is
   capable of "slicing" a primitive object. */
#define ARY__MXHSL 3

/* Unique ARY_ system facility number. */
#define ARY__FACNO 1503

/* Maximum length of a string describing an active mapping mode entry
   in the MCB, e.g. 'UPDATE'. */
#define ARY__SZAMM 6

/* Maximum length of a string describing a data object disposal mode,
   entry in the DCB, e.g. 'DELETE'. */
#define ARY__SZDSP 6

/* Maximum length of a string containing the name of an HDS container
   file (this value is somewhat arbitrary, since the maximum length of
   a file name appears to be a rather ill-defined quantity on most
   systems). */
#define ARY__SZFIL 255

/* Maximum length of a string describing a data object access mode
   entry in the DCB, e.g. 'UPDATE'. */
#define ARY__SZMOD 6

/* Maximum size of a string containing the path name of an HDS data
   object (this value is somewhat arbitrary, since the maximum depth of
   nesting of HDS objects does not appear to be defined). */
#define ARY__SZPTH 255

/* Number of access control flags maintained in each ACB, which
   determines the number of types of array access over which control
   can be exercised. */
#define ARY__MXACC 6

/* Maximum length of a string describing an initialisation option
   appended to an array mapping mode specification, e.g. 'ZERO'. */
#define ARY__SZIOP 4

/* Identifiers for block types (use a large starting value to reduce the
   chance of uninitialised integer values being interpreted as a legal
   type). */
typedef enum AryBlockType {
  ARY__DCBTYPE = 1000000,
  ARY__ACBTYPE,
  ARY__MCBTYPE,
  ARY__PCBTYPE
} AryBlockType;

/* Macro to convert a bit position to an integer value */
#define BIT_TO_VAL(bit) (1<<bit)

/* Bit masks defining each individual access control flag. */
typedef enum AryAccessControlFlag  {
  ARY__ACC_BOUND = BIT_TO_VAL(0), /* BOUNDS access control flag. */
  ARY__ACC_DELET = BIT_TO_VAL(1), /* DELETE access control flag. */
  ARY__ACC_SHIFT = BIT_TO_VAL(2), /* SHIFT access control flag. */
  ARY__ACC_TYPE  = BIT_TO_VAL(3), /* TYPE access control flag. */
  ARY__ACC_WRITE = BIT_TO_VAL(4), /* WRITE access control flag. */
  ARY__ACC_SCALE = BIT_TO_VAL(5)  /* SCALE access control flag. */
} AryAccessControlFlag;


/* Block header */
/* ------------ */
/* This structure is used as the first component in each of the
   block structures defined below. */
typedef struct AryObject {

/* Usage: This value is non-zero if the remaining values in the block
   structure are in use, and zero if it is not currently being used to
   describe an object and can thus be used to describe a new object. */
   int used;

/* Slot. The index of the block structure within the arrays maintained
   by ary1Ffs. For historical reasons, this is referred to as the slot
   number of the structure. */
   int slot;

/* Type: The type of block structure; DCB, ACB, MCB or PCB. */
   AryBlockType type;

/* Check: When an integer identifier is issued for an object, its value is
   stored in the "check" value and an identifier is only considered valid
   if it matches the "check" value in the object to which it refers. This
   allows identifiers to be rendered invalid either by the user or by the
   ARY_  system by assigning NULL to them. */
   int check;

} AryObject;

/* Data Control Block (DCB). */
/* ------------------------- */

/* Define a structure to hold information about a single physical data
   object (usually a disk file) handled by the ARY_ system.  A one-to-one
   correspondence is maintained between each such structure and the actual
   data objects known to the system. This is in contrast to the virtual
   objects which are described by the Access Control Block (ACB) structure,
   several of which may refer to a single actual data object via its
   associated DCB pointer.

   Historical note: The word "block" is a hangover from the Fortran version
   of the ARY library, where "block" refered to a Fortran common block.
   The "DCB" was a common block contining information about all known
   data objects (each one described by a "slot" in the "DCB"). By
   contract, in the C version of the ARY library a "DCB" is a single
   structure describing a single data object. In this new sense the word
   "block" can be thought of as refering to a block of memory containing the
   structure. The same issue also attaches to the use of the word "block"
   in the other structures described below (ACB, MCB, etc).
*/

typedef struct AryDCB {

/* Header. This contains components that are common to all types of block
   structure. */
   AryObject header;

/* Form of array: If the DCB is in used and "kform" is non-zero, then the
   "form" entry contains the storage form (e.g. 'SIMPLE') of the array data
   object associated with the DCB. If "kfrm" is zero, then this information
   is not available or is out of date. */
   int kform;
   char form[ ARY__SZFRM + 1 ];

/* Reference count: If the DCB is in use, then "refcount" stores the number
   of Access Control Block (ACB) entries which currently refer to the DCB,
   and hence to the associated data object. "nread" and "nwrite" count the
   number of these ACB entries for which mapped access is in effect and
   requires data to be read or written (respectively) to the data object;
   UPDATE access is included in both counts. */
   int refcount;
   int nread;
   int nwrite;

/* Data object locator: If the DCB is in use, then "loc" contains an HDS
   locator to the associated data object. */
   HDSLoc *loc;

/* Access mode: If the DCB is in use and the "kmode" value is non-zero,
   then the "mode" value contains the access mode available for the associated
   data object ('READ' or 'UPDATE'). If the "kmode" value is zero, then this
   information is not available, or is out of date. */
   int kmode;
   char mode[ ARY__SZMOD + 1 ];

/* Array state: If the DCB is in use and "kstate" is non-zero, then the
   "state" value indicates whether the array's data values have been
   defined by the completion of a WRITE operation on the array, while
   "init" indicates whether those array values which cannot be
   affected by current write operations have been initialised, so that
   they are not left undefined when the write operation completes.
   (Normally, "state" and "init" are set to zero when an array is created.
   "init" is set non-zero when the first write access is initiated and
   unaffected data in the array is initialised.  "state" is set non-zero
   when the first write operation completes.) If "kstate" is zero, then this
   information is not available or is out of date. */
   int kstate;
   int state;
   int init;

/* Disposal mode: If the DCB is in use, then the "dispose" value contains the
   disposal mode ('DELETE', 'KEEP' or 'TEMP') which indicates the action to
   be taken when the associated data object is released from the ARY_
   system. Release occurs when the "refcount" value for the object falls to
   zero. */
   char  dispose[ ARY__SZDSP + 1 ];

/* Data type and component locators: If the DCB is in use and the "ktype"
   value is non-zero, then the "type" value contains the numeric data type
   of the associated data object and "complex" indicates whether it holds
   complex data. The "dloc" value is an HDS locator to the non-imaginary
   data component and, if "complex" is non-zero, "iloc" is a locator to the
   imaginary data component. If "ktype" is zero, then this information is
   not available or is out of date. */
   int ktype;
   char type[ DAT__SZTYP + 1 ];
   int complex;
   HDSLoc *dloc;
   HDSLoc *iloc;

/* Bad pixel flag: If the DCB is in use and the "kbad" value is non-zero,
   then "bad" stores the value of the logical "bad pixel flag" for the
   associated data object. If the bad pixel flag is zero, then there are
   no "bad" pixels in the data and no checks for them need be made. If the
   bad pixel flag is non-zero, then there are (or may be) "bad" pixels
   present which must be checked for. If "kbad" is zero, then this
   information is not abailable or is out of date. */
   int kbad;
   int bad;

/* Dimensionality and bounds information: If the DCB is in use and the
   "kbnd" value is non-zero, then the "ndim" value holds the number of
   dimensions of the data object and "lbnd" and "ubnd" hold its lower and
   upper pixel index bounds in each dimension. These latter entries are
   padded with 1's to make the number of bounds up to ARY__MXDIM. If
   "kbnd" is zero, then this information is not available or is out of
   date. */
   int kbnd;
   int ndim;
   hdsdim lbnd[ ARY__MXDIM ];
   hdsdim ubnd[ ARY__MXDIM ];

/* Scale information: If the DCB is in use and the "kscl" value is non-zero
   and the storage form is "scaled", then this value contains a locator to
   a temporary HDS structure containing two components named SCALE and
   ZERO, which hold the values used to convert stored (scaled) data values
   into external (unscaled) data values:

     external = scale*stored + zero

   If "kscl" is zero, then this information is not available or is out
   of date, in which case NULL is stored. NULL is also stored if the array
   storage form is not "scaled". */
   int kscl;
   HDSLoc *scloc;

/* Accumulated pixel index shifts: If the DCB is in use, then the "shift"
   value holds an accumulated sum of all the pixel index shifts which have
   been applied to the associated data object since it was first made known
   to the ARY_ system. A total of ARY__MXDIM shifts is held, regardless of
   the actual data object dimensionality. */
   hdsdim shift[ ARY__MXDIM ];

/* Container file and HDS path names: If the DCB is in use, then the
   "file" value contains the full name of the HDS container file containing
   the associated data object and the "path" value contains the data object's
   HDS path name within that file. */
   char file[ ARY__SZFIL + 1 ];
   char path[ ARY__SZPTH + 1 ];

} AryDCB;











/* Mapping Control Block (MCB). */
/* --------------------------- */

/* Define a structure to hold information about mapped access to
   array data, so that appropriate "cleaning up" action can be taken
   (e.g. writing back of modified values) when the data are unmapped.
   A new MCB object is created whenever a mapping operation takes
   place and its address is stored in the "mcb" field of the ACB object
   for the virtual data being mapped. MCB objects are released (and the
   "mcb" pointer within the ACB reset to NULL) when the data are
   subsequently unmapped. */
typedef struct AryMCB {

/* Header. This contains components that are common to all types of block
   structure. */
   AryObject header;

/* Active mapping mode: If the MCB is in use, then the "amm" value holds the
   active mapping mode ('READ', 'WRITE' or 'UPDATE') associated with the
   MCB. */
   char amm[ ARY__SZAMM + 1 ];

/* Mapping region bounds: If the MCB is in use, then the "lmrb" and "umrb"
   values hold the lower and upper bounds of the mapping region. This is the
   region of data which the user perceives as currently being mapped, but is
   expressed in the pixel index system of the actual data object. These
   values are padded with 1's to give ARY__MXDIM bounds, regardless of the
   actual data object dimensionality. */
   hdsdim lmrb[ ARY__MXDIM ];
   hdsdim umrb[ ARY__MXDIM ];

/* Mapping transfer region: If the MCB is in use, then the "mtrex" value
   holds a logical value indicating whether a "mapping transfer region"
   exists for the current mapping. The mapping transfer region represents
   that region of the data object to which the mapping actually has
   access, and through which transfer of data values occurs during mapping
   and unmapping operations. It is formed from the intersection of the data
   object's bounds, the virtual (ACB) object's data transfer window and
   the virtual object's bounds as known to the user (having first converted
   them all into the same pixel index system).  If the "mtrex" value is
   non-zero, then the "lmtr" and "umtr" values hold the lower and upper
   bounds of the mapping transfer region, expressed in the pixel index
   system of the actual data object. These values are padded with 1's to
   give ARY__MXDIM bounds, regardless of the data object's actual
   dimensionality. */
   int mtrex;
   hdsdim lmtr[ ARY__MXDIM ];
   hdsdim umtr[ ARY__MXDIM ];

/* Mapping transfer region properties: If the MCB is in use and the "mtrex"
   value is non-zero, then the "mrful" value holds a logical value indicating
   whether the mapping transfer region completely fills the mapping region.
   If this value is non-zero, then the mapped data have not been "padded" to
   make them up to the size which the user requires. The "whole" value holds
   a logical value indicating whether the mapping and mapping transfer
   regions both match the actual data object extent. If this is so, then the
   whole data object will have been mapped directly (without any slicing or
   padding). */
   int mrful;
   int whole;

/* Data copy flags: If the MCB is in use, then the "dcopy" value holds a
   logical value indicating if the mapped non-imaginary data component is
   held in a temporary object representing a memory "copy" of the actual
   data. If mapped access to complex data is in effect, then "icopy" holds
   a similar flag for the imaginary component. */
   int dcopy;
   int icopy;

/* Mapped object locators: If the MCB is in use, then the "dloc" value is
   a locator to the data object which is mapped to contain the non-imaginary
   data component passed to the user. This may be a cloned locator to the
   actual data object, or may be a locator to a temporary object holding
   a data "copy".  If mapped access to complex data is in effect, then the
   "iloc" value holds a similar locator for the imaginary component. */
   HDSLoc *dloc;
   HDSLoc *iloc;

/* Pointers to mapped data: If the MCB is in use, then the "dpntr" value
   contains a pointer to the mapped non-imaginary component of the data.
   If mapped access to complex data is in effect, then the "ipntr" value
   contains a similar pointer to the imaginary component. */
   void *dpntr;
   void *ipntr;

/* Bad pixel flags: If the MCB is in use, then the "bad" value contains
   a logical value indicating whether "bad" values may be present in the
   mapping transfer region associated with the current mapping. This value
   is not used if no mapping transfer region exists. The "pbad" value
   similarly indicates if "bad" values are present in the "padding"
   region which exists around the mapping transfer region if the latter
   does not completely fill the mapping region. */
   int bad;
   int pbad;

/* Data type for access: If the MCB is in use, then the "type" value
   contains the numeric data type used to access the data and the "complex"
   value indicates if access to complex data was requested. */
   char type[ DAT__SZTYP + 1 ];
   int complex;

} AryMCB;




/* Access Control Block (ACB). */
/* --------------------------- */

/* Define a structure to hold information about the "virtual"
   objects used by the ARY_ system to provide user access to actual
   data objects described by the Data Control Block (DCB) structure.
   Each ACB object refers to an associated DCB object, and several ACB
   objects may refer to the same DCB object. A one-to-one correspondence
   is maintained between active ACB objects and the array identifier
   values issued to users. */

typedef struct AryACB {

/* Header. This contains components that are common to all types of block
   structure. */
   AryObject header;

/* Access control flags: If the ACB is in use, then "acc" is a bit field
   in which each bit is a logical flag controlling the types of access
   permitted to the virtual object described by the ACB object. Access is
   permitted if the bit corresponding to the flag is set, otherwise it is
   denied. Access control flags are propagated to new virtual objects when
   they are derived from existing ones. */
   AryAccessControlFlag access;

/* Whether access is to a cut: If the ACB is in use and the "cut" value is
   non-zero, then the object it describes is a "cut", which allows virtual
   access to a subset or superset of the data. If this value is zero, then
   the array is a "base array" which gives direct access to the actual data
   object. */
   int cut;

/* Index to data object value in the DCB: If the ACB is in use, then "dcb"
   is a pointer to the DCB structure that describes the actual data object
   to which the virtual (ACB) object refers. */
   AryDCB *dcb;

/* Index to mapping value in MCB: If the ACB is in use and the virtual
   object it describes is mapped for access, then "mcb" is a pointer to
   the MCB structure that controls access to the mapped data. If the object
   is not mapped, then the "mcb" will be NULL. */
   AryMCB *mcb;

/* Dimensionality and bounds information: If the ACB is in use, then the
   "ndiM" value holds the number of dimensions of the virtual object which
   it describes and the "lbnd" and "ubnd" values hold the lower and upper
   pixel index bounds of the object. These values are padded with 1's to
   give ARY__MXDIM bounds, regardless of the actual object dimensionality. */
   int ndim;
   hdsdim lbnd[ ARY__MXDIM ];
   hdsdim ubnd[ ARY__MXDIM ];

/* Data transfor window: If the ACB is in use, then the "dtwex" value
   indicates whether a "data transfer window" exists for the virtual
   object it describes. The data transfer window consists of lower and
   upper bounds on the region of actual data to which the virtual object
   has access and is used (for instance) to ensure that virtual objects
   which are derived from other objects cannot gain access to regions of
   data which were inaccessible to their parent. The data transfer window
   always has ARY__MXDIM dimensions and is stored using the "reference
   frame" pixel index system (this is the pixel index system which the
   actual data object had when it first became known to the ARY_ system).
   If the "dtwex" value is zero, then no access to actual data is
   available, otherwise the "ldtw" and "udtw" values hold the lower and
   upper bounds of the data transfer window. */
   int dtwex;
   hdsdim ldtw[ ARY__MXDIM ];
   hdsdim udtw[ ARY__MXDIM ];

/* Bad pixel flag: If the ACB is in use, then the "bad" value indicates
   whether there may be accessible "bad" values in that part of the data
   object which lies within the data transfer window (since access to data
   is always restricted by the bounds of the actual data object, this flag
   takes no account of the data transfer window extending beyond the edge of
   the data object). This value is only significant if a data transfer
   window exists. */
   int bad;

/* Accumulated pixel index shifts: If the ACB is in use, then the "shift"
   value contains an accumulated sum of the pixel index shifts applied to
   the virtual object. These shifts are initialised using the values
   associated with the data object value in the DCB at the time the ACB
   value is created. They subsequently accumulate any further shifts
   applied to the virtual (ACB) object. */
   hdsdim shift[ ARY__MXDIM ];

} AryACB;



/* Placeholder Control Block (PCB). */
/* -------------------------------- */

/* Define a structure to hold information about placeholders used by
   the ARY_ system. Objects in this class are kept in one-to-one
   correspondence with placeholder identifiers issued to users. */

typedef struct AryPCB {

/* Header. This contains components that are common to all types of block
   structure. */
   AryObject header;

/* Placeholder locator:  If the PCB is in use, then "loc is an HDS locator
   to an ARY_ placeholder, which reserves a position in the data system. */
   HDSLoc *loc;

/* Temporary placeholder flag: If the PCB is in use, then "tmp" indicates
   whether the object which replaces the placeholder object should be
   temporary. */
   int tmp;

} AryPCB;












#endif
