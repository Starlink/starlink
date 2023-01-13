/*
*+
*  Name:
*     NBS

*  Purpose:
*     High-level routines for the noticeboard system
*     --- this module contains only those routines that are user-callable.

*  Language:
*     ANSI C

*  Description:
*     The noticeboard system routines provide a fast means for processes to
*     share data in global memory. A given process may own as many noticeboards
*     as it wishes and may access noticeboards owned by other processes.
*     Normally the only process that writes to a noticeboard is its owner but
*     other processes that know what they are doing can subvert this rule
*     either by calling a special routine or else by accessing noticeboard data
*     by using a pointer.
*
*     The original interfaces were defined at the {AAO} workshop in October
*     1985. {V1.0} was implemented in C by William Lupton at {RGO} in January
*     1986. The changes for {V2.3.1} were made by William Lupton at {AAO} in
*     February 1990. Refer to Section~\ref {V2.3.1 New Features} for details.
*     Changes for {V2.4.0} were made by David Allan at the University of
*     Birmingham.
*
*     Noticeboards are identified by name and each can contain a hierarchy of
*     items. Each item has a name, a type, a structure / primitive attribute,
*     and, if primitive, a maximum number of dimensions, a maximum number of
*     bytes, a current shape and a current value. The type and shape are not
*     used by the routines but their values can be put and got and they can be
*     used when implementing higher-level routines on top of the noticeboard
*     routines. Noticeboards are self-defining --- a process can find and access
*     data from a noticeboard without knowing anything about what it contains.
*
*     \section {General Description}
*     \subsection {Names and Types} All names and types are converted to
*     upper-case and all white space and non-printing characters in them are
*     ignored. Thus " {\tt William Lupton} " is regarded as "WILLIAMLUPTON". An
*     upper limit of 16 characters is imposed on both names and types (the
*     limit applies to the length after the removal of white space and
*     non-printing characters).
*
*     \subsection {Data Consistency} Provided there is only one writer to a
*     noticeboard and the standard get and put routines are used, the routines
*     will guarantee that consistent data is read from the noticeboard. A
*     writer will never have to wait but a reader will retry potentially until
*     a timeout occurs.
*
*     \subsection {Static Definition} A (deliberate) restriction of the
*     noticeboard system is that a noticeboard is static in structure. Its
*     structure must be defined before any values are put into it and once the
*     definition is complete no more items can be created. There are a set of
*     routines called NBS_DEFINE_* and NBS_*_DEFINITION which allow definition
*     of noticeboard contents, saving definitions to and restoring them from
*     file (the initial state of a noticeboard is that all items have zero
*     length values). Only once the definition phase is complete can the
*     NBS_PUT_* and NBS_GET_* routines be used.
*
*     The initial reason for this was efficiency. However it is not in fact
*     particularly difficult to allow new items to created on the fly without
*     compromising efficiency and this restriction should be seen more as a way
*     of preventing the noticeboard routines from being used for purposes for
*     which they were not designed and for which better tools (such as HDS)
*     exist. Note that this "static" restriction refers only to the creation of
*     items in the noticeboard. It is always possible to change item shapes and
*     values.
*
*     \subsection {Saving to Disc} When saving a noticeboard to disc, the
*     programmer has a choice as to whether to save only the definition
*     or else the definition plus the data. If only the definition is saved
*     then each time that the definition is restored
*     all items will revert to having zero lengths. However if the data
*     is saved as well the NBS_RESTORE_NOTICEBOARD routine can be used
*     to restore both the definition and the data, and a subsequent call
*     to NBS_SAVE_NOTICEBOARD will update the disc file so that the next
*     time that the noticeboard is restored it will be in the same state
*     as when it was saved.
*
*     \subsection {C-callable Version} The NBS routines have been implemented
*     to make them easy to use from Fortran and for this reason all character
*     strings are passed by address of VMS descriptor and all other parameters
*     are passed by reference. The routines are actually written in C and it
*     would be unreasonable and wasteful to force C programmers to build
*     descriptors just so that they could be decoded back to the same C strings
*     that they started off as. For this reason, every NBS routine has an
*     associated NBC routine which passes all character parameters as C
*     zero-terminated strings, all input scalar parameters by value and all
*     other parameters by reference. See the source of the demonstration
*     programs described in Appendix~\ref {Demonstration Programs} for examples
*     of the use of this C-callable version.
*
*     \subsection {Portability to UNIX} The NBS routines have been implemented
*     in as portable a way as possible. In particular they have successfully
*     been ported to a UNIX System V system (well, {V2.2} was anyway), which
*     just happens to have a set of shared memory system services that are
*     sufficiently close in facilities to the VMS global section system
*     services to require only a small amount of conditionally compiled code in
*     the low-level "create section" and "map section" routines. Portability is
*     perhaps not a major issue at present but it is good to know that the use
*     of the noticeboard system routines will not necessarily be a bar to
*     portability in the future.
*
*     \subsection {Status Conventions} All the NBS routines use ADAM modified
*     status conventions in that they will do nothing if status is not NBS__OK
*     (0) on entry. They can all optionally be called as functions, in which
*     case the function value is the same as the returned status value.
*
*     \subsection {Terminology} These routines use the terminology "item" to
*     refer to either a structured noticeboard object (ie, one with no values
*     but possibly having lower-level objects) or to a primitive noticeboard
*     object (ie, one with a shape and with values). The terms
*     "item identifier", "identifier" or "ID" refer to "handles" (cf HDS
*     locators) which allow access to all information pertaining to an item. As
*     far as callers are concerned these are just integers. As far as the
*     noticeboard system is concerned they are pointers to data structures
*     called "item_descriptor"s. In the routine specifications they are
*     regarded as integers. A zero ID is always an invalid ID and all routines
*     will detect an attempt to use a zero ID. Similarly all routines that
*     return ID's will return zero ID's on failure. Note that there is a danger
*     of an access violation if true garbage ID's are passed. This is unlikely
*     to happen, because of the use of the modified status convention.

*  Copyright:
*     Copyright (C) 1986-1990, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 1999, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2005 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (AAO)
*     JAB: Jeremy Bailey (AAO)
*     DJA: David J. Allan (ROSAT)
*     BKM: Brian McIIwrath (Starlink)
*     AA: Alasdair Allan (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     03-Feb-1986 (WFL):
*        Original version
*     05-Jul-1986 (JAB):
*        Speed up search loop in NBS_FIND_ITEM: NBS_STRIMP call
*        removed from loop, NBS_RELOCATE_ADDRESS calls removed and
*        relocation coded directly.
*     13-Apr-1987 (WFL):
*        Correct bug whereby MAXNAME-char names are not found
*         in NBS_FIND_ITEM since they are not null-terminated.
*         Prevent NBS_END_DEFINITION from copying more than
*         MAXNAME characters from NAME.
*     17-Jul-1987 (WFL):
*        Change names of INCLUDEd files to be NBS_xxx.H.
*         Check for zero IDs on entry and replace NULL with NIL.
*         Remove ENVID from NBS_GET_NAME.
*         New NBS_GET_PARENT, NBS_GET_MODIFIED and NBS_INC_MODIFIED routines.
*     20-Jul-1987 (WFL):
*        In NBS_GET_SHAPE, regard MAXDIMS as modified argument
*         and return as maximum number of dimensions.
*         Alter interface to NBS_STR* routines.
*         Use ODD function for testing incoming LOGICALs.
*         Replace NBS_COPY with _CHMOVE macro.
*         New status NBS__TOOMANYBYTES in NBS_PUT_VALUE, plus
*         use NBS__TOOMANYDIMS in NBS_PUT_SHAPE.
*     21-Jul-1987 (WFL):
*        Add new NBS_PUT_SIZE routine.
*         Add definition size, section size and version to the
*         board info that is part of the global section - check version
*         in NBS_FIND_NOTICEBOARD
*         In NBS_FIND_NOTICEBOARD, make a relocated copy of the
*         entire noticeboard definition and thus dispense with the
*         relocating in NBS_FIND_ITEM and NBS_FIND_NTH_ITEM.
*         Correct apparent bug in NBS_FIND_NOTICEBOARD where
*         mapper's PID was copied to overwrite creator's PID.
*         Bring inline the three NBS_*_FIELDS routines and modify
*         so that items are added in alphabetical order.
*         Ensure that all routines that return ID's return NIL
*         ones if they fail.
*         Add logical argument to relocate routines telling
*         them whether to add or subtract the offset. Split IFS_OFFSET
*         argument into I_OFFSET for items and FBS_OFFSET for fixed,
*         board and shape information.
*     22-Jul-1987 (WFL):
*        Remove board_info BASE; set up BOARD pointers at
*            definition time and don't take local copies of board_info.
*     31-Jul-1987 (WFL):
*        Extend header comments.
*         Insert missing braces after IF_OK... macro calls.
*     06-Nov-1987 (WFL):
*        Portable VMS / UNIX version. Use vms / unix / strdescr
*         macros as necessary.
*     11-Feb-1988 (WFL):
*        Use macros for routine names and use NBS_ prefix when
*         strdescr is defined, NBC_ when it is not. Also use #module to
*         set module name explicitly. Change strdescr to c_string.
*         Change NBS_ADD_INTERLOCKED to ADD_INTERLOCKED.
*         Change SAVE to character OPTION in NBS_END_DEFINITION.
*         Add OFFSET arguments to NBS_PUT_VALUE and NBS_GET_VALUE.
*     16-Feb-1988 (WFL):
*        Add TIMEOUT_COUNT, WORLD_WRITE, INCREMENT_MODIFY and
*         CHECK_MODIFY static variables and NBS_TUNE routine to alter them
*         Add handling of FIXED_INFO CHILDREN component.
*         Add handling of BOARD_INFO MODIFIED component and return
*         it with NBS_GET_MODIFIED for non-primitive items.
*         Add NBS_GET_CHILDREN to return number of children and
*         NBS_GET_INFO to return miscellaneous information about a
*         noticeboard.
*         Make NBS_FIND_ITEM use a binary search (since the
*         names are known to be linked in alphabetical order).
*     17-Feb-1988 (WFL):
*        Receive definitions of MAXALLOC, ITEM_BASE and DATA_BASE
*         from NBS_MAC.H and allow tune of MAXALLOC (now called
*         MAX_DEFN_SIZE).
*     03-Mar-1988 (WFL):
*        Improve binary search.
*         Update comments ready for use in documentation.
*     25-Mar-1988 (WFL):
*        Simple-minded implementation of saving data to
*         disc, restoring from disc etc. Involves new NBS_RESTORE_NOTICE-
*         BOARD and NBS_SAVE_NOTICEBOARD routines.
*         Alter NBS_GET_INFO to take name of required item (so
*         that the interface need not continually change).
*     28-Mar-1988 (WFL):
*        Rationalise initialisation of BOARD_INFO components.
*     30-Mar-1988 (WFL):
*        Only use #module on VMS; allow NBS_TUNE to use same
*         arguments for new and old values; make NBS_END_DEFINITION
*         clear DEFINING even if it fails (once re-location is done);
*         re-order NBS_INC_MODIFIED and NBS_PUT_SIZE.
*     08-Apr-1988 (WFL):
*        (Again) update comments ready for use in documentation.
*     20-May-1988 (WFL):
*        Use NBS_INTIMP to import input integers (allows them
*         to be passed by value for the C interfaces).
*     10-Apr-1989 (WFL):
*        Use EXTERN rather than STATIC so that NBS and NBC
*         routines can be used intermingled (but not values alterable
*         using NBS_TUNE, since these are initialised)
*     01-Feb-1990 (WFL):
*        Add TIMEOUT_INTERVAL to list of tuneable parameters;
*         add NBS_TUNE_NOTICEBOARD and set extra board info parameters
*         on noticeboard creation; use noticeboard-specific parameters
*         on get and put; maintain ACCESSED count; allow use of same
*         variable for ENV(S)ID and (S)ID; on find noticeboard, wait
*         for it to be valid; delay between tries on get
*     02-Feb-1990 (WFL):
*        Add NBS_LOSE_NOTICEBOARD and NBS_LOSE_ITEM; if notice-
*         board existed when being created, adopt current process as
*         owner; on NBS_FIND_NOTICEBOARD and if owner, unmap section
*         mapped on create
*     06-Feb-1990 (WFL):
*        Account for PARENT, DATA and ACCESSED being in unions;
*         maintain TRIGGER and MODIFIED in ITEM_DESCR; use
*         _ADD_INTERLOCKED macro (uses _ADAWI); add new NBS_PUT_TRIGGER,
*         NBS_GET_MODIFIED_POINTER and NBS_GET_UPDATED routines; be
*         careful to unmap if mapping routine fails after having mapped;
*         add use of ORIGINAL_UNMAPPED
*     07-Feb-1990 (WFL):
*        Account for SHAPE and GLOBAL_BASE being in unions;
*         update modified count for NBS_PUT_SIZE; explicitly set NIL
*         top-level PARENT in relocated copy of definition
*     09-Feb-1990 (WFL):
*        Upgrade comments; GET_MODIFIED_POINTER to work for
*         structures too; always increment noticeboard modified count;
*         use ADD_INTERLOCKED for noticeboard modified count increment;
*         use OR of NBS_TUNE flags and NBS_TUNE_NOTICEBOARD flags for
*         upwards compatibility; correct test for NBS__DATASAVED
*     15-Feb-1990 (WFL):
*        Revert to regarding TIMEOUT_COUNT and TIMEOUT_DELAY
*         as per-process values
*     25-Mar-1993 (DJA):
*        Error reporting by EMS. All high level routines report errors
*        as well as setting STATUS bad.
*        #module moved to nbs_module.h to cope with failure of ULTRIX
*        to handle this preprocessor directive.
*        Use F77 and CNF package to do FORTRAN interface.
*        New routines NBS_GET_CVALUE, NBS_PUT_CVALUE and NBS_GET_CINFO
*        to handle put and fetch of character values portably.
*        Prologue formats changed to SST forms.
*      5-May-1993 (DJA):
*        New routine NBS_SLEEPMS provides millisecond timing. Replaced
*        _wait macro in NBS_MAC.H. Exit handler to remove remaining mapped
*        sections on UNIX.
*     24-Nov-1994 (DJA):
*        Correct Fortran version of NBS_GET_CVALUE call.
*     27-Apr-1999 (BKM):
*        Omit unnecessary malloc and free declarations.
*     28-Jun-2004 (AA):
*        Fixed ifdef logic for building under Mac OSX
*     12-Sep-2004 (TIMJ):
*        Minimize compiler warnings. Use new style ems calling convention.
*     04-Jul-2005 (PWD):
*        Comment out void arglist function prototypes. GCC4 doesn't allow those.
*     15-SEP-2008 (TIMJ):
*        3-arg emsSetc is deprecated.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* Include files	*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

/* Error codes */

#include "nbs_err.h"

/* Structure definitions */

#include "nbs_typ.h"


/* Depending on whether c_string macro is defined or not, define macros
   for routine names and define module name. */

#ifdef c_string

/* C strings are used for character string arguments and input scalars are
   passed by value. Module name is "nbc" and routine names are all prefixed
   "nbc_". */

#include "nbs.h"

#define NBS_TUNE		nbc_tune
#define NBS_TUNE_NOTICEBOARD	nbc_tune_noticeboard
#define NBS_BEGIN_DEFINITION	nbc_begin_definition
#define NBS_DEFINE_STRUCTURE	nbc_define_structure
#define NBS_DEFINE_PRIMITIVE	nbc_define_primitive
#define NBS_DEFINE_SHAPE	nbc_define_shape
#define NBS_END_DEFINITION	nbc_end_definition
#define NBS_RESTORE_DEFINITION	nbc_restore_definition
#define NBS_RESTORE_NOTICEBOARD	nbc_restore_noticeboard
#define NBS_SAVE_NOTICEBOARD	nbc_save_noticeboard
#define NBS_FIND_NOTICEBOARD	nbc_find_noticeboard
#define NBS_FIND_ITEM		nbc_find_item
#define NBS_FIND_NTH_ITEM	nbc_find_nth_item
#define NBS_LOSE_NOTICEBOARD	nbc_lose_noticeboard
#define NBS_LOSE_ITEM		nbc_lose_item
#define NBS_PUT_VALUE		nbc_put_value
#define NBS_PUT_CVALUE		nbc_put_cvalue
#define NBS_PUT_SHAPE		nbc_put_shape
#define NBS_INC_MODIFIED	nbc_inc_modified
#define NBS_PUT_SIZE		nbc_put_size
#define NBS_PUT_TRIGGER		nbc_put_trigger
#define NBS_GET_CVALUE		nbc_get_cvalue
#define NBS_GET_VALUE		nbc_get_value
#define NBS_GET_SHAPE		nbc_get_shape
#define NBS_GET_MODIFIED	nbc_get_modified
#define NBS_GET_MODIFIED_POINTER nbc_get_modified_pointer
#define NBS_GET_UPDATED		nbc_get_updated
#define NBS_GET_POINTER		nbc_get_pointer
#define NBS_GET_NAME		nbc_get_name
#define NBS_GET_TYPE		nbc_get_type
#define NBS_GET_SIZE		nbc_get_size
#define NBS_GET_PRIMITIVE	nbc_get_primitive
#define NBS_GET_PARENT		nbc_get_parent
#define NBS_GET_CHILDREN	nbc_get_children
#define NBS_GET_INFO		nbc_get_info
#define NBS_GET_CINFO		nbc_get_cinfo

#define NBS_INTIMP(_out,_in)	*(_out) = (_in)
#define NBS_PTRIMP(_type,_out,_in) {(_out)=NIL; *((_type *) &_out) = (_in);}


#else

/* String descriptors used for character string arguments and input scalars
   are passed by reference. Module name is "nbs" and routine names are all
   prefixed "nbs_". */

/* Fortran <-> C conversion macros */

#include "f77.h"

#define NBS_TUNE		F77_EXTERNAL_NAME(nbs_tune)
#define NBS_TUNE_NOTICEBOARD	F77_EXTERNAL_NAME(nbs_tune_noticeboard)
#define NBS_BEGIN_DEFINITION	F77_EXTERNAL_NAME(nbs_begin_definition)
#define NBS_DEFINE_STRUCTURE	F77_EXTERNAL_NAME(nbs_define_structure)
#define NBS_DEFINE_PRIMITIVE	F77_EXTERNAL_NAME(nbs_define_primitive)
#define NBS_DEFINE_SHAPE	F77_EXTERNAL_NAME(nbs_define_shape)
#define NBS_END_DEFINITION	F77_EXTERNAL_NAME(nbs_end_definition)
#define NBS_RESTORE_DEFINITION	F77_EXTERNAL_NAME(nbs_restore_definition)
#define NBS_RESTORE_NOTICEBOARD	F77_EXTERNAL_NAME(nbs_restore_noticeboard)
#define NBS_SAVE_NOTICEBOARD	F77_EXTERNAL_NAME(nbs_save_noticeboard)
#define NBS_FIND_NOTICEBOARD	F77_EXTERNAL_NAME(nbs_find_noticeboard)
#define NBS_FIND_ITEM		F77_EXTERNAL_NAME(nbs_find_item)
#define NBS_FIND_NTH_ITEM	F77_EXTERNAL_NAME(nbs_find_nth_item)
#define NBS_LOSE_NOTICEBOARD	F77_EXTERNAL_NAME(nbs_lose_noticeboard)
#define NBS_LOSE_ITEM		F77_EXTERNAL_NAME(nbs_lose_item)
#define NBS_PUT_VALUE		F77_EXTERNAL_NAME(nbs_put_value)
#define NBS_PUT_CVALUE		F77_EXTERNAL_NAME(nbs_put_cvalue)
#define NBS_PUT_SHAPE		F77_EXTERNAL_NAME(nbs_put_shape)
#define NBS_INC_MODIFIED	F77_EXTERNAL_NAME(nbs_inc_modified)
#define NBS_PUT_SIZE		F77_EXTERNAL_NAME(nbs_put_size)
#define NBS_PUT_TRIGGER		F77_EXTERNAL_NAME(nbs_put_trigger)
#define NBS_GET_CVALUE		F77_EXTERNAL_NAME(nbs_get_cvalue)
#define NBS_GET_VALUE		F77_EXTERNAL_NAME(nbs_get_value)
#define NBS_GET_SHAPE		F77_EXTERNAL_NAME(nbs_get_shape)
#define NBS_GET_MODIFIED	F77_EXTERNAL_NAME(nbs_get_modified)
#define NBS_GET_MODIFIED_POINTER F77_EXTERNAL_NAME(nbs_get_modified_pointer)
#define NBS_GET_UPDATED		F77_EXTERNAL_NAME(nbs_get_updated)
#define NBS_GET_POINTER		F77_EXTERNAL_NAME(nbs_get_pointer)
#define NBS_GET_NAME		F77_EXTERNAL_NAME(nbs_get_name)
#define NBS_GET_TYPE		F77_EXTERNAL_NAME(nbs_get_type)
#define NBS_GET_SIZE		F77_EXTERNAL_NAME(nbs_get_size)
#define NBS_GET_PRIMITIVE	F77_EXTERNAL_NAME(nbs_get_primitive)
#define NBS_GET_PARENT		F77_EXTERNAL_NAME(nbs_get_parent)
#define NBS_GET_CHILDREN	F77_EXTERNAL_NAME(nbs_get_children)
#define NBS_GET_INFO		F77_EXTERNAL_NAME(nbs_get_info)
#define NBS_GET_CINFO		F77_EXTERNAL_NAME(nbs_get_cinfo)

#define NBS_INTIMP(_out,_in)	*(_out) = *(_in)
#define NBS_PTRIMP(_type,_out,_in) {(_out)=NIL; *((_type *) &_out) = *(_in);}

#endif


/* Macro definitions   */

#include "nbs_mac.h"

/* Low level prototypes */

#include "nbs1.h"

/* Error processing */
#include "sae_par.h"
#include "ems.h"

/* Several utility macros handle standard checks at the tops of procedures */

#define IF_OK if (!OK (*status))\
		 ;\
	      else

#define IF_OK_AND_DEFINING if (!OK (*status))\
                              ;\
                           else if (!nbs_gl_defining)\
                              *status = NBS__NOTDEFINING;\
                           else

#define IF_OK_AND_NOT_DEFINING if (!OK (*status))\
                                  ;\
                               else if (nbs_gl_defining)\
                                  *status = NBS__DEFINING;\
                               else

/*  Various flavours of the NBS storage allocator */

#define NBS_ITEM_ALLOC() (item_id) NBS_ALLOC (ITEM_SIZE)
#define NBS_FIXED_ALLOC() (fixed_id) NBS_ALLOC (FIXED_SIZE)
#define NBS_BOARDINFO_ALLOC() (board_id) NBS_ALLOC (BOARDINFO_SIZE)
#define NBS_SHAPE_ALLOC(n) (shape_id) NBS_ALLOC (n*INT_SIZE)

/*  Various flavours of the standard C MALLOC and FREE */

#define ITEM_ALLOC() (item_id) malloc (ITEM_SIZE)
#define ITEM_FREE(id) free ((char *)id)

/* Local constant definitions	*/

/* ITEM_BASE is the address at which item addresses are to start relative to
   the start of the noticeboard.	*/

#define ITEM_BASE 4

/* DATA_BASE is the address at which data addresses are to start relative to
   the start of the noticeboard.	*/

#define DATA_BASE 4

/* External definitions   */

extern int nbs_gl_defining; /* Currently defining noticeboard contents? */
extern item_id nbs_ga_base; /* Pointer to base of noticeboard currently being
                               defined */

extern int nbs_gl_item_total;	/* Current total size of Item_descriptor's */
extern int nbs_gl_fixed_total;	/* Current total size of Fixed_info's */
extern int nbs_gl_shape_total;	/* Current total size of shape information */
extern int nbs_gl_boardinfo_total; /* Current total size of Board_info's */
extern int nbs_gl_data_total;	/* Current total size of primitive data */

extern int nbs_gl_pid;		/* PID of current process */

/* Module level definitions   */

/* These can all have their values altered by NBS_TUNE. Current values are
   passed to noticeboards on creation */

static int max_defn_size = 32768; /* Size of area used for building defn */
static int timeout_count = 100; /* Number of tries before timeout on FIND/GET */
static int timeout_interval = 100; /* Number of ms to wait between tries */
static int world_write = NO; /* Whether the world can write to noticeboards */
static int increment_modify = YES; /* Whether to increment MODIFIED on PUT */
static int check_modify = YES; /* Whether to check MODIFIED on GET */

/*
*  Section name:
*     NBS_TUNE

*  Purpose:
*     Routines used to alter global noticeboard system parameters

*  Language:
*     ANSI C

*  Description:
*     Two types of parameters can be altered. The first type is global to a
*     process and not to a noticeboard (they are essentially Fortran COMMON
*     block or C static variables). The second type is global to a noticeboard
*     and are thus shared by all processes accessing that noticeboard. The
*     original NBS_TUNE routine is used to alter parameters of the first type
*     and the newer NBS_TUNE_NOTICEBOARD is used to tune parameters of the
*     second type. When a noticeboard is created, its initial parameter values
*     are copied from the current values of the first type.
*
*     When a parameter is altered, its previous value is returned and this
*     permits a routine to alter a parameter, use the new value and then
*     restore the parameter to its previous value.
*
*     The NBS_FIND, NBS_GET and NBS_PUT routines make rather complicated use of
*     these values. For those parameters which are logical flags, they use
*     the OR of the value of the first type and the value of the second type.
*     For those parameters which are numeric values, they use the value of the
*     first type.
*/

/*
*+
*  Name:
*     NBS_TUNE

*  Purpose:
*     Alter the value of a global parameter

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_TUNE (NAME,VALUE,OLDVALUE,STATUS)

*  Description:
*     Check that the parameter name is legal. \\
*     Copy the previous value of the parameter to the supplied variable. \\
*     Alter the specified global parameter.
*
*     There are currently six global parameters which can be altered in
*     this way:
*
*     [MAX_DEFN_SIZE:] an integer that indicates how much memory should
*        be allocated for building the noticeboard definition during the
*        definition phase. The default is 32768 bytes.
*     [TIMEOUT_COUNT:] an integer that indicates how many times to loop
*        before timing out when finding noticeboards or when getting values
*        or shapes. The default is 100.
*     [TIMEOUT_INTERVAL:] an integer that indicates how many milliseconds
*        to wait between attempts at finding noticeboards or when getting
*        values or shapes. The default is 100.
*     [WORLD_WRITE:] a logical (only the lsb is used) that indicates whether
*        the world (ie non-owners) can write to noticeboards. The default is
*        FALSE.
*     [INCREMENT_MODIFY:] a logical (only the lsb is used) that indicates
*        whether the modified count should be incremented when putting the
*        values of items. The default is TRUE.
*     [CHECK_MODIFY:] a logical (only the lsb is used) that indicates whether
*        the modified count should be checked when getting the values of items.
*        The default is TRUE.
*
*     Note that the parameters which can be altered are global to a process and
*     not to a noticeboard (they are essentially Fortran COMMON block or C
*     static variables). When a parameter is altered its previous value is
*     returned and this permits a routine to alter a parameter, use the new
*     value and then restore the parameter to its previous value.
*
*     The NBS_FIND, NBS_GET and NBS_PUT routines make rather complicated use of
*     these values. For those parameters which are logical flags, they use
*     the OR of the default value (or the value set using NBS_TUNE) and the
*     value set using NBS_TUNE_NOTICEBOARD. For those parameters which are
*     numeric values, they use the value set using NBS_TUNE.
*
*     When a noticeboard is created, it inherits the default values or the
*     values set using NBS_TUNE and these values may subsequently be altered
*     using the NBS_TUNE_NOTICEBOARD routine.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name of the parameter to alter. See the above list. Can be
*        abbreviated so long as it remains unambiguous but this is not
*        recommended because new parameters may be supported in the future.
*        Case is not significant.
*     VALUE = INTEGER (Given)
*        The value that the parameter is to take.
*     OLDVALUE = INTEGER (Returned)
*        The old value of the parameter.
*     STATUS = INTEGER (Given and returned)
*        The global status.
*                NBS__BADOPTION	=> Illegal parameter name

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     01-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP         Import a string, converting to upper-case
*     C RTL:
*        STRNCMP            Compare one string with another up to a specified
*                           limiting number of characters

*  Prior Requirements:
*     None.

*-
*/
int NBS_TUNE ( RW_CHARACTER(name), R_INTEGER(value), W_INTEGER(oldvalue),
               W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(value)
  GENPTR_INTEGER(oldvalue)
  GENPTR_INTEGER(status)

/* External function declarations */

/* Local variable declarations */

   char		tname[MAXNAME];
   int		tvalue;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the parameter name (converting case) and set the value as
   appropriate. Allow abbreviation. */

      NBS_STRIMP(tname,name,MAXNAME);
      NBS_INTIMP (&tvalue,value);
      if (strncmp(tname,"MAX_DEFN_SIZE",1) == 0) {	   /* Max defn size */
         *oldvalue = max_defn_size;
         max_defn_size = tvalue;
         }
      else if (strncmp(tname,"TIMEOUT_COUNT",9) == 0) {	   /* Timeout count */
         *oldvalue = timeout_count;
         timeout_count = tvalue;
         }
      else if (strncmp(tname,"TIMEOUT_INTERVAL",9) == 0) { /* Timeout int'val */
         *oldvalue = timeout_interval;
         timeout_interval = tvalue;
         }
      else if (strncmp(tname,"WORLD_WRITE",1) == 0) {	   /* World write */
         *oldvalue = world_write;
         world_write = tvalue & 1;
         }
      else if (strncmp(tname,"INCREMENT_MODIFY",1) == 0) { /* Inc modify */
         *oldvalue = increment_modify;
         increment_modify = tvalue & 1;
         }
      else if (strncmp(tname,"CHECK_MODIFY",1) == 0) {	   /* Check modify */
         *oldvalue = check_modify;
         check_modify = tvalue & 1;
         }
      else                                                 /* Report error */
         {
	 *status = NBS__BADOPTION;
         emsSetnc( "OPT", tname, MAXNAME );
         emsRep( "NBS_TUNE_BADOPT", "Bad tune option /^OPT/", status );
         }
      }
   return (*status);
}

/*
*+
*  Name:
*     NBS_TUNE_NOTICEBOARD

*  Purpose:
*     Alter the value of a noticeboard-specific global parameter

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_TUNE_NOTICEBOARD (ID,NAME,VALUE,OLDVALUE,STATUS)

*  Description:
*     Check that the parameter name is legal. \\
*     Copy the previous value of the parameter to the supplied variable. \\
*     Alter the specified value in the noticeboard.
*
*     There are currently three global parameters which can be altered in
*     this way:
*
*     [WORLD_WRITE:] a logical (only the lsb is used) that indicates whether
*        the world (ie non-owners) can write to noticeboards. The default is
*        FALSE.
*     [INCREMENT_MODIFY:] a logical (only the lsb is used) that indicates
*        whether the modified count should be incremented when putting the
*        values of items. The default is TRUE.
*     [CHECK_MODIFY:] a logical (only the lsb is used) that indicates whether
*        the modified count should be checked when getting the values of items.
*        The default is TRUE.
*
*     Note that the parameters which can be altered are global to a specific
*     noticeboard. When a parameter is altered its previous value is returned
*     and this permits a routine to alter a parameter, use the new value and
*     then restore the parameter to its previous value.
*
*     The NBS_FIND, NBS_GET and NBS_PUT routines make rather complicated use of
*     these values. They use the OR of the default value (or the value set
*     using NBS_TUNE) and the value set using NBS_TUNE_NOTICEBOARD.
*
*     When a noticeboard is created, it inherits the default values or the
*     values set using NBS_TUNE and these values may subsequently be altered
*     using the NBS_TUNE_NOTICEBOARD routine.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of noticeboard or of any item in it,
*        whose parameter value is to be altered.
*     NAME = CHARACTER*(*) (Given)
*        The name of the parameter to alter. See the above list. Can be
*        abbreviated so long as it remains unambiguous but this is not
*        recommended because new parameters may be supported in the future.
*        Case is not significant.
*     VALUE = INTEGER (Given)
*        The value that the parameter is to take.
*     OLDVALUE = INTEGER (Returned)
*        The old value of the parameter.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*                NBS__NILID	=> NIL ID
*        	NBS__BADOPTION	=> Illegal parameter name

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     01-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP         Import a string, converting to upper-case
*     C RTL:
*        STRNCMP            Compare one string with another up to a specified
*                           limiting number of characters

*  Prior Requirements:
*     None.

*-
*/
int NBS_TUNE_NOTICEBOARD ( R_INTEGER(id), RW_CHARACTER(name), R_INTEGER(value),
                           W_INTEGER(oldvalue), W_INTEGER(status) TRAIL(name) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(value)
  GENPTR_INTEGER(oldvalue)
  GENPTR_INTEGER(status)

/* External function declarations */

/* Local variable declarations */

   item_id	tid;
   char		tname[MAXNAME];
   int		tvalue;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID, the parameter name (converting case) and the new
   value. */

      NBS_PTRIMP (int,tid,id);
      NBS_STRIMP (tname,name,MAXNAME);
      NBS_INTIMP (&tvalue,value);

/* Check that the item ID is not NIL. */

      if (tid == NIL)
         {
	 *status = NBS__NILID;
         emsRep( "NBS_TUNE_NB_NILID", "NIL item ID", status );
         }

/* Set the value as appropriate. Allow abbreviation. */

      else if (strncmp(tname,"WORLD_WRITE",1) == 0) {	   /* World write */
         *oldvalue = tid->board->world_write;
         tid->board->world_write = tvalue & 1;
         }
      else if (strncmp(tname,"INCREMENT_MODIFY",1) == 0) { /* Inc modify */
         *oldvalue = tid->board->increment_modify;
         tid->board->increment_modify = tvalue & 1;
         }
      else if (strncmp(tname,"CHECK_MODIFY",1) == 0) {	   /* Check modify */
         *oldvalue = tid->board->check_modify;
         tid->board->check_modify = tvalue & 1;
         }
      else
         {
	 *status = NBS__BADOPTION;
         emsSetnc( "OPT", tname, MAXNAME );
         emsRep( "NBS_TUNE_NB_BADOPT",
                "Bad noticeboard tune option /^OPT/", status );
         }
      }
   return (*status);
}

/*
*  Section name:
*     NBS_DEFINITION

*  Purpose:
*     Routines used to define the noticeboard structure

*  Language:
*     ANSI C

*  Description:
*     A private memory area is obtained and item definitions are created in it.
*     Storage is not allocated for data but nevertheless data pointers are
*     defined as though the data began at address zero. These pointers are
*     relocated at the time that the definition is either written to disc or
*     else copied to a noticeboard.
*
*/

/*
*+
*  Name:
*     NBS_BEGIN_DEFINITION

*  Purpose:
*     Begin definition of the contents of a noticeboard
*     and return a static identifier to the top level of the noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_BEGIN_DEFINITION (SID,STATUS)

*  Description:
*     Check that we are not currently in the middle of defining a noticeboard.\\
*     Allocate the memory area in which the noticeboard definition is built. \\
*     From this area, allocate space to describe the new noticeboard and fill
*     in fields appropriate to the top level of a noticeboard. \\
*     Remember the address of the memory area and note that we are now in the
*     middle of defining a noticeboard. \\
*     Return the address of the item descriptor to the caller for use in
*     subsequent calls.

*  Arguments:
*     SID = INTEGER (Returned)
*        Static identifier of the top-level of the noticeboard. This should
*        be used in subsequent calls to the NBS_DEFINE_* routines.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__DEFINING   => Already defining a noticeboard
*        	NBS__INITALLOCFAILED => Storage allocation failed
*        	NBS__NOMOREROOM => Storage area is full up

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     22-Jul-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_ALLOC          Allocate storage from private memory area
*        NBS_INIT_ALLOC     Initialise private memory area
*     C RTL:
*        STRNCPY            Copy one string to another up to a specified limit

*  Prior Requirements:
*     None.

*-
*/
int NBS_BEGIN_DEFINITION ( item_id *sid, W_INTEGER(status) )
{
  GENPTR_INTEGER(status)

/* External function declarations */

      /*extern char	*NBS_ALLOC();
        extern char 	*NBS_INIT_ALLOC();*/

/* Local variable declarations */

   board_id	bid;
   fixed_id	fid;

/* Ensure that a NIL value will be returned for the static ID if the routine
   fails.   */
   item_id      lsid = NIL;

/* Start of code */

/* Check that status is good and we are not in the middle of defining a
   noticeboard.	*/

   IF_OK_AND_NOT_DEFINING {

/* Initialise the routines that allocate storage during the definition phase.
   MAX_DEFN_SIZE is the size of the area that is allocated and DATA_BASE is an
   arbitrary constant (it must be greater than zero) that is used as the base
   for allocating storage for primitive data items - the storage is not really
   allocated until the noticeboard is created. The reason that DATA_BASE must
   be greater than zero is to allow a distinction between a NIL pointer and a
   pointer that has been relocated to zero. */

      if (NBS_INIT_ALLOC (max_defn_size,DATA_BASE) == NIL)
         {
	 *status = NBS__INITALLOCFAILED;
         emsRep( "NBS_BEGIN_DEFINITION_ALLOCFAIL",
              "Couldn't initialise storage allocator",
              status );
         }

/* Allocate an item descriptor (which consists of pointers), a fixed information
   area and a noticeboard information area. There is one each of the former two
   for each item in the noticeboard and only one of the latter per noticeboard.
   */

      else {
	 lsid = NBS_ITEM_ALLOC ();
	 fid = NBS_FIXED_ALLOC ();
	 bid = NBS_BOARDINFO_ALLOC ();
	 if (lsid == NIL || fid == NIL || bid == NIL)
            {
	    *status = NBS__NOMOREROOM;
            emsRep( "NBS_BEGIN_DEFINITION_NOMOREROOM",
              "Couldn't get memory - increase MAX_DEFN_SIZE when defining",
              status );
            }

	 else {

/* Fill in fields in the allocated structures. First the item descriptor. Most
   these fields are pointers and are relocated before saving to a file or
   copying to a noticeboard. The rest are only used in the private copies of
   noticeboard definitions which are maintained by mapping processes. */

	    lsid->vp.valid = NO;
	    lsid->heir = NIL;
	    lsid->sibling = NIL;
	    lsid->fixed = fid;
	    lsid->gs.global_base = NIL;
	    lsid->board = bid;
	    lsid->da.data = NIL;
	    lsid->trigger = NIL;
	    lsid->modified = 0;

/* Next the fixed information. This contains no pointers so never needs to be
   relocated.	*/

	    strncpy (fid->name,"NOTICEBOARD",MAXNAME);
	    strncpy (fid->type,"NOTICEBOARD",MAXTYPE);
	    fid->primitive = NO;
            fid->children = 0;
	    fid->maxbytes = 0;
	    fid->actbytes = 0;
	    fid->modified = 0;
	    fid->maxdims = 0;
	    fid->actdims = 0;

/* Finally the board information. Most fields are overwritten when their
   values are known.	*/

	    bid->version = NBSVERSION;
	    bid->file_size = 0;
	    bid->defn_size = 0;
	    bid->section_size = 0;
	    bid->pid = 0;
            bid->modified = 0;
	    bid->world_write = 0;
	    bid->increment_modify = 0;
	    bid->check_modify = 0;
	    bid->chan = NIL;
	    bid->global_base = NIL;
	    strncpy (bid->save_name,"",MAXFILE);

/* These five *_TOTAL variables keep a record of how many bytes have been
   allocated for item descriptors, board information, fixed information, shape
   information and data.    */

	    nbs_gl_item_total = ITEM_SIZE;
	    nbs_gl_boardinfo_total = BOARDINFO_SIZE;
	    nbs_gl_fixed_total = FIXED_SIZE;
	    nbs_gl_shape_total = 0;
	    nbs_gl_data_total = 0;

/* Remember the base address and note that we are now defining a noticeboard.
   */
	    nbs_ga_base = lsid;
	    nbs_gl_defining = YES;
	 }
      }

/* Export the pointer */
      EXPORT_POINTER(lsid,sid);

   }

   return (*status);
}

/*
*+
*  Name:
*     NBS_DEFINE_STRUCTURE

*  Purpose:
*     Define a new entry for a structured item within another structured item
*     and return a static identifier to the new item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_DEFINE_STRUCTURE (ENVSID,NAME,TYPE,SID,STATUS)

*  Description:
*     Check that we are currently in the middle of defining a noticeboard. \\
*     Check that the environment static ID is not NIL and does not pertain to
*     a primitive item. \\
*     Allocate space to describe the new item and fill in fields appropriate to
*     a structured item such that items at this level are in alphabetical order.
*     (If an item of this name already exists, create a new item but position it
*     before the existing item). \\
*     Return the address of the item descriptor to the caller for use in
*     subsequent calls.

*  Arguments:
*     ENVSID = INTEGER (Given)
*        Static identifier of the item in the noticeboard which is the
*        parent of the item to be created.
*     NAME = CHARACTER*(*) (Given)
*        Name of the new item.
*     TYPE = CHARACTER*(*) (Given)
*        Type of the new item.
*     SID = INTEGER (Returned)
*        Static identifier of the new structured item. This should be used in
*        subsequent calls to the NBS_DEFINE_* routines.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NOTDEFINING => Not currently defining a noticeboard
*        	NBS__NILSID	    => NIL static ID
*        	NBS__PRIMITIVE  => Prospective parent is primitive
*        	NBS__NOMOREROOM => Storage area is full up

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     22-Jul-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_ALLOC          Allocate storage from private memory area
*        NBS_STRIMP         Import a string, converting to upper-case
*     C RTL:
*        STRNCMP            Compare one string with another up to a specified
*                           limiting number of characters
*        STRNCPY            Copy one string to another up to a specified limit

*  Prior Requirements:
*     NBS_BEGIN_DEFINITION must have been called.

*-
*/
int NBS_DEFINE_STRUCTURE ( R_INTEGER(envsid), RW_CHARACTER(name),
                           RW_CHARACTER(type), item_id *sid,
                           W_INTEGER(status) TRAIL(name) TRAIL(type) )
{
  GENPTR_INTEGER(envsid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(type)
  GENPTR_INTEGER(status)

/* External function declarations */

      /*extern char	*NBS_ALLOC();*/

/* Local variable declarations */

   item_id	tenvsid;
   fixed_id	fid;
   item_id	last;

/* Ensure that a NIL value will be returned for the static ID if the routine
   fails.   */
   item_id      lsid = NIL;

   item_id	this;
   char		tname[MAXNAME];

/* Start of code */

/* Check that status is good and we are currently defining a noticeboard. */

   IF_OK_AND_DEFINING {

/* Import the environment static ID.	*/

      NBS_PTRIMP (int,tenvsid,envsid);

/* Check that the parent ID is not NIL and that the parent is not primitive. */

      if (tenvsid == NIL)
         {
	 *status = NBS__NILSID;
         emsRep( "NBS_DEFINE_STRUCTURE_NILSID", "NIL static ID", status );
         }
      else if (tenvsid->fixed->primitive)
         {
	 *status = NBS__PRIMITIVE;
         emsRep( "NBS_DEFINE_STRUCTURE_PRIM", "Item is primitive", status );
         }

/* Allocate an item descriptor (which consists of pointers) and a fixed
   information area and a noticeboard information area.  */

      else {
	 lsid = NBS_ITEM_ALLOC ();
	 fid = NBS_FIXED_ALLOC ();
	 if (lsid == NIL || fid == NIL)
            {
	    *status = NBS__NOMOREROOM;
            emsRep( "NBS_DEFINE_STRUCTURE_NOMOREROOM",
              "Couldn't get memory - increase MAX_DEFN_SIZE when defining",
              status );
            }

/* Import the name (this removes non-printing white-space characters and
   converts to upper case). Then search through the existing items at this
   level until they have all been searched or until one is found whose name
   comes later in the alphabet than the new one. */

	 else {
	    NBS_STRIMP (tname,name,MAXNAME);
	    for (last=NIL, this=tenvsid->heir;
		 this!=NIL;
		 last=this, this=this->sibling)
	       if (strncmp (tname,this->fixed->name,MAXNAME) <= 0) break;

/* Link the new item into the existing structures and fill in the appropriate
   fields. First the item descriptor.	*/

	    lsid->vp.parent = tenvsid;
	    lsid->heir = NIL;
	    lsid->sibling = this;
	    if (last == NIL)
	       tenvsid->heir = lsid;
	    else
	       last->sibling = lsid;
	    lsid->fixed = fid;
	    lsid->gs.shape = NIL;
	    lsid->board = tenvsid->board;
	    lsid->da.data = NIL;
	    lsid->trigger = NIL;
	    lsid->modified = 0;

/* Next increment the parent's count of its children. */

            tenvsid->fixed->children++;

/* Next the fixed information.	*/

	    strncpy (fid->name,tname,MAXNAME);
	    NBS_STRIMP (fid->type,type,MAXTYPE);
	    fid->primitive = NO;
            fid->children = 0;
	    fid->maxbytes = 0;
	    fid->actbytes = 0;
	    fid->modified = 0;
	    fid->maxdims = 0;
	    fid->actdims = 0;

/* Increment the variables that keep track of the number of bytes that have been
   allocated for item descriptors and fixed information.    */

	    nbs_gl_item_total += ITEM_SIZE;
	    nbs_gl_fixed_total += FIXED_SIZE;
	 }
      }

/* Export the pointer to the caller */
      EXPORT_POINTER(lsid,sid);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_DEFINE_PRIMITIVE

*  Purpose:
*     Define a new entry for a primitive item within another structured item
*     and return a static identifier to the new item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_DEFINE_PRIMITIVE (ENVSID,NAME,TYPE,MAXDIMS,MAXBYTES,
*        						   SID,STATUS)

*  Description:
*     Check that we are currently in the middle of defining a noticeboard. \\
*     Check that the environment static ID is not NIL and does not pertain to
*     a primitive item. \\
*     Allocate space to describe the new item and fill in fields appropriate to
*     a primitive item such that items at this level are in alphabetical order.
*     (If an item of this name already exists, create a new item but position it
*     before the existing item). \\
*     Return the address of the item descriptor to the caller for use in
*     subsequent calls.

*  Arguments:
*     ENVSID = INTEGER (Given)
*        Static identifier of the item in the noticeboard which is the
*        parent of the item to be created.
*     NAME = CHARACTER*(*) (Given)
*        Name of the new item.
*     TYPE = CHARACTER*(*) (Given)
*        Type of the new item.
*     MAXDIMS = INTEGER (Given)
*        Maximum number of dimensions possessed by this item.
*     MAXBYTES = INTEGER (Given)
*        Maximum number of bytes in this item's value
*     SID = INTEGER (Returned)
*        Static identifier of the new structured item. This should be used in
*        subsequent calls to the NBS_DEFINE_* routines (only NBS_DEFINE_SHAPE
*        is permitted though).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NOTDEFINING => Not currently defining a noticeboard
*        	NBS__NILSID	    => NIL static ID
*        	NBS__PRIMITIVE  => Prospective parent is primitive
*        	NBS__NOMOREROOM => Storage area is full up

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     22-Jul-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_ALLOC          Allocate storage from private memory area
*        NBS_STRIMP         Import a string, converting to upper-case
*     C RTL:
*        STRNCMP            Compare one string with another up to a specified
*                           limiting number of characters
*        STRNCPY            Copy one string to another up to a specified limit

*  Prior Requirements:
*     NBS_BEGIN_DEFINITION must have been called.

*-
*/
int NBS_DEFINE_PRIMITIVE ( R_INTEGER(envsid), RW_CHARACTER(name),
                           RW_CHARACTER(type), R_INTEGER(maxdims),
                           R_INTEGER(maxbytes), item_id *sid,
                           W_INTEGER(status) TRAIL(name) TRAIL(type) )
{
  GENPTR_INTEGER(envsid)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(type)
  GENPTR_INTEGER(maxdims)
  GENPTR_INTEGER(maxbytes)
  GENPTR_INTEGER(status)

/* External function declarations */

      /*extern char	*NBS_ALLOC();
        extern char	*NBS_DATA_ALLOC();*/

/* Local variable declarations */

   item_id	tenvsid;
   int		tmaxdims;
   int		tmaxbytes;
   data_id	data;
   fixed_id	fid;
   item_id	last;
   item_id	lsid = NIL;
   shape_id	shape;
   item_id	this;
   char		tname[MAXNAME];

/* Start of code */

/* Check that status is good and we are currently defining a noticeboard. */

   IF_OK_AND_DEFINING {

/* Import the environment static ID, the maximum number of dimensions and
   the maximum number of bytes.	*/

      NBS_PTRIMP (int,tenvsid,envsid);
      NBS_INTIMP (&tmaxdims,maxdims);
      NBS_INTIMP (&tmaxbytes,maxbytes);

/* Adjust the size of the data area to ensure correct alignment */
      tmaxbytes = _ALIGN_ITEM(tmaxbytes);

/* Check that the parent ID is not NIL and that the parent is not primitive. */

      if (tenvsid == NIL) {
	 *status = NBS__NILSID;
         emsRep( "NBS_DEFINE_PRIMITIVE_NILSID", "NIL static ID", status );
         }
      else if (tenvsid->fixed->primitive) /* Must be structured */
         {
	 *status = NBS__PRIMITIVE;
         emsRep( "NBS_DEFINE_PRIMITIVE_PRIM", "Item is primitive", status );
         }

/* Allocate an item descriptor (which consists of pointers) and a fixed
   information area and a noticeboard information area.  */

      else {
	 lsid = NBS_ITEM_ALLOC ();
	 fid = NBS_FIXED_ALLOC ();
	 shape = NBS_SHAPE_ALLOC (tmaxdims);
	 data = NBS_DATA_ALLOC (tmaxbytes);
	 if (lsid == NIL || fid == NIL || shape == NIL || data == NIL)
            {
	    *status = NBS__NOMOREROOM;
            emsRep( "NBS_BEGIN_PRIMITIVE_NOMOREROOM",
              "Couldn't get memory - increase MAX_DEFN_SIZE when defining",
              status );
            }

/* Import the name (this removes non-printing white-space characters and
   converts to upper case). Then search through the existing items at this
   level until they have all been searched or until one is found whose name
   comes later in the alphabet than the new one. */

	 else {
	    NBS_STRIMP (tname,name,MAXNAME);
	    for (last=NIL, this=tenvsid->heir;
		 this!=NIL;
		 last=this, this=this->sibling)
	       if (strncmp (tname,this->fixed->name,MAXNAME) <= 0) break;

/* Link the new item into the existing structures and fill in the appropriate
   fields. First the item descriptor.	*/

	    lsid->vp.parent = tenvsid;
	    lsid->heir = NIL;
	    lsid->sibling = this;
	    if (last == NIL)
	       tenvsid->heir = lsid;
	    else
	       last->sibling = lsid;
	    lsid->fixed = fid;
	    lsid->gs.shape = shape;
	    lsid->board = tenvsid->board;
	    lsid->da.data = data;
	    lsid->trigger = NIL;
	    lsid->modified = 0;

/* Next increment the parent's count of its children. */

            tenvsid->fixed->children++;

/* Next the fixed information.	*/

	    strncpy (fid->name,tname,MAXNAME);
	    NBS_STRIMP (fid->type,type,MAXTYPE);
	    fid->primitive = YES;
            fid->children = 0;
	    fid->maxbytes = tmaxbytes;
	    fid->actbytes = 0;
	    fid->modified = 0;
	    fid->maxdims = tmaxdims;
	    fid->actdims = 0;

/* Increment the variables that keep track of the number of bytes that have been
   allocated for item descriptors, fixed information, shape information and item
   data.    */

	    nbs_gl_item_total += ITEM_SIZE;
	    nbs_gl_fixed_total += FIXED_SIZE;
	    nbs_gl_shape_total += tmaxdims * INT_SIZE;
	    nbs_gl_data_total += tmaxbytes;
	 }
      }

/*  Export pointer to caller */
      EXPORT_POINTER(lsid,sid);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_DEFINE_SHAPE

*  Purpose:
*     Define an initial shape for a primitive item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_DEFINE_SHAPE (SID,NDIMS,DIMS,STATUS)

*  Description:
*     Check that we are currently in the middle of defining a noticeboard. \\
*     Check that the environment static ID is not NIL and pertains to a
*     primitive item. \\
*     Check that the requested number of dimensions is not too large. \\
*     Copy the shape information to the relevant parts of the item information.

*  Arguments:
*     SID = INTEGER (Given)
*        Static identifier of the item in the noticeboard
*        which is to be given an initial shape.
*     NDIM = INTEGER (Given)
*        Number of dimensions.
*     DIMS = INTEGER(*) (Given)
*        Dimensions.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NOTDEFINING => Not currently defining a noticeboard
*        	NBS__NILSID	    => NIL static ID
*        	NBS__PRIMITIVE  => Prospective parent is primitive
*        	NBS__TOOMANYDIMS => Too many dimensions

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     22-Jul-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_BEGIN_DEFINITION must have been called.

*-
*/
int NBS_DEFINE_SHAPE ( R_INTEGER(sid), R_INTEGER(ndims),
                       RW_INTEGER_ARRAY(dims), W_INTEGER(status) )
{
  GENPTR_INTEGER(sid)
  GENPTR_INTEGER(ndims)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tsid;
   int		tndims;
   int		i;

/* Start of code */

/* Check that status is good and we are currently defining a noticeboard. */

   IF_OK_AND_DEFINING {

/* Import the item static ID and the number of dimensions.	*/

      NBS_PTRIMP (int,tsid,sid);
      NBS_INTIMP (&tndims,ndims);

/* Check that the item static ID is not NIL, that the item is primitive and
   that the number of dimensions is not more than were declared when the item
   was created. */

      if (tsid == NIL)
         {
	 *status = NBS__NILSID;
         emsRep( "NBS_DEFINE_SHAPE_NILSID", "NIL static ID", status );
         }
      else if (!tsid->fixed->primitive) /* Must be primitive */
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_DEFINE_SHAPE_NOTPRIM", "Item is not primitive", status );
         }
      else if (tndims > tsid->fixed->maxdims) /* Too many dimensions? */
         {
         *status = NBS__TOOMANYDIMS;
         emsRep( "NBS_DEFINE_SHAPE_TOOMANYDIMS",
                 "More dimensions than maximum allowed", status );
         }

/* Update the number of dimensions and copy the dimension values.   */

      else {
	 tsid->fixed->actdims = tndims;
	 for (i=0; i<tndims; i++)
	   tsid->gs.shape[i] = dims[i];
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_END_DEFINITION

*  Purpose:
*     End the definition of a noticeboard
*     and then create the noticeboard, save the definition in a file, or save
*     the definition plus data in a file

*  Language:
*     ANSI C

*  Invocation:
*     (int) = NBS_END_DEFINITION (NAME,OPTION,STATUS)

*  Description:
*     Check that we are currently in the middle of defining a noticeboard. \\
*     Calculate how large the definition and data parts of the noticeboard
*     are. \\
*     Relocate all pointers in the definition so that they are relative to zero
*     rather than being actual program virtual addresses (actually they are
*     made relative to a small positive integer to avoid problems with zero
*     pointers). \\
*     If the option parameter indicates, write the definition and optionally
*     the data to a file (a default file extension of .NBD is applied). \\
*     Otherwise, create the noticeboard, copy the definition to it, write the
*     calling process' id to the global section to denote ownership and mark
*     the noticeboard as being valid. \\
*     De-allocate the memory area used to amass the noticeboard definition and
*     note that we are no longer defining a noticeboard. \\
*
*     If the noticeboard already existed, NBS__SECTIONEXISTED status is
*     returned and the calling process becomes its owner.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        If OPTION is DEFINITION_SAVE or NOTICEBOARD_SAVE, the name of the
*        file to write the definition or definition plus data to (with a
*        default file type of .NBD). Otherwise (OPTION is CREATE_NOTICEBOARD)
*        the name of the noticeboard to create.
*     OPTION = CHARACTER*(*) (Given)
*        Option that governs whether the noticeboard definition or definition
*        plus data is saved to a file or whether the noticeboard is simply
*        created on the spot without being associated with a file. Can be
*        abbreviated so long as it remains unambiguous but this is not
*        recommended because new options may be supported in the future. Case
*        is not significant. Possible values are:
*
*            DEFINITION_SAVE => Save the definition to a file that
*        		does not contain space allocated for the data.
*           NOTICEBOARD_SAVE => Save the definition to a file that
*        		does contains space allocated for the data.
*            CREATE_NOTICEBOARD => Create the noticeboard immediately
*        		without associating it with a file. This is assumed
*        		if an illegal value of OPTION is given.
*
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NOTDEFINING => Not currently defining a noticeboard
*        	NBS__CANTOPEN   => Can't create the definition file
*        	NBS__CANTWRITE  => Can't write the definition file
*        	NBS__SECTIONEXISTED => Noticeboard of this name already
*        			    existed
*
*             	SS$_*	    => System service codes from SYS$CRMPSC

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     09-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_CREATE_SECTION     Create a noticeboard
*        NBS_DEINIT_ALLOC       Deallocate the memory used whilst defining
*        NBS_RELOCATE_ADDRESS   Relocate an individual address
*        NBS_RELOCATE_POINTERS  Recursively relocate all pointers
*        NBS_ALLOC              Allocate storage from private memory area
*        NBS_STRIMP             Import a string, converting to upper-case
*        NBS_WRITE_FILE         Write a noticeboard definition file
*     C RTL:
*        GETPID                 Get process ID
*        STRNCPY                Copy one string to another up to a specified
*                               limit

*  Prior Requirements:
*     NBS_BEGIN_DEFINITION must have been called.

*-
*/
int NBS_END_DEFINITION ( RW_CHARACTER(name), RW_CHARACTER(option),
                         W_INTEGER(status) TRAIL(name) TRAIL(option) )
{
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(option)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   board_id	bid;
   item_id	global_base;
   char		toption;
   int		file_size;
   int		defn_size;
   int		section_size;

/* Start of code */

/* Check that status is good and we are currently defining a noticeboard. */

   IF_OK_AND_DEFINING {

/* Copy the noticeboard name (recall that no name is supplied when the
   definition is begun). */

      NBS_STRIMP (nbs_ga_base->fixed->name,name,MAXNAME);

/* Copy the first character of the option (converting case). */

      NBS_STRIMP (&toption,option,1);

/* Calculate the size of the definition part of the noticeboard and the total
   size of the noticeboard.	*/

      defn_size = nbs_gl_item_total + nbs_gl_fixed_total + nbs_gl_shape_total +
							 nbs_gl_boardinfo_total;
      section_size = defn_size + nbs_gl_data_total;

/* If a file is to be written, calculate its size - either just the definition
   or else the whole section (note that illegal option is taken to mean just
   create noticeboard). */

      if (toption == 'D')
	 file_size = defn_size;
      else if (toption == 'N')
	 file_size = section_size;
      else
         file_size = 0;

/* Copy these quantities to the board information. They are needed (or at least
   the definition size is needed) when the noticeboard is later mapped by a
   prospective user.	*/

      nbs_ga_base->board->file_size = file_size;
      nbs_ga_base->board->defn_size = defn_size;
      nbs_ga_base->board->section_size = section_size;

/* Relocate all pointers in the noticeboard definition. This makes all item
   descriptor, fixed information, shape information and board information
   pointers relative to the constant ITEM_BASE and makes all data pointers
   relative to the constant DATA_BASE (previously data pointers were relative
   to DATA_BASE but did not accound for the size of the definition - all the
   data immediately follows the definition in the noticeboard).	*/

      NBS_RELOCATE_POINTERS (nbs_ga_base, (int) nbs_ga_base - ITEM_BASE,
				(int) nbs_ga_base - ITEM_BASE, -defn_size, NO);

/* If requested, write the definition to the file.	*/

      if (file_size != 0)
         NBS_WRITE_FILE (CF_C_ARG(name),(char *) nbs_ga_base,file_size,
               defn_size, section_size,status CF_TRAIL(name) );

/* Otherwise, create and map the section. If this is OK, copy the definition
   from the private memory area to the low part of the noticeboard ...   */

      else {
	 global_base = (item_id) NBS_CREATE_SECTION
           (CF_C_ARG(name),section_size,status CF_TRAIL(name) );
	 if (OK (*status) || *status == NBS__SECTIONEXISTED) {
	    if (OK (*status))
	       _chmove (defn_size,(char *) nbs_ga_base,(char *) global_base);

/* ... and relocate the board information address so that information relevant
   to this (the owner) process can be filled in. Note that this is done even
   is the section already existed, except in UNIX where the section is not
   mapped and global_base will be -1 */

            if ( global_base != ((item_id) -1) ) {
	      bid = global_base->board;
	      bid = (board_id) NBS_RELOCATE_ADDRESS
	      			(bid, (int) global_base - ITEM_BASE, YES);
	      bid->pid = getpid ();
	      bid->world_write = world_write;
	      bid->increment_modify = increment_modify;
	      bid->check_modify = check_modify;
	      bid->chan = NIL;
	      bid->global_base = global_base;
	      bid->original_unmapped = NO;
	      strncpy (bid->save_name,"",MAXFILE);

/* Noticeboard is now valid and can be found. */

	      global_base->vp.valid = YES;
              }
	 }
      }

/* Deallocate the private memory area and note that we are no longer defining
   a noticeboard.   */

      NBS_DEINIT_ALLOC ();
      nbs_gl_defining = NO;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_RESTORE_DEFINITION

*  Purpose:
*     Restore a noticeboard definition from file
*     and create the noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_RESTORE_DEFINITION (NAME,SAVE_NAME,STATUS)

*  Description:
*     Open the file and determine the noticeboard size. \\
*     Check that the data part was not saved to the file. \\
*     Create the noticeboard. \\
*     Read the definition part of the file into the noticeboard. \\
*     Write the calling process' id to the noticeboard to denote ownership and
*     mark the noticeboard as being valid. \\
*     Close the file.
*
*     If the noticeboard already existed, NBS__SECTIONEXISTED status is
*     returned and the calling process becomes its owner.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name to give the noticeboard (and thus the name of the
*        noticeboard).
*     SAVE_NAME = CHARACTER*(*) (Given)
*        The name of the file from which to read the definition (with a
*        default file type of .NBD)
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*            NBS__CANTOPEN   => Can't open the definition file
*            NBS__DATASAVED  => Noticeboard data was saved to the
*        		       definition file --- cannot restore only defn
*            NBS__CANTREAD   => Can't read the definition file
*            NBS__BADVERSION => Wrong version in definition file
*            NBS__SECTIONEXISTED => Noticeboard of this name already
*        			    existed
*
*                 SS$_*	    => System service codes from SYS$CRMPSC /
*        		       SYS$DELTVA (VMS only)

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     09-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_CLOSE_FILE         Close a noticeboard definition file
*        NBS_OPEN_FILE          Open a noticeboard definition file
*        NBS_READ_FILE          Read a noticeboard definition file
*        NBS_CREATE_SECTION     Create a noticeboard
*        NBS_RELOCATE_ADDRESS   Relocate an individual address
*        NBS_STRIMP             Import a string, converting to upper-case
*     C RTL:
*        GETPID                 Get process ID

*  Prior Requirements:
*     None.

*-
*/
int NBS_RESTORE_DEFINITION ( RW_CHARACTER(name), RW_CHARACTER(save_name),
                             W_INTEGER(status) TRAIL(name) TRAIL(save_name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(save_name)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   board_id	bid;
   FILE	 	*chan;
   item_id	global_base;
   int		file_size;
   int		defn_size;
   int		section_size;
   int		ignore;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Open the noticeboard definition file and, if OK, create the noticeboard. */

      NBS_OPEN_FILE (CF_C_ARG(save_name),&chan,&file_size,&defn_size,
			   &section_size,status CF_TRAIL(save_name));

/* If the file was opened OK, check that it contains only the definition.
   If it contains the data as well then it may have been saved since creation
   and the definition part may contain invalid information. */

      if (OK (*status)) {
	 if (file_size != defn_size)
            {
	    *status = NBS__DATASAVED;
            emsRep( "NBS_RESTORE_DEFINITION_DATASAVED",
              "Data part of noticeboard saved - cannot restore definition",
              status );
            }
	 else {
	    global_base = (item_id) NBS_CREATE_SECTION
	       (CF_C_ARG(name),section_size,status CF_TRAIL(name) );

/* If the section was created OK (note that if it already existed then the
   returned status is NOT OK), read the definition file into it (unmap the
   section if this fails) ... */

	    if (OK (*status)) {
	       NBS_READ_FILE (chan,defn_size,(char *) global_base,status);
	       if (!OK (*status))
		  NBS_UNMAP_SECTION ((char *) global_base,section_size,&ignore);
	    }

/* ... and relocate the board information address so that information relevant
   to this (the owner) process can be filled in. Note that this is done even if
   the section already existed, as long as the returned address is sensible */

	    if ( (OK (*status) || *status == NBS__SECTIONEXISTED) &&
                 ( global_base != ((item_id) -1) ) ) {
	       bid = global_base->board;
	       bid = (board_id) NBS_RELOCATE_ADDRESS
	      			(bid, (int) global_base - ITEM_BASE, YES);
	       bid->pid = getpid ();
	       bid->world_write = world_write;
	       bid->increment_modify = increment_modify;
	       bid->check_modify = check_modify;
	       bid->chan = NIL;
	       bid->global_base = global_base;
	       bid->original_unmapped = NO;
	       NBS_STRIMP (bid->save_name,save_name,MAXFILE);

/* Noticeboard is now valid and can be found. */

	       global_base->vp.valid = YES;
	    }
	 }

/* If the file was opened, close it.	*/

         NBS_CLOSE_FILE (chan);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_RESTORE_NOTICEBOARD

*  Purpose:
*     Restore a noticeboard definition and data from file
*     and create the noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_RESTORE_NOTICEBOARD (NAME,SAVE_NAME,STATUS)

*  Description:
*     Open the file and determine the noticeboard size. \\
*     Create the noticeboard. \\
*     Read the file into the noticeboard. \\
*     Write the calling process' id to the noticeboard to denote ownership and
*     mark the noticeboard as being valid. \\
*     Close the file. \\
*     If the file only contained the definition and not the noticeboard,
*     return a warning status.
*
*     If the noticeboard already existed, NBS__SECTIONEXISTED status is
*     returned and the calling process becomes its owner.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name to give the noticeboard.
*     SAVE_NAME = CHARACTER*(*) (Given)
*        The name of the file from which to read the definition (with a
*        default file type of .NBD)
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*            NBS__CANTOPEN   => Can't open the definition file
*            NBS__CANTREAD   => Can't read the definition file
*            NBS__BADVERSION => Wrong version in definition file
*            NBS__SECTIONEXISTED => Noticeboard of this name already
*        		       existed
*            NBS__DATANOTSAVED => Noticeboard data not saved to the
*        		       definition file, so not restored
*
*                 SS$_*	    => System service codes from SYS$CRMPSC /
*        		       SYS$DELTVA (VMS only)

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     25-Mar-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_CLOSE_FILE         Close a noticeboard definition file
*        NBS_OPEN_FILE          Open a noticeboard definition file
*        NBS_READ_FILE          Read a noticeboard definition file
*        NBS_CREATE_SECTION     Create a noticeboard
*        NBS_RELOCATE_ADDRESS   Relocate an individual address
*        NBS_STRIMP             Import a string, converting to upper-case
*        NBS_CREATE_SECTION     Create a noticeboard
*        NBS_UNMAP_SECTION        Unmap an existing noticeboard
*     C RTL:
*        GETPID                 Get process ID

*  Prior Requirements:
*     None.

*-
*/
int NBS_RESTORE_NOTICEBOARD ( RW_CHARACTER(name), RW_CHARACTER(save_name),
                              W_INTEGER(status) TRAIL(name) TRAIL(save_name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(save_name)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   board_id	bid;
   FILE	 	*chan;
   item_id	global_base;
   int		file_size;
   int		defn_size;
   int		section_size;
   int		ignore;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Open the noticeboard definition file and, if OK, create the noticeboard. */

      NBS_OPEN_FILE (CF_C_ARG(save_name),&chan,&file_size,&defn_size,
		     &section_size,status CF_TRAIL(save_name) );
      if (OK (*status)) {
	 global_base = (item_id) NBS_CREATE_SECTION
            (CF_C_ARG(name),section_size,status CF_TRAIL(name) );

/* If the section was created OK (note that if it already existed then the
   returned status is NOT OK), read the definition file into it ... (note
   that if the file contains only the definition then the noticeboard data
   will not be read. Also note that if this fails, the section it unmapped) */

	 if (OK (*status)) {
            NBS_READ_FILE (chan,file_size,(char *) global_base,status);
	    if (!OK (*status))
	       NBS_UNMAP_SECTION ((char *) global_base,section_size,&ignore);
	 }

/* ... and relocate the board information address so that information relevant
   to this (the owner) process can be filled in. Note that this is done even if
   the section already existed, as long as address is sensible */

	 if ( (OK (*status) || *status == NBS__SECTIONEXISTED) &&
              ( global_base != ((item_id) -1) ) ) {
	    bid = global_base->board;
	    bid = (board_id) NBS_RELOCATE_ADDRESS
				(bid, (int) global_base - ITEM_BASE, YES);
	    bid->pid = getpid ();
	    bid->world_write = world_write;
	    bid->increment_modify = increment_modify;
	    bid->check_modify = check_modify;
	    bid->chan = NIL;
	    bid->global_base = global_base;
	    bid->original_unmapped = NO;
	    NBS_STRIMP (bid->save_name,save_name,MAXFILE);

/* Noticeboard is now valid and can be found. */

	    global_base->vp.valid = YES;

/* Check that the file did indeed contain the noticeboard data. If it did not,
   return a warning status. */

	    if (file_size != section_size)
               {
	       *status = NBS__DATANOTSAVED;
               emsRep( "NBS_RESTORE_NB_DATANOTSAVED",
                 "Data part of noticeboard not saved - cannot restore it",
                 status );
               }

	 }

/* If the file was opened, close it.	*/

         NBS_CLOSE_FILE (chan);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_SAVE_NOTICEBOARD

*  Purpose:
*     Save a noticeboard to its noticeboard definition file

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_SAVE_NOTICEBOARD (ID,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Check that the caller owns the noticeboard. \\
*     Check that the noticeboard was restored from a file which has room for
*     the noticeboard data. \\
*     Check that the file is open for write access. \\
*     Write the noticeboard data to it.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of noticeboard or of any item in
*         the noticeboard whose data is to be saved.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*            NBS__NILID	    => NIL ID
*            NBS__CANTOPEN   => Can't open the definition file
*            NBS__NOTOWNER   => Caller does not own the noticeboard
*            NBS__DATANOTRESTORED => Noticeboard data was not restored
*        		       from the definition file (so can't save it)
*            NBS__CANTOPEN   => Can't open the definition file
*            NBS__CANTWRITE  => Can't write the definition file

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     25-Mar-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_UPDATE_FILE        Write data to a noticeboard definition file
*        NBS_OPEN_WRITE         Open a noticeboard definition file
*                               for write access

*  Prior Requirements:
*     None.

*-
*/
int NBS_SAVE_NOTICEBOARD ( R_INTEGER(id), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that the caller owns the noticeboard.*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_SAVE_NB_NILID", "NIL item ID", status );
         }
      else if (nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_SAVE_NB_NOTOWN", "Not owner of noticeboard", status );
         }

/* Check that the noticeboard data was restored from a file in the first
   place. */

      else if (tid->board->file_size != tid->board->section_size)
         {
	 *status = NBS__DATANOTRESTORED;
         emsRep( "NBS_SAVE_NB_DATANOTSAVED",
               "Data was not restored from noticeboard file - cannot save it",
               status );
         }

/* Check that the file is open for write access (and save the channel number
   since the file will not be closed). */

      else {
	 if (tid->board->chan == NIL)
	    NBS_OPEN_WRITE (tid->board->save_name,&tid->board->chan,status);

/* Write the noticeboard data to the file. Note that the file is left open
   to minimise the overheads on subsequent calls. Note also that we ensure
   that the "noticeboard valid" flag is set false in the saved file. */

	 if (OK (*status)) {
	    tid->board->global_base->vp.valid = NO;
	    NBS_UPDATE_FILE (tid->board->chan,
			     tid->board->global_base,
			     tid->board->section_size,
			     status);
	    tid->board->global_base->vp.valid = YES;
	 }
      }
   }
   return (*status);
}

/*
*  Section name:
*     NBS_FIND

*  Purpose:
*     Routines that find noticeboards and items within noticeboards

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_SAVE_NOTICEBOARD (ID,STATUS)

*  Description:
*     Items can be found either by name or by position. When a noticeboard
*     is found a private copy of its item descriptors is made in which all
*     non NIL pointers are relocated by the virtual address of the start of
*     the relevant noticeboard. Thus, once a noticeboard has been found, all
*     information pertaining to it can in fact be accessed using standard C
*     structures.
*/

/*
*+
*  Name:
*     NBS_FIND_NOTICEBOARD

*  Purpose:
*     Find a named noticeboard and return an identifier to it

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_FIND_NOTICEBOARD (NAME,ID,STATUS)

*  Description:
*     Map the noticeboard of the given name. \\
*     Allocate an item descriptor and relocate it so that the version number
*     can be checked and the definition size determined. \\
*     Free that item descriptor and allocate a block of memory big enough for
*     the entire definition. \\
*     Copy the definition part to this memory and relocate it so that all
*     pointers are once again virtual memory addresses. \\
*     Get the process' id for future checking against the owner's process id. \\
*     If this is the owner process and it hasn't already been done, unmap the
*     copy mapped earlier.
*
*     Note that it is somewhat wasteful to build a complete copy of the
*     noticeboard definition in private memory when in fact only the pointers
*     must have private versions. A later version of the software should take
*     copies only of the pointers but this requires the pointer space to be
*     allocated separately during the definition phase --- in the current
*     implementation the pointer space is not contiguous; pointers are mixed up
*     with fixed information, shape information and board information.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        The name of the noticeboard which is to be found.
*     ID = INTEGER (Returned)
*        Identifier of the top-level of the noticeboard.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__SECTIONNOTFOUND => No section called NAME existed
*        	NBS__TIMEOUT    => Timeout awaiting valid noticeboard
*        	NBS__NOMOREROOM => Failed to allocate private memory area
*        	NBS__BADVERSION => Wrong version in noticeboard
*
*             	SS$_*	    	=> System service codes from SYS$CRMPSC /
*        			   SYS$DELTVA

*  Copyright:
*     Copyright (C) 1990, 1993-1994 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     06-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*      8-Mar-1994 (DJA):
*        Updated handling of external pointers using EXPORT_POINTER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_MAP_SECTION        Map an existing noticeboard
*        NBS_RELOCATE_ITEM        Relocate addresses in item descriptor
*        NBS_UNMAP_SECTION        Unmap an existing noticeboard
*     C RTL:
*        GETPID                 Get process ID
*        MALLOC                 Allocate dynamic memory

*  Prior Requirements:
*     None.

*-
*/
int NBS_FIND_NOTICEBOARD ( RW_CHARACTER(name), item_id *id,
                           W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   int		defn_size;
   item_id	global_base;
   int		count;
   int		ignore;
   item_id      locid = NIL;            /* Initialisation ensures that a NIL */
                                        /* is returned even if routine fails */

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Map the section and, if OK, await it becoming valid (it may be being created
   at this instant by another process). */

      global_base = (item_id) NBS_MAP_SECTION
          (CF_C_ARG(name),status CF_TRAIL(name) );

      if (OK (*status)) {
	 count = 0;
	 while (global_base->vp.valid == NO && count++ < timeout_count)
            NBS_SLEEPMS ( timeout_interval );
	 if (global_base->vp.valid == NO)
            {
	    *status = NBS__TIMEOUT;
            emsRep( "NBS_FIND_NB_TIMEOUT",
              "Time out finding noticeboard", status );
            }

/* If didn't time out, build a private relocated copy of the top-level item
   descriptor (it is guaranteed that the first part of the noticeboard
   definition is the top-level item descriptor).    */

         if (OK (*status)) {
	    locid = ITEM_ALLOC ();
	    if (locid == NIL)
               {
	       *status = NBS__NOMOREROOM;
               emsRep( "NBS_FIND_NB_NOMOREROOM",
                 "Couldn't get memory - increase MAX_DEFN_SIZE when defining",
                 status );
               }
	    else {
	       *locid = *global_base;
	       NBS_RELOCATE_ITEM (locid,(int) global_base - ITEM_BASE,
				      (int) global_base - ITEM_BASE,
	    		     	      (int) global_base - DATA_BASE, YES);

/* Check that the version number held in the noticeboard (which is the version
   of the software that created the noticeboard) matches the software version.*/

	       if (locid->board->version != NBSVERSION) {
                  ITEM_FREE (locid);
	          *status = NBS__BADVERSION;
                  emsRep( "NBS_FIND_NB_BADVER",
                    "Noticeboard or definition file had wrong version", status );
	       }

/* We now know the size of the noticeboard definition part (it is part of the
   information held in the board information), so deallocate the top-level item
   descriptor and allocate enough space for the entire definition part of the
   noticeboard.	*/

	       else {
	          defn_size = locid->board->defn_size;
                  ITEM_FREE (locid);
	          locid = (item_id) malloc (defn_size);
	          if (locid == NIL)
                     {
	             *status = NBS__NOMOREROOM;
                     emsRep( "NBS_FIND_NB_NOMOREROOM",
                      "Couldn't get memory - increase MAX_DEFN_SIZE when defining",
                      status );
                     }

/* Copy the definition from the noticeboard to the private memory area and
   relocate addresses so that all pointers are virtual memory addresses again.
   All information in this noticeboard can now be accessed directly using C
   language constructs.	*/

	          else {
		     _chmove (defn_size,(char *) global_base,(char *) locid);
		     NBS_RELOCATE_POINTERS (locid,(int) locid -ITEM_BASE,
					        (int)global_base-ITEM_BASE,
				    	        (int)global_base-DATA_BASE,YES);

/* The noticeboard VALID flag (which is in union with the PARENT pointer) will
   have been relocated. Explicitly set it to NIL - in the private copy it is
   once again regarded as a PARENT pointer). */

		     locid->vp.parent = NIL;

/* Save the noticeboard start address so that it can later be lost if
   necessary. */

		     locid->gs.global_base = global_base;

/* Save this process' id for future checking against owner process ids. */

		     nbs_gl_pid = getpid ();

/* The section below is a remnant from version 2.3, and is need only for the
   VMS version. NBS logic elsewhere ensures that the UNIX version never tries
   to map the same section twice, as this would fail. */

/* #ifdef vms */

/* If this process is the owner and if this is the first time that this
   noticeboard has been found, it has the noticeboard mapped twice. Unmap the
   first one and change the base address to be that of the second. */

		     if (locid->board->pid == nbs_gl_pid &&
				!locid->board->original_unmapped) {
			NBS_UNMAP_SECTION ((char *) locid->board->global_base,
					   locid->board->section_size,&ignore);
			locid->board->global_base = global_base;
			locid->board->original_unmapped = YES;
		     }

/* #endif */

	          }
	       }
	    }
         }

/* If the section was mapped but a subsequent error has occurred, unmap it. */

	 if (!OK (*status))
	   {
	     NBS_UNMAP_SECTION ((char *) global_base,
					locid->board->section_size,&ignore);
	   }
      }

/*   Export the pointer */
      EXPORT_POINTER(locid,id);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_FIND_ITEM

*  Purpose:
*     Find an item with a specified name contained in a structure
*     associated with a specified identifier and return the located item's
*     ID

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_FIND_ITEM (ENVID,NAME,ID,STATUS)

*  Description:
*     Check that the environment ID is not NIL and does not pertain to a
*     primitive item. \\
*     Search for an item of the required name (don't assume that they are in
*     any particular order).
*
*     The searching is performed using a binary search. This means that
*     it takes roughly the same time to find all the items --- best case
*     performance is degraded but worst case performance is substantially
*     improved.

*  Arguments:
*     ENVID = INTEGER (Given)
*        Identifier of the parent of the item which is to be found.
*     NAME = CHARACTER*(*) (Given)
*        The name of the item to be found.
*     ID = INTEGER (Returned)
*        Identifier of the found item (zero if not found).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__PRIMITIVE	  => Parent is primitive
*        	NBS__ITEMNOTFOUND => No item of this name exists

*  Copyright:
*     Copyright (C) 1987, 1993-1994 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     22-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*      8-Mar-1994 (DJA):
*        Updated handling of external pointers using EXPORT_POINTER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP         	Import a string, converting to upper-case
*     C RTL:
*        STRNCMP        	Compare one string with another up to
*                               specified limit

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_FIND_ITEM ( R_INTEGER(envid), RW_CHARACTER(name), item_id *id,
                    W_INTEGER(status) TRAIL(name) )
{
  GENPTR_INTEGER(envid)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

/* External function declarations */

/* Local variable declarations */

   item_id	tenvid;
   item_id	firstid;
   char		tname[MAXNAME];
   int		first;
   int		last;
   item_id      locid = NIL;            /* Pointer to be exported.Set to NIL */
                                        /* so that failure returns NIL item */
   int		mid;
   int		cmp;
   int		i;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the parent ID.	*/

      NBS_PTRIMP (int,tenvid,envid);

/* Check that the parent ID is not NIL and that the parent is not primitive. */

      if (tenvid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_FIND_ITEM_NILID", "NIL item ID", status );
         }
      else if (tenvid->fixed->primitive)
         {
	 *status = NBS__PRIMITIVE;
         emsRep( "NBS_FIND_ITEM_PRIM", "Item is primitive", status );
         }

/* Search for an item of the required name. Use a binary chop search to
   cut down on string comparisons.	*/

      else {
	 NBS_STRIMP (tname,name,MAXNAME);
	 firstid = tenvid->heir;
      	 first = 1;
         last = tenvid->fixed->children;
	 cmp = 1;
	 while (cmp && first <= last) {
	    mid = (first + last) / 2;
	    for (locid = firstid, i = first; i < mid; locid = locid->sibling, i++);
	    if ((cmp = strncmp (locid->fixed->name,tname,MAXNAME)) < 0) {
	       firstid = locid->sibling;
	       first = mid + 1;
	    }
	    else if (cmp > 0)
	       last = mid - 1;
	 }

/* A non-zero value for CMP means that the item was not found.	*/

	 if (cmp)
	    locid = NIL;

/* If not found, return failure status.	*/

	 if (locid == NIL)
            {
            *status = NBS__ITEMNOTFOUND;
            emsRep( "NBS_FIND_ITEM_NOTFOUND", "Item not found", status );
            }
	 else
 	    tenvid->da.accessed++;
      }

/*   Export the pointer */
      EXPORT_POINTER(locid,id);

   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_FIND_NTH_ITEM

*  Purpose:
*     Find the Nth item contained in a structure
*     associated with a specified identifier and return the located item's ID
*     ID

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_FIND_NTH_ITEM (ENVID,POSN,ID,STATUS)

*  Description:
*     Check that the environment ID is not NIL and does not pertain to a
*     primitive item. \\
*     Extract the Nth item.

*  Arguments:
*     ENVID = INTEGER (Given)
*        Identifier of the parent of the item which is to be found.
*     POSN = INTEGER (Given)
*        Number of item to find (first item is item #1).
*     ID = INTEGER (Returned)
*        Identifier of the found item (zero if not found).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__PRIMITIVE	  => Parent is primitive
*        	NBS__ITEMNOTFOUND => No item of this name exists

*  Copyright:
*     Copyright (C) 1987, 1993-1994 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*      8-Mar-1994 (DJA):
*        Updated handling of external pointers using EXPORT_POINTER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_FIND_NTH_ITEM ( R_INTEGER(envid), R_INTEGER(posn),
                        item_id *id, W_INTEGER(status) )
{
  GENPTR_INTEGER(envid)
  GENPTR_INTEGER(posn)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id      locid = NIL;            /* Pointer to be exported.Set to NIL */
                                        /* so that failure returns NIL item */
   item_id	tenvid;
   int		tposn;
   int		i;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the parent ID and item position.	*/

      NBS_PTRIMP (int,tenvid,envid);
      NBS_INTIMP (&tposn,posn);

/* Check that the parent ID is not NIL and that the parent is not primitive. */

      if (tenvid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_FIND_NTH_ITEM_NILID", "NIL item ID", status );
         }
      else if (tenvid->fixed->primitive)
         {
	 *status = NBS__PRIMITIVE;
         emsRep( "NBS_FIND_NTH_ITEM_PRIM", "Item is primitive", status );
         }

/* Search for the item in the required position. */

      else {
	 for (locid  = tenvid->heir, i = 1;
	      locid != NIL;
	      locid  = locid->sibling, i++)
	    if (i == tposn) break;

/* If not found, return failure status.	*/

	 if (locid == NIL)
            {
            *status = NBS__ITEMNOTFOUND;
            emsRep( "NBS_FIND_NTH_ITEM_NOTFOUND", "Item not found", status );
            }
	 else
 	    tenvid->da.accessed++;
      }

/*   Export the pointer */
      EXPORT_POINTER(locid,id);

   }
   return (*status);
}

/*
*  Section name:
*     NBS_LOSE

*  Purpose:
*     Routines which unmap and free resources associated with noticeboards
*     and the items within them.

*  Language:
*     ANSI C

*  Description:
*     The noticeboard system maintains counts of how many items have been
*     found using NBS_FIND_ITEM and NBS_FIND_NTH_ITEM. The routines in this
*     section can be used to indicate that noticeboards and items are no
*     longer required. When a noticeboard is no longer required, it is marked
*     for deletion. When an item is no longer required, its parent's item
*     count is decremented. Normally, noticeboards and items can only be lost
*     if the count of items found from them is zero (this is to minimise the
*     chance of using an item ID to access a non-existent noticeboard), but
*     this can be overridden.
*/

/*
*+
*  Name:
*     NBS_LOSE_NOTICEBOARD

*  Purpose:
*     Unmap a specified noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_LOSE_NOTICEBOARD (ID,OPTION,STATUS)

*  Description:
*     Check that the ID is indeed a top-level one. \\
*     Check that no items are currently derived from the top level of the
*     noticeboard (not if the "FORCE" option is specified). \\
*     Unmap the noticeboard. \\
*     Free the local copy of the noticeboard definition.
*
*     Calls to this routine should match calls to NBS_FIND_NOTICEBOARD.
*     After calling this routine, all identifiers associated with this
*     noticeboard will be invalid. The noticeboard will only be deleted if no
*     other process has it mapped.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the top-level of the noticeboard.
*     OPTION = CHARACTER*(*) (Given)
*        Option that governs whether to check that there are no identifiers
*        currently derived from this one. Can be abbreviated so long as it
*        remains unambiguous but this is not recommended because new options
*        may be supported in the future. Case is not significant. Possible
*        values are{:}
*
*            FORCE => Unmap the noticeboard regardless of whether there
*        	     identifiers derived from this one.
*           CHECK => Check that no identifiers are derived from this
*                     one (assumed if invalid option is given).
*
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTTOPLEVEL  => ID is not a top-level identifier
*        	NBS__HASIDS	  => Noticeboard has identifiers derived
*        			     from it
*        	SS$_*	          => System service codes from SYS$DELTVA

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     02-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP         Import a string, converting to upper-case
*        NBS_UNMAP_SECTION  Unmap an existing noticeboard
*     C RTL:
*        FREE               Free dynamic memory

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_LOSE_NOTICEBOARD ( R_INTEGER(id), RW_CHARACTER(option),
                           W_INTEGER(status) TRAIL(option) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(option)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;
   char		toption;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID and the option. */

      NBS_PTRIMP (int,tid,id);
      NBS_STRIMP (&toption,option,1);

/* Check that the item ID is not NIL, that it is not primitive, has no IDs
   derived from it (or that the "FORCE" option is specified) and that it is
   indeed associated with a top-level item (ie a noticeboard) */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_LOSE_NB_NILID", "NIL item ID", status );
         }
      else if (tid->vp.parent != NIL)
         {
         *status = NBS__NOTTOPLEVEL;
         emsRep( "NBS_LOSE_NB_NOTTOPLEV",
           "Item is not top-level (ie. not a noticeboard) - cannot lose it", status );
         }
      else if (!tid->fixed->primitive && tid->da.accessed > 0 && toption != 'F')
         {
         *status = NBS__HASIDS;
         emsRep( "NBS_LOSE_NB_HASIDS",
           "Noticeboard has items derived from it - cannot lose it", status );
         }

/* Unmap the section and delete the local copy of the noticeboard definition.*/

      else {
         NBS_UNMAP_SECTION ((char *) tid->gs.global_base,
				     tid->board->section_size,status);
	 if (OK (*status))
	    ITEM_FREE (tid);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_LOSE_ITEM

*  Purpose:
*     Declare an intention never again to use a specified item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_LOSE_ITEM (ID,OPTION,STATUS)

*  Description:
*     Check that the ID is not a top-level one. \\
*     Check that at least one item is derived from this item's parent. \\
*     Check that no items are currently derived from this item (not if the
*     "FORCE" option is specified). \\
*     Decrement the parent's count of derived items.
*
*     Calls to this routine should match calls to NBS_FIND_ITEM /
*     NBS_FIND_NTH_ITEM. After calling it, this identifier should not
*     be used again (even though it is in fact still valid until
*     NBS_LOSE_NOTICEBOARD is called).

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item which is to be lost.
*     OPTION = CHARACTER*(*) (Given)
*        Option that governs whether to check that there are no identifiers
*        currently derived from this one. Can be abbreviated so long as it
*        remains unambiguous but this is not recommended because new options
*        may be supported in the future. Case is not significant. Possible
*        values are{:}
*
*            FORCE => Unmap the item regardless of whether there
*                     identifiers derived from this one.
*           CHECK => Check that no identifiers are derived from this
*        	     one (assumed if invalid option is given).
*
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTTOPLEVEL  => ID is not a top-level identifier
*        	NBS__NEVERFOUND   => Item was never found (or more items
*        			     have been lost than were found)
*        	NBS__HASIDS	  => Noticeboard has identifiers derived
*        			     from it
*        	SS$_*	          => System service codes from SYS$DELTVA

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     02-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated FORTRAN string handling. Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP         Import a string, converting to upper-case

*  Prior Requirements:
*     NBS_FIND_ITEM must have been called.

*-
*/
int NBS_LOSE_ITEM ( R_INTEGER(id), RW_CHARACTER(option),
                    W_INTEGER(status) TRAIL(option) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(option)
  GENPTR_INTEGER(status)

/* External function declarations */

/* Local variable declarations */

   item_id	tid;
   char		toption;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID and the option. */

      NBS_PTRIMP (int,tid,id);
      NBS_STRIMP (&toption,option,1);

/* Check that the item ID is not NIL, that it has no IDs derived from it (or
   that the "FORCE" option is specified), that it is not associated with a
   top-level item (ie a noticeboard) and that the parent's count of items
   derived from it is greater than zero. */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_LOSE_ITEM_NILID", "NIL item ID", status );
         }
      else if (tid->vp.parent == NIL)
         {
         *status = NBS__TOPLEVEL;
         emsRep( "NBS_LOSE_ITEM_TOPLEV",
            "Item is top-level (ie noticeboard) - cannot lose it", status );
         }
      else if (tid->vp.parent->da.accessed <= 0)
         {
	 *status = NBS__NEVERFOUND;
         emsRep( "NBS_LOSE_ITEM_NEVERFOUND",
           "Parent has no items derived from it - cannot lose it", status );
         }
      else if (!tid->fixed->primitive && tid->da.accessed > 0 && toption != 'F')
         {
         *status = NBS__HASIDS;
         emsRep( "NBS_LOSE_ITEM_HASIDS",
           "Item has items derived from it - cannot lose it", status );
         }

/* Decrement the parent's count of items derived from it. */

      else
	 tid->vp.parent->da.accessed--;
   }
   return (*status);
}

/*
*  Section Name:
*     NBS_PUT

*  Purpose:
*     Routines that write information to a noticeboard

*  Language:
*     ANSI C

*  Description:
*     This can normally only be done by the owner of the noticeboard. The only
*     things which can be written are shape information and data. The item's
*     modified count is incremented both before and after each write using the
*     ADD_INTERLOCKED instruction. The noticeboard's modified count is
*     incremented after each write. These modified counts allow other processes
*     to monitor the noticeboard for changes.
*
*     When writing data to an item, it is possible to specify the offset
*     at which the data is to start. The actual size of the item's data
*     is the high-water mark of all the data that has been written to the
*     item. For example, if 10 bytes are written at offset 0, the size is
*     10 bytes, but if then 100 bytes are written at offset 4000, the size
*     is 4100 bytes, even though bytes 11 to 4000 have not been written.
*     It is guaranteed that all such unwritten bytes are zero. It is
*     occasionally necessary to adjust the actual size of an item's data
*     and there is a routine explicitly to do this. For example, the size
*     of the above item could be adjusted back to 10 bytes. Note that if
*     1 byte were now written at an offset of 10000, the item size would
*     now be 10001 bytes and the 100 bytes at offset 4000 would retain the
*     values that they were given earlier (they would not be reset to zero).
*
*     There are two global parameters which can be altered using the
*     NBS_TUNE_NOTICEBOARD routine and which affect the behaviour of these
*     routines:
*
*     1. WORLD_WRITE is FALSE by default and this prevents processes other
*        than the noticeboard's owner from writing to it. If set TRUE, this
*        process will be allowed to write to the noticeboard even if it does
*        not own it. If set TRUE then it is possible for a reader to read data
*        that is currently varying, so only do this if you know what you are
*        doing. All processes can always access all noticeboard's directly if
*        they do so via pointer --- such access is not affected by the
*        WORLD_WRITE flag.
*
*     2. INCREMENT_MODIFY is TRUE by default and this causes the item's
*        modified count to be incremented before and after the update and the
*        noticeboard's modified count to be incremented after the update. If
*        set FALSE, neither count is altered. Clearly, if this is done, other
*        processes monitoring the modified counts will not realise that
*        noticeboard data is changing.
*
*     There is also a lower-level routine that must only be used by people who
*     really know what they are doing. It increments either the noticeboard
*     modified count or a primitive item's modified count. This {\em must} be
*     called according to the same conventions as are applied in NBS_PUT_VALUE.
*     If this is not done, the whole system is subverted.
*
*     Finally, there is a routine which specifies a user-supplied routine to be
*     called whenever an item's shape, data, size or modified count is altered.
*     This routine is called in the context of the process which alters the
*     noticeboard --- there are no facilities for notifying directly any other
*     processes (although the user-supplied routine may of course do this
*     itself).
*/

/*
*+
*  Name:
*     NBS_PUT_VALUE

*  Purpose:
*     Put a byte array into a slice of a primitive item
*     associated with a specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_PUT_VALUE (ID,OFFSET,NBYTES,BYTE_ARRAY,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Check that the caller owns the noticeboard (or WORLD_WRITE is TRUE). \\
*     Check that the offset into the data is not negative. \\
*     Check that the item is large enough to accept all of the supplied
*     values. \\
*     Increment the item's modified count. \\
*     Update the item size (maintain a high-water mark). \\
*     Copy the values to the noticeboard. \\
*     Increment the item's modified count again. \\
*     Increment the noticeboard modified count.
*
*     The item and noticeboard modified counts will not be incremented if
*     INCREMENT_MODIFY is FALSE.
*
*     Note that this routine only alters the specified number of bytes starting
*     at the specified offset --- all bytes are initially zero and bytes above
*     and below those that are being altered will not be affected. The actual
*     size of the item will be adjusted upwards if the new data extends past the
*     previous end of the data but the size of the item cannot be decreased by
*     this routine. To alter the size of an item, use the NBS_PUT_SIZE routine.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item which the value is to be put.
*     OFFSET = INTEGER (Given)
*        Byte offset into item data.
*     NBYTES = INTEGER (Given)
*        Number of bytes to put.
*     BYTES = BYTE(*) (Given)
*        Bytes to be put.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard
*        	NBS__BADOFFSET	  => Negative offset specified
*        	NBS__TOOMANYBYTES => Not room to put all the data

*  Notes:
*     In versions prior to {V2.4.0} this routine could be used to write
*     character strings on the VAX using the %REF() mechanism to pass
*     the character data. A new routine, NBS_PUT_CVALUE, has been provided
*     to remove the need for %REF and make the method portable.

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     16-Feb-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_PUT_VALUE ( R_INTEGER(id), R_INTEGER(offset), R_INTEGER(nbytes),
                    RW_BYTE_ARRAY(byte_array), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(offset)
  GENPTR_INTEGER(nbytes)
  GENPTR_BYTE_ARRAY(byte_array)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;
   int		toffset;
   int		tnbytes;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID, the offset and the number of bytes to copy.	*/
      NBS_PTRIMP (int,tid,id);
      NBS_INTIMP (&toffset,offset);
      NBS_INTIMP (&tnbytes,nbytes);

/* Check that the item ID is not NIL, that the item is primitive, that the
   caller owns the noticeboard (or WORLD_WRITE is TRUE), that the offset is
   not negative and that the number of bytes to copy is not greater than the
   allocated space.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_PUT_VALUE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_PUT_VALUE_NOTPRIM", "Item is not primitive", status );
         }
      else if (!world_write && !tid->board->world_write &&
						nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_PUT_VALUE_NOTOWN",
              "Non-owner attempted to alter noticeboard", status );
         }
      else if (toffset < 0)
         {
         *status = NBS__BADOFFSET;
         emsRep( "NBS_PUT_VALUE_BADOFF",
                    "Offset is less than zero", status );
         }
      else if (toffset + tnbytes > tid->fixed->maxbytes)
         {
         *status = NBS__TOOMANYBYTES;
         emsRep( "NBS_PUT_VALUE_TOOMANYBYTES",
                 "More bytes than maximum allowed", status );
         }

/* If INCREMENT_MODIFY is TRUE: increment the item's modified count (which
   should make it odd), then adjust the item size, copy the data and increment
   the item's modified count again (which should make it even again). Finally
   increment the noticeboard's modified count. Call the trigger routine if
   enabled. */

      else if (increment_modify || tid->board->increment_modify) {
	 _add_interlocked (&tid->fixed->modified);
       	 tid->fixed->actbytes = MAX (tid->fixed->actbytes,
					   toffset + tnbytes);
	 _chmove (tnbytes,byte_array,(char *)tid->da.data + toffset);
	 _add_interlocked (&tid->fixed->modified);
         _add_interlocked (&tid->board->modified);
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }

/* If INCREMENT_MODIFY is FALSE: adjust the item size and copy the data. Call
   the trigger routine if enabled. */

      else {
    	 tid->fixed->actbytes = MAX (tid->fixed->actbytes,
	 				   toffset + tnbytes);
	 _chmove (tnbytes,byte_array,(char *)tid->da.data + toffset);
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_PUT_CVALUE

*  Purpose:
*     Put a character string into a slice of a primitive item
*     associated with a specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_PUT_CVALUE (ID,OFFSET,STRING,STATUS)

*  Description:
*     Simply extract the string data pointer and length and pass this
*     information to NBS_PUT_VALUE.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item which the value is to be put.
*     OFFSET = INTEGER (Given)
*        Byte offset into item data.
*     STRING = CHARACTER*(*) (Given)
*        The string to be put.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard
*        	NBS__BADOFFSET	  => Negative offset specified
*        	NBS__TOOMANYBYTES => Not room to put all the data

*  Notes:
*     Replaces NBS_PUT_VALUE when writing character strings to noticeboards.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     05-Mar-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_PUT_VALUE        Write a byte array to an item

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_PUT_CVALUE ( R_INTEGER(id), R_INTEGER(offset), RW_CHARACTER(string),
                     W_INTEGER(status) TRAIL(string) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(offset)
  GENPTR_CHARACTER(string)
  GENPTR_INTEGER(status)

#ifdef c_string
  int len;
#endif

IF_OK {
#ifdef c_string
  len = strlen(string);
  NBS_PUT_VALUE( id, offset, len, string, status );
#else
  NBS_PUT_VALUE( id, offset, &string_length, (F77_BYTE_TYPE *) string, status );
#endif
  }

return (*status);
}

/*
*+
*  Name:
*     NBS_PUT_SHAPE

*  Purpose:
*     Put a new shape to a primitive item
*     associated with a specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_PUT_SHAPE (ID,NDIMS,DIMS,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Check that the caller owns the noticeboard (or WORLD_WRITE is TRUE). \\
*     Check that the item has enough potential dimensions to accept all of the
*     supplied dimensions. \\
*     Increment the modified count for this item. \\
*     Copy the dimensions to the noticeboard. \\
*     Increment the modified count once more. \\
*     Increment the noticeboard modified count.
*
*     The item and noticeboard modified counts will not be incremented if
*     INCREMENT_MODIFY is FALSE.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item which the shape is to be put.
*     NDIM = INTEGER (Given)
*        Number of dimensions to be put.
*     DIMS = INTEGER(*) (Given)
*        Dimensions to be put.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard
*        	NBS__TOOMANYDIMS  => NDIMS is greater than item max dims

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     16-Feb-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_PUT_SHAPE ( R_INTEGER(id), R_INTEGER(ndims), RW_INTEGER_ARRAY(dims),
                    W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(ndims)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;
   int		tndims;
   int		i;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID and the number of dimensions to copy.	*/

      NBS_PTRIMP (int,tid,id);
      NBS_INTIMP (&tndims,ndims);

/* Check that the item ID is not NIL, that the item is primitive, that the
   caller owns the noticeboard (or WORLD_WRITE is TRUE) and that the number of
   dimensions to copy is not greater than the allocated dimension space. */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_PUT_SHAPE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_PUT_SHAPE_NOTPRIM", "Item is not primitive", status );
         }
      else if (!world_write && !tid->board->world_write &&
						nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_PUT_SHAPE_NOTOWN",
              "Non-owner attempted to alter noticeboard", status );
         }
      else if (tndims > tid->fixed->maxdims)
         {
         *status = NBS__TOOMANYDIMS;
         emsRep( "NBS_PUT_SHAPE_TOOMANYDIMS",
                 "More dimensions than maximum allowed", status );
         }

/* If INCREMENT_MODIFY is TRUE: increment the item's modified count (which
   should make it odd), then copy the dimensions and increment the item's
   modified count again (which should make it even again).  Finally increment
   the noticeboard's modified count. Call the trigger routine if enabled. */

      else if (increment_modify || tid->board->increment_modify) {
	 _add_interlocked (&tid->fixed->modified);
	 tid->fixed->actdims = tndims;
	 for (i=0; i<tndims; i++)
    	    tid->gs.shape[i] = dims[i];
	 _add_interlocked (&tid->fixed->modified);
	 _add_interlocked (&tid->board->modified);
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }

/* If INCREMENT_MODIFY is FALSE: adjust the dimensions and copy the data. Call
   the trigger routine if enabled. */

      else {
	 tid->fixed->actdims = tndims;
	 for (i=0; i<tndims; i++)
    	    tid->gs.shape[i] = dims[i];
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_PUT_SIZE

*  Purpose:
*     Put a new size to a primitive item
*     associated with a specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_PUT_SIZE (ID,NBYTES,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Check that the caller owns the noticeboard (or WORLD_WRITE is TRUE). \\
*     Check that the item is large enough to be the proposed size. \\
*     Increment the item's modified count. \\
*     Update the internal record of the item's size. \\
*     Increment the item's modified count again. \\
*     Increment the noticeboard modified count.
*
*     The item and noticeboard modified counts will not be incremented if
*     INCREMENT_MODIFY is FALSE.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item which the shape is to be put.
*     NBYTES = INTEGER (Given)
*        new item size in bytes.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard
*        	NBS__TOOMANYBYTES => NBYTES is greater than item size

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_PUT_SIZE ( R_INTEGER(id), R_INTEGER(nbytes), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(nbytes)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;
   int		tnbytes;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID and the new size.	*/

      NBS_PTRIMP (int,tid,id);
      NBS_INTIMP (&tnbytes,nbytes);

/* Check that the item ID is not NIL, that the item is primitive, that the
   caller owns the noticeboard (or WORLD_WRITE is TRUE) and that the new size
   is not greater than the maximum size for this item.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_PUT_SIZE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_PUT_SIZE_NOTPRIM", "Item is not primitive", status );
         }
      else if (!world_write && !tid->board->world_write &&
						nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_PUT_SIZE_NOTOWN",
              "Non-owner attempted to alter noticeboard", status );
         }
      else if (tnbytes > tid->fixed->maxbytes)
         {
         *status = NBS__TOOMANYBYTES;
         emsRep( "NBS_PUT_SIZE_TOOMANYBYTES",
                 "More bytes than maximum allowed", status );
         }

/* If INCREMENT_MODIFY is TRUE: increment the item's modified count (which
   should make it odd), then adjust the item size and increment the item's
   modified count again (which should make it even again). Finally increment
   the noticeboard's modified count. Call the trigger routine if enabled. */

      else if (increment_modify || tid->board->increment_modify) {
	 _add_interlocked (&tid->fixed->modified);
	 tid->fixed->actbytes = tnbytes;
	 _add_interlocked (&tid->fixed->modified);
	 _add_interlocked (&tid->board->modified);
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }

/* If INCREMENT_MODIFY is FALSE: adjust the item size. Call the trigger routine
   if enabled. */

      else {
	 tid->fixed->actbytes = tnbytes;
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_INC_MODIFIED

*  Purpose:
*     Increment the noticeboard modified count or an item modified count
*     depending on whether this is a structured or primitive item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_INC_MODIFIED (ID,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Check that the caller owns the noticeboard (or WORLD_WRITE is TRUE). \\
*     If the item is structured increment the noticeboard modified count. \\
*     If the item is primitive increment the item's modified count.
*
*     Note that this is a very dangerous routine when called on behalf of
*     primitive items. Calls to it {\em must} be paired. Any reader of an item
*     will time out if the modified count for the item being read is an odd
*     number.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item whose modified count is be incremented.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_INC_MODIFIED ( R_INTEGER(id), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that the caller owns the noticeboard
   (or WORLD_WRITE is TRUE).	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_INC_MODIFIED_NILID", "NIL item ID", status );
         }
      else if (!world_write && !tid->board->world_write &&
						nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_INC_MODIFIED_NOTOWN",
              "Non-owner attempted to alter noticeboard", status );
         }

/* If primitive, increment the item's modified count. Call the trigger routine
   if enabled. */

      else if (tid->fixed->primitive) {
         _add_interlocked (&tid->fixed->modified);
         if (tid->trigger)
            (*tid->trigger) (id,status);
      }

/* If structured, increment the noticeboard modified count. This does not call
   the trigger routine. */

      else
         _add_interlocked (&tid->board->modified);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_PUT_TRIGGER

*  Purpose:
*     Specify a routine to be called whenever a primitive item is updated

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_PUT_TRIGGER (ID,TRIGGER,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Check that the caller owns the noticeboard (or WORLD_WRITE is TRUE). \\
*     Copy the address of the routine to be called on item update.
*
*     In this context "update" means any change to the item's shape, data, size
*     or modified count. The supplied routine is called with the following
*     calling sequence:
*
*     TRIGGER (ID,STATUS)
*
*     where ID is the identifier of the item which has been altered and STATUS
*     is as usual. Any bad status returned by the trigger routine will be
*     passed back to the caller.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for which a trigger routine is to be specified.
*     TRIGGER = EXTERNAL (Given)
*        The address of the routine to call whenever the item is updated.
*        From FORTRAN, declare it as EXTERNAL. Pass zero (requires %VAL(0)
*        from FORTRAN) to disable the facility).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTOWNER 	  => Caller does not own the noticeboard

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     06-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_PUT_TRIGGER ( R_INTEGER(id), int (*trigger)(), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID. */

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL, that the item is primitive and that the
   caller owns the noticeboard (or WORLD_WRITE is TRUE). */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_PUT_TRIGGER_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_PUT_TRIGGER_NOTPRIM", "Item is not primitive", status );
         }
      else if (!world_write && !tid->board->world_write &&
						nbs_gl_pid != tid->board->pid)
         {
         *status = NBS__NOTOWNER;
         emsRep( "NBS_PUT_TRIGGER_NOTOWN",
              "Non-owner attempted to alter noticeboard", status );
         }

/* Update the internal record of the trigger routine.	*/

      else
	 tid->trigger = trigger;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET

*  Purpose:
*     Routines that read information from a noticeboard

*  Language:
*     ANSI C

*  Description:
*     Normally this is a very straightforward operation, but when reading
*     shape information and data it is necessary to check that the owner of
*     the information has not altered it whilst it is being read.
*
*     There are three global parameters which can be altered using the
*     NBS_TUNE_NOTICEBOARD routine and which affect the behaviour of the
*     shape and data reading routines:
*
*     1. CHECK_MODIFY is TRUE by default and this means that the item's
*        modified count is checked both before and after reading the data. The
*        data is read repeatedly until the value of this count is even and
*        unchanging or until a timeout occurs. If set FALSE, no such checks are
*        made and no timeout can occur.
*
*     2. TIMEOUT_COUNT is 100 by default and is the maximum number of times
*        that the data reading will be tried.
*
*     3. TIMEOUT_INTERVAL is 100 by default and is the delay in milliseconds
*        between tries.

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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*
*+
*  Name:
*     NBS_GET_VALUE

*  Purpose:
*     Get a byte array from a slice of a primitive item
*     associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_VALUE (ID,OFFSET,MAXBYTES,BYTE_ARRAY,ACTBYTES,STATUS)

*  Description:
*     \begin {tabbing}
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Check that the offset into the data is not negative. \\
*     Repeat \= \{ \\
*            \> Read the modified count for this item. \\
*            \> Copy as many bytes as there is room for in the user's buffer
*               from the noticeboard starting \\
*            \> at the specified offset and return the actual number of bytes
*               in the item. \\
*            \> Read the modified count for this item once more. \\
*            \> \} \= Until time out or the two modified counts are equal and
*                     even \\
*            \>    \> (which means that the values were not updated whilst they
*                     were being read).
*     \end {tabbing}
*
*     If CHECK_MODIFY is FALSE, the item's modified count is not checked
*     at all and a timeout cannot occur.
*
*     If the specified offset is greater than the current size of the item's
*     data, no error status will be returned and no data will be copied, but
*     the returned number of bytes (ACTBYTES) will be less than the offset
*     (OFFSET) and this case should always be checked for.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item from which thew value is to be got.
*     OFFSET = INTEGER (Given)
*        Byte offset into item data.
*     MAXBYTES = INTEGER (Given)
*        Size in bytes of the user's buffer.
*     BYTE_ARRAY = BYTE(*) (Returned)
*        User's buffer into which bytes will be got.
*     ACTBYTES = INTEGER (Returned)
*        Actual number of values associated with the item. This may be
*        greater than OFFSET + MAXBYTES but no more than MAXBYTES bytes
*        will be copied into the user's buffer.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*         	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__BADOFFSET	  => Negative offset specified
*        	NBS__TIMEOUT	  => Timeout awaiting valid data

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_VALUE ( R_INTEGER(id), R_INTEGER(offset), R_INTEGER(maxbytes),
                    RW_BYTE_ARRAY(byte_array), W_INTEGER(actbytes),
                    W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(offset)
  GENPTR_INTEGER(maxbytes)
  GENPTR_BYTE_ARRAY(byte_array)
  GENPTR_INTEGER(actbytes)
  GENPTR_INTEGER(status)

/* Local variable declarations.	*/

   item_id	tid;
   int		toffset;
   int		tmaxbytes;
   int		count;
   int		after;
   int		before;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID, the offset and the maximum number of bytes to get. */

      NBS_PTRIMP (int,tid,id);
      NBS_INTIMP (&toffset,offset);
      NBS_INTIMP (&tmaxbytes,maxbytes);

/* Check that the item ID is not NIL, that the item is primitive and that
   the offset is not negative.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_VALUE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_GET_VALUE_NOTPRIM", "Item is not primitive", status );
         }
      else if (toffset < 0)
         {
         *status = NBS__BADOFFSET;
         emsRep( "NBS_GET_VALUE_BADOFF",
                    "Offset is less than zero", status );
         }

/* If CHECK_MODIFY is TRUE: repeat: read modified count, return actual size of
   item, copy item data to user's buffer, read modified count; until time out
   or both values of the modified count are equal and even.	*/

      else if (check_modify || tid->board->check_modify) {
         count = 0;
	 do {
	    if (count > 0)
	       NBS_SLEEPMS ( timeout_interval );
	    before = tid->fixed->modified;
	    *actbytes = tid->fixed->actbytes;
	    _chmove (MIN (tmaxbytes,MAX (0,*actbytes - toffset)),
	    	       (char *)tid->da.data + toffset,byte_array);
	    after = tid->fixed->modified;
            count++;
	    } while (count < timeout_count && (before != after || ODD (after)));
         if (before != after || ODD (after))
            {
	    *status = NBS__TIMEOUT;
            emsRep( "NBS_GET_VALUE_TIMEOUT",
              "Time out getting item value", status );
            }
      }

/* If CHECK_MODIFY is FALSE: return actual size of item, copy item data to
   user's buffer.	*/

      else {
	 *actbytes = tid->fixed->actbytes;
	 _chmove (MIN (tmaxbytes,MAX (0,*actbytes - toffset)),
	    	       (char *)tid->da.data + toffset,byte_array);
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_CVALUE

*  Purpose:
*     Get a character string from a slice of a primitive item
*     associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_CVALUE (ID,OFFSET,STRING,ACTBYTES,STATUS)

*  Description:
*     Extracts the string data pointer and string length from the
*     FORTRAN argument list, and passes this information to NBS_GET_VALUE.
*     Thus, the number of bytes read is at most the length the string
*     supplied.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item from which thew value is to be got.
*     OFFSET = INTEGER (Given)
*        Byte offset into item data.
*     STRING = CHARACTER*(*) (Returned)
*        Users string buffer into which item bytes will be got.
*     ACTBYTES = INTEGER (Returned)
*        Actual number of values associated with the item. This may be
*        greater than OFFSET + LEN(STRING) but no more than LEN(STRING) bytes
*        will be copied into the user's buffer.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*         	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__BADOFFSET	  => Negative offset specified
*        	NBS__TIMEOUT	  => Timeout awaiting valid data

*  Notes:
*     No C version of this routine is supplied because C strings are
*     by convention null terminated. As NBS_GET_CVALUE writes new
*     string data, its correct functioning in the C case would rely on
*     the unwarranted assumption that sufficient space existed in the
*     destination string for the data to be written. By forcing the use
*     of NBS_GET_VALUE in this case, the user must at least state the
*     destination length explicitly.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     05-Mar-1993 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_GET_VALUE      Read byte array from primitive item.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
#ifndef c_string
int NBS_GET_CVALUE ( R_INTEGER(id), R_INTEGER(offset),
                     RW_CHARACTER(string), W_INTEGER(actbytes),
                     W_INTEGER(status) TRAIL(string) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(offset)
  GENPTR_CHARACTER(string)
  GENPTR_INTEGER(actbytes)
  GENPTR_INTEGER(status)

IF_OK {
  return NBS_GET_VALUE( id, offset, &string_length, (F77_BYTE_TYPE *) string,
                 actbytes, status );
  }

  return 0;
}
#endif

/*
*+
*  Name:
*     NBS_GET_SHAPE

*  Purpose:
*     Get the shape of a primitive item
*     associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_SHAPE (ID,MAXDIMS,DIMS,ACTDIMS,STATUS)

*  Description:
*     \begin {tabbing}
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Repeat \= \{ \\
*            \> Read the modified count for this item. \\
*            \> Copy as many dimensions as there is room for in the user's
*               buffer from the noticeboard and \\
*            \> return the actual number of dimensions in the item. \\
*            \> Read the modified count for this item once more. \\
*            \> \} \= Until time out or the two modified counts are equal and
*                     even \\
*            \>    \> (which means that the values were not updated whilst they
*                     were being read).
*     \end {tabbing}
*
*     If CHECK_MODIFY is FALSE, the item's modified count is not checked
*     at all and a timeout cannot occur.
*
*     Note also that the MAXDIMS parameter to this routine is a MODIFIED
*     parameter.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item from which the shape is to be got.
*     MAXDIMS = INTEGER (Given and returned)
*        On entry, size of the DIMS array. On exit the maximum number
*        of dimensions that this item can have.
*     DIMS = INTEGER(*) (Returned)
*        Returned dimensions.
*     ACTDIMS = INTEGER (Returned)
*        Actual number of dimensions associated with the item. This may
*        be greater than MAXDIMS but no more than MAXDIMS values will be
*        copied into the DIMS array.
*     MAXBYTES = INTEGER (Given)
*        Size in bytes of the user's buffer.
*     BYTE_ARRAY = BYTE(*) (Returned)
*        User's buffer into which bytes will be got.
*     ACTBYTES = INTEGER (Returned)
*        Actual number of values associated with the item. This may be
*        greater than OFFSET + MAXBYTES but no more than MAXBYTES bytes
*        will be copied into the user's buffer.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*         	NBS__NOTPRIMITIVE => Item is not primitive
*        	NBS__TIMEOUT	  => Timeout awaiting valid data

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_SHAPE ( R_INTEGER(id), W_INTEGER(maxdims), RW_INTEGER_ARRAY(dims),
                    W_INTEGER(actdims), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(maxdims)
  GENPTR_INTEGER_ARRAY(dims)
  GENPTR_INTEGER(actdims)
  GENPTR_INTEGER(status)

/* Local variable declarations.	*/

   item_id	tid;
   int		count;
   int		after;
   int		before;
   int		i;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that the item is primitive.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_SHAPE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_GET_SHAPE_NOTPRIM", "Item is not primitive", status );
         }

/* If CHECK_MODIFY is TRUE: repeat: read modified count, return actual number
   of dimensions in item, copy item data to user's buffer, read modified count;
   until time out or both values of the modified count are equal and even.
   Return the maximum number of dimensions that this item can have.	*/

      else if (check_modify || tid->board->check_modify) {
         count = 0;
	 do {
	    if (count > 0)
	       NBS_SLEEPMS ( timeout_interval );
	    before = tid->fixed->modified;
            *actdims = tid->fixed->actdims;
            for (i=0; i < MIN (*maxdims,*actdims); i++)
 	       dims[i] = tid->gs.shape[i];
	    after = tid->fixed->modified;
            count++;
	    } while (count < timeout_count && (before != after || ODD (after)));
         if (before != after || ODD (after))
            {
	    *status = NBS__TIMEOUT;
            emsRep( "NBS_GET_SHAPE_TIMEOUT",
              "Time out getting item shape", status );
            }
         *maxdims = tid->fixed->maxdims;
      }

/* If CHECK_MODIFY is FALSE: return actual size of item, copy item data to
   user's buffer and return the number of dimensions that this item can have.*/

      else {
         *actdims = tid->fixed->actdims;
         for (i=0; i < MIN (*maxdims,*actdims); i++)
	    dims[i] = tid->gs.shape[i];
         *maxdims = tid->fixed->maxdims;
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_MODIFIED

*  Purpose:
*     Get the noticeboard modified count or an item modified count
*     depending on whether this is a structured or primitive item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_MODIFIED (ID,MODIFIED,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     If the item is structured get the noticeboard modified count. \\
*     If the item is primitive get the item's modified count.
*
*     For structured items, this value is incremented each time an item in the
*     noticeboard is updated.
*
*     For primitive items, if this value is even then the associated values
*     are not currently being updated. If it is odd then they are currently
*     being updated. The total number of updates to this item is half the
*     value of the modified count.
*
*     An "update" is an update of an item's value, shape or size.
*
*     Note that when item data is accessed directly via pointer then the
*     modified count is not updated unless this is done explicitly using the
*     NBS_INC_MODIFIED routine.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item from which the modified count is to be got.
*     MODIFIED = INTEGER (Returned)
*        The current value of the noticeboard or item's modified count.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     09-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_MODIFIED ( R_INTEGER(id), W_INTEGER(modified), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(modified)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL. */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_MODIFIED_NILID", "NIL item ID", status );
         }

/* If primitive, get the item's modified count.	*/

      else if (tid->fixed->primitive)
         *modified = tid->fixed->modified;

/* If structured, get the noiceboard modified count. */

      else
         *modified = tid->board->modified;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_MODIFIED_POINTER

*  Purpose:
*     Get a pointer to the noticeboard modified count or an item modified count
*     depending on whether this is a structured or primitive item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_MODIFIED_POINTER (ID,POINTER,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     If the item is structured get the noticeboard modified count. \\
*     If the item is primitive get the item's modified count. \\
*     Return a pointer to the appropriate modified count.
*
*     For structured items, this value is incremented each time an item in the
*     noticeboard is updated.
*
*     For primitive items, if this value is even then the associated values
*     are not currently being updated. If it is odd then they are currently
*     being updated. The total number of updates to this item is half the
*     value of the modified count.
*
*     An "update" is an update of an item's value, shape or size.
*
*     Note that when item data is accessed directly via pointer then the
*     modified count is not updated unless this is done explicitly using the
*     NBS_INC_MODIFIED routine.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for which the pointer to its modified count
*        is to be got.
*     POINTER = INTEGER (Returned)
*        The address of the item's modified count.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     09-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_MODIFIED_POINTER ( R_INTEGER(id), data_id *pointer,
                               W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_MODIFIED_POINTER_NILID", "NIL item ID", status );
         }

/* If primitive, return a pointer to the item's modified count.	*/

      else if (tid->fixed->primitive)
         *pointer = (data_id) &tid->fixed->modified;

/* If structured, return a pointer to the noiceboard modified count. */

      else
         *pointer = (data_id) &tid->board->modified;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_UPDATED

*  Purpose:
*     Determine whether a primitive item or the noticeboard has been updated
*     since the noticeboard was found or this routine was last called.

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_UPDATED (ID,UPDATED,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     If the item is structured get the noticeboard modified count. \\
*     If the item is primitive get the item's modified count. \\
*     Return TRUE (1) if the modified count is greater than the count the last
*     time that this routine was called. \\
*     Return FALSE (0) otherwise. \\
*     Remember the modified count for next time.
*
*     For structured items, always use the same ID, since the remembered count
*     is associated with the ID and not with the noticeboard.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for which to determine whether it has been
*        updated since the last call on its behalf.
*     UPDATED = INTEGER (Returned)
*         Whether updated (1) or not (0).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     09-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_UPDATED ( R_INTEGER(id), W_INTEGER(updated), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(updated)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL. */

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_UPDATED_NILID", "NIL item ID", status );
         }

/* If primitive, use the item's modified count.	*/

      else if (tid->fixed->primitive) {
         *updated = tid->fixed->modified > tid->modified;
	 tid->modified = tid->fixed->modified;
      }

/* If structured, use the noiceboard modified count. */

      else {
         *updated = tid->board->modified > tid->modified;
	 tid->modified = tid->board->modified;
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_POINTER

*  Purpose:
*     Return a pointer to the first byte of the data of a primitive item
*     associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_POINTER (ID,POINTER,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Return the address of the start of the item's noticeboard data.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for which the pointer to its noticeboard
*        data is to be got.
*     POINTER = INTEGER (Returned)
*        The address of the first byte of this item's noticeboard data.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__NOTPRIMITIVE => Item is not primitive

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_POINTER ( R_INTEGER(id), data_id *pointer, W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that the item is primitive.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_POINTER_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_GET_POINTER_NOTPRIM", "Item is not primitive", status );
         }

/* Return the address of the first byte of the item's noticeboard data.	*/

      else
        EXPORT_POINTER(tid->da.data,pointer);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_NAME

*  Purpose:
*     Get the name of an item associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_NAME (ID,NAME,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Return the item's name.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for whose name is to be got.
*     NAME = CHARACTER*(*) (Returned)
*        The item's name.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STREXP        Export a string

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_NAME ( R_INTEGER(id), RW_CHARACTER(name),
                   W_INTEGER(status) TRAIL(name) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_NAME_NILID", "NIL item ID", status );
         }

/* Export the item name.    */

      else
         NBS_STREXP (name,tid->fixed->name,MAXNAME);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_TYPE

*  Purpose:
*     Get the type of an item associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_TYPE (ID,TYPE,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Return the item's type.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for whose type is to be got.
*     TYPE = CHARACTER*(*) (Returned)
*        The item's type.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STREXP        Export a string

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_TYPE ( R_INTEGER(id), RW_CHARACTER(type),
                   W_INTEGER(status) TRAIL(type) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(type)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_TYPE_NILID", "NIL item ID", status );
         }

/* Export the item type.    */

      else
         NBS_STREXP (type,tid->fixed->type,MAXTYPE);
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_SIZE

*  Purpose:
*     Get the maximum and actual sizes of a primitive item
*     associated with the specified identifier

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_SIZE (ID,MAXBYTES,ACTBYTES,STATUS)

*  Description:
*     Check that the ID is not NIL and that it pertains to a primitive item. \\
*     Return the maximum and actual sizes of the item's noticeboard data.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item for whose size is to be got.
*     MAXBYTES = INTEGER (Returned)
*        Maximum size in bytes.
*     ACTBYTES = INTEGER (Returned)
*        Actual size in bytes.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*               NBS__NOPRIMITIVE  => Item is not primitive

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_SIZE ( R_INTEGER(id), W_INTEGER(maxbytes),
                   W_INTEGER(actbytes), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(maxbytes)
  GENPTR_INTEGER(actbytes)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that the item is primitive.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_SIZE_NILID", "NIL item ID", status );
         }
      else if (!tid->fixed->primitive)
         {
	 *status = NBS__NOTPRIMITIVE;
         emsRep( "NBS_GET_SIZE_NOTPRIM", "Item is not primitive", status );
         }

/* Return the maximum and actual sizes of the item's noticeboard data.	*/

      else {
         *maxbytes = tid->fixed->maxbytes;
         *actbytes = tid->fixed->actbytes;
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_PRIMITIVE

*  Purpose:
*     Determine whether or not an item is primitive

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_PRIMITIVE (ID,PRIMITIVE,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Return FALSE (0) if the item is a structure and TRUE (1) if it is
*     primitive.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item concerned.
*     PRIMITIVE = INTEGER (Returned)
*        Whether primitive (1) or structured (0).
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_PRIMITIVE ( R_INTEGER(id), W_INTEGER(primitive), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(primitive)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_PRIMITIVE_NILID", "NIL item ID", status );
         }

/* Return YES (1) if primitive, NO (0) if structured.	*/

      else
         *primitive = tid->fixed->primitive;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_PARENT

*  Purpose:
*     Get the identifier of an item's parent structure

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_PARENT (ID,PARENT,STATUS)

*  Description:
*     Check that the ID is not NIL. \\
*     Return the identifier of the item's parent.
*
*     If the item has no parent (ie, if it pertains to a noticeboard), then a
*     zero ID will be returned.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item whose parent is to be got.
*     PARENT = INTEGER (Returned)
*        Identifier of item's parent. If the item has no parent then
*        a NIL ID will be returned.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_PARENT ( R_INTEGER(id), item_id *envid, W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Ensure that a NIL value will be returned for the ID if the routine fails. */

      *envid = NIL;

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_PARENT_NILID", "NIL item ID", status );
         }

/* Return the ID of the item's parent.	*/

      else
         *envid = tid->vp.parent;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_CHILDREN

*  Purpose:
*     Get the number of children of a structured item

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_CHILDREN (ID,CHILDREN,STATUS)

*  Description:
*     Check that the ID is not NIL and does not pertain to a primitive item. \\
*     Return the number of children that it has.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of the item whose number of children is to be got.
*     CHILDREN = INTEGER (Returned)
*        Number of children.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*        	NBS__PRIMITIVE	  => Parent is primitive

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     16-Feb-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_CHILDREN ( R_INTEGER(id), W_INTEGER(children), W_INTEGER(status) )
{
  GENPTR_INTEGER(id)
  GENPTR_INTEGER(children)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL and that it pertains to a structured
   item.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_CHILDREN_NILID", "NIL item ID", status );
         }
      else if (tid->fixed->primitive)
         {
	 *status = NBS__PRIMITIVE;
         emsRep( "NBS_GET_CHILDREN_PRIM", "Item is primitive", status );
         }

/* Return the number of children.	*/

      else
         *children = tid->fixed->children;
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_INFO

*  Purpose:
*     Get general non-character information on a given noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_INFO (ID,NAME,VALUE,STATUS)

*  Description:
*     Check that the item name is legal. \\
*     Copy the current value of the item from the relevant noticeboard.
*
*     There are currently eight items which can be returned. Most are from
*     a common noticeboard area but GLOBAL_BASE is an address within the
*     address space of the caller. Unless otherwise stated, all are integers.
*
*     CHAN        	=> Channel to open noticeboard file (zero if not open)
*     DEFN_SIZE        	=> Size of definition part of noticeboard
*     FILE_SIZE        	=> Size of noticeboard definition file
*     GLOBAL_BASE        => Address of noticeboard start
*     MODIFIED        	=> Total number of times values have been modified
*     PID        	=> PID of owner of this noticeboard
*     SECTION_SIZE        => Total size of noticeboard including data
*     VERSION        	=> Software version creating file / noticeboard

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of noticeboard or of any item in the relevant noticeboard.
*     NAME = CHARACTER*(*) (Given)
*        The name of the item to obtain. See the above list. Can be abbreviated
*        so long as it remains unambiguous but this is not recommended because
*        new items may be supported in the future. Case is not significant.
*     VALUE = Depends on NAME (Returned)
*        The item's value. Declared as pointer to integer, but may be
*        coerced to pointer to real.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*                NBS__BADOPTION	  => Illegal item name

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     07-Feb-1990 (WFL):
*        Original version.
*     31-Mar-1993 (DJA):
*        Added error reporting and updated string handling. SAVE_NAME
*        option removed to NBS_GET_CINFO.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STRIMP       Import a string, converting to upper-case.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_INFO ( R_INTEGER(id), RW_CHARACTER(name), W_INTEGER(value),
                   W_INTEGER(status) TRAIL(name) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(value)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   int          *ivalue = value;
   item_id	tid;
   char		tname[2];

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_INFO_NILID", "NIL item ID", status );
         }

/* Import the name (converting case) and set the value as appropriate. */

      else {
         NBS_STRIMP (tname,name,2);

/* Values from the common area. */

         if (strncmp (tname,"CHAN",2) == 0)		/* Channel number */
	    *ivalue = (int) tid->board->chan;
         else if (strncmp (tname,"DEFN_SIZE",2) == 0)	/* Definition size */
	    *ivalue = tid->board->defn_size;
         else if (strncmp (tname,"FILE_SIZE",2) == 0)	/* File size */
	    *ivalue = tid->board->file_size;
         else if (strncmp (tname,"MODIFIED",2) == 0)	/* Modified count */
	    *ivalue = tid->board->modified;
         else if (strncmp (tname,"PID",2) == 0)		/* Owner PID */
	    *ivalue = tid->board->pid;
         else if (strncmp (tname,"SAVE_NAME",2) == 0)	/* File name */
            {
	    *status = NBS__BADOPTION;
            emsSetnc( "OPT", tname, MAXNAME );
            emsRep( "NBS_GET_INFO_BADOPT",
               "Bad info option /^OPT/ - now supported by NBS_GET_CINFO",
               status );
            }
         else if (strncmp (tname,"SECTION_SIZE",2) == 0)/* Section size */
	    *ivalue = tid->board->section_size;
         else if (strncmp (tname,"VERSION",2) == 0)	/* Software version */
	    *ivalue = tid->board->version;

/* Process-specific values. */

         else if (strncmp (tname,"GLOBAL_BASE",2) == 0)	/* Base address */
	    *ivalue = (int) tid->gs.global_base;
	 else
            {
	    *status = NBS__BADOPTION;
            emsSetnc( "OPT", tname, MAXNAME );
            emsRep( "NBS_GET_INFO_BADOPT", "Bad info option /^OPT/", status );
            }
      }
   }
   return (*status);
}

/*
*+
*  Name:
*     NBS_GET_CINFO

*  Purpose:
*     Get general character information on a given noticeboard

*  Language:
*     ANSI C

*  Invocation:
*     (Int) = NBS_GET_CINFO (ID,NAME,VALUE,STATUS)

*  Description:
*     Check that the item name is legal. \\
*     Copy the current value of the item from the relevant noticeboard.
*
*     There is currently only one character item which can be returned.
*
*     SAVE_NAME        	=> Name of noticeboard file (character)

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier of noticeboard or of any item in the relevant noticeboard.
*     NAME = CHARACTER*(*) (Given)
*        The name of the item to obtain. See the above list. Can be abbreviated
*        so long as it remains unambiguous but this is not recommended because
*        new items may be supported in the future. Case is not significant.
*     VALUE = CHARACTER*(*) (Returned)
*        The item's value.
*     STATUS = INTEGER (Given and returned)
*        The global status. Possible return values are,
*        	NBS__NILID	  => NIL ID
*                NBS__BADOPTION	  => Illegal item name

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     31-Mar-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_STREXP       Export a string.
*        NBS_STRIMP       Import a string, converting to upper-case.

*  Prior Requirements:
*     NBS_FIND_NOTICEBOARD must have been called.

*-
*/
int NBS_GET_CINFO ( R_INTEGER(id), RW_CHARACTER(name), RW_CHARACTER(value),
                    W_INTEGER(status) TRAIL(name) TRAIL(value) )
{
  GENPTR_INTEGER(id)
  GENPTR_CHARACTER(name)
  GENPTR_CHARACTER(value)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   item_id	tid;
   char		tname[2];

/* Start of code */

/* Check that status is good. */

   IF_OK {

/* Import the item ID.	*/

      NBS_PTRIMP (int,tid,id);

/* Check that the item ID is not NIL.	*/

      if (tid == NIL)
         {
         *status = NBS__NILID;
         emsRep( "NBS_GET_CINFO_NILID", "NIL item ID", status );
         }

/* Import the name (converting case) and set the value as appropriate. */

      else {
         NBS_STRIMP (tname,name,2);

/* Values from the common area. */

         if (strncmp (tname,"SAVE_NAME",2) == 0)	/* File name */
	    NBS_STREXP (value,tid->board->save_name,MAXFILE);

	 else
            {
	    *status = NBS__BADOPTION;
            emsSetnc( "OPT", tname, MAXNAME );
            emsRep( "NBS_GET_CINFO_BADOPT", "Bad info option /^OPT/", status );
            }
      }
   }
   return (*status);
}
