/*
*+
*  Name:
*     nbs_typ.h

*  Purpose:
*     Type definitions for noticeboard system.

*  Language:
*     C

*  Description:
*     The same types are used both internally and in noticeboard definition
*     files. There is a complication with pointers since they are invalidated
*     when data is moved to a different virtual address. Thus all pointers are
*     relocated relative to a small positive integer when noticeboard
*     definitions are saved to disc or copied to a global section. All
*     processes accessing a given noticeboard take a private copy of all the
*     pointers and relocate them so that they are valid pointers for that
*     process. This allows direct access to noticeboard data using C
*     structures.
*
*     Apart from the above, the structure of a noticeboard definition is
*     identical in private memory, in a global section and on disc. The basic
*     data structure is the "item descriptor". This consists entirely of
*     pointers and is the only structure that contains pointers (thus only item
*     descriptors need relocating). It has the following structure.
*
*            Pointer to parent item descriptor
*            Pointer to eldest child item descriptor
*            Pointer to next sibling item descriptor
*            Pointer to fixed information
*            Pointer to general noticeboard information
*            Pointer to shape information
*            Pointer to associated data
*          Number of lower-level items accessed via this item
*
*     The first pointers are to other item descriptors. It can be seen that an
*     item gains access to its children by first going to its eldest child and
*     then searching through its eldest child's siblings. This is simple but can
*     be inefficient when there are many children (and results in very heavily
*     nested recursive calls when relocating pointers).
*
*     The other pointers are data structures, none of which contain any
*     pointers. Because of this there is no need to take private copies of
*     these structures and the always reside in the global section (any that
*     can change must be in the global section anyway). The fixed information
*     has the following structure.
*
*            Item name (up to 16 bytes, null-terminated if shorter)
*            Item type (up to 16 bytes, null-terminated if shorter)
*            Whether or not the item is primitive
*            Max dimensionality of item
*            Current dimensionality of item
*            Max size of item in bytes
*            Current size of item in bytes
*            Modified count (incremented before and after each modification)
*
*     There is only one copy of the general noticeboard information per
*     noticeboard. It has the following structure.
*
*            Software version creating file / noticeboard
*          Size of noticeboard definition file (0 if no file)
*                 Size of definition part of noticeboard
*            Total size of noticeboard including data
*            PID of owner (first mapper) of this noticeboard
*          Number of times noticeboard data has been modified
*
*          Timeout count on find and get
*          Timeout interval on find and get
*          Whether everybody can write noticeboard
*          Whether to increment modify count on put
*          Whether to check modify count on get
*
*          Channel number on which noticeboard definition file is open
*          Address of start of noticeboard
*          Name of file from which noticeboard was restored
*
*     (The last three items are only used when updating noticeboard data to
*     disc and are only meaningful to the noticeboard owner.)
*
*     The shape information is simply an array of integers.
*
*     The associated data is simply an array of bytes.
*
*     The only assumption that is made about the relative placement of these
*     structures within a noticeboard definition is that the item descriptor
*     for the top-level item starts at offset zero. The data always starts
*     at the byte following the last byte of the definition.
*
*     (In fact it would be a good idea if the item descriptors were allocated
*     in a separate contiguous part of memory. If this were done then mapping
*     processes would need to take a copy only of the item descriptors,
*     rather than of the entire definition. This is not possible at present
*     because the item descriptors are all mixed in with the fixed information,
*     the noticeboard information and the shape information.)

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (AAO)
*     {enter_new_authors_here}

*  History:
*     03-Feb-86 (WFL):
*        Original version
*     17-Jul-87 (WFL):
*        Ensure that MODIFIED is word-aligned with respect to
*             the rest of the structure. Also ensure that all structures
*          are an integral number of words in length.
*     21-Jul-87 (WFL):
*        o Add DEFN_SIZE, SECTION_SIZE and VERSION to BOARD_INFO.
*          o Excise all mention of OTHER_SELF.
*          o Token effort at saving space - make MAXDIMS and ACTDIMS
*            into SHORT integers.
*          o Move definition of VERSION to here.
*     22-Jul-87 (WFL):
*        Remove BASE from BOARD_INFO.
*     06-Nov-87 (WFL):
*        Portable VMS / UNIX version. Use strdescr macro to
*             decide whether to use VMS string descriptors or C strings.
*     11-Feb-88 (WFL):
*        Don't alter strdescr macro definition and use the
*          c_string macro instead.
*     16-Feb-88 (WFL):
*        Add CHILDREN to FIXED_INFO, add MODIFIED to BOARD_INFO,
*          and increment VERSION.
*     25-Mar-88 (WFL):
*        o Add FILE_SIZE, SAVE_NAME and CHAN to BOARD_INFO and
*            increment VERSION. Move file header definition from NBSLOW.
*          o Add RESERVE fields to all data structures to minimise
*            necessity for incrementing VERSION on all changes (and increment
*            VERSION).
*     20-May-88 (WFL):
*        Add INT_ID type (type used for input integers - int
*          for c_string, int* otherwise).
*     01-Feb-90 (WFL):
*        Add ACCESSED to ITEM_INFO, and TIMEOUT_COUNT,
*          TIMEOUT_INTERVAL, WORLD_WRITE, INCREMENT_MODIFY and
*          CHECK_MODIFY to BOARD_INFO; no need to alter VERSION
*     06-Feb-90 (WFL):
*        Use variant for ITEM_INFO PARENT/VALID and for
*          DATA/ACCESSED; add ITEM_INFO TRIGGER and MODIFIED; add
*          ORIGINAL_UNMAPPED
*     07-Feb-90 (WFL):
*        Add variant ITEM_INFO GLOBAL_BASE/SHAPE, with
*          GLOBAL_BASE used only for top-level items
*     15-Feb-90 (WFL):
*        Remove TIMEOUT_COUNT and TIMEOUT_DELAY from BOARD_INFO
*     19-Nov-93 (DJA):
*        string_id removed in favour of F77 usage.
*     24-Nov-94 (DJA):
*        Allocated item sizes rounded up to long boundaries
*     11-Sep-95 (DJA):
*        Fixed bug in _ALIGN_ITEM macro
*     12-Sep-04 (TIMJ):
*        VERSION now NBSVERSION.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */

/* Constant definitions	*/

/*  VERSION is the current software version and is written in the header of
 *  all noticeboard definition files and in all noticeboard globals sections.
 *  It is changed only when the format of noticeboard definition files changes.
 *  Note that this format changes implicitly when practically any of the data
 *  types defined here change.	*/

#define NBSVERSION 5

/*  MAXNAME and MAXTYPE are maximum lengths of item names and types */

#define MAXNAME 16
#define MAXTYPE 16

/*  MAXFILE is maximum length of noticeboard file name */

#define MAXFILE 80

/*  Various data structure sizes */

#define _ALIGN_ITEM(_x) ((((_x)-1)/sizeof(double))*sizeof(double)+sizeof(double) )

#define ITEM_SIZE      _ALIGN_ITEM(sizeof(struct item_descriptor))
#define FIXED_SIZE     _ALIGN_ITEM(sizeof(struct fixed_info))
#define BOARDINFO_SIZE _ALIGN_ITEM(sizeof(struct board_info))
#define HEADER_SIZE    _ALIGN_ITEM(sizeof(struct header_info))

#define INT_SIZE sizeof(int)

/* Typedef definitions */

/*  Shape_id is a pointer to an integer array holding shape information */

typedef int *shape_id;

/*  Board_id is a pointer to Board_info, which is defined later */

typedef struct board_info *board_id;

/*  Data_id is a pointer to a character array holding data associated with a
 *  primitive data item */

typedef char *data_id;

/*  Mapping_id is a pointer to Mapping_info,which is defined later */

typedef struct mapping_info *mapping_id;

/* Structure definitions */

/* The c_string macro governs how input scalar arguments are passed.
 * They are passed as type int_id and this is int if c_string is true and
 * int* otherwise. */

#ifdef c_string
typedef int int_id;
#else
typedef int *int_id;
#endif

/*  Fixed_info is the part of the information associated with a noticeboard item
 *  that contains no addresses and thus that does not have to be relocated.
 *  Fixed_id is a pointer to it. The MODIFIED field MUST be word-aligned,
 *  since it is incremented using the ADAWI instruection. MALLOC returns
 *  quadword-aligned addresses so it is necessary to ensure that MODIFIED is
 *  at an even byte offset from the start of FIXED_INFO and that all structures
 *  are of an even length.	*/

typedef struct fixed_info *fixed_id;

struct fixed_info {
   char name[MAXNAME];    /* Item name */
   char type[MAXTYPE];    /* Item type */
   short primitive;       /* Whether or not the item is primitive */
   short children;        /* Number of children */
   short maxdims;         /* Max dimensionality of item */
   short actdims;         /* Current dimensionality of item */
   int maxbytes;          /* Max size of item in bytes */
   int actbytes;          /* Current size of item in bytes */
   int modified;          /* Used in checking whether modded during read */
   int reserve[2];
};

/*  Item_descriptor is the set of pointers defining a noticeboard item's
 *  position within a noticeboard and values pertaining to an item which are
 *  local to a given process. Item_id is a pointer to it. The VALID flag
 *  is only used in top-level items (and is thus the first four bytes of a
 *  noticeboard). (S) means that a component is used only for structures.
 *  (P) means that it is used only for primitives. (SP) means that it is used
 *  for both. Unions have been used in order to add new facilities without
 *  having to lose upwards compatibility of noticeboard format. */

typedef struct item_descriptor *item_id;

struct item_descriptor {
   union {
      int valid;          /* Whether noticeboard is valid (top-level S) */
      item_id parent;     /* Pointer to parent item descriptor (other S) */
   } vp;
   item_id heir;          /* Pointer to eldest child item descriptor (S) */
   item_id sibling;       /* Pointer to next sibling item descriptor (SP) */
   fixed_id fixed;        /* Pointer to fixed info - no pointers (SP) */
   union {
      item_id global_base; /* Address of noticeboard start (S top-level) */
      shape_id shape;     /* Pointer to shape information (P) */
   } gs;
   board_id board;	  /* Pointer to general noticeboard information (SP) */
   union {
      data_id data;       /* Pointer to associated data (P) */
      short accessed;     /* Number of items accessed from this one (S) */
   } da;
   int (*trigger)();      /* Routine to call when item is updated (P) */
   int modified;          /* Used to check whether updated (SP) */
};

/*  Board_info is general information that is held in each noticeboard.
 *  Board_id is a pointer to it. There is some duplication with the file
 *  header but they are logically distinct in purpose. Note that the channel
 *  number and start address are meaningful only to the noticeboard owner! */

struct board_info {
   int version;		  /* Software version creating file / noticeboard */
   int file_size;	  /* Size of noticeboard definition file */
   int defn_size;	  /* Size of definition part of noticeboard */
   int section_size;	  /* Total size of noticeboard including data */
   int pid;		  /* PID of owner (first mapper) of this noticeboard */
   int modified;          /* Total number of times values have been modified */
   FILE *chan;		  /* Channel to open noticeboard file (NIL if closed) */
   item_id global_base;	  /* Address of noticeboard start (owner addr space) */
   char save_name[MAXFILE]; /* Name of noticeboard file */
   unsigned original_unmapped:1; /* Whether original section has been unmapped*/
   unsigned world_write:1; /* Whether everybody can write to this noticeboard */
   unsigned increment_modify:1; /* Whether to increment modified count on put */
   unsigned check_modify:1; /* Whether to check modified count on get */
   unsigned fill:4;
   char reserve[7];
};

/*  Header_info is the structure written in the definition file header */

struct header_info {
   int version;		  /* Software version creating file */
   int file_size;	  /* Size of noticeboard definition file */
   int defn_size;	  /* Size of definition part of noticeboard */
   int section_size;	  /* Total size of noticeboard including data */
   long time;		  /* Time of noticeboard definition file creation */
   int reserve[2];
};


/* Mapping_Info is a structure used internally by NBS to keep track of UNIX
   global sections mapped by the current process */

struct mapping_info {
   int memid;             /* The memory identifier of the global section */
   int acount;            /* Attach count */
   int destroy; 	  /* Destroy NB when unmapped? */
   char *addr;            /* Address of process which is mapped to section */
   mapping_id next;       /* Link to next mapping_info in list */
};
