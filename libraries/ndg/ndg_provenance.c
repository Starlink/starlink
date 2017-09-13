/*
*  Name:
*     ndg_provenance.c

*  Purpose:
*     Provides functions for handling NDF provenance information.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines all the functions used for reading, writing,
*     modifying and querying provenance information in an NDF.
*
*     The provenance information in an NDF encapsulates details of all
*     the other NDFs that were used in the creation of the NDF. The
*     information is heirarchical and includes parents, grandparents,
*     great-grandparents, etc., all the way back to "root ancestors" (a
*     root ancestor is an ancestor NDF that has no recorded parents).
*
*     On disk, the provenance information is stored in an NDF extension
*     called "PROVENANCE" (for details see the section "The PROVENANCE
*     Extension" below). The ndgReadProv function reads this information
*     and copies it into an in-memory structure for faster access
*     (ndgReadProv can also create an empty anonymous provenance structure
*     that is not associated with an NDF). All the other public functions
*     defined by this module accept an identifier for such an in-memory
*     structure as their first argument. The ndgWriteProv function can be
*     used to write the in-memory structure back out to disk as a PROVENANCE
*     extension in an NDF. The in-memory structure should be freed when no
*     longer needed, using ndgFreeProv.

*  Functions Provides:
*     This modules provides the following public functions. There is an
*     equivalent set of F77 routines with names formed by converting the
*     C name to upper case and inserting an underscore after the initial
*     "NDG" string (C and F77 versions are documented individually in
*     separate prologues below):
*
*     - ndfAddProv: Record multiple input NDFs as ancestors in an output NDF.
*     - ndgCopyProv: Create a deep copy of a provenance structure.
*     - ndgCountProv: Return the number of ancestors in a provenance structure.
*     - ndgFormatProv: Format all the information in a provenance structure.
*     - ndgFreeProv: Free the resources used by a provenance structure.
*     - ndgGetProv: Get information about a specific ancestor.
*     - ndgHideProv: Hide a specific ancestor.
*     - ndgIsHiddenProv: See if an ancestor is hidden.
*     - ndgModifyProv: Modify information stored for a specific ancestor.
*     - ndgPutProv: Add a new ancestor NDF into a provenance structure.
*     - ndgReadProv: Create a new provenance structure by reading a given NDF.
*     - ndgReadVotProv: Create a new provenance structure from a VOTABLE.
*     - ndgRemoveProv: Remove ancestors from a provenance structure.
*     - ndgRootProv: Identify root ancestors in a provenance structure.
*     - ndgUnhashProv: Clear the hash code describing the creation of the Provenance.
*     - ndgUnhideProv: Ensure an ancestor is not hidden.
*     - ndgWriteProv: Writes a provenance structure out to an NDF.
*     - ndgWriteVotProv: Writes a provenance structure out to a VOTABLE.

*  The PROVENANCE Extension:
*     This section describes the format of the NDF extension used to
*     store provenance information in NDG version 6.0 and later (for the
*     pre-V6.0 format, see the next section). The PROVENANCE extension in
*     an NDF contains the following components:
*
*     - "DATA": A one-dimensional integer array containing descriptions
*     of all the NDFs that were used to create the main NDF. These
*     descriptions are encoded into am opaque set of integer values in
*     order to save time and space. See the next section for a list of the
*     separate items of information that go into the DATA array (but note
*     the MORE component described in the next section is not included in
*     the DATA array).
*
*     - "MORE": An optional one-dimensional array of structures containing
*     arbitrary extra information about selected ancestor NDFs. If
*     present, each element of this array contains supplemental information
*     for a single ancestor NDF, and the DATA array will contain indices
*     into the MORE array for those ancestors which have additional
*     information.

*  The pre-V6.0 PROVENANCE Extension:
*     The format in which provenance information is stored within an
*     NDF's PROVENANCE extension changed radically at NDG version 6.0.
*     Prior to v6.0, the seperate numerical values, strings, etc, that
*     form the provenance information were stored in separate HDS
*     components. But for large provenance systems this proved to be in
*     efficient in terms of both processing time and disk space.
*     Therefore, as of NDG v6.0, the numerical values, strings, etc,
*     forming the information are encoded into a single array of integers
*     as described in the previous section. The current version of NDG
*     will read both formats of provenance extension, but always writes
*     the new integer-encoded format.
*
*     The rest of this section describes the old format (note, the use of
*     the present tense is purely historical). In addition to documenting
*     the old format, this description serves to illustrate the concepts
*     behind the provenance system. These concepts have not changed - the
*     only thing that has changed is how these concepts are stored within
*     an HDS object.
*
*     - The PROVENANCE extension in an NDF contains four components:
*     "PARENTS", "ANCESTORS", "CREATOR", "DATE" and "HASH". The DATE
*     component is a character string holding the date and time at which the
*     information in the provenance extension was last modified. The date is
*     UTC formatted by PSX_ASCTIME. The ANCESTORS component is a 1D array
*     of "PROV" structures (described below). Each element describes a
*     single NDF that was used in the creation of the main NDF, either
*     directly or indirectly. The PARENTS component is a 1D integer
*     array holding the indices within the ANCESTORS array of the NDFs
*     that are the direct parents of the main NDF. The CREATOR component
*     holds an arbitrary identifier for the software that created the
*     main NDF. The HASH component is an integer that identifies the
*     contents of the current History record in the NDF at the time the
*     PROVENANCE extension was created. This is used to determine which
*     history records to copy into the PROVENANCE extension if the main
*     NDF is used in the creation of another NDF.
*     - Each PROV structure describes a single NDF that was used in the
*     creation of the main NDF, and can contain the following components;
*     "PARENTS", "DATE", "PATH", "CREATOR", "HISTORY" and "MORE". If present,
*     the PARENTS component is a 1D integer array holding the the indices
*     within the ANCESTORS array of the direct parents of the ancestor NDF.
*     If PARENTS is not present, the ancestor NDF is a "root" NDF (that is,
*     it has no known parents). If present, the DATE component is a string
*     holding the formatted UTC date at which the provenance information for
*     the ancestor NDF was determined. If this date is not known, the DATE
*     component will not be present (this will be the case, for instance, for
*     all root NDFs). The PATH component will always be present, and is a
*     string holding the full path to the ancestor NDF. This includes any HDS
*     path within the container file, but will not include any NDF or HDS
*     section specifier. Neither will it include the trailing ".sdf" suffix.
*     If present, the MORE component is an arbitrary HDS structure in which
*     any extra information about the ancestor NDF can be stored. The
*     CREATOR component holds an arbitrary identifier for the software that
*     created the ancestor NDF. The HISTORY component is an array of "HISREC"
*     structures, each containing a copy of a single History record from
*     the NDF described by the PROV structure. Only History records that
*     describe operations performed on the NDF itself are stored (including
*     the record that describes the creation of the NDF). That is, History
*     records inherited from the NDF's own parents are not included.
*     - Each HISREC structure contains the following components (all
*     taken from the corresponding items in the NDF History record):
*     DATE, COMMAND, USER and TEXT. If the history record was created by
*     the default NDF history writing mechanism, the TEXT component will
*     contain a list of environment parameter values used by (or created
*     by) the corresponding command, and another statement of the
*     software that performed the action.

*  Copyright:
*     Copyright (C) 2009-2014 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)

*  History:
*     20-JUN-2009 (DSB):
*        Original version, based on earlier provenance module. All public
*        function names have been changed, and they now use AST KeyMaps
*        rather than HDS structures for communicating provenance
*        information, (except that an HDS locator is still used for the
*        MORE component). Also, each ancestor in the PROVENANCE extension
*        now stores NDF history information that is speciifc to that
*        ancestor.
*     6-JUL-2009 (DSB):
*        Added facility for flagging ancestors as hidden, and producing a
*        deep copy of a Provenance structure.
*     20-JUL-2009 (DSB):
*        Added ndgWriteVotProv and ndgReadVotProv.
*     13-FEB-2010 (DSB):
*        Do not purge duplicate entries before leaving ndg1Rmprv, as
*        removing any such entries would upset subsequent ancestor indexing.
*     15-FEB-2010 (DSB):
*        - Purge duplicate entries before leaving ndgModifyProv.
*        - Report an error if inconsitent entries for the same ancestor
*        are found within a provenance structure.
*     13-APR-2010 (DSB):
*        - Modify ndg1TheSame so that CREATOR must match (in addition to
*        PATH and DATE) for two Prov structures to be considered the same.
*        - Remove trailing white space.
*      28-MAY-2010 (DSB):
*        Change ndg1TheSame to include checks on sameness of all ancestors.
*      21-JUN-2010 (DSB):
*         - Save calculated ProvId value in the Prov structure to avoid
*         recalculating it each time it is needed.
*         - Indicate that the ProvId value in a Prov is stale out of date
*         when the list of parents in the Prov is changed.
*      212-JUN-2010 (DSB):
*         Fix loop termination bug in ndg1Rmprv.
*      10-AUG-2010 (DSB):
*          The old hash function (Bernstein) was creating collisions. Swap to
*          FNV which seems to work better.
*      12-AUG-2010 (DSB):
*          Guard against using NULL Prov structures in ndg1ClearProvId
*          and ndg1GetProvId (NULL parent or child pointers can occur in
*          partially constructed Prov structure).
*      18-AUG-2010 (DSB):
*          Added ndgUnhashProv.
*      3-DEC-2010 (DSB):
*          Added debug facilities for tracking issue and release of Prov structures.
*      6-DEC-2010 (DSB):
*          When freeing a Prov structure, erase the temporary HDS object holding the
*          MORE info, rather than just annulling its locator.
*      7-DEC-2010 (DSB):
*          Create all NDG temporary HDS obejcts within a single component of the HDS
*          temporary file (avoids problems using datParen with temporary files).
*          Application code should now use ndgAntmp/NDG_ANTMP to free MORE locators
*          returned by NDG.
*      28-JAN-2011 (DSB):
*          Allow ndgReadProv to create a new empty provenance structure that is not
*          associated with an NDF.
*      2-MAR-2011 (DSB):
*          Do not clear the provid value in child structures if the provid in the
*          parent has already been cleared. This saves a lot of time in ndg1ClearProvId.
*      3-MAR-2011 (DSB):
*          In ndg1PurgeProvenance, speed up the way in which redundant Provs are removed.
*      8-MAR-2011 (DSB):
*          In ndg1PurgeProvenance, speed up the way in which redundant Provs are
*          identified (use quick sort to sort them into increasing ProvId
*          order then look for blocks of Provs with identical provid value).
*      9-MAR-2011 (DSB):
*          Change the format for the HDS provenance extension. Using the
*          old system large provenance systems occupied a lot of disk space,
*          and took a lot of time to read and write. The new system
*          avoids this by encoding descriptions of all ancestors into a
*          single array of integers, rather than into many different HDS
*          components. Both old and new formats can be read, but only the
*          new format is written.
*      10-MAR-2011 (DSB):
*          Guard against integer overlflow in ndg1CmpProv when finding
*          the difference between two integer provid values. This bug
*          caused the purging of duplicate ancestors to fail sometimes.
*      16-MAR-2011 (DSB):
*          - When decoding the integer values used to store a Prov
*          structure in ndg1DecodeProvData, allow for history text up to
*          100000 characters long. Put lower limits on other strings.
*          - Ensure subsequent string pointers are NULLified if an error
*          occurs whilst decoding Prov data.
*      13-MAY-2011 (DSB):
*          Fix memory leak in ndg1WriteProvenanceExtension.
*      13-JUNE-2011 (DSB):
*          Add a sticking paster ( a single bit shift) to get round a problem
*          in ndg1GetProvId that caused equal ProvId values to be generated
*          for different NDFs.
*      14-JUNE-2011 (DSB):
*          Equality of two hash codes does not guarantee that the two
*          hashed objects are equal, it just suggests they may be. So
*          when checking for equality of two NDFs, do not simply rely
*          on equality of hash codes - if the hash codes are equal go
*          on to perform further checks.
*      29-AUG-2011 (DSB):
*         - Change ndg1TheSame to exclude check on same number of children
*         (a single NDF may aquire more children throughout its life, but
*         it is still the same NDF).
*         - Change ndg1TheSame to compare equivalent parents. Previously,
*         this depended on the parents being stored in the same order in
*         the two Provs being compared. But the order is arbitrary.
*      4-JAN-2012 (DSB):
*         Added ndgAddProv/NDF_ADDPROV.
*      17-SEP-2012 (DSB):
*         Use KeyMaps instead of HDS to store the MORE information for
*         each ancestor. This limits what can be stored a little but
*         should speed up access a lot on some file systems. This required
*         a change in the API for ndgPutProv, ndgGetProv and ndgModifyProv.
*      20-SEP-2012 (DSB):
*         Modify ndgAddProv so that it can take the AUTOPROV environment
*         variable into account.
*      27-MAY-2013 (DSB):
*         Modify ndgAddProv so that it checks the input NDFs for
*         existing provenance info, rather than (incorrectly) checking the
*         output NDF.
*      2-MAY-2014 (DSB):
*         When encoding a Prov structure into a list of bytes, use a
*         special magic string to represent NULL pointers so that they
*         can be distinguished from zero-length strings when being read
*         back in again.
*/


/* Module Macros. */
/* -------------- */
/* HDS names for components used in the PROVENANCE extension of an NDF */
#define EXT_NAME "PROVENANCE"
#define DATE_NAME "DATE"
#define CREATOR_NAME "CREATOR"
#define ANCESTORS_NAME "ANCESTORS"
#define PARENTS_NAME "PARENTS"
#define PATH_NAME "PATH"
#define MORE_NAME "MORE"
#define ID_NAME "ID"
#define HASH_NAME "HASH"
#define HIST_NAME "HISTORY"
#define COMMAND_NAME "COMMAND"
#define USER_NAME "USER"
#define TEXT_NAME "TEXT"
#define HIDDEN_NAME "HIDDEN"
#define DATA_NAME "DATA"

/* HDS types for components used in the PROVENANCE extension of an NDF */
#define TEMP_TYPE "STARLINK_PROV"
#define EXT_TYPE "PROVENANCE"
#define ANCESTORS_TYPE "PROV"
#define MORE_TYPE "MORE"
#define HIST_TYPE "HISREC"

/* Max length allowed for each text component in the PROVENANCE extension
   of an NDF */
#define DATE_LEN 27
#define CREATOR_LEN 50
#define PATH_LEN 256

/* The key used to store a Provenance pointer in a KeyMap used to
   represent a NdgProvenance pointer. */
#define ID_KEY  "NDG_PROV"

/* Constants for the Fowler/Noll/Vo hash function. */
#define FNV1_32_INIT ( (unsigned int) 0x811c9dc5 )
#define FNV_32_PRIME ( (unsigned int) 0x01000193 )

/* The version number for the HDS Provenance encoding scheme that used
   HDS structures to store the MORE information. */
#define PDATA_VERSION_HDSMORE 1

/* The current version number for the HDS Provenance encoding scheme that
   uses AST KeyMap to store the MORE information. */
#define PDATA_VERSION 2

/* "int" value used to flag the end of the encoded provenance data */
#define END_FLAG -123456789

/* "char" value used to fill unused space at the end of the encoded
   provenance data */
#define PAD_FLAG 123

/* Compare two strings, allowing for NULL pointers. Note, "a" and "b"
   are evaluated multiple times in this macro, so they should not have any
   side effects. Macro value is non-zero if the strings match. */
#define CMP_STRING(a,b) ((!(a)&&!(b))||((a)&&(b)&&!strcmp((a),(b))))

/* The string used to flag null pointers in an encoded provenance structure. */
#define NULL_DATA "NDG_NULL"
#define NULL_DATA_LEN 9


/* Include files. */
/* -------------- */
/* Starlink packages. */
#include "star/hds.h"
#include "star/hds_fortran.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
#include "star/atl.h"
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "ndg.h"

/* C header files. */
#include <stdio.h>
#include <string.h>
#include <time.h>

/* Module Variables. */
/* ----------------- */

/* Used to communicate with ndg1Hout1 - a service routine called by
   ndfHout. The NDF library is written in Fortran and so is not
   thread-safe. So this module cannot be thread-safe, so there is no
   reason not to use global variables. */
static unsigned int history_hash;
static char *history_text = NULL;
static int history_length = 0;


/* Type Definitions. */
/* ----------------- */
/* A structure that stores a pointer to a dynamic memory area and its
   length. It is is used to communicate with source and sink functions used
   by the AstChannel class. */
typedef struct ChanData {
   char *mem;
   int memsize;
   char *linebuffer;
} ChanData;

/* A structure that stores history information for a single ancestor.
   These are just the same as the corresponding fields in the NDF
   history record.  */
typedef struct HistRec {
   char *date;
   char *command;
   char *user;
   char *text;
} HistRec;

/* A structure that stores the provenance information for a single
   ancestor NDF. */
typedef struct Prov {
   int provid;                 /* Hash code describing the content of
                                  this Prov and its ancestors */
   char *path;                 /* String holding the NDF path as returned
                                  by ndfMsg */
   char *date;                 /* String holding the formated UTC date & time
                                  at which the NDF's provenance was recorded */
   char *creator;              /* String describing the software that created
                                  the NDF */
   AstKeyMap *more;            /* A KeyMap holding extra information about the
                                  NDF */
   struct Prov **parents;      /* Array of pointers to the Prov structures for
                                  the direct parents of the NDF */
   struct Prov **children;     /* Array of pointers to the Prov structures for
                                  the direct children of the NDF */
   int nparent;                /* The length of the parents array */
   int nchild;                 /* The length of the children array */
   int index;                  /* The index within the ANCESTORS array, or
                                  -1 if the index is not currently known */
   int hhash;                  /* A hash code for the most recent NDF history
                                  record at the time provenance info was
                                  stored in the ancestor. */
   HistRec *hist_recs;         /* Array of history records */
   int nhrec;                  /* Number of history records */
   int hidden;                 /* Should the ancestor NDF be hidden? */

#ifdef NDG_DEBUG
   int id;
#endif

} Prov;


/* A structure that stores an ordered list of all Prov structures that
   are derived from a given NDF PROVENANCE extension. */
typedef struct Provenance {
   Prov *main;                 /* Pointer to the Prov structure for the main
                                  NDF */
   Prov **provs;               /* Array of pointers to Prov structures for the
                                  main NDF and all ancestors NDFs */
   int nprov;                  /* The length of the "provs" array */
} Provenance;


/* Prototypes for Private Functions. */
/* --------------------------------- */
static AstKeyMap *ndg1FormatProv( Provenance *, int, int, AstKeyMap *, int * );
static AstKeyMap *ndg1Hd2ky( HDSLoc *, int * );
static AstKeyMap *ndg1ShowProv( Prov *, int, AstKeyMap *, FILE *, int * )__attribute__((unused));
static AstObject *ndg1ReadObject( char *, int *, int * );
static HDSLoc *ndg1GtAnc( HDSLoc *, size_t *, int * );
static HDSLoc *ndg1Temp( const char *, int, const hdsdim *, int * );
static NdgProvenance *ndg1Encode( Provenance *, int * );
static Prov *ndg1CopyProv( Prov *, int * );
static Prov *ndg1FreeProv( Prov *, int * );
static Prov *ndg1MakeProv( int, const char *, const char *, const char *, int, AstKeyMap *, Provenance *, int, int * );
static Provenance *ndg1Decode( NdgProvenance *, const char *, int * );
static Provenance *ndg1FreeProvenance( Provenance *, int, int * );
static Provenance *ndg1MakeProvenance( Prov *, int * );
static Provenance *ndg1ReadOldProvenanceExtension( HDSLoc *, const char *, AstKeyMap *, const char *, int, int * );
static Provenance *ndg1ReadProvenanceExtension( HDSLoc *, const char *, AstKeyMap *, const char *, int, int * );
static Provenance *ndg1ReadProvenanceNDF( int, AstKeyMap *, const char *, int, int * );
static Provenance *ndg1ReadProvenanceXml( const char *, const char *, const char *, int * );
static char *ndg1DecodeProvData( char *, int, int, HDSLoc *, size_t, Provenance *, int * );
static char *ndg1EncodeProvData( char *, int, Prov *, Provenance *, size_t *, int * );
static char *ndg1GetTextComp( HDSLoc *, const char *, char *, size_t, int * );
static char *ndg1StoreCharData( char *, const void *, size_t, size_t *, int * );
static char *ndg1StoreObject( char *, AstObject *, size_t *, int * );
static char ndg1XmlSource( void *, int * );
static const char *ndg1ChSource( void );
static const char *ndg1Date( int * );
static const char *ndg1WriteProvenanceXml( Provenance *, int * );
static char *ndg1KeyMapSummary( AstKeyMap *, int, int *, int * );
static int *ndg1ParentIndicies( Prov *, Provenance *, int *, int *, int * );
static int ndg1CheckSameParents( Prov *, Prov *, int * );
static int ndg1FindAncestorIndex( Prov *, Provenance *, int * );
static int ndg1GetLogicalComp( HDSLoc *, const char *, int, int * );
static int ndg1GetProvId( Prov *, int * );
static int ndg1IntCmp( const void *, const void * );
static int ndg1IsWanted( AstXmlElement *, int * );
static int ndg1ProvCmp( const void *, const void * );
static int ndg1TheSame( Prov *, Prov *, int * );
static unsigned int ndg1HashFun( const char *,  unsigned int, int * );
static void ndg1AddHistKM( AstKeyMap *, const char *, Prov *, int * );
static void ndg1Antmp( HDSLoc **, int * );
static void ndg1Check( const char *, Prov *, AstKeyMap *, int * )__attribute__((unused));
static void ndg1ClearProvId( Prov *, int * );
static void ndg1ChSink( const char * );
static void ndg1Disown( Prov *, Prov *, int * );
static void ndg1FindAliens( Prov *, int * );
static void ndg1Hout1( int, char *const[], int * );
static void ndg1ParentChild( Prov *, Prov *, int, int * );
static void ndg1ParentChildIndex( Provenance *, int, int, int, int * );
static void ndg1PurgeProvenance( Provenance *, int * );
static void ndg1ReadHistRec( Prov *, int, int, int *, int * );
static void ndg1ResetIndices( Provenance *, int * );
static void ndg1Rmprv( Provenance *, int, int * );
static void ndg1StoreMore( Prov *, AstKeyMap *, int * );
static void ndg1WriteProvenanceExtension( Provenance *, HDSLoc *, int * );
static void ndg1WriteProvenanceNDF( Provenance *, int, int, int * );

/* Debug stuff.... */
static void ndg1DumpInfo( Prov *prov1, Prov *prov2, int *status );
static int static_badtype = 0;
static const char *static_badpath = NULL;
static int static_badprovid = -2;
static Provenance *static_provenance = NULL;
static int static_provid1 = -2;
static int static_provid2 = -2;


/* Facilities for debugging the use of provenance identifiers */
#ifdef NDG_DEBUG
static int nextid = 0;
static Prov **issued = NULL;
static void Issue( Prov * );
#define ISSUE( pv ) Issue( pv )
#define DEISSUE( pv ) Deissue( pv )
#else
#define ISSUE( pv )
#define DEISSUE( pv )
#endif


/* Public F77 wrapper functions. */
/* ============================= */

F77_SUBROUTINE(ndg_addprov)( INTEGER(indf), CHARACTER(fcreator), INTEGER(nndf),
                             INTEGER_ARRAY(ndfs), LOGICAL(autoprov),
                             INTEGER(status) TRAIL(fcreator) ){
/*
*+
*  Name:
*     NDG_ADDPROV

*  Purpose:
*     Record multiple input NDFs as ancestors in an output NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_ADDPROV( INDF, CREATOR, NNDF, NDFS, AUTOPROV, STATUS )

*  Description:
*     This routine reads provenance from the specified output NDF, and
*     then records each of the specified input NDFs as ancestors within
*     the output provenance. It then writes the modified provenance back
*     out to the output NDF.
*
*     It is a simplified wrapper for NDG_READPROV, NDG_PUTPROV AND
*     NDG_WRITEPROV. It is more restrictive than use of NDF_PUTPROV
*     since it stores no extra information ("MORE") with any of the
*     ancestors, and does not force any of the ancestors to be root
*     ancestors.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the output NDF.
*     CREATOR = CHARACTER * ( * ) (Given)
*        A text identifier for the software that created INDF (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*     NNDF = INTEGER (Given)
*        The number of input NDFs.
*     NDFS( NNDF ) = INTEGER (Given)
*        An array of identifiers for the input NDFs.
*     AUTOPROV = LOGICAL (Given)
*        If .TRUE., then the the AUTOPROV environment variable is
*        checked to see if provenance information should be stored in
*        the output NDF (see "Notes:" below). If .FALSE., the provenance
*        information is stored regardless of the environment variable.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the AUTOPROV argument is supplied .TRUE., then the following
*     checks are performed on the AUTOPROV environment variable: if the
*     environment variable is set to '1' then the input NDFs are added to the
*     provenance information in the output NDF. If the environment variable
*     is set to anything other than '1' then the output provenance is not
*     updated. If the environment variable is not set at all, the provenance
*     will be updated if one or more of the input NDFs contains a PROVENANCE
*     extension.
*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_CHARACTER(fcreator)
   GENPTR_INTEGER(nndf)
   GENPTR_INTEGER_ARRAY(ndfs)
   GENPTR_LOGICAL(autoprov)
   GENPTR_INTEGER(status)
   char *creator = NULL;

   creator = cnfCreim( fcreator, fcreator_length );
   ndgAddProv( *indf, creator, *nndf, ndfs, F77_ISTRUE( *autoprov ) ? 1 : 0,
               status );
   cnfFree( creator );
}

F77_SUBROUTINE(ndg_copyprov)( INTEGER(iprov), LOGICAL(cleanse),
                              INTEGER(iprov2), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_COPYPROV

*  Purpose:
*     Copy a Provenance structure, optionally removing any hidden ancestors.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_COPYPROV( IPROV, CLENSE, IPROV2, STATUS )

*  Description:
*     This routine produces a deep copy of the supplied Provenance
*     structure, and then optionally uses NDG_REMOVEPROV to remove any
*     hidden ancestors from the copy.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     CLEANSE = LOGICAL (Given)
*        If .TRUE., then any ancestors which have been hidden using
*        NDG_HIDEPROV are removed from the returned Provenance structure
*        (see NDG_REMOVEPROV).
*     IPROV2 = INTEGER (Returned)
*        An identifier for the new Provenance structure, which should be freed
*        using NDG_FREEPROV when no longer needed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*-
*/

   GENPTR_INTEGER(iprov)
   GENPTR_LOGICAL(cleanse)
   GENPTR_INTEGER(iprov2)
   GENPTR_INTEGER(status)
   *iprov2 = astP2I( ndgCopyProv( (NdgProvenance *) astI2P( *iprov ),
                                  F77_ISTRUE( *cleanse ) ? 1 : 0, status ) );
}

F77_SUBROUTINE(ndg_countprov)( INTEGER(iprov), INTEGER(count) ,
                               INTEGER(status) ){
/*
*+
*  Name:
*     NDG_COUNTPROV

*  Purpose:
*     Return the number of ancestors in a provenance structure.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_COUNTPROV( IPROV, COUNT, STATUS )

*  Description:
*     This routine returns the number of ancestors described in the
*     supplied provenance structure.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     COUNT = INTEGER (Returned)
*        The number of ancestors in the supplied provenance structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(count)
   GENPTR_INTEGER(status)
   *count = ndgCountProv( astI2P( *iprov ), status );
}

F77_SUBROUTINE(ndg_formatprov)( INTEGER(iprov), LOGICAL(base), INTEGER(fkeymap),
                                INTEGER(status) ){
/*
*+
*  Name:
*     NDG_FORMATPROV

*  Purpose:
*     Format the information in a provenance structure.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_FORMATPROV( IPROV, BASE, KEYMAP, STATUS )

*  Description:
*     This routine returns an AST KeyMap holding a set of text strings
*     containing information taken from the supplied provenance structure.
*
*     The returned KeyMap has an entry with key "0" that describes the NDF
*     from which the provenance was read. It also has an entry describing
*     each ancestor NDF. These entries have keys "1", "2", "3", etc, up to
*     the number of ancestors in the NDF.
*
*     Each of these entries contains a pointer to another AST KeyMap
*     which may contain any subset of the following entries (all of which,
*     except for HISTORY, are strings):
*
*     "ID" - the integer index within the ancestors array (zero for the
*            main NDF).
*
*     "PATH" - The full path or base name for the NDF (see "base").
*
*     "DATE" - The date of creation of the NDF.
*
*     "CREATOR" - The software item that created the NDF.
*
*     "PARENTS" - A comma-separated list of indices into the ancestors
*            array that identifies the direct parents of the NDF.
*
*     "MORE" - A summary of the contents of the MORE structure associated
*            with the NDF.
*
*     "HISTORY" - A vector entry holding one or more KeyMaps. Each
*            KeyMap contains items that describe an action performed on
*            the ancestor. The actions are stored in chronological order
*            within the vector entry. The last KeyMap in the vector
*            describes the action that created the ancestor NDF. Any
*            earlier KeyMaps in the vector describe any subsequent actions
*            performed on the ancestor NDF prior to it being used in the
*            creation of its parent. Each KeyMap contains the following
*            scalar character entries (all taken from the corresponding
*            record in the NDF HISTORY component):
*
*            - "DATE": The date and time of the action (e.g. "2009-JUN-24
*                      14:00:53.752" ).
*            - "COMMAND": An indication of the command that performed the
*                      action (e.g. "WCSATTRIB (KAPPA 1.10-6)" ).
*            - "USER": The user name that performed the action
*                      (e.g. "dsb").
*            - "TEXT": The full text of the NDF history record. This is
*                      arbitrary, but for NDFs created by Starlink
*                      software it will usually include environment
*                      parameter values, and the full path of the command
*                      that performed the action.
*
*     Finally, the returned KeyMap has an entry with key "MXLEN" that is
*     again a pointer to another KeyMap with the same entries listed
*     above (except that it has no "HISTORY" entry). However, this time
*     the entries are integers, not strings, and holds the maximum field
*     width used to format the strings.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     BASE = LOGICAL (Given)
*        If .TRUE., then the PATH field in the returned KeyMap holds the
*        base name of each NDF rather than the full path.
*     KEYMAP = INTEGER (Returned)
*        A pointer to the returned AST KeyMap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_LOGICAL(base)
   GENPTR_INTEGER(fkeymap)
   GENPTR_INTEGER(status)

   AstKeyMap *keymap = NULL;
   if( *status == SAI__OK ) {
      ndgFormatProv( astI2P( *iprov ), F77_ISTRUE( *base ), &keymap, status );
   }
   *fkeymap = astP2I( keymap );
}

F77_SUBROUTINE(ndg_freeprov)( INTEGER(iprov), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_FREEPROV

*  Purpose:
*     Free a structure holding provenance information.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_FREEPROV( IPROV, STATUS )

*  Description:
*     This routine frees the resources used to hold a provenance structure.

*  Arguments:
*     IPROV = INTEGER (Given and Returned)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV. Returned holding
*        NDG__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*-
*/

   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(status)
   *iprov = astP2I( ndgFreeProv( (NdgProvenance *) astI2P( *iprov ),
                                 status ) );
}

F77_SUBROUTINE(ndg_getprov)( INTEGER(iprov), INTEGER(ianc),
                             INTEGER(km), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_GETPROV

*  Purpose:
*     Create a KeyMap holding information about an ancestor.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_GETPROV( IPROV, IANC, KM, STATUS )

*  Description:
*     This routine returns information about a specified ancestor in the
*     supplied provenance structure.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF for which information should be
*        returned. A value of zero will result in information being returned
*        that describes the NDF from which the provenance information was
*        read. Otherwise, the IANC value is used as an index into the
*        ANCESTORS array. No error is reported if IANC is too large, but a
*        null identifier will be returned as the function value.
*     KM = INTEGER (Returned)
*        A pointer to an AST KeyMap containing entries with the following
*        keys and values:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at
*          which the provenance information for the ancestor NDF was
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "PARENTS": A 1D vector of integers that are the indices of the
*          immediate parents of the ancestor.
*        - "MORE": A KeyMap containing any extra information that has
*          been stored with the ancestor.
*        - "HISTORY": A vector entry holding one or more KeyMaps. Each
*          KeyMap contains items that describe an action performed on
*          the ancestor. The actions are stored in chronological order
*          within the vector entry. The last KeyMap in the vector
*          describes the action that created the ancestor NDF. Any
*          earlier KeyMaps in the vector describe any subsequent actions
*          performed on the ancestor NDF prior to it being used in the
*          creation of its parent. Each KeyMap contains the following
*          scalar character entries (all taken from the corresponding
*          record in the NDF HISTORY component):
*          - "DATE": The date and time of the action (e.g. "2009-JUN-24
*            14:00:53.752" ).
*          - "COMMAND": An indication of the command that performed the
*            action (e.g. "WCSATTRIB (KAPPA 1.10-6)" ).
*          - "USER": The user name that performed the action (e.g. "dsb").
*          - "TEXT": The full text of the NDF history record. This is
*            arbitrary, but for NDFs created by Starlink software it will
*            usually include environment parameter values, and the full
*            path of the command that performed the action.
*
*        If the specified ancestor does not have any of these items of
*        information, then the corresponding entry will not be present
*        in the returned KeyMap. For instance, if the ancestor has no
*        immediate parent NDFs, then the "PARENTS" entry will not be
*        present in the KeyMap. A NULL pointer will be returned if the
*        NDF has no provenance extension, or if "ianc" is outside the
*        bounds of the ANCESTORS array (and is not zero). The returned
*        KeyMap pointer should be annulled when it is no longer needed,
*        either by calling astAnnul explicitly, or by relying on astEnd
*        to annul it (together with all the other AST Objects created in
*        the current AST Object context).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(ianc)
   GENPTR_INTEGER(km)
   GENPTR_INTEGER(status)

   *km = astP2I( ndgGetProv( astI2P( *iprov ), *ianc, status ) );
}

F77_SUBROUTINE(ndg_hideprov)( INTEGER(iprov), INTEGER(ianc),
                              INTEGER(status) ){
/*
*+
*  Name:
*     NDG_HIDEPROV

*  Purpose:
*     Hide an ancestor in a provenance structure.

*  Invocation:
*     CALL NDG_HIDEPROV( IPROV, IANC, STATUS )

*  Description:
*     This function flags a specified ancestor as "hidden". The only
*     effect this has is that the ancestor will not be included in
*     Provenance structures created by the NDG_COPYPROV function.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF to be hidden. The value is used as an
*        index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 1 (the main NDF cannot be
*        hidden).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(ianc)
   GENPTR_INTEGER(status)
   ndgHideProv( astI2P( *iprov ), *ianc, status );
}

F77_SUBROUTINE(ndg_ishiddenprov)( INTEGER(iprov), INTEGER(ianc),
                                  LOGICAL(hidden), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_ISHIDDENPROV

*  Purpose:
*     See if an ancestor in a provenance structure is hidden.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_ISHIDDENPROV( IPROV, IANC, HIDDEN, STATUS )

*  Description:
*     This function returns a TRUE. value for HIDDEN if the specified
*     ancestor has been hidden. See NDG_HIDEPROV and NDG_COPYPROV.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF to be checked. The value is used as
*        an index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 0.
*     HIDDEN = LOGICAL (Returned)
*        .TRUE. if the ancestor is hidden.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(ianc)
   GENPTR_LOGICAL(hidden)
   GENPTR_INTEGER(status)
   *hidden = ndgIsHiddenProv( astI2P( *iprov ), *ianc, status ) ?
             F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndg_modifyprov)( INTEGER(iprov), INTEGER(ianc), INTEGER(km),
                                INTEGER(status) ){
/*
*+
*  Name:
*     NDG_MODIFYPROV

*  Purpose:
*     Modify the information stored for a particular ancestor.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_MODIFYPROV( IPROV, IANC, KM, STATUS )

*  Description:
*     This routine modifies the information stored for a given ancestor
*     in the supplied provenance structure. The new values to store
*     are supplied in an AST KeyMap such as returned by NDG_GETPROV.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF for which information should be
*        modified. A value of zero will result in information about the NDF
*        specified by INDF being modified. Otherwise, the IANC value
*        is used as an index into the ANCESTORS array. An error is reported
*        if IANC is too large.
*     KM = INTEGER (Given)
*        A pointer to an AST KeyMap containing the values to store. Entries
*        with the following keys are recognised:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at
*          which the provenance information for the ancestor NDF was
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": A KeyMap containing extra information to store with
*          the ancestor.
*
*        If the "DATE", "CREATOR" or "MORE" components are missing then
*        corresponding item of information will be deleted from the
*        provenance extension. An error is reported if the supplied KeyMap
*        has no "PATH" entry. Note, the PARENTS list and HISTORY information
*        stored with the specified ancestor cannot be modified (any "PARENTS"
*        or "HISTORY" component in the supplied HDS structure will be
*        ignored).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(ianc)
   GENPTR_INTEGER(km)
   GENPTR_INTEGER(status)

   if( *status != SAI__OK ) return;
   ndgModifyProv( astI2P( *iprov ), *ianc, astI2P( *km ), status );

}

F77_SUBROUTINE(ndg_putprov)( INTEGER(iprov), INTEGER(indf), INTEGER(more),
                             LOGICAL(isroot), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_PUTPROV

*  Purpose:
*     Add an NDF to the list of ancestors.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_PUTPROV( IPROV, INDF, MORE, ISROOT, STATUS )

*  Description:
*     This routine modifies the supplied provenance structure to indicate
*     that a given NDF was used in the creation of the NDF associated with
*     the supplied provenance structure.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     INDF = INTEGER (Given)
*        An identifier for an NDF that is to be added into the list of
*        ancestor NDFs in the supplied provenance information.
*     MORE = INTEGER (Given)
*        A pointer to an AstKeyMap holding arbitrary additional information
*        about the new ancestor NDF, and how it was used in the creation of
*        the output NDF.
*     ISROOT = LOGICAL (Given)
*        If TRUE, then the new ancestor NDF will be treated as a root
*        NDF. That is, any provenance information in the supplied NDF is
*        ignored. If FALSE, then any provenance information in the NDF is
*        copied into the supplied provenance structure. The new ancestor NDF
*        is then only a root NDF if it contains no provenance information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(more)
   GENPTR_LOGICAL(isroot)
   GENPTR_INTEGER(status)

   if( *status != SAI__OK ) return;
   ndgPutProv( astI2P( *iprov ), *indf, astI2P( *more ),
               F77_ISTRUE( *isroot ), status );
}

F77_SUBROUTINE(ndg_readprov)( INTEGER(indf), CHARACTER(fcreator),
                              INTEGER(iprov), INTEGER(status)
                              TRAIL(fcreator) ){
/*
*+
*  Name:
*     NDG_READPROV

*  Purpose:
*     Read the provenance information from an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_READPROV( INDF, CREATOR, IPROV, STATUS )

*  Description:
*     This function reads the information stored in the "PROVENANCE"
*     extension of an NDF, storing it in a memory-resident structure for
*     faster access. An identifier for this structure is returned, and
*     can be passed to other NDG provenance functions to manipulate the
*     contents of the structure.
*
*     If the NDF has no provenance information (for instance, if it is a
*     newly created NDF), or if no NDF is supplied, the returned structure
*     will contain just the supplied creator name (which may be blank), and
*     an empty ancestor list.
*
*     The structure should be freed when it is no longer needed by
*     calling NDG_FREEPROV.
*
*     The structure should be freed when it is no longer needed by
*     calling NDG_FREEPROV.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information
*        to be read. This may be NDF__NOID, in which case a new provenance
*        structure with the supplied creator name and an empty ancestor
*        list will be created and returned.
*     CREATOR = CHARACTER * ( * ) (Given)
*        A text identifier for the software that created INDF (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*     IPROV = INTEGER (Returned)
*        An identifier for the structure holding the provenance information
*        read from the NDF. NDG__NULL is returned if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_CHARACTER(fcreator)
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(status)
   char *creator = NULL;

   creator = cnfCreim( fcreator, fcreator_length );
   *iprov = astP2I( ndgReadProv( *indf, creator, status ) );
   cnfFree( creator );
}

F77_SUBROUTINE(ndg_removeprov)( INTEGER(iprov), INTEGER(nanc),
                                INTEGER_ARRAY(anc), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_REMOVEPROV

*  Purpose:
*     Remove one or more ancestors from a provenance structure.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_REMOVEPROV( IPROV, NANC, IANC, STATUS )

*  Description:
*     This routine removes one or more ancestors from the supplied
*     provenance structure. The direct parents of the removed ancestor
*     are assigned to the direct children of the removed ancestor. Note,
*     any history records stored in the removed ancestors are lost.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     NANC = INTEGER (Given)
*        The length of the ANC array.
*     ANC( * )  = INTEGER (Given)
*        An array holding the indices of the ancestor NDFs to be removed.
*        Each supplied value must be at least 1, and must be no more than
*        the number of ancestors in the provenance extension (as returned
*        by NDG_COUNTPROV). An error is reported otherwise. The supplied
*        list is sorted into decreasing order before use so that the
*        highest index ancestor is removed first.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(nanc)
   GENPTR_INTEGER_ARRAY(anc)
   GENPTR_INTEGER(status)

   ndgRemoveProv( astI2P( *iprov ), *nanc, anc, status );

}

F77_SUBROUTINE(ndg_rootprov)( INTEGER(iprov), INTEGER(km), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_ROOTPROV

*  Purpose:
*     Identify the root ancestors in a provenance structure.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_ROOTPROV( IPROV, KM, STATUS )

*  Description:
*     This routine searches the supplied provenance structure for root
*     ancestors, and returns information about them. An ancestor is a root
*     ancestor if it does not itself have any ancestors.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     KM = INTEGER (Returned)
*        A pointer to an AST KeyMap containing an entry for each root
*        ancestor. The key associated with each entry is the path to the
*        NDF and the value of the entry is an integer that gives the
*        position of the root ancestor within the list of all ancestors.
*        This integer value can be supplied to ndgGetProv in order to get
*        further information about the root ancestor. The first ancestor NDF
*        has an index of one. An index of zero refers to the NDF from which
*        the provenance information was read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(km)
   GENPTR_INTEGER(status)
   *km = astP2I( ndgRootProv( astI2P( *iprov ), status ) );
}

F77_SUBROUTINE(ndg_unhashprov)( INTEGER(iprov), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_UNHASHPROV

*  Purpose:
*     Clear the hash code describing the creation of the Provenance.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_UNHASHPROV( IPROV, STATUS )

*  Description:
*     Each ancestor in a Provenance structure may contain a copy of the
*     History information stored in the associated ancestor NDF. Storing
*     the complete History component from each ancestor NDF would be very
*     wastefull since the NDF History component will usually contain not
*     only records of operations performed on the ancestor NDF, but also
*     all History records inherited from the "primary" NDF (i.e. the NDF
*     from which the ancestor was propagated). Since these inherited
*     History records will already be stored with other ancestors in the
*     Provenance structure, it is not necessary to store them again.
*     However, this means that when we add a new parent into a Provenance
*     structure using NDG_PUTPROV, NDG needs some way of knowing which
*     records within the new NDF are unique to the NDF (and should thus
*     be stored in the Provenance structure), and which were inherited
*     from earlier ancestors (and will thus already be stored in the
*     Provenance structure). The solution is for each PROVENANCE extension
*     to include a "creator" hash code for the History record that describes
*     the creation of the NDF. When an NDF is supplied to NDG_PUTPROV, each
*     History record, starting with the most recent, is copied from the
*     NDF into the Provenance structure, until a History record is found
*     which has a hash code equal to the creator hash code in the NDF. The
*     copying of history records then stops since all earlier history
*     records will already be present in the Provenance structure.
*
*     This routine clears the creator hash code in the supplied
*     Provenance structure, so that a new one will be calculated when the
*     Provenance structure is written to an NDF using NDG_WRITEPROV. This
*     is useful for instance if the Provenance was written to the NDF
*     using NDG_WRITEPROV before the NDF History record was completed. In
*     this case, you would probably want to re-read the Provenance from
*     the NDF, use this function to clear the creator hash code, and then
*     re-write the Provenance to the NDF, thus forcing a new creator hash
*     code to be stored in the NDF.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*-
*/

   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(status)
   ndgUnhashProv( (NdgProvenance *) astI2P( *iprov ), status );
}

F77_SUBROUTINE(ndg_unhideprov)( INTEGER(iprov), INTEGER(ianc),
                                INTEGER(status) ){
/*
*+
*  Name:
*     NDG_UNHIDEPROV

*  Purpose:
*     Un-hide an ancestor in a provenance structure.

*  Invocation:
*     CALL NDG_UNHIDEPROV( IPROV, IANC, STATUS )

*  Description:
*     This function ensures that a given ancestor is not flagged as
*     "hidden". See NDG_HIDEPROV and NDG_COPYPROV.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by ndgReadProv
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF to be un-hidden. The value is used as
*        an index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - No error is reported if the specified ancestor is not currently
*     hidden (in which case this function returns without action).
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(ianc)
   GENPTR_INTEGER(status)
   ndgUnhideProv( astI2P( *iprov ), *ianc, status );
}

F77_SUBROUTINE(ndg_writeprov)( INTEGER(iprov), INTEGER(indf), INTEGER(whdef),
                               INTEGER(status) ){
/*
*+
*  Name:
*     NDG_WRITEPROV

*  Purpose:
*     Write provenance information to an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_WRITEPROV( IPROV, INDF, WHDEF, STATUS )

*  Description:
*     This routine writes the contents of the supplied provenance
*     structure out to a given NDF, replacing any existing provenance
*     information in the NDF.

*  Arguments:
*     IPROV = INTEGER (Given)
*        An identifier for a structure holding the provenance information
*        read from an NDF, as returned by NDG_READPROV.
*     INDF = INTEGER (Given)
*        Identifier for the NDF in which to store the provenance
*        information.
*     WHDEF = INTEGER (Given)
*        The correct recording of history information within the
*        PROVENANCE extension requires that the current history record
*        within the supplied NDF at the time this function is called,
*        describes the creation of the NDF. Very often, an application
*        will not itself add any history to the NDF, but will instead
*        rely on the automatic recording of default history provided by
*        the NDF library. Normally, default history is recorded when the
*        NDF is released from the NDF system (e.g. using NDF_ANNUL or
*        NDF_END). So if this function is called prior to the release of
*        the NDF (which it normally will be), then the default history
*        information will not yet have been recorded, resulting in
*        incorrect information being stored in the PROVENANCE extension.
*        For this reason, the WHDEF argument is supplied. If it is set
*        to .TRUE., a check is made to see if default history has already
*        been stored in the NDF. If .FALSE., default history is stored
*        in the NDF before going on to create the PROVENANCE extension.
*        Applications that do not use the default history recording
*        mechanism, but instead store their own history information,
*        should supply .FALSE. for WHDEF, and should also ensure that
*        history information has been stored in the NDF before calling
*        this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/
   GENPTR_INTEGER(iprov)
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(whdef)
   GENPTR_INTEGER(status)
   ndgWriteProv( astI2P( *iprov ), *indf, *whdef,  status );
}





/* Public C functions. */
/* =================== */

void ndgAddProv( int indf, const char *creator, int nndf, int *ndfs,
                 int autoprov, int *status ){
/*
*+
*  Name:
*     ndgAddProv

*  Purpose:
*     Record multiple input NDFs as ancestors in an output NDF.

*  Invocation:
*     ndgAddProv( int indf, const char *creator, int nndf, int *ndfs,
*                 int autoprov, int *status )

*  Description:
*     This routine reads provenance from the specified output NDF, and
*     then records each of the specified input NDFs as ancestors within
*     the output provenance. It then writes the modified provenance back
*     out to the output NDF.
*
*     It is a simplified wrapper for ndgReadProv, ndgPutProv and
*     ndgWriteProv. It is more restrictive than use of ndgPutProv
*     since it stores no extra information ("MORE") with any of the
*     ancestors, and does not force any of the ancestors to be root
*     ancestors.

*  Arguments:
*     indf
*        An identifier for the output NDF.
*     creator
*        A text identifier for the software that created INDF (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*        This value is only used if the the NDF does not contain any
*        existing provenance information.
*     nndf
*        The number of input NDFs.
*     ndfs
*        A pointer to an array of identifiers for the input NDFs.
*     autoprov
*        If non-zero, then the the AUTOPROV environment variable is
*        checked to see if provenance information should be stored in
*        the output NDF (see "Notes:" below). If zero, the provenance
*        information is stored regardless of the environment variable.
*     status
*        The global status.

*  Notes:
*     - If the "autoprov" argument is supplied non-zero, then the following
*     checks are performed on the AUTOPROV environment variable: if the
*     environment variable is set to '1' then the input NDFs are added to the
*     provenance information in the output NDF. If the environment variable
*     is set to anything other than '1' then the output provenance is not
*     updated. If the environment variable is not set at all, the provenance
*     will be updated if one or more of the input NDFs contains a PROVENANCE
*     extension.

*-
*/

/* Local Variables: */
   NdgProvenance *prov;
   const char *autopv;
   int i;
   int store;

/* Return if an error has occurred. */
   if( *status != SAI__OK ) return;

/* Get the value of environment variable AUTOPROV. */
   if( autoprov ) {
      autopv = getenv( "AUTOPROV" );
   } else {
      autopv = "1";
   }

/* We only propagate provenance if AUTOPROV is set to '1', or if AUTOPROV
   is unset and at least one input NDF had a provenance extension. */
   if( !autopv || !strcmp( autopv, "1" ) ) {

/* Get the provenance info from the output NDF. */
      prov = ndgReadProv( indf, creator, status );

/* Initialise a flag saying whether the provenance info should be stored
   in the output NDF. */
      store = ( autopv != NULL );

/* Loop round, adding each input NDF as an ancestor into the output
   provenance info. */
      for( i = 0; i < nndf; i++ ) {
         ndgPutProv( prov, ndfs[ i ], NULL, 0, status );

/* If any of the input NDFs contained explicit Provenance info then we
   must always store the output provenance. */
         if( !store ) ndfXstat( ndfs[ i ], EXT_NAME, &store, status );
      }

/* If autopv is set, or if any of the input NDFs had provenance info, write
   the provenance info back out to the output NDF. Ensure default NDF history
   is written to the NDF before writing the provenance info. */
      if( store ) ndgWriteProv( prov, indf, 1, status );

/* Free the provenance info. */
      prov = ndgFreeProv( prov, status );
   }
}

NdgProvenance *ndgCopyProv( NdgProvenance *prov, int cleanse, int *status ){
/*
*+
*  Name:
*     ndgCopyProv

*  Purpose:
*     Copy a Provenance structure, optionally removing any hidden ancestors.

*  Invocation:
*     NdgProvenance *ndgCopyProv( NdgProvenance *prov, int cleanse,
*                                 int *status )

*  Description:
*     This function produces a deep copy of the supplied Provenance
*     structure, and then optionally uses ndgRemoveProv to remove any
*     hidden ancestors from the copy. A pointer to the copy is returned.

*  Arguments:
*     prov
*        A pointer to the provenance information to be copied.
*     cleanse
*        If non-zero, then any ancestors which have been hidden using
*        ndgHideProv are removed from the returned Provenance structure
*        (see ndgRemoveProv).
*     status
*        The global status.

*  Returned Value:
*     A pointer to the new Provenance structure, which should be freed
*     using ndgFreeProv when no longer needed.
*-
*/

/* Local Variables: */
   NdgProvenance *result = NULL;
   Prov *prov1;
   Prov *prov2;
   Provenance *newprov;
   Provenance *provenance;
   int *old_status;
   int i;
   int ianc;
   int index;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgCopyProv", status );
   if( provenance ) {

/* Allocate memory and store a copy of the supplied provenance
   structure in it. */
      newprov = astStore( NULL, provenance, sizeof( Provenance ) );
      if( *status == SAI__OK ){

/* For safety, nullify all pointers in the new Provenance. */
         newprov->main = NULL;
         newprov->provs = NULL;

/* Allocate an array to hold the Prov pointers. */
         newprov->provs = astMalloc( sizeof( Prov * )*( newprov->nprov ) );

/* Loop round each Prov structure . */
         for( ianc = 0; ianc < newprov->nprov; ianc++ ) {

/* Create a deep copy of the Prov structure. */
            newprov->provs[ ianc ] = ndg1CopyProv( provenance->provs[ ianc ],
                                                   status );

/* If this Prov structure describes the main NDF, store a pointer to it
   in the new Provenance structure. */
            if( provenance->provs[ ianc ] == provenance->main ) {
               newprov->main = newprov->provs[ ianc ];
            }
         }

/* Now loop through each ancestor, storing pointers to the children and
   parents in the new provenance structure. */
         for( ianc = 0; ianc < newprov->nprov; ianc++ ) {

/* Get pointers to the old and new Prov structures describing the ancestor
   with index "ianc". */
            prov1 = provenance->provs[ ianc ];
            prov2 = newprov->provs[ ianc ];

/* For each child of the "prov1" ancestor,  find its index by searching
   the old provenance for the same pointer. Then store a pointer to the
   corresponding Prov structure within the new provenance. */
            for( i = 0; i < prov1->nchild; i++ ) {
               index = ndg1FindAncestorIndex( prov1->children[ i ],
                                              provenance, status );
               prov2->children[ i ] = newprov->provs[ index ];
            }

/* Do the same for the parents. */
            for( i = 0; i < prov1->nparent; i++ ) {
               index = ndg1FindAncestorIndex( prov1->parents[ i ],
                                              provenance, status );
               prov2->parents[ i ] = newprov->provs[ index ];
            }
         }

/* If required, remove hidden ancestors. */
         if( cleanse ) {

/* Work backwards through the ancestors list. */
            for( ianc = newprov->nprov - 1; ianc >= 0; ianc-- ) {

/* If the ancestor has been hidden, remove it. */
               if( newprov->provs[ ianc ]->hidden ) ndg1Rmprv( newprov, ianc,
                                                               status );
            }

/* Indicate that the indices of the parents of each prov structure needs
   to be re-calculated to take account of the removal of the ancestors. */
            ndg1ResetIndices( newprov, status );
         }
      }

/* If no error has occurred, encode the pointer to the new Provenance into
   an AST KeyMap pointer to be returned. Otherwise free the new
   Provenance structure. */
      if( *status == SAI__OK ) {
         result = ndg1Encode( newprov, status );
      } else {
         newprov = ndg1FreeProvenance( newprov, 1, status );
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

/* Return the result. */
   return result;
}

int ndgCountProv( NdgProvenance *prov, int *status ){
/*
*+
*  Name:
*     ndgCountProv

*  Purpose:
*     Return the number of ancestors in a provenance structure.

*  Invocation:
*     result = ndgCountProv( NdgProvenance *prov, int *status )

*  Description:
*     This function returns the number of ancestors described in the
*     supplied provenance structure.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     status
*        The global status.

*  Returned Value:
*     The number of ancestor NDFs stored in the supplied provenance
*     structure, or zero if an error occurs.
*-
*/

/* Local Variables: */
   Provenance *provenance;
   int *old_status;
   int result;

/* Initialise. */
   result = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgCountProv", status );
   if( provenance ) {

/* The number of ancestors is equal to the number of Prov structures in
   the Provenance structure, minus one (because there is a Prov structure
   for the NDF itself, as well as for all the ancestors). */
      result = provenance->nprov - 1;
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

/* Return the result. */
   return result;
}

void ndgFormatProv( NdgProvenance *prov, int base, AstKeyMap **keymap,
                    int *status ){
/*
*+
*  Name:
*     ndgFormatProv

*  Purpose:
*     Format the information in a provenance structure.

*  Invocation:
*     void ndgFormatProv( NdgProvenance *prov, int base, AstKeyMap **keymap,
*                         int *status )

*  Description:
*     This function returns an AST KeyMap holding a set of text strings
*     containing information taken from the supplied provenance structure.
*
*     The returned KeyMap has an entry with key "0" that describes the NDF
*     from which the provenance was read. It also has an entry describing
*     each ancestor NDF. These entries have keys "1", "2", "3", etc, up to
*     the number of ancestors in the NDF.
*
*     Each of these entries contains a pointer to another AST KeyMap
*     which may contain any subset of the following entries (all of which
*     are strings):
*
*     "ID" - the integer index within the ancestors array (zero for the
*            main NDF).
*
*     "PATH" - The full path or base name for the NDF (see "base").
*
*     "DATE" - The date of creation of the NDF.
*
*     "CREATOR" - The software item that created the NDF.
*
*     "PARENTS" - A comma-separated list of indices into the ancestors
*            array that identifies the direct parents of the NDF.
*
*     "MORE" - A summary of the contents of the MORE structure associated
*            with the NDF.
*
*     "HISTORY" - A vector entry holding one or more KeyMaps. Each
*            KeyMap contains items that describe an action performed on
*            the ancestor. The actions are stored in chronological order
*            within the vector entry. The last KeyMap in the vector
*            describes the action that created the ancestor NDF. Any
*            earlier KeyMaps in the vector describe any subsequent actions
*            performed on the ancestor NDF prior to it being used in the
*            creation of its parent. Each KeyMap contains the following
*            scalar character entries (all taken from the corresponding
*            record in the NDF HISTORY component):
*
*            - "DATE": The date and time of the action (e.g. "2009-JUN-24
*                      14:00:53.752" ).
*            - "COMMAND": An indication of the command that performed the
*                      action (e.g. "WCSATTRIB (KAPPA 1.10-6)" ).
*            - "USER": The user name that performed the action
*                      (e.g. "dsb").
*            - "TEXT": The full text of the NDF history record. This is
*                      arbitrary, but for NDFs created by Starlink
*                      software it will usually include environment
*                      parameter values, and the full path of the command
*                      that performed the action.
*
*     Finally, the returned KeyMap has an entry with key "MXLEN" that is
*     again a pointer to another KeyMap with the same entries listed
*     above (except that it has no "HISTORY" entry). However, this time
*     the entries are integers, not strings, and holds the maximum field
*     width used to format the strings.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     base
*        If non-zero, then the PATH field in the returned KeyMap holds the
*        base name of each NDF rather than the full path.
*     keymap
*        A location at which to returned a pointer to the returned AST KeyMap.
*     status
*        The global status.
*-
*/

/* Local variables: */
   AstKeyMap *anckey = NULL;
   AstKeyMap *mxkey = NULL;
   Provenance *provenance = NULL;
   char key[10];
   int *old_status;
   int i;

/* Initialise */
   *keymap = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgFormatProv", status );
   if( provenance ) {

/* Create the returned KeyMap. */
      *keymap = astKeyMap( " " );

/* Create a KeyMap to hold the field widths. */
      mxkey = astKeyMap( " " );

/* Loop round every ancestor. */
      for( i = 0; i < provenance->nprov;  i++ ) {

/* Create a new KeyMap holding the formatted details of the ancestor.
   This also updates the maximum field widths for each field (held in
   "mxkey"). */
         anckey = ndg1FormatProv( provenance, i, base, mxkey, status );

/* Put the "anckey" KeyMap into the returned KeyMap, using the formatted
   integer index as the key. */
         sprintf( key, "%d", i );
         astMapPut0A( *keymap, key, anckey, NULL );

/* Free resources specific to this ancestor. */
         (void) astAnnul( anckey );
      }

/* Put the "mxkey" KeyMap into the returned KeyMap. */
      astMapPut0A( *keymap, "MXLEN", mxkey, NULL );

/* Free resources. */
      mxkey = astAnnul( mxkey );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

NdgProvenance *ndgFreeProv( NdgProvenance *prov, int *status ){
/*
*+
*  Name:
*     ndgFreeProv

*  Purpose:
*     Free a structure holding provenance information.

*  Invocation:
*     NdgProvenance *ndgFreeProv( NdgProvenance *prov, int *status )

*  Description:
*     This function frees the resources used to hold a provenance
*     structure.

*  Arguments:
*     prov
*        A pointer to the provenance information to be freed.
*     status
*        The global status.

*  Returned Value:
*     A NULL pointer is returned.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*-
*/

/* Local Variables: */
   Provenance *provenance;
   int *old_status;

/* Return if no identifier was supplied. */
   if( !prov ) return NULL;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgFreeProv", status );

/* Free the provenance structure. */
   provenance = ndg1FreeProvenance( provenance, 1, status );

/* Annul the KeyMap used to store the provenance pointer. */
   prov = (NdgProvenance*) astAnnul( (AstKeyMap *) prov );

/* Re-instate the original AST status variable. */
   astWatch( old_status );

/* Return the NULL pointer. */
   return prov;
}

AstKeyMap *ndgGetProv( NdgProvenance *prov, int ianc, int *status ){
/*
*+
*  Name:
*     ndgGetProv

*  Purpose:
*     Create a KeyMap holding information about an ancestor.

*  Invocation:
*     result = ndgGetProv( NdgProvenance *prov, int ianc, int *status )

*  Description:
*     This function returns information about a specified ancestor in the
*     supplied provenance structure.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     ianc
*        The index of the ancestor NDF for which information should be
*        returned. A value of zero will result in information being returned
*        that describes the NDF from which the provenance information was
*        read. Otherwise, the "ianc" value is used as an index into the
*        ANCESTORS array. No error is reported if "ianc" is too large, but a
*        NULL pointer will be returned as the function value.
*     status
*        The global status.

*  Returned Value:
*     A pointer to an AST KeyMap containing entries with the following
*     keys and values:
*
*     - "PATH": A string holding the path of the ancestor NDF.
*     - "DATE": A string holding the formatted UTC date and time at
*       which the provenance information for the ancestor NDF was
*       recorded.
*     - "CREATOR": A string identifying the software that created the
*       ancestor NDF.
*     - "PARENTS": A 1D vector of integers that are the indices of
*       the immediate parents of the ancestor.
*     - "MORE": A KeyMap containing any extra information that has been
*       stored with the ancestor.
*     - "HISTORY": A vector entry holding one or more KeyMaps. Each
*       KeyMap contains items that describe an action performed on
*       the ancestor. The actions are stored in chronological order
*       within the vector entry. The last KeyMap in the vector
*       describes the action that created the ancestor NDF. Any
*       earlier KeyMaps in the vector describe any subsequent actions
*       performed on the ancestor NDF prior to it being used in the
*       creation of its parent. Each KeyMap contains the following
*       scalar character entries (all taken from the corresponding
*       record in the NDF HISTORY component):
*       - "DATE": The date and time of the action (e.g. "2009-JUN-24
*         14:00:53.752" ).
*       - "COMMAND": An indication of the command that performed the
*         action (e.g. "WCSATTRIB (KAPPA 1.10-6)" ).
*       - "USER": The user name that performed the action (e.g. "dsb").
*       - "TEXT": The full text of the NDF history record. This is
*         arbitrary, but for NDFs created by Starlink software it will
*         usually include environment parameter values, and the full
*         path of the command that performed the action.
*
*     If the specified ancestor does not have any of these items of
*     information, then the corresponding entry will not be present
*     in the returned KeyMap. For instance, if the ancestor has no
*     immediate parent NDFs, then the "PARENTS" entry will not be
*     present in the KeyMap. A NULL pointer will be returned if the
*     NDF has no provenance extension, or if "ianc" is outside the
*     bounds of the ANCESTORS array (and is not zero). The returned
*     KeyMap pointer should be annulled when it is no longer needed,
*     either by calling astAnnul explicitly, or by relying on astEnd
*     to annul it (together with all the other AST Objects created in
*     the current AST Object context).
*-
*/

/* Local variables: */
   AstKeyMap *more = NULL;
   AstKeyMap *result = NULL;
   Prov *prov1 = NULL;
   Provenance *provenance = NULL;
   int *old_status;
   int *parents;
   int npar;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgGetProv", status );
   if( provenance ) {

/* Get a pointer to the Prov structure describing the required NDF. */
      if( ianc == 0 ) {
         prov1 = provenance->main;
      } else if( ianc < provenance->nprov ) {
         prov1 = provenance->provs[ ianc ];
      }

/* Check the pointer can be used. */
      if( prov1 ) {

/* Create the returned (empty) KeyMap. */
         result = astKeyMap( " " );

/* Get an array holding the indices of the direct parents of the NDF. */
         parents = ndg1ParentIndicies( prov1, provenance, NULL, &npar,
                                       status );

/* Store the required values in the KeyMap. */
         if( prov1->date ) astMapPut0C( result, DATE_NAME, prov1->date, NULL );
         if( prov1->creator ) astMapPut0C( result, CREATOR_NAME, prov1->creator, NULL );
         if( prov1->path ) astMapPut0C( result, PATH_NAME, prov1->path, NULL );
         if( npar ) astMapPut1I( result, PARENTS_NAME, npar, parents, NULL );

/* If the ancestor has a non-empty MORE KeyMap store a deep copy of it in the
   returned KeyMap. */
         if( prov1->more && astMapSize( prov1->more ) > 0 ) {
            more = astCopy( prov1->more );
            astMapPut0A( result, MORE_NAME, more, NULL );
            more = astAnnul( more );
         }

/* Free the parents array. */
         parents = astFree( parents );

/* Add a vector entry to the KeyMap holding a list of KeyMaps containing
   any history records in the ancestor. */
         ndg1AddHistKM( result, HIST_NAME, prov1, status );
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

   return result;
}

void ndgHideProv( NdgProvenance *prov, int ianc, int *status ){
/*
*+
*  Name:
*     ndgHideProv

*  Purpose:
*     Hide an ancestor in a provenance structure.

*  Invocation:
*     ndgHideProv( NdgProvenance *prov, int ianc, int *status )

*  Description:
*     This function flags a specified ancestor as "hidden". The only
*     effect this has is that the ancestor will not be included in
*     Provenance structures created by the ndgCopyProv function.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     ianc
*        The index of the ancestor NDF to be hidden. The value is used as an
*        index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 1 (the main NDF cannot be
*        hidden).
*     status
*        The global status.
*-
*/

/* Local variables: */
   Provenance *provenance = NULL;
   int *old_status;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Report an error if "ianc" is zero. */
   if( ianc == 0 ) {
      *status = SAI__ERROR;
      errRep( " ", "ndgHideProv: Ancestor zero (the main NDF) cannot be "
              "hidden.", status );
   }

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgHideProv", status );
   if( provenance ) {

/* Report an error if "ianc" is out of bounds. */
      if( ianc < 1 || ianc >= provenance->nprov ) {
         *status = SAI__ERROR;
         msgSeti( "I", ianc );
         msgSeti( "N", provenance->nprov );
         errRep( " ", "ndgHideProv: Ancestor index ^I is illegal: the NDF "
                 "has ^N ancestor(s).", status );

/* Otherwise, set the flag. */
      } else {
         provenance->provs[ ianc ]->hidden = 1;
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

int ndgIsHiddenProv( NdgProvenance *prov, int ianc, int *status ){
/*
*+
*  Name:
*     ndgIsHiddenProv

*  Purpose:
*     See if an ancestor in a provenance structure is hidden.

*  Invocation:
*     int ndgIsHiddenProv( NdgProvenance *prov, int ianc, int *status );

*  Description:
*     This function returns a non-zero value if the specified ancestor
*     has been hidden. See ndgHideProv and ndgCopyProv.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     ianc
*        The index of the ancestor NDF to be checked. The value is used as
*        an index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 0.
*     status
*        The global status.
*-
*/

/* Local variables: */
   Provenance *provenance = NULL;
   int *old_status;
   int result;

/* Initialise */
   result = 0;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgUnhideProv", status );
   if( provenance ) {

/* Report an error if "ianc" is out of bounds. */
      if( ianc < 0 || ianc >= provenance->nprov ) {
         *status = SAI__ERROR;
         msgSeti( "I", ianc );
         msgSeti( "N", provenance->nprov );
         errRep( " ", "ndgIsHiddenProv: Ancestor index ^I is illegal: the NDF "
                 "has ^N ancestor(s).", status );

/* Otherwise, return the flag. */
      } else {
         result = provenance->provs[ ianc ]->hidden;
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

/* Return the result. */
   return result;

}

void ndgModifyProv( NdgProvenance *prov, int ianc, AstKeyMap *km,
                    int *status ){
/*
*+
*  Name:
*     ndgModifyProv

*  Purpose:
*     Modify the information stored for a particular ancestor.

*  Invocation:
*     void ndgModifyProv( NdgProvenance *prov, int ianc, AstKeyMap *km,
*                         int *status )

*  Description:
*     This function modifies the information stored for a given ancestor
*     in the supplied provenance structure. The new values to store
*     are supplied in an Ast KeyMap such as returned by ndgGetProv.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     ianc
*        The index of the ancestor NDF for which information should be
*        modified. A value of zero will result in information being modified
*        for the NDF from which the the supplied provenance structure was
*        read. Otherwise, the "ianc" value is used as an index into the
*        ANCESTORS array. An error is reported if "ianc" is too large.
*     km
*        A pointer to an AST KeyMap containing the values to store. Entries
*        with the following keys are recognised:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at
*          which the provenance information for the ancestor NDF was
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": A KeyMap containing extra information to store with
*          the ancestor.
*
*        If the "DATE", "CREATOR" or "MORE" components are missing then
*        corresponding item of information will be deleted from the
*        provenance extension. An error is reported if the supplied KeyMap
*        has no "PATH" entry. Note, the PARENTS list and HISTORY information
*        stored with the specified ancestor cannot be modified (any "PARENTS"
*        or "HISTORY" component in the supplied HDS structure will be
*        ignored).
*     status
*        The global status.
*-
*/

/* Local variables: */
   AstObject *obj = NULL;
   Prov *anc = NULL;
   Provenance *provenance = NULL;
   const char *cval = NULL;
   int *old_status;
   int nc;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgModifyProv", status );
   if( provenance ) {

/* Check the "ianc" value is within the bounds of the ANCESTORS array. */
      if( ianc >= 0 && ianc < provenance->nprov ) {

/* Get a pointer to the ancestors Prov structure. */
         if( ianc == 0 ) {
            anc = provenance->main;
         } else {
            anc = provenance->provs[ ianc ];
         }

/* If defined, copy the PATH component. Report an error otherwise. */
         if( astMapGet0C( km, PATH_NAME, &cval ) && cval &&
             ( nc = strlen( cval ) ) ) {
            anc->path = astStore( anc->path, cval, nc + 1 );

         } else if( *status == SAI__OK ){
            *status = SAI__ERROR;
            errRep( " ", "ndgModifyProv: Cannot modify provenance "
                    "information: no new PATH supplied.", status );
         }

/* If defined, copy the DATE component. Delete it otherwise. */
         if( astMapGet0C( km, DATE_NAME, &cval ) && cval &&
             ( nc = strlen( cval ) ) ) {
            anc->date = astStore( anc->date, cval, nc + 1 );

         } else {
            anc->date = astFree( anc->date );
         }

/* If defined, copy the CREATOR component. Delete it otherwise. */
         if( astMapGet0C( km, CREATOR_NAME, &cval ) && cval &&
             ( nc = strlen( cval ) ) ) {
            anc->creator = astStore( anc->creator, cval, nc + 1 );

         } else {
            anc->creator = astFree( anc->creator );
         }

/* If the KeyMap contains a MORE component, and it is a pointer to a
   non-empty KeyMap, store a deep copy in the Prov structure. Delete it
   otherwise. */
         if( astMapHasKey( km, MORE_NAME ) ) {
            if( astMapType( km, MORE_NAME ) == AST__OBJECTTYPE ) {
               astMapGet0A( km, MORE_NAME, &obj );
               if( astIsAKeyMap( obj ) ) {
                  if( astMapSize( (AstKeyMap *) obj ) > 0 ) {
                     anc->more = astCopy( obj );
                  } else if( anc->more ) {
                     anc->more = astAnnul( anc->more );
                  }
               } else {
                  *status = SAI__ERROR;
                  errRepf( " ", "ndgModifyProv; Cannot modify provenance "
                           "ancestor %d: supplied MORE component holds a "
                           "%s (should be a KeyMap).", status, ianc,
                           astGetC( obj, "Class" ) );
               }
            } else {
               errRepf( " ", "ndgModifyProv; Cannot modify provenance "
                        "ancestor %d: supplied MORE component does not "
                        "hold a KeyMap.", status, ianc );
            }
         } else if( anc->more ) {
            anc->more = astAnnul( anc->more );
         }

/* Indicate the ProvId value need to be recalculated to take account of
   the changes. */
         ndg1ClearProvId( anc, status );

/* It is possible that the changes may have resulted in the ancestor
   having the same path as another ancestor. So now check for duplicated
   ancestors and purge them. */
         ndg1PurgeProvenance( provenance, status );

/* Report an error if the ianc value is bad. */
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "IANC", ianc );
         msgSeti( "N", provenance->nprov );
         errRep( " ", "ndgModifyProv; Cannot modify provenance ancestor "
                 "^IANC: only ^N ancestors found.", status );
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgPutProv( NdgProvenance *prov, int indf, AstKeyMap *more, int isroot,
                 int *status ){
/*
*+
*  Name:
*     ndgPutProv

*  Purpose:
*     Add an NDF to the list of ancestors.

*  Invocation:
*     ndgPutProv( NdgProvenance *prov, int indf, AstKeyMap *more,
*                 int isroot, int *status )

*  Description:
*     This function modifies the supplied provenance structure to indicate
*     that a given NDF was used in the creation of the NDF associated with
*     the supplied provenance structure.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     indf
*        An identifier for an NDF that is to be added into the list of
*        ancestor NDFs in the supplied provenance information.
*     more
*        A pointer to an AstKeyMap holding arbitrary additional information
*        about the new ancestor NDF, and how it was used in the creation of
*        the output NDF. A NULL pointer can be supplied if required.
*     isroot
*        If non-zero, then the new ancestor NDF will be treated as a root
*        NDF. That is, any provenance information in the supplied NDF is
*        ignored. If zero, then any provenance information in the NDF is
*        copied into the supplied provenance structure. The new ancestor NDF
*        is then only a root NDF if it contains no provenance information.
*     status
*        The global status.
*-
*/

/* Local variables: */
   Provenance *prov2 = NULL;
   Provenance *provenance = NULL;
   int *old_status;
   int *ph;
   int free_provs;
   int hash;
   int hhash;
   int i;
   int irec;
   int there;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgPutProv", status );
   if( provenance ) {

/* Get the provenance information from the new ancestor NDF. */
      prov2 = ndg1ReadProvenanceNDF( indf, more, NULL, isroot, status );

/* Indicate that the "Prov" structures referred to by prov2 should be
   freed when ndgFreeProvenance is called. */
      free_provs = 1;

/* Extend the "provs" list in "provenance" so that we can add pointers to
   all the Prov structures in "prov2. */
      if( provenance && prov2 ) {
         provenance->provs = astGrow( provenance->provs,
                                      provenance->nprov + prov2->nprov,
                                      sizeof( Prov *) );
      }
      if( astOK ) {

/* Copy history records from the NDF HISTORY component into the main
   Prov structure in "prov2". We do not copy records that were
   propagated to the NDF from input NDFs since such records will already
   be present in the other Prov structures in "prov2". Thus, the only
   records copied are those that describe modifications that have been
   made to the NDF since it was created (e.g. changing a WCS attribute,
   changing a value in the FITS extension, etc), plus the record that
   describes the creation of the NDF. So we work backwards through the
   HISTORY component, from youngest to oldest history record, until the
   record is reached that describes the creation of the NDF (as
   indicated by the fact that its hash code matches the hash code
   stored when provenance information was added to the NDF, i.e. at
   its creation). Each such record (including the final one) is coped
   into the Prov structure. We copy *all* history records if the NDF is
   to be treated as a root ndf. We copy *no* records if the main ndf has
   no hash code. */
         ndfState( indf, "History", &there, status );
         hhash = prov2->main->hhash;
         if( there && ( hhash || isroot ) ) {
            ndfHnrec( indf, &irec, status );
            ph = isroot ? NULL : &hash;
            prov2->main->hhash = 0;

            for( ; irec > 0; irec-- ) {
               ndg1ReadHistRec( prov2->main, indf, irec, ph, status );
               if( ph && *ph == hhash ) break;
            }
         }

/* Copy the Prov pointers from "prov2" to "provenance". */
         for( i = 0; i < prov2->nprov; i++ ) {
            provenance->provs[ i + provenance->nprov ] = prov2->provs[ i ];
         }

/* Update the length of the "provs" array in "provenance". */
         provenance->nprov += prov2->nprov;

/* Indicate that the "Prov" structures referred to by "prov2" should not
   be freed when ndgFreeProvenance is called. This is because they are
   now the responsibility of "provenance", having been copied into the
   provenance->provs list above. */
         free_provs = 0;

/* Indicate that the indices of the parents of each prov structure needs
   to be re-calculated to take account of the addition of the new ancestors. */
         ndg1ResetIndices( provenance, status );

/* Record the new ancestor NDF as a parent of the main NDF. */
         ndg1ParentChild( prov2->main, provenance->main, 1, status );

/* Purge any duplicate entries in the extended provenance information. */
         ndg1PurgeProvenance( provenance, status );
      }

/* Free the provenance structure for the new ancestor NDF. */
      ndg1FreeProvenance( prov2, free_provs, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

NdgProvenance *ndgReadProv( int indf, const char *creator, int *status ){
/*
*+
*  Name:
*     ndgReadProv

*  Purpose:
*     Read the provenance information from an NDF.

*  Invocation:
*     NdgProvenance *ndgReadProv( int indf, const char *creator, int *status )

*  Description:
*     This function reads the information stored in the "PROVENANCE"
*     extension of an NDF, storing it in a memory-resident structure for
*     faster access. A pointer that identifies this structure is returned,
*     and can be passed to other NDG provenance functions to manipulate
*     the contents of the structure.
*
*     If the NDF has no provenance information (for instance, if it is a
*     newly created NDF), or if no NDF is supplied, the returned structure
*     will contain just the supplied creator name (which may be blank), and
*     an empty ancestor list.
*
*     The structure should be freed when it is no longer needed by
*     calling ndgFreeProv.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information
*        to be read. This may be NDF__NOID, in which case a new provenance
*        structure with the supplied creator name and an empty ancestor
*        list will be created and returned.
*     creator
*        A text identifier for the software that created INDF (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*        This value is only used if the the NDF does not contain any
*        existing provenance information.
*     status
*        The global status.

*  Returned Value:
*     A pointer that identifies the structure holding the provenance
*     information read from the NDF. Note, this is not a genuine pointer
*     to the structure and should not be de-referenced. A NULL pointer is
*     returned if an error occurs.
*-
*/

/* Read the provenance extension from the NDF, and encode the resulting
   "Provenance *" pointer into an opaque identifier to be returned. */
   return ndg1Encode( ndg1ReadProvenanceNDF( indf, NULL, creator,
                                             0, status ), status );
}

NdgProvenance *ndgReadVotProv( const char *xml, const char *path,
                               const char *creator, int *status ){
/*
*+
*  Name:
*     ndgReadVotProv

*  Purpose:
*     Read the provenance information from a VOTABLE.

*  Invocation:
*     NdgProvenance *ndgReadVotProv( const char *xml, const char *path,
*                                    const char *creator, int *status )

*  Description:
*     This function reads provenance information from a string of XML text
*     read from a VOTABLE, storing it in a memory-resident structure for
*     faster access. A pointer that identifies this structure is returned,
*     and can be passed to other NDG provenance functions to manipulate
*     the contents of the structure.
*
*     If the XML text has no provenance information, the returned structure
*     will contain just the supplied creator name (which may be blank), and
*     an empty ancestor list.
*
*     The structure should be freed when it is no longer needed by
*     calling ndgFreeProv.

*  Arguments:
*     xml
*        Pointer to a null terminated string holding XML read from a
*        VOTABLE. The provenance information is read from the first
*        element found in the text that has the opening tag:
*
*        "<GROUP name="PROVENANCE" utype="hds_type:PROVENANCE">"
*
*        This is the form produced by function ndgWriteVotProv.
*     path
*        The path to the file from which the XML provenance text was read.
*     creator
*        A text identifier for the software that created the NDF with
*        which the provnance is associated (usually the name of the calling
*        application). The format of the identifier is arbitrary, but the
*        form "PACKAGE:COMMAND" is recommended. This value is only used if
*        the the supplied XML text does not contain any provenance information.
*     status
*        The global status.

*  Returned Value:
*     A pointer that identifies the structure holding the provenance
*     information read from the VOTABLE. Note, this is not a genuine pointer
*     to the structure and should not be de-referenced. A NULL pointer is
*     returned if an error occurs.
*-
*/

/* Read the provenance extension from the XML, and encode the resulting
   "Provenance *" pointer into an opaque identifier to be returned. */
   return ndg1Encode( ndg1ReadProvenanceXml( xml, path, creator, status ),
                      status );
}

void ndgRemoveProv( NdgProvenance *prov, int nanc, int *anc, int *status ){
/*
*+
*  Name:
*     ndgRemoveProv

*  Purpose:
*     Remove one or more ancestors from a provenance structure.

*  Invocation:
*     void ndgRemoveProv( NdgProvenance *prov, int nanc, int *anc,
*                         int *status )

*  Description:
*     This routine removes one or more ancestors from the supplied
*     provenance structure. The direct parents of the removed ancestor
*     are assigned to the direct children of the removed ancestor. Note,
*     any history records stored in the removed ancestors are lost.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     nanc
*        The length of the "anc" array.
*     anc
*        Pointer to an array holding the indices of the ancestor NDFs to be
*        removed. Each supplied value must be at least 1, and must be no
*        more than the number of ancestors in the provenance extension
*        (as returned by ndgCountProv). An error is reported otherwise. The
*        supplied list is sorted into decreasing order before use so that
*        the highest index ancestor is removed first.
*     status
*        The global status.
*-
*/

/* Local Variables: */
   Provenance *provenance = NULL;
   int *old_status;
   int *sanc;
   int i;
   int ianc;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgRemoveProv", status );
   if( provenance ) {

/* Produce a sorted copy of the supplied array of indices, so that the
   ancestor indices decrease. This is necessary since removing an
   ancestor will modify the indices of the remaining higher ancestor
   indices. */
      sanc = astStore( NULL, anc, sizeof( *anc )*nanc );

/* Sort the array so that the largest index is first. */
      if( sanc ) qsort( sanc, (size_t) nanc, sizeof( *sanc ), ndg1IntCmp );

/* Loop round the array of ancestor indices. */
      ianc = -1;
      for( i = 0; i < nanc && *status == SAI__OK; i++ ) {

/* Skip duplicated indices. */
         if( sanc[ i ] != ianc ) {
            ianc = sanc[ i ];

/* Remove the ancestor. */
            ndg1Rmprv( provenance, ianc, status );
         }
      }

/* Free resources. */
      sanc = astFree( sanc );

/* Indicate that the indices of the parents of each prov structure needs
   to be re-calculated to take account of the removal of the ancestors. */
      ndg1ResetIndices( provenance, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

AstKeyMap *ndgRootProv( NdgProvenance *prov, int *status ){
/*
*+
*  Name:
*     ndgRootProv

*  Purpose:
*     Identify the root ancestors in a provenance structure.

*  Invocation:
*     AstKeyMap *ndgRootProv( NdgProvenance *prov, int *status )

*  Description:
*     This function searches the supplied provenance structure for root
*     ancestors, and returns information about them. An ancestor is a root
*     ancestor if it does not itself have any ancestors.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     status
*        The global status.

*  Returned Value:
*     A pointer to an AST KeyMap containing an entry for each root
*     ancestor. The key associated with each entry is the path to the
*     NDF and the value of the entry is an integer that gives the
*     position of the root ancestor within the list of all ancestors.
*     This integer value can be supplied to ndgGetProv in order to get
*     further information about the root ancestor. The first ancestor NDF
*     has an index of one. An index of zero refers to the NDF from which
*     the provenance information was read.
*-
*/

/* Local variables: */
   AstKeyMap *result = NULL;
   Prov *prov1 = NULL;
   Provenance *provenance = NULL;
   int *old_status;
   int i;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgRootProv", status );
   if( provenance ) {

/* Create the returned KeyMap. */
      result = astKeyMap( " " );

/* Loop round every ancestor. */
      for( i = 0; i < provenance->nprov;  i++ ) {
         prov1 = provenance->provs[ i ];

/* If this ancestor has no parents, add its path and index to the returned
   KeyMap. */
         if( prov1->nparent == 0 ) astMapPut0I( result, prov1->path, i, " " );
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

   return result;
}

void ndgUnhashProv( NdgProvenance *prov, int *status ){
/*
*+
*  Name:
*     ndgUnhashProv

*  Purpose:
*     Clear the hash code describing the creation of the Provenance.

*  Invocation:
*     void ndgUnhashProv( NdgProvenance *prov, int *status )

*  Description:
*     Each ancestor in a Provenance structure may contain a copy of the
*     History information stored in the associated ancestor NDF. Storing
*     the complete History component from each ancestor NDF would be very
*     wastefull since the NDF History component will usually contain not
*     only records of operations performed on the ancestor NDF, but also
*     all History records inherited from the "primary" NDF (i.e. the NDF
*     from which the ancestor was propagated). Since these inherited
*     History records will already be stored with other ancestors in the
*     Provenance structure, it is not necessary to store them again.
*     However, this means that when we add a new parent into a Provenance
*     structure using ndgPutProv, NDG needs some way of knowing which
*     records within the new NDF are unique to the NDF (and should thus
*     be stored in the Provenance structure), and which were inherited
*     from earlier ancestors (and will thus already be stored in the
*     Provenance structure). The solution is for each PROVENANCE extension
*     to include a "creator" hash code for the History record that describes
*     the creation of the NDF. When an NDF is supplied to ndgPutProv, each
*     History record, starting with the most recent, is copied from the
*     NDF into the Provenance structure, until a History record is found
*     which has a hash code equal to the creator hash code in the NDF. The
*     copying of history records then stops since all earlier history
*     records will already be present in the Provenance structure.
*
*     This routine clears the creator hash code in the supplied
*     Provenance structure, so that a new one will be calculated when the
*     Provenance structure is written to an NDF using ndgWriteProv. This
*     is useful for instance if the Provenance was written to the NDF
*     using ndgWriteProv before the NDF History record was completed. In
*     this case, you would probably want to re-read the Provenance from
*     the NDF, use this function to clear the creator hash code, and then
*     re-write the Provenance to the NDF, thus forcing a new creator hash
*     code to be stored in the NDF.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     status
*        The global status.

*-
*/

/* Local variables: */
   Provenance *provenance = NULL;
   int *old_status;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgUnhashProv", status );
   if( provenance ) {

/* Clear the hash code. */
      provenance->main->hhash = 0;

   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgUnhideProv( NdgProvenance *prov, int ianc, int *status ){
/*
*+
*  Name:
*     ndgUnhideProv

*  Purpose:
*     Un-hide an ancestor in a provenance structure.

*  Invocation:
*     ndgUnhideProv( NdgProvenance *prov, int ianc, int *status )

*  Description:
*     This function ensures that a given ancestor is not flagged as
*     "hidden". See ndgHideProv and ndgCopyProv.

*  Arguments:
*     prov
*        An identifier for a structure holding the provenance information
*        as returned by ndgReadProv or ndgReadVotProv.
*     ianc
*        The index of the ancestor NDF to be un-hidden. The value is used as
*        an index into the ANCESTORS array. An error will be reported if the
*        value is too large, or is less than 0.
*     status
*        The global status.

*  Notes:
*     - No error is reported if the specified ancestor is not currently
*     hidden (in which case this function returns without action).
*-
*/

/* Local variables: */
   Provenance *provenance = NULL;
   int *old_status;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgUnhideProv", status );
   if( provenance ) {

/* Report an error if "ianc" is out of bounds. */
      if( ianc < 0 || ianc >= provenance->nprov ) {
         *status = SAI__ERROR;
         msgSeti( "I", ianc );
         msgSeti( "N", provenance->nprov );
         errRep( " ", "ndgUnhideProv: Ancestor index ^I is illegal: the NDF "
                 "has ^N ancestor(s).", status );

/* Otherwise, clear the flag. */
      } else {
         provenance->provs[ ianc ]->hidden = 0;
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgWriteProv( NdgProvenance *prov, int indf, int whdef, int *status ){
/*
*+
*  Name:
*     ndgWriteProv

*  Purpose:
*     Write provenance information to an NDF.

*  Invocation:
*     void ndgWriteProv( NdgProvenance *prov, int indf, int whdef,
*                        int *status )

*  Description:
*     This function writes the contents of the supplied provenance
*     structure out to a given NDF, replacing any existing provenance
*     information in the NDF.

*  Arguments:
*     prov
*        A pointer to the provenance information to be written out.
*     indf
*        Identifier for the NDF in which to store the provenance
*        information.
*     whdef
*        The correct recording of history information within the
*        PROVENANCE extension requires that the current history record
*        within the supplied NDF at the time this function is called,
*        describes the creation of the NDF. Very often, an application
*        will not itself add any history to the NDF, but will instead
*        rely on the automatic recording of default history provided by
*        the NDF library. Normally, default history is recorded when the
*        NDF is released from the NDF system (e.g. using ndfAnnul or
*        ndfEnd). So if this function is called prior to the release of
*        the NDF (which it normally will be), then the default history
*        information will not yet have been recorded, resulting in
*        incorrect information being stored in the PROVENANCE extension.
*        For this reason, the "whdef" argument is supplied. If it is set
*        to a non-zero value, a check is made to see if default history
*        has already been stored in the NDF. If not, default history is
*        stored in the NDF before going on to create the PROVENANCE
*        extension. Applications that do not use the default history
*        recording mechanism, but instead store their own history
*        information, should supply a zero value for "whdef" and should
*        also ensure that history information has been stored in the NDF
*        before calling this function.
*     status
*        The global status.
*-
*/

/* Local Variables: */
   Provenance *provenance;
   int *old_status;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgWriteProv", status );
   if( provenance ) {

/* Attempt to write the provenance information to the NDF. */
      ndg1WriteProvenanceNDF( provenance, indf, whdef, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

}

const char *ndgWriteVotProv( NdgProvenance *prov, int *status ){
/*
*+
*  Name:
*     ndgWriteVotProv

*  Purpose:
*     Write provenance information out to a VOTABLE.

*  Invocation:
*     const char *ndgWriteVotProv( NdgProvenance *prov, int *status )

*  Description:
*     This function writes the contents of the supplied provenance
*     structure out as a text string holding an XML snippet suitable
*     for inclusion in a VOTABLE.

*  Arguments:
*     prov
*        A pointer to the provenance information to be written out.
*     status
*        The global status.

*  Returned Value:
*     A pointer to a dyamically allocated string holding the XML text, or
*     NULL if an error occurs. The string should be freed using astFree
*     when it is no longer needed. The text will contain a single top
*     level element with the following opening tag:
*
*        "<GROUP name="PROVENANCE" utype="hds_type:PROVENANCE">"
*
*     The HDS structure of an NDF PROVENANCE extension is replicated
*     using PARAM elements to hold primitive values and GROUP elements to
*     hold structures. The "utype" attributes are used to hold the
*     corresponding HDS data types.

*-
*/

/* Local Variables: */
   Provenance *provenance;
   const char *result = NULL;
   int *old_status;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Decode the supplied identifier to obtain a pointer to a Provenance
   structure. */
   provenance = ndg1Decode( prov, "ndgWriteVotProv", status );
   if( provenance ) {

/* Attempt to convert the provenance information into XML. */
      result = ndg1WriteProvenanceXml( provenance, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );

/* Return the result. */
   return result;
}





/* Private functions. */
/* ================= */
static void ndg1AddHistKM( AstKeyMap *km, const char *key, Prov *prov,
                           int *status ){
/*
*  Name:
*     ndg1AddHistKM

*  Purpose:
*     Copy history records from a Prov structure into a KeyMap.

*  Invocation:
*     void ndg1AddHistKM( AstKeyMap *km, const char *key, Prov *prov,
*                         int *status )

*  Description:
*     This function returns without action unless the supplied Prov
*     structure contains one or more history records.Otherwise, it adds
*     a vector entry to the supplied KeyMap to hold a list of KeyMap
*     pointers. It then creates these KeyMaps, one for each history
*     record in the Prov structure, and copies the history information
*     into them.

*  Arguments:
*     km
*        The KeyMap to receive the new vector entry.
*     key
*        The key to use for the new KeyMap vector entry.
*     prov
*        Pointer to the Prov structure containing the history records to
*        copy.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstKeyMap *kmrec = NULL;
   AstObject **vector = NULL;
   HistRec *hrec = NULL;
   int i;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Create an array to hold the KeyMap pointers. */
   vector = astMalloc( sizeof( AstObject *)*prov->nhrec );

/* Check we have some history records to copy ("vector" will be null if
   prov->nhrec - the number of history records - is zero). */
   if( vector ) {

/* Loop round them all. */
      hrec = prov->hist_recs;
      for( i = 0; i < prov->nhrec; i++,hrec++ ) {

/* Create a KeyMap to hold this history record, and store a pointer to be
   stored in the vector. */
         kmrec = astKeyMap( " " );
         vector[ i ] = (AstObject *) kmrec;

/* Store the history items in it. */
         astMapPut0C( kmrec, DATE_NAME, hrec->date, NULL );
         astMapPut0C( kmrec, COMMAND_NAME, hrec->command, NULL );
         astMapPut0C( kmrec, USER_NAME, hrec->user, NULL );
         astMapPut0C( kmrec, TEXT_NAME, hrec->text, NULL );
      }

/* Store the KeyMap pointers in a new vector entry in the supplied
   KeyMap. */
      astMapPut1A( km, key, prov->nhrec, vector, NULL );

/* Annul the individual KeyMap pointers. */
      for( i = 0; i < prov->nhrec; i++ ) {
         vector[ i ] = astAnnul( vector[ i ] );
      }

/* Free the array holding the pointers. */
      vector = astFree( vector );
   }
}

static void ndg1Antmp( HDSLoc **loc, int *status ){
/*
*  Name:
*     ndg1Antmp

*  Purpose:
*     Erase a temporary HDS object created by the NDG library.

*  Invocation:
*     void ndg1Antmp( HDSLoc **loc, int *status )

*  Description:
*     The function annuls a locator to a temporary object created by the
*     NDG library, thereby causing the associated object to be erased and
*     the file space associated with it to be released.

*  Arguments:
*     loc
*        Address of a pointer to an HDS locator for a temporary object
*        created by NDG. The pointer is returned set to NULL. The
*        function returns without action if a NULL address or pointer
*        is supplied.
*     status
*        The global status.

*/

/* Local variables: */
   HDSLoc *locp = NULL;          /* Locator to parent object */
   char name[ DAT__SZNAM + 1 ];  /* Name of object to be erased */

/* Check a pointer was supplied. */
   if( !loc || !*loc ) return;

/* Begin a new error reporting context. */
   errBegin( status );

/* Find the temporary object's name. */
   datName( *loc, name, status );

/* Find its parent. */
   datParen( *loc, &locp, status );

/* Annul the object's locator. */
   datAnnul( loc, status );

/* Erase the object. */
   datErase( locp, name, status );

/* Annul the parent's locator. */
   datAnnul( &locp, status );

/* End the error reporting context. */
   errEnd( status );
}

static void ndg1Check( const char *text, Prov *prov, AstKeyMap *km,
                       int *status ) {
/*
*  Name:
*     ndg1Check

*  Purpose:
*     Check that all parent-child links are consistent.

*  Invocation:
*     void ndg1Check( const char *text, Prov *prov, AstKeyMap *km,
*                     int *status )

*  Description:
*     This function checks that all parents of the supplied Prov include
*     the Prov within their list of children. It then checks that all
*     children of the supplied Prov include the Prov within their list
*     of parents. It also calls itself recursively to check each parent
*     and child in the same way, taking care to avoid infinite loops.
*
*     If any of these checks fail, an error is reported.

*  Arguments:
*     text
*        A text message to include at the start of the error report. The
*        remaining text in the error report identifies the parent and child
*        for which the check failed.
*     prov
*        The Prov structure from which to start the checks.
*     km
*        An AstKeyMap containing a list of the Prov structures that have
*        already been checked and so should not be re-checked. This is
*        used to avoid infinite loops. Each entry in the KeyMap should
*        have a key which is the address of a Prov structure, formatted
*        into a character string using "%p". The value associated with
*        the key is arbitrary and unused. A NULL pointer can be supplied
*        on the top level entry to this function, in which case a new
*        KeyMap will be created.
*     status
*        Inherited status pointer.

*/

/* Local Variables: */
   Prov *child;
   Prov *parent;
   char key[40];
   int annul;
   int i, j, ok;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Create a KeyMap is required, noting whether we should annull the
   KeyMap before leaving this function. */
   if( !km ) {
      km = astKeyMap( " " );
      annul = 1;
   } else {
      annul = 0;
   }

/* Add the supplied Prov to the list of Provs that should not be
   re-checked. Do it now, at the start, as this prevents subsequent calls
   to this function re-checking the supplied Prov. */
   sprintf( key, "%p", prov );
   astMapPut0I( km, key, 0, NULL );

/* Check each parent of the supplied Prov. */
   for( i = 0; i < prov->nparent && *status == SAI__OK; i++ ) {
      parent = prov->parents[ i ];

/* See if the current parent recognises the supplied Prov as a child. */
      ok = 0;
      for( j = 0; j < parent->nchild; j++ ) {
         if( parent->children[ j ] == prov ) {
            ok = 1;
            break;
         }
      }

/* If not report an error. */
      if( ! ok && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "P", parent->path );
         msgSetc( "C", prov->path );
         msgSetc( "T", text );
         errRep( " ", "^T: ^P is a parent of ^C, but the inverse "
                 "relationship does not exist.", status );

/* If this parent looks OK so far, and it has not previously been checked,
   call this function recursively to check the parent's other family
   connections. */
      } else {
         sprintf( key, "%p", parent );
         if( !astMapHasKey( km, key ) ) {
            ndg1Check( text, parent, km, status );
         }

      }
   }

/* Check each child of the supplied Prov. */
   for( i = 0; i < prov->nchild && *status == SAI__OK; i++ ) {
      child = prov->children[ i ];

/* See if the current child recognises the suppleid Prov as a parent. */
      ok = 0;
      for( j = 0; j < child->nparent; j++ ) {
         if( child->parents[ j ] == prov ) {
            ok = 1;
            break;
         }
      }

/* If not report an error. */
      if( ! ok && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "C", child->path );
         msgSetc( "P", prov->path );
         msgSetc( "T", text );
         errRep( " ", "^T: ^C is a child of ^P, but the inverse "
                 "relationship does not exist.", status );

/* If this child looks OK so far, and it has not previously been checked,
   call this function recursively to check the child's other family
   connections. */
      } else {
         sprintf( key, "%p", child );
         if( !astMapHasKey( km, key ) ) {
            ndg1Check( text, child, km, status );
         }

      }
   }

/* If the KeyMap was created within this invocation, annul it. */
   if( annul ) km = astAnnul( km );
}

static int ndg1CheckSameParents( Prov *prov1, Prov *prov2, int *status ) {
/*
*  Name:
*     ndg1CheckSameParents

*  Purpose:
*     Checks if two Prov structures may have the same parents.

*  Invocation:
*     void ndg1CheckSameParents( Prov *prov1, Prov *prov2, int *status )

*  Description:
*     This function returns a flag indicating if the two supplied Prov
*     structures may have the same parents.
*
*     Note, two parents are considered equal if they have the same path.
*     This is fast(ish) but not totally reliable since two distinct NDFs
*     can have the same path is they were created at different times.
*     Better would be an algorithm that used ndg1TheSame to determine if
*     two NDFs are the same, but this would be slow.

*  Arguments:
*     prov1
*        The first Prov structure.
*     prov2
*        The second Prov structure.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the two supplied Prov structures may have the same list
*     of parents. Zero otherwise, or if an error occurs.

*/

/* Local Variables: */
   AstKeyMap *km;
   Prov *temp;
   int result;
   int i;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return 0;

/* Initialise */
   result = 1;

/* Create a KeyMap. */
   km = astKeyMap( " " );

/* Add an entry to the KeyMap for each parent of prov1. The key is the
   path to parent and the value is a pointer to the Prov structure. */
   for( i = 0; i < prov1->nparent; i++ ) {
      astMapPut0P( km, prov1->parents[ i ]->path, prov1->parents[ i ], NULL );
   }

/* Loop round all the parents of prov2. */
   for( i = 0; i < prov2->nparent; i++ ) {

/* Clear the returned flag and leave the loop if the KeyMap does not
   contain an entry with key equal to the path of the current parent. */
      if( ! astMapHasKey( km, prov2->parents[ i ]->path ) ) {
         static_badtype = 0;
         static_badpath = prov2->parents[ i ]->path;
         static_badprovid = ndg1GetProvId(  prov2->parents[ i ], status );
         result = 0;
         break;

/* If the parent was found in the KeyMap, remove the entry. */
      } else {
         astMapRemove( km, prov2->parents[ i ]->path );
      }
   }

/* If any entries remain in the KeyMap (indicating parents of prov1
   that are not parents of prov2), clear the returned flag. */
   if( result && astMapSize( km ) > 0 ) {
      static_badtype = 1;
      static_badpath = astMapKey( km, 0 );
      (void) astMapGet0P( km, static_badpath, (void **) &temp );
      static_badprovid = ndg1GetProvId(  temp, status );
      result = 0;
   }

/* Free resources. */
   km = astAnnul( km );

/* Return the result. */
   return result;
}

static void ndg1ChSink( const char *text ) {
/*
*  Name:
*     ndg1ChSink

*  Purpose:
*     Appends the supplied line of text to dynamic memory area.

*  Invocation:
*     void ndg1ChSink( const char *text );

*  Description:
*     This function is invoked by the astWrite method to append a line of
*     an AST Object dump to a dynamically expanding memory area. The
*     memory area is accessed via a pointer passed in a thread-safe
*     global variable.

*  Arguments:
*     text
*        Pointer to a null-terminated string to be appended to the
*        dynamic memory area.

*/

/* Local Variables: */
   ChanData *data;

/* Get a pointer t the structure holding information about the dynamic
   memory area. */
   data = (ChanData *) astChannelData;

/* Expand the dynamic memory and append a copy of the supplied text. This
   also updates the length of the dynamic memory area. */
   data->mem = astAppendString( data->mem, &(data->memsize), text );

/* Append a newline to the end so that ndg1ChSource can tell where the
   line ends. */
   data->mem = astAppendString( data->mem, &(data->memsize), "\n" );
}

static const char *ndg1ChSource( void ){
/*
*  Name:
*     ndg1ChSource

*  Purpose:
*     Return the next line of text from an Object dump held in a dynamic
*     memory area.

*  Invocation:
*     const char *ndg1ChSource( void )

*  Description:
*     This function is invoked by the astRead method to retrieve the next
*     line of an AST Object dump from a dynamically expanding memory area.

*  Returned Value:
*     Pointer to a null-terminated string containing the next line from
*     the AST Object dump.

*/

/* Local Variables: */
   ChanData *data;
   char *result;
   const char *newline;
   size_t len;

/* Get a pointer to the structure holding information about the dynamic
   memory area. */
   data = (ChanData *) astChannelData;

/* Find the next newline. */
   newline = strchr( data->mem, '\n' );
   if( newline ) {

/* Get the number of characters in the first line, including the
   newline. */
      len = newline - data->mem + 1;

/* Get a temporary copy of the line, including the newline. */
      result = data->linebuffer = astStore( data->linebuffer, data->mem, len );

/* Change the newline to a null. */
      result[ len - 1 ] = 0;

/* Increment the pointer to the next line and update the number of
   characters read. */
      data->mem += len;
      data->memsize += len;

/* If no newline is found, something must be wrong so return a NULL. */
   } else {
      result = NULL;
   }

   return result;
}

static void ndg1ClearProvId( Prov *prov, int *status ) {
/*
*  Name:
*     ndg1ClearProvId

*  Purpose:
*     Clears the integer hash code associated with a Prov structure.

*  Invocation:
*     void ndg1ClearProvId( Prov *prov, int *status )

*  Description:
*     This function clears the hash code describing the content of the
*     supplied Prov by setting it to zero, indicating that a new hash
*     code should be calculated next time the hash code is acquired using
*     ndg1GetProvId. It should be called when any of the text items
*     within a Prov (path, date or creator), or the list of parents, is
*     changed. It also clears the hash codes in all child Prov structures
*     (but not parents).

*  Parameters:
*     prov
*        Pointer to the Prov structure to be cleared.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int ichild;

/* Check the local error status. Partially constructed Prov structures
   can contain NULL parent or child pointers, so also return if the Prov
   pointer is NULL. */
   if( *status != SAI__OK || ! prov ) return;

/* Do nothing if the hash code has already been cleared in the supplied
   prov (the provid values within the children will also have been cleared
   at the same time). */
   if( prov->provid != 0 ) {

/* Clear the hash code in the supplied Prov.*/
      prov->provid = 0;

/* Clear the hash code in any child Provs.*/
      for( ichild = 0; ichild < prov->nchild; ichild++ ) {
         ndg1ClearProvId( prov->children[ ichild ], status );
      }
   }
}

static Prov *ndg1CopyProv( Prov *prov, int *status ){
/*
*  Name:
*     ndg1CopyProv

*  Purpose:
*     Make a deep copy of an existing Prov structure.

*  Invocation:
*     Prov *ndg1CopyProv( Prov *prov, int *status )

*  Description:
*     This function allocates dynamic memory holding a deep copy of the
*     supplied Prov structure.

*  Arguments:
*     prov
*        The Prov structure to be copied.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Prov structure. It should be freed using
*     ndg1FreeProv when no longer needed.

*/

/* Local Variables: */
   Prov *result;
   size_t len;
   int i;
   HistRec *inrec;
   HistRec *outrec;

/* Initialise */
   result = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Allocate memory and store a copy of the supplied Prov structure. */
   result = astStore( NULL, prov, sizeof( Prov ) );
   if( result ) {
      ISSUE( result );

/* For safety, first nullify all pointers in the copy. */
      result->path = NULL;
      result->date = NULL;
      result->creator = NULL;
      result->more = NULL;
      result->parents = NULL;
      result->children = NULL;
      result->hist_recs = NULL;

/* Produce copies of all the strings in the Prov structure. */
      len = prov->path ? strlen( prov->path ) + 1 : 0;
      result->path = astStore( NULL, prov->path, len*sizeof( char ) );

      len = prov->date ? strlen( prov->date ) + 1 : 0;
      result->date = astStore( NULL, prov->date, len*sizeof( char ) );

      len = prov->creator ? strlen( prov->creator ) + 1 : 0;
      result->creator = astStore( NULL, prov->creator, len*sizeof( char ) );

/* Store a deep copy of the "more" KeyMap. */
      if( prov->more ) result->more = astCopy( prov->more );

/* Create arrays to hold the parent and children pointers, and fill them
   with NULL pointers. */
      result->parents = astMalloc( sizeof( Prov * )*( prov->nparent ) );
      if( result->parents ) {
         for( i = 0; i < prov->nparent; i++ ) result->parents[ i ] = NULL;
      }

      result->children = astMalloc( sizeof( Prov * )*( prov->nchild ) );
      if( result->children ) {
         for( i = 0; i < prov->nchild; i++ ) result->children[ i ] = NULL;
      }

/* Since the parents are not currently known, clear the provid value. */
      ndg1ClearProvId( result, status );

/* Create an array of history records. */
      result->hist_recs = astMalloc( sizeof( HistRec )*( prov->nhrec ) );
      if( result->hist_recs ) {

/* Now store copies of all the History records. */
         outrec = result->hist_recs;
         inrec = prov->hist_recs;
         for( i = 0; i < prov->nhrec; i++, outrec++, inrec++ ) {

            len = inrec->date ? strlen( inrec->date ) + 1 : 0;
            outrec->date = astStore( NULL, inrec->date, len*sizeof( char ) );

            len = inrec->command ? strlen( inrec->command ) + 1 : 0;
            outrec->command = astStore( NULL, inrec->command, len*sizeof( char ) );

            len = inrec->text ? strlen( inrec->text ) + 1 : 0;
            outrec->text = astStore( NULL, inrec->text, len*sizeof( char ) );

            len = inrec->user ? strlen( inrec->user ) + 1 : 0;
            outrec->user = astStore( NULL, inrec->user, len*sizeof( char ) );

         }
      }
   }

/* If anything went wrong attempt to free the new Prov structure. */
   if( !astOK ) result = ndg1FreeProv( result, status );

/* Return the result */
   return result;
}

static const char *ndg1Date( int *status ){
/*
*  Name:
*     ndg1Date

*  Purpose:
*     Return the current UTC date and time in ISO Gregorian calendar
*     format.

*  Invocation:
*     const char *ndg1Date( int *status )

*  Description:
*     This function returns a pointer to a static string holding the
*     current UTC date and time in ISO Gregorian calendar format.

*  Arguments:
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstTimeFrame *tf;
   const char *result;

/* Initialise */
   result = " ";

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Create a TimeFrame representing UTC, then use the TimeFrame to
   determine and format the current time. */
   tf = astTimeFrame("TimeScale=UTC,Format(1)=iso.0");
   result = astFormat( tf, 1, astCurrentTime( tf ) );
   tf = astAnnul( tf );

   return result;
}

static Provenance *ndg1Decode( NdgProvenance *prov, const char *method,
                               int *status  ) {
/*
*  Name:
*     ndg1Decode

*  Purpose:
*     Convert an NdgProvenance identifier into a pointer to a Provenance
*     structure.

*  Invocation:
*     Provenance *ndg1Decode( NdgProvenance *prov, const char *method,
*                             int *status  )

*  Description:
*     This function returns a pointer to the Provenance structure
*     identified by the supplied NdgProvenance pointer.

*  Arguments:
*     prov
*        The identifier to be decoded.
*     method
*        The name of the public method being called.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The resulting pointer, or NULL if an error has occurred.

*/

/* Local Variables: */
   Provenance *result = NULL;
   int ok;

/* Start a new error reporting context so that we can annul any error
   that coccurs in astMapGet0P. */
   errBegin( status );

/* Cast the supplied pointer to an KeyMap pointer, and attempt to get the
   Provenance structure pointer out of it. */
   ok = prov ? astMapGet0P( (AstKeyMap *) prov, ID_KEY, (void **) &result ) : 0;

/* If an error occurred, annul the error and indicate we have no result. */
   if( *status != SAI__OK ) {
      ok = 0;
      errAnnul( status );
   }

/* Report an error if anything went wrong. */
   if( !ok || !result ) {
      msgSetc( "M", method );
      *status = SAI__ERROR;
      errRep( " ", "^M: Supplied NdgProvenance pointer is invalid.", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Return the Provenance pointer. */
   return result;
}

static char *ndg1DecodeProvData( char *pdata, int iprov, int version,
                                 HDSLoc *mloc, size_t nmore,
                                 Provenance *provenance, int *status ){
/*
*  Name:
*     ndg1DecodeProvData

*  Purpose:
*     Decode a single encoded ancestor description and store in a supplied
*     Prov structure.

*  Invocation:
*     char *ndg1DecodeProvData( char *pdata, int iprov, int version,
*                               HDSLoc *mloc, size_t nmore,
*                               Provenance *provenance, int *status )

*  Description:
*     This function decodes a stream of bytes containing a description of
*     a single ancestor NDF, and stores the decoded description in a
*     supplied Prov structure.

*  Arguments:
*     pdata
*        Pointer to the first byte to decode.
*     iprov
*        The zero-based index of the ancestor within "provenance".
*     version
*        The version number of the format used to encode the byte stream.
*     mloc
*        An HDS locator for an array of structures. Each structure
*        contains extra "more" information associated with a single ancestor.
*        May be NULL. Only used when decoding data created by version 6
*        of NDG.
*     nmore
*        The length of the array located by "mloc". Only used when decoding
*        data created by version 6 of NDG.
*     provenance
*        Pointer to the Provenance structure containing the ancestor.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the next byte to be read from the data stream, or NULL
*     if the ancestor could not be decoded.

*/

/* Macro to allocate memory holding a copy of a string read from the
   start of the remaining part of the data array (pointed to by "result"),
   checking that the string is not unbelievably long. MaxLen should be a
   generous upper limit on the reaonable expected length of the string. */
#define STORESTRING(Item,MaxLen) \
      len = result ? strlen( result ) + 1 : MaxLen; \
      if( len == NULL_DATA_LEN && !strcmp( result, NULL_DATA ) ) { \
         Item = NULL; \
         result += len; \
      } else if( len > 0 && len < MaxLen ) { \
         Item = astStore( NULL, result, len ); \
         result += len; \
      } else { \
         Item = NULL; \
         result = NULL; \
      }

/* Macro to copy an int from the start of the remaining part of the data
   array (pointed to by "result") to a specified variable. */
#define STOREINT(Item) \
      if( result ) { \
         Item = *( (int *) result ); \
         result += sizeof( int ); \
      }

/* Local Variables: */
   HDSLoc *cloc = NULL;
   HistRec *hist_rec;
   Prov *prov;
   char *result;
   hdsdim  dim;
   int imore;
   int iparent;
   int irec;
   int len;
   int parindex;
   int nparent;

/* Initialise */
   result = pdata;

/* Check inherited status amd supplied data array pointer. */
   if( *status != SAI__OK || !pdata ) return result;

/* Get a pointer to the Prov structure in which to store the values
   decoded from the integer data array. */
   prov = provenance->provs[ iprov ];

/* If the data array is encoded using encoding version 1 or 2 ... */
   if( version == 1 || version == 2 ) {

/* Get the ancestor's path. */
      STORESTRING( prov->path, 2000 );

/* Get the ancestor's date. */
      STORESTRING( prov->date, 100 );

/* Get the ancestor's hidden flag. */
      if( result ) prov->hidden = *(result++);

/* Get the ancestor's creator. */
      STORESTRING( prov->creator, 100 );

/* If we are decoding the Prov structure for the main NDF, get the
   history record hash code. */
      if( prov == provenance->main ) {
         STOREINT( prov->hhash );

/* If we are not decoding the provenance for the main NDF, decode any
   history records.  */
      } else {

/* First get the number of history records. Check it is not negative. */
         STOREINT( prov->nhrec );
         if( prov->nhrec < 0 ) {
            result = NULL;
         } else {

/* Create the array of HistRec structures. */
            prov->hist_recs = astMalloc( prov->nhrec*sizeof( HistRec ) );
            if( *status == SAI__OK ) {

/* Now decode each history record. */
               hist_rec = prov->hist_recs;
               for( irec = 0; irec < prov->nhrec; irec++,hist_rec++ ) {
                  STORESTRING( hist_rec->date, 100 );
                  STORESTRING( hist_rec->command, 2000 );
                  STORESTRING( hist_rec->user, 100 );
                  STORESTRING( hist_rec->text, 100000 );
               }
            }
         }
      }

/* Get the index of the associated MORE information. Pass on if there is no
   associated MORE information. */
      STOREINT( imore );
      if( imore >= 0 ) {

/* For version 1, MORE information is stored in a separate HDS structure. */
         if( version == 1 ) {

/* If there is an associated MORE structure, create a KeyMap holding all
   its components and store it in the returned Prov structure. */
            if( imore < nmore ) {
               dim = imore + 1;
               datCell( mloc, 1, &dim, &cloc, status );
               prov->more = ndg1Hd2ky( cloc, status );
               datAnnul( &cloc, status );
            } else {
               result = NULL;
            }

/* For version 2, MORE information is stored in KeyMap included in the
   integer array. */
         } else if( result ) {
            prov->more = (AstKeyMap *) ndg1ReadObject( result, &len, status );
            result += len;
         }
      }

/* Get the number of parents. Check it is not negative. */
      STOREINT( nparent );
      if( nparent < 0 ) result = NULL;

/* For each, establish a parent-child link between the Prov structure
   being constructed (the child) and the parent Prov structure. */
      for( iparent = 0; iparent < nparent; iparent++ ) {
         STOREINT( parindex );
         ndg1ParentChildIndex( provenance, parindex, iprov, 0, status );
      }

/* Store the index of the parent within the list of ancestors (zero is
   the main NDF). */
      prov->index = iprov;

/* Unsupported versions. */
   } else {
      result = NULL;
   }

/* Return the updated pointer into the encoded data array. */
   return result;

#undef STOREINT
#undef STORESTRING

}

static void ndg1Disown( Prov *parent, Prov *child, int *status ){
/*
*  Name:
*     ndg1Disown

*  Purpose:
*     Break a parent-child link between two supplied Prov structures.

*  Invocation:
*     void ndg1Disown( Prov *parent, Prov *child, int *status )

*  Description:
*     This function first checks to see that the supplied "parent" Prov
*     is genuinely a parent of the supplied "child" Prov. If so, it breaks
*     the parent-child relationship between them.
*
*     Each Prov structure describes a single NDF. The parent NDF is used
*     in the construction of ("gives birth to") the child NDF.

*  Arguments:
*     parent
*        The parent Prov structure.
*     child
*        The child Prov structure.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int j;
   int i;

/* Check the inherited status value, and the two pointers. */
   if( *status != SAI__OK || !parent || !child ) return;

/* NULLify all occurrences of the supplied parent within the list of
   parents stored in the supplied child. */
   for( i = 0; i < child->nparent; i++ ) {
      if( child->parents[ i ] == parent ) child->parents[ i ] = NULL;
   }

/* Shuffle the remaining parent pointers down to over-write the NULL
   pointers introduced above. */
   j = 0;
   for( i = 0; i < child->nparent; i++ ) {
      if( child->parents[ i ] ) {
         child->parents[ j++ ] = child->parents[ i ];
      }
   }

/* Nullify the trailing elements of the array that are no longer used. */
   for( i = j; i < child->nparent; i++ ) child->parents[ j ] = NULL;

/* Reduce the number of parents. */
   child->nparent = j;

/* NULLify all occurrences of the supplied child within the list of
   children stored in the supplied parent. */
   for( i = 0; i < parent->nchild; i++ ) {
      if( parent->children[ i ] == child ) parent->children[ i ] = NULL;
   }

/* Shuffle the remaining child pointers down to over-write the NULL
   pointers introduced above. */
   j = 0;
   for( i = 0; i < parent->nchild; i++ ) {
      if( parent->children[ i ] ) {
         parent->children[ j++ ] = parent->children[ i ];
      }
   }

/* Nullify the trailing elements of the array that are no longer used. */
   for( i = j; i < parent->nchild; i++ ) parent->children[ j ] = NULL;

/* Clear the ProvId hash code that describes the contents of the child
   (since it's list of parents has changed). */
   ndg1ClearProvId( child, status );

/* Reduce the number of children. */
   parent->nchild = j;

}

static NdgProvenance *ndg1Encode( Provenance *prov, int *status ) {
/*
*  Name:
*     ndg1Encode

*  Purpose:
*     Convert a pointer to a Provenance structure into an NdgProvenance
*     identifier.

*  Invocation:
*     NdgProvenance *ndg1Encode( Provenance *prov, int *status )

*  Description:
*     This function returns a pointer that can be used as an identifier
*     for the suspplied Provenance structure.

*  Arguments:
*     prov
*        The pointer to be encoded.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The encoded identifier, or NULL if an error occurrs.

*/

/* Local Variables: */
   AstKeyMap *keymap;

/* Check inherited status. */
   if( *status != SAI__OK ) return NULL;

/* Create a KeyMap and store the Provenance pointer in it. */
   keymap = astKeyMap( " " );
   astMapPut0P( keymap, ID_KEY, prov, NULL );

/* Ensure the KeyMap pointer is not annulled when the current AST Object
   context is ended. */
   astExempt( keymap );

/* Return the KeyMap pointer, cast to "NdgProvenance *". */
   return (NdgProvenance *) keymap;
}

static char *ndg1EncodeProvData( char *pdata, int ismain, Prov *prov,
                                 Provenance *provenance,
                                 size_t *pdlen, int *status ){
/*
*  Name:
*     ndg1EncodeProvData

*  Purpose:
*     Encode the contents of a Prov structure into a stream of bytes.

*  Invocation:
*     char *ndg1EncodeProvData( char *pdata, int ismain, Prov *prov,
*                               Provenance *provenance,
*                               size_t *pdlen, int *status )

*  Description:
*     This function converts a single Prov structure into a stream of
*     bytes and appends them to the end of a supplied array.

*  Arguments:
*     pdata
*        Pointer to a pre-allocated memory area in which to store the
*        encoded Prov data.
*     ismain
*        Non-zero if the Prov structure describes the main NDF, rather
*        than an ancestor.
*     prov
*        Pointer to the Prov structure to be encoded.
*     provenance
*        Pointer to the Provenance structure containing "Prov".
*     pdlen
*        Pointer to a size of the array, in bytes. This is increased by
*        this function to take account of the supplied Prov.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the extended memory area containing the encoded Prov.
*     Should be freed using astFree when no longer needed.

*/

/* Local Variables: */
   AstKeyMap *more = NULL;
   HistRec *hist_rec = NULL;
   char *path = NULL;
   char cval;
   const char *date = NULL;
   int imore;
   int *parents = NULL;
   int irec;
   int len;
   int npar;
   char *result;

/* Initialise the returned poitner to be the supplied pointer. */
   result = pdata;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* If this is not the main NDF provenance, use components from the Prov
   structure. */
   if( !ismain ) {
      path = prov->path;
      date = prov->date;
      more = prov->more;

/* If it is the main NDF provenance, do not write out the PATH (since the
   NDF may subsequently be moved), and force the DATE to the current
   date. Also no "more" information is stored for the main NDF since the
   information that may be stored in "more" will usually be available at
   other places in the NDF. It is the responsibilty of each application
   to extract the information it thinks is relevant from each input NDF
   and store it in the "more" structure with the input NDF's provenance
   information. */
   } else {
      path = NULL;
      date = ndg1Date( status );
      more = NULL;
   }

/* Append the path to the end of the array, extending the array as needed.
   We must always store something, even if the path is undefined. */
   if( path ) {
      len = astChrLen( path );
      if( len ) {
         if( path[ len ] != 0 ) {
            path[ len ] = 0;
            ndg1ClearProvId( prov, status );
         }
      }
      len++;
   }
   result = ndg1StoreCharData( result, path, len, pdlen, status );

/* Append the date to the end of the array, extending the array as needed. */
   if( date ) len = astChrLen( date ) + 1;
   result = ndg1StoreCharData( result, date, len, pdlen, status );

/* Store the hidden flag. */
   cval = prov->hidden ? 1 : 0;
   result = ndg1StoreCharData( result, &cval, sizeof( char ), pdlen, status );

/* Store the creator (the same code for both main and ancestor NDFs). */
   if( prov->creator ) {
      len = astChrLen( prov->creator );
      if( len ) {
          if( prov->creator[ len ] != 0 ) {
            prov->creator[ len ] = 0;
            ndg1ClearProvId( prov, status );
         }
      }
      len++;
   }
   result = ndg1StoreCharData( result, prov->creator, len, pdlen, status );

/* If we are writing the provenance for the main NDF, write out the history
   record hash code. */
   if( ismain ) {
      result = ndg1StoreCharData( result, &(prov->hhash),
                                  sizeof( prov->hhash ), pdlen, status );

/* If we are not writing the provenance for the main NDF, write out any
   history records.  */
   } else {

/* First write the number of history records. */
      result = ndg1StoreCharData( result, &(prov->nhrec),
                                  sizeof( prov->nhrec ), pdlen, status );

/* Now write out each history record. */
      hist_rec = prov->hist_recs;
      for( irec = 0; irec < prov->nhrec; irec++,hist_rec++ ) {

         len = hist_rec->date ? strlen( hist_rec->date ) + 1 : 0;
         result = ndg1StoreCharData( result, hist_rec->date, len, pdlen,
                                     status );

         len = hist_rec->command ? strlen( hist_rec->command ) + 1 : 0;
         result = ndg1StoreCharData( result, hist_rec->command, len, pdlen,
                                     status );

         len = hist_rec->user ? strlen( hist_rec->user ) + 1 : 0;
         result = ndg1StoreCharData( result, hist_rec->user, len, pdlen,
                                     status );

         len = hist_rec->text ? strlen( hist_rec->text ) + 1 : 0;
         result = ndg1StoreCharData( result, hist_rec->text, len, pdlen,
                                     status );
      }
   }

/* If the supplied Prov has a MORE structure, append it to the end of the
   array. */
   imore = more ? 1 : -1;
   result = ndg1StoreCharData( result, &imore, sizeof( imore ), pdlen,
                               status );
   result = ndg1StoreObject( result, (AstObject *) more, pdlen, status );

/* Store the number of parents. */
   result = ndg1StoreCharData( result, &(prov->nparent),
                               sizeof( prov->nparent ), pdlen, status );

/* Append the array of parent indices to the returned array. */
   if( prov->nparent ) {
      parents = ndg1ParentIndicies( prov, provenance, NULL, &npar, status );
      result = ndg1StoreCharData( result, parents, npar*sizeof( int ),
                                  pdlen, status );
      parents = astFree( parents );
   }

   return result;

}

static void ndg1FindAliens( Prov *prov, int *status ) {
/*
*  Name:
*     ndg1FindAliens

*  Purpose:
*     Flag Provs that are not part of the family tree.

*  Invocation:
*     static void ndg1FindAliens( Prov *prov, int *status )

*  Description:
*     If the supplied Prov has no children, the parent-child link between
*     it and each of its parents is broken, leaving it as an alien (i.e.
*     no parents and no children). Each parent of the supplied Prov is
*     then checked in the same way.

*  Arguments:
*     prov
*        The Prov structure to be checked.
*     status
*        Inherited status pointer.

*/

/* Local Variables: */
   Prov *parent;
   int i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Do nothing if the supplied Prov has any children. */
   if( prov->nchild == 0 ) {

/* Loop round each parent of the supplied Prov. Loop backwards to avoid
   changing the indices of remaining parents as we delete them. */
      for( i = prov->nparent - 1; i >= 0 && *status == SAI__OK; i-- ) {
         parent = prov->parents[ i ];

/* Break the parent-child link between the supplied Prov and the current
   parent. */
         ndg1Disown( parent, prov, status );

/* Now invoke this function recursively to see if the current parent
   now has no children (i.e. is an alien). */
         ndg1FindAliens( parent, status );
      }
   }
}

static int ndg1FindAncestorIndex( Prov *prov, Provenance *provenance,
                                  int *status ){
/*
*  Name:
*     ndg1FindAncestorIndex

*  Purpose:
*     Return the index of a Prov structure within the ANCESTORS array.

*  Invocation:
*     int ndg1FindAncestorIndex( Prov *prov, Provenance *provenance,
*                                int *status )

*  Description:
*     This function returns the integer index of a Prov structure within
*     the ANCESTORS array described by a Provenance structure.

*  Arguments:
*     prov
*        The Prov structure.
*     provenance
*        The Provenance structure.
*     status
*        Pointer to the inherited status variable.

* Returned Value:
*   The integer index. This is a one-based value (zero refers to the main
*   NDF).

*/

/* Local Variables: */
   int imain;
   int iprov;
   int result;

/* Initialise. */
   result = 0;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* If the index has already been found, return it. The "index" value
   should be set to -1 when elements within the "provenance->provs" are
   moved, added or deleted. */
   result = prov->index;

/* If the index is not known, find it now. */
   if( result < 0 ) {

/* Find the index of the supplied Prov within the provs array. Also
   note the index of the main NDF in the provs array. */
      imain = provenance->nprov + 1;
      for( iprov = 0; iprov < provenance->nprov; iprov++ ) {
         if( provenance->provs[ iprov ] == provenance->main ) imain = iprov;
         if( provenance->provs[ iprov ] == prov ) break;
      }

/* If it is greater than the index of the main NDF in "provs" then reduce
   it by one since the main NDF is not stored in the ANCESTORS array. */
      if( iprov > imain ) iprov--;

/* Sanity check. */
      if( iprov  < 0 || iprov > provenance->nprov - 2 ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSeti( "I", iprov + 1 );
            msgSeti( "N", provenance->nprov - 1 );
            errRep( " ", "Ancestor index ^I illegal - should be in range 1 to "
                    "^N (internal NDG programming error).",  status );
         }
      }

/* Add 1 to convert from zero-based to one-based and store it in the Prov
   structure. . */
      result = iprov + 1;
      prov->index = result;
   }

   return result;
}

static AstKeyMap *ndg1FormatProv( Provenance *provenance, int i, int base,
                                  AstKeyMap *mxlenkey, int *status ){
/*
*  Name:
*     ndg1FormatProv

*  Purpose:
*     Format information in a Prov structure.

*  Invocation:
*     AstKeyMap *ndg1FormatProv( Provenance *provenance, int i, int base,
*                                AstKeyMap *mxlenkey, int *status )

*  Description:
*     This function formats the information in a Prov structure and
*     puts the formatted strings into a new AstKeyMap. It also updates
*     the maximum length for any formatted item.
*
*     The returned KeyMap has string entries for a subset of the following
*     keys:
*
*        "ID" - the integer index within the ancestors array (zero for the
*               main NDF).
*
*        "PATH" - The full path or base name for the NDF (see "base").
*
*        "DATE" - The date of creation of the NDF.
*
*        "CREATOR" - The software item that created the NDF.
*
*        "PARENTS" - A comma-separated list of indices into the ancestors
*                    array that identifies the direct parents of the NDF.
*
*        "MORE" - A summary of the contents of the MORE structure associated
*                 with the NDF.
*
*        "HISTORY" - A vector entry holding one or more KeyMaps. Each
*               KeyMap contains items that describe an action performed
*               on the ancestor. The actions are stored in chronological
*               order within the vector entry. The last KeyMap in the
*               vector describes the action that created the ancestor NDF.
*               Any earlier KeyMaps in the vector describe any subsequent
*               actions performed on the ancestor NDF prior to it being
*               used in the creation of its parent. Each KeyMap contains
*               the following scalar character entries (all taken from the
*               corresponding record in the NDF HISTORY component):
*
*               - "DATE": The date and time of the action
*                         (e.g. "2009-JUN-24 14:00:53.752" ).
*               - "COMMAND": An indication of the command that performed
*                         the action (e.g. "WCSATTRIB (KAPPA 1.10-6)" ).
*               - "USER": The user name that performed the action
*                         (e.g. "dsb").
*               - "TEXT": The full text of the NDF history record. This is
*                         arbitrary, but for NDFs created by Starlink
*                         software it will usually include environment
*                         parameter values, and the full path of the
*                         command that performed the action.
*
*     A key will be missing in the returned KeyMap if the Provenance
*     structure does not contain that item.

*  Arguments:
*     provenance
*        Pointer to the Provenance structure that contains the Prov
*        structure that is to be formatted.
*     i
*        The index of the Prov structure to be formatted within the
*        supplied Provenance structure.
*     base
*        If non-zero, then the PATH field in the returned KeyMap holds the
*        base name of each NDF rather than the full path.
*     mxlenkey
*        Pointer to an existing KeyMap. On exit it will contain an entry
*        for each of the keys listed under "Description:" above (except
*        it will not have a "HISTORY" entry). All entries will be be
*        scalar integers. Each integer value is updated by this function
*        so that it holds the larger of the supplied value and the field
*        width used to format the corresponding item in the returned
*        KeyMap. The supplied value is left unchanged if the returned
*        KeyMap does not contain a value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new KeyMap describing the supplied Prov structure.

*/

/* Local Variables: */
   AstKeyMap *result = NULL;
   Prov *prov = NULL;
   char *p = NULL;
   char *list = NULL;
   char buf[ 20 ];
   int k;
   int mxlen;
   int nc;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Get a pointer to the Prov structure to be formatted. */
   prov = provenance->provs[ i ];

/* Create the returned KeyMap. */
   result = astKeyMap( " " );

/* Store the ID value. */
   nc = sprintf( buf, "%d", i );
   astMapPut0C( result, ID_NAME, buf, NULL );

/* Update the maximum length of the ID field. */
   if( astMapGet0I( mxlenkey, ID_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }
   astMapPut0I( mxlenkey, ID_NAME, mxlen, NULL );

/* If the supplied Prov structure has a "path" component, store it in the
   returned KeyMap, and update the maximum length of the "PATH" field. If
   "base" is non-zero, we only store the bit following the final "/"
   character. */
   if( prov->path ) {
      nc = strlen( prov->path );
      if( base ) {
         p = prov->path + nc - 1;
         while( p > prov->path && p[ -1 ] != '/' ) p--;
         nc = strlen( p );
      } else {
         p = prov->path;
      }

   } else {
      nc = 0;
      p = NULL;
   }

   if( nc ) astMapPut0C( result, PATH_NAME, p, NULL );

   if( astMapGet0I( mxlenkey, PATH_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }
   astMapPut0I( mxlenkey, PATH_NAME, mxlen, NULL );

/* If the supplied Prov structure has a "date" component, store it in the
   returned KeyMap, and update the maximum length of the "DATE" field. */
   nc = ( prov->date ) ? strlen( prov->date ) : 0;
   if( nc ) astMapPut0C( result, DATE_NAME, prov->date, NULL );

   if( astMapGet0I( mxlenkey, DATE_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }
   astMapPut0I( mxlenkey, DATE_NAME, mxlen, NULL );

/* If the supplied Prov structure has a "creator" component, store it in the
   returned KeyMap, and update the maximum length of the "CREATOR" field. */
   nc = ( prov->creator ) ? strlen( prov->creator ) : 0;
   if( nc ) astMapPut0C( result, CREATOR_NAME, prov->creator, NULL );

   if( astMapGet0I( mxlenkey, CREATOR_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }
   astMapPut0I( mxlenkey, CREATOR_NAME, mxlen, NULL );

/* Initialise a pointer to a dynamic string holding the comma-separated
   list of parent indices. */
   list = NULL;
   nc = 0;

/* If the supplied Prov structure has any parents, store a comma-separated
   list of the corresponding ANCESTOR indices in the returned KeyMap, and
   update the maximum length of the "PARENTS" field. */
   if( prov->nparent > 0 ) {

/* Loop round each of the parents. */
      for( k = 0; k < prov->nparent && *status == SAI__OK; k++ ) {

/* Find the index of this parent in the ANCESTORS array, and format it. */
         sprintf( buf, "%d", ndg1FindAncestorIndex( prov->parents[ k ],
                                                     provenance, status ) );

/* Append this string to the end of the comma-separated list, preceeding
   it with a comma unless it is the first value. */
         if( k > 0 ) list = astAppendString( list, &nc, "," );
         list = astAppendString( list, &nc, buf );
      }
   }

/* If the list is not empty, store it in the returned KeyMap, and then
   free it. */
   if( list ) {
      astMapPut0C( result, PARENTS_NAME, list, NULL );
      list = astFree( list );
   }

/* Find the maximum of the current field width and the previous maximum
   field width. */
   if( astMapGet0I( mxlenkey, PARENTS_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }

/* Store the new maximum field width. */
   astMapPut0I( mxlenkey, PARENTS_NAME, mxlen, NULL );

/* If the supplied Prov structure has a MORE object, create a summary
   of it, and store it in the returned KeyMap. */
   if( prov->more ) {
      list = ndg1KeyMapSummary( prov->more, 120, &nc, status );
      astMapPut0C( result, MORE_NAME, list, NULL );
      list = astFree( list );

/* Find the maximum of the current field width and the previous maximum
   field width. */
      if( astMapGet0I( mxlenkey, MORE_NAME, &mxlen ) ) {
         mxlen = ( nc > mxlen ) ? nc : mxlen;
      } else {
         mxlen = nc;
      }

/* Store the new maximum field width. */
      astMapPut0I( mxlenkey, MORE_NAME, mxlen, NULL );
   }

/* Store the History entry (a vector of KeyMaps, one for each history
   record in the Prov structure). */
   ndg1AddHistKM( result, HIST_NAME, prov, status );

/* Return the result. */
   return result;
}

static Prov *ndg1FreeProv( Prov *prov, int *status ){
/*
*  Name:
*     ndg1FreeProv

*  Purpose:
*     Release the resources used by a Prov structure.

*  Invocation:
*     Prov *ndg1FreeProv( Prov *prov, int *status )

*  Description:
*     This function releases all the resoures used by a Prov structure
*     and returns a NULL pointer.

*  Arguments:
*     prov
*        Pointer to the Prov to be released.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer.

*/

/* Local Variables: */
   HistRec *hist_rec;
   int i;

/* Check that something has been supplied, but ignore the inherited
   status since we want this function to execute even if an error has
   occurred. */
   if( !prov ) return NULL;

/* Release the memory used to store information within the Prov
   structure. */
   prov->path = astFree( prov->path );
   prov->date = astFree( prov->date );
   prov->creator = astFree( prov->creator );
   prov->parents = astFree( prov->parents );
   prov->children = astFree( prov->children );
   prov->nparent = 0;
   prov->nchild = 0;
   prov->hhash = 0;
   prov->provid = 0;

   hist_rec = prov->hist_recs;
   for( i = 0; i < prov->nhrec; i++,hist_rec++ ) {
      hist_rec->date = astFree( hist_rec->date );
      hist_rec->command = astFree( hist_rec->command );
      hist_rec->user = astFree( hist_rec->user );
      hist_rec->text = astFree( hist_rec->text );
   }
   prov->hist_recs = astFree( prov->hist_recs );
   prov->nhrec = 0;

   if( prov->more ) prov->more = astAnnul( prov->more );

/* Remove the Prov from the list of active Provs. */
   DEISSUE( prov );

/* Free the memory used to hold the Prov itself, and return a NULL pointer. */
   return astFree( prov );
}

static Provenance *ndg1FreeProvenance( Provenance *provenance,
                                       int free_provs, int *status ){
/*
*  Name:
*     ndg1FreeProvenance

*  Purpose:
*     Release the resources used by a Provenance structure.

*  Invocation:
*     Provenance *ndg1FreeProvenance( Provenance *provenance,
*                                     int free_provs, int *status )

*  Description:
*     This function releases all the resoures used by a Provenance
*     structure, including (if required) all the enclosed Prov structures,
*     and returns a NULL pointer.

*  Arguments:
*     provenance
*        Pointer to the Provenance to be released.
*     free_provs
*        Should the enclosed Prov structures also be freed?
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer.

*/

/* Local Variables; */
   int i;

/* Check that something has been supplied, but ignore the inherited
   status since we want this function to execute even if an error has
   occurred. */
   if( !provenance ) return NULL;

/* If required, free each Prov structure listed in the Provenance structure. */
   if( free_provs ) {
      for( i = 0; i < provenance->nprov; i++ ) {
         provenance->provs[ i ] = ndg1FreeProv( provenance->provs[ i ],
                                                    status );
      }

/* Otherwise, just nullify each Prov pointer listed in the Provenance
   structure. */
   } else {
      for( i = 0; i < provenance->nprov; i++ ) provenance->provs[ i ] = NULL;
   }

/* For safety, nullify other items in the structure. */
   provenance->main = NULL;
   provenance->nprov = 0;

/* Release the memory used to store information within the Prov
   structure. */
   provenance->provs = astFree( provenance->provs );

/* Free the memory used to hold the Provenance itself, and return a NULL
   pointer. */
   return astFree( provenance );
}

static int ndg1GetLogicalComp( HDSLoc *loc, const char *comp, int def,
                               int *status ){
/*
*  Name:
*     ndg1GetLogicalComp

*  Purpose:
*     Return a pointer to a string holding the value of an HDS _CHAR component

*  Invocation:
*     int ndg1GetLogicalComp( HDSLoc *loc, const char *comp, int def,
*                             int *status )

*  Description:
*     This function reads the value of a named logical component in a
*     supplied HDS structure. If the component exists its value is
*     returned. Otherwise, the supplied default value is returned (without
*     error).

*  Arguments:
*     loc
*        Locator for the HDS structure.
*     comp
*        The name of the component to read.
*     def
*        The default value to return, if the named component does not
*        exist in the supplied HDS structure.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The logical value; zero or non-zero.

*/

/* Local Variables: */
   HDSLoc *cloc = NULL;
   int there;
   int result;

/* Initialise */
   result = def;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Does the component exist? If not return with the NULL result. */
   datThere( loc, comp, &there, status );
   if( there ) {

/* If it is there, get a locator to it. */
      datFind( loc, comp, &cloc, status );

/* Read the value of the component into the results buffer. */
      datGet0L( cloc, &result, status );

/* Annul the component locator. */
      datAnnul( &cloc, status );
   }

/* Return the result. */
   return result;
}

static int ndg1GetProvId( Prov *prov, int *status ) {
/*
*  Name:
*     ndg1GetProvId

*  Purpose:
*     Returns an integer hash code for a Prov structure.

*  Invocation:
*     int ndg1GetProvId( Prov *prov, int *status )

*  Description:
*     This function returns an integer hash code describing the content
*     of the supplied Prov and all its ancestors (but not children). The
*     hash code within the Prov structure (prov->provid) should never be
*     accessed directly since it may be out of date. Instead, this function
*     should always be used to get the hash code, since it will update the
*     hash code when needed.

*  Parameters:
*     prov
*        Pointer to the Prov structure for which a hash code is required.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     An integer hash code.

*/

/* Local Variables: */
   unsigned int ihash = 0;
   int iparent;

/* A union used for converting between signed and unsigned ints. */
   union Tmp {
      int signed_val;
      unsigned int unsigned_val;
   } tmp;

/* Check the local error status. Partially constructed Prov striuctures
   can contain NULL parent or child pointers, so also return if the Prov
   pointer is NULL. */
   if( *status != SAI__OK || ! prov ) return 0;

/* We only need to calculate a new hash code if the existing one is invalid. */
   if( prov->provid == 0 ) {

/* For each text item in the Prov structure, create a hash code and then
   combine it with the existing hash code using a bit-wise exclusive-OR
   operation. */
      if( prov->path ) ihash = ndg1HashFun( prov->path, FNV1_32_INIT, status );
      if( prov->date ) ihash = ndg1HashFun( prov->date, ihash, status );
      if( prov->creator ) ihash = ndg1HashFun( prov->creator, ihash, status );

/* Obtain a hash code for each parent Prov, and combine it with the
   existing hash code using a bit-wise exclusive-OR operation. Shift the
   new hash code by one bit since for some reason this prevents some
   cases where identical hash codes are produced for different files. */
      for( iparent = 0; iparent < prov->nparent; iparent++ ) {
         ihash ^= ( ndg1GetProvId( prov->parents[ iparent ], status ) << 1 );
      }

/* For efficiency, store the hash code in the Prov structure so that it
   does not need to be recalculated next time it is required. Convert
   from unsigned to signed. */
      tmp.unsigned_val = ihash;
      prov->provid = tmp.signed_val;
   }

/* Return the hash code. */
   return prov->provid;
}

static char *ndg1GetTextComp( HDSLoc *loc, const char *comp, char *buf,
                              size_t buflen, int *status ){
/*
*  Name:
*     ndg1GetTextComp

*  Purpose:
*     Return a pointer to a string holding the value of an HDS _CHAR component

*  Invocation:
*     char *ndg1GetTextComp( HDSLoc *loc, const char *comp, char *buf,
*                            size_t buflen, int *status )

*  Description:
*     This function reads the value of a named character component in a
*     supplied HDS structure. If the component exists a pointer to its
*     value is returned. Otherwise, a NULL pointer is returned (without
*     error).

*  Arguments:
*     loc
*        Locator for the HDS structure.
*     comp
*        The name of the component to read.
*     buf
*        A pointer to a buff in which to store the value, if it exists.
*        If NULL is supplied, dynamic memory is allocated to store the
*        value.
*     buflen
*        The length of the supplied buffer (excluding the trailing null).
*        Not used if "buf" is NULL.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a string holding the component value, or a NULL pointer
*     if the component does not exist in the structure. If "buf" is NULL,
*     this pointer will point to newly allocated dynamic memory that should
*     be freed using astFree when no longer needed. This buffer will be
*     long enough to hold the whole component value. If "buf" is not NULL,
*     then the returned pointer will equal "buf" (if the component was
*     found).

*/

/* Local Variables: */
   HDSLoc *cloc = NULL;
   int there;
   char *result;

/* Initialise */
   result = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Does the component exist? If not return with the NULL result. */
   datThere( loc, comp, &there, status );
   if( there ) {

/* If it is there, get a locator to it. */
      datFind( loc, comp, &cloc, status );

/* If no buffer was supplied, create one of the correct length for the
   component (including the trailing null). */
      result = buf;
      if( !result ) {
         datLen( cloc, &buflen, status );
         result = astMalloc( ( buflen + 1 )*sizeof( char ) );
      }

/* Read the value of the component into the results buffer. */
      if( result ) datGet0C( cloc, result, buflen + 1, status );

/* Annul the component locator. */
      datAnnul( &cloc, status );
   }

/* Return the result. */
   return result;
}

static HDSLoc *ndg1GtAnc( HDSLoc *xloc, size_t *nanc, int *status ){
/*
*  Name:
*     ndg1GtAnc

*  Purpose:
*     Get a locator to the ANCESTORS array and return its length.

*  Invocation:
*     HDSLoc *ndg1GtAnc( HDSLoc *xloc, size_t *nanc, int *status )

*  Description:
*     This function returns a locator for the ANCESTORS array in the
*     supplied PROVENANCE structure, and also returns its length.

*  Arguments:
*     xloc
*        HDS locator for a PROVENANCE extension.
*     nanc
*        Pointer to an int in which to store the length of the ANCESTORS
*        array. Zero is returned if the supplied structure does not
*        contain an ANCESTORS array.
*     status
*        The global status.

*  Returned Value:
*     A pointer to an HDS locator for the ANCESTORS array in the supplied
*     structure, or NULL if the supplied structure does not contain an
*     ANCESTORS array.

*/

/* Local variables: */
   HDSLoc *result = NULL;
   int there;

/* Initialise */
   *nanc = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Check that the supplied provenance structure contains an ANCESTORS
   array. */
   datThere( xloc, ANCESTORS_NAME, &there, status );
   if( there ) {

/* Get a locator to it and find its size. */
      datFind( xloc, ANCESTORS_NAME, &result, status );
      datSize( result, nanc, status );
   }

/* Return the locator. */
   return result;
}

static unsigned int ndg1HashFun( const char *str,  unsigned int hval, int *status ){
/*
*  Name:
*     ndg1HashFun

*  Purpose:
*     Returns an integer hash code for a text string

*  Invocation:
*     unsigned int ndg1HashFun( const char *str,  unsigned int hval, int *status )

*  Description:
*     This function returns an integer hash code for the supplied text
*     string, using the FNV hash function. See the "Fowler-Noll-Vo hash
*     function" wikipedia entry for details.

*  Parameters:
*     str
*        Pointer to the text string. Trailing spaces are ignored.
*     hval
*        The previous calculated hash value. Used as the basis for the
*        next hash value in order to spread them out. Supply FNV1_32_INIT
*        on the first call.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     An integer hash code.

*/

/* Local Variables: */
   unsigned char *s;

/* Check the local error status. */
   if( *status != SAI__OK ) return 0;

/* FNV-1 hash each character in the string. */
   s = (unsigned char *) str;
   while( *s ) {

/* Multiply by the 32 bit FNV magic prime mod 2^32. */
      hval *= FNV_32_PRIME;

/* XOR the bottom with the current char. */
      hval ^= (unsigned int) *s++;
   }

/* Return our new hash value */
   return hval;
}

static AstKeyMap *ndg1Hd2ky( HDSLoc *loc, int *status ){
/*
*  Name:
*     ndg1Hd2ky

*  Purpose:
*     Create a KeyMap holding the contents of a given HDS object.

*  Invocation:
*     AstKeyMap *ndg1Hd2ky( HDSLoc *loc, int *status )

*  Description:
*     If the supplied HDS object is a structure, this function creates and
*     returns a new KeyMap that holds one entry for each component in the
*     supplied HDS structure. If the supplied HDS object is a primitive,
*     this function creates and returns a new KeyMap that holds a single
*     entry holding the value of the supplied HDS primitive.

*  Arguments:
*     loc
*        The HDS object to be copied.
*     status
*        Inherited status pointer.

*  Returned Value:
*     A pointer to a KeyMap holding an entry for each component int he
*     supplied HDS structure.

*/

/* Local Variables: */
   AstKeyMap *result;
   HDSLoc *cloc = NULL;
   int icomp;
   int ncomp;
   int prim;

/* Initialise */
   result = NULL;

/* Check status */
   if( *status != SAI__OK ) return result;

/* Create the empty KeyMap. */
   result = astKeyMap( " " );

/* If the object is a primitive, add an entry to the KeyMap describing
   it. */
   datPrim( loc, &prim, status );
   if( prim ) {
      atlHd2ky( loc, result, status );

/* If it is a structure, loop round its components. */
   } else {
      datNcomp( loc, &ncomp, status );
      for( icomp = 1; icomp <= ncomp; icomp++ ) {
         datIndex( loc, icomp, &cloc, status );

/* Create an entry in the KeyMap holding the component's value. */
         atlHd2ky( cloc, result, status );

/* Annul the component locator. */
         datAnnul( &cloc, status );
      }
   }

   return result;
}

static void ndg1Hout1( int nlines, char *const text[], int *status ){
/*
*  Name:
*     ndg1Hout1

*  Purpose:
*     Stores text from an NDF history record.

*  Invocation:
*     void ndg1Hout1( int nlines, char *const text[], int *status )

*  Description:
*     This is a service function called by ndfHout. It concatenates
*     all the lines of text from a single history record into a
*     single string, and then, optionally, calculates an integer hash
*     code for the text. It stores the hash code and concatenated string
*     in global variables (the NDF library does not provide any other
*     means of communication).

*  Parameters:
*     nlines
*        Number of lines of text.
*     text
*        Array of text lines.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Loop round all lines of text, concatenating them into a single
   dynamically allocated memory area. */
   history_length = 0;
   history_text = astAppendString( history_text, &history_length,
                                   text[ 0 ] );
   for( i = 1; i < nlines; i++ ) {
      history_text = astAppendString( history_text, &history_length, " " );
      history_text = astAppendString( history_text, &history_length,
                                      text[ i ] );
   }

/* If required, form a hash code from the concatenated text. */
   if( history_hash ) history_hash = ndg1HashFun( history_text,
                                                  history_hash, status );

}

static int ndg1IntCmp( const void *a, const void *b ){
/*
*  Name:
*     ndg1IntCmp

*  Purpose:
*     qsort int comparison function

*  Invocation:
*     int ndg1IntCmp(const void *a, const void *b );

*  Description:
*     This function returns a positive, zero, or negative value if
*     the integer pointed to by "b" is larger than, equal to, or
*     less than, the integer pointed to by "a". Note, this results in
*     qsort sorting into descending order, rather than ascending.

*  Arguments:
*     a
*        Pointer to the first integer to be compared.
*     b
*        Pointer to the second integer to be compared.

*/

    const int *ia = (const int *) a;
    const int *ib = (const int *) b;
    return *ib  - *ia;
}

static int ndg1IsWanted( AstXmlElement *elem, int *status ){
/*
*  Name:
*     ndg1IsWanted

*  Purpose:
*     See if an XML element should be read or not.

*  Invocation:
*     int ndg1IsWanted( AstXmlElement *elem, int *status )

*  Description:
*     This function is called by astXmlReadDocument to decide if the
*     client is interested in each element start tag which has just
*     been read.

*  Arguments:
*     elem
*        The XML element which has just been read.

*  Returned Value:
*     -1 : the element is not itself of interest but it may contain
*          an interesting element, so look through the content and
*          ask the client again about any elements found inside it.
*      0 : the element definately contains nothing of interest to
*          the client. kip its content and continue looking for new
*          elements.
*      1 : the element is definately of interest to the client so
*          read its contents and return a pointer to it.

*/

/* Local Variables: */
   const char *name = NULL;
   const char *utype = NULL;
   int result;

/* Initialise the result to indicate that the element is not itself of
   interest but it may contain an interesting element. */
   result = -1;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Check the element name is "GROUP". */
   if( !strcmp( astXmlGetName( elem ), "GROUP" ) ) {

/* Get any "name" and "utype" attributes in the element. */
      name = astXmlGetAttributeValue( elem, "name" );
      utype = astXmlGetAttributeValue( elem, "utype" );

/* Check they are both defined and have the expected values. If so,
   indicate that the element is definiately of interest. */
      if( name && !strcmp( name, EXT_NAME ) &&
          utype && !strncmp( utype, "hds_type:", 9 )
                && !strcmp( utype + 9, EXT_TYPE ) ) {
         result = 1;
      }
   }

/* Return the result. */
   return result;
}

static char *ndg1KeyMapSummary( AstKeyMap *km, int maxlen, int *nc, int *status ){
/*
*  Name:
*     ndg1KeyMapSummary

*  Purpose:
*     Produces a string summarising the contents of a KeyMap.

*  Invocation:
*     char *ndg1KeyMapSummary( AstKeyMap *km, int maxlen, int *nc,
*                                    int *status )

*  Description:
*     This function returns a dynamically allocated string holding a
*     summary of the contents of a KeyMap.

*  Arguments:
*     km
*        The KeyMap.
*     maxlen
*        The maximum allowed length of the returned string.
*     nc
*        Pointer to an integer in which to return the actual length of
*        the returned string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the dynamic string. Free it using astFree when it is no
*     longer needed.

*/

/* Local Constants: */
#define BUFLEN 100

/* Local Variables: */
   AstObject *aval;
   char *cval;
   char *result;
   char buf[ BUFLEN + 1 ];
   const char *key;
   int avlen;
   int ielem;
   int ikey;
   int limit;
   int mc;
   int nelem;
   int nkey;
   int type;

/* Initialise */
   result = NULL;
   *nc = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* The number of entries in the KeyMap. */
   nkey = astMapSize( km );

/* Loop round all entries in the KeyMap. */
   for( ikey = 0; ikey < nkey && *nc < maxlen; ikey++ ) {
      key = astMapKey( km, ikey );

/* Get the data type. */
      type = astMapType( km, key );

/* If this is not the first entry, append a comma to separate it from the
   previous entry. */
      if( ikey > 0 ) result = astAppendString( result, nc, "," );

/* Append the entry key name and an equals sign. */
      result = astAppendString( result, nc, key );
      result = astAppendString( result, nc, "=" );

/* Get the vector length of the entry. */
      nelem = astMapLength( km, key );

/* If a vector, start with an open parenthesis. */
      if( nelem > 1 ) result = astAppendString( result, nc, "(" );

/* Get the average number of characters per entry for the remaining
   enties, and find the limit for the next entry. */
      avlen = ( maxlen - *nc )/( nkey - ikey );
      limit = *nc + avlen;

/* Loop round all element. */
      for( ielem = 0; ielem < nelem && *nc < limit; ielem++ ) {

/* Delimiting comma comes before every element, except the first. */
         if( ielem > 0 ) result = astAppendString( result, nc, "," );

/* Get the value and append to the end of the returned string. */
         if( type == AST__UNDEFTYPE ) {
            result = astAppendString( result, nc, "<undef>" );

         } else if( type == AST__POINTERTYPE ) {
            result = astAppendString( result, nc, "<pointer>" );

         } else if( type == AST__OBJECTTYPE ) {
            astMapGetElemA( km, key, ielem, &aval );
            if( astIsAKeyMap( aval ) ) {
               cval = ndg1KeyMapSummary( (AstKeyMap *) aval, 2*avlen, &mc, status );
               result = astAppendString( result, nc, "{" );
               result = astAppendString( result, nc, cval );
               result = astAppendString( result, nc, "}" );
               cval = astFree( cval );
            } else {
               result = astAppendString( result, nc, "<" );
               result = astAppendString( result, nc, astGetC( aval, "Class" ) );
               result = astAppendString( result, nc, ">" );
            }

         } else {
            astMapGetElemC( km, key, BUFLEN, ielem, buf );
            result = astAppendString( result, nc, buf );
         }
      }

/* If there was not room for all the elements, append an ellipsis. */
      if( ielem < nelem ) result = astAppendString( result, nc, "..." );

/* If a vector, end with a closing parenthesis. */
      if( nelem > 1 ) result = astAppendString( result, nc, ")" );
   }

/* If there was not room for all the entries, append an ellipsis. */
   if( ikey < nkey ) {
      if( *nc > maxlen - 3 ) *nc = maxlen - 3;
      result = astAppendString( result, nc, "..." );
   } else if( *nc > maxlen ) {
      *nc = maxlen - 3;
      result = astAppendString( result, nc, "..." );
   }

   return result;
}

static Prov *ndg1MakeProv( int index, const char *path, const char *date,
                           const char *creator, int hhash, AstKeyMap *more,
                           Provenance *provenance, int hidden, int *status ){
/*
*  Name:
*     ndg1MakeProv

*  Purpose:
*     Make a new Prov structure and add it into an existing Provenance.

*  Invocation:
*     Prov *ndg1MakeProv( int index, const char *path, const char *date,
*                         const char *creator, int hhash, AstKeyMap *more,
*                         Provenance *provenance, int hidden, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Prov structure,
*     and stores the supplied values in it. The new structure is appended
*     to the end of the list of Prov structures held in the supplied
*     Provenance structure. No parents or children are stored in the
*     new Prov structure.

*  Arguments:
*     index
*        The index of the new prov structure within the ANCESTORS array,
*        or -1 if the index is not known. Zero should refer to the main
*        NDF.
*     path
*        Pointer to a string holding the path to the NDF described by the
*        new Prov structure. Can be NULL.
*     date
*        Pointer to a string holding the formatted UTC date and time at
*        which provenance information was recorded for the NDF.
*     creator
*        Pointer to a string holding an arbitrary identifier for the
*        software that created the NDF.
*     hhash
*        An integer hash code representing the youngest NDF history record
*        at the time the provenance information was written to the NDF.
*     creator
*        Pointer to a string holding an arbitrary identifier for the
*     more
*        Pointer to a KeyMap holding extra items of information about
*        the NDF. A deep copy is taken of the supplied KeyMap.
*     provenance
*        Pointer to an existing Provenance structure to which the new
*        Prov structure will be added, or NULL.
*     hidden
*        Should the ancestor be flagged as hidden?
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Prov structure. It should be freed using
*     ndg1FreeProv when no longer needed.

*/

/* Local Variables: */
   Prov *result;
   size_t len;

/* Initialise */
   result = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Allocate the memory for the new Prov structure. */
   result = astMalloc( sizeof( Prov ) );
   if( result ) {
      ISSUE( result );

/* Store copies of the supplied strings. Store NULL pointers for any
   unspecified strings. */
      result->index = index;

      len = path ? strlen( path ) + 1 : 0;
      result->path = astStore( NULL, path, len*sizeof( char ) );

      len = date ? strlen( date ) + 1 : 0;
      result->date = astStore( NULL, date, len*sizeof( char ) );

      len = creator ? strlen( creator ) + 1 : 0;
      result->creator = astStore( NULL, creator, len*sizeof( char ) );

/* Store the "hidden" flag. */
      result->hidden = hidden;

/* Indicate no hash code describing the contents of the Prov has yet been
   calculated. */
      result->provid = 0;

/* Store a deep copy of any supplied MORE KeyMap. */
      result->more = NULL;
      ndg1StoreMore( result, more, status );

/* Store the hash code that identifies the most recent history record. */
      result->hhash = hhash;

/* Indicate the ancestor does not have any history records yet. */
      result->hist_recs = NULL;
      result->nhrec = 0;

/* Initialise the lists of direct parents and children. */
      result->parents = NULL;
      result->nparent = 0;
      result->children = NULL;
      result->nchild = 0;

/* Extend the provs array in the supplied Provenance structure. */
      if( provenance ) {
         provenance->provs = astGrow( provenance->provs,
                                      provenance->nprov + 1,
                                      sizeof( Prov *) );

/* Store a pointer to the new Prov structure and increment the number of
   provs. */
         if( astOK ) provenance->provs[ provenance->nprov++ ] = result;
      }
   }

/* If anything went wrong attempt to free the Prov structure. */
   if( !astOK ) result = ndg1FreeProv( result, status );

/* Return the result */
   return result;
}

static Provenance *ndg1MakeProvenance( Prov *main, int * status ){
/*
*  Name:
*     ndg1MakeProvenance

*  Purpose:
*     Make a new Provenance structure for a main NDF.

*  Invocation:
*     Provenance *ndg1MakeProvenance( Prov *main, int * status )

*  Description:
*     This function allocates dynamic memory to hold a new Provenance
*     structure, and stores the supplied main NDF Prov structure in it.
*
*     A Provenance structure represents the entire NDF PROVENANCE extension
*     of a particular NDF (the "main" NDF, as opposed to the ancestor
*     NDFs that were used to create the main NDF).

*  Arguments:
*     main
*        Pointer to a Prov structure describing the main NDF. The
*        returned Provenance structure represents the PROVENANCE extension
*        in the main NDF.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Provenance structure. It should be freed using
*     ndg1FreeProvenance when no longer needed.

*/

/* Local Variables: */
   Provenance *result;

/* Initialise */
   result = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Allocate the memory for the new Provenance structure. */
   result = astMalloc( sizeof( Provenance ) );
   if( result ) {

/* Store a pointer to the main NDF Prov structure. */
      result->main = main;

/* Initialise the provs array to be one element long and put a
   pointer to the supplied main NDF Prov in it. */
      result->provs = astMalloc( sizeof( Prov * ) );
      if( astOK ) {
         result->provs[ 0 ] = main;
         result->nprov = 1;
      } else {
         result->nprov = 0;
      }
   }

/* If anything went wrong attempt to free the Provenance structure. */
   if( !astOK ) result = ndg1FreeProvenance( result, 1, status );

/* Return the result */
   return result;
}

static void ndg1ParentChild( Prov *parent, Prov *child, int check,
                             int *status ){
/*
*  Name:
*     ndg1ParentChild

*  Purpose:
*     Create a parent-child link between two supplied Prov structures.

*  Invocation:
*     void ndg1ParentChild( Prov *parent, Prov *child, int check,
*                           int *status )

*  Description:
*     This function joins the two supplied Prov structures together into
*     a parent-child relationsip.
*
*     Each Prov structure describes a single NDF. The parent NDF is used
*     in the construction of ("gives birth to") the child NDF.

*  Arguments:
*     parent
*        The parent Prov structure.
*     child
*        The child Prov structure.
*     check
*        If non-zero, only set up the parent-child link if it does not
*        already exist. This check is based on the ProvId values in the
*        Prov structures, and so is unreliable if the full family tree
*        has not yet been established (as is the case when reading in a
*        provenance extension for instance).
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int found;
   int i;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Check that the parent is not already a parent of the child. */
   found = 0;
   if( check ) {
      for( i = 0; i < child->nparent; i++ ) {
         if( ndg1TheSame( child->parents[ i ], parent, status ) ) {
            found = 1;
            break;
         }
      }
   }

/* Jump to the end if the parent-child link already exists. */
   if( !found ) {

/* Extend the array of parent pointers within the child, and add the new
   parent to the end of the array, incrementing the count of parents in the
   child. */
      child->parents = astGrow( child->parents, child->nparent + 1,
                                sizeof( Prov * ) );
      if( astOK ) child->parents[ child->nparent++ ] = parent;

/* Extend the array of child pointers within the parent, and add the new
   child to the end of the array, incrementing the count of children in the
   parent. */
      parent->children = astGrow( parent->children, parent->nchild + 1,
                                  sizeof( Prov * ) );
      if( astOK ) parent->children[ parent->nchild++ ] = child;
   }

/* Clear the ProvId hash code that describes the contents of the child
   (since it's list of parents has changed). */
   ndg1ClearProvId( child, status );

}

static void ndg1ParentChildIndex( Provenance *provenance, int iparent,
                                  int ichild, int check, int *status ){
/*
*  Name:
*     ndg1ParentChildIndex

*  Purpose:
*     Create a parent-child link between two Prov structures with given
*     indices.

*  Invocation:
*     void ndg1ParentChildIndex( Provenance *provenance, int iparent,
*                                int ichild, int check, int *status )

*  Description:
*     This function looks up the Prov structures with the specified
*     indices within the supplied Provenance structure, and joins them
*     together into a parent-child relationsip.
*
*     Each Prov structure describes a single NDF. The parent NDF is used
*     in the construction of ("gives birth to") the child NDF.

*  Arguments:
*     provenance
*        The Provenance structure holding the Prov structures that are to
*        be joined together.
*     iparent
*        The index within the "provenance->provs" array of the parent
*        Prov structure.
*     ichild
*        The index within the "provenance->provs" array of the child
*        Prov structure.
*     check
*        If non-zero, only set up the parent-child link if it does not
*        already exist. This check is based on the ProvId values in the
*        Prov structures, and so is unreliable if the full family tree
*        has not yet been established (as is the case when reading in a
*        provenance extension for instance).
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   Prov *parent;
   Prov *child;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Get pointers to the parent and child Prov structures. */
   parent = provenance->provs[ iparent ];
   child = provenance->provs[ ichild ];

/* set up the connection. */
   ndg1ParentChild( parent, child, check, status );
}

static int *ndg1ParentIndicies( Prov *prov, Provenance *provenance,
                                int *array, int *npar, int *status ){
/*
*  Name:
*     ndg1ParentIndicies

*  Purpose:
*     Find the indices of the direct parents of a given ancestor.

*  Invocation:
*     int *ndg1ParentIndicies( Prov *prov, Provenance *provenance,
*                              int *array, int *npar, int *status )

*  Description:
*     This function returns the indices of the direct parents of a
*     specified ancestor. The values can be stored in a supplied array,
*     or a new array can be allocated to hold them.

*  Arguments:
*     prov
*        The Prov structure.
*     provenance
*        The Provenance structure.
*     array
*        Pointer to an array in which to store the parent indices, or
*        NULL if a new array is to be allocated. The length of any
*        supplied array is not checked.
*     npar
*        Pointer to an int in which to return the number of parents, or
*        NULL if this value is not needed.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the array in which the indices have been stored. If
*     NULL was supplied for "array", the returned pointer should be freed
*     using astFree when it is no onger needed.

*/

/* Local Variables: */
   int *result;
   int k;

/* Initialise. */
   if( npar ) *npar = 0;

/* Check inherited status. */
   if( *status != SAI__OK ) return NULL;

/* If required, return the number of parents. */
   if( npar ) *npar = prov->nparent;

/* If required, allocate memory for the returned array. */
   if( array ) {
      result = array;
   } else {
      result = astMalloc( sizeof(int)*( prov->nparent ) );
   }

/* Check memory allocated successfully. */
   if( result ) {

/* Loop round each parent. */
      for( k = 0; k < prov->nparent && *status == SAI__OK; k++ ) {

/* Find and store the index of this parent in the ANCESTORS array. */
         result[ k ] = ndg1FindAncestorIndex( prov->parents[ k ],
                                              provenance, status );
      }
   }
   return result;
}

static int ndg1ProvCmp( const void *a, const void *b ){
/*
*  Name:
*     ndg1ProvCmp

*  Purpose:
*     qsort Prov comparison function

*  Invocation:
*     int ndg1ProvCmp(const void *a, const void *b );

*  Description:
*     This function returns a positive, zero, or negative value if
*     the ProvId value in the Prov pointer pointed to by "b" is larger
*     than, equal to, or less than, the ProvId value in the Prov pointer
*     pointed to by "a". Note, this results in qsort sorting into
*     descending ProvId order, rather than ascending.

*  Arguments:
*     a
*        Pointer to the first Prov pointer to be compared.
*     b
*        Pointer to the second Prov pointer to be compared.

*/
    int result;
    int status = SAI__OK;
    long int ida, idb;

    Prov *pa = *((Prov **) a );
    Prov *pb = *((Prov **) b );

    if( pa && pb ) {

/* Need to guard against integer overflow. */
       ida = (long int) ndg1GetProvId( pa, &status );
       idb = (long int) ndg1GetProvId( pb, &status );

       if( idb > ida ) {
          result = 1;
       } else if( idb < ida ) {
          result = -1;
       } else {
          result = 0;
       }

    } else if( pa ) {
       result = -1;

    } else if( pb ) {
       result = 1;

    } else {
       result = 0;
    }

    return result;

}

static void ndg1PurgeProvenance( Provenance *provenance,
                                 int *status ){
/*
*  Name:
*     ndg1PurgeProvenance

*  Purpose:
*     Purge duplicate ancestors from a Provenance structure.

*  Invocation:
*     void ndg1PurgeProvenance( Provenance *provenance,
*                               int *status )

*  Description:
*     This function removes any duplicated ancestors in the supplied
*     Provenance structure.

*  Arguments:
*     provenance
*        Pointer to the Provenance to be checked.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables; */
   Prov **provs;
   Prov *child;
   Prov *prov2;
   Prov *prov1;
   int *pids;
   int i;
   int ichild;
   int j;
   int provid;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Record debug info. */
   static_provenance = provenance;

/* Create a copy of the array of Prov pointers. */
   provs = astStore( NULL, provenance->provs,
                     provenance->nprov*sizeof( *provs ) );

/* Allocate memory for a copy of the ProvId values. */
   pids = astMalloc( provenance->nprov*sizeof( *pids ) );

   if( *status == SAI__OK ) {

/* Sort the pointers in this array so that they are in descreasing order
   of ProvId value. */
      qsort( provs, (size_t) provenance->nprov, sizeof( *provs ), ndg1ProvCmp );

/* Store the original ProvId values (as these will be cleared in the actual
   Prov structures by the calls to ndg1Disown below). */
      for( i = 0; i < provenance->nprov ; i++ ) {
         pids[ i ] = ndg1GetProvId( provs[ i ], status );
      }

/* Now work through the array looking for blocks of Provs that have equal
   ProvId value (i.e. may represent the same ancestor NDF). We ignore null
   Provs or alien Provs (i.e. Provs that have no parents or children -
   such as may be generated on a previous pass round this loop). */
      for( i = 0; i < provenance->nprov - 1; i++ ) {
         prov1 = provs[ i ];
         if( prov1 && ( prov1->nchild > 0 || prov1->nparent > 0 ) ) {
            provid = pids[ i ];

/* Move forward from the i'th Prov until we find a Prov which has a
   different provid value (i.e definately describes a different ancestor). */
            for( j = i + 1; j < provenance->nprov; j++ ) {
               prov2 = provs[ j ];
               if( prov2 && ( prov2->nchild > 0 || prov2->nparent > 0 ) ) {

/* If the j'th Prov definately does not refer to the same ancestor NDF as the
   i'th Prov, then we have reached the end of the block (if any) so break out
   of the "j" loop, setting the j'th Prov as the start of the next block. */
                  if( pids[ j ] != provid ){
                     i = j - 1;
                     break;

/* If the two ProvId values are the same, the two provenance structures
   may refer to the same NDF. Do further checks to make sure they do. */
                  } else if( ndg1TheSame( prov1, prov2, status ) ){

/* Check they have the same list of parents, and report an error if not. This
   is currently not a totally reliable check in that there are possible cases
   where the parents are indicated as being the same when in fact they are
   not the same. But this partial check is at least better than no check
   at all. */
                     if( !ndg1CheckSameParents( prov1, prov2, status ) ) {
                        if( *status == SAI__OK ) {

/* >>>  Temporary fix to dump info to see why this error is being
        reported. <<< */
                           ndg1DumpInfo( prov1, prov2, status );

                           *status = SAI__ERROR;
                           msgSetc( "N", prov1->path );
                           errRep( " ", "The ancestor NDF '^N' was included "
                                   "twice within the provenance structure, but "
                                   "each occurrence specifies a different set "
                                   "of parents.", status );
                        }

/* If the two provenance structures refer to the same NDF, then the first
   provenance structure adopts all the children of the second provenance
   structure, which is then left childless and so can be deleted later. For
   each child in the second structure, break its parent-child link with the
   second structure, and register it as a child of the first structure.
   Loop backwards through the child list to avoid changing the indices of
   remaining children as we delete them. */
                     } else {
                        for( ichild = prov2->nchild - 1; ichild >= 0; ichild-- ) {
                           child = prov2->children[ ichild ];
                           ndg1Disown( prov2, child, status );
                           ndg1ParentChild( prov1, child, 1, status );
                        }

/* The childless provenance structure is now of no use. If it is itself
   the only child of any of its parents, they too are of no use. Recurse
   up the family tree looking for such "alien" Prov structures. These can
   later be recognised by the fact that they have no parents and no
   children. */
                        ndg1FindAliens( prov2, status );
                     }
                  }
               }
            }
         }
      }

/* Free resources. */
      pids = astFree( pids );
      provs = astFree( provs );

   }

/* Free any aliens (i.e. Prov structures that have no parents and no
   children). */
   for( i = 0; i < provenance->nprov; i++ ) {
      prov1 = provenance->provs[ i ];
      if( prov1 ) {
         if( prov1->nchild == 0 &&  prov1->nparent == 0 ) {
            provenance->provs[ i ] = ndg1FreeProv( prov1, status );
         }
      }
   }

/* Shuffle all the remaining Provs down to the beginning of the Prov
   list, and adjust the length of the list. Read from index "i", write to
   index "j". */
   j = 0;
   for( i = 0; i < provenance->nprov; i++ ) {
      if( provenance->provs[ i ] ) {
         provenance->provs[ j++ ] = provenance->provs[ i ];
      }
   }
   provenance->nprov = j;

}

static void ndg1ReadHistRec( Prov *prov, int indf, int irec, int *hash,
                             int *status ){
/*
*  Name:
*     ndg1ReadHistRec

*  Purpose:
*     Read an NDF history record, store it in a new HistRec structure,
*     and calculate its hash code.

*  Invocation:
*     void ndg1ReadHistRec( Prov *prov, int indf, int irec, int *hash,
*                           int *status )

*  Description:
*     This function reads a specified record from the HISTORY *     of the given NDF. It then optionally creates a new empty HistRec
*     structure by extending the "hist_recs" array in the supplied Prov
*     structure and copies the history record into it. Additionally,
*     if "hash" is not NULL, it computes a hash code for the history
*     record is created and returned in "*hash".

*  Arguments:
*     prov
*        Pointer to the Prov structure in which to store the new HistRec
*        structure. This may be NULL, in which case no new HistRec
*        structure is created, but the history record is still read
*        and the hash code returned if "hash" is not NULL.
*     indf
*        The identifier for the NDF containing the history to read.
*     irec
*        The index of the history record to read (the oldest history
*        record in the NDF has index one).
*     hash
*        A pointer to an integer in which to store a hash code describing
*        the history record. A NULL pointer may be supplied if the
*        hash code is not needed.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define TEXT_LEN 200

/* Local Variables: */
   char text[ TEXT_LEN ];
   HistRec *histrec = NULL;

/* A union used for converting between signed and unsigned ints. */
   union Tmp {
      int signed_val;
      unsigned int unsigned_val;
   } tmp;

/* Initialise */
   if( hash ) *hash = 0;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* If a Prov structure was supplied, extend the "hist_recs" array
   within it and get a pointer to the new HistRec structure. */
   if( prov ) {
      prov->hist_recs = astGrow( prov->hist_recs, ++(prov->nhrec),
                                 sizeof( HistRec ) );
      histrec = prov->hist_recs + prov->nhrec - 1;
   }

/* Check the pointer can be used safely. */
   if( *status == SAI__OK ) {

/* Get the history text and, if required, calculate a hash value from
   it. The NDF library does not provide direct access to the history
   text, so we use the ndg1Hout1 function to process each line of
   text supplied to it by the NDF library. A pointer to the text is
   returned in the global variable "history_text", and the hash value
   is returned in global variable "history_hash" (the hash code is
   only calculated if "history_hash" initially contains a non-zero
   value, so initialise it using "hash"). */
      history_hash = ( hash != NULL ) ? FNV1_32_INIT : 0;
      ndfHout( indf, irec, ndg1Hout1, status );

/* If we are creating a new HistRec structure, store the text
   pointer in the new HistRec, and then nullify the global variable
   so that the memory is not re-used when ndg1Hout1 is called again. */
      if( histrec ) {
         histrec->text = history_text;
         history_text = NULL;

/* If we are just creating the hash code, we do not need the text
   any more so just free it. */
      } else {
         history_text = astFree( history_text );
      }

/* In either case, indicate that the "history_text" string is now
   empty. */
      history_length = 0;

/* Get the text of the APPLICATION item in the history record. */
      ndfHinfo( indf, "APPLICATION", irec, text, TEXT_LEN, status );

/* If we are creating a new HistRec structure, store a copy of the
   APPLICATION text in the new HistRec. */
      if( histrec ) histrec->command = astStore( NULL, text,
                                    (strlen( text ) + 1)*sizeof( char ) );

/* If we are creating a hash code, create a hash code for the
   APPLICATION text, combining it with the existing hash code. */
      if( hash ) history_hash = ndg1HashFun( text, history_hash, status );

/* Do the same for the other items of the HistRec structure. */
      ndfHinfo( indf, "DATE", irec, text, TEXT_LEN, status );
      if( histrec ) histrec->date = astStore( NULL, text,
                                    (strlen( text ) + 1)*sizeof( char ) );
      if( hash ) history_hash = ndg1HashFun( text, history_hash, status );

      ndfHinfo( indf, "USER", irec, text, TEXT_LEN, status );
      if( histrec ) histrec->user = astStore( NULL, text,
                                    (strlen( text ) + 1)*sizeof( char ) );
      if( hash ) history_hash = ndg1HashFun( text, history_hash, status );

/* The remaining items of history information are not stored in
   the HistRec structure, but we include them in the hash code if
   required. */
      if( hash ) {
         ndfHinfo( indf, "HOST", irec, text, TEXT_LEN, status );
         history_hash = ndg1HashFun( text, history_hash, status );

         ndfHinfo( indf, "NLINES", irec, text, TEXT_LEN, status );
         history_hash = ndg1HashFun( text, history_hash, status );

         ndfHinfo( indf, "REFERENCE", irec, text, TEXT_LEN, status );
         history_hash = ndg1HashFun( text, history_hash, status );

/* Return the hash code, converting from unsigned to signed. */
         tmp.unsigned_val = history_hash;
         *hash = tmp.signed_val;
      }
   }

/* Undefine local constants: */
#undef TEXT_LEN
}

static AstObject *ndg1ReadObject( char *mem, int *len, int *status ){
/*
*  Name:
*     ndg1ReadObject

*  Purpose:
*     Read an AST Object from a dump at the start of the supplied
*     dynamic memory area.

*  Invocation:
*     AstObject *ndg1ReadObject( char *mem, int *len, int *status )

*  Description:
*     This function creates a new AST Object by reading a dump from the
*     start of the supplied memory area.

*  Arguments:
*     mem
*        Pointer to the start of a dump of an AST Object.
*     len
*        Returned holding the number of characters read from the supplied
*        memory area.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new AST Object, or NULL if an error occurs.

*/

/* Local Variables: */
   AstChannel *ch;
   ChanData data;
   AstObject *result;

/* Initialise the returned pointer. */
   result = NULL;

/* Check the inherited status and the supplied Object pointer. */
   if( *status != SAI__OK || !mem ) return result;

/* Create an AST Channel to read the dump. */
   ch = astChannel( ndg1ChSource, NULL, " " );

/* Set up the data to be passed to the Channel source function
   (ndg1ChSource). */
   data.mem = mem;
   data.memsize = 0;
   data.linebuffer = NULL;
   astPutChannelData( ch, &data );

/* Read the object from the Channel (and thus from the supplied memory
   area). */
   result = astRead( ch );

/* Update the returned values. */
   *len = data.memsize;

/* Free resources. */
   data.linebuffer = astFree( data.linebuffer );
   ch = astAnnul( ch );

/* Return a pointer to the Object. */
   return result;
}

static Provenance *ndg1ReadOldProvenanceExtension( HDSLoc *xloc, const char *npath,
                                                   AstKeyMap *more,
                                                   const char *creator,
                                                   int isroot, int *status ){
/*
*  Name:
*     ndg1ReadOldProvenanceExtension

*  Purpose:
*     Create a new Provenance structure from a non-encoded HDS PROVENANCE
*     extension.

*  Invocation:
*     Provenance *ndg1ReadOldProvenanceExtension( HDSLoc *xloc,  const char *npath,
*                                                 AstKeyMap *more,
*                                                 const char *creator,
*                                                 int isroot, int *status )

*  Description:
*     This function is just like ndg1ReadProvenanceExtension except that
*     it expects the supplied provenance extension to conform to the
*     structure used by NDG prior to version 6.0. Prior to version 6.0
*     each item of information within a Prov structure was held in a separate
*     HDS component. This caused speed and size problems when reading
*     or writing provenance extensions that describe several thousand
*     ancestors, and so version 6.0 of NDG changed to using a single HDS
*     _INTEGER array to hold all the provenance information in a binary
*     encoded form.
*
*     This function is retained so that NDFs containing pre version 6.0
*     Provenance extensions can still be read.

*  Arguments:
*     xloc
*        An HDS structure holding provenance information. Can be NULL.
*     npath
*        The path of the NDF from which the provenance was read. Can be
*        NULL.
*     more
*        An optional KeyMap holding additional information. This is
*        stored in the main Prov structure in the returned Provenance.
*     creator
*        An optional text string to be stored as the "creator" string in
*        the returned Prov structure. This is only used if the supplied
*        provenance has no creator string.
*     isroot
*        If non-zero, then the NDF is treated like a root NDF (that is,
*        information about any parents in the provenance information for
*        the NDF is ignored, and it is assumed the NDF has no parents).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Provenance structure. It should be freed using
*     ndg1FreeProvenance when no longer needed.

*/

/* Local Variables: */
   AstKeyMap *more2 = NULL;
   HDSLoc *aloc = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *hcloc = NULL;
   HDSLoc *hloc = NULL;
   HDSLoc *mloc = NULL;
   HDSLoc *ploc = NULL;
   HistRec *hist_rec;
   Prov *anc_prov = NULL;
   Prov *main_prov = NULL;
   Provenance *result = NULL;
   char *date = NULL;
   char *path = NULL;
   char creator_buf[ CREATOR_LEN + 1 ];
   char date_buf[ DATE_LEN + 1 ];
   char path_buf[ PATH_LEN + 1 ];
   hdsdim  dim[ 1 ];
   hdsdim  subs;
   int *parents = NULL;
   int hhash;
   int hidden;
   int i;
   int irec;
   int j;
   int there;
   size_t nanc;
   size_t nhrec;
   size_t npar;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Indicate we do not as yet have a hash of the history record associated
   with the operation that created the NDF from which the provenance
   information is being read. */
   hhash = 0;

/* See if a provenance extension was supplied. If we are pretending that the
   NDF is a root NDF (i.e. if we are ignoring any provenance information
   within it) then skip this bit. */
   if( xloc && !isroot ) {

/* Get the values of the DATE, CREATOR and HASH components, if they exist. */
      date = ndg1GetTextComp( xloc, DATE_NAME, date_buf,
                              DATE_LEN, status );
      creator = ndg1GetTextComp( xloc, CREATOR_NAME, creator_buf,
                                 CREATOR_LEN, status );
      datThere( xloc, HASH_NAME, &there, status );
      if( there ) {
         datFind( xloc, HASH_NAME, &cloc, status );
         datGet0I( cloc, &hhash, status );
         datAnnul( &cloc, status );
      }

/* Get a locator for the ANCESTORS array, and get its length. */
      aloc = ndg1GtAnc( xloc, &nanc, status );
   }

/* Create a Prov structure to describe the main NDF. */
   main_prov = ndg1MakeProv( 0, npath, date, creator, hhash, more,
                             NULL, 0, status );

/* Create the basic Provenance structure. As yet it only contains
   information about the main NDF. */
   result = ndg1MakeProvenance( main_prov, status );

/* If the PROVENANCE extension specifies any ancestors for the main NDF,
   then create a Prov structure for each and add it into the Provenance
   struture. Links to parents and children are left blank for the moment. */
   if( aloc ) {

/* Loop round the elements of the ANCESTORS array. */
      for( i = 1; i <= nanc; i++ ) {
         dim[ 0 ] = i;
         datCell( aloc, 1, dim, &cloc, status );

/* Get the PATH, DATE, CREATOR and HIDDEN components. */
         date = ndg1GetTextComp( cloc, DATE_NAME, date_buf,
                                 DATE_LEN, status );
         path = ndg1GetTextComp( cloc, PATH_NAME, path_buf,
                                 PATH_LEN, status );
         creator = ndg1GetTextComp( cloc, CREATOR_NAME, creator_buf,
                                 CREATOR_LEN, status );
         hidden = ndg1GetLogicalComp( cloc, HIDDEN_NAME, 0, status );

/* If the ancestor has a MORE structure, get a locator for it, convert it
   to a KeyMap, and then annul the locator. */
         datThere( cloc, MORE_NAME, &there, status );
         if( there ) {
            datFind( cloc, MORE_NAME, &mloc, status );
            more2 = ndg1Hd2ky( mloc, status );
            datAnnul( &mloc, status );
         } else {
            more2 = NULL;
         }

/* Create a Prov structure to describe the current ancestor, and add it
   into the returned Provenance structure. */
         anc_prov = ndg1MakeProv( i, path, date, creator, 0, more2,
                                  result, hidden, status );

/* We now copy any History records from the ancestor into the Prov
   structure. */
         datThere( cloc, HIST_NAME, &there, status );
         if( there ) {
            datFind( cloc, HIST_NAME, &hloc, status );
            datSize( hloc, &nhrec, status );

            hist_rec = astMalloc( sizeof( HistRec )*nhrec );
            if( hist_rec ) {

               anc_prov->hist_recs = hist_rec;
               anc_prov->nhrec = nhrec;

               for( irec = 0; irec < nhrec; irec++,hist_rec++ ) {
                  subs = irec + 1;
                  datCell( hloc, 1, &subs, &hcloc, status );

                  hist_rec->date = ndg1GetTextComp( hcloc, DATE_NAME, NULL, 0, status );
                  hist_rec->command = ndg1GetTextComp( hcloc, COMMAND_NAME, NULL, 0, status );
                  hist_rec->user = ndg1GetTextComp( hcloc, USER_NAME, NULL, 0, status );
                  hist_rec->text = ndg1GetTextComp( hcloc, TEXT_NAME, NULL, 0, status );

                  datAnnul( &hcloc, status );
               }
            }

            datAnnul( &hloc, status );
         }

/* Free resources. */
         if( more2 ) more2 = astAnnul( more2 );
         datAnnul( &cloc, status );
      }

/* Get the list of parents from the main NDF. */
      datThere( xloc, PARENTS_NAME, &there, status );
      if( there ) {
         datFind( xloc, PARENTS_NAME, &ploc, status );
         datMapV( ploc, "_INTEGER", "READ", (void **) &parents, &npar,
                  status );

/* For each, establish a parent-child link between the main NDF Prov
   structure and the parent Prov structure. The values in the PARENTS
   array are one-based, and the "provs" array is zero-based. However,
   "provs[0]" is the main NDF and so the first ANCESTORS value will be
   stored at "prov1[1]". So this means we can use the PARENT values
   directly without needing to change from one-based to zero-based. */
         for( i = 0; i < npar; i++ ) {
            ndg1ParentChildIndex( result, parents[ i ], 0, 1, status );
         }

/* Free the main NDF parents list. */
         datUnmap( ploc, status );
         datAnnul( &ploc, status );
      }

/* Now loop through the ANCESTORS array again. */
      for( i = 1; i <= nanc; i++ ) {
         dim[ 0 ] = i;
         datCell( aloc, 1, dim, &cloc, status );

/* Get the list of parents from the ancestor NDF. */
         datThere( cloc, PARENTS_NAME, &there, status );
         if( there ) {
            datFind( cloc, PARENTS_NAME, &ploc, status );
            datMapV( ploc, "_INTEGER", "READ", (void **) &parents, &npar,
                     status );

/* For each, establish a parent-child link between the ancestor NDF Prov
   structure and the parent Prov structure. */
            for( j = 0; j < npar; j++ ) {
               ndg1ParentChildIndex( result, parents[ j ], i, 1, status );
            }

/* Free the main NDF parents list. */
            datUnmap( ploc, status );
            datAnnul( &ploc, status );
         }

/* Free locators. */
         datAnnul( &cloc, status );
      }

   }

/* Free resources */
   if( aloc ) datAnnul( &aloc, status );

/* If an error occurred, free the result. */
   if( !astOK ) result = ndg1FreeProvenance( result, 1, status );

/* Return the result */
   return result;
}

static Provenance *ndg1ReadProvenanceExtension( HDSLoc *xloc, const char *npath,
                                                AstKeyMap *more, const char *creator,
                                                int isroot, int *status ){
/*
*  Name:
*     ndg1ReadProvenanceExtension

*  Purpose:
*     Create a new Provenance structure from an HDS PROVENANCE extension.

*  Invocation:
*     Provenance *ndg1ReadProvenanceExtension( HDSLoc *xloc,  const char *npath,
*                                              AstKeyMap *more, const char *creator,
*                                              int isroot, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Provenance
*     structure, and copies provenance information from the supplied HDS
*     structure into the new Provenance structure.
*
*     If a NULL pointer is supplied for "xloc", then the returned
*     Provenance structure contains only a single Prov structure, for the
*     supplied NDF itself. The only items stored in this Prov structure is
*     the NDF path, plus any "more" and "creator" values supplied as
*     arguments to this function.

*  Arguments:
*     xloc
*        An HDS structure holding provenance information. Can be NULL.
*     npath
*        The path of the NDF from which the provenance was read. Can be
*        NULL.
*     more
*        An optional KeyMap holding additional information. This is
*        stored in the main Prov structure in the returned Provenance.
*     creator
*        An optional text string to be stored as the "creator" string in
*        the returned Prov structure. This is only used if the supplied
*        provenance has no creator string.
*     isroot
*        If non-zero, then the NDF is treated like a root NDF (that is,
*        information about any parents in the provenance information for
*        the NDF is ignored, and it is assumed the NDF has no parents).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Provenance structure. It should be freed using
*     ndg1FreeProvenance when no longer needed.

*/

/* Local Variables: */
   HDSLoc *dloc = NULL;
   HDSLoc *mloc = NULL;
   Prov *main_prov = NULL;
   Provenance *result = NULL;
   char type[DAT__SZTYP + 1];
   hdsdim dims[ 10 ];
   int actdim;
   int iprov;
   int nprov;
   int there;
   int version;
   size_t nmore = 0;
   char *pdata = NULL;
   char *pdend = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* First handle cases where a provenance extension is available and is to
   be used. */
   if( xloc && !isroot ) {

/* If the provenance extension contains a component called ANCESTORS,
   then it uses the old non-encoded format. [This old format was replaced
   by a format that encodes all the provenance info into a single integer
   array. This avoid the large speed and size overheads associated with
   complex HDS structures.] If the extension uses the old format, call a
   function to read it. */
      datThere( xloc, ANCESTORS_NAME, &there, status );
      if( there ) {
         result = ndg1ReadOldProvenanceExtension( xloc, npath, more,
                                                  creator, isroot, status );

/* Otherwise, we use the new encoded format. */
      } else {

/* Get a locator to the DATA component within the supplied provenance
   extension. This is a 1-D integer array containing the binary encoded
   provenance information. Check it is a 1D _INTEGER array. If so, map it. */
         datThere( xloc, DATA_NAME, &there, status );
         if( there ) {
            datFind( xloc, DATA_NAME, &dloc, status );
            datType( dloc, type, status );
            datShape( dloc, 10, dims, &actdim, status );
            if( !strcmp( type, "_INTEGER" ) && actdim == 1 ) {
               datMap( dloc, "_INTEGER", "READ", 1, dims, (void **) &pdata,
                       status );
               pdend = pdata + dims[ 0 ]*sizeof( int );
            }
         }

/* The first int is the version number. */
         if( pdata ) {
            version = ( (int *) pdata )[ 0 ];
            pdata += sizeof( int );
            if( version < 1 || version > 1000000 ) pdata = NULL;
         }

/* The second int is the number of Prov structures. */
         if( pdata ) {
            nprov = ( (int *) pdata )[ 0 ];
            pdata += sizeof( int );
            if( nprov < 1 || nprov > 1000000 ) pdata = NULL;
         }

/* If OK, create a Provenance structure, and fill it with "nprov" new,
   zero-filled, Prov structures. */
         if( pdata ) {
            result = astMalloc( sizeof( Provenance ) );
            if( *status == SAI__OK ) {
               result->nprov = nprov;
               result->main = NULL;
               result->provs = astMalloc( nprov*sizeof( Prov * ) );
               if( *status == SAI__OK ) {
                  for( iprov = 0; iprov < nprov; iprov++ ) {
                     (result->provs)[ iprov ] = astCalloc( 1, sizeof( Prov ) );
                  }
                  result->main = (result->provs)[ 0 ];
               }
            }

/* If the extension uses HDS structures to hold the MORE information, see
   if there is a MORE component, get a locator to it, and get its length. */
            if( version <= PDATA_VERSION_HDSMORE ) {
               datThere( xloc, MORE_NAME, &there, status );
               if( there ) {
                  datFind( xloc, MORE_NAME, &mloc, status );
                  datSize( mloc, &nmore, status );
               }
            } else {
               mloc = NULL;
               nmore = 0;
            }

/* Decode and copy each Prov structure from the integer array into the
   empty Prov structures created above. */
            for( iprov = 0; iprov < nprov; iprov++ ) {
               pdata = ndg1DecodeProvData( pdata, iprov, version, mloc, nmore,
                                           result, status );
            }

/* Force the Prov structure for the main NDF to use the supplied values
   for path and more. */
            if( result && result->main ) {
               if( npath ) {
                  result->main->path = astStore( result->main->path, npath,
                                                 strlen( npath ) + 1 );
               } else {
                  result->main->path = astFree( result->main->path );
               }
               ndg1StoreMore( result->main, more, status );
            }

/* If the data was decoded succesfully, check that the next integer has
   the value given by END_FLAG. */
            if( pdata ) {
               int end_value = ( (int *) pdata )[ 0 ];
               pdata += sizeof( int );
               if( end_value != END_FLAG ) pdata = NULL;
            }

/* Check that we have not read beyond the end of the data array. */
            if( pdata ) {
               if( pdata > pdend ) {
                  pdata = NULL;

/* If not, check that any remaining bytes in the data array are equal to
   PAD_FLAG. */
               } else {
                  while( pdata < pdend ) {
                     if( *pdata++ != PAD_FLAG ) {
                        pdata = NULL;
                        break;
                     }
                  }
               }
            }

/* If the data could not be decoded, free the returned Provenance
   structure. */
            if( !pdata ) result = ndg1FreeProvenance( result, 1, status );
         }

/* Issue a warning if the provenance information could not be decoded. */
         if( !pdata ) {
            msgBlank( status );
            if( npath ) {
               msgSetc( "N", npath );
               msgOut( "", "!! Could not read full provenance information "
                       "from ^N - continuing anyway...", status );
            } else {
               msgOut( "", "!! Could not read full input provenance "
                       "information - continuing anyway...", status );
            }
         }

/* Free resources. */
         if( dloc ) datAnnul( &dloc, status );
         if( mloc ) datAnnul( &mloc, status );
      }
   }

/* If no Provenance structure was created above (either because no
   provenance extension is available (or is not to be used), or an error
   occurred whilst reading the provenance extension), create one now
   describing just the supplied NDF. */
   if( !result && *status == SAI__OK ) {
      main_prov = ndg1MakeProv( 0, npath, NULL, creator, 0, more,
                                NULL, 0, status );
      result = ndg1MakeProvenance( main_prov, status );
   }

/* If an error occurred, free the result. */
   if( !astOK ) result = ndg1FreeProvenance( result, 1, status );

/* Return the result */
   return result;
}

static Provenance *ndg1ReadProvenanceNDF( int indf, AstKeyMap *more,
                                          const char *creator,
                                          int isroot, int *status ){
/*
*  Name:
*     ndg1ReadProvenanceNDF

*  Purpose:
*     Create a new Provenance structure from an NDF PROVENANCE extension.

*  Invocation:
*     Provenance *ndg1ReadProvenanceNDF( int indf, AstKeyMap *more,
*                                        const char *creator,
*                                        int isroot, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Provenance
*     structure, and copies provenance information from the PROVENANCE
*     extension of the supplied NDF into the new Provenance structure.
*     If the NDF does not have a PROVENANCE extension, or if no NDF is
*     supplied, then the returned Provenance structure contains only a
*     single Prov structure, for the supplied NDF itself (an anonymous
*     "pretend" NDF is no NDF identifier was supplied). The only items
*     stored in this Prov structure is the NDF path (NULL if no NDF was
*     supplied), plus any "more" and "creator" values supplied as
*     arguments to this function.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information
*        to be read. This may be NDF__NOID, in which case a new anonymous
*        provenance structure will be created and returned.
*     more
*        An optional KeyMap holding additional information about the NDF.
*        This is stored in the main Prov structure in the returned Provenance.
*     creator
*        An optional text string to be stored as the "creator" string in
*        the returned Prov structure. This is only used if the supplied
*        NDF has no creator string in its provenance.
*     isroot
*        If non-zero, then the NDF is treated like a root NDF (that is,
*        information about any parents in the provenance information for
*        the NDF is ignored, and it is assumed the NDF has no parents).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Provenance structure. It should be freed using
*     ndg1FreeProvenance when no longer needed.

*/

/* Local Variables: */
   HDSLoc *xloc = NULL;
   Provenance *result = NULL;
   char *path = NULL;
   char path_buf[ PATH_LEN + 1 ];
   int path_len;
   int there;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* See if the supplied NDF has a provenance extension. If it has, get a
   HDS locator to it. If we are pretending that the NDF is a root NDF
   (i.e. if we are ignoring any provenance information within it) then skip
   this bit. */
   if( indf != NDF__NOID ) {
      ndfXstat( indf, EXT_NAME, &there, status );
   } else {
      there = 0;
   }
   if( there && !isroot ) ndfXloc( indf, EXT_NAME, "Read", &xloc, status );

/* Get the path to the supplied NDF. */
   if( indf != NDF__NOID ) {
      ndfMsg( "NDF", indf );
      msgLoad( " ", "^NDF", path_buf, PATH_LEN, &path_len, status );
      path = path_buf;
   } else {
      path = NULL;
   }

/* Read the information from the extension. */
   result = ndg1ReadProvenanceExtension( xloc, path, more, creator,
                                         isroot, status );

/* Free resources */
   if( xloc ) datAnnul( &xloc, status );

/* Return the result */
   return result;
}

static Provenance *ndg1ReadProvenanceXml( const char *xml, const char *path,
                                          const char *creator, int *status ){
/*
*  Name:
*     ndg1ReadProvenanceXml

*  Purpose:
*     Create a new Provenance structure from XML text taken from a VOTABLE.

*  Invocation:
*     Provenance *ndg1ReadProvenanceXml( const char *xml, const char *path,
*                                        const char *creator, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Provenance
*     structure, and reads provenance information from the supplied XML
*     text (which is assumed to have been created by ndg1WriteProvenanceXml).

*  Arguments:
*     xml
*        The XML text read from the VOTABLE.
*     path
*        The path to the file from which the XML provenance text was read.
*     creator
*        An optional text string to be stored as the "creator" string in
*        the returned Prov structure. This is only used if there is no
*        creator string in the supplied provenance.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Provenance structure. It should be freed using
*     ndg1FreeProvenance when no longer needed.

*/

/* Local Variables: */
   AstXmlDocument *doc = NULL;
   AstXmlElement *elem = NULL;
   HDSLoc *tloc = NULL;
   HDSLoc *xloc = NULL;
   Provenance *result = NULL;
   const char *text;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Convert the supplied text into an AstXmlElement. */
   text = xml;
   elem = astXmlReadDocument( &doc, ndg1IsWanted, 0, ndg1XmlSource,
                              (void *) &text );

/* Create a temporary HDS structure to hold the provenance information
   read from the XML. */
   tloc = ndg1Temp( "PROV_TEMP", 0, NULL, status);

/* Convert the XML into an HDS structure. This should replicate the
   structure of an NDF PROVENANCE extension. */
   xloc = ndgVot2hds( elem, tloc, status );

/* Read provenance information from the HDS structure. */
   result = ndg1ReadProvenanceExtension( xloc, path, NULL, creator, 0,
                                         status );
/* Free resources. */
   datAnnul( &xloc, status );
   ndg1Antmp( &tloc, status );
   doc = astXmlAnnul( doc );

/* If an error occurred, free the result. */
   if( !astOK ) result = ndg1FreeProvenance( result, 1, status );

/* Return the result */
   return result;
}

static void ndg1ResetIndices( Provenance *provenance, int *status ){
/*
*  Name:
*     ndg1ResetIndices

*  Purpose:
*     Indicate that the index of each Prov structure within the ANCESTORS
*     list needs to be re-calculated.

*  Invocation:
*     void ndg1ResetIndices( Provenance *provenance, int *status )

*  Description:
*     This function stores -1 for the "index" component of each "Rrov"
*     stucture in the supplied Provenance. This causes the next
*     invocation of the ndg1FindAncestorIndex function to re-calculate
*     the index of the Prov.
*
*     This function should be called whener anything is done to the
*     Provenance that may cause the indices of individual ancestors to
*     change (e.g. when an ancestor is added, deleted or moved).

*  Arguments:
*     provenance
*        Pointer to the structure holding provenance information read
*        from an NDF.
*     status
*        The global status.
*/

/* Local Variables: */
   Prov **prov;
   int iprov;
   int nprov;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Loop round all Prov structures in the Provenance array, and set the
   "index" value to -1. */
   prov = provenance->provs;
   nprov = provenance->nprov;
   for( iprov = 0; iprov < nprov; iprov++, prov++ ) (*prov)->index = -1;

}

static void ndg1Rmprv( Provenance *prov, int ianc, int *status ){
/*
*  Name:
*     ndg1Rmprv

*  Purpose:
*     Remove an ancestor from a Provenance structure.

*  Invocation:
*     void ndgRmprv( Provenance *prov, int ianc, int *status )

*  Description:
*     This function removes a given ancestor from the supplied Provenance
*     structure. The direct parents of the removed ancestor are assigned
*     to the direct children of the removed ancestor.

*  Arguments:
*     prov
*        Pointer to the structure holding provenance information read
*        from an NDF.
*     ianc
*        The index of the ancestor to be removed. The supplied value
*        must be at least 1, and must be no more than the number of
*        ancestors in the provenance extension (as returned by NDG_CTPRV).
*        An error is reported otherwise.
*     status
*        The global status.
*/

/* Local Variables: */
   Prov *anc = NULL;
   Prov *child = NULL;
   Prov *parent = NULL;
   int i;
   int ichild;
   int iparent;
   int n;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check the "ianc" value is within the bounds of the ANCESTORS array. */
   if( ianc > 0 && ianc < prov->nprov ) {

/* Get a pointer to the ancestor Prov structure. */
      anc = prov->provs[ ianc ];

/* Loop round all the direct children of the ancestor. */
      n = anc->nchild;
      for( ichild = 0; ichild < n; ichild++ ) {

/* The index into the children array is fixed at zero because the
   following call to ndg1Disown will remove the first child from "anc" on
   each pass. */
         child = anc->children[ 0 ];

/* Break the parent-child link between the ancestor and the current
   child. This reduces the number of children in anc (i.e. anc->nchild)
   by 1. */
         ndg1Disown( anc, child, status );

/* Loop round all the direct parents of the ancestor. */
         for( iparent = 0; iparent < anc->nparent; iparent++ ) {
            parent = anc->parents[ iparent ];

/* Create a parent-child link between the parent and the child, thus
   skipping the ancestor that is to be removed. */
            ndg1ParentChild( parent, child, 1, status );
         }
      }

/* Loop round all the direct parents of the ancestor. */
      n = anc->nparent;
      for( iparent = 0; iparent < n; iparent++ ) {
         parent = anc->parents[ 0 ];

/* Break the parent-child link between the ancestor and the current
   parent. */
         ndg1Disown( parent, anc, status );
      }

/* Now free the resources used by the ancestor. */
      prov->provs[ ianc ] = ndg1FreeProv( anc, status );

/* Shuffle all the remaining Provs down one slot. */
      for( i = ianc + 1; i < prov->nprov; i++ ) {
         prov->provs[ i - 1 ] = prov->provs[ i ];
      }
      prov->provs[ i - 1 ] = NULL;
      ( prov->nprov )--;

/* Report an error if the ianc value is bad. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "IANC", ianc );
      msgSeti( "N", prov->nprov );
      errRep( " ", "Cannot remove provenance ancestor ^IANC: "
              "only ^N ancestors found.", status );
   }

}

static AstKeyMap *ndg1ShowProv( Prov *prov, int depth, AstKeyMap *km,
                                FILE *fd, int *status ){
/*
*  Name:
*     ndg1ShowProv

*  Purpose:
*     Display a Prov structure with all its parents and children.

*  Invocation:
*     AstKeyMap *ndg1ShowProv( Prov *prov, int depth, AstKeyMap *km,
*                              FILE *fd, int *status )

*  Description:
*     This function is a debugging tool that displays a Prov structure.
*
*     The displayed information is a bit confusing at the moment. Could be
*     improved.

*  Arguments:
*     prov
*        Pointer to the Prov structure to display.
*     depth
*        The level of recursive nesting.
*     km
*        A KeyMap containing the addresses of the Prov structurs already
*        displayed, or NULL if no structures have yet been displayed.
*     fd
*        Pointer to a file descriptor to which output is written. It is
*        ignored and a new file is opened if depth is zero.
*     status
*        Inherited status pointer.

*  Returned Value:
*     A pointer to a KeyMap holding the address of all the Prov
*     structures displayed. Should be freed using astAnnul after the top
*     level entry to this function returns.

*/

/* Local Variables: */
   AstKeyMap *result;
   char indent[2000];
   char key[100];
   int i;

/* Initialise */
   result = km;

/* Check status */
   if( *status != SAI__OK ) return result;

/* If this is the top level entry, open an output file. */
   if( depth == 0 || ! fd ) {
      sprintf( key, "prov_%p.log", prov );
      fd = fopen( key, "w" );
   }

/* A keyMap to store the addresses of the Prov structures that have already
   been show. */
   if( !result ) result = astKeyMap( " " );

/* Get a string representation of the address of the supplied Prov
   structure. */
   sprintf( key, "%p", prov );

/* Set up the indentation string. */
   for( i = 0; i < 2*depth; i++ ) indent[ i ] = ' ';
   indent[ 2*depth ] = 0;

/* Display the path */
   fprintf( fd, "%s path: %s\n", indent, prov->path ? prov->path : "" );

/* If the Prov has already been displayed, issue a warning. */
   if( astMapHasKey( result, key ) ) {
      fprintf( fd, "%s >>> This Prov structure has already been displayed\n",
              indent );

/* Otherwise, display the remaining details of this Prov */
   } else {
      fprintf( fd, "%s date: %s\n", indent, prov->date ? prov->date : "" );
      fprintf( fd, "%s creator: %s\n", indent, prov->creator ? prov->creator : "" );
      fprintf( fd, "%s index: %d\n", indent, prov->index );
      fprintf( fd, "%s hidden: %d\n", indent, prov->hidden );

/* Store the Prov address in the KeyMap so that the following recursive
   calls to this function can detect if the same Prov is reached by a
   second path. */
      astMapPut0I( result, key, 1, NULL );

/* Display the child Provs (to avoid infinite loops). */
      for( i = 0; i < prov->nchild; i++ ) {
         fprintf( fd, "%s child %d:\n", indent, i );
         (void) ndg1ShowProv( prov->children[ i ], depth + 1, result, fd,
                              status );
      }

/* Display the parent Provs (to avoid infinite loops). */
      for( i = 0; i < prov->nparent; i++ ) {
         fprintf( fd, "%s parent %d:\n", indent, i );
         (void) ndg1ShowProv( prov->parents[ i ], depth + 1, result, fd,
                              status );
      }

   }

/* If this is the top level entry, close the output file. */
   if( depth == 0 ) fclose( fd );

/* Return a pointer to the KeyMap. */
   return result;
}

static char *ndg1StoreCharData( char *mem, const void *data, size_t len,
                                size_t *memsize, int *status ){
/*
*  Name:
*     ndg1StoreCharData

*  Purpose:
*     Append supplied bytes to the end of a supplied array, extending the
*     array as required.

*  Invocation:
*     char *ndg1StoreCharData( char *mem, const void *data, size_t len,
*                              size_t *memsize, int *status )

*  Description:
*     This function extends a supplied memory area and then appends the
*     supplied bytes to the end of the area.

*  Arguments:
*     mem
*        Pointer to a pre-allocated memory area to which the supplied
*        bytes should be appended.
*     data
*        Pointer to the bytes to be appended to "mem".
*     len
*        The number of bytes to be appended to the end of "mem".
*     memsize
*        Pointer to a location holding the number of bytes in the "mem"
*        area. The supplied value is increment by "len" on exit.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - If "data" is NULL or "len" is zero, the string "NDG_NULL" is
*     appended to the end of "mem"

*  Returned Value:
*     Pointer to the extended memory area containing the original
*     contents of "mem" plus the new data. Returned equal to "mem" if an
*     error occurs.

*/

/* Local Variables: */
   char *result;
   size_t newlen;
   const char *adata;
   size_t alen;

/* Initialise the returned memory pointer. */
   result = mem;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* If some data has been supplied, use it. */
   if( data && len ) {
      adata = data;
      alen = len;

/* If no data has been supplied, store a magic string. */
   } else {
      adata = NULL_DATA;
      alen = NULL_DATA_LEN;
   }

/* Get the new size of the memory area. */
   newlen = *memsize + alen;

/* Extend the memory area to become the required new size. */
   result = astGrow( result, newlen, sizeof( char ) );

/* If the memory was allocated succesfully, copy the new data to the end
   of it, and update the size of the memory area. */
   if( *status == SAI__OK ) {
      memcpy( result + *memsize, adata, alen );
      *memsize = newlen;
   }

/* Return a pointer to the extended memory area. */
   return result;
}

static void ndg1StoreMore( Prov *prov, AstKeyMap *more, int *status ){
/*
*  Name:
*     ndg1StoreMore

*  Purpose:
*     Store supplemental information about an ancestor NDF.

*  Invocation:
*     void ndg1StoreMore( Prov *prov, AstKeyMap *more, int *status )

*  Description:
*     This function removes any supplemental information already stored
*     with the supplied Prov structure. It then stores a deep copy of the
*     supplied KeyMao.

*  Arguments:
*     prov
*        Pointer to a Prov structure describing the ancestor NDF that is
*        to be changed.
*     more
*        Pointer to a KeyMap holding extra information about the NDF. A
*        deep copy is taken.
*     status
*        Pointer to the inherited status variable.

*/

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Annul any existing more KeyMap in the Prov structure. */
   if( prov->more ) prov->more = astAnnul( prov->more );

/* Store a deep copy of any supplied "more" KeyMap. */
   if( more ) prov->more = astCopy( more );

}

static char *ndg1StoreObject( char *mem, AstObject *obj, size_t *memsize,
                              int *status ){
/*
*  Name:
*     ndg1StoreObject

*  Purpose:
*     Append a dump of the supplied AST Object to the end of a supplied
*     array, extending the array as required.

*  Invocation:
*     char *ndg1StoreObject( char *mem, AstObject *obj, size_t *memsize,
*                            int *status )

*  Description:
*     This function extends a supplied memory area and then appends a
*     dump of the supplied AST Object to the end of the area.

*  Arguments:
*     mem
*        Pointer to a pre-allocated memory area to which the object
*        dump should be appended.
*     obj
*        Pointer to the AST Object to be appended to "mem".
*     memsize
*        Pointer to a location holding the number of bytes in the "mem"
*        area. The supplied value is increment by the length of the dump
*        on exit.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - If "obj" is NULL, this function returns without appending
*     anything to the end of "mem".

*  Returned Value:
*     Pointer to the extended memory area containing the original
*     contents of "mem" plus the new data. Returned equal to "mem" if an
*     error occurs.

*/

/* Local Variables: */
   AstChannel *ch;
   ChanData data;
   char *result;

/* Initialise the returned memory pointer. */
   result = mem;

/* Check the inherited status and the supplied Object pointer. */
   if( *status != SAI__OK || !obj ) return result;

/* Create an AST Channel to create the dump. */
   ch = astChannel( NULL, ndg1ChSink, "Comment=0,Full=-1" );

/* Set up the data to be passed to the Channel sink function
   (ndg1ChSink). */
   data.mem = mem;
   data.memsize = *memsize;
   astPutChannelData( ch, &data );

/* Write the object to the Channel (and thus to the supplied memory
   area). */
   astWrite( ch, obj );

/* Update the returned values. */
   result = data.mem;
   *memsize = data.memsize;

/* Annull the CHannel. */
   ch = astAnnul( ch );

/* Return a pointer to the extended memory area. */
   return result;
}

static HDSLoc *ndg1Temp( const char *type, int ndim, const hdsdim *dim, int *status ){
/*
*  Name:
*     ndg1Temp

*  Purpose:
*     Create a temporary HDS object.

*  Invocation:
*     HDSLoc *ndg1Temp( const char *type, int ndim, const hdsdim *dim, int *status )

*  Description:
*     This function creates a temporary HDS object with the specified
*     type and shape. On the first invocation a temporary structure is
*     created to contain such objects. Subsequently, temporary objects
*     are created within this enclosing structure.

*  Arguments:
*     type
*        String holding HDS type of object to be created.
*     ndim
*        Number of object dimensions.
*     dim
*        Pointer to array of object dimensions.
*     status
*        The global status.

*  Notes:
*     -  This routine is a work-around to avoid the problems associated
*     with calling datTemp if the objects created must subsequently be
*     erased.

*  Returned Value:
*     A pointer to a locator for a new temporary HDS object. It should be
*     erased using ndg1Antmp when no longer needed.

*/

/* Local variables: */
   HDSLoc *result = NULL;        /* Returned locator for temporary object */
   char name[ DAT__SZNAM + 1 ];  /* Temporary object name */
   hdsdim dummy[ 1 ];            /* Dummy dimensions array */

/* Static variables! But the NDF library is not thread safe, so we cannot
   be using this module in a thread-safe environment, so there is no harm
   in using static variables. */
   static int count = 0;         /* Count of objects created */
   static HDSLoc *tmploc = NULL; /* Locator to enclosing structure */

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Increment the count of temporary objects created. */
   count++;

/* Before creating the first object, create a temporary enclosing
   structure and tune HDS to expect a large number of components in it. */
   if( count == 1 ) {
      tmploc = NULL;
      datTemp( "NDG_TEMP", 0, dummy, &tmploc, status );
      hdsTune( "NCOMP", 20, status );
   }

/* Form a unique name for the temporary object. */
   if( *status == SAI__OK ) {
      sprintf( name, "NDG_%d", count );

/*  Create an object inside the enclosing structure and obtain a locator
    to it. */
      datNew( tmploc, name, type, ndim, dim, status );
      datFind( tmploc, name, &result, status );
   }

/* Return the resulting locator. */
   return result;
}

static int ndg1TheSame( Prov *prov1, Prov *prov2, int *status ) {
/*
*  Name:
*     ndg1TheSame

*  Purpose:
*     Checks if two Prov structures are for the same NDF.

*  Invocation:
*     int ndg1TheSame( Prov *prov1, Prov *prov2, int *status )

*  Description:
*     This function returns non-zero if the two supplied Prov structures
*     describe the same NDF, potentially used in two different contexts
*     (i.e. they may have different numbers of children).

*  Arguments:
*     prov1
*        The first Prov structure.
*     prov2
*        The second Prov structure.
*     status
*        Pointer to the inherited status variable.

* Returned Value:
*   Non-zero if the two Prov structures describe the same NDF, and zero
*   otherwise.

*/

/* Local Variables: */
   int result;
   int i;

/* Initialise. */
   result = 0;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* If the pointers are the same, they describe the same NDF. */
   if( prov1 == prov2 ) {
      result = 1;
      static_provid1 = -1;
      static_provid2 = -1;

/* If the ProvId values differ, the NDFs are definitely different. The
   ProvId is a hash code describing the content of the Prov and all its
   ancestors. Since it is possible for the same NDF name to be re-used to
   hold different data, just comparing the path is not good enough. Even
   including the command and date is not good enough since the same
   command may be used more than once to produce NDFs with the same name
   within a very short time. For this reason we use the ProvId which
   incorporates not only the name, command and date of this Prov, but
   also all its ancestor Provs. However, hash code clashes are possible
   so equality of hash code does not guarantee equality of NDF. */
   } else {
      static_provid1 = ndg1GetProvId( prov1, status );
      static_provid2 = ndg1GetProvId( prov2, status );
      if( static_provid1 != static_provid2 ) {
         result = 0;

/* If the ProvId values are equal we need to do more checks to see if the
   NDFs are the same. */
      } else {

/* Compare the path, date, creator, and no. of parents for both structures.
   If any of these values differ, the NDFs are different. Note, the
   number of children may differ as the same NDF may be used to create
   different groups of children without it changing its identity in any
   sense. */
         result = CMP_STRING( prov1->path, prov2->path ) &&
                  CMP_STRING( prov1->date, prov2->date ) &&
                  CMP_STRING( prov1->creator, prov2->creator ) &&
                  ( prov1->nparent == prov2->nparent );

/* If the above components match, check that all parents are the same, if
   there are any parents. */
         if( result && prov1->nparent > 0 ) {

/* If there is only one parent, just compare them. */
            if( prov1->nparent == 1 ) {
               result = ndg1TheSame( prov1->parents[ 0 ], prov2->parents[ 0 ],
                                     status );

/* If there is more than one parent, things are harder since we need to
   ensure we are comnparing corresponding parents (the order of the
   parents within the parents array is arbitrary). */
            } else {
               int j;

/* Take a copy of the parent pointers from the second Prov structure. */
               Prov **par2 = astStore( NULL, prov2->parents,
                                       prov2->nparent*sizeof(*prov2->parents) );
               if( *status == SAI__OK ) {

/* For each parenty in prov1... */
                  for( i = 0; i < prov1->nparent; i++ ) {

/* Assume no matching parent in prov2 exists. */
                     result = 0;

/* Check each parent in prov2 to see if it matches the current parent
   from prov1. Ignore prov2 parents that have already matched earlier
   prov1 parents. */
                     for( j = 0; j < prov2->nparent; j++ ) {
                        if( par2[ j ] ) {

/* Compare the two parents. If they match, flag a matching parents of
   parents has been found, nullify the prov2 parent pointer to prevent
    it from being used in further comparisons, and leave the loop. */
                           if( !ndg1TheSame( prov1->parents[ i ], par2[ j ],
                                             status ) ) {
                              result = 1;
                              par2[ j ] = NULL;
                              break;
                           }
                        }
                     }

/* If no parent in prov2 matched the current parent from prov1, the
   supplied Provs are not the same, so leave the "i" loop. */
                     if( ! result ) break;
                  }
               }

/* Free resources. */
               par2 = astFree( par2 );
            }
         }
      }
   }

/* Return the result. */
   return result;
}

static void ndg1WriteProvenanceExtension( Provenance *provenance,
                                          HDSLoc *xloc, int *status ){
/*
*  Name:
*     ndg1WriteProvenanceExtension

*  Purpose:
*     Create a new HDS structure from a Provenance structure.

*  Invocation:
*     void ndg1WriteProvenanceExtension( Provenance *provenance,
*                                        HDSLoc *xloc, int *status )

*  Description:
*     This function stores the supplied provenance information in the
*     supplied HDS object. The components added to the HDS structure are
*     those required for an NDF PROVENANCE extension.

*  Arguments:
*     provenance
*        The Provenance structure.
*     xloc
*        The locator for the HDS structure into which the provenance
*        information is to be written.
*     status
*        Pointer to the inherited status variable.

*/


/* Local Variables: */
   HDSLoc *dloc = NULL;
   Prov *prov = NULL;
   hdsdim dim;
   int *ipdata = NULL;
   int i;
   int ival;
   size_t pdlen = 0;
   char *pdata = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* The first int in the buffer is a version number that identifies the
   way in which the provenance information is encoded. */
   ival = PDATA_VERSION;
   pdata = ndg1StoreCharData( pdata, &ival, sizeof( ival ), &pdlen, status );

/* The second int in the buffer is the number of Prov structures (I.e.
   the number of ancestors plus the main NDF). */
   ival = provenance->nprov;
   pdata = ndg1StoreCharData( pdata, &ival, sizeof( ival ), &pdlen, status );

/* Add a description of the main NDF to the above buffer. */
   pdata = ndg1EncodeProvData( pdata, 1, provenance->main, provenance, &pdlen,
                               status );

/* Loop round all the Prov structures. */
   for( i = 0; i < provenance->nprov; i++ ) {
      prov = provenance->provs[ i ];

/* Skip the main NDF (which has already been added to the buffer. */
      if( prov != provenance->main ) {

/* Add the Prov structure to the buffer. */
         pdata = ndg1EncodeProvData( pdata, 0, prov, provenance, &pdlen,
                                     status );
      }
   }

/* Add an end flag to the buffer. */
   ival = END_FLAG;
   pdata = ndg1StoreCharData( pdata, &ival, sizeof( ival ), &pdlen, status );

/* Get the number of integers needed to store the data (round up to
   ensure nothing is lost). */
   dim = pdlen/sizeof( int );
   if( dim*sizeof(int ) < pdlen ) dim++;

/* Store a copy of the buffer in a new HDS component within the supplied
   structure. Fill any extra space following the buffer with pad values. */
   datNew( xloc, DATA_NAME, "_INTEGER", 1, &dim, status );
   datFind( xloc, DATA_NAME, &dloc, status );
   datMapI( dloc, "WRITE", 1, &dim, &ipdata, status );
   if( *status == SAI__OK ) {
      memcpy( ipdata, pdata, pdlen );
      size_t npad = dim*sizeof(int) - pdlen;
      if( npad > 0 ) memset( (char *) ipdata + pdlen, PAD_FLAG, npad );
   }
   datAnnul( &dloc, status );

/* Free the buffer. */
   pdata = astFree( pdata );
}

static void ndg1WriteProvenanceNDF( Provenance *provenance, int indf,
                                    int whdef, int *status ){
/*
*  Name:
*     ndg1WriteProvenanceNDF

*  Purpose:
*     Create a new NDF PROVENANCE extension from a Provenance structure.

*  Invocation:
*     void ndg1WriteProvenanceNDF( Provenance *provenance, int indf,
*                                  int whdef, int *status )

*  Description:
*     This function erases any existing PROVENANCE extension within the
*     supplied NDF, and creates a new one holding the information in the
*     supplied Provenance structure.

*  Arguments:
*     provenance
*        The Provenance structure.
*     indf
*        The NDF identifier.
*     whdef
*        The correct recording of history information within the
*        PROVENANCE extension requires that the current history record
*        within the supplied NDF at the time this function is called,
*        describes the creation of the NDF. Very often, an application
*        will not itself add any history to the NDF, but will instead
*        rely on the automatic recording of default history provided by
*        the NDF library. Normally, default history is recorded when the
*        NDF is released from the NDF system (e.g. using ndfAnnul or
*        ndfEnd). So if this function is called prior to the release of
*        the NDF (which it normally will be), then the default history
*        information will not yet have been recorded, resulting in
*        incorrect information being stored in the PROVENANCE extension.
*        For this reason, the "whdef" argument is supplied. If it is set
*        to a non-zero value, a check is made to see if default history
*        has already been stored in the NDF. If not, default history is
*        stored in the NDF before going on to create the PROVENANCE
*        extension. Applications that do not use the default history
*        recording mechanism, but instead store their own history
*        information, should supply a zero value for "whdef" and should
*        also ensure that history information has been stored in the NDF
*        before calling this function.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   HDSLoc *xloc = NULL;
   char *path;
   char path_buf[ PATH_LEN + 1 ];
   int irec;
   int path_len;
   int there;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* See if the supplied NDF has a provenance extension. If it has, delete
   it. */
   ndfXstat( indf, EXT_NAME, &there, status );
   if( there ) ndfXdel( indf, EXT_NAME, status );

/* If a Provenance structure was supplied, create a new PROVENANCE
   extension. */
   if( provenance ) {
      ndfXnew( indf, EXT_NAME, EXT_TYPE, 0, NULL, &xloc, status );

/* If the provenance for the main NDF does not have a hash code for the most
   recent history record at the time the structure was first stored in the
   NDF, get one now. */
      if( provenance->main->hhash == 0 ) {

/* Check the NDF has a history component. */
         ndfState( indf, "History", &there, status );
         if( there ) {

/* If the default history may not yet have been written, ensure it is
   written by calling ndfHdef. */
            if( whdef ) ndfHdef( indf, "", status );

/* Ensure descriptions of any registered GRP groups have been appended to
   the current history record (an application registers a group by
   calling NDG_ADDGH). */
            ndgHwrgh( indf, status );

/* Get the index of the current history record, and if there is a current
   record, get its hash code. */
            ndfHnrec( indf, &irec, status );
            if( irec ) ndg1ReadHistRec( NULL, indf, irec,
                                        &(provenance->main->hhash), status );
         }
      }

/* If the provenance for the main NDF does not have a path, record it
   temporarily now. */
      path = provenance->main->path;
      if( !path ) {
         ndfMsg( "NDF", indf );
         msgLoad( " ", "^NDF", path_buf, PATH_LEN, &path_len, status );
         provenance->main->path = astStore( NULL, path_buf,
                                            strlen( path_buf ) + 1 );
      }

/* Add the required components to the extension. */
      ndg1WriteProvenanceExtension( provenance, xloc, status );


/* If the provenance for the main NDF originally had no path, remove the
   temporary path added earlier. */
      if( !path ) provenance->main->path = astFree( provenance->main->path );

/* Free resources */
      datAnnul( &xloc, status );
   }
}

static const char *ndg1WriteProvenanceXml( Provenance *provenance, int *status ){
/*
*  Name:
*     ndg1WriteProvenanceXml

*  Purpose:
*     Create an XML representation of a Provenance structure.

*  Invocation:
*     const char *ndg1WriteProvenanceXml( Provenance *provenance, int *status )

*  Description:
*     This function creates an XML representation of the supplied
*     provenance information, suitable for inclusion in a VOTABLE.

*  Arguments:
*     provenance
*        The Provenance structure.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dyamically allocated string holding the XML text, or
*     NULL if an error occurs. The string should be freed using astFree
*     when it is no longer needed. The text will contain a single top
*     level element with the following opening tag:
*
*        "<GROUP name="PROVENANCE" utype="hds_type:PROVENANCE">"
*
*     The HDS structure of an NDF PROVENANCE extension is replicated
*     using PARAM elements to hold primitive values and GROUP elements to
*     hold structures. The "utype" attributes are used to hold the
*     corresponding HDS data types.

*/

/* Local Variables: */
   AstXmlElement *elem = NULL;
   HDSLoc *tloc = NULL;
   HDSLoc *xloc = NULL;
   const char *result = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* Create a temporary HDS structure holding the provenance information. This
   structure is the same as would be stored in an NDF PROVENANCE extension. */
   tloc = ndg1Temp( "TEMP", 0, NULL, status );
   datNew( tloc, EXT_NAME, EXT_TYPE,  0, NULL, status );
   datFind( tloc, EXT_NAME, &xloc, status );
   ndg1WriteProvenanceExtension( provenance, xloc, status );

/* Convert the HDS structure into an AstXmlElement. */
   elem = ndgHds2vot( xloc, NULL, status );

/* Format the AstXmlElement as a text string without white space padding
   or newlines. */
   result = astXmlFormat( elem );

/* Free resources. */
   elem = astXmlAnnul( elem );
   datAnnul( &xloc, status );
   ndg1Antmp( &tloc, status );

/* If an error has occurred, free the returned string. */
   if( *status != SAI__OK ) result = astFree( (void *) result );

/* Return the result. */
   return result;

}

static char ndg1XmlSource( void *data, int *status ){
/*
*  Name:
*     ndg1XmlSource

*  Purpose:
*     Read the next character of text from an XML string.

*  Invocation:
*     char ndg1XmlSource( void *data, int *status )

*  Description:
*     This function is called by astXmlReadDocument to read a character
*     from the XML text string.

*  Arguments:
*     data
*        Pointer to the data structure passed to astXmlReadDocument. The
*        data structure is a pointer to the next character to be returned.
*        The pointer is incremented by this function.
*     status
*        Inherited status pointer.

*  Returned Value:
*     The next character, or zero if the end of the source has been reached.

*/
   if( *status != SAI__OK ) return 0;
   return *( (* (char **) data )++ );
}


static void ndg1DumpInfo( Prov *prov1, Prov *prov2, int *status ){

   AstKeyMap *pkm;
   FILE *fd=NULL;
   Prov **prov;
   char logfile[200];
   char pf[50];
   const char *home;
   int iprov, jprov, j;

   if( *status != SAI__OK ) return;

   home = getenv( "HOME" );
   if( !home ) {
      *status = SAI__ERROR;
      errRep( " ", "WARNING: ndg1DumpInfo failed to get HOME directory.", status );
      errFlush( status );
      return;
   }

   sprintf( logfile, "%s/ndg.log", home );
   fd = fopen( logfile, "w" );
   if( !fd ) {
      *status = SAI__ERROR;
      msgSetc( "F", logfile );
      errRep( " ", "WARNING: ndg1DumpInfo failed to open ndg log file '^F'.", status );
      errFlush( status );
      return;
   }

   fprintf( fd, "Error occurred whilst purging duplicate ancestors "
                "in %s\n", static_provenance->main->path );

   prov = static_provenance->provs;
   pkm = astKeyMap( " " );
   for( iprov = 0; iprov < static_provenance->nprov; iprov++,prov++ ) {
      sprintf( pf, "%p", *prov );
      astMapPut0I( pkm, pf, iprov, NULL );
   }

   fprintf( fd, "PROV1: provid=%d path=%s index=", static_provid1,
            prov1->path );

   sprintf( pf, "%p", prov1 );
   if( !astMapGet0I( pkm, pf, &iprov ) ) {
      fprintf( fd, "unknown\n" );
   } else {
      fprintf( fd, "%d\n", iprov );
   }

   fprintf( fd, "PROV2: provid=%d path=%s index=", static_provid2,
            prov2->path );

   sprintf( pf, "%p", prov2 );
   if( !astMapGet0I( pkm, pf, &iprov ) ) {
      fprintf( fd, "unknown\n" );
   } else {
      fprintf( fd, "%d\n", iprov );
   }

   if( static_badtype == 0 ) {
      fprintf( fd, "PROV2 is a child of %s (provid=%d) but PROV1 is not.\n",
               static_badpath, static_badprovid );
   } else {
      fprintf( fd, "PROV1 is a child of %s (provid=%d) but PROV2 is not.\n",
               static_badpath, static_badprovid );
   }

   fprintf( fd, "\n\n" );


   prov = static_provenance->provs;
   for( iprov = 0; iprov < static_provenance->nprov; iprov++,prov++ ) {
      fprintf( fd, "Index %d:\n", iprov );
      fprintf( fd, "   path=%s\n", (*prov)->path );
      fprintf( fd, "   provid=%d\n", (*prov)->provid );
      fprintf( fd, "   date=%s\n", (*prov)->date );
      fprintf( fd, "   creator=%s\n", (*prov)->creator );
      fprintf( fd, "   index=%d\n", (*prov)->index );
      fprintf( fd, "   hhash=%d\n", (*prov)->hhash );
      fprintf( fd, "   more=%p\n", (*prov)->more );
      fprintf( fd, "   nparent=%d\n", (*prov)->nparent );
      fprintf( fd, "   nchild=%d\n", (*prov)->nchild );
      fprintf( fd, "   hist_recs=%p\n", (*prov)->hist_recs );
      fprintf( fd, "   nhrec=%d\n", (*prov)->nhrec );
      fprintf( fd, "   hidden=%d\n", (*prov)->hidden );

      fprintf( fd, "   Parents: " );
      for( j = 0; j < (*prov)->nparent; j++ ) {
         sprintf( pf, "%p", (*prov)->parents[ j ] );
         if( !astMapGet0I( pkm, pf, &jprov ) ) {
            fprintf( fd, "?? " );
         } else {
            fprintf( fd, "%d ", jprov );
         }
      }
      fprintf( fd, "\n");

      fprintf( fd, "   Children: " );
      for( j = 0; j < (*prov)->nchild; j++ ) {
         sprintf( pf, "%p", (*prov)->children[ j ] );
         if( !astMapGet0I( pkm, pf, &jprov ) ) {
            fprintf( fd, "?? " );
         } else {
            fprintf( fd, "%d ", jprov );
         }
      }
      fprintf( fd, "\n");

   }

   pkm = astAnnul( pkm );
   if( fclose( fd ) ) perror( "ndg1DumpInfo");

}


#ifdef NDG_DEBUG

/* Append a Prov pointer to the end of a list of issued Prov pointers. */
static void Issue( Prov *prov ){
   prov->id = nextid++;
   issued = astGrow( issued, nextid, sizeof( *issued ) );
   issued[ prov->id ] = prov;
}

/* Remove a Prov pointer from a list of issued Prov pointers. */
static void Deissue( Prov *prov ){
   prov->id = nextid++;
   issued = astGrow( issued, nextid, sizeof( *issued ) );
   issued[ prov->id ] = prov;
}

/* List any Prov structurs that are still in use. */
F77_SUBROUTINE(ndg_listprov)( INTEGER(status) ){
   int i, first;
   first = 1;
   for( i = 0; i < nextid; i++ ) {
      if( issued[ i ] ) {
         if( first ) {
            printf( "Following provenance identifiers are still active: ");
            first = 0;
         }
         printf(" %d (%s)", i, issued[i]->path );
      }
   }
   if( first ) printf( "All provenance identifiers have been freed.");
   printf("\n");
}

#endif




