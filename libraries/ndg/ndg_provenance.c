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

/* HDS types for components used in the PROVENANCE extension of an NDF */
#define TEMP_TYPE "STARLINK_PROV"
#define EXT_TYPE "PROVENANCE"
#define ANCESTORS_TYPE "PROV"
#define MORE_TYPE "MORE"

/* Max length allowed for each text component in the PROVENANCE extension
   of an NDF */
#define DATE_LEN 27
#define CREATOR_LEN 50
#define PATH_LEN 256

/* Include files. */
/* -------------- */
/* Starlink packages. */
#include "star/hds.h"
#include "star/hds_fortran.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
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


/* Type Definitions. */
/* ----------------- */
/* A structure that stores the provenance information for a single
   ancestor NDF. */
typedef struct Prov {
   char *path;                 /* String holding the NDF path as returned
                                  by ndfMsg */
   char *date;                 /* String holding the formated UTC date & time 
                                  at which the NDF's provenance was recorded */
   char *creator;              /* String describing the software that created 
                                  the NDF */
   HDSLoc *more;               /* HDS object holding extra information about 
                                  the NDF */
   struct Prov **parents;      /* Array of pointers to the Prov structures for 
                                  the direct parents of the NDF */
   struct Prov **children;     /* Array of pointers to the Prov structures for 
                                  the direct children of the NDF */
   int nparent;                /* The length of the parents array */
   int nchild;                 /* The length of the children array */
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
static HDSLoc *ndg1GtAnc( HDSLoc *, size_t *, int * );
static HDSLoc *ndg1TCopy( HDSLoc *, const char *, int * );
static Prov *ndg1FreeProv( Prov *, int * );
static Prov *ndg1MakeProv( const char *, const char *, const char *, HDSLoc *, Provenance *, int * );
static Provenance *ndg1FreeProvenance( Provenance *, int, int * );
static Provenance *ndg1MakeProvenance( Prov *, int * );
static Provenance *ndg1ReadProvenanceExtension( int, HDSLoc *, const char *, int, int * );
static char *ndg1GetTextComp( HDSLoc *, const char *, char *, size_t, int * );
static const char *ndg1Date( int * );
static int ndg1FindAncestorIndex( Prov *, Provenance *, int * );
static int ndg1TheSame( Prov *, Prov *, int * );
static void ndg1A2h0c( const char *, AstKeyMap *, HDSLoc *, int * );
static void ndg1A2h1i( const char *, AstKeyMap *, HDSLoc *, int * );
static void ndg1CopyComps( HDSLoc *, HDSLoc *, int * );
static void ndg1Disown( Prov *, Prov *, int * );
static void ndg1H2a0c( const char *, HDSLoc *, AstKeyMap *, int * );
static void ndg1H2a0h( const char *, HDSLoc *, AstKeyMap *, int * );
static void ndg1H2a1i( const char *, HDSLoc *, AstKeyMap *, int * );
static void ndg1ParentChildIndex( Provenance *, int, int, int * );
static void ndg1ParentChild( Prov *, Prov *, int * );
static void ndg1PurgeProvenance( Provenance *, int * );
static void ndg1WriteProvenanceExtension( Provenance *, int, int * );

/* Public functions. */
/* ================= */

F77_SUBROUTINE(ndg_ctprv)( INTEGER(indf), INTEGER(nanc), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_CTPRV

*  Purpose:
*     Count the number of ancestors used in the creation of an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_CTPRV( INDF, NANC, STATUS )

*  Description:
*     This routine returns the number of ancestors described in the 
*     "PROVENANCE" extension of the supplied INDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     NANC = INTEGER (Returned)
*        The number of ancestor NDFs stored in the provenance information
*        of INDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(nanc)
   GENPTR_INTEGER(status)

/* Initialise */
   *nanc = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Call the C function to do the work. */
   ndgCtprv( *indf, nanc, status );

}

F77_SUBROUTINE(ndg_fmprv)( INTEGER(indf), LOGICAL(base), INTEGER(fkeymap), 
                           INTEGER(status) ){
/*
*+
*  Name:
*     NDG_FMPRV

*  Purpose:
*     Format the provenance information from an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_FMPRV( INDF, BASE, KEYMAP, STATUS )

*  Description:
*     This routine returns an AST KeyMap holding a set of text strings
*     containing information taken from the "PROVENANCE" extension in INDF.
*
*     The returned KeyMap has an entry with key "0" that describes the 
*     supplied NDF. It also has an entry describing each ancestor NDF.
*     These entries have keys "1", "2", "3", etc, up to the number of
*     ancestors in the NDF. 
*
*     Each of these entries contains a pointer to another AST KeyMap
*     which may contain any subset of the following entries (all of which 
*     are strings):
*
*     "ID" - the integer index within the ancestors array (zero for the
*            main NDF).
*
*     "PATH" - The full path or base name for the NDF (see argument "BASE").
*
*     "DATE" - The date of creation of the NDF.
*
*     "CREATOR" - The software item that created the NDF.
*
*     "PARENTS" - A comma separated list of indicies into the ancestors
*                 array that identifies the direct parents of the NDF.
*
*     "MORE" - A summary of the contents of the MORE structure associated
*              with the NDF.
*
*     A missing key implies that the corresponding item of information is
*     not available.
*
*     Finally, the returned KeyMap has an entry with key "MXLEN" that is
*     again a pointer to another KeyMap with the same entries listed above.
*     However, this time the entries are integers, not strings, and holds 
*     the maximum field width used to format the strings. Also, all
*     entries are guaranteed to be present in the keymap (but may hold
*     zero if none of the ancestors contained a particular item of 
*     information).

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     BASE = LOGICAL (Given)
*        If .TRUE., then the PATH field in the returned KeyMap holds the 
*        base name of each NDF rather than the full path.
*     KEYMAP = INTEGER (Returned)
*        A pointer to the returned AST KeyMap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_LOGICAL(base)
   GENPTR_INTEGER(fkeymap)
   GENPTR_INTEGER(status)

   AstKeyMap *keymap = NULL;

/* Check the inherited status. */
   if( *status == SAI__OK ) {

/* Call the C function to do the work. */
      ndgFmprv( *indf, F77_ISTRUE( *base ), &keymap, status );
   }

/* Export the AST pointer. */
   *fkeymap = astP2I( keymap );
}

F77_SUBROUTINE(ndg_gtprv)( INTEGER(indf), INTEGER(ianc), CHARACTER(fprov),
                           INTEGER(status) TRAIL(fprov) ){
/*
*+
*  Name:
*     NDG_GTPRV

*  Purpose:
*     Get provenance information from an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_GTPRV( INDF, IANC, PROV, STATUS )

*  Description:
*     This routine returns information from the "PROVENANCE" extension
*     in INDF, describing the ancestor NDF with a given index.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF for which information should be
*        returned. A value of zero will result in information about the NDF 
*        specified by INDF being returned. Otherwise, the IANC value
*        is used as an index into the ANCESTORS array in the PROVENANCE 
*        extension. No error is reported if IANC is too large (i.e.
*        larger than the value returned by NDG_CTPRV), but DAT__NOLOC 
*        will be returned for PROV. 
*     PROV = CHARACTER * (DAT__SZLOC) (Returned)
*        A locator for a temporary HDS object containing the following 
*        components:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at 
*          which the provenance information for the ancestor NDF was 
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": Any extra information stored with the ancestor.
*        - "PARENTS": A 1D vector of integers that are the indices of the
*          immediate parents of the ancestor.
*
*        If the specified ancestor does not have any of these items of
*        information, then the corresponding component will not be
*        present in the returned HDS object. For instance, if the
*        ancestor has no immediate parent NDFs, then the "PARENTS" 
*        component will not be present in the returned HDS object. The
*        returned locator should be annulled using DAT_ANNUL when no
*        longer needed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(ianc)
   GENPTR_CHARACTER(fprov) 
   GENPTR_INTEGER(status)

/* Local variables: */
   HDSLoc *prov = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Call the C function to do the work. */
   ndgGtprv( *indf, *ianc, &prov, status );

/* Export the locator string. */
   datExportFloc( &prov, 0, fprov_length, fprov, status );
}

F77_SUBROUTINE(ndg_mdprv)( INTEGER(indf), INTEGER(ianc), CHARACTER(fprov),
                           INTEGER(status) TRAIL(fprov) ){
/*
*+
*  Name:
*     NDG_MDPRV

*  Purpose:
*     Modify provenance information in an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_MDPRV( INDF, IANC, PROV, STATUS )

*  Description:
*     This routine modifies the information stored for a given ancestor 
*     in the "PROVENANCE" extension of an NDF. The new values to store 
*     are supplied in an HDS structure such as is returned by NDG_GTPRV.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF for which information should be
*        modified. A value of zero will result in information about the NDF 
*        specified by INDF being modified. Otherwise, the IANC value
*        is used as an index into the ANCESTORS array in the PROVENANCE 
*        extension. An error is reported if IANC is too large.
*     PROV = CHARACTER * (DAT__SZLOC) (Returned)
*        A locator for an HDS object containing the values to store. It
*        should have at least the following components:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at 
*          which the provenance information for the ancestor NDF was 
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": Any extra information stored with the ancestor.
*
*        If the "DATE", "CREATOR" or "MORE" components are missing then 
*        the corresponding item of information will be deleted from the 
*        provenance extension. An error is reported if "PATH" is missing.
*        Note, the PARENTS list stored with the specified ancestor cannot 
*        be modified (any "PARENTS" component in the supplied HDS structure 
*        will be ignored).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(ianc)
   GENPTR_CHARACTER(fprov) 
   GENPTR_INTEGER(status)

/* Local variables: */
   HDSLoc *prov = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Import the locator string. */
   if( strncmp( DAT__NOLOC, fprov, fprov_length) ){
      datImportFloc( fprov, fprov_length, &prov, status );
   }

/* Call the C function to do the work. */
   ndgMdprv( *indf, *ianc, prov, status );

}

F77_SUBROUTINE(ndg_ptprv)( INTEGER(indf1), INTEGER(indf2), CHARACTER(fmore),
                           LOGICAL(isroot), CHARACTER(creatr), INTEGER(status)
                           TRAIL(fmore) TRAIL(creatr) ){
/*
*+
*  Name:
*     NDG_PTPRV

*  Purpose:
*     Add provenance information to an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_PTPRV( INDF1, INDF2, MORE, ISROOT, CREATR, STATUS )

*  Description:
*     This routine stores information in the "PROVENANCE" extension of
*     INDF1 indicating that INDF2 was used in the creation of INDF1. 
*     The provenance information is stored in an NDF extension call 
*     "PROVENANCE".

*  Arguments:
*     INDF1 = INTEGER (Given)
*        An identifier for a newly created NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for an NDF that was used in the creation of INDF1.
*     MORE = CHARACTER * (DAT__SZLOC) (Given)
*        A locator for an HDS structure containing arbitrary additional 
*        information about INDF2, and how INDF2 was used in the creation 
*        of INDF1. This information is stored in the provenance extension 
*        of INDF1.
*     ISROOT = LOGICAL (Given)
*        If TRUE, then INDF2 will be treated as a root NDF. That is,
*        any provenance information in INDF2 is ignored. If FALSE, then
*        any provenance information in INDF2 is copied into INDF1. INDF2
*        is then only a root NDF if it contains no provenance information.
*     CREATR = CHARACTER * ( * ) (Given)
*        A text identifier for the software that created INDF1 (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The PROVENANCE extension in an NDF contains four components:
*     "PARENTS", "ANCESTORS", "CREATOR" and "DATE". The DATE component is 
*     a character string holding the date and time at which the information 
*     in the provenance extension was last modified. The date is UTC
*     formatted by PSX_ASCTIME. The ANCESTORS component is a 1D array
*     of "PROV" structures (described below). Each element describes a
*     single NDF that was used in the creation of the main NDF, either
*     directly or indirectly. The PARENTS component is a 1D integer 
*     array holding the indices within the ANCESTORS array of the NDFs
*     that are the direct parents of the main NDF. The CREATOR component
*     holds an arbitrary identifier for the software that created the
*     main NDF.
*     - Each PROV structure describes a single NDF that was used in the
*     creation of the main NDF, and contains up to four components; "PARENTS", 
*     "DATE", "PATH", "CREATOR" and "MORE". If present, the PARENTS component 
*     is a 1D integer array holding the the indices within the ANCESTORS array 
*     of the direct parents of the ancestor NDF. If PARENTS is not present, 
*     the ancestor NDF is a "root" NDF (that is, it has no known parents).
*     If present, the DATE component is a string holding the formatted UTC 
*     date at which the provenance information for the ancestor NDF was 
*     determined. If this date is not known, the DATE component will not
*     be present (this will be the case, for instance, for all root NDFs).
*     The PATH component will always be present, and is a string holding
*     the full path to the ancestor NDF. This includes any HDS path
*     within the container file, but will not include any NDF or HDS section 
*     specifier. Neither will it include the trailing ".sdf" suffix. If
*     present, the MORE component is an arbitrary HDS structure in which 
*     any extra information about the ancestor NDF can be stored. The
*     CREATOR component holds an arbitrary identifier for the software that 
*     created the ancestor NDF.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf1)
   GENPTR_INTEGER(indf2)
   GENPTR_CHARACTER(fmore) 
   GENPTR_LOGICAL(isroot)
   GENPTR_CHARACTER(creatr) 
   GENPTR_INTEGER(status)

/* Local variables: */
   char *creator = NULL;
   HDSLoc *more = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Import the creator string */
   creator = cnfCreim( creatr, creatr_length );

/* Import the locator string. */
   if( strncmp( DAT__NOLOC, fmore, fmore_length) ){
      datImportFloc( fmore, fmore_length, &more, status );
   }

/* Call the C function to do the work. */
   ndgPtprv( *indf1, *indf2, more, F77_ISTRUE( *isroot ), creator, status );

/* Free the memory used to hold local copies of the supplied strings */
   cnfFree( creator );
}


F77_SUBROUTINE(ndg_rmprv)( INTEGER(indf), INTEGER(ianc), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_RMPRV

*  Purpose:
*     Remove provenance information from an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_RMPRV( INDF, IANC, STATUS )

*  Description:
*     This routine removes a given ancestor from the "PROVENANCE" 
*     extension in INDF. The direct parents of the removed ancestor are
*     assigned to the direct children of the removed ancestor.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     IANC = INTEGER (Given)
*        The index of the ancestor NDF to be removed. The supplied value
*        must be at least 1, and must be no more than the number of
*        ancestors in the provenance extension (as returned by NDG_CTPRV).
*        An error is reported otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-FEB-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(ianc)
   GENPTR_INTEGER(status)

/* Call the C function to do the work. */
   ndgRmprv( *indf, *ianc, status );

}

F77_SUBROUTINE(ndg_rtprv)( INTEGER(indf), INTEGER(roots), INTEGER(status) ){
/*
*+
*  Name:
*     NDG_RTPRV

*  Purpose:
*     Identify the root ancestors of an NDF.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL NDG_RTPRV( INDF, ROOTS, STATUS )

*  Description:
*     This routine uses the PROVENANCE extension of the supplied NDF to 
*     obtain and return information identifying the root ancestors of
*     the supplied NDF. An ancestor is a root ancestor if it does not itself
*     have any ancestors.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF containing the provenance information.
*     ROOTS = INTEGER (Returned)
*        A pointer to a new AST KeyMap. This KeyMap will contain an entry for 
*        each root ancestor. The key associated with each entry is the path 
*        to the NDF and the value of the entry is an integer that gives the 
*        position of the root ancestor within the list of all ancestors. This 
*        integer value can be supplied to ndgGtprv in order to get further 
*        information about the root ancestor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(indf)
   GENPTR_INTEGER(roots)
   GENPTR_INTEGER(status)

   AstKeyMap *keymap = NULL;

/* Check the inherited status. */
   if( *status == SAI__OK ) {

/* Call the C function to do the work. */
      ndgRtprv( *indf, &keymap, status );
   }

/* Export the AST pointer. */
   *roots = astP2I( keymap );
}

void ndgCtprv( int indf, int *nanc, int *status ){
/*
*+
*  Name:
*     ndgCtprv

*  Purpose:
*     Count the number of ancestors used in the creation of an NDF.

*  Invocation:
*     void ndgCtprv( int indf, int *nanc, int *status )

*  Description:
*     This routine returns the number of ancestors described in the 
*     "PROVENANCE" extension of the supplied INDF.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     nanc
*        Pointer to an int in which to returned the number of ancestor NDFs 
*        stored in the provenance information of INDF.
*     status 
*        The global status.
*-
*/

/* Local variables: */
   HDSLoc *aloc = NULL;
   HDSLoc *xloc = NULL;
   int *old_status;
   int there;
   size_t size;

/* Initialise. */
   *nanc = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* See if the supplied NDF has a provenance extension. If it has, get a 
   HDS locator to it. */
   ndfXstat( indf, EXT_NAME, &there, status ); 
   if( there ) {
      ndfXloc( indf, EXT_NAME, "Read", &xloc, status ); 

/* Get the length of the ANCESTORS array, then annul the locator for the
   ANCESTORS array. */
      aloc = ndg1GtAnc( xloc, &size, status );
      *nanc = (int) size;
      if( aloc ) datAnnul( &aloc, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgFmprv( int indf, int base, AstKeyMap **keymap, int *status ){
/*
*+
*  Name:
*     ndgFmprv

*  Purpose:
*     Format the provenance information from an NDF.

*  Invocation:
*     void ndgFmprv( int indf, int base, AstKeyMap **keymap, int *status )

*  Description:
*     This routine returns an AST KeyMap holding a set of text strings
*     containing information taken from the "PROVENANCE" extension in INDF.
*
*     The returned KeyMap has an entry with key "0" that describes the 
*     supplied NDF. It also has an entry describing each ancestor NDF.
*     These entries have keys "1", "2", "3", etc, up to the number of
*     ancestors in the NDF. 
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
*     "PARENTS" - A comma separated list of indicies into the ancestors
*                 array that identifies the direct parents of the NDF.
*
*     "MORE" - A summary of the contents of the MORE structure associated
*              with the NDF.
*
*     Finally, the returned KeyMap has an entry with key "MXLEN" that is
*     again a pointer to another KeyMap with the same entries listed above.
*     However, this time the entries are integers, not strings, and holds 
*     the maximum field width used to format the strings.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     base
*        If non-zero, then the PATH field in the returned KeyMap holds the 
*        base name of each NDF rather than the full path.
*     keymap
*        A location at which to returned a pointer to the returned AST KeyMap.
*     status
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables: */
   AstKeyMap *mxkey = NULL;
   AstKeyMap *anckey = NULL;
   Provenance *prov1 = NULL;
   char key[10];
   int *old_status;
   int i;

/* Initialise */
   *keymap = NULL;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Read the provenance extension from the NDF. */
   prov1 = ndg1ReadProvenanceExtension( indf, NULL, NULL, 0, status );

/* Create the returned KeyMap. */
   *keymap = astKeyMap( " " );

/* Create a KeyMap to hold the field widths. */
   mxkey = astKeyMap( " " );

/* Loop round every ancestor. */
   for( i = 0; i < prov1->nprov;  i++ ) {

/* Create a new KeyMap holding the formatted details of the ancestor. This
   also updates the maximum field widths for each field (held in "mxkey"). */
      anckey = ndg1FormatProv( prov1, i, base, mxkey, status );

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
   (void) astAnnul( mxkey );
   ndg1FreeProvenance( prov1, 1, status );

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgGtprv( int indf, int ianc, HDSLoc **prov, int *status ){
/*
*+
*  Name:
*     ndgGtprv

*  Purpose:
*     Get provenance information from an NDF.

*  Invocation:
*     void ndgGtprv( int indf, int ianc, HDSLoc **prov, int *status )

*  Description:
*     This routine returns information from the "PROVENANCE" extension
*     in INDF, describing the ancestor NDF with a given index.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     ianc
*        The index of the ancestor NDF for which information should be
*        returned. A value of zero will result in information about the NDF 
*        specified by "indf" being returned. Otherwise, the "ianc" value
*        is used as an index into the ANCESTORS array in the PROVENANCE 
*        extension. No error is reported if "ianc" is too large, but a
*        NULL pointer will be returned for "prov".
*     prov
*        The location at which to return a pointer to a locator for a
*        temporary HDS object containing the following components:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at 
*          which the provenance information for the ancestor NDF was 
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": Any extra information stored with the ancestor.
*        - "PARENTS": A 1D vector of integers that are the indices of the
*          immediate parents of the ancestor.
*
*        If the specified ancestor does not have any of these items of
*        information, then the corresponding component will not be
*        present in the returned HDS object. For instance, if the
*        ancestor has no immediate parent NDFs, then the "PARENTS" 
*        component will not be present in the returned HDS object. The
*        returned locator should be annulled using datAnnul when no
*        longer needed.
*     status 
*        The global status.
*-
*/

/* Local variables: */
   AstKeyMap *km = NULL;
   HDSLoc *more = NULL;
   HDSLoc *new_more = NULL;
   int *old_status;

/* Initialise. */
   *prov = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Get a KeyMap holding the required values. */
   ndgGtprvk( indf, ianc, &km, &more, status );

/* If any provenance information was read, create the returned temporary 
   HDS object. */
   if( km ) {
      datTemp( TEMP_TYPE, 0, NULL, prov, status );

/* Copy the required components form the KeyMap to the returned HDS
   structure. */
      ndg1A2h0c( PATH_NAME, km, *prov, status );
      ndg1A2h0c( DATE_NAME, km, *prov, status );
      ndg1A2h0c( CREATOR_NAME, km, *prov, status );
      ndg1A2h1i( PARENTS_NAME, km, *prov, status );

/* If defined, copy the MORE structure. */
      if( more ) {
         datNew( *prov, MORE_NAME, MORE_TYPE, 0, NULL, status );
         datFind( *prov, MORE_NAME, &new_more, status );
         ndg1CopyComps( more, new_more, status );
         datAnnul(  &new_more, status );
      }
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgGtprvk( int indf, int ianc, AstKeyMap **prov, HDSLoc **more, 
                int *status ){
/*
*+
*  Name:
*     ndgGtprvk

*  Purpose:
*     Create a KeyMap holding provenance information from an NDF.

*  Invocation:
*     void ndgGtprvk( int indf, int ianc, AstKeyMap **prov, HDSLoc **more, 
*                     int *status )

*  Description:
*     This routine returns information from the "PROVENANCE" extension
*     in INDF, describing the ancestor NDF with a given index. It is
*     similar to ndgGtprv except that the nformation is returned in the 
*     form of an AST KeyMap rather than an HDS structure.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     ianc
*        The index of the ancestor NDF for which information should be
*        returned. A value of zero will result in information about the NDF 
*        specified by "indf" being returned. Otherwise, the "ianc" value
*        is used as an index into the ANCESTORS array in the PROVENANCE 
*        extension. No error is reported if "ianc" is too large (i.e.
*        larger than the value returned by ndgCtprv), but NULL will be 
*        returned in "*prov".
*     prov
*        The location at which to return a pointer to an AST KeyMap
*        containing entries with the following keys and values:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at 
*          which the provenance information for the ancestor NDF was 
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "PARENTS": A 1D vector of integers that are the indices of the
*          immediate parents of the ancestor.
*        - "MORE": A KeyMap containing extra scalar information stored 
*        with the ancestor. Only scalar items stored directly within the
*        top level of the MORE structure are returned in the KeyMap. The
*        HDS name of the component is used as the key. The full contents
*        of the MORE structure are returned by the "more" parameter (see 
*        below).
*
*        If the specified ancestor does not have any of these items of
*        information, then the corresponding entry will not be present 
*        in the returned KeyMap. For instance, if the ancestor has no 
*        immediate parent NDFs, then the "PARENTS" entry will not be 
*        present in the KeyMap. A NULL pointer will be returned if the
*        NDF has no provenance extension, or if "ianc" is outside the
*        bounds of the ANCESTORS array (and is not zero).
*     more
*        The location at which to return a pointer to a locator for a
*        temporary HDS object containing a full deep copy of the MORE
*        structure associated with the requested ancestor. The
*        returned locator should be annulled using datAnnul when no
*        longer needed. A NULL pointer may be supplied for this parameter
*        if the MORE structure is not needed. A NULL pointer will be
*        returned if the requested ancestor has no MORE component.
*     status 
*        The global status.
*-
*/

/* Local variables: */
   HDSLoc *aloc = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *xloc = NULL;
   char path_buf[ PATH_LEN + 1 ];     
   hdsdim  dim[ 1 ];
   int *old_status;
   int path_len;
   int there;
   size_t nanc;

/* Initialise. */
   if( more ) *more = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* See if the supplied NDF has a provenance extension. If it has, get a 
   HDS locator to it. */
   ndfXstat( indf, EXT_NAME, &there, status ); 
   if( there ) {
      ndfXloc( indf, EXT_NAME, "Read", &xloc, status ); 

/* Create the returned (empty) KeyMap. */
      *prov = astKeyMap( " " );

/* If information about the supplied NDF itself is required, copy the 
   DATE, CREATOR and PARENTS components to the KeyMap, if they exist. */
      if( ianc == 0 ) {
         ndg1H2a0c( DATE_NAME, xloc, *prov, status );
         ndg1H2a0c( CREATOR_NAME, xloc, *prov, status );
         ndg1H2a1i( PARENTS_NAME, xloc, *prov, status );

/* Get the path to the supplied NDF, and store in the KeyMap. The path is
   obtained from the NDF library since the PROVENANCE extension only
   contains paths for ancestor NDFs, not the main NDF itself. */
         ndfMsg( "NDF", indf ); 
         msgLoad( " ", "^NDF", path_buf, PATH_LEN, &path_len, status );
         astMapPut0C( *prov, PATH_NAME, path_buf, " " );

/* Now deal with cases where we are returning information about an
   ancestor. */
      } else {

/* Get a locator for the ANCESTORS array, and get its length. */
         aloc = ndg1GtAnc( xloc, &nanc, status );
         if( aloc ) {

/* Check the supplied (one-based) index is within the bounds of the 
   ANCESTORS array. */
            if( ianc >= 1 && ianc <= (int) nanc ) {

/* Get a locator to the requested element of the ANCESTORS array. */
               dim[ 0 ] = ianc;
               datCell( aloc, 1, dim, &cloc, status );

/* Copy the PARENTS, DATE, PATH and CREATOR components from the ANCESTORS
   array element to the KeyMap. */
               ndg1H2a0c( DATE_NAME, cloc, *prov, status );
               ndg1H2a0c( CREATOR_NAME, cloc, *prov, status );
               ndg1H2a0c( PATH_NAME, cloc, *prov, status );
               ndg1H2a1i( PARENTS_NAME, cloc, *prov, status );

/* Copy top-level scalar items from any HDS MORE structure into a new
   KeyMap (with key "MORE") stored within the returned KeyMap. */
               ndg1H2a0h( MORE_NAME, cloc, *prov, status );

/* If an HDS locator for the full MORE structure is required, create a
   temporary HDS object containing a copy of it. */
               if( more ) *more = ndg1TCopy( cloc, MORE_NAME, status );

/* Annul the locator to the ANCESTORS array element. */
               datAnnul( &cloc, status );
            }

/* Annul the locator to the ANCESTORS array. */
            datAnnul( &aloc, status );
         }
      }

/* Annul the locator to the PROVENANCE extension. */
      datAnnul( &xloc, status );
   }

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgPtprv( int indf1, int indf2, HDSLoc *more, int isroot, 
               const char *creator, int *status ){
/*
*+
*  Name:
*     ndgPtprv

*  Purpose:
*     Add provenance information to an NDF.

*  Invocation:
*     void ndgPtprv( int indf1, int indf2, HDSLoc *more, int isroot, 
*                    const char *creator, int *status )

*  Description:
*     This routine stores information in the "PROVENANCE" extension of
*     INDF1 indicating that INDF2 was used in the creation of INDF1. 
*     The provenance information is stored in an NDF extension call 
*     "PROVENANCE".

*  Arguments:
*     indf1 
*        An identifier for a newly created NDF.
*     indf2 
*        An identifier for an NDF that was used in the creation of INDF1.
*     more 
*        A locator for an HDS structure containing arbitrary additional 
*        information about INDF2, and how INDF2 was used in the creation 
*        of INDF1. This information is stored in the provenance extension 
*        of INDF1.
*     isroot 
*        If non-zero, then INDF2 will be treated as a root NDF. That is,
*        any provenance information in INDF2 is ignored. If zero, then
*        any provenance information in INDF2 is copied into INDF1. INDF2
*        is then only a root NDF if it contains no provenance information.
*     creator
*        A text identifier for the software that created INDF1 (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*     status 
*        The global status.
*-
*/

/* Local variables: */
   Provenance *prov1 = NULL;
   Provenance *prov2 = NULL;
   int free_provs;
   int i;
   int *old_status;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Get the existing provenance information from the two NDFs. */
   prov1 = ndg1ReadProvenanceExtension( indf1, NULL, creator, 0, status );
   prov2 = ndg1ReadProvenanceExtension( indf2, more, NULL, isroot, status );

/* Indicate that the "Prov" structures referred to by prov2 should be
   freed when ndgFreeProvenance is called. */
   free_provs = 1;

/* Extend the "provs" list in "prov1" so that we can add pointers to all the 
   Prov structures in "prov2. */
   if( prov1 && prov2 ) {
      prov1->provs = astGrow( prov1->provs, prov1->nprov + prov2->nprov,
                              sizeof( Prov *) );
   }
   if( astOK ) {

/* Copy the Prov pointers from "prov2" to "prov1". */
      for( i = 0; i < prov2->nprov; i++ ) {
         prov1->provs[ i + prov1->nprov ] = prov2->provs[ i ];
      }   

/* Update the length of the "provs" array in "prov1". */
      prov1->nprov += prov2->nprov;   

/* Indicate that the "Prov" structures referred to by "prov2" should not be
   freed when ndgFreeProvenance is called. This is because they are now
   the responsibility of "prov1", having been copied into the
   prov1->provs list above. */
      free_provs = 0;

/* Record INDF2 as a parent of INDF1. */
      ndg1ParentChild( prov2->main, prov1->main, status );

/* Purge any duplicate entries in the extended provenance information. */
      ndg1PurgeProvenance( prov1, status );

/* Write the extended provenance information out to INDF1. */
      ndg1WriteProvenanceExtension( prov1, indf1, status );
   }

/* Free the provenance structures. */
   ndg1FreeProvenance( prov1, 1, status );
   ndg1FreeProvenance( prov2, free_provs, status );

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgRmprv( int indf, int ianc, int *status ){
/*
*-
*  Name:
*     ndgRmprv

*  Purpose:
*     Remove provenance information from an NDF.

*  Invocation:
*     void ndgRmprv( int indf, int ianc, int *status )

*  Description:
*     This routine removes a given ancestor from the "PROVENANCE" 
*     extension in INDF. The direct parents of the removed ancestor are
*     assigned to the direct children of the removed ancestor.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     ianc
*        The index of the ancestor NDF to be removed. The supplied value
*        must be at least 1, and must be no more than the number of
*        ancestors in the provenance extension (as returned by NDG_CTPRV).
*        An error is reported otherwise.
*     status
*        The global status.

*-
*/

/* Local Variables: */
   Prov *anc = NULL;
   Prov *child = NULL;
   Prov *parent = NULL;
   Provenance *prov = NULL;
   int *old_status;
   int i;             
   int ichild;
   int iparent;
   int n;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Read the provenance extension from the NDF. */
   prov = ndg1ReadProvenanceExtension( indf, NULL, NULL, 0, status );

/* Check the "ianc" value is within the bounds of the ANCESTORS array. */
   if( prov && ianc > 0 && ianc < prov->nprov ) {

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
            ndg1ParentChild( parent, child, status );
         }
      }

/* Loop round all the direct parents of the ancestor. */
      n = anc->nparent;
      for( iparent = 0; iparent < anc->nparent; iparent++ ) {
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

/* Purge any duplicate entries in the provenance information. */
      ndg1PurgeProvenance( prov, status );

/* Store the modified provenance informtion in the NDF. */
      ndg1WriteProvenanceExtension( prov, indf, status );

/* Report an error if the ianc value is bad. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "NDF", indf );
      if( prov ) {
         msgSeti( "IANC", ianc );
         msgSeti( "N", prov->nprov );
         errRep( " ", "Cannot remove provenance ancestor ^IANC from '^NDF': "
                 "only ^N ancestors found.", status );
      } else {
         errRep( " ", "Cannot remove provenance information from '^NDF': "
                 "no provenance found.", status );
      }
   }

/* Free resources. */
   ndg1FreeProvenance( prov, 1, status );

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgRtprv( int indf, AstKeyMap **roots, int *status ){
/*
*+
*  Name:
*     ndgRtprv

*  Purpose:
*     Identify the root ancestors of an NDF.

*  Invocation:
*     void ndgRtprv( int indf, AstKeyMap **roots, int *status )

*  Description:
*     This routine uses the PROVENANCE extension of the supplied NDF to 
*     obtain and return information identifying the root ancestors of
*     the supplied NDF. An ancestor is a root ancestor if it does not itself
*     have any ancestors.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     roots
*        A location at which to return a pointer to an AST KeyMap. This
*        KeyMap will contain an entry for each root ancestor. The key
*        associated with each entry is the path to the NDF and the value 
*        of the entry is an integer that gives the position of the root
*        ancestor within the list of all ancestors. This integer value
*        can be supplied to ndgGtprv in order to get further information
*        about the root ancestor.
*     status 
*        The global status.
*-
*/

/* Local variables: */
   Prov *prov = NULL;
   Provenance *prov1 = NULL;
   int *old_status;
   int i;

/* Initialise. */
   *roots = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Read the provenance extension from the NDF. */
   prov1 = ndg1ReadProvenanceExtension( indf, NULL, NULL, 0, status );

/* Create the returned KeyMap. */
   *roots = astKeyMap( " " );

/* Loop round every ancestor. */
   for( i = 0; i < prov1->nprov;  i++ ) {
      prov = prov1->provs[ i ];

/* If this ancestor has no parents, add its details to the returned
   KeyMap. */
      if( prov->nparent == 0 ) astMapPut0I( *roots, prov->path, i, " " );
   }

/* Free resources. */
   ndg1FreeProvenance( prov1, 1, status );

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}

void ndgMdprv( int indf, int ianc, HDSLoc *prov, int *status ){
/*
*+
*  Name:
*     ndgMdprv

*  Purpose:
*     Modify provenance information in an NDF.

*  Invocation:
*     void ndgMdprv( int indf, int ianc, HDSLoc *prov, int *status )

*  Description:
*     This routine modifies the information stored for a given ancestor 
*     in the "PROVENANCE" extension of an NDF. The new values to store 
*     are supplied in an HDS structure such as is returned by ndgGtprv.

*  Arguments:
*     indf
*        An identifier for the NDF containing the provenance information.
*     ianc
*        The index of the ancestor NDF for which information should be
*        modified. A value of zero will result in information about the NDF 
*        specified by "indf" being modified. Otherwise, the "ianc" value
*        is used as an index into the ANCESTORS array in the PROVENANCE 
*        extension. An error is reported if "ianc" is too large.
*     prov
*        A locator for an HDS object containing the values to store. It
*        should have at least the following components:
*
*        - "PATH": A string holding the path of the ancestor NDF.
*        - "DATE": A string holding the formatted UTC date and time at 
*          which the provenance information for the ancestor NDF was 
*          recorded.
*        - "CREATOR": A string identifying the software that created the
*          ancestor NDF.
*        - "MORE": Any extra information stored with the ancestor.
*
*        If the "DATE", "CREATOR" or "MORE" components are missing then 
*        the corresponding item of information will be deleted from the 
*        provenance extension. An error is reported if "PATH" is missing.
*        Note, the PARENTS list stored with the specified ancestor cannot 
*        be modified (any "PARENTS" component in the supplied HDS structure 
*        will be ignored).
*     status 
*        The global status.
*-
*/

/* Local variables: */
   HDSLoc *loc = NULL;
   Prov *anc = NULL;
   Provenance *prov1 = NULL;
   char *cval = NULL;
   int *old_status;
   int there;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status variable. */
   old_status = astWatch( status );

/* Read the provenance extension from the NDF. */
   prov1 = ndg1ReadProvenanceExtension( indf, NULL, NULL, 0, status );

/* Check the "ianc" value is within the bounds of the ANCESTORS array. */
   if( prov1 && ianc >= 0 && ianc < prov1->nprov ) {

/* Get a pointer to the ancestors Prov structure. */
      anc = prov1->provs[ ianc ];

/* If defined, copy the PATH component. Report an error otherwise. */
      cval = ndg1GetTextComp( prov, PATH_NAME, NULL, 0, status );
      if( cval ) {
         anc->path = astStore( anc->path, cval, strlen( cval ) + 1 );
         cval = astFree( cval );

      } else if( *status == SAI__OK ){
         *status = SAI__ERROR;
         ndfMsg( "NDF", indf );
         errRep( " ", "Cannot modify provenance information in '^NDF': "
                 "no new PATH supplied.", status );
      }

/* If defined, copy the DATE component. Delete it otherwise. */
      cval = ndg1GetTextComp( prov, DATE_NAME, NULL, 0, status );
      if( cval ) {
         anc->date = astStore( anc->date, cval, strlen( cval ) + 1 );
         cval = astFree( cval );
      } else {
         anc->date = astFree( anc->date );
      }

/* If defined, copy the CREATOR component. Delete it otherwise. */
      cval = ndg1GetTextComp( prov, CREATOR_NAME, NULL, 0, status );
      if( cval ) {
         anc->creator = astStore( anc->creator, cval, strlen( cval ) + 1 );
         cval = astFree( cval );
      } else {
         anc->creator = astFree( anc->creator );
      }

/* Delete any pre-existing MORE component and then, if a new one is
   available in the supplied HDS structure, copy it into the Provenance
   structure. */
      if( anc->more ) datAnnul( &(anc->more), status );
      datThere( prov, MORE_NAME, &there, status );
      if( there ) {
         datFind( prov, MORE_NAME, &loc, status );
         datTemp( TEMP_TYPE, 0, NULL, &( anc->more ), status );
         datCopy( loc, anc->more, MORE_NAME, status );
         datAnnul( &loc, status );
      }

/* Store the modified provenance informtion in the NDF. */
      ndg1WriteProvenanceExtension( prov1, indf, status );

/* Report an error if the ianc value is bad. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "NDF", indf );
      if( prov1 ) {
         msgSeti( "IANC", ianc );
         msgSeti( "N", prov1->nprov );
         errRep( " ", "Cannot modify provenance ancestor ^IANC in '^NDF': "
                 "only ^N ancestors found.", status );
      } else {
         errRep( " ", "Cannot modify provenance information in '^NDF': "
                 "no provenance found.", status );
      }
   }

/* Free resources. */
   ndg1FreeProvenance( prov1, 1, status );

/* Re-instate the original AST status variable. */
   astWatch( old_status );
}




/* Private functions. */
/* ================= */
static void ndg1A2h0c( const char *key, AstKeyMap *km, HDSLoc *loc, 
                       int *status ){
/*
*  Name:
*     ndg1A2h0c

*  Purpose:
*     Copy a scalar character value from an AST KeyMap to an HDS structure.

*  Invocation:
*     void ndg1A2h0c( const char *key, AstKeyMap *km, HDSLoc *loc, 
*                     int *status )

*  Description:
*     This routine copies a scalar character value from an AST KeyMap
*     to a supplied HDS structure. No error is reported if the specified
*     key does not exist in the KeyMap, but no component is added to the 
*     HDS structure. The KeyMap entry key is used as the name for the new 
*     HDS component.

*  Arguments:
*     key
*        The key for the KeyMap entry to copy.
*     km
*        Pointer to the KeyMap containing the value to copy.
*     loc
*        Locator for the HDS structure in which to store the copy.
*     status 
*        The global status.

*/

/* Local variables: */
   HDSLoc *cloc = NULL;
   const char *value;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Attempt to get a pointer to value. Skip to the end if the KeyMap does
   not hold a value for the requested key. */
   if( astMapGet0C( km, key, &value ) ) {

/* Create a new component of the required type in the sypplied HDS
   structure, and get a lcoator to it. */
      datNew0C( loc, key, strlen( value ), status );
      datFind( loc, key, &cloc, status );

/* Store the value in the new component. */
      datPut0C( cloc, value, status );

/* Annul the locator for the new component. */
      datAnnul( &cloc, status );
   }
}

static void ndg1A2h1i( const char *key, AstKeyMap *km, HDSLoc *loc, 
                       int *status ){
/*
*  Name:
*     ndg1A2h1i

*  Purpose:
*     Copy a vector integer value from an AST KeyMap to an HDS structure.

*  Invocation:
*     void ndg1A2h1i( const char *key, AstKeyMap *km, HDSLoc *loc, 
*                     int *status )

*  Description:
*     This routine copies a vector integer value from an AST KeyMap
*     to a supplied HDS structure. No error is reported if the specified
*     key does not exist in the KeyMap, but no component is added to the 
*     HDS structure. The KeyMap entry key is used as the name for the new 
*     HDS component.

*  Arguments:
*     key
*        The key for the KeyMap entry to copy.
*     km
*        Pointer to the KeyMap containing the value to copy.
*     loc
*        Locator for the HDS structure in which to store the copy.
*     status 
*        The global status.

*/

/* Local variables: */
   HDSLoc *cloc = NULL;
   hdsdim dim[ 1 ];
   int *value;
   int nval;
   int size;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the number of elements in the vector. */
   size = astMapLength( km, key );

/* Skip to the end if the KeyMap does not hold a value for the requested 
   key. */
   if( size > 0 ) {

/* Create a new component of the required type in the sypplied HDS
   structure, and get a locator to it. */
      datNew1I( loc, key, size, status );
      datFind( loc, key, &cloc, status );

/* Map the vector. */
      dim[ 0 ] = size;
      datMapI( cloc, "WRITE", 1, dim, &value, status );

/* Store the vector values in the mapped array. */
      (void) astMapGet1I( km, key, size, &nval, value );

/* Annul the locator for the new component. */
      datAnnul( &cloc, status );
   }
}

static void ndg1CopyComps( HDSLoc *loc1, HDSLoc *loc2, int *status ){
/*
*  Name:
*     ndg1CopyComps

*  Purpose:
*     Copy the contents of "loc1" into "loc2".

*  Invocation:
*     void ndg1CopyComps( HDSLoc *loc1, HDSLoc *loc2, int *status )
     
*  Description:
*     This routine copies every component from "loc1" into "loc2".

*  Arguments:
*     loc1
*        Locator for the source object.
*     loc2
*        Locator for the destination object.
*     status 
*        The global status.

*/

/* Local variables: */
   HDSLoc *loc = NULL;
   char name[ DAT__SZNAM + 1 ];
   int icomp;
   int ncomp;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the number of components to copy. */
   datNcomp( loc1, &ncomp, status );

/* Loop round every component. */
   for( icomp = 1; icomp <= ncomp; icomp++ ) {

/* Get a locator for the current component in the source object. */
      datIndex( loc1, icomp, &loc, status );

/* Get its name. */
      datName( loc, name, status );

/* Copy it into the destination, using the same name. */
      datCopy( loc, loc2, name, status );

/* Annull the source component locator. */
      datAnnul( &loc, status );
   }
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

/* Reduce the number of children. */
   parent->nchild = j;

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

/* Add 1 to convert from zero-based to one-based and return. */
   return iprov + 1;
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
*     puts the formatted strings into a new AstKeyMap. It also updates the 
*     maximum length for any formatted item.
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
*        "PARENTS" - A comma separated list of indicies into the ancestors
*                    array that identifies the direct parents of the NDF.
*        
*        "MORE" - A summary of the contents of the MORE structure associated
*                 with the NDF.
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
*        Pointer to an existing KeyMap. On exit it will contain an entry for
*        each of the keys listed under "Description:" above. All entries
*        will be be scalar integers. Each integer value is updated by this
*        function so that it holds the larger of the supplied value and the 
*        field width used to format the corresponding item in the returned 
*        KeyMap. The supplied value is left unchanged if the returned
*        KeyMap does not contain a value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new KeyMap describing the supplied Prov structure.

*/

/* Local Variables: */
   AstKeyMap *result = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *mloc = NULL;
   HDSLoc *dloc = NULL;
   Prov *prov = NULL;
   char *p = NULL;
   char *list = NULL;
   char *val = NULL;
   char buf[ 20 ];
   char name[DAT__SZNAM+1];
   char type[DAT__SZTYP + 1];
   hdsdim dims[ NDF__MXDIM ];
   int icomp;
   int idim;
   int iel;
   int k;
   size_t len;
   int mxlen;
   int nc;
   int ncomp;
   int ndim;
   int prim;

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

/* Initialise a pointer to a dynamic string holding the comma separated
   list of parent indices. */
   list = NULL;
   nc = 0;

/* If the supplied Prov structure has any parents, store a comma separated 
   list of the corresponding ANCESTOR indices in the returned KeyMap, and 
   update the maximum length of the "PARENTS" field. */
   if( prov->nparent > 0 ) {

/* Loop round each of the parents. */
      for( k = 0; k < prov->nparent && *status == SAI__OK; k++ ) {

/* Find the index of this parent in the ANCESTORS array, and format it. */
         sprintf( buf, "%d", ndg1FindAncestorIndex( prov->parents[ k ], 
                                                     provenance, status ) );

/* Append this string to the end of the comma separated list, preceeding
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

/* Initialise a pointer to a dynamic string holding the summary of the
   MORE structure. */
   list = NULL;
   nc = 0;

/* If the supplied Prov structure has a MORE object, create a summary
   of it. */
   if( prov->more ) {

/* The MORE object itself is contained within prov->more. */
      datFind( prov->more, "MORE", &mloc, status );

/* See if the MORE object is primtive. */
      datPrim( mloc, &prim, status );

/* If the MORE object is an array, we just give its type and shape. */
      datShape( mloc, NDF__MXDIM, dims, &ndim, status );
      if( ndim > 0 ) {
         datType( mloc, type, status );
         list = astAppendString( list, &nc, "<" );
         list = astAppendString( list, &nc, type );
         list = astAppendString( list, &nc, ">(" );
         sprintf( buf, "%d", dims[ 0 ] );
         list = astAppendString( list, &nc, buf );

         for( idim = 1; idim < ndim; idim++ ) {
            list = astAppendString( list, &nc, "," );
            sprintf( buf, "%d", dims[ idim ] );
            list = astAppendString( list, &nc, buf );
         }
         list = astAppendString( list, &nc, ")" );

/* Otherwise if the MORE object is a primitive scalar, just store its value. */
      } else if( prim ){
         datLen( mloc, &len, status );
         nc = len;
         list = astMalloc( ( nc + 1 )* sizeof( char ) );
         datGet0C( mloc, list, nc + 1, status );
      
/* Otherwise the MORE object is a single structure. Loop round each of its
   components. */
      } else {
         datNcomp( mloc, &ncomp, status );
         for( icomp = 1; icomp <= ncomp; icomp++ ) {
            datIndex( mloc, icomp, &cloc, status );

/* Unless this is the first component, add a comma to separate this
   component from the previous one in the summary string. */
            if( icomp > 1 ) list = astAppendString( list, &nc, ", " );

/* Get the component name and type, whether it is a primitive, and its
   shape. */
            datName( cloc, name, status );
            datType( cloc, type, status );
            datPrim( cloc, &prim, status );
            datShape( cloc, NDF__MXDIM, dims, &ndim, status );

/* If it is scalar... */
            if( ndim == 0 ) {

/* If it is primitive, display it as "name=value". */
               if( prim ) {
                  list = astAppendString( list, &nc, name );
                  list = astAppendString( list, &nc, "=" );
                  datClen( cloc, &len, status );
                  val = astMalloc( ( len + 1 )*sizeof( char ) );
                  datGet0C( cloc, val, len + 1, status );
                  list = astAppendString( list, &nc, val );
                  val = astFree( val );

/* If it is a structure, display it as "name=<type>". */
               } else {
                  list = astAppendString( list, &nc, name );
                  list = astAppendString( list, &nc, "=<" );
                  list = astAppendString( list, &nc, type );
                  list = astAppendString( list, &nc, ">" );
               }

/* If it is an array... */
            } else {

/* If it is a 1D primitive, display it as "name=(value1,value2,...)" up to a
   maximum of 10 values. Append an ellipsis if there are more than 10
   values. */
               if( prim && ndim == 1 ) {
                  list = astAppendString( list, &nc, name );
                  list = astAppendString( list, &nc, "=(" );
                  datClen( cloc, &len, status );
                  val = astMalloc( ( len + 1 )*sizeof( char ) );

                  for( iel = 1; iel <= dims[ 0 ]; iel++ ) {
                     datCell( cloc, 1, (hdsdim *) &iel, &dloc, status );
                     datGet0C( dloc, val, len + 1, status );

                     if( iel > 1 ) list = astAppendString( list, &nc, "," );
                     list = astAppendString( list, &nc, val );
                     datAnnul( &dloc, status );

                     if( iel == 10 ) break;
                  }

                  if( dims[ 0 ] > 10 ) list = astAppendString( list, &nc, 
                                                               ",..." );
                  list = astAppendString( list, &nc, ")" );
                  val = astFree( val );

/* If it is a structure array, or if it is a multi-dimensional primitive
   array, display it as "name=<type>(shape)". */
               } else {
                  list = astAppendString( list, &nc, name );
                  list = astAppendString( list, &nc, "=<" );
                  list = astAppendString( list, &nc, type );
                  list = astAppendString( list, &nc, ">(" );
                  sprintf( buf, "%d", dims[ 0 ] );
                  list = astAppendString( list, &nc, buf );
         
                  for( idim = 1; idim < ndim; idim++ ) {
                     list = astAppendString( list, &nc, "," );
                     sprintf( buf, "%d", dims[ idim ] );
                     list = astAppendString( list, &nc, buf );
                  }
                  list = astAppendString( list, &nc, ")" );

               }
            }

/* Annul the component locator. */
            datAnnul( &cloc, status );
         }
      }

/* Annul the MORE locator. */
      datAnnul( &mloc, status );
   }

/* If the summary is not empty, store it in the returned KeyMap, and then 
   free it. */
   if( list ) {
      astMapPut0C( result, MORE_NAME, list, NULL );
      list = astFree( list );
   } 

/* Find the maximum of the current field width and the previous maximum
   field width. */
   if( astMapGet0I( mxlenkey, MORE_NAME, &mxlen ) ) {
      mxlen = ( nc > mxlen ) ? nc : mxlen;
   } else {
      mxlen = nc;
   }

/* Store the new maximum field width. */
   astMapPut0I( mxlenkey, MORE_NAME, mxlen, NULL );

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

/* Annul the HDS locator for the MORE structure. */
   if( prov->more ) datAnnul( &(prov->more), status );

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
*     This routine returns a locator for the ANCESTORS array in the 
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

static void ndg1H2a0c( const char *name, HDSLoc *loc, AstKeyMap *km, 
                       int *status ){
/*
*  Name:
*     ndg1H2a0c

*  Purpose:
*     Copy a scalar character component from an HDS structure to an AST
*     KeyMap.

*  Invocation:
*     void ndg1H2a0c( const char *name, HDSLoc *loc, AstKeyMap *km, 
                      int *status )

*  Description:
*     This routine copies a named scalar component from an HDS structure
*     to a supplied AST KeyMap. No error is reported if the component does 
*     not exist in the HDS structure, but no entry is added to the KeyMap.
*     The HDS component name is used a the key for the new KeyMap entry.

*  Arguments:
*     name
*        The name of the scalar HDS component to copy.
*     loc
*        Locator for the HDS structure containing the component to copy.
*     km
*        Pointer to the KeyMap in which to store the copy.
*     status 
*        The global status.

*/

/* Local variables: */
   char *value;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Attempt to get a dynamically allocated string holding the component
   value. A NULL pointer is returned if the component does not exist. */
   value = ndg1GetTextComp( loc, name, NULL, 0, status );

/* If succesful, put the string cvalue into the KeyMap and free the string. */
   if( value ) {
      astMapPut0C( km, name, value, NULL );  
      value = astFree( value );
   }
}

static void ndg1H2a0h( const char *name, HDSLoc *loc, AstKeyMap *km, 
                       int *status ){
/*
*  Name:
*     ndg1H2a0h

*  Purpose:
*     Copy scalar values in an HDS component of an HDS structure to 
*     an AST KeyMap.

*  Invocation:
*     void ndg1H2a0h( const char *name, HDSLoc *loc, AstKeyMap *km, 
                      int *status )

*  Description:
*     This routine copies all scalar values in a named component of an HDS 
*     structure to a supplied AST KeyMap. No error is reported if the 
*     named component does not exist in the HDS structure, but no entry is 
*     added to the KeyMap. The HDS component name is used a the key for 
*     the new KeyMap entry. The new KeyMap entry will be a pointer to
*     another KeyMap containing the copied scalar values. NB, any structures 
*     or arrays within the named component are ignored. If the named
*     component does not contain any scalar primitive values, then no new
*     entry will be added to the supplied KeyMap.

*  Arguments:
*     name
*        The name of the HDS component containing the scalar values to copy.
*     loc
*        Locator for the HDS structure containing the named component.
*     km
*        Pointer to the KeyMap in which to store the copied values.
*     status 
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-2009 (DSB):
*        Original version.
*     4-MAR-2009 (TIMJ):
*        Use correct locator when reading components.
*     {enter_further_changes_here}


*/

/* Local variables: */
   AstKeyMap *km2 = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *dloc = NULL;
   char *value = NULL;
   char dname[ DAT__SZNAM + 1 ];
   int empty;
   int icomp;
   int isprim;
   int ncomp;
   int there;
   size_t clen;
   size_t size;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check that the required component exists within the supplied object. */
   datThere( loc, name, &there, status );
   if( there ) {

/* Get a locator for the component to be copied. */
      datFind( loc, name, &cloc, status );

/* Create a new KeyMap to hold the copied values. */
      km2 = astKeyMap( " " );

/* Indicate this KeyMap is currently empty. */
      empty = 1;

/* Get the number of components within the named component. */
      datNcomp( cloc, &ncomp, status );

/* Loop round every component. */
      for( icomp = 1; icomp <= ncomp; icomp++ ) {

/* Get a locator for the current component in the source object. */
         datIndex( cloc, icomp, &dloc, status );

/* Check its size. This will be 1 for a scalar component. */
         datSize( dloc, &size, status );
         if( size == 1 ) {

/* Check it is primtive. */
            datPrim( dloc, &isprim, status );
            if( isprim ) {
         
/* Get its name. */
               datName( dloc, dname, status );

/* Get its length as a string. */
               datClen( dloc, &clen, status );

/* Ensure the buffer is big enough to hold its value. */
               value = astGrow( value, 1, clen + 1 );

/* Get its value as a string. */
               if( value ) {
                  datGet0C( dloc, value, clen + 1, status );

/* Put it into the KeyMap. */
                  astMapPut0C( km2, dname, value, NULL );  

/* Indicate the KeyMap is not empty. */
                  empty = 0;
               }
            }
         }

/* Annull the source component locator. */
         datAnnul( &dloc, status );
      }

/* Free the memory used to hold the string values. */
      value = astFree( value );

/* Annul the component locator (this also unmaps the vector). */
      datAnnul( &cloc, status );

/* If the KeyMap is not empty, add it to the supplied KeyMap. */
      if( !empty ) astMapPut0A( km, name, km2, NULL );  

/* Annul the new KeyMap pointer. */
      km2 = astAnnul( km2 );      

   }
}

static void ndg1H2a1i( const char *name, HDSLoc *loc, AstKeyMap *km, 
                       int *status ){
/*
*  Name:
*     ndg1H2a1i

*  Purpose:
*     Copy a integer vector component from an HDS structure to an AST
*     KeyMap.

*  Invocation:
*     void ndg1H2a1i( const char *name, HDSLoc *loc, AstKeyMap *km, 
                      int *status )

*  Description:
*     This routine copies a named integer vector component from an HDS 
*     structure to a supplied AST KeyMap. No error is reported if the 
*     component does not exist in the HDS structure, but no entry is added 
*     to the KeyMap. The HDS component name is used a the key for the new 
*     KeyMap entry.

*  Arguments:
*     name
*        The name of the integer vector HDS component to copy.
*     loc
*        Locator for the HDS structure containing the component to copy.
*     km
*        Pointer to the KeyMap in which to store the copy.
*     status 
*        The global status.

*/

/* Local variables: */
   HDSLoc *cloc = NULL;
   int there;
   int *value = NULL;
   size_t size;
   hdsdim dim[ 1 ];

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check that the required component exists within the supplied object. */
   datThere( loc, name, &there, status );
   if( there ) {

/* Get a locator for the component to be copied. */
      datFind( loc, name, &cloc, status );

/* Get the length of the vector. */
      datSize( cloc, &size, status );

/* Map the vector. */
      dim[ 0 ] = size;
      datMapI( cloc, "READ", 1, dim, &value, status );

/* Copy the vector into the KeyMap. */
      astMapPut1I( km, name, size, value, NULL );

/* Annul the component locator (this also unmaps the vector). */
      datAnnul( &cloc, status );
   }
}

static Prov *ndg1MakeProv( const char *path, const char *date, 
                           const char *creator, HDSLoc *more, 
                           Provenance *provenance, int *status ){
/*
*  Name:
*     ndg1MakeProv

*  Purpose:
*     Make a new Prov structure and add it into an existing Provenance.

*  Invocation:
*     Prov *ndg1MakeProv( const char *path, const char *date, 
*                         const char *creator, HDSLoc *more, 
*                         Provenance *provenance, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Prov structure,
*     and stores the supplied values in it. The new structure is appended
*     to the end of the list of Prov structures held in the supplied
*     Provenance structure. No parents or children are stored in the
*     new Prov structure.

*  Arguments:
*     path
*        Pointer to a string holding the path to the NDF described by the
*        new Prov structure.
*     date
*        Pointer to a string holding the formatted UTC date and time at
*        which provenance information was recorded for the NDF.
*     creator
*        Pointer to a string holding an arbitrary identifier for the
*        software that created the NDF.
*     more
*        Pointer to a locator for an HDS object holding extra information
*        about the NDF. A deep copy is taken of the supplied structure.
*     provenance
*        Pointer to an existing Provenance structure to which the new
*        Prov structure will be added, or NULL.
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

/* Store copies of the supplied strings. Store NULL pointers for any
   unspecified strings. */
      len = path ? strlen( path ) + 1 : 0;
      result->path = astStore( NULL, path, len*sizeof( char ) );

      len = date ? strlen( date ) + 1 : 0;
      result->date = astStore( NULL, date, len*sizeof( char ) );

      len = creator ? strlen( creator ) + 1 : 0;
      result->creator = astStore( NULL, creator, len*sizeof( char ) );

/* Store a deep copy of the "more" structure in a temporary HDS object. */
      result->more = NULL;
      if( more ) {
         datTemp( TEMP_TYPE, 0, NULL, &( result->more ), status );
         datCopy( more, result->more, MORE_NAME, status );
      }

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

/* Store a poitner ot the mina NDF Prov structure. */
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

static void ndg1ParentChild( Prov *parent, Prov *child, int *status ){
/*
*  Name:
*     ndg1ParentChild

*  Purpose:
*     Create a parent-child link between two supplied Prov structures.

*  Invocation:
*     void ndg1ParentChild( Prov *parent, Prov *child, int *status )

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
   for( i = 0; i < child->nparent; i++ ) {
      if( ndg1TheSame( child->parents[ i ], parent, status ) ) {
         found = 1;
         break;
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
}

static void ndg1ParentChildIndex( Provenance *provenance, int iparent, 
                                  int ichild, int *status ){
/*
*  Name:
*     ndg1ParentChildIndex

*  Purpose:
*     Create a parent-child link between two Prov structures with given
*     indices.

*  Invocation:
*     void ndg1ParentChildIndex( Provenance *provenance, int iparent, 
*                                int ichild, int *status )

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
   ndg1ParentChild( parent, child, status );
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
   Prov *better;
   Prov *child;
   Prov *poorer;
   Prov *prov1;
   Prov *prov2;
   int done;
   int i;
   int ichild;
   int j;
   int keep;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return;

/* Loop through every Prov structure in the supplied Provenance. */
   for( i = 0; i < provenance->nprov; i++ ) {
      prov1 = provenance->provs[ i ];
      if( prov1 ) {

/* Check all the remaining Prov structures to see if any of them refer to
   the same NDF. */
         for( j = i + 1; j < provenance->nprov; j++ ) {
            prov2 = provenance->provs[ j ];
            if( prov2 ) {
               keep = ndg1TheSame( prov1, prov2, status );
               if( keep ) {

/* If the two provenance structures refer to the same NDF, break the
   parent-child link for the poorer Prov and register the better Prov with
   the parent in place of the poorer Prov. The better Prov is the one
   that contains most information. */
                  if( keep == 1 ) {
                     better = prov1;
                     poorer = prov2;
                  } else {
                     better = prov2;
                     poorer = prov1;
                  }
   
                  for( ichild = 0; ichild < poorer->nchild; ichild++ ) {
                     child = poorer->children[ ichild ];
                     ndg1Disown( poorer, child, status );
                     ndg1ParentChild( better, child, status );
                  }
               }
            }
         }
      }
   }

/* Now look for Prov structures that have no children, and remove them. Do
   this repeatedly to go down the chain of parent-child links to the
   roots. */
   done = 0;
   while( !done) {

/* Assume there are no childless entries in the list of Prov structures. */
      done = 1;

/* Loop round all non-NULL entries in the list of Prov structures. */
      for( i = 0; i < provenance->nprov; i++ ) {
         prov1 = provenance->provs[ i ];
         if( prov1 ) {

/* Is this Prov structure without any children? If so, it is not used in
   the creation of any of the other NDFs, so we can remove it, so long as
   it is not the main NDF. */
            if( prov1 != provenance->main && prov1->nchild == 0 ) {

/* First, check all remaining Provs to see if the Prov that we are about
   to remove is a child. If so, break the parent child relationship. */
               for( j = 0; j < provenance->nprov; j++ ) {
                  ndg1Disown( provenance->provs[ j ], prov1, status );
               }

/* Now free the resources used by the Prov. */
               provenance->provs[ i ] = ndg1FreeProv( prov1, status );

/* The above call to ndg1Disown may have produced a new childless Prov
   structure, so indicate we need to check again. */
               done = 0;
            }
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

static Provenance *ndg1ReadProvenanceExtension( int indf, HDSLoc *more, 
                                                const char *creator,
                                                int isroot, int *status ){
/*
*  Name:
*     ndg1ReadProvenanceExtension

*  Purpose:
*     Create a new Provenance structure from an NDF PROVENANCE extension.

*  Invocation:
*     Provenance *ndg1ReadProvenanceExtension( int indf, HDSLoc *more, 
*                                              const char *creator,
*                                              int isroot, int *status )

*  Description:
*     This function allocates dynamic memory to hold a new Provenance
*     structure, and copies provenance information from the PROVENANCE
*     extension of the supplied NDF into the new Provenance structure.
*     If the NDF does not have a PROVENANCE extension, then the returned
*     Provenance structure contains only a single Prov structure, for the 
*     supplied NDF itself. The only item stored in this Prov structure is
*     the NDF path.

*  Arguments:
*     indf
*        The NDF identifier.
*     more
*        An optional HDS structure holding additional information about
*        the NDF. This is stored in the main Prov structure in the
*        returned Provenance.
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
   HDSLoc *aloc = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *mloc = NULL;
   HDSLoc *ploc = NULL;
   HDSLoc *xloc = NULL;
   Prov *anc_prov = NULL;
   Prov *main_prov = NULL;
   Provenance *result = NULL;
   char *date = NULL;
   char *path = NULL;
   char creator_buf[ CREATOR_LEN + 1 ];     
   char date_buf[ DATE_LEN + 1 ];     
   char path_buf[ PATH_LEN + 1 ];     
   hdsdim  dim[ 1 ];
   int path_len;
   int there;
   size_t nanc;
   size_t npar;
   int i;
   int j;
   int *parents = NULL;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* See if the supplied NDF has a provenance extension. If it has, get a 
   HDS locator to it. */
   ndfXstat( indf, EXT_NAME, &there, status ); 
   if( there && !isroot ) {
      ndfXloc( indf, EXT_NAME, "Read", &xloc, status ); 

/* Get the values of the DATE and CREATOR components, if they exist. */
      date = ndg1GetTextComp( xloc, DATE_NAME, date_buf, 
                              DATE_LEN, status );
      creator = ndg1GetTextComp( xloc, CREATOR_NAME, creator_buf,
                                 CREATOR_LEN, status );

/* Get a locator for the ANCESTORS array, and get its length. */
      aloc = ndg1GtAnc( xloc, &nanc, status );
   }

/* Get the path to the supplied NDF. */
   ndfMsg( "NDF", indf ); 
   msgLoad( " ", "^NDF", path_buf, PATH_LEN, &path_len, status );
   path = path_buf;

/* Create a Prov structure to describe the main NDF. */
   main_prov = ndg1MakeProv( path, date, creator, more, NULL, status );

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

/* Get the PATH, DATE and CREATOR components. */
         date = ndg1GetTextComp( cloc, DATE_NAME, date_buf, 
                                 DATE_LEN, status );
         path = ndg1GetTextComp( cloc, PATH_NAME, path_buf,
                                 PATH_LEN, status );
         creator = ndg1GetTextComp( cloc, CREATOR_NAME, creator_buf,
                                 CREATOR_LEN, status );
/* If the ancestor has a MORE structure, get a locator for it. */
         datThere( cloc, MORE_NAME, &there, status );
         if( there ) {
            datFind( cloc, MORE_NAME, &mloc, status );
         } else {
            mloc = NULL;
         }

/* Create a Prov structure to describe the current ancestor, and add it
   into the returned Provenance structure. */
         anc_prov = ndg1MakeProv( path, date, creator, mloc, 
                                  result, status );
/* Free locators. */
         if( mloc ) datAnnul( &mloc, status );
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
   directly without needing to cvhange from one-based to zero-based. */
         for( i = 0; i < npar; i++ ) {
            ndg1ParentChildIndex( result, parents[ i ], 0, status );
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
               ndg1ParentChildIndex( result, parents[ j ], i, status );
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
   if( xloc ) datAnnul( &xloc, status );

/* If an error occurred, free the result. */
   if( !astOK ) result = ndg1FreeProvenance( result, 1, status );

/* Return the result */
   return result;
}

static HDSLoc *ndg1TCopy( HDSLoc *loc, const char *name, int *status ){
/*
*  Name:
*     ndg1TCopy

*  Purpose:
*     Create a temporary copy of an HDS object.

*  Invocation:
*     HDSLoc *ndg1TCopy( HDSLoc *loc, const char *name, int *status )

*  Description:
*     This routine creates a new temporary HDS object that is a copy of
*     a specified component of the supplied structure.

*  Arguments:
*     loc
*        locator for the HDS structure containing the object to copy. 
*     name 
*        The name of the component to copy. It must be scalar (i.e. not an
*        array). If the supplied structure does not contain a component with
*        the requested name, a NULL pointer is returned without error.
*     status 
*        The global status.

*  Returned Value:
*     A pointer to a locator for a new temporary HDS object which is a
*     copy of the specified HDS object.

*/

/* Local variables: */
   HDSLoc *result = NULL;
   HDSLoc *cloc = NULL;
   char type[ DAT__SZNAM + 1 ];
   int there;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Check that the required component exists within the supplied object. */
   datThere( loc, name, &there, status );
   if( there ) {

/* Get a locator for the component to be copied. */
      datFind( loc, name, &cloc, status );

/* Create a temporary HDS object with arbitrary name and type. */
      datTemp( "TEMP", 0, NULL, &result, status );

/* Give it the name and type of the supplied object */
      datRenam( result, name, status );
      datType( cloc, type, status );
      datRetyp( result, type, status );

/* Copy all the components inside the component into "result". */
      ndg1CopyComps( cloc, result, status );

/* Annull the component locator. */
      datAnnul( &cloc, status );
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
*     describe the same NDF.

*  Arguments:
*     prov1
*        The first Prov structure.
*     prov2
*        The second Prov structure.
*     status
*        Pointer to the inherited status variable.

* Returned Value:
*   Non-zero if the two Prov structures describe the same NDF, and zero
*   otherwise. If the two Prov structures describe the same NDF, then the
*   returned value will be +1 if "prov1" contains more information than
*   "prov2" and will be +2 otherwise.

*/

/* Local Variables: */
   int result;
   int nitem1;
   int nitem2;

/* Initialise. */
   result = 0;

/* Check the inherited status value. */
   if( *status != SAI__OK ) return result;

/* If the pointer are the same, they describe the same ND. */
   if( prov1 == prov2 ) {
      result = 2;

/* Otherwise, we assume a match if the NDF path and (if known) the date 
   of the provenance information are the same. */
   } else {
      result = 1;

      if( prov1->path ) {
         if( prov2->path ) {
            if( strcmp( prov1->path, prov2->path ) ) result = 0;
         } else {
            result = 0;
         }
      } else if( prov2->path ) {
         result = 0;
      }

      if( prov1->date && prov2->date ) {
         if( strcmp( prov1->date, prov2->date ) ) result = 0;
      }

/* If they refer to the same NDF, count the number of items of information 
   stored for "prov1" and "prov2". */
      if( result ) {
         nitem1 = 0;
         if( prov1->date ) nitem1++;
         if( prov1->creator ) nitem1++;
         if( prov1->more ) nitem1++;
         if( prov1->nparent ) nitem1++;

         nitem2 = 0;
         if( prov2->date ) nitem2++;
         if( prov2->creator ) nitem2++;
         if( prov2->more ) nitem2++;
         if( prov2->nparent ) nitem2++;

/* Return a non-zero result indicating which of the two Prov structures 
   contains the most information. */
         result = ( nitem1 > nitem2 ) ? 1 : 2;

      }
   }

/* Return the result. */
   return result;
}


static void ndg1WriteProvenanceExtension( Provenance *provenance, int indf, 
                                          int *status ){
/*
*  Name:
*     ndg1WriteProvenanceExtension

*  Purpose:
*     Create a new NDF PROVENANCE extension from a Provenance structure.

*  Invocation:
*     void ndg1WriteProvenanceExtension( Provenance *provenance, int indf, 
*                                        int *status )

*  Description:
*     This function erases any existing PROVENANCE extension within the
*     supplied NDF, and creates a new one holding the information in the
*     supplied Provenance structure.

*  Arguments:
*     provenance
*        The Provenance structure.
*     indf
*        The NDF identifier.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   HDSLoc *aloc = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *loc = NULL;
   HDSLoc *more = NULL;
   HDSLoc *mloc = NULL;
   HDSLoc *xloc = NULL;
   Prov *prov = NULL;
   const char *date = NULL;
   char *path = NULL;
   hdsdim  dim[ 1 ];
   int *parents = NULL;
   int i;
   int k;
   int len;
   int there;
   time_t t;

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

/* Create an ANCESTORS array of the correct length (exclude the main NDF
   Prov structure), and get a locator for it. */
      if( provenance->nprov > 1 ) {
         dim[ 0 ] = provenance->nprov - 1;
         datNew( xloc, ANCESTORS_NAME, ANCESTORS_TYPE,  1, dim, status );
         datFind( xloc, ANCESTORS_NAME, &aloc, status );
      }

/* Loop round all the Prov structures. */
      for( i = 0; i < provenance->nprov; i++ ) {
         prov = provenance->provs[ i ];
      
/* Get a locator to the place where this Prov structure should be
   written. The Prov structure for the main NDF is written to the 
   PROVENANCE extension itself. Other Prov structures are written
   to an element of the ANCESTORS array. */
         if( prov == provenance->main ) {
            datClone( xloc, &cloc, status );
         } else {
            dim[ 0 ] = i;
            datCell( aloc, 1, dim, &cloc, status );
         }

/* If this is not the main NDF provenance, use the PATH and DATE components
   from the Prov structure. */
         if( prov != provenance->main ) {
            path = prov->path;
            date = prov->date;
            more = prov->more;

/* If it is the main NDF provenance, do not write out the PATH (since the
   NDF may subsequently be moved), and force the DATE to the current
   date. Also no "more" information is stored for the main NDF since the
   information that may be stored in "more" will usually be available at 
   other places in the NDF. It is the responsibilty of each application
   to extract the information it thinks is relevant from each input NDF and
   store it in the "more" structure with the input NDF's provenance
   information. */
         } else {
            path = NULL;
            time( &t );
            date = ndg1Date( status );
            more = NULL;
         }

/* Store the path. */
         if( path ) {
            len = astChrLen( path );
            if( len ) {
               path[ len ] = 0;
               datNew0C( cloc, PATH_NAME, len, status );
               datFind( cloc, PATH_NAME, &loc, status );
               datPut0C( loc, path, status );
               datAnnul( &loc, status );
            }
         }

/* Store the date. */
         if( date ) {
            len = astChrLen( date );
            if( len ) {
               datNew0C( cloc, DATE_NAME, len, status );
               datFind( cloc, DATE_NAME, &loc, status );
               datPut0C( loc, date, status );
               datAnnul( &loc, status );
            }
         }

/* Store the creator (the same code for both main and ancestor NDFs). */
         if( prov->creator ) {
            len = astChrLen( prov->creator );
            if( len ) {
               prov->creator[ len ] = 0;
               datNew0C( cloc, CREATOR_NAME, len, status );
               datFind( cloc, CREATOR_NAME, &loc, status );
               datPut0C( loc, prov->creator, status );
               datAnnul( &loc, status );
            }
         }

/* Store a deep copy of "more". Note, the "more" variable is a locator
   for a temporary structure that contains the required MORE struture as
   its one and only component. It is not a locator for the MORE structure
   itself. */
         if( more ) {
            datFind( more, MORE_NAME, &mloc, status );
            datCopy( mloc, cloc, MORE_NAME, status );
            datAnnul( &mloc, status );
         }

/* Create a PARENTS array of the correct length, and map it. */
         if( prov->nparent > 0 ) {
            datNew1I( cloc, PARENTS_NAME, prov->nparent, status );
            datFind( cloc, PARENTS_NAME, &loc, status );
            dim[ 0 ] = prov->nparent;
            datMapI( loc, "WRITE", 1, dim, &parents, status );

/* Loop round each cell of this array (i.e. each parent). */
            for( k = 0; k < prov->nparent && *status == SAI__OK; k++ ) {

/* Find and store the index of this parent in the ANCESTORS array. */
               parents[ k ] = ndg1FindAncestorIndex( prov->parents[ k ], 
                                                     provenance, status );
            }

/* Free the parents array. */
            datUnmap( loc, status );
            datAnnul( &loc, status );
         }

/* Free the locator to the Prov structure. */
         datAnnul( &cloc, status );
      } 

/* Free resources */
      if( aloc ) datAnnul( &aloc, status );
      datAnnul( &xloc, status );
   }

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

































