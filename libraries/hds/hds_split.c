#include "hds.h"
#include "f77.h"

F77_SUBROUTINE( hds_split )( CHARACTER( NAME ),
                             INTEGER( F1 ),
                             INTEGER( F2 ),
                             INTEGER( P1 ),
                             INTEGER( P2 ),
                             INTEGER( STATUS )
                             TRAIL( NAME ) ){
/*
*+
*  Name:
*     HDS_SPLIT

*  Purpose:
*     Split an HDS object name into a file name and a path name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDS_SPLIT( NAME, F1, F2, P1, P2, STATUS )

*  Description:
*     This routine analyses a general HDS object name and locates the
*     substrings which specify the container file name and the path
*     name of the object within the file.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        HDS object name to be analysed.
*     F1 = INTEGER (Returned)
*        Character position of the start of the file name.
*     F2 = INTEGER (Returned)
*        Character position of the end of the file name.
*     P1 = INTEGER (Returned)
*        Character position of the start of the path name.
*     P2 = INTEGER (Returned)
*        Character position of the end of the path name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the routine succeeds, then F1 and F2 will always identify
*     the container file name.
*     -  If the object describes a top-level object, then there will be
*     no path name. In this case, P2 will be returned greater than P1.
*     Otherwise, P1 and P2 will identify the path name.
*     -  This routine performs some checks on the validity of the
*     object name supplied, but these are not comprehensive. Only an
*     attempt to locate the object will fully validate the name.
*     -  Any blank characters which surround the file or path names
*     will be excluded from the returned character string positions.
*     - If the string begins with a quote, it is assumed that the name is
*     quoted and the component part follows. eg "test.sdfx".MORE will
*     result in a filename called test.sdfx and a .MORE component.
*     - If the first component after the root is ".sdf" this will be absorbed
*     into the filename (which HDS can open without problem) unless there
*     is a component at the top level of the HDS file called "SDF". If the
*     file can not be opened by HDS the ".sdf" will be assumed to be part of
*     the filename. This approach is not full proof since HDS_SPLIT is not
*     always called with a full path to a valid file. In generaly the best place
*     for disambiguating would be the caller but this routine is used in places
*     other than HDS_FIND so it is better to absorb the overhead. The HDS open
*     will only occur for the .sdf case. The earlier note comments that some
*     validation occurs but not all, this is probably at odds with that sentiment.

*  Machine-specific features used:
*     This routine unavoidably has to make assumptions about the format
*     of VAX/VMS and POSIX file names.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     28-MAY-2018 (DSB):
*        Original version of the C wrapper. The original F77 code written
*        by RFWS et al has been ported to C (hdsSplit.c).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Create a nullterminated copy of the supplied name. */
   char *name = cnfCreim( NAME, NAME_length );

/* Call the C function. */
   size_t f1, f2, p1, p2;
   hdsSplit( name, &f1, &f2, &p1, &p2, STATUS );

/* Free te null-terminated buffer. */
   cnfFree( name );

/* Convert returned indices from zero-based to one-based. */
   *F1 = ++f1;
   *F2 = ++f2;
   *P1 = ++p1;
   *P2 = ++p2;

}
