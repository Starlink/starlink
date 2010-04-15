#if HAVE_CONFIG_H
#  include "config.h"
#endif
      INTEGER FUNCTION DYN_ELEMENT( ADDRESS )
*+
*  Name:
*     DYN_ELEMENT

*  Purpose:
*     Dynamic memory array element corresponding to an address.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = DYN_ELEMENT( ADDRESS )

*  Description:
*     Given a particular virtual memory address, this routine returns
*     the element number of the conceptual array DYNAMIC_MEM that can be
*     used to reference it. This routine uses the function %LOC.

*  Arguments:
*     ADDRESS = INTEGER (Given)
*        The memory address in question.

*  Returned Value:
*     DYN_ELEMENT = INTEGER
*        The conceptual dynamic memory element corresponding to ADDRESS.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     21 Jul 1987 (ks):
*        Original version.
*     27 Jun 1992 (hme):
*        Port to Unix.
*     30 Jul 1996 (mjcl):
*        Added DLOC variable as part of port to Linux.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'CNF_PAR'

*  Arguments Given:
      INTEGER ADDRESS

*  Local Variables:
      INTEGER*8 DLOC
*.

#if HAVE_PERCENTLOC
      DLOC = %LOC(DYNAMIC_MEM)
#elif HAVE_INTRINSIC_LOC
      DLOC = LOC(DYNAMIC_MEM)
#else
      Do not have LOC or %LOC
#endif
      DYN_ELEMENT = CNF_PVAL(ADDRESS) - DLOC + 1

      END
