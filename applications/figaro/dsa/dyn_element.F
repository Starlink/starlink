#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
C                          D Y N _ E L E M E N T
C
C  Routine name:
C     DYN_ELEMENT
C
C  Function:
C     Returns the dynamic memory array element corresponding to an
C     address.
C
C  Description:
C     Given a particular virtual memory address, this routine returns
C     the element number of the conceptual array DYNAMIC_MEM that can be
C     used to reference it.
C     This routine uses the function %LOC, which fortunately exists on
C     VAX, DECstation, SparcStation, and Convex.
C
C  Language:
C     FORTRAN
C
C  Call:
C     ELEMENT = DYN_ELEMENT (ADDRESS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ADDRESS     (Integer,ref) The memory address in question.
C
C  Returns:
C
C     (<) ELEMENT     (Integer,function value) The conceptual dynamic
C                     memory element corresponding to ADDRESS.
C
C-
C  History:
C     21st July 1987.    Original version.  KS / AAO.
C     27th June 1992.    Port to Unix. HME / UoE, Starlink.
C+
      INTEGER FUNCTION DYN_ELEMENT (ADDRESS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ADDRESS
      INTEGER*8 DLOC
C
C     Dynamic memory system common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'CNF_PAR'
C
#if HAVE_PERCENTLOC
      DLOC = %LOC(DYNAMIC_MEM)
#elif HAVE_INTRINSIC_LOC
      DLOC = LOC(DYNAMIC_MEM)
#else
      Do not have LOC or %LOC
#endif
      DYN_ELEMENT = CNF_PVAL(ADDRESS) - DLOC + 1
C
      END
