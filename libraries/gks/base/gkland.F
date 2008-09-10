#include "config.h"

      INTEGER FUNCTION GKLAND(IOP1, IOP2)
*
* (C) COPYRIGHT ICL & SERC  1990
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    System Interface Routine
*  Author:             PLP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs the logical AND operation.
*
*  MAINTENANCE LOG
*  ---------------
*     30/10/90  PLP  Original Sun version stabilized.
*     17/06/04  TIMJ Autoconf version
*
*  ARGUMENTS
*  ---------
*     INP   IOP1    First integer operand.
*     INP   IOP2    Second integer operand.
*
      INTEGER IOP1, IOP2
*
*  FUNCTION RETURN VALUE
*  ---------------------
*
*     Integer result of the AND operation.
*
*  COMMENTS
*  --------
*
*     This routine is SYSTEM DEPENDENT.
*
*-----------------------------------------------------------------------
#if HAVE_INTRINSIC_AND || HAVE_AND
      GKLAND = AND(IOP1, IOP2)
#elif HAVE_INTRINSIC_IAND || HAVE_IAND
      GKLAND = IAND(IOP1, IOP2)
#else
 error 'Unable to implement logical AND'
#endif
      END
