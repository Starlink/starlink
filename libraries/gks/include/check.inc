*  -------------
*  BEGIN INCLUDE    CHECK.INC
*  -------------

*  CHANGE LOG
*     06/04/87  RMK   Prime version - added IMPLICIT COMPLEX.
*     06/07/87  PJWR  4.2 BSD / System V UNIX version - changed IMPLICIT
*                     COMPLEX to IMPLICIT UNDEFINED and modified  layout
*                     to match other INCLUDE files.
*     09/07/87  PJWR  Removed the typecheck - Pyramid FORTRAN 77 4.0.1
*                     compiler gets confused and aborts.
*     17/04/89  PLP   Re-introduced the type-checking.
*     22/05/87  PLP   Added conditional code to eliminate type-checking on
*                     Pyramid. The 'confusion' PJWR mentions above occurs
*                     in some of the files which have references to external
*                     subroutines. Compiler returns an obscure error message
*                     which gives nothing to go on.
*     22/05/92  CDS   Changed pre-processor bits to be dependant
*                     on $(SYSTEM) name from Makefile and added
*                     bits for all supported systems.
*     06/04/95  BKM   Added Linux ifdef
*     15/06/04  TIMJ  Integrate into autoconf system
*                     - Remove OS variants
*
*  Notes:
*     Original version of this file had these variants:
*       No type checking for pyr
*       IMPLICIT UNDEFINED (A-Z)  [sun4/sun4]
*       IMPLICIT LOGICAL (A-Z)    [iris3130]
*       IMPLICIT NONE             [everything else]
*
*    All current compilers support IMPLICIT NONE so use it
*
*  ---------------------------------
*   RAL GKS 4.2 BSD / System V UNIX
*  ---------------------------------

       IMPLICIT NONE

*  -----------
*  END INCLUDE
*  -----------
