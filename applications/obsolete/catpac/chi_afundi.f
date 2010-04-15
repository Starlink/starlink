*+  ADC_XFUNDI - Evaluate a numeric function
      subroutine chi_afundi(fnumber, nargs, arg, value, status)
*    Description :
*     Performs an evaluation of a numeric function to an INTEGER result.
*    Invocation
*     CALL ADC_XFUNDI(FNUMBER, NARGS, ARG, VALUE, STATUS)
*    Parameters :
*     FNUMBER=INTEGER(INPUT)
*           The number of the function
*     NARGS=INTEGER(INPUT)
*           The argument count
*     ARG=DOUBLE(*)(INPUT)
*           The array of argument list values
*     VALUE=INTEGER(OUTPUT)
*           Value of the expression
*     STATUS=INTEGER(UPDATE)
*           Status variable
*    Method :
*     Go to the function specified
*     If another function value is calculated then
*        Store it.
*     Endif
*     Note : This routine assumes that functions are always
*            evaluated in the same order.  This halves the time
*            for the calculation of both longitude and latitude
*            coordinates.
*    Authors :
*     Jon Fairclough (RAL::IPMAF)
*    History :
*     3-Feb-1985: Original
*    20-Apr-1985: ADIFF and GRCIRC functions added. LOG10/LOG reversed
*    17-May-1985: SUPGAL and GALSUP routines added
*    27-Nov-1986: renamed from ADC_CALC1 distinguishing from ADC_XFUNDD
*    Type Definitions :
      implicit none
*    Global constants :
      include 'sae_par'                 ! SAI Symbolic Constants
      include 'CHI_PAR'
      include 'CHIPAR_PAR'
      include 'CHIPAR1_PAR'
      include 'CHIPAR_ERR'
*    Import
      integer fnumber
      integer nargs
      double precision arg(*)                ! Values
*    Export
      integer value
*    Status :
      integer status
*    Global variables
      include 'CHIWRK_CMN'
*    External references
      integer iras_temp12
      integer iras_temp23
      integer iras_temp34
*    Local
      integer i
      logical error
*-
*    Begin
*
*
      if (status .ne. SAI__OK) then
         return
      endif
*
      if (fnumber .lt. 1 .or. fnumber .gt. CHI__MXFUN) then
          status = CHI__NTSUP
          goto 100
      endif
*
*   Branch to function required
*
      i = fnumber - 43
      if (i.le.0) then
         status = CHI__NTSUP
         goto 100
      endif
*
      goto (44, 45, 46, 47, 48), i
*
      status = CHI__NTSUP
      goto 100
*
   44 continue
*     value = iras_temp23(arg(1),arg(2))
      goto 100
*
   45 continue
*     value = iras_temp12(arg(1),arg(2))
      goto 100
*
   46 continue
*     {value = iras_temp34(arg(1),arg(2))
      goto 100
*
   47 value = int  (arg(1))
      goto 100
*
   48 value = nint (arg(1))
      goto 100
*
  100 continue
*
*    Check arithmetic error
*
      end
