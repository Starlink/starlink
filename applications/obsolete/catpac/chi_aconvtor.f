*+ CHI_ACONVTOR - Convert the value of an element to a real.
      subroutine chi_aconvtor(ed, rvalue, status)
*    Description :
*     Convert the value of an element to a real.
*
*    Invocation :
*     CALL CHI_ACONVTOR(ED, RVALUE, STATUS)
*    Parameters :
*     ED=INTEGER(INPUT)
*           Variable containing pointer into the common block.
*     RVALUE=REAL(OUTPUT)
*           Variable to receive the value in the field.
*     STATUS=INTEGER(UPDATE)
*           Variable holding the status value.   If this variable
*           is not SAI__OK on input, then the routine will return
*           without action.   If the routine fails to complete,
*           this variable will be set to an appropriate error
*           number.
*    Method :
*    Authors :
*     Alan Wood (STADAT::ARW) Sid Wright  (UCL::SLW)
*     Jon Fairclough (RAL::IPMAF)
*    History :
*     14-Feb-1992: Original.
*    Global constants :
      include 'sae_par'			! SAI Constants
      include 'chi_par'
      include 'chipar_par'
*    Import :
      integer ed			! element descriptor
*    Export :
      real rvalue
*    Status return :
      integer status			! Status Return
*    Global variables :
      include 'chiwrk_cmn'
*    Variables :
*    Local constant
*-
*
      if (status .ne. SAI__OK) return
*
*    Get the field value according to its stored type and convert
*    to the required type.
*
         if (Etype(ed).eq.D_type) then
             rvalue = doubvals(ed)
         elseif (Etype(ed).eq.I_type) then
             rvalue = float(intvals(ed))
         else
             rvalue = realvals(ed)
         endif
      end
