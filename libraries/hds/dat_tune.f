*+ DAT_TUNE - suggest allocation quantities to DAT
      subroutine dat_tune(name, value, status)
*    Description :
*     This routine suggests a value for DAT allocation quantities.
*     The following are currently allowed :
*
*        NCOMP - suggest number of components to be used in structure.
*
*    Invocation :
*     CALL DAT_TUNE(NAME, VALUE; STATUS)
*    Parameters :
*     NAME=CHARACTER*(DAT__SZNAM)
*           Variable containing the name of the allocation quantity.
*     VALUE=INTEGER
*           Suggested value for the allocation quantity.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     If allocation quantity is valid for users, call HDS_TUNE.
*    Authors :
*     Sid Wright  (UCL::SLW)
*    History :
*     12-Jan-1984:  Original.  (UCL::SLW)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) name		! allocation quantity
      integer value			! suggested value of quantity
*    Export :
*    Status return :
      integer status			! Status Return
*    External References :
      logical chr_simlr
*    Local variables :
*-

      if (status .ne. SAI__OK) then
         return
      endif

      if (chr_simlr(name, 'NCOMP')) then
         call hds_tune(name, value, status)
      endif

      end
