*+ DAT_RCOPY - recursive copy
      subroutine dat_rcopy(sloc, dloc, cname, status)
*    Description :
*     This routine is obsolete. Its function is now performed  by
*     DAT_COPY.
*     The routine recursively copies the data structure pointed to by
*     the source locator into the data structure pointed to by the
*     destination locator with the given component name.
*    Invocation :
*     CALL DAT_RCOPY(SLOC, DLOC, CNAME; STATUS)
*    Parameters :
*     SLOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with the source
*           object.
*     DLOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with the data
*           structure to which the object is to be copied.
*     CNAME=CHARACTER*(DAT__SZNAM)
*           Variable containing the name of the resulting object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Call DAT_COPY modified to do recursive copy.
*    Authors :
*     Sid Wright  (UCL::SLW)
*     A Chipperfield  (RAL::AJC)
*    History :
*     29-Mar-1983:  Original.  (UCL::SLW)
*     27-Sep-1984:  Generalised to accept structure arrays.  (RAL::MDL)
*     09.04.1985:   Remove calls to ERR and rename DAT_$CCOPY 
*                                (REVAD::BDK)
*     27.02.1987:   Handle single element structure arrays
*                   and add comments  (RLVAD::AJC)
*     18.03.1987:   Straight through call DAT_COPY for HDS_C_3_4 (RLVAD::AJC)
*     15.04.1987:   Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) sloc		! Source Locator
      character*(*) dloc		! Environment Locator
      character*(*) cname		! component to be created
*    Export :
*    Status return :
      integer status			! Status Return
*    Local variables :
*-

      if (status .ne. SAI__OK) return

      call dat_copy(sloc, dloc, cname, status)

      end
