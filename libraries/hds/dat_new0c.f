*+ DAT_NEW0C - Create new scalar string component, with specified precision
      subroutine dat_new0c(struct, comp, size, status)
*    Description :
*     Create scalar structure component of type '_CHAR*<n>', with the
*     string precision, <n>, specified by the LEN parameter.
*    Invocation :
*     CALL DAT_NEW0C(LOC, NAME, LEN; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     LEN=INTEGER
*           Expression specifying the number of characters per value.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Construct the string "_CHAR*SIZE" and use DAT_NEW0.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1982:  Original. (UCL::SLW)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer size			! String size
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZTYP) type	! Component TYPE string
*-

      if (status .eq. SAI__OK) then

*       Construct the type string
         call dat_cctyp(size, type)
*       Create the new variable
         call dat_new0(struct, comp, type, status)

      endif

      end
