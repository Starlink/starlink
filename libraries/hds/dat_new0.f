*+ DAT_NEW0 - Create new scalar component
      subroutine dat_new0(struct, comp, type, status)
*    Description :
*     Create a scalar structure component with specified type.
*    Invocation :
*     CALL DAT_NEW0(LOC, NAME, TYPE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     TYPE=CHARACTER*(*)
*           Expression specifying the type of the component.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW.
*     Be careful to conform to Fortran 77 standard, with regard to passing
*     arrays to subprograms.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1983:  Original.  (UCL::SLW)
*     05.11.1984:   Remove calls to error system. (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      character*(*) type		! component type
*    Status return :
      integer status			! Status Return
*    Local variables :
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_new(struct, comp, type, ndim, dims, status)
      endif

      end


