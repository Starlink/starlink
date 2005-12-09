*+ DAT_GET1C - Read vector object values
      subroutine dat_get1C(loc, maxval, values, actval, status)
*    Description :
*     This routine reads the values from a vector primitive object.
*     There is a routine for each access type,
*
*        DAT_GET1D    DOUBLE PRECISION
*        DAT_GET1R    REAL
*        DAT_GET1I    INTEGER
*        DAT_GET1L    LOGICAL
*        DAT_GET1C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL DAT_GET1C(LOC, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that can
*           be held in the array, VALUES.
*     VALUES(MAXVAL)=CHARACTER*(*)
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*           Variable to receive the actual number of values read.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_SIZE and DAT_GET.
*     Be careful to conform to Fortran 77 standard, with regard to passing
*     arrays to subprograms.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
*    Import :
      character*(*) loc			! Object Locator
      integer maxval			! Maximum number of values
*    Export :
      CHARACTER*(*) values(*)			! Array to receive values
      integer actval			! Actual number of values read
*    Status return :
      integer status			! Status Return
*    Local variables :
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/1/
*-

      if (status .eq. SAI__OK) then
         call dat_size(loc, actval, status)
         if (maxval .lt. actval) then
            status = DAT__BOUND
         else
            dims(1) = actval
            call dat_getC(loc, ndim, dims(1), values, status)
         endif
      endif

      end
