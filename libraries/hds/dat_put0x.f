*+ DAT_PUT0C - Write scalar object value
      subroutine dat_put0C(loc, value, status)
*    Description :
*     This routine writes a value into a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT0D    DOUBLE PRECISION
*        DAT_PUT0R    REAL
*        DAT_PUT0I    INTEGER
*        DAT_PUT0L    LOGICAL
*        DAT_PUT0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_PUT0C(LOC, VALUE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=CHARACTER*(*)
*           Expression specifying the value to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_PUTC directly.
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
*    Import :
      character*(*) loc			! Object Locator
      CHARACTER*(*) value			! Scalar to supply value
*    Status return :
      integer status			! Status Return
*    Local variables :
*     integer actval			! number of values in object
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_putC(loc, ndim, dims, value, status)
      endif

      end


*+ DAT_PUT0D - Write scalar object value
      subroutine dat_put0D(loc, value, status)
*    Description :
*     This routine writes a value into a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT0D    DOUBLE PRECISION
*        DAT_PUT0R    REAL
*        DAT_PUT0I    INTEGER
*        DAT_PUT0L    LOGICAL
*        DAT_PUT0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_PUT0D(LOC, VALUE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=DOUBLE PRECISION
*           Expression specifying the value to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_PUTD directly.
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
*    Import :
      character*(*) loc			! Object Locator
      DOUBLE PRECISION value			! Scalar to supply value
*    Status return :
      integer status			! Status Return
*    Local variables :
*     integer actval			! number of values in object
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_putD(loc, ndim, dims, value, status)
      endif

      end


*+ DAT_PUT0I - Write scalar object value
      subroutine dat_put0I(loc, value, status)
*    Description :
*     This routine writes a value into a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT0D    DOUBLE PRECISION
*        DAT_PUT0R    REAL
*        DAT_PUT0I    INTEGER
*        DAT_PUT0L    LOGICAL
*        DAT_PUT0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_PUT0I(LOC, VALUE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=INTEGER
*           Expression specifying the value to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_PUTI directly.
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
*    Import :
      character*(*) loc			! Object Locator
      INTEGER value			! Scalar to supply value
*    Status return :
      integer status			! Status Return
*    Local variables :
*     integer actval			! number of values in object
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_putI(loc, ndim, dims, value, status)
      endif

      end


*+ DAT_PUT0L - Write scalar object value
      subroutine dat_put0L(loc, value, status)
*    Description :
*     This routine writes a value into a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT0D    DOUBLE PRECISION
*        DAT_PUT0R    REAL
*        DAT_PUT0I    INTEGER
*        DAT_PUT0L    LOGICAL
*        DAT_PUT0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_PUT0L(LOC, VALUE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=LOGICAL
*           Expression specifying the value to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_PUTL directly.
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
*    Import :
      character*(*) loc			! Object Locator
      LOGICAL value			! Scalar to supply value
*    Status return :
      integer status			! Status Return
*    Local variables :
*     integer actval			! number of values in object
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_putL(loc, ndim, dims, value, status)
      endif

      end


*+ DAT_PUT0R - Write scalar object value
      subroutine dat_put0R(loc, value, status)
*    Description :
*     This routine writes a value into a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT0D    DOUBLE PRECISION
*        DAT_PUT0R    REAL
*        DAT_PUT0I    INTEGER
*        DAT_PUT0L    LOGICAL
*        DAT_PUT0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_PUT0R(LOC, VALUE; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=REAL
*           Expression specifying the value to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_PUTR directly.
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
*    Import :
      character*(*) loc			! Object Locator
      REAL value			! Scalar to supply value
*    Status return :
      integer status			! Status Return
*    Local variables :
*     integer actval			! number of values in object
      integer ndim
      integer dims(1)
*    Local data :
      data ndim/0/
      data dims/0/	! Note this must be done to be Standard !
*-

      if (status .eq. SAI__OK) then
         call dat_putR(loc, ndim, dims, value, status)
      endif

      end


