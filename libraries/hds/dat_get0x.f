*+ DAT_GET0C - Read scalar object value
      subroutine dat_get0C(loc, value, status)
*    Description :
*     This routine reads the value from a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_GET0D    DOUBLE PRECISION
*        DAT_GET0R    REAL
*        DAT_GET0I    INTEGER
*        DAT_GET0L    LOGICAL
*        DAT_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_GET0C(LOC; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=CHARACTER*(*)
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_GETC directly.
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
*    Export :
      CHARACTER*(*) value			! Scalar to receive value
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
         call dat_getC(loc, ndim, dims, value, status)
      endif

      end
*+ DAT_GET0D - Read scalar object value
      subroutine dat_get0D(loc, value, status)
*    Description :
*     This routine reads the value from a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_GET0D    DOUBLE PRECISION
*        DAT_GET0R    REAL
*        DAT_GET0I    INTEGER
*        DAT_GET0L    LOGICAL
*        DAT_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_GET0D(LOC; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=DOUBLE PRECISION
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_GETD directly.
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
*    Export :
      DOUBLE PRECISION value			! Scalar to receive value
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
         call dat_getD(loc, ndim, dims, value, status)
      endif

      end
*+ DAT_GET0I - Read scalar object value
      subroutine dat_get0I(loc, value, status)
*    Description :
*     This routine reads the value from a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_GET0D    DOUBLE PRECISION
*        DAT_GET0R    REAL
*        DAT_GET0I    INTEGER
*        DAT_GET0L    LOGICAL
*        DAT_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_GET0I(LOC; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=INTEGER
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_GETI directly.
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
*    Export :
      INTEGER value			! Scalar to receive value
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
         call dat_getI(loc, ndim, dims, value, status)
      endif

      end
*+ DAT_GET0L - Read scalar object value
      subroutine dat_get0L(loc, value, status)
*    Description :
*     This routine reads the value from a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_GET0D    DOUBLE PRECISION
*        DAT_GET0R    REAL
*        DAT_GET0I    INTEGER
*        DAT_GET0L    LOGICAL
*        DAT_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_GET0L(LOC; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=LOGICAL
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_GETL directly.
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
*    Export :
      LOGICAL value			! Scalar to receive value
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
         call dat_getL(loc, ndim, dims, value, status)
      endif

      end
*+ DAT_GET0R - Read scalar object value
      subroutine dat_get0R(loc, value, status)
*    Description :
*     This routine reads the value from a scalar primitive object.
*     There is a routine for each access type,
*
*        DAT_GET0D    DOUBLE PRECISION
*        DAT_GET0R    REAL
*        DAT_GET0I    INTEGER
*        DAT_GET0L    LOGICAL
*        DAT_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_GET0R(LOC; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     VALUE=REAL
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_GETR directly.
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
*    Export :
      REAL value			! Scalar to receive value
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
         call dat_getR(loc, ndim, dims, value, status)
      endif

      end
