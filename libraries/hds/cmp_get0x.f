*+ CMP_GET0C - Read scalar structure component value
      subroutine cmp_get0C(struct, comp, value, status)
*    Description :
*     This routine reads the value from a scalar primitive component
*     of a structure.
*     There is a routine for each access type,
*
*        CMP_GET0D    DOUBLE PRECISION
*        CMP_GET0R    REAL
*        CMP_GET0I    INTEGER
*        CMP_GET0L    LOGICAL
*        CMP_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL CMP_GET0C(LOC, NAME; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     VALUE=CHARACTER*(*)
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the value with DAT_GET0C.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      CHARACTER*(*) value		! Scalar to receive value
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Parameter Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_get0C(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GET0D - Read scalar structure component value
      subroutine cmp_get0D(struct, comp, value, status)
*    Description :
*     This routine reads the value from a scalar primitive component
*     of a structure.
*     There is a routine for each access type,
*
*        CMP_GET0D    DOUBLE PRECISION
*        CMP_GET0R    REAL
*        CMP_GET0I    INTEGER
*        CMP_GET0L    LOGICAL
*        CMP_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL CMP_GET0D(LOC, NAME; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     VALUE=DOUBLE PRECISION
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the value with DAT_GET0D.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      DOUBLE PRECISION value		! Scalar to receive value
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Parameter Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_get0D(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GET0I - Read scalar structure component value
      subroutine cmp_get0I(struct, comp, value, status)
*    Description :
*     This routine reads the value from a scalar primitive component
*     of a structure.
*     There is a routine for each access type,
*
*        CMP_GET0D    DOUBLE PRECISION
*        CMP_GET0R    REAL
*        CMP_GET0I    INTEGER
*        CMP_GET0L    LOGICAL
*        CMP_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL CMP_GET0I(LOC, NAME; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     VALUE=INTEGER
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the value with DAT_GET0I.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      INTEGER value		! Scalar to receive value
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Parameter Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_get0I(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GET0L - Read scalar structure component value
      subroutine cmp_get0L(struct, comp, value, status)
*    Description :
*     This routine reads the value from a scalar primitive component
*     of a structure.
*     There is a routine for each access type,
*
*        CMP_GET0D    DOUBLE PRECISION
*        CMP_GET0R    REAL
*        CMP_GET0I    INTEGER
*        CMP_GET0L    LOGICAL
*        CMP_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL CMP_GET0L(LOC, NAME; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     VALUE=LOGICAL
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the value with DAT_GET0L.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      LOGICAL value		! Scalar to receive value
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Parameter Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_get0L(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GET0R - Read scalar structure component value
      subroutine cmp_get0R(struct, comp, value, status)
*    Description :
*     This routine reads the value from a scalar primitive component
*     of a structure.
*     There is a routine for each access type,
*
*        CMP_GET0D    DOUBLE PRECISION
*        CMP_GET0R    REAL
*        CMP_GET0I    INTEGER
*        CMP_GET0L    LOGICAL
*        CMP_GET0C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL CMP_GET0R(LOC, NAME; VALUE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     VALUE=REAL
*           Variable to receive the value associated with the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the value with DAT_GET0R.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      REAL value		! Scalar to receive value
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Parameter Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_get0R(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

