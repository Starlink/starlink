*+ CMP_PUTVC - Write component values as if it were a vector
      subroutine cmp_putvC(struct, comp, nval, values, status)
*    Description :
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_PUTVC(LOC, NAME, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=CHARACTER*(*)
*           Array to containing the values to be written.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.
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
      integer nval			! Number of values to be written
      CHARACTER*(*) values(*)		! Vector to supply values
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putvC(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


*+ CMP_PUTVD - Write component values as if it were a vector
      subroutine cmp_putvD(struct, comp, nval, values, status)
*    Description :
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_PUTVD(LOC, NAME, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=DOUBLE PRECISION
*           Array to containing the values to be written.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.
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
      integer nval			! Number of values to be written
      DOUBLE PRECISION values(*)		! Vector to supply values
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putvD(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


*+ CMP_PUTVI - Write component values as if it were a vector
      subroutine cmp_putvI(struct, comp, nval, values, status)
*    Description :
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_PUTVI(LOC, NAME, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=INTEGER
*           Array to containing the values to be written.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.
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
      integer nval			! Number of values to be written
      INTEGER values(*)		! Vector to supply values
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putvI(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


*+ CMP_PUTVL - Write component values as if it were a vector
      subroutine cmp_putvL(struct, comp, nval, values, status)
*    Description :
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_PUTVL(LOC, NAME, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=LOGICAL
*           Array to containing the values to be written.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.
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
      integer nval			! Number of values to be written
      LOGICAL values(*)		! Vector to supply values
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putvL(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


*+ CMP_PUTVR - Write component values as if it were a vector
      subroutine cmp_putvR(struct, comp, nval, values, status)
*    Description :
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,
*
*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_PUTVR(LOC, NAME, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=REAL
*           Array to containing the values to be written.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.
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
      integer nval			! Number of values to be written
      REAL values(*)		! Vector to supply values
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putvR(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


