*+ CMP_GETNC - Read component values as an n-dimensional array
      subroutine cmp_getnC(struct, comp, ndim, maxd, values, actd,
     :  status)
*    Description :
*     Read the values associated with an n-dimensional primitive
*     component of a structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,
*
*        CMP_GETND    DOUBLE PRECISION
*        CMP_GETNR    REAL
*        CMP_GETNI    INTEGER
*        CMP_GETNL    LOGICAL
*        CMP_GETNC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETNC(LOC, NAME, NDIM, MAXDIM; VALUES, ACTDIM, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NDIM=INTEGER
*           Expression specifying the number of dimensions of the
*           program array, VALUES.   This must match the actual number
*           of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*           Array specifying the dimensions of the program array
*           to receive the data values.
*     VALUES(*)=CHARACTER*(*)
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*           Array to receive the actual object dimensions.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get locator to component using DAT_FIND, and then use DAT_GETNx.
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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
*    Export :
      CHARACTER*(*) values(*)		! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc		! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_getnC(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETND - Read component values as an n-dimensional array
      subroutine cmp_getnD(struct, comp, ndim, maxd, values, actd,
     :  status)
*    Description :
*     Read the values associated with an n-dimensional primitive
*     component of a structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,
*
*        CMP_GETND    DOUBLE PRECISION
*        CMP_GETNR    REAL
*        CMP_GETNI    INTEGER
*        CMP_GETNL    LOGICAL
*        CMP_GETNC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETND(LOC, NAME, NDIM, MAXDIM; VALUES, ACTDIM, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NDIM=INTEGER
*           Expression specifying the number of dimensions of the
*           program array, VALUES.   This must match the actual number
*           of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*           Array specifying the dimensions of the program array
*           to receive the data values.
*     VALUES(*)=DOUBLE PRECISION
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*           Array to receive the actual object dimensions.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get locator to component using DAT_FIND, and then use DAT_GETNx.
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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
*    Export :
      DOUBLE PRECISION values(*)		! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc		! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_getnD(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETNI - Read component values as an n-dimensional array
      subroutine cmp_getnI(struct, comp, ndim, maxd, values, actd,
     :  status)
*    Description :
*     Read the values associated with an n-dimensional primitive
*     component of a structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,
*
*        CMP_GETND    DOUBLE PRECISION
*        CMP_GETNR    REAL
*        CMP_GETNI    INTEGER
*        CMP_GETNL    LOGICAL
*        CMP_GETNC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETNI(LOC, NAME, NDIM, MAXDIM; VALUES, ACTDIM, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NDIM=INTEGER
*           Expression specifying the number of dimensions of the
*           program array, VALUES.   This must match the actual number
*           of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*           Array specifying the dimensions of the program array
*           to receive the data values.
*     VALUES(*)=INTEGER
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*           Array to receive the actual object dimensions.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get locator to component using DAT_FIND, and then use DAT_GETNx.
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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
*    Export :
      INTEGER values(*)		! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc		! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_getnI(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETNL - Read component values as an n-dimensional array
      subroutine cmp_getnL(struct, comp, ndim, maxd, values, actd,
     :  status)
*    Description :
*     Read the values associated with an n-dimensional primitive
*     component of a structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,
*
*        CMP_GETND    DOUBLE PRECISION
*        CMP_GETNR    REAL
*        CMP_GETNI    INTEGER
*        CMP_GETNL    LOGICAL
*        CMP_GETNC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETNL(LOC, NAME, NDIM, MAXDIM; VALUES, ACTDIM, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NDIM=INTEGER
*           Expression specifying the number of dimensions of the
*           program array, VALUES.   This must match the actual number
*           of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*           Array specifying the dimensions of the program array
*           to receive the data values.
*     VALUES(*)=LOGICAL
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*           Array to receive the actual object dimensions.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get locator to component using DAT_FIND, and then use DAT_GETNx.
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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
*    Export :
      LOGICAL values(*)		! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc		! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_getnL(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETNR - Read component values as an n-dimensional array
      subroutine cmp_getnR(struct, comp, ndim, maxd, values, actd,
     :  status)
*    Description :
*     Read the values associated with an n-dimensional primitive
*     component of a structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,
*
*        CMP_GETND    DOUBLE PRECISION
*        CMP_GETNR    REAL
*        CMP_GETNI    INTEGER
*        CMP_GETNL    LOGICAL
*        CMP_GETNC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETNR(LOC, NAME, NDIM, MAXDIM; VALUES, ACTDIM, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     NDIM=INTEGER
*           Expression specifying the number of dimensions of the
*           program array, VALUES.   This must match the actual number
*           of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*           Array specifying the dimensions of the program array
*           to receive the data values.
*     VALUES(*)=REAL
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*           Array to receive the actual object dimensions.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get locator to component using DAT_FIND, and then use DAT_GETNx.
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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
*    Export :
      REAL values(*)		! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc		! Object Locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_getnR(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

