*+ CMP_GETVC - Read component values as if it were a vector
      subroutine cmp_getvC(struct, comp, maxval, values, actval,
     :  status)
*    Description :
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,
*
*        CMP_GETVD    DOUBLE PRECISION
*        CMP_GETVR    REAL
*        CMP_GETVI    INTEGER
*        CMP_GETVL    LOGICAL
*        CMP_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETVC(LOC, NAME, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
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
*     Use DAT_FIND to get a locator to the comnponent, and then use
*     DAT_GETV.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     R. Warren-Smith (RFWS)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     15-OCT-1993: Fixed bug - locator wrongly declared with length
*                  DAT__SZNAM (RFWS)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer maxval			! Maximum number of values
*    Export :
      CHARACTER*(*) values(*)		! Array to receive values
      integer actval			! Actual number of values read
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
            call dat_getvC(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETVD - Read component values as if it were a vector
      subroutine cmp_getvD(struct, comp, maxval, values, actval,
     :  status)
*    Description :
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,
*
*        CMP_GETVD    DOUBLE PRECISION
*        CMP_GETVR    REAL
*        CMP_GETVI    INTEGER
*        CMP_GETVL    LOGICAL
*        CMP_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETVD(LOC, NAME, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that can
*           be held in the array, VALUES.
*     VALUES(MAXVAL)=DOUBLE PRECISION
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
*     Use DAT_FIND to get a locator to the comnponent, and then use
*     DAT_GETV.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     R. Warren-Smith (RFWS)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     15-OCT-1993: Fixed bug - locator wrongly declared with length
*                  DAT__SZNAM (RFWS)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer maxval			! Maximum number of values
*    Export :
      DOUBLE PRECISION values(*)		! Array to receive values
      integer actval			! Actual number of values read
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
            call dat_getvD(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETVI - Read component values as if it were a vector
      subroutine cmp_getvI(struct, comp, maxval, values, actval,
     :  status)
*    Description :
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,
*
*        CMP_GETVD    DOUBLE PRECISION
*        CMP_GETVR    REAL
*        CMP_GETVI    INTEGER
*        CMP_GETVL    LOGICAL
*        CMP_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETVI(LOC, NAME, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that can
*           be held in the array, VALUES.
*     VALUES(MAXVAL)=INTEGER
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
*     Use DAT_FIND to get a locator to the comnponent, and then use
*     DAT_GETV.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     R. Warren-Smith (RFWS)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     15-OCT-1993: Fixed bug - locator wrongly declared with length
*                  DAT__SZNAM (RFWS)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer maxval			! Maximum number of values
*    Export :
      INTEGER values(*)		! Array to receive values
      integer actval			! Actual number of values read
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
            call dat_getvI(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETVL - Read component values as if it were a vector
      subroutine cmp_getvL(struct, comp, maxval, values, actval,
     :  status)
*    Description :
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,
*
*        CMP_GETVD    DOUBLE PRECISION
*        CMP_GETVR    REAL
*        CMP_GETVI    INTEGER
*        CMP_GETVL    LOGICAL
*        CMP_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETVL(LOC, NAME, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that can
*           be held in the array, VALUES.
*     VALUES(MAXVAL)=LOGICAL
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
*     Use DAT_FIND to get a locator to the comnponent, and then use
*     DAT_GETV.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     R. Warren-Smith (RFWS)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     15-OCT-1993: Fixed bug - locator wrongly declared with length
*                  DAT__SZNAM (RFWS)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer maxval			! Maximum number of values
*    Export :
      LOGICAL values(*)		! Array to receive values
      integer actval			! Actual number of values read
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
            call dat_getvL(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

*+ CMP_GETVR - Read component values as if it were a vector
      subroutine cmp_getvR(struct, comp, maxval, values, actval,
     :  status)
*    Description :
*     This routine reads the values from a primitive component of a
*     structure as if it were vectorized (i.e. regardless of its
*     actual dimensionality).
*     There is a routine for each access type,
*
*        CMP_GETVD    DOUBLE PRECISION
*        CMP_GETVR    REAL
*        CMP_GETVI    INTEGER
*        CMP_GETVL    LOGICAL
*        CMP_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL CMP_GETVR(LOC, NAME, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that can
*           be held in the array, VALUES.
*     VALUES(MAXVAL)=REAL
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
*     Use DAT_FIND to get a locator to the comnponent, and then use
*     DAT_GETV.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     R. Warren-Smith (RFWS)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     15-OCT-1993: Fixed bug - locator wrongly declared with length
*                  DAT__SZNAM (RFWS)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer maxval			! Maximum number of values
*    Export :
      REAL values(*)		! Array to receive values
      integer actval			! Actual number of values read
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
            call dat_getvR(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

