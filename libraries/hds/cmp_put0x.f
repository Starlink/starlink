      subroutine cmp_put0C(struct, comp, value, status)
*+
*  Name:
*     CMP_PUT0C

*  Purpose:
*     Write scalar structure component value.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT0C(LOC, NAME, VALUE, STATUS)

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT0D    DOUBLE PRECISION
*        CMP_PUT0R    REAL
*        CMP_PUT0I    INTEGER
*        CMP_PUT0L    LOGICAL
*        CMP_PUT0C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     VALUE=CHARACTER*(*)
*        Expression specifying the value to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the value with DAT_PUT0C.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      CHARACTER*(*) value		! Scalar to supply value

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Parameter Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_put0C(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put0D(struct, comp, value, status)
*+
*  Name:
*     CMP_PUT0D

*  Purpose:
*     Write scalar structure component value.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT0D(LOC, NAME, VALUE, STATUS)

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT0D    DOUBLE PRECISION
*        CMP_PUT0R    REAL
*        CMP_PUT0I    INTEGER
*        CMP_PUT0L    LOGICAL
*        CMP_PUT0C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     VALUE=DOUBLE PRECISION
*        Expression specifying the value to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the value with DAT_PUT0D.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      DOUBLE PRECISION value		! Scalar to supply value

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Parameter Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_put0D(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put0I(struct, comp, value, status)
*+
*  Name:
*     CMP_PUT0I

*  Purpose:
*     Write scalar structure component value.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT0I(LOC, NAME, VALUE, STATUS)

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT0D    DOUBLE PRECISION
*        CMP_PUT0R    REAL
*        CMP_PUT0I    INTEGER
*        CMP_PUT0L    LOGICAL
*        CMP_PUT0C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     VALUE=INTEGER
*        Expression specifying the value to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the value with DAT_PUT0I.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      INTEGER value		! Scalar to supply value

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Parameter Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_put0I(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put0L(struct, comp, value, status)
*+
*  Name:
*     CMP_PUT0L

*  Purpose:
*     Write scalar structure component value.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT0L(LOC, NAME, VALUE, STATUS)

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT0D    DOUBLE PRECISION
*        CMP_PUT0R    REAL
*        CMP_PUT0I    INTEGER
*        CMP_PUT0L    LOGICAL
*        CMP_PUT0C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     VALUE=LOGICAL
*        Expression specifying the value to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the value with DAT_PUT0L.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      LOGICAL value		! Scalar to supply value

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Parameter Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_put0L(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put0R(struct, comp, value, status)
*+
*  Name:
*     CMP_PUT0R

*  Purpose:
*     Write scalar structure component value.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT0R(LOC, NAME, VALUE, STATUS)

*  Description:
*     This routine writes a value into a scalar primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT0D    DOUBLE PRECISION
*        CMP_PUT0R    REAL
*        CMP_PUT0I    INTEGER
*        CMP_PUT0L    LOGICAL
*        CMP_PUT0C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     VALUE=REAL
*        Expression specifying the value to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the value with DAT_PUT0R.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987: Improve prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      REAL value		! Scalar to supply value

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Parameter Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_put0R(loc, value, status)
            call dat_annul(loc, status)
         endif
      endif

      end

