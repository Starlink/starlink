      subroutine cmp_put1C(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUT1C

*  Purpose:
*     Write vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT1C(LOC, NAME, NVAL VALUES, STATUS)

*  Description:
*     Write the values associated with a vector primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT1D    DOUBLE PRECISION
*        CMP_PUT1R    REAL
*        CMP_PUT1I    INTEGER
*        CMP_PUT1L    LOGICAL
*        CMP_PUT1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NVAL=INTEGER
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     VALUES(NVAL)=CHARACTER*(*)
*        Array containing the values to be written into the object.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the values with DAT_PUT1C.

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
      integer nval			! number of values
      CHARACTER*(*) values(*)		! Scalar to supply values

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
            call dat_put1C(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put1D(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUT1D

*  Purpose:
*     Write vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT1D(LOC, NAME, NVAL VALUES, STATUS)

*  Description:
*     Write the values associated with a vector primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT1D    DOUBLE PRECISION
*        CMP_PUT1R    REAL
*        CMP_PUT1I    INTEGER
*        CMP_PUT1L    LOGICAL
*        CMP_PUT1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NVAL=INTEGER
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     VALUES(NVAL)=DOUBLE PRECISION
*        Array containing the values to be written into the object.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the values with DAT_PUT1D.

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
      integer nval			! number of values
      DOUBLE PRECISION values(*)		! Scalar to supply values

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
            call dat_put1D(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put1I(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUT1I

*  Purpose:
*     Write vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT1I(LOC, NAME, NVAL VALUES, STATUS)

*  Description:
*     Write the values associated with a vector primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT1D    DOUBLE PRECISION
*        CMP_PUT1R    REAL
*        CMP_PUT1I    INTEGER
*        CMP_PUT1L    LOGICAL
*        CMP_PUT1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NVAL=INTEGER
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     VALUES(NVAL)=INTEGER
*        Array containing the values to be written into the object.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the values with DAT_PUT1I.

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
      integer nval			! number of values
      INTEGER values(*)		! Scalar to supply values

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
            call dat_put1I(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put1L(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUT1L

*  Purpose:
*     Write vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT1L(LOC, NAME, NVAL VALUES, STATUS)

*  Description:
*     Write the values associated with a vector primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT1D    DOUBLE PRECISION
*        CMP_PUT1R    REAL
*        CMP_PUT1I    INTEGER
*        CMP_PUT1L    LOGICAL
*        CMP_PUT1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NVAL=INTEGER
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     VALUES(NVAL)=LOGICAL
*        Array containing the values to be written into the object.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the values with DAT_PUT1L.

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
      integer nval			! number of values
      LOGICAL values(*)		! Scalar to supply values

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
            call dat_put1L(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_put1R(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUT1R

*  Purpose:
*     Write vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUT1R(LOC, NAME, NVAL VALUES, STATUS)

*  Description:
*     Write the values associated with a vector primitive component
*     of a structure.
*     There is a routine for each access type,

*        CMP_PUT1D    DOUBLE PRECISION
*        CMP_PUT1R    REAL
*        CMP_PUT1I    INTEGER
*        CMP_PUT1L    LOGICAL
*        CMP_PUT1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NVAL=INTEGER
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     VALUES(NVAL)=REAL
*        Array containing the values to be written into the object.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to put a Locator for the structure component,
*     and then put the values with DAT_PUT1R.

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
      integer nval			! number of values
      REAL values(*)		! Scalar to supply values

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
            call dat_put1R(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end

