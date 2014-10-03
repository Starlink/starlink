      subroutine cmp_putnC(struct, comp, ndim, maxd, values, actd,
     :  status)
*+
*  Name:
*     CMP_PUTNC

*  Purpose:
*     Write component values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTNC(LOC, NAME, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive component of a
*     structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        CMP_PUTND    DOUBLE PRECISION
*        CMP_PUTNR    REAL
*        CMP_PUTNI    INTEGER
*        CMP_PUTNL    LOGICAL
*        CMP_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=CHARACTER*(*)
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Get locator to component using DAT_FIND, and then use DAT_PUTNx.

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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      CHARACTER*(*) values(*)		! Array to supply values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Object Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putnC(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_putnD(struct, comp, ndim, maxd, values, actd,
     :  status)
*+
*  Name:
*     CMP_PUTND

*  Purpose:
*     Write component values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTND(LOC, NAME, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive component of a
*     structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        CMP_PUTND    DOUBLE PRECISION
*        CMP_PUTNR    REAL
*        CMP_PUTNI    INTEGER
*        CMP_PUTNL    LOGICAL
*        CMP_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=DOUBLE PRECISION
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Get locator to component using DAT_FIND, and then use DAT_PUTNx.

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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      DOUBLE PRECISION values(*)		! Array to supply values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Object Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putnD(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_putnI(struct, comp, ndim, maxd, values, actd,
     :  status)
*+
*  Name:
*     CMP_PUTNI

*  Purpose:
*     Write component values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTNI(LOC, NAME, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive component of a
*     structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        CMP_PUTND    DOUBLE PRECISION
*        CMP_PUTNR    REAL
*        CMP_PUTNI    INTEGER
*        CMP_PUTNL    LOGICAL
*        CMP_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=INTEGER
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Get locator to component using DAT_FIND, and then use DAT_PUTNx.

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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      INTEGER values(*)		! Array to supply values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Object Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putnI(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_putnL(struct, comp, ndim, maxd, values, actd,
     :  status)
*+
*  Name:
*     CMP_PUTNL

*  Purpose:
*     Write component values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTNL(LOC, NAME, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive component of a
*     structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        CMP_PUTND    DOUBLE PRECISION
*        CMP_PUTNR    REAL
*        CMP_PUTNI    INTEGER
*        CMP_PUTNL    LOGICAL
*        CMP_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=LOGICAL
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Get locator to component using DAT_FIND, and then use DAT_PUTNx.

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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      LOGICAL values(*)		! Array to supply values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Object Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putnL(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_putnR(struct, comp, ndim, maxd, values, actd,
     :  status)
*+
*  Name:
*     CMP_PUTNR

*  Purpose:
*     Write component values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTNR(LOC, NAME, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive component of a
*     structure.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        CMP_PUTND    DOUBLE PRECISION
*        CMP_PUTNR    REAL
*        CMP_PUTNI    INTEGER
*        CMP_PUTNL    LOGICAL
*        CMP_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=REAL
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.   These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Get locator to component using DAT_FIND, and then use DAT_PUTNx.

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
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      REAL values(*)		! Array to supply values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) loc	! Object Locator

*.


      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_putnR(loc, ndim, maxd, values, actd, status)
            call dat_annul(loc, status)
         endif
      endif

      end

