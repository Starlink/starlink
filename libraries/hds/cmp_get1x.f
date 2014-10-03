      subroutine cmp_get1C(struct, comp, maxval, values, actval, status)
*+
*  Name:
*     CMP_GET1C

*  Purpose:
*     Read vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_GET1C(LOC, NAME, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,

*        CMP_GET1D    DOUBLE PRECISION
*        CMP_GET1R    REAL
*        CMP_GET1I    INTEGER
*        CMP_GET1L    LOGICAL
*        CMP_GET1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     MAXVAL=INTEGER
*        Expression specifying the maximum number of values that can
*        be held in the array, VALUES.
*     VALUES(MAXVAL)=CHARACTER*(*)
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*        Variable to receive the actual number of values read.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the values with DAT_GET1C.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improve prologue layout  (RAL::AJC)
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
      integer maxval			! maximum number of values

*  Arguments Returned:
      CHARACTER*(*) values(*)		! Scalar to receive values
      integer actval			! actual number of values read
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
            call dat_get1C(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_get1D(struct, comp, maxval, values, actval, status)
*+
*  Name:
*     CMP_GET1D

*  Purpose:
*     Read vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_GET1D(LOC, NAME, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,

*        CMP_GET1D    DOUBLE PRECISION
*        CMP_GET1R    REAL
*        CMP_GET1I    INTEGER
*        CMP_GET1L    LOGICAL
*        CMP_GET1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     MAXVAL=INTEGER
*        Expression specifying the maximum number of values that can
*        be held in the array, VALUES.
*     VALUES(MAXVAL)=DOUBLE PRECISION
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*        Variable to receive the actual number of values read.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the values with DAT_GET1D.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improve prologue layout  (RAL::AJC)
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
      integer maxval			! maximum number of values

*  Arguments Returned:
      DOUBLE PRECISION values(*)		! Scalar to receive values
      integer actval			! actual number of values read
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
            call dat_get1D(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_get1I(struct, comp, maxval, values, actval, status)
*+
*  Name:
*     CMP_GET1I

*  Purpose:
*     Read vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_GET1I(LOC, NAME, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,

*        CMP_GET1D    DOUBLE PRECISION
*        CMP_GET1R    REAL
*        CMP_GET1I    INTEGER
*        CMP_GET1L    LOGICAL
*        CMP_GET1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     MAXVAL=INTEGER
*        Expression specifying the maximum number of values that can
*        be held in the array, VALUES.
*     VALUES(MAXVAL)=INTEGER
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*        Variable to receive the actual number of values read.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the values with DAT_GET1I.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improve prologue layout  (RAL::AJC)
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
      integer maxval			! maximum number of values

*  Arguments Returned:
      INTEGER values(*)		! Scalar to receive values
      integer actval			! actual number of values read
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
            call dat_get1I(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_get1L(struct, comp, maxval, values, actval, status)
*+
*  Name:
*     CMP_GET1L

*  Purpose:
*     Read vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_GET1L(LOC, NAME, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,

*        CMP_GET1D    DOUBLE PRECISION
*        CMP_GET1R    REAL
*        CMP_GET1I    INTEGER
*        CMP_GET1L    LOGICAL
*        CMP_GET1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     MAXVAL=INTEGER
*        Expression specifying the maximum number of values that can
*        be held in the array, VALUES.
*     VALUES(MAXVAL)=LOGICAL
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*        Variable to receive the actual number of values read.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the values with DAT_GET1L.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improve prologue layout  (RAL::AJC)
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
      integer maxval			! maximum number of values

*  Arguments Returned:
      LOGICAL values(*)		! Scalar to receive values
      integer actval			! actual number of values read
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
            call dat_get1L(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

      subroutine cmp_get1R(struct, comp, maxval, values, actval, status)
*+
*  Name:
*     CMP_GET1R

*  Purpose:
*     Read vector structure component values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_GET1R(LOC, NAME, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     This routine reads the values associated with a vector primitive
*     component of a structure. There is a routine for each access type,

*        CMP_GET1D    DOUBLE PRECISION
*        CMP_GET1R    REAL
*        CMP_GET1I    INTEGER
*        CMP_GET1L    LOGICAL
*        CMP_GET1C    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     MAXVAL=INTEGER
*        Expression specifying the maximum number of values that can
*        be held in the array, VALUES.
*     VALUES(MAXVAL)=REAL
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*        Variable to receive the actual number of values read.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a Locator for the structure component,
*     and then get the values with DAT_GET1R.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improve prologue layout  (RAL::AJC)
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
      integer maxval			! maximum number of values

*  Arguments Returned:
      REAL values(*)		! Scalar to receive values
      integer actval			! actual number of values read
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
            call dat_get1R(loc, maxval, values, actval, status)
            call dat_annul(loc, status)
         endif
      endif

      end

