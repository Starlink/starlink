      subroutine cmp_putvC(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUTVC

*  Purpose:
*     Write component values as if it were a vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTVC(LOC, NAME, NVAL, VALUES, STATUS)

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,

*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]

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
*        written into the object.   It must match the actual object
*        size.
*     VALUES(NVAL)=CHARACTER*(*)
*        Array to containing the values to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.

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
      integer nval			! Number of values to be written
      CHARACTER*(*) values(*)		! Vector to supply values
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
            call dat_putvC(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


      subroutine cmp_putvD(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUTVD

*  Purpose:
*     Write component values as if it were a vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTVD(LOC, NAME, NVAL, VALUES, STATUS)

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,

*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]

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
*        written into the object.   It must match the actual object
*        size.
*     VALUES(NVAL)=DOUBLE PRECISION
*        Array to containing the values to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.

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
      integer nval			! Number of values to be written
      DOUBLE PRECISION values(*)		! Vector to supply values
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
            call dat_putvD(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


      subroutine cmp_putvI(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUTVI

*  Purpose:
*     Write component values as if it were a vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTVI(LOC, NAME, NVAL, VALUES, STATUS)

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,

*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]

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
*        written into the object.   It must match the actual object
*        size.
*     VALUES(NVAL)=INTEGER
*        Array to containing the values to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.

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
      integer nval			! Number of values to be written
      INTEGER values(*)		! Vector to supply values
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
            call dat_putvI(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


      subroutine cmp_putvL(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUTVL

*  Purpose:
*     Write component values as if it were a vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTVL(LOC, NAME, NVAL, VALUES, STATUS)

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,

*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]

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
*        written into the object.   It must match the actual object
*        size.
*     VALUES(NVAL)=LOGICAL
*        Array to containing the values to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.

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
      integer nval			! Number of values to be written
      LOGICAL values(*)		! Vector to supply values
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
            call dat_putvL(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


      subroutine cmp_putvR(struct, comp, nval, values, status)
*+
*  Name:
*     CMP_PUTVR

*  Purpose:
*     Write component values as if it were a vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_PUTVR(LOC, NAME, NVAL, VALUES, STATUS)

*  Description:
*     Write the values into a primitive component of a structure
*     as if it were vectorized (i.e. regardless of its actual
*     dimensionality).
*     There is a routine for each access type,

*        CMP_PUTVD    DOUBLE PRECISION
*        CMP_PUTVR    REAL
*        CMP_PUTVI    INTEGER
*        CMP_PUTVL    LOGICAL
*        CMP_PUTVC    CHARACTER[*n]

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
*        written into the object.   It must match the actual object
*        size.
*     VALUES(NVAL)=REAL
*        Array to containing the values to be written.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_FIND to get a locator to the component, and then use
*     DAT_PUTV to write the values.

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
      integer nval			! Number of values to be written
      REAL values(*)		! Vector to supply values
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
            call dat_putvR(loc, nval, values, status)
            call dat_annul(loc, status)
         endif
      endif

      end


