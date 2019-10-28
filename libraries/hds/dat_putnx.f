      subroutine dat_putnC(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTNC

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTNC(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
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
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTC routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTC, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      CHARACTER*(*) values(*)			! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putC(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putC(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putC(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
      subroutine dat_putnD(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTND

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTND(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
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
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTD routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTD, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      DOUBLE PRECISION values(*)			! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putD(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putD(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putD(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
      subroutine dat_putnI(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTNI

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTNI(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
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
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTI routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTI, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      INTEGER values(*)			! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putI(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putI(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putI(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
      subroutine dat_putnK(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTNK

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTNK(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
*     NDIM=INTEGER
*        Expression specifying the number of dimensions of the
*        program array, VALUES.   This must match the actual number
*        of object dimensions.
*     MAXDIM(NDIM)=INTEGER
*        Array specifying the dimensions of the program array
*        containing the data values.
*     VALUES(*)=INTEGER*8
*        Array containing the values to be written into the object.
*     ACTDIM(NDIM)=INTEGER
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTI routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTI, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      INTEGER*8 values(*)		! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putK(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putK(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putK(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
      subroutine dat_putnL(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTNL

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTNL(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
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
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTL routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTL, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      LOGICAL values(*)			! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putL(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putL(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putL(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
      subroutine dat_putnR(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_PUTNR

*  Purpose:
*     Write object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_PUTNR(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Write the values into an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_PUTND    DOUBLE PRECISION
*        DAT_PUTNR    REAL
*        DAT_PUTNI    INTEGER
*        DAT_PUTNK    INTEGER*8
*        DAT_PUTNL    LOGICAL
*        DAT_PUTNC    CHARACTER[*n]

*     If the object data type differs from the access type, then
*     conversion is performed.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a primitive
*        data object.
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
*        Array containing the actual object dimensions.  These must
*        match the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it is contained in the supplied array.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_PUTR routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_PUTR, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) loc			! Object Locator
      integer ndim			! Number of dimensions
      integer maxd(*)			! Program array dimensions
      REAL values(*)			! Array to receive values
      integer actd(*)			! Object dimensions

*  Arguments Returned:
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer objd(DAT__MXDIM)		! actual object dimensions
      logical same			! whether ACTD and OBJD are same
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
*       Enquire object size and shape
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, objd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
*       0-D case
         elseif (ndim .eq. 0) then
            call dat_putR(loc, 0, 0, values, status)
*       1-D case
         elseif (ndim .eq. 1) then
            if (actd(1) .ne. objd(1)) then
               status = DAT__BOUND
            elseif (actd(1) .gt. maxd(1)) then
               status = DAT__BOUND
            else
               call dat_putR(loc, 1, actd(1), values, status)
            endif
*       n-D cases
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            same = .true.
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (objd(i) .ne. actd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (same) then
                  same = actd(i) .eq. maxd(i)
                  if (same) then
                     actb = actb*actd(i)
                     maxb = maxb*maxd(i)
                  endif
               endif
            enddo
 1          continue
*          Only continue if arrays are OK
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  call dat_slice(whole, 1, actp+1, actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_putR(vec, 1, dims(1), values(maxp+1), status)
                  call dat_annul(vec, status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  actp = actp + actb
                  maxp = maxp + maxb
               enddo
 2             continue
               call dat_annul(whole, status)
            endif
         endif
      endif

      end
