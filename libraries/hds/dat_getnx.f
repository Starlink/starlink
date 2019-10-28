      subroutine dat_getnC(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETNC

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETNC(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=CHARACTER*(*)
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETC routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETC, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      CHARACTER*(*) values(*)			! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getC(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getC(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getC(vec, 1, dims(1), values(maxp+1), status)
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
      subroutine dat_getnD(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETND

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETND(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=DOUBLE PRECISION
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETD routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETD, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      DOUBLE PRECISION values(*)			! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getD(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getD(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getD(vec, 1, dims(1), values(maxp+1), status)
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
      subroutine dat_getnI(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETNI

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETNI(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=INTEGER
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETI routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETI, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      INTEGER values(*)			! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getI(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getI(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getI(vec, 1, dims(1), values(maxp+1), status)
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
      subroutine dat_getnK(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETNK

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETNK(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=INTEGER*8
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETI routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETI, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      INTEGER*8 values(*)	        ! Array to receive values
      integer actd(*)			! Object dimensions

*  Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getK(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getK(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getK(vec, 1, dims(1), values(maxp+1), status)
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
      subroutine dat_getnL(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETNL

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETNL(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=LOGICAL
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETL routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETL, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      LOGICAL values(*)			! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getL(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getL(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getL(vec, 1, dims(1), values(maxp+1), status)
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
      subroutine dat_getnR(loc, ndim, maxd, values, actd, status)
*+
*  Name:
*     DAT_GETNR

*  Purpose:
*     Read object values as an n-dimensional array.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL DAT_GETNR(LOC, NDIM, MAXDIM, VALUES, ACTDIM, STATUS)

*  Description:
*     Read the values from an n-dimensional primitive object.
*     The supplied number of dimensions, NDIM, is expected to match the
*     actual number of object dimensions.   The dimensions, MAXDIM, of
*     the program array must be of sufficient size (along each axis)
*     to contain the object array.
*     There is a routine for each access type,

*        DAT_GETND    DOUBLE PRECISION
*        DAT_GETNR    REAL
*        DAT_GETNI    INTEGER
*        DAT_GETNK    INTEGER*8
*        DAT_GETNL    LOGICAL
*        DAT_GETNC    CHARACTER[*n]

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
*        to receive the data values.
*     VALUES(*)=REAL
*        Array to receive the values associated with the object.
*        It must be of sufficient size to contain them all.
*     ACTDIM(NDIM)=INTEGER
*        Array to receive the actual object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     Use DAT_SHAPE to get the object dimensions, and check that
*     it can be read into the array provided.
*     Treate the 0-D and 1-D cases directly using the
*     DAT_GETR routine;  treate the n-D case by a series of calls
*     to DAT_VEC and DAT_GETR, copying blocks where the program
*     and array dimensions concur.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
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

*  Arguments Returned:
      REAL values(*)			! Array to receive values
      integer actd(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  Local Variables:
      character*(DAT__SZLOC) whole	! Whole object as a vector
      character*(DAT__SZLOC) vec	! Locator for Vector Slice
      integer size			! object size
      integer nd			! actual number of dimensions
      integer actb			! block size in ACTD
      integer maxb			! block size in MAXD
      integer actp			! offset in object
      integer maxp			! offset in values array
      integer nblock			! number of transfer blocks
      integer i				! loop index
      integer dims(1)

*.


      if (status .eq. SAI__OK) then
         call dat_size(loc, size, status)
         call dat_shape(loc, ndim, actd, nd, status)
         if (status .ne. SAI__OK) then
            continue
         elseif (nd .ne. ndim) then
            status = DAT__DIMIN
         elseif (ndim .eq. 0) then
            call dat_getR(loc, 0, 0, values, status)
         elseif (ndim .eq. 1) then
            if (maxd(1) .ge. actd(1)) then
               call dat_getR(loc, 1, actd(1), values, status)
            else
               status = DAT__BOUND
            endif
         else
*          Find the last index where MAXD = ACTD
            maxb = maxd(1)
            actb = actd(1)
            do i = 2, ndim
               if (actd(i) .gt. maxd(i)) then
                  status = DAT__BOUND
                  goto 1
               elseif (actd(i) .ne. maxd(i)) then
                  goto 1
               else
                  actb = actb*actd(i)
                  maxb = maxb*maxd(i)
               endif
            enddo
*          Only continue if arrays are OK
 1          continue
*          Vectorize whole object
            call dat_vec(loc, whole, status)
            if (status .ne. SAI__OK) then
               continue
            else
               nblock = size/actb
               actp = 0
               maxp = 0
*             Copy each block separately
               do i = 1, nblock
                  dims(1) = actp+1
                  call dat_slice(whole, 1, dims(1), actp+actb, vec,
     :              status)
                  if (status .ne. SAI__OK) then
                     goto 2
                  endif
                  dims(1) = actb
                  call dat_getR(vec, 1, dims(1), values(maxp+1), status)
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
