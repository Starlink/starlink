*+ DAT_MAPN - Map object values as N-dimensional array
      subroutine dat_mapn(loc, type, mode, ndim, ptr, dims, status)
*    Description :
*     This routine maps the primitive object data for reading, writing
*     or updating.   The caller is expected to know the number of
*     object dimensions, NDIM.   The object dimensions are returned
*     in the array, DIMS(NDIM).
*
*     Note that it is not possible to map data of type '_CHAR'.
*    Invocation :
*     CALL DAT_MAPN(LOC, TYPE, MODE, NDIM; PNTR, DIMS, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     TYPE=CHARACTER*(*)
*           Expression specifying the data type of the mapped values.
*           If the actual type of the data object differs from this,
*           then conversion will be performed in 'READ' and 'UPDATE'
*           modes.
*     MODE=CHARACTER*(*)
*           Expression specifying the mode in which the data are to be
*           mapped.  (Either 'READ', 'WRITE' or 'UPDATE'.)
*     NDIM=INTEGER
*           Expression specifying the number of array dimensions.
*           This must match the actual number of object dimensions.
*     PNTR=INTEGER
*           Variable to receive the virtual memory pointer for the
*           mapped values.   This can be used in conjunction with the
*           VAX Fortran "%VAL" construct.
*     DIMS(NDIM)=INTEGER
*           Array to receive the dimensions of the object mapped.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_SHAPE to get the object dimensions, and check
*     that the supplied dimensionality agrees with that of
*     the object.
*     Use DAT_MAP to map the whole object.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:  Calls to error system removed (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) type		! Access type
      character*(*) mode		! Access mode
      integer ndim			! Number of dimensions required
*    Export :
      integer ptr			! Address pointer
      integer dims(*)			! Actual object dimensions
*    Status return :
      integer status			! Status Return
*    Local variables :
      integer actdim			! Actual number of dimensions
*-

      if (status .eq. SAI__OK) then

         call dat_shape(loc, ndim, dims, actdim, status)

         if (status .ne. SAI__OK) then
            continue
         elseif (actdim .ne. ndim) then
            status = DAT__DIMIN
         else
            call dat_map(loc, type, mode, ndim, dims, ptr, status)
         endif

      endif

      end


