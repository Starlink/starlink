*+ DAT_MAPV - Map values associated with an object as if it were vectorized
      subroutine dat_mapv(loc, type, mode, ptr, actval, status)
*    Description :
*     This routine maps the primitive object data for reading, writing
*     or updating, as if it were vectorized.   The number of values
*     mapped is returned in the variable, ACTVAL.
*
*     Note that it is not possible to map data of type '_CHAR'.
*    Invocation :
*     CALL DAT_MAPV(LOC, TYPE, MODE; PNTR, ACTVAL, STATUS)
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
*     PNTR=INTEGER
*           Variable to receive the virtual memory pointer for the
*           mapped values.   This can be used in conjunction with the
*           VAX Fortran "%VAL" construct.
*     ACTVAL=INTEGER
*           Variable to receive the number of values mapped.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Enquire size and shape, then use DAT_MAP.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) type		! Access type
      character*(*) mode		! Access mode
*    Export :
      integer ptr			! Address pointer
      integer actval			! Number of values mapped
*    Status return :
      integer status			! Status Return
*    Local variables :
      integer ndim			! Number of dimensions
      integer dims(DAT__MXDIM)		! Dimensions
*-

      data dims / DAT__MXDIM * 0 /

      if (status .eq. SAI__OK) then
         call dat_size(loc, actval, status)
         call dat_shape(loc, DAT__MXDIM, dims, ndim, status)
         call dat_map(loc, type, mode, ndim, dims, ptr, status)
      endif

      end


