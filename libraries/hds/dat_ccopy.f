*+ DAT_CCOPY - Copy one level of structure
      subroutine dat_ccopy(sloc, dloc, cname, cloc, status)
*    Description :
*     This routine copies the data object pointed to by the source
*     locator into a component of the data structure pointed to by
*     the destination locator.
*     If the object to be copied is a structure, a new structure of
*     the correct type and dimensions is created but the contents of
*     the structure are not copied.
*     A locator to the new object is returned.
*    Invocation :
*     CALL DAT_CCOPY(SLOC, DLOC, CNAME; CLOC, STATUS)
*    Parameters :
*     SLOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with the source
*           object.
*     DLOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with the data
*           structure to which the object is to be copied.
*     CNAME=CHARACTER*(DAT__SZNAM)
*           Variable containing the name of the component in the
*           structure to which the object is to be copied.
*     CLOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with the new
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     If the source object is a primitive, do a straight copy.
*     Otherwise get the necessary quantities and create the new
*     object.
*    Authors :
*     Sid Wright  (UCL::SLW)
*    History :
*     29-Mar-1983:  Original.  (UCL::SLW)
*     05.11.1984:   Remove calls to error system.
*                   Change routine name from DAT_$CCOPY (REVAD::BDK)
*     15.04.1987:   Improve prologue layout (RLVAD::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
*    Import :
      character*(*) sloc		! Source Locator
      character*(*) dloc		! Environment Locator
      character*(*) cname		! component to be created
*    Export :
      character*(*) cloc		! locator to new component
*    Status return :
      integer status			! Status Return
*    Local variables :
      logical struct    		! Whether object is a structure or not
      character*(DAT__SZTYP) type
      integer ndim
      integer dims(DAT__MXDIM)
      LOGICAL STATE

*-

      if (status .ne. SAI__OK) return

      call dat_struc(sloc, struct, status)
      if (status .ne. SAI__OK) return
      if (.not.struct) then
         CALL DAT_STATE( SLOC, STATE, STATUS )
         IF ( STATE ) THEN
            CALL DAT_COPY( SLOC, DLOC, CNAME, STATUS )
         ELSE
            CALL DAT_TYPE( SLOC, TYPE, STATUS )
            CALL DAT_SHAPE( SLOC, DAT__MXDIM, DIMS, NDIM, STATUS )
            CALL DAT_NEW( DLOC, CNAME, TYPE, NDIM, DIMS, STATUS )
         ENDIF
      else
         call dat_type(sloc, type, status)
         call dat_shape(sloc, DAT__MXDIM, dims, ndim, status)
         call dat_new(dloc, cname, type, ndim, dims, status)
      endif
      call dat_find(dloc, cname, cloc, status)

      end
