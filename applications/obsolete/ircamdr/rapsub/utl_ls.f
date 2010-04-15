
*-h- ls.f
*    Description :
*     This file contains the LS utility program, which lists the
*     contents of a structured object stored in the Starlink Data
*     System.
*

*+ LS - List the contents of a structure.
*     subroutine ls(status)
      subroutine utl_ls(status)
*    Description:
*      List the contents of a dataset.
*
*      The contents of the dataset specified by the DATASET parameter are
*      listed on Standard Output.
*      A summary of the contents of the dataset is given including the
*      name, type and for primitive objects the first few values.
*
*    Parameters:
*       DATASET = UNIV( READ )
*                 dataset to be listed
*
*    Method :
*     Check that the object is a Scalar Structure, and then use
*     UTDIRT to list its contents.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     17-FEB-1983:  Original.  (UCL::JRG)
*     15-AUG-1984:  Help info in prologue  (RAL::AJC)
*    Global constants :
      include 'SAE_PAR'			! SAI Constants
      include 'DAT_PAR'                 ! Necessary for non-VMS
*    Status return :
      integer status			! status return
*    Local constants :
      integer INDENT			! Indentation step
      parameter(INDENT=3)
*    Local variables :
      character*(DAT__SZLOC) act	! Possibly Scalarised Locator
      character*(DAT__SZLOC) loc	! Object Locator
      character*(DAT__SZTYP) type	! Object type
      integer size			! Size as if vector
      integer ndim			! Number of object dimensions
      integer dims(DAT__MXDIM)		! Object dimensions
      logical struc			! Whether object is structure
*-

*    Get locator for DATASET parameter
      call dat_assoc('DATASET', 'READ', loc, status)
      if (status .eq. SAI__OK) then
*       Get object characteristics
         call dat_type(loc, type, status)
         call dat_struc(loc, struc, status)
         call dat_size(loc, size, status)
         call dat_shape(loc, DAT__MXDIM, dims, ndim, status)
         if (status .ne. SAI__OK) then
            call dat_erdsc(loc, status)
         elseif (struc) then
            if (ndim .gt. 1) then
               status = SAI__ERROR
               call msg_loc('LOC', loc)
               call err_rep(' ', '$^LOC : is not scalar structure',
     :           status)
            elseif (size .gt. 1) then
               status = SAI__ERROR
               call msg_loc('LOC', loc)
               call err_rep(' ', '$^LOC : is not a 1-D scalar'/
     :           /' structure', status)
            else
*             Scalarise if a single element vector
               if (ndim .eq. 1) then
                  call dat_cell(loc, 1, 1, act, status)
               else
                  call dat_clone(loc, act, status)
               endif
*             Print title
               call msg_out('BLANK', ' ', status)
               call msg_setc('TYPE', type)
               call msg_setc('PAR', 'DATASET')
               call msg_out('LS_START', '^TYPE  $^PAR', status)
               call msg_out('BLANK', ' ', status)
*             List contents
               call lsdir(act, INDENT, status)
               if (status .eq. SAI__OK) then
                  call msg_out('BLANK', ' ', status)
                  call msg_out('LS_END', 'END', status)
                  call msg_out('BLANK', ' ', status)
               endif
               call dat_annul(act, status)
            endif
         else
            status = SAI__ERROR
            call msg_loc('LOC', loc)
            call err_rep(' ', '$^LOC : is not a data structure', status)
         endif
         call dat_annul(loc, status)
      endif

      return
      end
