*+ DAT_PREC - Enquire the storage-precision for primitive object
      subroutine dat_prec(loc, nbyte, status)
*    Description :
*     This routine returns the number of basic machine units (bytes)
*     used to store a single value of the specified data object.
*    Invocation :
*     CALL DAT_PREC(LOC; NBYTES, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     NBYTES=INTEGER
*           Variable to receive the number of machine units (bytes)
*           needed to store a single data value.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get object type using DAT_TYPE.
*     Go through list of types, and return element size in bytes.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     05.11.1984:   Calls to error system removed (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object locator
*    Export :
      integer nbyte			! Number of bytes per type
*    Status return :
      integer status			! Status Return
*    External references :
      logical chr_simlr			! Caseless string equality
*    Local constants :
      integer MAXTYP			! Number of types
      parameter(MAXTYP=9)
*    Local variables :
      character*(DAT__SZTYP)		! SDS types
     :  types(MAXTYP)
      integer nbytes(MAXTYP)		! Number of bytes per type
      character*(DAT__SZTYP) type	! Access type
      integer itype			! Type index
*    Local data :
      data types / '_DOUBLE', '_REAL', '_INTEGER', '_LOGICAL',
     :  '_CHAR', '_UWORD', '_WORD', '_UBYTE', '_BYTE'/
      data nbytes / 8, 4, 4, 4, 1, 2, 2, 1, 1/
*-

      if (status .eq. SAI__OK) then

*       Get object type
         call dat_type(loc, type, status)
         if (status .ne. SAI__OK) then
            continue
         else

*          Go through type list
            itype = MAXTYP
            dowhile (itype .gt. 0)
               if (chr_simlr(type, types(itype))) then
                  nbyte = nbytes(itype)
                  goto 1
               endif
               itype = itype - 1
            enddo
 1          continue

*          If no match was found, let DAT_LEN see if it is a string
            if (itype .lt. 1) then
               call dat_len(loc, nbyte, status)
            endif

         endif

      endif

      end
