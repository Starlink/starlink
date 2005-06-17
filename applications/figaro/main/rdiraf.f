      subroutine rdiraf(file,im,axlen,naxis,dataptr,datatype,nelm,slot,
     :     status)
*+
* Name:
*    RDIRAF

* Invocation:
*    CALL RDIRAF(FILE,IM,AXLEN,NAXIS,DATAPTR,DATATYPE,NELM,SLOT,
*          STATUS)

* Purpose:
*  To read data in from an IRAF format file.

* Description:
*  To read data in from an IRAF format file.
*  This is a version of iraf_in to work within the FIGARO environment
*  using dynamic-memory allocation. This only deals with the pixel
*  array.
*
* Arguments:
*      FILE        (c* = INTEGER (Given)
*        Name of file
*      IM = INTEGER (Returned)
*        Index of file (pointer for use by IMFORT)
*      DATAPTR = INTEGER (Returned)
*        Pointer to data array (array index of
*                          dynamic_mem)
*      DATATYPE = CHARACTER*(*) (Returned)
*        Type of data (len(datatype)>=5)
*      NELM = INTEGER (Returned)
*        Number of elements in data array
*      SLOT = INTEGER (Returned)
*        Slot of virtual memory
*      STATUS = INTEGER (Returned)
*        Error status (0=ok)
*
*   T.N.Wilkins Cambridge 1/3/89
*        "          "   FIGARO version 16/3/89
*        "          "   13/8/91 Changed to use imgs.. (get section)
*                               routines
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      character*(*) file,datatype
      integer status,im,nstat
      integer dataptr,nelm,slot
      integer axlen(7),dtype,i,naxis

      status = 0

* Open data file

      call imopen(file,1,im,status)
      if(status.ne.0) then
        call par_wruser('Error opening file',nstat)
        goto 500
      end if

* Get size of data file

      call imgsiz(im,axlen,naxis,dtype,status)
      if(status.ne.0) then
        call par_wruser('Error getting file size',nstat)
        goto 500
      else if(naxis.gt.3) then
        call par_wruser('Sorry, I can only handle up to 3 axix',nstat)
        goto 500
      end if
      nelm = 1
      do i = 1, naxis
        nelm = nelm*axlen(i)
      end do
      if(dtype.eq.3) then
        datatype = 'short'

* assume real

      else
        datatype = 'float'
      end if
      call dsa_get_work_array(nelm,datatype,dataptr,slot,status)

* Read in data


*   If i*2 data then read into i*2 array

        if(dtype.eq.3) then
          if(naxis.eq.1) then
            call imgs1s(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),status)
          else if(naxis.eq.2) then
            call imgs2s(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),1,
     :                  axlen(2),status)
          else
            call imgs3s(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),1,
     :                  axlen(2),1,axlen(3),status)
          end if
        else

*   otherwise read into real array

          if(naxis.eq.1) then
            call imgs1r(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),status)
          else if(naxis.eq.2) then
            call imgs2r(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),1,
     :                  axlen(2),status)
          else
            call imgs3r(im,%VAL(CNF_PVAL(dataptr)),1,axlen(1),1,
     :                  axlen(2),1,axlen(3),status)
          end if
        end if

* If all is ok then return

      if(status.eq.SAI__OK) then
        return
      end if
      call par_wruser('Error reading from Iraf file',nstat)

* Error exit

  500 continue
      if(status.ne.0) then
        call irafemess(status)
      else

* Some non-IMFORT error

        status = 1
      end if
      end
