      subroutine c2ldel(array,lrow,urow,lcol,ucol,rowcol,indxcr,status)
*+
* Name:
*    C2LDEL

* Invocation:
*    CALL C2LDEL(ARRAY,LROW,UROW,LCOL,UCOL,ROWCOL,INDXCR,STATUS)

* Purpose:
*    Delete row or column in 2d character array

* Description:
*    Given a 2D CHARACTER array, and a column (or row) within
*    that array
*    this subroutine deletes that column (or row). It does so by
*    "squeezing" the array in the appropiate dimension, so that all
*    values migrate towards the lower boundary.
*    The routine can also handle 1D arrays if :
*
*    LROW = 1, UROW = 1, ROWCOL = 'COLUMNS',
*    and INDXCR = position of entry to be deleted.
*
*
*
* Calling sequence :
*    CALL C2LDEL (ARRAY, LROW, UROW, LCOL, UCOL, ROWCOL, INDXCR, STATUS)
*
*
* Arguments:
*     LROW = INTEGER (Given)
*        Lower row boundary of ARRAY
*     UROW = INTEGER (Given)
*        Upper row boundary of ARRAY
*     LCOL = INTEGER (Given)
*        Lower column boundary of ARRAY
*     UCOL = INTEGER (Given)
*        Upper column boundary of ARRAY
*     ROWCOL = CHARACTER*(*) (Given)
*        If this begins with a "R" or "r",
*                                   then a row is deleted
*     INDXCR = INTEGER (Given)
*        Column or row to be deleted
*     ARRAY(LROW:UROW,LCOL:UCOL) = CHARACTER*(*) ARRAY (Given and returned)
*        Array containing values
*     STATUS = INTEGER (Returned)
*        Status return
*                         0 = O.K.
*                         1 = Invalid row given
*                         2 = Invalid column given
*
* Variables :
*   COL    : Local loop counter for moving through columns of ARRAY
*   NEWCOL : Pointer to new column in the array
*   NEWROW : Pointer to new row in the array
*   ROW    : Local loop counter for moving through rows of ARRAY
*   ROWDEL : .TRUE. if rows are being deleted
*
*
* Author
*    DJA            Date :  09-Mar-1984
*
*-
      implicit none
      include 'SAE_PAR'
*
      integer status,lrow,urow,lcol,ucol,indxcr,newrow,newcol,row,col
      character*(*) array(lrow:urow, lcol:ucol)
      character*(*) rowcol
      logical rowdel
*
*
* Check which are being deleted, rows or columns.
*
      rowdel = ((rowcol(1:1) .eq. 'R') .or. (rowcol(1:1) .eq. 'r'))
*
* Check for the validity of the entry line being deleted.
*
      if(status.ne.SAI__OK) return
      if (rowdel) then
        if ((indxcr .lt. lrow) .or. (indxcr .gt. urow)) then
          status = SAI__ERROR
          goto 9999
        end if
      else
        if ((indxcr .lt. lcol) .or. (indxcr .gt. ucol)) then
          status = SAI__ERROR
          goto 9999
        end if
      end if
*
* Delete the required line by "squeezing" the arc line list. This is
* done with two pointers, the first (ROW or COL) pointing to the old
* position, and the second (NEWROW or NEWCOL) pointing to the position
* in the modified list.
*
      if (rowdel) then
*
*     Step through rows, and delete the appropiate one.
*
        newrow = lrow - 1
        do row = lrow, urow
          if (row .ne. indxcr) then
*
*     Include the line in the list.
*
            newrow = newrow + 1
            do col = lcol, ucol
              array(newrow,col) = array(row,col)
            end do
          end if
        end do
*
      else
*
*           Step through columns, and delete the appropiate one.
*
        newcol = lcol - 1
        do col = lcol, ucol
          if (col .ne. indxcr) then
*
*     Include the line in the list.
*
            newcol = newcol + 1
            do row = lrow, urow
              array(row, newcol) = array(row, col)
            end do
*
          end if
        end do
*
      end if
*
 9999 continue
      end
