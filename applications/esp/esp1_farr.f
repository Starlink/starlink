      subroutine esp1_farr (fioid,indf,maxrow,maxcol,a,al,
     :     ncolmin,ncolmax,nrow,status)
*+
*  Name:
*    esp1_farr
*
*  Purpose:
*    Reads an array of numbers from a text file.
*
*  Description:
*
*    The routine is provided with a file descriptor, opened using
*    fio_open (see SUN/143) and two arrays `a(maxrow,maxcol)' and
*    `al(maxrow)', which are filled with the array contents and array
*    line lengths respectively.  If there are more rows or columns than
*    will fit into the array, the extras are silently ignored.
*
*    If the `indf' parameter is passed non-zero, it is taken to be an
*    NDF identifier, and the first two columns in each row are parsed as
*    coordinates, using the WCS information found in the NDF.
*
*    The input file may have `structured comments' within it, on lines
*    starting with the string `##'.  These lines consist of `## key =
*    value'.  At present, the only key and value recognised is
*    `wcsdomain=grid', and this is recognised simply by matching the
*    line.  However, this may, in principle, be generalised in future,
*    so that applications which write files for consumption by ESP
*    should avoid starting lines with the `##' comment string.
*
*    If the `wcsdomain=grid' comment is found, then it is taken to
*    indicate that the coordinates in the file are in the WCS GRID
*    domain, and do not need to be converted from WCS Current
*    coordinates.  The only application to exploit this is the GAIA ESP
*    interface.
*
*  Language:
*    Fortran 77
*
*  Arguments:
*    fioid = integer (given)
*      File id for the input file.
*    indf = integer (Given)
*      Identifier for the NDF which the coordinates describe.  If this
*      is non-zero, and there are at least two coordinates on the input line,
*      then the first two will be taken to be coordinates, and parsed
*      appropriately, using coordinate transformations as specified in
*      this NDF, and using the function esp1_s2pr.
*    maxrow = integer (given)
*      Row dimension of the A array, and maximum number of rows to
*      be read.
*    maxcol = integer (given)
*      Column dimension of the A array, and maximum number of columns to
*      be read.
*    a(maxrow,maxcol) = real (returned)
*      Array to be filled with read numbers.
*    al(maxrow) = integer (returned)
*      al(i) is set to the number of good elements in row i of array a()
*    ncolmin = integer (returned)
*      The number of columns in the shortest row.
*    ncolmax = integer (returned)
*      The number of columns in the longest row.
*    nrow = integer (returned)
*      The number of rows actually read.
*    status = integer (given and returned)
*      The inherited status.
*
*  Authors:
*    NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*    21-NOV-1999 (NG)
*      Initial version
*
*  Bugs:
*    None known.
*-

*   Type definitions
      implicit none

*   Global constants
      include 'SAE_PAR'         ! Standard SAE constants
      include 'FIO_ERR'         ! FIO error definitions

*   Arguments given
      integer fioid             ! input file
      integer indf              ! Input NDF
      integer maxcol, maxrow    ! Size of array

*   Arguments returned
      real a(maxrow,maxcol)     ! array to be filled
      integer al(maxrow)        ! Array of row lengths
      integer ncolmin, ncolmax, nrow ! Actual number of elements read

*   Status
      integer status            ! global status

*   Local
      character *(80) buffer    ! Character string input from file
      integer nchar             ! number of characters read into buffer
      integer row, col, i       ! loop counters
      integer indexs(maxcol)    ! start and end of a word in the buffer
      integer indexe(maxcol)
      character*80 words(maxcol)! list of words obtained from file
      logical finishedcols      ! loop flag
      logical finishedrows      ! loop flag
      logical nonblankstring    ! flag
      logical gridcoords        ! are input coordinates in WCS grid domain?
      integer lstatus           ! local status for chr_dcwrd


*   Check inherited status
      if (status.ne.sai__ok) return

*   Initialise
      ncolmax = -1
      ncolmin = maxcol
      finishedrows = .false.
      gridcoords = .false.

*   Start error context
      call err_mark

*   Run through the rows.  Jump out of this loop if we have any errors,
*   or when we run out of file.
      row = 0
      do while ((row.lt.maxrow) .and. .not.finishedrows)

*      Read a line
         call fio_read (fioid,buffer,nchar,status)
         if (status .ne. sai__ok) then
*         End of file
            finishedrows = .true.
            call err_annul(status)

         else


*         Find the first non-blank character, indexs(1) will be returned
*         greater than indexs(2) if the string is blank.
            call chr_fandl (buffer, indexs(1), indexs(2))
            nonblankstring = (indexs(1).lt.indexs(2))

*         Check for structured comment: a line beginning `##'
            if (nonblankstring .and.
     :           buffer(1:1).eq.'#' .and. buffer(2:2).eq.'#') then
               call chr_rmblk (buffer)
               call chr_lcase (buffer)
               if (buffer.eq.'##wcsdomain=grid') then
                  gridcoords = .true.
               else
                  call msg_setc('LINE',buffer)
                  call msg_out(' ',
     :                 'Unrecognised structured comment: ^LINE',status)
               endif
               nonblankstring = .false.
            endif

*         Check for comment lines (starting # or !) or blank line
            if (nonblankstring
     :           .and. (buffer(1:1).ne.'#')
     :           .and. (buffer(1:1).ne.'!')) then

               call chr_dcwrd (buffer, maxrow, col,
     :              indexs, indexe, words, lstatus)

*            We've split up the line, now convert the `words' to numbers
               i = 1
               do while (i.le.col .and. status.eq.sai__ok)

                  if (i.eq.1 .and. .not.gridcoords
     :                 .and. indf.ne.0 .and. col.ge.2) then

*                  Convert coordinates using esp1_s2pr
                     call esp1_s2pr(indf,words(1),words(2),
     :                    a(row+1,1),a(row+1,2),status)

                     i = i + 2

                  else

                     call chr_ctor (words(i), a(row+1,i), status)
                     i = i + 1

                  endif

*               If there was an error, stop
                  if (status.ne.sai__ok) then
                     finishedcols = .true.
                     finishedrows = .true.
                  endif

               enddo

               if (.not.finishedrows) then
*               We've successfully read a row -- increment the counter
                  row = row+1

                  al(row) = col
                  if (col .lt. ncolmin) ncolmin = col
                  if (col .gt. ncolmax) ncolmax = col
               endif

            endif               ! End of `if ((buffer.ne.' ')...'
         endif                  ! End of `if (status.ne.sai__ok)' (fio_read)
      enddo

 100  continue
      nrow = row

*   Display error messages, and tidy up the error system
      if (status .ne. sai__ok) then
         if (status .eq. fio__eof) then
            call err_annul(status) ! No problem
         else
            call msg_out (' ','Errors found reading the data file.',
     :           status)
         endif
      endif

*   end the error context
      call err_rlse

      end

* $Id$
