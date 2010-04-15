*+  CHI_ADECODE - Analyses the ASCII string and returns its type and format spec
      subroutine chi_adecode(expression, type, width, format_spec,
     :                      string, status)
*    Description :
*     Analyses the ASCII expression and returns its type and format spec.
*    Invocation
*     CALL CHI_ADECODE(EXPRESSION, TYPE, WIDTH, FORMAT_SPEC, STRING, STATUS)
*    Parameters :
*     EXPRESSION=CHAR(INPUT)
*           The expression to be analysed.
*     TYPE=CHAR(OUTPUT)
*           The data type of the expression: C, D, I, L, R.
*     WIDTH=INTEGER(OUTPUT)
*           The field width.
*     FORMAT_SPEC=CHAR(OUTPUT)
*           The format spec of the expression
*     STRING=CHAR(OUTPUT)
*           The input string with leading blanks and apostrophes removed
*     STATUS=INTEGER(UPDATE)
*           Status variable.
*           If the string is too longer than 99999 bytes then
*           Status = CHI__DECOD is returned.
*           Else if the string cannot be decoded then
*           Status = CHI__DECOD is returned
*           STRING is set to EXPRESSION on error end
*    Method :
*     Parse assuming the string is a FORTRAN constant.
*     Note : Text constants are delimted by the: " character, not: '.
*     If the parsing fails then
*        It is an ASCII text string
*     Endif
*     Clear the error handler flag
*    Deficiences :
*     Integer, real, double precision, logical, character constants only.
*     Octal, hexadecimal constants etc not parsed for.
*    Authors :
*     Alan Wood (STADAT::ARW)     Jon Fairclough (RAL::IPMAF)
*    History :
*     30-Jan-1992:Original
*    Type Definitions :
      implicit none
*    Global constants :
      include 'sae_par'                 ! SAI Symbolic Constants
      include 'chipar_par'
      include 'chipar_err'
*    Import
      character*(*) expression
*    Export
      integer width                     ! Width of field
      character*(*) type
      character*(*) format_spec
      character*(*) string
*    Status :
      integer status
*    External refernces
*    Local constant
      integer szmx                      ! Maximium length of w
      parameter (szmx=5)                ! Limit of 99999 bytes !!!!!
*    Local variables :
      integer i,j                       ! Counters
      integer l                         ! Length of string
      logical start_str                 ! Start of string
      logical end_str                   ! End of string flag
      integer digits                    ! Number of digits in fraction
      character*(szmx) w                ! "w" part of format
      integer wsz                       ! Number of digits in "w"
      character*(szmx) d                ! "d" part of format
      integer dsz                       ! Number of digits in "d"
      integer locE                      ! Location of "E"
      integer locD                      ! Location of "D"
      integer dot                       ! Location of "."
      character*20 ctest
      double precision dtest
      integer itest
      logical ltest
      real rtest
      integer istat
      logical error
*-
*    Begin
*
      if (status .ne. SAI__OK) return
*
*    Initialise variables
*
      string = expression
      width  = 0
      type   = ' '
      format_spec = ' '
      d = ' '
      w = ' '
*
*    Remove leading blanks
*
      i = 0
      l = len(expression)
      start_str = .false.
      do while (i .lt. l .and. .not. start_str)
         i = i + 1
         if (expression(i:i) .ne. ' ')then
            start_str = .true.
         endif
      enddo
*
*    Remove trailing blanks
*
      if (start_str) then
         j = l
         end_str = .false.
         do while (j .gt. i .and. .not. end_str)
            if (expression(j:j) .eq. ' ') then
               j = j - 1
            else
               end_str = .true.
            endif
         enddo
*
*       Set width
*
         width = j - i + 1
*
*       Check for CHAR
*
         if (expression(i:i) .eq. CHI__DELIM) then
*          Leading delimiter present
            if (expression(j:j) .eq. CHI__DELIM) then
*             Trailing delimiter present
               if (j .gt. i + 1) then
*                Characters between apostrophe
                  string = expression (i+1 : j-1)
                  type = 'C'
                  format_spec = 'A'
                  width = width - 2
               else
                  string = expression(i:j)
                  goto 2
               endif
            else
               string = expression(i:j)
               goto 2
            endif
         else
*
*          Check for other  types
*
*          Look for dot
            dot = index(expression(i:j), '.')
*          Test for INTEGERS (with exponeents)
            if (dot .eq. 0) then
               locE = index(expression(i:j),'E')
               if (locE .gt. 1) then
                  type = 'R'
                  width = width + 1
*                Set the "w" part of format statement.
                  wsz    = int(log10(real(width))) + 1
                  write (w, 10, err=1) width
   10             format(i<wsz>)
                  format_spec = 'E'//w(:wsz)//'.0'
                  string = expression(i:i+locE-2)//'.'//
     :                     expression(i+locE-1:j)
               else
*                Set the "w" part of format statement.
                  wsz    = int(log10(real(width))) + 1
                  write (w, 10, err=1) width
                  type = 'I'
                  format_spec = 'I'//w
                  string = expression(i:j)
               endif
            else
*             Set the "w" part of format statement.
               wsz    = int(log10(real(width))) + 1
               write (w, 10, err=1) width
*             Test for LOGICAL
               if (index(expression(i:j),'.TRUE.') .or.
     :             index(expression(i:j),'.FALSE.')) then
                  type = 'L'
                  format_spec = 'L'//w
                  string = expression(i:j)
               else
*                Test for REAL, DOUBLE
                  locE = index(expression(dot:j),'E')
                  locD = index(expression(dot:j),'D')
                  if (locE .ne. 0) then
                     type = 'E'
                     digits = locE - 2
                  elseif (locD .ne. 0) then
                     type = 'D'
                     digits = locD - 2
                  else
                     if (wsz .gt. 1) then
                        type = 'D'
                     else
                        type = 'F'
                     endif
                     digits = j - dot - i + 1
                  endif
                  if (digits .le. 0) then
                     dsz = 1
                  else
                     dsz = int(log10(real(digits))) + 1
                  endif
                  write (d, 20, err=1) digits
   20             format (I<dsz>)
                  if (type(:1) .eq. 'F') then
                     format_spec = 'F'//w(:wsz)//'.'//d
                     type = 'R'
                  elseif (type(:1) .eq. 'E') then
                     format_spec = 'E'//w(:wsz)//'.'//d
                     type = 'R'
                  elseif (type(:1) .eq. 'D') then
                     format_spec = 'D'//w(:wsz)//'.'//d
                  endif
                  string = expression(i:j)
               endif
            endif
         endif
*
*       Test the type and format spec
*
*
         if (type(:1) .eq. 'C') then
            read(string, '('//format_spec//')', err=2) ctest
         elseif (type(:1) .eq. 'D') then
            read(string, '('//format_spec//')', err=2) dtest
         elseif (type(:1) .eq. 'I') then
            read(string, '('//format_spec//')', err=2) itest
         elseif (type(:1) .eq. 'L') then
            read(string, '('//format_spec//')', err=2) ltest
         elseif (type(:1) .eq. 'R') then
            read(string, '('//format_spec//')', err=2) rtest
         else
            goto 2
         endif
      else
         goto 2
      endif
*
      goto 4
*
1     continue
      status = CHI__DECOD
      goto 3
*
2     continue
      status = CHI__DECOD
      goto 3
*
3     continue
*    Assume that is of CHAR type !
      type = 'C'
      format_spec = 'A'
      string = expression
*
*    Debug info
*
4     continue
*
      end
