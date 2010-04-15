      subroutine runmed( y, N, len, work , save, NW,  err)
*+
* Name:
*    RUNMED

* Invocation:
*    CALL RUNMED( Y, N, LEN, WORK , SAVE, NW,  ERR)

* Purpose:
*   Smooth Y() by running medians of length LEN

* Description:
*   Smooth Y() by running medians of length LEN
*   Note the use of s3 for running medians of 3 instead of RUNMED
* Arguments:
*   WORK () is a local array in which data values are sorted.
*   SAVE () acts as a window on the data
      implicit none

      integer n,  len, NW, err, status
      real y(n), work(nw), save(nw)
*-
      integer  OK
      parameter( OK = 0)
      real temp, TWO
      parameter (TWO = 2.0)
      integer savept, smopt , lenp1 , i , j
      logical FOUND , LOOP

* functions
      real  MEDIAN


      if ( len .LE. NW) then
        do i = 1 , len
          work(i) = y(i)
          save(i) = y(i)
        end do
        savept = 1
        smopt = (len + TWO) / TWO
        lenp1 = len + 1
        do i = lenp1 , n

c          call sort(work , len , err)
          call gen_sortf(work , len)

          if (err .eq. OK ) then

            Y(smopt) = MEDIAN (work, len)
            temp = save(savept)
            FOUND = .FALSE.
            LOOP = .TRUE.
            J = 1
            do while (LOOP)

              if ( work(j) .EQ. Temp ) then

                FOUND = .TRUE.
                LOOP  = .FALSE.

              else
                if(J .LT. len) then
                  J = J + 1
                else
                  LOOP = .FALSE.
                end if
              end if
            end do

* has the point been found?
            if(FOUND) then

              work(j) = y(i)
              save(savept) = y(i)
              savept = mod(savept, len) + 1
              smopt = smopt + 1
            else
              err = 63
              go to 999
            end if


          else
            err = 63
            go to 999
          end if
        end do
        call gen_sortf(work , len)
c        call sort(work , len  , err)

        if (err .EQ. OK) then
          y(smopt) = median(work, len)
        end if
      end if
  999 call par_wruser(' ',status)
      end
