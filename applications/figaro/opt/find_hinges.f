      subroutine Find_Hinges( x, y, L, Hu, Hl, err)
*+
* Name:
*    FIND_HINGES

* Invocation:
*    CALL FIND_HINGES( X, Y, L, HU, HL, ERR)
* Description:
*   Just as the MEDIAN splits an ordered data set in half, the HINGES
*   or the summary values are the middle of each of these halfs.
*   These form the basis of robust extimators of the spread and centre
*   of a distribution.
*   This routine returns the Uppper and Lower Hinges HU and HL for
*   the input frequency distribution (Y,X). It is assumed that
*   X is in ASCENDING ORDER already and NO checks are Carried out here
*   for that condition, it is therfore essential for any calling programme
*   to have tested this first.
*

      Implicit None
      integer L
      real X(L), Y(L)

* local variables
      real D, HL, HU, T, Total_counts
      Integer I, K , LP1MI
      logical Dfound

* error returns

      integer status, err
      integer lp1
*-
      K = L - 1
      lp1 = L + 1
      Total_counts = 0.0

* form total counts in the line. Note that if there is a background
* this must have been taken care of first - otherwise we will be
* working with a distribution with big tails.

      Do i = 1,l
        Total_counts = Total_counts + y(i)
      end do

      D = 0.5 * (1.0 + aint( 0.5* (Total_counts + 1.0)))

* if the lower hinge falls in the left-open bin then its an error

      if (d .le. Y(1) )  then
        Call par_wruser('Error : Lower Hinge in First Bin',status)
        err = 92
        go to 999
      end if

* Search for the Lower Hinge
      t = y(1)
      Dfound = .false.
      i = 2

      do while ((.Not.Dfound) .And. (I.Le.K) )
        t = t + y(i)
        if (t .ge. d) then
          dfound = .TRUE.
        else
          i = i + 1
        end if
      end do

* lower hinge falls in the right open bin --> also an error

      if (.NOT.Dfound) then
        err = 92
        Call par_wruser('Error : Lower Hinge in Last Bin',
     :  status)
        go to 999
      end if

* find lower hinge by interpolation

      t = t - y(i)
      hl = x(i-1) + (x(i) - x(i-1) ) * (d - t - 0.5) / y(i)

* now perform similar checks and find the upper hinge. This time we
* count in from the RHS of the data.

      if (d .ge. Y(L) )  then
        Call par_wruser('Error : Upper Hinge in Last Bin',status)
        err = 92
        go to 999
      end if
      t = y(L)
      Dfound = .false.
      i = 2

      do while ((.Not.Dfound) .And. (I.Le.K) )
        lp1mi = lp1 - i
        t = t + y(lp1mi)

        if (t .ge. d) then
          dfound = .TRUE.
        else
          i = i + 1
        end if
      end do

* Upper hinge falls in the right open bin --> also an error

      if ( .NOT.Dfound) then
        err = 92
        Call par_wruser('Error : Upper Hinge in Last Bin',
     :  status)
        go to 999
      end if

      t = t - y(lp1mi)
      hu = x(lp1mi) - (x(lp1mi) - x(lp1mi-1) ) * (d - t - 0.5)
     :                   / y(lp1mi)

  999 call Par_wruser(' ',status)

      end
