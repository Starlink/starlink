*+LIN_INTERP - Perform a linear interpolation.
	subroutine lin_interp(x1,fx1,x2,fx2,x,res)
	implicit none

* Input:
	double precision x1
	double precision fx1
	double precision x2
	double precision fx2
	double precision x

* Output:
	double precision res

* P. McGale - Aug 92.
* P McGale May 95 - UNIX mods
*-

* Local variables.
	double precision grad

	if (x1 .eq. x2) then
	  grad = 0.0
	else
	  grad = (fx2-fx1)/(x2-x1)
	endif

	res = fx1 + grad*(x-x1)

	end
