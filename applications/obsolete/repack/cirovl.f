*+CIROVL Gets the overlap area of two circles
	REAL FUNCTION CIROVL(R1, R2, SEP)
	IMPLICIT 	NONE
	INCLUDE		'CONSTANTS.INC'
* Input
	REAL		R1, R2, SEP
* Local
	REAL		THETA1, THETA2, AREA1, AREA2
	REAL		TAREA2

* trap the easy ones
	if (sep .gt. (r1+r2)) then
	  CIROVL = 0.
	  return
	elseif (sep .lt. (r1-r2)) then
	  CIROVL = 1.
	  return
	endif

	tarea2 = pi*r2**2
	theta2=2.0*acos((r2**2+sep**2-r1**2)/(2.0*sep*r2))
	if (theta2 .eq. pi) then
	  CIROVL=0.5*pi*(r2**2)/tarea2
	else if (theta2 .lt. pi) then
	  theta1=2.0*acos((sep-r2*cos(theta2/2.0))/r1)
	  area1=0.5*(theta1-sin(theta1))*(r1**2)
	  area2=0.5*(theta2-sin(theta2))*(r2**2)
	  CIROVL=(area1+area2)/tarea2
	else if (theta2 .gt. pi) then
	  theta1=2.0*acos((sep-r2*cos(theta2/2.0))/r1)
	  area1=0.5*(theta1-sin(theta1))*(r1**2)
	  theta2=(2.0*pi)-theta2
	  area2=tarea2-0.5*(theta2-sin(theta2))*(r2**2)
	  CIROVL=(area1+area2)/tarea2
	endif

	end
