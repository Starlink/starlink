/* static char sccsid[] = "@(#) ST-ECF as/src/cotr.c	4.1	12/6/91"; */
/*++++++
.TYPE           Module
.IDENTIFICATION cotr.c
.VERSION 1.0	26-Feb-1987: Creation .
.VERSION 1.1	24-Oct-1988: Changed Names
.VERSION 1.2	03-May-1989: Changed statics
.Language       C
.AUTHOR         Francois Ochsenbein [ESO-IPG]  
.KEYWORDS       Spherical Coordinate transformations

.ENV            

.COMMENTS
The routines provided in this module all deal with coordinate transformations. 
All spherical coordinates are assumed to be expressed in DEGREES.
No function is traced.

\begin{TeX}

The parameter mnemonics are:

\begin{itemize}
 \item o = array $[\alpha,\delta]$ of spherical coordinates,
		expressed in degrees.
 \item R = $3 \times 3$  Rotation (orthogonal)   matrix from old to new coordinate frame
 \item u = vector $[x,y,z]$  of  Unit (cosine) direction $(x^2+y^2+z^2=1)$
\end{itemize}
\end{TeX}

-----------------------------------------------------------------------*/

#include "trigo.h"		/* Definitions */
#include "ok.h"	

#define ra           o[0] 
#define dec          o[1]
#define x            u[0]
#define y            u[1]
#define z            u[2]
#define X            p[0]
#define Y            p[1]

#define ze	Euler_angles[0]
#define theta	Euler_angles[1]
#define zeta	Euler_angles[2]

/*============================================================================*/
int tr_Euler (Euler_angles, R)   
/*+++
.PURPOSE  Compute the rotation matrix from Euler angles 
.METHOD
\begin{TeX}
$(z, \theta, \zeta)$. This rotation matrix is actually defined by
$$ R = R_z(-z) \cdot R_y(\theta) \cdot R_z(-\zeta)$$
(from old to new frame).
\end{TeX}
.RETURNS OK
---*/
	double Euler_angles[3];	/* IN: Euler angles (z, theta, zeta) */
	double R[3][3];  	/* OUT: rotation matrix */

{ 	double w;

  R[0][2] =  cosd(ze);
  R[1][2] =  sind(ze);
  R[2][0] =  cosd(zeta);
  R[2][1] =  sind(zeta);
  R[2][2] =  cosd(theta);
  w       =  sind(theta);
  R[0][0] =  R[2][0]*R[2][2]*R[0][2] - R[2][1]*R[1][2];
  R[1][0] =  R[2][0]*R[2][2]*R[1][2] + R[2][1]*R[0][2];
  R[0][1] = -R[2][1]*R[2][2]*R[0][2] - R[2][0]*R[1][2];
  R[1][1] = -R[2][1]*R[2][2]*R[1][2] + R[2][0]*R[0][2];
  R[2][0] =  R[2][0]*w;
  R[2][1] = -R[2][1]*w;
  R[0][2] = -w*R[0][2];
  R[1][2] = -w*R[1][2];

  return(OK);
}

/*============================================================================*/
int tr_oo( o , o2, R)
/*+++
.PURPOSE  Rotate polar coordinates,
           using a R rotation matrix (old to new frame)
.METHOD	  Use unit vectors
.RETURNS  OK    
---*/
	double o[2];  	/* IN: Original angles */
	double o2[2]; 	/* OUT: rotated angles  */
	double R[3][3];	/* IN: Rotation matrix */
{ 
	double us[3];

  tr_ou ( o, us);     		/* tranforms polar angles into dir cos */
  tr_uu (us, us, R);		/* rotates dir cos                     */
  return(tr_uo(us, o2)); 	/* transform unit vector to angles     */
}

/*============================================================================*/
int tr_oo1( o , o2 , R) 
/*+++
.PURPOSE   Rotate polar coordinates, using the inversed R matrix 
           (new to old frame).
.METHOD	  Use unit vectors
.RETURNS   Non-zero value
---*/
	double o[2];  	/* IN: original angles */
	double o2[2]; 	/* OUT: rotated angles  */
	double R[3][3];	/* IN: rotation matrix */
{ 
	double us[3];

  tr_ou ( o, us);         /* tranforms polar angles into dir cos */
  tr_uu1 (us, us, R);    /* rotates dir cos                     */
  return(tr_uo(us, o2)); /* transform unit vector to angles     */
}

/*============================================================================*/
int tr_oR ( o , R ) 
/*+++
.PURPOSE  Creates the rotation matrix R[3][3] defined as
\begin{TeX}
\begin{itemize}
\item R[0] (first row) = unit vector towards Zenith
\item R[1] (second row) = unit vector towards East
\item R[2] (third row) = unit vector towards North
\end{itemize}
The resulting $R$ matrix can then be used to get the components $\vec{v}_{loc}$ 
of a vector $\vec{v}$ in the local frame, as {\tt tr\_uu}($\vec{v}$, 
$\vec{v}_{loc}$, $R$).
\end{TeX}
.RETURNS OK
---*/
	double o[2]; 	/* IN: original angles */
	double R[3][3];	/* OUT: rotation matrix */
{
  R[2][2] =  cosd(dec);
  R[0][2] =  sind(dec);
  R[1][1] =  cosd(ra);
  R[1][0] =  -sind(ra);
  R[1][2] =  0.e0;
  R[0][0] =  R[2][2] * R[1][1];  
  R[0][1] = -R[2][2] * R[1][0];
  R[2][0] = -R[0][2] * R[1][1];
  R[2][1] =  R[0][2] * R[1][0];
  return(OK);
}

/*============================================================================*/
int tr_ou (o , u)  
/*+++
.PURPOSE  Transformation from polar coordinates to Unit vector
.RETURNS  OK
---*/
	double o[2]; 	/* IN: angles ra + dec in degrees */
	double u[3]; 	/* OUT: dir cosines                */

{ 	double cosdec;

  cosdec = cosd(dec);
  x = cosdec * cosd(ra);
  y = cosdec * sind(ra);
  z = sind(dec);

  return (OK);
}  

/*============================================================================*/
int tr_uo (u,o)   
/*+++
.PURPOSE Computes angles from direction cosines
.RETURNS OK / NOK (x=y=z=0)
---*/
	double u[3]; 	/* IN: Dir cosines */
	double o[2]; 	/* OUT: Angles ra + dec in degrees */
{
	double r2;  		/* sqrt(x*x+y*y) */

  r2 = x*x + y*y; ra =0.e0;
  if (r2  == 0.e0)                                       /* in case of poles */
  { 	if (z == 0.e0) 	return(NOK);
	dec = ( z>0.e0 ? 90.e0 : -90.e0); 
	return(OK); 
  }

  dec = atand  ( z / sqrt(r2));
  ra  = atan2d (y , x );
  if (ra < 0.e0) ra += 360.e0;
  return (OK);
} 

/*============================================================================*/
int tr_uR ( u , R )   
/*+++
.PURPOSE   Creates the rotation matrix R[3][3] with
\begin{TeX}
\begin{itemize}
\item R[0] (first row) = unit vector towards Zenith
\item R[1] (second row) = unit vector towards East
\item R[2] (third row) = unit vector towards North
\end{itemize}
\end{TeX}
.RETURNS OK
.REMARKS For the poles,
\begin{TeX}
  ($|z|=1$), the rotation axis is assumed be the $y$ axis, i.e.
  the right ascension is assumed to be 0.
\end{TeX}
---*/
	double u[3]; 	/* IN: Original direction */
	double R[3][3]; /* OUT: Rotation matrix    */
{
  R[0][0] = x; R[0][1] = y; R[0][2] = z;
  R[2][2] = hypot ( x , y );
  R[1][0] = 0.e0;
  R[1][1] = 1.e0;		/* These are defaults for poles	*/
  R[1][2] = 0.e0;
  if (R[2][2] != 0.e0)
  {	R[1][1] = x  / R[2][2];
	R[1][0] = -y / R[2][2];
  }
  R[2][0] = -R[0][2] * R[1][1];
  R[2][1] =  R[0][2] * R[1][0];
  return(OK);
}

/*============================================================================*/
int tr_uu( u1 , u2 , R ) 
/*+++
.PURPOSE  Rotates the unit vector u1 to u2, as
\begin{TeX}
\quad $\vec{u_2} = R \cdot  \vec{u_1}$ \quad (old to new frame)
\end{TeX}
.RETURNS OK
---*/
	double u1[3]; 	/* IN: Unit vector */
	double u2[3]; 	/* OUT: Resulting unit vector after rotation */
	double R[3][3];	/* IN: rotation matrix (e.g. created by tr_oR)*/
{
 	register int i,j;
	register double val;
	double u_stack[3];	/* allows same address for input/output      */


  for (i=0; i<3; i++)
  { 	val = 0.e0;
	for (j=0; j<3; j++)	val += R[i][j]*u1[j];
	u_stack[i] = val;
  }
  for (i=0; i<3; i++)		u2[i] = u_stack[i]; 	/* copies to output */
  return(OK);
}

/*============================================================================*/
int tr_uu1 ( u1 , u2 , R)   
/*+++
.PURPOSE  Rotates the unit vector u1 to u2, as
\begin{TeX}
\quad $\vec{u_2} = R^{-1} \cdot \vec{u_1}$ \quad (new to old frame).
\end{TeX}
.RETURNS OK
---*/
	double u1[3]; 	/* IN: Unit vector */
	double u2[3]; 	/* OUT: Resulting unit vector after rotation */
	double R[3][3];	/* IN: rotation matrix (e.g. created by tr_oR) */
{
	register int i,j;
	register double val;
	double u_stack[3];	/* allows same address for input/output      */

  for (i=0; i<3; i++)
  { 	for (j=0, val = 0.0e0; j<3; j++)
  		val += R[j][i]*u1[j];
	u_stack[i] = val;
  }
  for (i=0; i<3; i++) 		u2[i] = u_stack[i];	/* copies to output */
  return(OK);
}

/*============================================================================*/
int tr_RR ( A , B , R)   
/*+++
.PURPOSE  Product of orthogonal matrices
\begin{TeX}
\quad $B = R \cdot A$ \quad 
\end{TeX}
.RETURNS OK
---*/
	double A[3][3]; 	/* IN: First Matrix */
	double B[3][3]; 	/* OUT: Result Matrix */
	double R[3][3]; 	/* IN: Rotation Matrix	*/
{
	register int i, j, k;
	double	val;
	double Rs[3][3];		/* Local copy	*/

  for (i=0; i<3; i++)   	for (j=0; j<3; j++)
  {
    	for (k=0, val=0.0e0; k<3; k++)	val += R[i][k]*A[k][j];
	Rs[i][j] = val;
  }

  for (i=0; i<3; i++)   	for (j=0; j<3; j++)
  	B[i][j] = Rs[i][j];

  return(OK);
}

/*============================================================================*/
int tr_RR1 ( A , B , R)   
/*+++
.PURPOSE  Product of orthogonal matrices
\begin{TeX}
\quad $B = R^{-1} \cdot A$ \quad 
\end{TeX}
.RETURNS OK
---*/
	double A[3][3]; 	/* IN: First Matrix */
	double B[3][3]; 	/* OUT: Result Matrix */
	double R[3][3]; 	/* IN: Rotation Matrix	*/
{
	register int i, j, k;
	double	val;
	double Rs[3][3];		/* Local copy	*/

  for (i=0; i<3; i++)   	for (j=0; j<3; j++)
  {
    	for (k=0, val=0.0e0; k<3; k++)	val += R[k][i]*A[k][j];
	Rs[i][j] = val;
  }

  for (i=0; i<3; i++)   	for (j=0; j<3; j++)
  	B[i][j] = Rs[i][j];

  return(OK);
}

   
                                                              

                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
          
