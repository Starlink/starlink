/* static char sccsid[] = "@(#) ST-ECF as/src/jprec.c	4.1	12/6/91"; */
/*+++++++++++++++       
.TYPE           Module
.IDENTIFICATION jprec.c
.VERSION 1.0	27-Feb-1987: Creation .
.VERSION 1.1	26-Oct-1988: Cosmetic modifications
.Language       C
.AUTHOR         Francois Ochsenbein [ESO-IPG]  
.KEYWORDS       Precession of Coordinates in new IAU system.

.ENV            

.COMMENTS
\begin{TeX}
This module uses the new (IAU 76) precession constants,
and assumes the FK5 system. 
Precession constants are taken from Lederl\'e and
Schwan (Astron. Astrophys. {\bf 134}, 1, 1984),
Liske J.H. (Astron. Astrophys. {\bf 73}, 282, 1979),
Dates must be expressed in {\em Julian Years}.

The precession may be applied on unit vectors (mnemonic {\tt u}), or
on equatorial coordinates (mnemonic {\tt q}). 

\end{TeX}

-----------------------------------------------------------------------*/
 
#include "trigo.h"
#include "ok.h"

#define ze	Euler_angles[0]
#define theta	Euler_angles[1]
#define zeta	Euler_angles[2]

/*============================================================================*/
int prej_R  (R, eq0, eq1 )   
/*+++
.PURPOSE  Compute the precession matrix, using the new IAU constants.
	(IAU 1976). The resulting matrix is such that
\begin{TeX}
\quad $\vec{u}(t_1) = R \cdot \vec{u}(t_0)$ \quad (old to new frame).
\end{TeX}
.RETURNS OK
---*/
	double R[3][3];	/* OUT: rotation matrix */
	double eq0; 	/* IN: Initial equinox (Julian Years)	*/
	double eq1; 	/* IN: Final equinox (Julian Years) 	*/
{
 	double t0, dt, w;
	double Euler_angles[3];


  t0 = (eq0 - 2000.e0)/100.e0;	/* Origin is J2000	*/
  dt = (eq1 - eq0)/100.e0;

				
  w = 2306.2181e0+(1.39656e0-0.000139e0*t0)*t0;		/* Arc seconds	*/
  zeta = (w + ( (0.30188e0-0.000344e0*t0) + 0.017998e0*dt) *dt)
	*dt/3600.e0;  					/* Degrees	*/
  ze = (w + ( (1.09468e0+0.000066e0*t0) + 0.018203e0*dt) *dt)
	*dt/3600.e0;					/* Degrees	*/
  theta = ( (2004.3109e0 + (-0.85330e0-0.000217e0*t0)*t0)
	+( (-0.42665e0-0.000217e0*t0) - 0.041833e0*dt) *dt) *dt/3600.e0;

	/*  Computation of rotation matrix	*/

  return(tr_Euler(Euler_angles, R));
}

/*============================================================================*/
int prej_q (q0, q1, eq0, eq1)  
/*+++
.PURPOSE Performs a complete precession between 2 equinoxes.
	Use the new IAU constants.
.METHOD  Compute the precession rotation matrix if necessary,
	then apply the rotation.
.RETURNS OK
---*/
	double q0[2];	/* IN: ra+dec at equinox eq0 in degrees */
	double q1[2]; 	/* OUT: precessed to equinox eq1	*/
	double eq0; 	/* IN: Initial equinox (Julian Years)	*/
	double eq1; 	/* IN: Final equinox (Julian Years) 	*/
{ 
	double us[3];
	
  if (eq0 == eq1)	/* No precession at all, same equinox!!!	*/
  {	q1[0] = q0[0];
	q1[1] = q0[1];
	return(OK);
  }

  tr_ou(q0, us);		/* Convert to unit vector...	*/

  prej_u(us, us, eq0, eq1);	/* precess on unit vectors...	*/
  
  return (tr_uo ( us, q1));	/* And finally -> coordinates	*/
}

/*============================================================================*/
int prej_u (u0, u1, eq0, eq1)  
/*+++
.PURPOSE Performs a complete precession between 2 equinoxes.
	Use the new IAU constants.
.METHOD  Compute the precession rotation matrix if necessary,
	then apply the rotation.
.RETURNS OK
---*/
	double u0[3];	/* IN: Unit vector at equinox eq0 */
	double u1[3]; 	/* OUT: precessed to equinox eq1	*/
	double eq0; 	/* IN: Initial equinox (Julian Years)	*/
	double eq1; 	/* IN: Final equinox (Julian Years) 	*/
{ 
	static double _eq0 = 2000.e0;
	static double _eq1 = 2000.e0;
	static double  _r[3][3]  = { 	{1.e0, 0.e0, 0.e0},
				{0.e0, 1.e0, 0.e0},
				{0.e0, 0.e0, 1.e0}};

  if (eq0 == eq1)		/* No precession at all, same equinox!!!	*/
  {	u1[0] = u0[0];
	u1[1] = u0[1];
	u1[2] = u0[2];
	return(OK);
  }

  if ( (_eq0 != eq0) || (_eq1 != eq1) )
  {	_eq0 = eq0;
	_eq1 = eq1;
	prej_R(_r, eq0, eq1);	/* Compute precession matrix	*/
  }

  return (tr_uu ( u0, u1, _r));	/* And finally rotate...	*/
}

#if 0
/*============================================================================*/
int prej_qv (q0, v0, q1, v1, eq0, ep0, eq1, ep1)
/*+++
.PURPOSE Performs a complete precession between (equinox0, epoch 0)
	to (equinox 1, epoch 1).
	Use the new IAU constants.
.METHOD  Unit vectors
.RETURNS OK
---*/
	double q0[2];	/* IN: Position at equinox eq0, epoch ep0 	*/
	double v0[3];	/* IN: Velocity vector				*/
	double q1[2]; 	/* OUT: precessed to equinox + epoch ep1	*/
	double v1[3];	/* OUT: Velocity vector				*/
	double eq0;	/* IN: Initial equinox	(Julian Years)	*/
	double ep0;	/* IN: Initial epoch	(Julian Years)	*/
	double eq1;	/* IN: Final equinox	(Julian Years)	*/
	double ep1;	/* IN: Final epoch + equinox (Julian Years)	*/
{ 
	static double us[3];

  tr_ou(q0, us);			/* Convert to unit vector...	*/
  prej_uv(us, v0, us, v1, eq0, ep0, eq1, ep1);	
  					/* precess on unit vectors...	*/
  return (tr_uo ( us, q1));		/* And finally -> coordinates	*/
}

/*============================================================================*/
int prej_uv (u0, v0, u1, v1, eq0, ep0, eq1, ep1)
/*+++
.PURPOSE Performs a complete precession between (equinox0, epoch 0)
	to (equinox 1, epoch 1).
	Use the new IAU constants.
.METHOD  Apply the velocity corrections, then
	precess to new equinox.
.RETURNS OK
---*/
	double u0[3];	/* IN: Unit vector at equinox eq0, epoch ep0 	*/
	double v0[3];	/* IN: Velocity vector	("/yr)			*/
	double u1[3]; 	/* OUT: precessed to equinox eq1 and epoch ep1	*/
	double v1[3];	/* OUT: Velocity vector	("/yr)		*/
	double eq0;	/* IN: Initial equinox	(Julian Years)	*/
	double ep0;	/* IN: Initial epoch	(Julian Years)	*/
	double eq1;	/* IN: Final equinox	(Julian Years)	*/
	double ep1;	/* IN: Final epoch + equinox (Julian Years)	*/
{ 
	static double _eq0 = 2000.e0;
	static double _eq1 = 2000.e0;
	static double  _Rf[3][3]  = { 	{1.e0, 0.e0, 0.e0},	/* Precession */
				{0.e0, 1.e0, 0.e0},
				{0.e0, 0.e0, 1.e0}};
	double u[3], du[3], f;
	register int	i;


	/* Express velocities in eq0 Frame	*/

  tr_vw (v0, du, u0);


	/* Apply Proper Motion in eq0 Frame	*/
	
  f = (ep1 - ep0)/3600.e0/DEG;
  for (i=0; i<3; i++)	u[i] = u0[i] + f * v0[i];
  

	/* Renormalize	*/
	
  f = sqrt(u[0]*u[0] + u[1]*u[1] * u[2]*u[2]);
  for (i=0; i<3; i++)		u[i] /= f;


	/* Precess position and velocity from eq0 to eq1	*/

  if (eq0 != eq1)
  {	if ((_eq0 != eq0) || (_eq1 != eq1))
  		_eq0 = eq0, _eq1 = eq1, prej_R(_Rf, _eq0, _eq1);
	tr_uu( u,  u, _Rf);
	tr_uu(du, du, _Rf);
  }

	/* Output the new position	*/

  for (i=0; i<3; i++)	u1[i] = u[i];

	/* Compute new velocity vector	*/

  tr_wv(du, v1, u1);		/* to Local Frame */

  return(OK);
}

#endif
                                                        

                                                               
                                                               
                                                               
                                                               
                              
