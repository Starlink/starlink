 /*
 				neurro.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	neurro.c
*
*	Author:		E.BERTIN, Institut d'Astrophysique de Paris.
*
*	Contents:	global definitions.
*
*	Last modify:	30/03/95
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*--------------------------- Neural Network parameters ---------------------*/
#define		LAYERS		3	/* max. number of hidden+i/o layers */
#define		CONNEX		LAYERS-1
#define		NEURONS		10	/* maximum number of neurons/layer */

/*------------------------------- structures --------------------------------*/
typedef	struct
	{
	int	layersnb;
	int	nn[LAYERS];
	double	inbias[NEURONS];
	double	inscale[NEURONS];
	double	outbias[NEURONS];
	double	outscale[NEURONS];
	double	ni[NEURONS];
	double	no[NEURONS];
	double	n[LAYERS][NEURONS];
	double	w[CONNEX][NEURONS][NEURONS];
	double	b[CONNEX][NEURONS];
	}	brainstruct;

/*------------------------------- globals ----------------------------------*/

extern double	f(double);
