/*
 				bpro.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	Any back-propagation-ANN-oriented software
*
*	Author:		E.BERTIN, IAP/LDAC
*
*	Contents:	Routines for BP-neural network management ("read-only"
*			mode).
*
*	Requirements:	The LDACTools.
*
*	Last modify:	08/10/96
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*------------------------------- definitions -------------------------------*/
#define	SIGMOID(u)	((u)<15.0?((u)>-15.0?1/(1+exp(-(u))):0.0):1.0)
				/* In-line activation function */

/*---------------------------------- types ----------------------------------*/
typedef	float	NFLOAT;		/* Floating point units for neural data */

/*------------------------------- structures --------------------------------*/
typedef	struct structbpann
	{
	int	nlayers;		/* Number of "active" layers */
	int	*nn;			/* Nb of neurons per "active" layer */
/*------ The ANN itself */
	NFLOAT	**neuron;		/* Neuron array (layer,pos in layer) */
	NFLOAT	**weight;		/* Weight array (layer,pos in layer) */
	int	linearoutflag;		/* Flag: 0 if outputs are non-linear */
	}	bpannstruct;


/*------------------------------ Prototypes ---------------------------------*/

bpannstruct	*loadtab_bpann(tabstruct *tab, char *filename);

void		free_bpann(bpannstruct *bpann),
		play_bpann(bpannstruct *bpann, NFLOAT *invec, NFLOAT *outvec);

