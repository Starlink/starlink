 /*
 				neurro.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	A program using neural networks.
*
*	Author:		E.BERTIN, Institut d'Astrophysique de Paris.
*
*	Contents:	run only version of the neural network.
*
*	Last modify:	30/03/95
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"neurro.h"

brainstruct	*brain;

/******************************** neurinit **********************************/
/*
Initialization of the "brain".
*/
void	neurinit()
  {
  QMALLOC(brain, brainstruct, 1);

  return;
  }

/********************************* neurend **********************************/
/*
Close the "brain".
*/
void	neurclose()
  {
  free(brain);

  return;
  }

/******************************** neurresp **********************************/
/*
Neural network response to an input vector.
*/
void	neurresp(double *input, double *output)

  {
   int	i, j, l, lastlay = brain->layersnb-1;
   double	neursum;

  for (i=0; i<brain->nn[0]; i++)
    brain->n[0][i] = input[i]*brain->inscale[i] + brain->inbias[i];
  for (l=0; l<lastlay; l++)
    for (j=0; j<brain->nn[l+1]; j++)
      {
      neursum = brain->b[l][j];
      for (i=0; i<brain->nn[l]; i++)
        neursum += brain->w[l][i][j] * brain->n[l][i];
      brain->n[l+1][j] = f(neursum);
      }
  for (i=0; i<brain->nn[lastlay]; i++)
    output[i] = (brain->n[lastlay][i]-brain->outbias[i])
	/ brain->outscale[i];

  return;
  }


/************************************ f *************************************/
/*
Sigmoid function for a neural network.
*/
double	f(double x)

  {
  return 1.0 / (1.0 + exp(-x));
  }


/********************************* getnnw ***********************************/
/*
Read the NNW table that contains the weights.
*/
void    getnnw()

  {
   FILE	*infile;
   int	i, j, k, step;
   char	str[MAXCHAR], *sstr, *null;

  if ((infile = fopen(prefs.nnw_name,"r")) == NULL)
    error(EXIT_FAILURE,"*ERROR*: can't read ", prefs.nnw_name);

  fgets(str, MAXCHAR, infile);
  if (strncmp(str,"NNW",3))
    error(EXIT_FAILURE, prefs.nnw_name, " is NOT a NNW table!");

  step = 1;
  while (fgets(str, MAXCHAR, infile))
    {
    sstr = &str[(int)strspn(str," \t")];
    if (sstr[0]!=(char)'#' && sstr[0]!=(char)'\n')
      {
      null = sstr;
      switch(step)
        {
        case 1:	brain->layersnb = atoi(strtok(sstr, " \t\n"));
		for (i=0; i<brain->layersnb; i++)
		  brain->nn[i] = atoi(strtok(NULL, " \t\n"));
		step++;
		break;

        case 2:	for (i=0; i<brain->nn[0]; i++)
		  {
		  brain->inbias[i] = atof(strtok(null, " \t\n"));
		  null = NULL;
		  }
		step++;
		break;

        case 3:	for (i=0; i<brain->nn[0]; i++)
		  {
		  brain->inscale[i] = atof(strtok(null, " \t\n"));
		  null = NULL;
		  }
		i=j=0;
		step++;
		break;

        case 4:	if (j == brain->nn[i+1])
		  {
		  j = 0;
		  i++;
		  }
		if (i < brain->layersnb-1)
		  {
		  for (k=0; k<brain->nn[i]; k++)
		    {
		    brain->w[i][k][j] = atof(strtok(null, " \t\n"));
		    null = NULL;
		    }
		  brain->b[i][j] = atof(strtok(NULL, " \t\n"));
		  j++;
		  break;
		  }
		else
		  step++;

        case 5:	for (i=0; i<brain->nn[brain->layersnb-1]; i++)
		  {
		  brain->outbias[i] = atof(strtok(null, " \t\n"));
		  null = NULL;
		  }
		step++;
		break;
        case 6:	for (i=0; i<brain->nn[brain->layersnb-1]; i++)
		  {
		  brain->outscale[i] = atof(strtok(null, " \t\n"));
		  null = NULL;
		  }
		step++;
		break;
	default:error(EXIT_FAILURE, "*Error*: inconsistency in ", prefs.nnw_name);
	}

      }
    }

  fclose(infile);

  return;
  }

