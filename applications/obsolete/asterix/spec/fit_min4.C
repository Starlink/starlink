/*Function : fit_min4
  Called from FORTRAN subroutine FIT_MIN

  Description : Minimises the FORTRAN function FIT_STAT within
    the parameter bounds specified by the lb and ub arrays using a
    genetic algorithm described below.

  Genetic Algorithm : The genetic algorithm creates a population of
    solutions (number is 100 at present) using a pseudo-random
    number generator. The value of FIT_STAT for each solution is
    evaluated and stored. The fitness of each solution is then calculated
    by linear normalization. A reproduction operator is then selected
    at random using Roulette Wheel selection with the initial operator
    weights (which are fixed parameters at present). The algorithm has the
    following reproduction operators :
          Uniform List Crossover
          Average Crossover
          Real Number Mutation
          Real Number Creep (Large)
          Real Number Creep (Small)
    The parent(s) are then selected by Roulette Wheel selection using
    the solutions fitnesses. The reproduction operator is applied and
    the lowest fitness solution(s) are removed from the population
    (save discared solutions for present) to make way for the new
    solution(s) if it is not a duplicate of a solution already in the
    population. Record operator that produced solutions and their
    parents. The value of FIT_STAT is evaluated for the solution
    and the fitnesses of the solutions are recalculated. If the new
    solution is best in population then give it credit proportional
    to the amount it exceeds the previous best evaluation. Pass a
    portion of the credit back to parents and so on. The process is
    then repeated. After a number (fixed parameter 50) of individuals has
    been created average the credit given to the solutions produced
    by the operators to get the operator fitnesses. Use the operator
    fitnesses to adapt the operator weights but do not allow any of
    the operator weights to fall below a certain value (fixed parameter 10).

*/

/* Incude files */
#include <stdlib.h>
#include <iostream.h>
#include <math.h>
#include <time.h>

/* Define macros */
#define TRUE    1
#define FALSE   0
#define NONE -1

/* Floating point array container class */
    class FloatArray {
    public:
     FloatArray(unsigned nrows, unsigned ncols);
     FloatArray(unsigned nrows);
     ~FloatArray();

     float& operator() (unsigned i, unsigned j);
     float& operator() (unsigned i);

    private:
      float* data_;
      unsigned nrows_, ncols_;
    };

    inline float& FloatArray::operator() (unsigned row, unsigned col)
    {
      if (row >= nrows_ || col >= ncols_)
       {
        cout << "Array bounds violation" << endl;
        exit(1);
       }
      return data_[col*nrows_ + row];
    }

    inline float& FloatArray::operator() (unsigned row)
    {
      if (row >= nrows_ )
       {
        cout << "Array bounds violation" << endl;
        exit(1);
       }
      return data_[row];
    }

    FloatArray::FloatArray(unsigned nrows, unsigned ncols)
      : data_  (new float[nrows * ncols]),
        nrows_ (nrows),
        ncols_ (ncols)
    {
      if (nrows == 0 || ncols == 0)
       {
        cout << "Error : Bad array size" << endl;
        exit(1);
       }
    }

    FloatArray::FloatArray(unsigned nrows)
      : data_  (new float[nrows]),
        nrows_ (nrows),
        ncols_ (1)
    {
      if (nrows == 0)
       {
        cout << "Error : Bad array size" << endl;
        exit(1);
       }
    }

    FloatArray::~FloatArray()
    {
      delete[] data_;
    }

/* Declaration of external FORTRAN structure DATASET */
struct dataset {
  int d_id;
  int b_id;
  int v_id;
  int setindex;
  int ndim;
  int idim[7];
  int ndat;
  int dptr;
  int vptr;
  int qptr;
  int wptr;
  int bptr;
  int vigptr;
  int qflag;
  char datname[80];
  float teff;
  int gflag;
  int ngdat;
  int gptr;
  int gdptr;
  int gwptr;
  int gqptr;
  int gvptr;
  int spar[3];
 };

/* Declaration of external FORTRAN structure PREDICTION */
struct prediction {
  int convolve;
  int nmdim;
  int idimm[7];
  int nmdat;
  int nmbound;
  int mlbndptr;
  int mubndptr;
  int dptr;
  int gdptr;
  int mptr;
  int predptr[2];
  int pgdptr[2];
  int dfdpptr;
  int spar[4];
 };

/* Declaration of external FORTRAN structure INSTR_RESP */
struct instr_resp {
  int r_id;
  int a_id;
  int nresp;
  int miptr;
  int diptr;
  int resptr;
 };

/* Declaration of external FORTRAN structure MODEL_SPEC */
struct model_spec {
  char genus[4];
  char spec[80];
  char polish[80];
  int ncomp;
  char key[5][30];
  int additive[30];
  int npar;
  int istart[30];
  char parname[25][60];
  char units[30][60];
  char format[12][60];
  int stackptr;
  int ntie;
  int tstart[16];
  int tgroup[60];
  int m_id;
 };

/* Declaration of external FORTRAN structure MIN_CTRL */
/*struct min_ctrl {
  int initialise;
  int method;
  int nitmax;
  int curfit;
  float slope;
  int intit;
  float temp;
  float tdec;
  double oldstat[4];
  int ispare[4];
  float rspare[4];
};*/

/* Declaration of global variables */
float u[97], c, cd, cm;
int i97, j97, test;
double eval[100], oldeval[10000];
float cred[100], oldcred[10000];
int nold, parents[100][2], oldparents[10000][2], creator[100], oldcreator[10000];

/* Declaration of external FORTRAN subroutine FIT_STAT */
extern "C" void fit_stat_(int *nds, int *model, float *newpar, int *fstat, int *predictor,
    double *newstat, double *lndfac, int *status);

/* Declaration of external FORTRAN subroutine FIT_PRGRS */
extern "C" void fit_prgrs_(int* nit, float* param, int* frozen, int *pegged, int *model,
     int* finished, int* status);

/* Declaration of external FORTRAN subroutine ADI_CGET0I */
extern "C" void adi_cget0i_(int* mctrl, char* string, int* returned, int* status, const int strlen);

/* Declaration of external FORTRAN subroutine ADI_CGET0R */
extern "C" void adi_cget0r_(int* mctrl, char* string, float* returned, int* status, const int strlen);

/* Declaration of external FORTRAN subroutine FLUSH */
extern "C" void flush_(const int* unit);

/* Declaration of pseudorandom number generator functions */
int rmarin(int ij, int kl);
int ranmar(FloatArray* rvec, int len);
void sow(int *seed1, int *seed2);

/* Declaration of sorting functions */
void qsort(int *array, int left, int right);
void split(int *array, int left, int right, int *pivotpoint);
void swapi(int *x, int *y);
int selectpivot(int *array, int left, int right);
void binaryinsert(int n, int *list, int *place);

/* Declaration of genetic algorithm functions */
int selectparent(float *array, float dec);
void uniform(float *parent1, float *parent2, FloatArray* child1, FloatArray* child2, int npar);
int inpop(FloatArray* child, FloatArray* popl, int npar);
void insert(FloatArray* child1, FloatArray* child2, FloatArray* popl, int *list, int * num,
    int npar, int *nds, int *model, int *fstat, int *predictor, int *status, int parent1,
    int parent2, int method, FloatArray* oldpopl, double *lndfac);
void reunite(int n);
void calcfit(int *list, float *fit, float dec);
void average(float *parent1, float *parent2, FloatArray* child, int npar);
void mutate(float *parent, FloatArray* child, int npar, int *frozen, float *lb, float *ub);
void creep(float *parent, FloatArray* child, int npar, int *frozen, float *lb, float *ub, float size);
void credit(int last, int *list);
void recurse(int n, float value, int *list, int level);
void opfitness(float* opfit, int* nopc);

/* Genetic algorithm function fit_min4 - called from FORTRAN */
extern "C" {
void fit_min4_(int *nds, int *model, int *prgres, int *npar,  float *lb, float *ub, int *frozen,
   int *sscale, double *lndfac, int *fstat, int *predictor, float *param, int *pegged,
   double *stat, int *finished, int *fiterr, int *mctrl, int *status)
 {
  /* Declare local variables */
  float rval, count, fit[100];
  int seed1, seed2, i, j, list[100], nsol, parent1, parent2, nc, neval, nit, nopc[5], nitmax;
  double minstat, avstat, avstat2;
  float minslope,lscale;
  FloatArray oldpopl(*npar,10000), popl(*npar,100), temp(*npar), child1(*npar), child2(*npar);

  /* Genetic algorithm parameters */
  float dec = 0.95;
  float opfit[5];
  opfit[0]=35;
  opfit[1]=35;
  opfit[2]=10;
  opfit[3]=10;
  opfit[4]=10;

  /* Initialize variables */
  nold=0;
  const int unit=6;

  adi_cget0i_(mctrl, "MaxIt", &nitmax, status, 5);
  adi_cget0r_(mctrl, "MinSlope", &minslope, status, 8);

  if (*sscale > 0)
   {
    lscale=(float)(*sscale);
   }
  else
   {
    lscale=1.0;
   }

  for(i = 0; i <= 99; i++)
   {
    cred[i]=0.0;
    parents[i][0]=100;
    parents[i][1]=100;
    creator[i]=5;
   }

  for(i = 0; i <= 9999; i++)
   {
    oldcred[i]=0.0;
    oldparents[i][0]=100;
    oldparents[i][1]=100;
    oldcreator[i]=5;
   }

  /* Create seeds from system clock */
  sow(&seed1, &seed2);

  /* Initialize random number generator */
  if(rmarin(seed1, seed2) == 1)
   {
    cout << "Error intializing random number generator" << endl;
   }

  /* Initialize random population */
  for(i = 0; i <= 98; i++)
   {
    /* Generate random numbers */
    if(ranmar(&temp, *npar) == 1)
     {
      cout << "Error generating random numbers" << endl;
     }

    /* Set random parameters between bounds */
    for(j = 0; j < *npar; j++)
     {
      if(frozen[j] == FALSE)
       {
        popl(j, i)=((ub[j]-lb[j])*temp(j)+lb[j]);
       }
      else
       {
        popl(j, i)=param[j];
       }
     }
   }

  /* Put initial solution in population */
  for(j = 0; j < *npar; j++)
   {
    popl(j, 99)=param[j];
   }

  /* Evaluate solutions */
  for(i = 0; i <= 99; i++)
   {
    fit_stat_(nds, model, &popl(0, i), fstat, predictor, &eval[i], lndfac, status);
   }

  /* Initialize list array */
  for(i = 0; i <= 99; i++)
   {
     list[i]=i;
   }

  /* Quicksort list array according to evaluations */
  qsort(list, 0, 99);

  /* Calculate fitness array using list order and fitness decrement */
  calcfit(list, fit, dec);

  /*cout << eval[list[0]] << eval[list[0]]/(double)*sscale << endl;*/
//  for(i = 0; i <= 99; i++)
//   {
//    cout << eval[list[i]] << endl;
//   }

  j = -1;

  neval = 100;
  nit=0;

  for(i = 0; i <= 4; i++)
   {
     nopc[i]=0;
   }

  if (*fstat == 2)
   {
    cout << endl << " Initial Cash statistic & scaled statistic : ";
   }
  else
   {
    cout << endl << " Initial chisq & chi-red : ";
   }
  cout << "\t" << eval[list[0]] << "\t" << eval[list[i]]/lscale << endl << endl;
  cout.flush();

  do {
     /* Initialize new solution counter */
     nsol = 0;
     j++;

    minstat = eval[list[0]];

    avstat = 0.0;
    for(i = 0; i <= 97; i++)
     {
      avstat = avstat + eval[list[i]];
     }

    /* Loop until fifty solutions have been added to population */
    do {
      /* Select operator by roulette wheel selection */
      if(ranmar(&temp, *npar) == 1)
       {
        cout << "Error generating  random numbers" << endl;
       }

      rval = 100.0*temp(0);
      count = 0.0;
      i = 0;

      do {
        count = count + opfit[i];
        i++;
       } while (count < rval);

      switch(i) {
        case 1: /* Uniform List Crossover Operator */
          nc = 2;
          parent1 = selectparent(fit, dec);
          parent2 = selectparent(fit, dec);
          uniform(&popl(0, parent1), &popl(0, parent2), &child1, &child2, *npar);
          insert(&child1, &child2, &popl, list, &nc,*npar, nds,
            model, fstat, predictor, status, parent1, parent2, 0, &oldpopl, lndfac);
          if(nc > 0)
           {
            calcfit(list, fit, dec);
            nsol = nsol + nc;
            nopc[i-1]=nopc[i-1]+nc;
           }
          break;
        case 2: /* Average Crossover Operator */
          nc = 1;
          parent1 = selectparent(fit, dec);
          parent2 = selectparent(fit, dec);
          average(&popl(0, parent1), &popl(0, parent2), &child1, *npar);
          insert(&child1, &child2, &popl, list, &nc,*npar, nds,
            model, fstat, predictor, status, parent1, parent2, 1, &oldpopl, lndfac);
          if(nc > 0)
           {
            calcfit(list, fit, dec);
            nsol = nsol + nc;
            nopc[i-1]=nopc[i-1]+nc;
           }
          break;
        case 3: /* Real Number Mutation */
          nc = 1;
          parent1 = selectparent(fit, dec);
          mutate(&popl(0, parent1), &child1, *npar, frozen, lb, ub);
          insert(&child1, &child2, &popl, list, &nc,*npar, nds,
            model, fstat, predictor, status, parent1, 100, 2, &oldpopl, lndfac);
          if(nc > 0)
           {
            calcfit(list, fit, dec);
            nsol = nsol + nc;
            nopc[i-1]=nopc[i-1]+nc;
           }
          break;
        case 4: /* Real Number Creep (Large) */
          nc = 1;
          parent1 = selectparent(fit, dec);
          creep(&popl(0, parent1), &child1, *npar, frozen, lb, ub, 0.02);
          insert(&child1, &child2, &popl, list, &nc,*npar, nds,
            model, fstat, predictor, status, parent1, 100, 3, &oldpopl, lndfac);
          if(nc > 0)
           {
            calcfit(list, fit, dec);
            nsol = nsol + nc;
            nopc[i-1]=nopc[i-1]+nc;
           }
          break;
        case 5: /* Real Number Creep (Small) */
          nc = 1;
          parent1 = selectparent(fit, dec);
          creep(&popl(0, parent1), &child1, *npar, frozen, lb, ub, 0.0005);
          insert(&child1, &child2, &popl, list, &nc,*npar, nds,
            model, fstat, predictor, status, parent1, 100, 4, &oldpopl, lndfac);
          if(nc > 0)
           {
            calcfit(list, fit, dec);
            nsol = nsol + nc;
            nopc[i-1]=nopc[i-1]+nc;
           }
          break;
       }

     } while (nsol  < 50);

    neval = neval + nsol;
    nit++;

    /*cout << eval[list[0]] << eval[list[0]]/(double)*sscale << endl;*/
//   for(i = 0; i <= 99; i++)
//    {
//     cout << eval[list[i]] << endl;
//    }

    opfitness(opfit, nopc);

//    for(i = 0; i <= 4; i++)
//     {
//      cout << opfit[i] << endl;
//     }

    avstat2 = 0.0;
    for(i = 0; i <= 97; i++)
     {
      avstat2 = avstat2 + eval[list[i]];
     }

    if (minstat != eval[list[0]] || (avstat-avstat2)/fabs(avstat) > minslope)
     {
       *finished=FALSE;
      }
    else
     {
       *finished=TRUE;
      }

    for(i = 0; i < *npar; i++)
     {
       if(popl(i, list[0])==lb[i] || popl(i, list[0])==ub[i])
        {
          pegged[i]=TRUE;
        }
      else
       {
         pegged[i]=FALSE;
       }
     }

    if (*fstat == 2)
     {
      cout << endl << " it,scaled Cash,Cash,slope : ";
     }
    else
     {
      cout << endl << " it,chi-red,chisq,slope : ";
     }
    cout << "\t" << nit << "\t" << eval[list[0]]/lscale << "\t" << eval[list[0]];
    cout << "\t" << (avstat - avstat2)/fabs(avstat) << endl << endl;
    cout.flush();

    fit_prgrs_(&nit, &popl(0, list[0]), frozen, pegged, model, finished, status);
    flush_(&unit);

//    cout << "deltaavstat = " << (avstat - avstat2)/fabs(avstat) << endl;
//    cout << "number of evaluations = " << neval << endl;
//    cout << "x = " << popl(0, list[0]) << "y = " << popl(1, list[0]) << endl;
   } while (*finished == FALSE && nit < nitmax);

  for(i = 0; i < *npar; i++)
   {
     param[i]=popl(i, list[0]);
   }
  *stat=eval[list[0]];
 }
}

/************************************************************************
 This random number generator originally appeared in "Toward a Universal
 Random Number Generator" by George Marsaglia and Arif Zaman.
 Florida State University Report: FSU-SCRI-87-50 (1987)

 It was later modified by F. James and published in "A Review of Pseudo-
 random Number Generators"

 Converted from FORTRAN to C by Phil Linttell, James F. Hickling
 Management Consultants Ltd, Aug. 14, 1989.

 THIS IS THE BEST KNOWN RANDOM NUMBER GENERATOR AVAILABLE.
       (However, a newly discovered technique can yield
         a period of 10^600. But that is still in the development stage.)

 It passes ALL of the tests for random number generators and has a period
   of 2^144, is completely portable (gives bit identical results on all
   machines with at least 24-bit mantissas in the floating point
   representation).

 The algorithm is a combination of a Fibonacci sequence (with lags of 97
   and 33, and operation "subtraction plus one, modulo one") and an
   "arithmetic sequence" (using subtraction).

 On a Vax 11/780, this random number generator can produce a number in
    13 microseconds.
************************************************************************/

/************************************************************************
 This is the initialization routine for the random number generator RANMAR()
 NOTE: The seed variables can have values between:    0 <= IJ <= 31328
                                                      0 <= KL <= 30081
 The random number sequences created by these two seeds are of sufficient
 length to complete an entire calculation with. For example, if several
 different groups are working on different parts of the same calculation,
 each group could be assigned its own IJ seed. This would leave each group
 with 30000 choices for the second seed. That is to say, this random
 number generator can create 900 million different subsequences -- with
 each subsequence having a length of approximately 10^30.

 Use IJ = 1802 & KL = 9373 to test the random number generator. The
 subroutine RANMAR should be used to generate 20000 random numbers.
 Then display the next six random numbers generated multiplied by 4096*4096
 If the random number generator is working properly, the random numbers
 should be:
           6533892.0  14220222.0   7275067.0
           6172232.0   8354498.0  10633180.0
************************************************************************/

int rmarin(int ij, int kl)
{

        float s, t;
        int i, j, k, l, m;
        int ii, jj;

        /* Change FALSE to TRUE in the next statement to test the
           random routine.*/

        test = TRUE;

        if ( ( ij < 0 || ij > 31328 ) ||
                ( kl < 0 || kl > 30081 ) )
        {
                cout << "RMARIN: The first random number seed must have a "
                        << "value between 0 and 31328" << endl;
                cout << "        The second random number seed must have a "
                        << "value between 0 and 30081";
                return 1;
        }

        i = (int)fmod(ij/177.0, 177.0) + 2;
        j = (int)fmod(ij      , 177.0) + 2;
        k = (int)fmod(kl/169.0, 178.0) + 1;
        l = (int)fmod(kl      , 169.0);

        for ( ii=0; ii<=96; ii++ )
        {
                s = (float)0.0;
                t = (float)0.5;
                for ( jj=0; jj<=23; jj++ )
                {
                        m = (int)fmod( fmod(i*j,179.0)*k , 179.0 );
                        i = j;
                        j = k;
                        k = m;
                        l = (int)fmod( 53.0*l+1.0 , 169.0 );
                        if ( fmod(l*m,64.0) >= 32)
                                s = s + t;
                        t = (float)(0.5 * t);
                }
                u[ii] = s;
        }

        c  = (float)(  362436.0 / 16777216.0);
        cd = (float)( 7654321.0 / 16777216.0);
        cm = (float)(16777213.0 / 16777216.0);

        i97 = 96;
        j97 = 32;

        test = TRUE;

        return 0;
}

int ranmar(FloatArray* rvec, int len)
{
        float uni;
        int ivec;

        if ( !test )
        {
                cout << "RANMAR: Call the initialization routine (RMARIN) "
                        << "before calling RANMAR." << endl;
                return 1;
        }

        for ( ivec=0; ivec < len; ivec++)
        {
                uni = u[i97] - u[j97];
                if ( uni < 0.0F )
                        uni = uni + 1.0;
                u[i97] = uni;
                i97--;
                if ( i97 < 0 )
                        i97 = 96;
                j97--;
                if ( j97 < 0 )
                        j97 = 96;
                c = c - cd;
                if ( c < 0.0F )
                        c = c + cm;
                uni = uni - c;
                if ( uni < 0.0F )
                        uni = uni + 1.0;
                (*rvec)(ivec) = uni;
        }
        return 0;
}

/* I use the following procedure in TC to generate seeds:

  The sow() procedure calculates two seeds for use with the random number
  generator from the system clock.  I decided how to do this myself, and
  I am sure that there must be better ways to select seeds; hopefully,
  however, this is good enough.  The first seed is calculated from the values
  for second, minute, hour, and year-day; weighted with the second most
  significant and year-day least significant.  The second seed weights the
  values in reverse.
*/

void sow( int *seed1, int *seed2 )
{
        struct tm *tm_now;
        float s_sig, s_insig, maxs_sig, maxs_insig;
        long secs_now;
        int s, m, h, d, s1, s2;

        time(&secs_now);
        tm_now = localtime(&secs_now);

        s = tm_now->tm_sec + 1;
        m = tm_now->tm_min + 1;
        h = tm_now->tm_hour + 1;
        d = tm_now->tm_yday + 1;

        maxs_sig   = (float)(60.0 + 60.0/60.0 + 24.0/60.0/60.0 +
              366.0/24.0/60.0/60.0);
        maxs_insig = (float)(60.0 + 60.0*60.0 + 24.0*60.0*60.0 +
              366.0*24.0*60.0*60.0);

        s_sig      = (float)(s + m/60.0 + h/60.0/60.0 + d/24.0/60.0/60.0);
        s_insig    = (float)(s + m*60.0 + h*60.0*60.0 + d*24.0*60.0*60.0);

        s1 = (int)(s_sig   / maxs_sig   * 31328.0);
        s2 = (int)(s_insig / maxs_insig * 30081.0);

        *seed1 = s1;
        *seed2 = s2;
}

/* Quicksort Algorithm (slightly adapted) */
void qsort (int *array, int left, int right) {
  int pivotpoint;

  if (left < right) {
    split (array, left, right, &pivotpoint);
    if (pivotpoint != NONE)
    {
      qsort(array, left, pivotpoint - 1);
      qsort(array, pivotpoint + 1, right);
    }
   }
 }

void split (int *array, int left, int right, int *pivotpoint) {

  int first = left;              /* remember the left position */
  int correctside;               /* are we on the correct side? */
  int point = selectpivot(array, left, right);
  double pivotval;

  if (point != NONE)
   {
     pivotval = eval[array[point]];

    do {
      correctside = TRUE;
      while (correctside)
        if (eval[array[left]] > pivotval) correctside = FALSE;
        else {
          left++;
          correctside = (left <= right);
         }

      correctside = (left <= right);

      while (correctside)
        if (eval[array[right]] <= pivotval) correctside = FALSE;
        else {
          right--;
          correctside = (left <= right);
         }

        if (left < right) {
          swapi(&array[left], &array[right]);
          left++;
          right--;
         }
       } while (left <= right);

    *pivotpoint = right;
    swapi (&array[point], &array[*pivotpoint]);
   }
  else
   {
    *pivotpoint = NONE;
   }
 }

/* Swap array elements */
void swapi (int *x, int *y) {
  int temp = *x;
  *x = *y;
  *y = temp;
 }

/* Select pivot point */
int selectpivot(int* array, int left, int right) {
  double first = eval[array[left]];
  int lcv;
  for (lcv = left + 1; lcv <= right; lcv++) {
    if (eval[array[lcv]] > first) return lcv;
    else if (eval[array[lcv]] < first) return left;
   }
  return (NONE);
 }

/* Select parent using roulette wheel selection */
int selectparent(float *array, float dec)
 {
   FloatArray temp(1);
   float rval, count;
   int i;

   if(ranmar(&temp, 1) == 1)
    {
     cout << "Error generating random numbers" << endl;
    }

   rval = (10000.0-4950.0*dec)*temp(0);
   count = 0.0;
   i = 0;

   do {
     count = count + array[i];
     i++;
    } while (count < rval);

  return (i-1);
 }

/* Uniform List Crossover Operator */
void uniform(float* parent1, float* parent2, FloatArray* child1, FloatArray* child2, int npar)
 {
   FloatArray temp(npar);
   int i;

   if(ranmar(&temp, npar) == 1)
    {
      cout << "Error generating random numbers" << endl;
    }

   for(i = 0; i < npar; i++)
    {
      if(temp(i) < 0.5)
       {
        (*child1)(i) = parent1[i];
        (*child2)(i) = parent2[i];
       }
      else
       {
        (*child1)(i) = parent2[i];
        (*child2)(i) = parent1[i];
       }
    }
 }

/* Check if child is already in population */
int inpop(FloatArray* child, FloatArray* popl, int npar)
 {
  int i, j, diff;

  i = 0;
  do {
     diff=FALSE;
     j = 0;
    do {
       if((*child)(j) != (*popl)(j, i))
        {
         diff=TRUE;
        }
       j++;
      } while (j < npar && diff == FALSE);
    i++;
   } while (i <= 99 && diff  == TRUE);
  return(!diff);
 }

/* Insert children into population if not already in it */
void insert(FloatArray* child1, FloatArray* child2, FloatArray* popl, int *list, int * num,
    int npar, int *nds, int *model, int *fstat, int *predictor, int *status,
    int parent1, int parent2, int method, FloatArray* oldpopl, double *lndfac)
 {
  int j, place;
  int nc = 0;

  if(inpop(child1, popl, npar) == FALSE)
   {
    nc++;
    for(j = 0; j < npar; j++)
     {
      (*oldpopl)(j, nold)=(*popl)(j, list[99]);
      (*popl)(j, list[99])=(*child1)(j);
      oldeval[nold]=eval[list[99]];
      oldcred[nold]=cred[list[99]];
      oldparents[nold][0]=parents[list[99]][0];
      oldparents[nold][1]=parents[list[99]][1];
      oldcreator[nold]=creator[list[99]];
      cred[list[99]]=0;
      parents[list[99]][0]=parent1;
      parents[list[99]][1]=parent2;
      creator[list[99]]=method;
      reunite(99);
     }
    nold++;
    fit_stat_(nds, model, &(*popl)(0, list[99]), fstat, predictor, &eval[list[99]], lndfac, status);
    if(*num == 2)
     {
      if(inpop(child2, popl, npar) == FALSE)
       {
        nc++;
        for(j = 0; j < npar; j++)
         {
          (*oldpopl)(j, nold)=(*popl)(j, list[98]);
          (*popl)(j, list[98])=(*child2)(j);
          oldeval[nold]=eval[list[98]];
          oldcred[nold]=cred[list[98]];
          oldparents[nold][0]=parents[list[98]][0];
          oldparents[nold][1]=parents[list[98]][1];
          oldcreator[nold]=creator[list[98]];
          cred[list[98]]=0;
          parents[list[98]][0]=parent1;
          parents[list[98]][1]=parent2;
          creator[list[98]]=method;
          reunite(98);
         }
        nold++;
        fit_stat_(nds, model, &(*popl)(0, list[98]), fstat, predictor, &eval[list[98]], lndfac, status);
       }
     }
   }
  else if(*num == 2)
   {
    if(inpop(child2, popl, npar) == FALSE)
     {
      nc++;
      for(j = 0; j < npar; j++)
       {
        (*oldpopl)(j, nold)=(*popl)(j, list[99]);
        (*popl)(j, list[99])=(*child2)(j);
        oldeval[nold]=eval[list[99]];
        oldcred[nold]=cred[list[99]];
        oldparents[nold][0]=parents[list[99]][0];
        oldparents[nold][1]=parents[list[99]][1];
        oldcreator[nold]=creator[list[99]];
        cred[list[99]]=0;
        parents[list[99]][0]=parent1;
        parents[list[99]][1]=parent2;
        creator[list[99]]=method;
        reunite(99);
       }
      nold++;
      fit_stat_(nds, model, &(*popl)(0, list[99]), fstat, predictor, &eval[list[99]], lndfac, status);
     }
   }

  if(nc == 2)
   {
    binaryinsert(97, list, &place);
    if (place == 0)
     {
      credit(97, list);
     }
    binaryinsert(98, list, &place);
    if (place == 0)
     {
      credit(98, list);
     }
   }
  else if(nc == 1)
   {
    binaryinsert(98, list, &place);
    if (place == 0)
     {
      credit(98, list);
     }
   }

  *num=nc;
 }

/* Reunite children with parent */
void reunite(int n)
  {
    int i;
   for(i=0; i<=n; i++)
    {
     if(parents[i][0]==n)
      {
       parents[i][0]=nold+255;
      }
     if(parents[i][1]==n)
      {
       parents[i][1]=nold+255;
      }
    }
   for(i=0; i<=nold; i++)
    {
     if(oldparents[i][0]==n)
      {
       oldparents[i][0]=nold+255;
      }
     if(oldparents[i][1]==n)
      {
       oldparents[i][1]=nold+255;
      }
    }
  }

/* Insert new value into correct position in array */
void binaryinsert(int n, int *list, int *place)
 {
  int i, j, k, index, pivot;

  i = 0;
  j = n;
  pivot=list[j + 1];

  do {
    index=(i + j)/2;
    if(eval[pivot] < eval[list[index]])
     {
      j = index - 1;
     }
    else
     {
      i = index + 1;
     }
   } while (j > i);

  if(eval[pivot] > eval[list[i]])
   {
    i++;
   }

  for(k = n; k >= i; k--)
   {
    list[k + 1]=list[k];
   }

  list[i]=pivot;
  *place=i;
 }

/* Calculate fitness from list and decrement */
void calcfit(int *list, float *fit, float dec)
 {
  int i;

  for(i = 0; i <= 99; i++)
   {
    fit[list[i]]=100.0-(float)i*dec;
    if(fit[list[i]] < 0.0)
     {
      fit[list[i]]=0.0;
     }
   }
 }

/* Average Crossover Operator */
void average(float* parent1, float* parent2, FloatArray* child, int npar)
 {
  int i;

  for(i = 0; i < npar; i++)
   {
    (*child)(i)=(parent1[i] + parent2[i])/2.0;
   }
 }

/* Mutation Operator */
void mutate(float* parent, FloatArray* child, int npar, int *frozen, float *lb, float *ub)
 {
  int i;
  FloatArray temp(2*npar);

  /* Generate random numbers */
  if(ranmar(&temp, 2*npar) == 1)
   {
    cout << "Error generating random numbers" << endl;
   }

  /* Set random parameters between bounds */
  for(i = 0; i <= npar-1; i++)
   {
    if(frozen[i] == FALSE)
     {
      if(temp(npar + i) < 0.5)
       {
        (*child)(i)=((ub[i]-lb[i])*temp(i)+lb[i]);
       }
      else
       {
        (*child)(i) = parent[i];
       }
     }
    else
     {
      (*child)(i)=parent[i];
     }
   }
 }

/* Creep Operator */
void creep(float* parent, FloatArray* child, int npar, int *frozen, float *lb, float *ub, float size)
 {
  int i;
  FloatArray temp(3*npar);

  /* Generate random numbers */
  if(ranmar(&temp, 3*npar) == 1)
   {
    cout << "Error generating random numbers" << endl;
   }

  for(i = 0; i < npar; i++)
   {
    if(frozen[i] == FALSE)
     {
       if(temp(2*npar + i) < 0.7)
        {
         if(temp(npar + i) < 0.5)
          {
           (*child)(i) = parent[i] - (ub[i] - lb[i])*temp(i)*size;
           if((*child)(i) < lb[i])
            {
             (*child)(i) = lb[i];
            }
          }
         else
          {
           (*child)(i) = parent[i] + (ub[i] - lb[i])*temp(i)*size;
           if((*child)(i) > ub[i])
            {
             (*child)(i) = ub[i];
            }
          }
       }
      else
       {
        (*child)(i)=parent[i];
       }
     }
   else
    {
     (*child)(i) = parent[i];
    }
   }
 }

/* Give credit to ancestors */
void credit(int last, int *list)
 {
  float value;

  value=(eval[list[1]]-eval[list[0]]);

  recurse(0, value, list, 0);
 }

/* Recurse through ancestors */
void recurse(int n, float value, int *list, int level)
 {
  level++;
  if (level < 4)
   {
    if (n < 100)
     {
      cred[list[n]]=0.5*value + cred[list[n]];
      if (parents[list[n]][1] != 100)
       {
        recurse(parents[list[n]][0], 0.25*value, list, level);
        recurse(parents[list[n]][1], 0.25*value, list, level);
       }
      else
       {
        if (parents[list[n]][0] != 100)
         {
          recurse(parents[list[n]][0], 0.5*value, list, level);
         }
       }
     }
    else
     {
      oldcred[n-255]=0.5*value + oldcred[n-255];
      if (oldparents[n-255][1] != 100)
       {
        recurse(oldparents[n-255][0], 0.25*value, list, level);
        recurse(oldparents[n-255][1], 0.25*value, list, level);
       }
      else
       {
        if (oldparents[n-255][0] != 100)
         {
          recurse(oldparents[n-255][0], 0.5*value, list, level);
         }
       }
     }
   }
 }

/* Calculate operator fitness */
void opfitness(float* opfit, int* nopc)
 {
  int i, j, n, alt[5];
  float sum, change, opcred[5];


  for(i = 0; i <= 4; i++)
   {
    opcred[i]=0.0;
   }

  for (i=0; i<=4; i++)
   {
    for (j=0; j<=99; j++)
     {
       if (creator[j]==i)
        {
          opcred[i]=opcred[i]+cred[j]/(float)nopc[i];
        }
     }
    for (j=0; j<nold; j++)
     {
       if (oldcreator[j]==i)
        {
          opcred[i]=opcred[i]+oldcred[j]/(float)nopc[i];
        }
     }
   }

  sum=0.0;

  for (i=0; i<=4; i++)
   {
     sum = sum + opcred[i];
   }

//  for (i=0; i<=4; i++)
//   {
//     cout << opcred[i] << endl;
//   }

  if (sum != 0.0)
   {
    for (i=0; i<=4; i++)
     {
      opcred[i]=15.0*opcred[i]/sum;
     }

    for (i=0; i<=4; i++)
     {
      opfit[i]=0.85*opfit[i];
     }

    change=0.0;
    n=0;
    for (i=0; i<=4; i++)
     {
      alt[i]=0;
     }

    for (i=0; i<=4; i++)
     {
      if ((opfit[i]+opcred[i])<10.0)
       {
        change=change+(10.0-(opfit[i]+opcred[i]));
        opcred[i]=10.0-opfit[i];
        alt[i]=1;
        n++;
       }
     }

    if (n != 0)
     {
      change=change/(float)(5-n);

      for (i=0; i<=4; i++)
       {
        if (alt[i]==0)
         {
          opcred[i]=opcred[i]-change;
         }
       }
     }
   }

  for (i=0; i<=4; i++)
   {
    opfit[i]=opfit[i]+opcred[i];
   }

   for (i=0; i<=99; i++)
    {
     cred[i]=0.0;
    }

   for (i=0; i<nold; i++)
    {
     oldcred[i]=0.0;
    }
 }
