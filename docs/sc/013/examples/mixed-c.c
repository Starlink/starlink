#include <stdio.h>

int main (void)
{
    float m[3][3];
    int i, j, n;

    /*
     * Note the C declaration of this Fortran function.  Although
     * we're passing a matrix, we declare the first argument just
     * float* .  This is because a dynamically-dimensioned array can't
     * be expressed in C.
     */
    extern void addarr (float *m, int *n);

    n = 3;                      /* dimension of matrix */
    for (i=0; i<3; i++)
        for (j=0; j<3; j++)
            m[i][j] = (float)(10*i+j);

    printf ("Before:\n");
    for (i=0; i<3; i++)
    {
        printf ("m[%d][j]: ", i);
        for (j=0; j<3; j++)
            printf ("%6.0f", m[i][j]);
        printf ("\n");
    }

    /* Pass the array, coerced to (float*). */
    /* &m[0][0] (address of the first element of the array) would also work. */
    addarr ((float*)m, &n);

    printf ("\nAfter:\n");
    for (i=0; i<3; i++)
    {
        printf ("m[%d][j]: ", i);
        for (j=0; j<3; j++)
            printf ("%6.0f", m[i][j]);
        printf ("\n");
    }
}

/* cadd2 adds twice its second argument to its first. */
void cadd2 (float *a1, float a2)
{
    a2 *= 2;            /* double a2 - this doesn't change a2 in caller */
    *a1 += a2;          /* add a2 to what a1 points to */
}
