#include <stdio.h>

int bananas (int i)
{
    char buf[80];
    sprintf (buf, "banana number %d\n", i);    
    if (i != 0)
        fputs (buf, stdout);
    else
    {
        int *zp = 0;
        printf ("banana split: %d\n", *zp);
    }
    return 0;
}

int main (void)
{
    printf ("Entering the bananas function: 1\n");
    bananas (1);
    printf ("No bananas left!\n");
    bananas (0);
    printf ("We made it out!\n");
}
