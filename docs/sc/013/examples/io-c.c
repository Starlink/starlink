#include <stdio.h>

main ()
{
    FILE *ofile;
    float arr[10];
    int i;

    arr[0] = 1.0;
    for (i=1; i<10; i++) arr[i] = arr[i-1]*2;

    ofile = fopen ("outputc.dat", "w");
    fwrite ((void*)arr, sizeof(arr[0]), 10, ofile);
    fclose (ofile);
}
