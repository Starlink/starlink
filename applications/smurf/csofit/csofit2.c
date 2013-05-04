
/* needed for -std=c99 */
#define _XOPEN_SOURCE

#include "csofit2.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <time.h>


csofit2_t * csofit2_open(const char * filename)
{
    FILE * f = fopen(filename, "r");
    if(!f)
    {
        return NULL;
    }

    /* count lines in file; prealloc structure. */
    int nlines = 0;
    int c;
    while((c = fgetc(f)) != EOF)
    {
        if(c == '\n')
        {
            nlines += 1;
        }
    }
    /* NOTE polys[1] handles case no newline at end of file */
    csofit2_t * fits = (csofit2_t*)malloc(sizeof(csofit2_t) + nlines*sizeof(csofit2_poly_t));
    memset(fits, 0, sizeof(csofit2_t) + nlines*sizeof(csofit2_poly_t));

    /* parse lines and fill in polys */
    char buf[1024];
    rewind(f);
    while(fgets(buf, sizeof(buf), f))
    {
        csofit2_poly_t poly;
        memset(&poly, 0, sizeof(poly));
        char delim[] = " \t\r\n";
        char * saveptr = NULL;
        char * tok = strtok_r(buf, delim, &saveptr); if(!tok) continue;
        struct tm utc;
        if(!strptime(tok, "%Y-%m-%dT%H:%M:%S", &utc)) continue;
        poly.start = mktime(&utc);
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;
        if(!strptime(tok, "%Y-%m-%dT%H:%M:%S", &utc)) continue;
        poly.end = mktime(&utc);
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;
        char * endptr = NULL;
        poly.deg = strtol(tok, &endptr, 0); if(*endptr != '\0') continue;
        if(poly.deg < 0 || poly.deg > 19) continue;
        int i;
        for(i = 0; i < poly.deg+1; i++)
        {
            tok = strtok_r(NULL, delim, &saveptr); if(!tok) break;
            poly.coefs[i] = strtod(tok, &endptr); if(*endptr != '\0') break;
        }
        if(i != (poly.deg+1)) continue;
        memcpy(&fits->polys[fits->npolys], &poly, sizeof(poly));
        fits->npolys += 1;
    }
    fclose(f);
    return fits;
}

csofit2_t * csofit2_subset(csofit2_t * fits, double t0, double t1)
{
    if(!fits)
    {
        return NULL;
    }
    /* fudge 1s for blending */
    t0 -= 1.0;
    t1 += 1.0;
    /* count overlapping fits and allocate return value */
    int npolys = 0;
    int i;
    for(i = 0; i < fits->npolys; i++)
    {
        if((fits->polys[i].end > t0) && (fits->polys[i].start < t1))
        {
            npolys += 1;
        }
    }
    csofit2_t * subset = (csofit2_t*)malloc(sizeof(csofit2_t) + npolys*sizeof(csofit2_poly_t));
    subset->npolys = 0;
    for(i = 0; i < fits->npolys; i++)
    {
        if((fits->polys[i].end > t0) && (fits->polys[i].start < t1))
        {
            memcpy(&subset->polys[subset->npolys], &fits->polys[i], sizeof(csofit2_poly_t));
            subset->npolys += 1;
        }
    }
    return subset;
}

double csofit2_calc(csofit2_t * fits, double t)
{
    if(!fits || fits->npolys <= 0)
    {
        return 0.0;  /* an improbable value */
    }
    double w = 0.0;
    double y = 0.0;
    int i;
    for(i = 0; i < fits->npolys; i++)
    {
        double width = fits->polys[i].end - fits->polys[i].start;
        double center = (fits->polys[i].end + fits->polys[i].start) * 0.5;
        /* calc weight, double smoothstep envelope with 1s fudge */
        double fw = 1.0 - (fabs(t - center) / (width*0.5 + 1.0));
        if(fw < 0.0)
        {
            continue;
        }
        fw = 3.0*fw*fw - 2.0*fw*fw*fw;
        /* evaluate polynomial fit at this point */
        double fy = fits->polys[i].coefs[fits->polys[i].deg];
        double ft = (t - fits->polys[i].start) / width;
        double ftpow = ft;
        int j;
        for(j = fits->polys[i].deg-1; j >= 0; j--)
        {
            fy += fits->polys[i].coefs[j] * ftpow;
            ftpow *= ft;
        }
        y += fw*fy;
        w += fw;
    }
    if(w == 0.0)
    {
        return 0.0;
    }
    else
    {
        return y/w;
    }
}
