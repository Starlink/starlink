
/* needed for strptime, -std=c99 */
#define _XOPEN_SOURCE

#include "csofit2.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <time.h>

#include "prm_par.h"

/* Local definition of timegm */
static time_t my_timegm (struct tm *tm );

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
        poly.start = my_timegm(&utc);
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;
        if(!strptime(tok, "%Y-%m-%dT%H:%M:%S", &utc)) continue;
        poly.end = my_timegm(&utc);
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
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;  /* rms_limit */
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;  /* dev_limit */
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;
        poly.mar = strtod(tok, &endptr); if(*endptr != '\0') continue;
        tok = strtok_r(NULL, delim, &saveptr); if(!tok) continue;
        poly.mar_w = strtod(tok, &endptr); if(*endptr != '\0') continue;
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
        return VAL__BADD;
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
        return VAL__BADD;
    }
    else
    {
        return y/w;
    }
}

/*
  timegm is a BSD and GNU extension and technically not part of POSIX.
  The version below comes from the 'Seconds Since the Epoch' formula
  presented in the Single Unix Specification, section 4.14 of the XBD:
  http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap04.html#tag_04_14

  Since strptime(3) may not set the tm_yday field, the function
  calls mktime(3) to make sure it is filled in and that tm is normalized.

  It may be that simply setting _BSD_SOURCE would be enough on all the systems that matter
  but for now we are defensive.

  This implementation should be thread-safe, unlike alternate implementations
  (such as that suggested by the timegm(3) manpage) which rely on
  setenv(TZ) + mktime.

 */

time_t
my_timegm(struct tm *tm)
{
    mktime(tm);
    time_t ret = tm->tm_sec + tm->tm_min*60 + tm->tm_hour*3600 + tm->tm_yday*86400;
    ret += ((time_t)31536000) * (tm->tm_year-70);
    ret += ((tm->tm_year-69)/4)*86400 - ((tm->tm_year-1)/100)*86400 + ((tm->tm_year+299)/400)*86400;
    return ret;
}

