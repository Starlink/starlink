#ifndef FLU_DEFINED
#define FLU_DEFINED

#include <stdio.h>

#define FLU_NPLANET 10
#define FLU_MAXFILT 15
#define FLU_NOTELEN 81
#define FLU_FILTLEN 11
#define FLU_MAXNOTE 50

extern char* planet[FLU_NPLANET];
extern int lup[FLU_NPLANET];

typedef struct {
        double lon;
        double lat;
        double height;
        char name[11];
        char fullname[41];
} FluTelescope;

typedef struct {
        int iy;
        int m;
        int id;
        int ih;
        int im;
        int is;
        double s;
        char cmon[4];
} FluDateTime;

FILE* flu_asfio(
        char* pnfile, char* acmode, int* exclaim,
        char* pathname, size_t pathname_size, int* status);

int flu_customfilt(
        const char* const reqbody, char fname[][FLU_FILTLEN],
        int* nf, int* nb, double freq[][2],
        double* hpbw1, double* hpbw2, double* amp1, double* amp2,
        double* tbnorm, double* error,
        size_t* inote, char note[][FLU_NOTELEN], int* status);

void flu_geoeph(
        int nopl, double* exra, double* exdec, double* dist,
        double rjd, FluTelescope* telescope, int* status);

void flu_pbflux(
        double omega, double freq[][2], double tb[],
        double hpbw[], double hpbw2[], double amp1[], double amp2[],
        int nf, char fname[][FLU_FILTLEN], double er[],
        char* filter, char* body, int screen, FILE* fiod, int* status);

void flu_poleplan(int ib, double ajd, double* rap, double* decp);

void flu_read_data(
        char* pathname, char fname[][FLU_FILTLEN],
        int* nf, int* nb, double freq[][2],
        double hpbw[], double hpbw2[], double amp1[], double amp2[],
        double tbnorm[][FLU_MAXFILT], double error[][FLU_MAXFILT],
        size_t* inote, char note[][FLU_NOTELEN], int* status);

double flu_rjdate(FluDateTime* time);

void flu_slajpl(
        double date, int np, double elong, double phi, double height,
        double* ra, double* dec, double* r, double* stl, double* airm);

void flu_slajpl2(
        double date, int np, double elong, double* ra, double* dec, double* r);

double flu_slalast(double date, double elong);

double flu_solidangle(
        int j, double rjd, double ra, double dec, double gd,
        int screen, FILE* fiod, int* status);

double flu_tb350(double rjd, int* status);

void flu_topeph(
        char* reqbody, double rjd, FluDateTime* time,
        FluTelescope* telescope, int pos,
        int screen, FILE* fiod, int* status);
#endif
