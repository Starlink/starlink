#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#include <cstdarg>
#else
#include <stdlib.h>
#include <stdarg.h>
#endif
#include <string>
#include <iostream>

#include "DviError.h"

DviError::DviError(const char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

void DviError::print() const { cerr << "DVI error: " << problem_ << endl; }
void DviBug::print() const { cerr << "BUG: " << problem_ << endl; }

DviBug::DviBug(const char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

