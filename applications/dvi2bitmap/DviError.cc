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

#if HAVE_STD_NAMESPACE
#define STD std
#else
#define STD
#endif

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

void DviError::print() const {
    STD::cerr << "DVI error: " << problem_ << STD::endl; }
void DviBug::print() const {
    STD::cerr << "BUG: " << problem_ << STD::endl; }

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

