## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

## The only sane way to attach the driver files to the installed libgks.la
## is to build up a local libgks_drivers.la consisting of subdirs
## libraries and then LIBADD that in the parent. The parent makefile should
## not have to know which drivers were requested in drivers/configure.ac

noinst_LTLIBRARIES = libgks_drivers.la

libgks_drivers_la_SOURCES =

## LIBADD should be dynamic
libgks_drivers_la_LIBADD = wiss/libgks_driver_wiss.la \
mi/libgks_driver_mi.la \
mo/libgks_driver_mo.la \
cgmi/libgks_driver_cgmi.la \
cgmo/libgks_driver_cgmo.la \
postscript/libgks_driver_postscript.la \
tek4010/libgks_driver_tek4010.la \
xwindows/libgks_driver_xwindows.la \
canon/libgks_driver_canon.la \
regis/libgks_driver_regis.la
