%define name sextractor
%define version 2.3
%define release 1tpx

Summary: resample and combine astronomical FITS images
Name: %{name}
Version: %{version}
Release: %{release}
Source0: ftp://ftp.iap.fr/pub/from_users/bertin/sextractor/%{name}-%{version}.tar.gz
URL: http://terapix.iap.fr/soft/sextractor/
License: LGPL
Group: Sciences/Astronomy
BuildRoot: %{_tmppath}/%{name}-buildroot
Prefix: %{_prefix}

%description
SExtractor stands for ``Source Extractor'': a software for making catalog of sources from astronomical images.

%prep
%setup -q

%build
./configure --prefix=$RPM_BUILD_ROOT/usr/local/ --mandir=$RPM_BUILD_ROOT/usr/local/man/

make

%install
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/local/bin/sex
/usr/local/man/man1/sex.1
/usr/local/man/manx/sex.x
%doc AUTHORS BUGS ChangeLog COPYING HISTORY INSTALL README THANKS doc/README.DOC doc/sex2_doc.ps

%changelog
* Tue May 13 2003 Emmanuel Bertin <bertin@iap.fr>
- RPM build for V2.3
* Fri Apr 04 2003 Emmanuel Bertin <bertin@iap.fr>
- RPM build for V2.3b4
* Wed Mar 05 2003 Emmanuel Bertin <bertin@iap.fr>
- RPM build for V2.3b3

# end of file
