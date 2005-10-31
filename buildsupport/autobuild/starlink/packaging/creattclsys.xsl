<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
 <xsl:param name="tcl">tcl.xml</xsl:param>
 <xsl:param name="tk">tk.xml</xsl:param>
 <xsl:param name="itcl">itcl.xml</xsl:param>
<xsl:template match="/package">

%define debug_package %{nil}
%define name tclsys
%define version <xsl:value-of select="document($tcl)//package/version"/>
%define release <xsl:value-of select="document($tcl)//package/release"/><xsl:text>&#10;</xsl:text>
%define destdir %{_topdir}/tmp/%{name}-%{version}
Summary: TCL <xsl:value-of select="document($tcl)//package/version"/>, TK <xsl:value-of select="document($tk)//package/version"/> and ITCL <xsl:value-of select="document($itcl)//package/version"/> combined package.
Name: %{name}
Version: %{version}
Release: %{release}
Copyright: BSD
Vendor: <xsl:value-of select="document($tcl)//package/vendor"/>
Packager: <xsl:value-of select="document($tcl)//package/packager"/>
URL: <xsl:value-of select="document($tcl)//package/url"/>
Source0: <xsl:value-of select="document($tcl)//package/sourceurl"/>
Source1: <xsl:value-of select="document($tk)//package/sourceurl"/>
Source2: <xsl:value-of select="document($itcl)//package/sourceurl"/>
Group: <xsl:value-of select="document($tcl)//package/group"/>
BuildRoot: %{_topdir}/tmp/%{name}-%{version}
Prefix: %{starprefix}
provides: tcl >= <xsl:value-of select="document($tcl)//package/version"/>
provides: tk >= <xsl:value-of select="document($tk)//package/version"/>
provides: itcl >= <xsl:value-of select="document($itcl)//package/version"/>
AutoReqProv: no
%description
<xsl:value-of select="document($tcl)//package/description"/><xsl:text>&#10;</xsl:text>
<xsl:value-of select="document($tk)//package/description"/><xsl:text>&#10;</xsl:text>
<xsl:value-of select="document($itcl)//package/description"/>
%prep
mkdir -p %{name}-%{version}

%setup -D -T -a 0
%setup -D -T -a 1
%setup -D -T -a 2

%build
mkdir -p $RPM_BUILD_ROOT
cd %{_topdir}/BUILD/%{name}-%{version}/tcl-%{version}
./configure --prefix=%{starprefix} --with-starlink=%{starlink}

make
cd %{_topdir}/BUILD/%{name}-%{version}/tk-%{version}
./configure --prefix=%{starprefix} --with-starlink=%{starlink} \
            --with-tcl=%{_topdir}/BUILD/%{name}-%{version}/tcl-%{version}/unix

make
cd %{_topdir}/BUILD/%{name}-%{version}/itcl-<xsl:value-of select="document($itcl)//package/version"/>
./configure --prefix=%{starprefix} --with-starlink=%{starlink} \
            --with-tcl=%{_topdir}/BUILD/%{name}-%{version}/tcl-%{version}/unix \
            --with-tk=%{_topdir}/BUILD/%{name}-%{version}/tk-%{version}/unix

make SHLIB_LDFLAGS="-L%{_topdir}/BUILD/%{name}-%{version}/tcl-%{version}/unix"


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p %{destdir}/%{starprefix}/bin
mkdir -p %{destdir}/%{starprefix}/docs
mkdir -p %{destdir}/%{starprefix}/etc
mkdir -p %{destdir}/%{starprefix}/help
mkdir -p %{destdir}/%{starprefix}/include
mkdir -p %{destdir}/%{starprefix}/lib
mkdir -p %{destdir}/%{starprefix}/man
mkdir -p %{destdir}/%{starprefix}/manifests
mkdir -p %{destdir}/%{starprefix}/news
mkdir -p %{destdir}/%{starprefix}/share
mkdir -p %{destdir}/%{starprefix}/buildsupport
cd %{_topdir}/BUILD/%{name}-%{version}/tcl-%{version}
make DESTDIR="%{destdir}" install
cd %{_topdir}/BUILD/%{name}-%{version}/tk-%{version}
make DESTDIR="%{destdir}" install
cd %{_topdir}/BUILD/%{name}-%{version}/itcl-3.1.0
make DESTDIR="%{destdir}" install

%clean
rm -rf $RPM_BUILD_ROOT

%post

if test -f %{starprefix}/manifests/tcl; then
sed -e 's#%{_topdir}/tmp/%{name}-%{version}##g' \
%{starprefix}/manifests/tcl > \
/tmp/tcl.tmp
cp -f /tmp/tcl.tmp %{starprefix}/manifests/tcl
rm -f /tmp/tcl.tmp
fi
if test -f %{starprefix}/manifests/tk; then
sed -e 's#%{_topdir}/tmp/%{name}-%{version}##g' \
%{starprefix}/manifests/tk > \
/tmp/tk.tmp
cp -f /tmp/tk.tmp %{starprefix}/manifests/tk
rm -f /tmp/tk.tmp
fi
if test -f %{starprefix}/manifests/itcl; then
sed -e 's#%{_topdir}/tmp/%{name}-%{version}##g' \
%{starprefix}/manifests/itcl > \
/tmp/itcl.tmp
cp -f /tmp/itcl.tmp %{starprefix}/manifests/itcl
rm -f /tmp/itcl.tmp
fi

%files
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/bin
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/docs
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/etc
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/help
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/include
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/lib
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/man
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/manifests
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/news
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/share
%dir %attr(0755, %{staradmin}, %{stargroup}) %{prefix}/buildsupport
<xsl:for-each select="document($tcl)//package/files/file">
<xsl:choose>
<xsl:when test="@dir = 'docs' and @type != 'd'">
%doc %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@dir = 'bin'">
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@type = 'd'">
%dir %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:otherwise>
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:otherwise>
</xsl:choose>
</xsl:for-each>
<xsl:for-each select="document($tk)//package/files/file">
<xsl:choose>
<xsl:when test="@dir = 'docs' and @type != 'd'">
%doc %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@dir = 'bin'">
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@type = 'd'">
%dir %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:otherwise>
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:otherwise>
</xsl:choose>
</xsl:for-each>
<xsl:for-each select="document($itcl)//package/files/file">
<xsl:choose>
<xsl:when test="@dir = 'docs' and @type != 'd'">
%doc %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@dir = 'bin'">
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@type = 'd'">
%dir %attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:otherwise>
%attr(<xsl:value-of select="@permissions-octal"/>, -, -) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:otherwise>
</xsl:choose>
</xsl:for-each>
</xsl:template>
 
 <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="with"/>
    <xsl:choose>
      <xsl:when test="contains($text,$replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$with"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text"
select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="with" select="$with"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
