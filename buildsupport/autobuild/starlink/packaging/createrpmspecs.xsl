<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
 
<xsl:template match="/package">

%define debug_package %{nil}
%define name <xsl:value-of select="@component"/>
%define version <xsl:value-of select="version"/>
%define release <xsl:value-of select="release"/><xsl:text>&#10;</xsl:text>
%define destdir %{_topdir}/tmp/%{name}-%{version}
Summary: <xsl:value-of select="description"/>
Name: %{name}
Version: %{version}
Release: %{release}
Copyright: <xsl:value-of select="copyright"/>
Vendor: <xsl:value-of select="vendor"/>
Packager: <xsl:value-of select="packager"/>
URL: <xsl:value-of select="url"/>
Source0: <xsl:value-of select="sourceurl"/>
Group: <xsl:value-of select="group"/>
BuildRoot: %{_topdir}/tmp/%{name}-%{version}
Prefix: %{starprefix}
<xsl:for-each select="run-dependencies/run">
   <xsl:variable name="newversion">
      <xsl:call-template name="replace-string"> <!-- imported template -->
        <xsl:with-param name="text" select="."/>
        <xsl:with-param name="replace" select="'-'"/>
        <xsl:with-param name="with" select="'.'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
     <xsl:when test="@name = 'tcl' or @name = 'tk' or @name = 'itcl'">
Requires: tclsys
     </xsl:when>
     <xsl:otherwise>
Requires: <xsl:value-of select="@name"/> >= <xsl:value-of select="$newversion"/>
     </xsl:otherwise>
    </xsl:choose>
    </xsl:for-each>
<xsl:choose>
<xsl:when test="@component = 'htx'">
Requires: init
</xsl:when>
<xsl:when test="@component = 'init'">
provides: /bin/sh
</xsl:when>
<xsl:otherwise>
Requires: htx
Requires: init
</xsl:otherwise>
</xsl:choose>
<xsl:for-each select="build-dependencies/build">
   <xsl:variable name="newversion">
      <xsl:call-template name="replace-string"> <!-- imported template -->
        <xsl:with-param name="text" select="."/>
        <xsl:with-param name="replace" select="'-'"/>
        <xsl:with-param name="with" select="'.'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
     <xsl:when test="@name = 'tcl' or @name = 'tk' or @name = 'itcl'">
BuildRequires: tclsys
     </xsl:when>
     <xsl:otherwise>
BuildRequires: <xsl:value-of select="@name"/> >= <xsl:value-of select="$newversion"/>
     </xsl:otherwise>
    </xsl:choose>
    </xsl:for-each>
AutoReqProv: no
%description
<xsl:value-of select="abstract"/>
%prep
%setup -q

%build
mkdir -p $RPM_BUILD_ROOT
./configure --prefix=%{starprefix} --with-starlink=%{starlink}
make

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
make DESTDIR="%{destdir}" install

%clean
rm -rf $RPM_BUILD_ROOT

%post
if test -f %{starlink}/bin/hlink; then
 %{starlink}/bin/hlink %{starprefix}/docs

else if test -f %{starprefix}/bin/hlink; then
 %{starprefix}/bin/hlink %{starprefix}/docs
else 
 echo "No hlink command found, cannot re-link documents."
fi
fi
if test -f %{starprefix}/manifests/%{name}; then
sed -e 's#%{_topdir}/tmp/%{name}-%{version}##g' \
%{starprefix}/manifests/%{name} > \
/tmp/%{name}.tmp
cp -f /tmp/%{name}.tmp %{starprefix}/manifests/%{name}
rm -f /tmp/%{name}.tmp
fi

%postun
if test -f %{starlink}/bin/hlink; then
 %{starlink}/bin/hlink %{starprefix}/docs

else if test -f %{starprefix}/bin/hlink; then
 %{starprefix}/bin/hlink %{starprefix}/docs
else 
 echo "No hlink command found, cannot re-link documents."
fi
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
<xsl:for-each select="files/file">
<xsl:choose>
<xsl:when test="@dir = 'docs' and @type != 'd'">
%doc %attr(<xsl:value-of select="@permissions-octal"/>, %{staradmin}, %{stargroup}) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@dir = 'bin'">
%attr(<xsl:value-of select="@permissions-octal"/>, %{staradmin}, %{stargroup}) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:when test="@type = 'd'">
%dir %attr(<xsl:value-of select="@permissions-octal"/>, %{staradmin}, %{stargroup}) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
</xsl:when>
<xsl:otherwise>
%attr(<xsl:value-of select="@permissions-octal"/>, %{staradmin}, %{stargroup}) %{prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>
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
