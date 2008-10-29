<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
                xmlns:vs="http://www.ivoa.net/xml/VODataService/v1.0" 
                xmlns:stc="http://www.ivoa.net/xml/STC/stc-v1.30.xsd" 
                xmlns:vr="http://www.ivoa.net/xml/VOResource/v1.0" 
                xmlns:ri="http://www.ivoa.net/xml/RegistryInterface/v1.0" 
                xmlns:vot="http://www.ivoa.net/xml/VOTable/v1.1" 
                xmlns="http://www.ivoa.net/xml/VOTable/v1.1" 
                version="1.0">

<!--
  -  Changes:
  -  17Jun08  rlp   for capability information, attempt to only select
  -                    data for standard services.  This is likely a temporary
  -                    solution.
  -  20Oct08  pwd   copied from NVO source:
  -  http://trac.us-vo.org/nvo/browser/registry/STReg/trunk/src/dev/nvonew/xsl/RegistryResults_vot3.xsl?rev=466
  -->

   <xsl:template match="/">
      <xsl:apply-templates select="ri:VOResources" />
   </xsl:template>

   <xsl:template match="ri:VOResources" xml:space="preserve">
<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.1">
   <DESCRIPTION>Registry Search Results</DESCRIPTION>
   <RESOURCE name="Search Results">
      <TABLE name="results">
         <FIELD ID="tags" name="categories" datatype="char" arraysize="*"/>
         <FIELD ID="shortName" name="shortName" datatype="char" arraysize="*"/>
         <FIELD ID="title" name="title" datatype="char" arraysize="*"/>
         <FIELD ID="description" name="description" datatype="char" arraysize="*"/>
         <FIELD ID="publisher" name="publisher" datatype="char" arraysize="*"/>
         <FIELD ID="waveband" name="waveband" datatype="char" arraysize="*"/>
         <FIELD ID="identifier" name="identifier" datatype="char" arraysize="*" ucd="ID_MAIN"/>
         <FIELD ID="updated" name="descriptionUpdated" datatype="char" arraysize="*"/>
         <FIELD ID="subject" name="subject" datatype="char" arraysize="*"/>
         <FIELD ID="type" name="type" datatype="char" arraysize="*"/>
         <FIELD ID="contentLevel" name="contentLevel" datatype="char" arraysize="*"/>
         <FIELD ID="regionOfRegard" name="typicalRegionSize" datatype="int" unit="arcsec"/>
         <FIELD ID="version" name="version" datatype="char" arraysize="*"/>
         <FIELD ID="capabilityClass" name="capabilityClass" datatype="char" arraysize="*"/>
         <FIELD ID="capabilityID" name="capabilityStandardID" datatype="char" arraysize="*"/>
         <FIELD ID="capabilityValidationLevel" name="capabilityValidationLevel" datatype="char" arraysize="*"/>
         <FIELD ID="interfaceClass" name="interfaceClass" datatype="char" arraysize="*"/>
         <FIELD ID="interfaceVersion" name="interfaceVersion" datatype="char" arraysize="*"/>
         <FIELD ID="interfaceRole" name="interfaceRole" datatype="char" arraysize="*"/>
         <FIELD ID="accessURL" name="accessURL" datatype="char" arraysize="*"/>
         <FIELD ID="supportedInputParam" name="supportedInputParam" datatype="char" arraysize="*"/> 
         <FIELD ID="maxRadius" name="maxSearchRadius" datatype="int"/>
         <FIELD ID="maxRecords" name="maxRecords" datatype="int"/>
         <FIELD ID="publisherID" name="publisherIdentifier" datatype="char" arraysize="*"/>
         <FIELD ID="referenceURL" name="referenceURL" datatype="char" arraysize="*"/>
         <DATA>
            <TABLEDATA>
               <xsl:apply-templates select="ri:Resource" />
            </TABLEDATA>
         </DATA>
      </TABLE>
   </RESOURCE>

</VOTABLE>
   </xsl:template>

   <xsl:template match="ri:Resource">
      <TR><xsl:text>
</xsl:text>
         <xsl:text>   </xsl:text>
         <TD><xsl:apply-templates select="." mode="gettag"/></TD><xsl:text>
</xsl:text>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="shortName" />
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="title" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="content/description" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="curation/publisher" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="coverage/waveband" />   
            <xsl:with-param name="asarray" select="true()"/>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="identifier" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="@updated" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="content/subject" />   
            <xsl:with-param name="asarray" select="true()"/>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="content/type" />   
            <xsl:with-param name="asarray" select="true()"/>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="content/contentLevel" />   
            <xsl:with-param name="asarray" select="true()"/>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="coverage/stc:STCResourceProfile/stc:AstroCoords/stc:Position1D/stc:Size" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="curation/version" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type and 
                                    interface[@role='std']]/@xsi:type" />
            <xsl:with-param name="removeScope" select="true()"></xsl:with-param>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type and 
                                    interface[@role='std']]/@standardID" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type and 
                                    interface[@role='std']]/validationLevel" />
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val"
                 select="capability[@xsi:type]/interface[@role='std']/self::node()[1]/@xsi:type" />
            <xsl:with-param name="removeScope" select="true()"></xsl:with-param>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type]/interface[@role='std']/self::node()[1]/@version" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type]/interface[@role='std']/self::node()[1]/@role" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type]/interface[@role='std']/self::node()[1]/accessURL[1]" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" 
                 select="capability[@xsi:type]/interface[@role='std']/self::node()[1]/param/name" />   
            <xsl:with-param name="asarray" select="true()"/>
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="capability/maxSearchRadius|capability/maxSR" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="capability/maxRecords" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="curation/publisher/@ivo-id" />   
         </xsl:call-template>
         <xsl:call-template name="valOrNull">
            <xsl:with-param name="val" select="content/referenceURL" />   
         </xsl:call-template>
      </TR><xsl:text>
</xsl:text>
   </xsl:template>

  <xsl:template name="valOrNull">
      <xsl:param name="val"/>
      <xsl:param name="asarray" select="false()"/>
      <xsl:param name="removeScope" select="false()"/>
      <xsl:variable name="count" select="count($val)"/>

      <xsl:text>   </xsl:text>
      <TD>
         <xsl:choose>
            <xsl:when test="$asarray">
               <xsl:if test="count($val)>0">
                  <xsl:text>#</xsl:text>
               </xsl:if>
               <xsl:for-each select="$val">
                  <xsl:value-of select="normalize-space(.)"/>
                  <xsl:text>#</xsl:text>
               </xsl:for-each>
            </xsl:when>
           <xsl:otherwise>
             <xsl:variable name="withscope">
               <xsl:value-of select="normalize-space($val)"/>
             </xsl:variable>
             <xsl:choose>
               <xsl:when test="$removeScope and contains($withscope, ':')">
                 <xsl:value-of select="substring-after($withscope, ':')"/>
               </xsl:when>
               <xsl:otherwise>
                 <xsl:value-of select="$withscope"/>
               </xsl:otherwise>
             </xsl:choose>
           </xsl:otherwise>
         </xsl:choose>
      </TD><xsl:text>
</xsl:text>
   </xsl:template>

   <xsl:template match="*[identifier]" mode="gettag">
      <xsl:variable name="rxsitype" select="substring-after(@xsi:type,':')"/>
                    
      <xsl:text>#</xsl:text>
      <xsl:choose>
         <xsl:when test="$rxsitype='Registry'">
            <xsl:if test="capability[substring-after(@xsi:type,':')='Search']">
               <xsl:text>Searchable</xsl:text>
            </xsl:if>
            <xsl:if test="capability[substring-after(@xsi:type,':')='Search'] and capability[substring-after(@xsi:type,':')='Harvest']">
               <xsl:text> </xsl:text>
            </xsl:if>
            <xsl:if test="capability[substring-after(@xsi:type,':')='Harvest']">
               <xsl:text>Publishing</xsl:text>
            </xsl:if>
            <xsl:text> Registry#</xsl:text>
         </xsl:when>
         <xsl:when test="capability">
           <xsl:for-each select="capability">
            <xsl:variable name="cxsitype" select="substring-after(@xsi:type,':')"/>
            <xsl:choose>
               <xsl:when test="$cxsitype='ConeSearch' or $cxsitype='OpenSkyNode'">
                  <xsl:text>Catalog</xsl:text>
               </xsl:when>
               <xsl:when test="$cxsitype='SimpleImageAccess'">
                   <xsl:text>Images</xsl:text>
               </xsl:when>
               <xsl:when test="$cxsitype='SimpleSpectralAccess'">
                   <xsl:text>Spectra</xsl:text>
               </xsl:when>
               <xsl:when test="$cxsitype='Search' or $cxsitype='Harvest'">
                   <xsl:text>Registry</xsl:text>
               </xsl:when>
              <xsl:when test="interface">
                <xsl:for-each select="interface">
                  <xsl:variable name="ixsitype" select="substring-after(@xsi:type,':')"/>
                  <xsl:choose>
                    <!--handled above, if this is a well-formed record-->
                    <xsl:when test="$ixsitype='ConeSearch' or $ixsitype='OpenSkyNode'">
                      <xsl:text></xsl:text>
                    </xsl:when>
                    <xsl:when test="$ixsitype='ParamHTTP'">
                      <xsl:text>HTTP Request</xsl:text>
                    </xsl:when>
                    <xsl:when test="$ixsitype='WebBrowser'">
                      <xsl:text>Web Page</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:text>Custom Service</xsl:text>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:for-each>
              </xsl:when>
               <xsl:otherwise>
                  <xsl:text>Custom Service</xsl:text>
               </xsl:otherwise>
            </xsl:choose>
            <xsl:text>#</xsl:text>
           </xsl:for-each>
         </xsl:when>
         <xsl:when test="$rxsitype='DataCollection'">
            <xsl:text>Data Collection#</xsl:text>
         </xsl:when>
         <xsl:when test="$rxsitype='Organisation'">
            <xsl:text>Organisation#</xsl:text>
         </xsl:when>
         <xsl:when test="contains($rxsitype,'Standard') or $rxsitype='Authority'">
            <xsl:text>VO Support#</xsl:text>
         </xsl:when>
         <xsl:when test="not(@xsi:type)">
            <xsl:text>Generic Resource</xsl:text>
         </xsl:when>
         <xsl:otherwise><xsl:value-of select="$rxsitype"/>#</xsl:otherwise>
      </xsl:choose>
   </xsl:template>  

</xsl:stylesheet>
