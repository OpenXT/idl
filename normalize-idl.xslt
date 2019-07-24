<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:tp="http://telepathy.freedesktop.org/wiki/DbusSpec#extensions-v0">
  <xsl:output method="xml" indent="yes" omit-xml-declaration="yes" doctype-public="-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" doctype-system="http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"/>
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="tp:docstring p li ul ol"/>

  <!-- start from the root node, sorting interfaces by name. -->
  <xsl:template match="/node">
    <xsl:copy>
      <xsl:apply-templates select="@*">
        <xsl:sort select="name()"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="tp:enum">
        <xsl:sort select="@name"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="./interface">
        <xsl:sort select="@name"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <!-- Preserve everything in comments, attributes, etc -->
  <xsl:template match="@*|comment()|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Organize the contents of interface definitions. -->
  <xsl:template match="/node/interface">
    <xsl:comment>~~~~~~~~~~~~~~~~~~~~~~
  ~~~~  DBus Interface  ~~~~
  ~~~~~~~~~~~~~~~~~~~~~~~</xsl:comment>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>

      <!-- Maintain enumeration definitions, but sort them by name -->
      <xsl:apply-templates select="tp:enum">
        <xsl:sort select="@name"/>
      </xsl:apply-templates>

      <!-- Copy any defined documentation -->
      <xsl:apply-templates select="tp:docstring"/>

      <!-- Copy all methods, if there are any, and sort them by name -->
      <xsl:if test="method">
        <xsl:comment>~~~~~~~~~~~~~~~~~~~~~~
    ~~~~  DBus Methods    ~~~~
    ~~~~~~~~~~~~~~~~~~~~~~~</xsl:comment>
        <xsl:apply-templates select="method">
          <xsl:sort select="@name"/>
        </xsl:apply-templates>
      </xsl:if>

      <!-- Copy all signals, if there are any, and sort them by name -->
      <xsl:if test="signal">
        <xsl:comment>~~~~~~~~~~~~~~~~~~~~~~
    ~~~~  DBus Signals    ~~~~
    ~~~~~~~~~~~~~~~~~~~~~~~</xsl:comment>
        <xsl:apply-templates select="signal">
          <xsl:sort select="@name"/>
        </xsl:apply-templates>
      </xsl:if>

      <!-- Copy all properties, if there are any, and sort them by name -->
      <xsl:if test="property">
        <xsl:comment>~~~~~~~~~~~~~~~~~~~~~~
    ~~~~  DBus Properties ~~~~
    ~~~~~~~~~~~~~~~~~~~~~~~</xsl:comment>
        <xsl:apply-templates select="property">
          <xsl:sort select="@name"/>
        </xsl:apply-templates>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <!-- Maintain all subnodes of methods. Order of <arg> elements DOES matter, so never reorder them. -->
  <xsl:template match="/node/interface/method">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Maintain all subnodes of signals. Order of <arg> elements DOES matter, so never reorder them. -->
  <xsl:template match="/node/interface/signal">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Maintain all subnodes of properties. -->
  <xsl:template match="/node/interface/property">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Maintain all documentation as-is. May change when auto-generating documentation based on that tool's requirements. -->
  <xsl:template match="tp:docstring">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Maintain all enum documentation and values as-is. -->
  <xsl:template match="tp:enum">
    <xsl:copy-of select="."/>
  </xsl:template>
</xsl:stylesheet>
