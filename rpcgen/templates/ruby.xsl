<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:tp="http://telepathy.freedesktop.org/wiki/DbusSpec#extensions-v0"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="tp str">

  <xsl:param name="signal_file"/>
  <xsl:param name="method_file"/>
  <xsl:param name="constants_file"/>
  <xsl:param name="signal_class"/>
  <xsl:param name="method_class"/>
  <xsl:param name="method_impl_class"/>

  <xsl:variable name="lcase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="ucase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />

  <xsl:template match="node">
    <xsl:document href="{$signal_file}" method="text">
require 'dbus_rpc/dbus_patch'
class <xsl:value-of select="$signal_class"/> <![CDATA[< DBus::Object
]]><xsl:apply-templates select="interface[signal]"/>
end
    </xsl:document>
    <xsl:document href="{$method_file}" method="text">
require 'dbus_rpc/dbus_patch'
class <xsl:value-of select="$method_class"/> <![CDATA[< DBus::Object
]]><xsl:apply-templates select="interface[method]"/>
end
    </xsl:document>
    <xsl:document href="{$constants_file}" method="text">
<xsl:apply-templates select="tp:enum"/>
    </xsl:document>
  </xsl:template>

  <xsl:template match="interface">
    <xsl:apply-templates select="tp:docstring"/>
	dbus_interface "<xsl:value-of select="@name"/>" do
<xsl:apply-templates select="signal|method"/>
	end
  </xsl:template>

  <xsl:template name="join-args">
    <xsl:if test="arg">
      <xsl:text>, "</xsl:text>
      <xsl:for-each select="arg">
        <xsl:if test="parent::method">
          <xsl:value-of select="@direction"/><xsl:text> </xsl:text>
        </xsl:if>
        <xsl:value-of select="@name"/>:<xsl:value-of select="@type"/>
        <xsl:if test="position() != last()">, </xsl:if>
      </xsl:for-each>
      <xsl:text>"</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="signal">
    <xsl:apply-templates select="tp:docstring"/>
    <xsl:apply-templates select="arg/tp:docstring"/>
		dbus_signal :<xsl:value-of select="@name"/><xsl:call-template name="join-args"/>
    <xsl:text>
    </xsl:text>
  </xsl:template>

  <xsl:template match="method">
    <xsl:apply-templates select="tp:docstring"/>
    <xsl:apply-templates select="arg/tp:docstring"/>
		dbus_method :<xsl:value-of select="@name"/><xsl:call-template name="join-args"/><xsl:text> do</xsl:text>
        <xsl:if test="arg[@direction='in']">
          <xsl:text> |</xsl:text>
          <xsl:for-each select="arg[@direction='in']">
            <xsl:value-of select="@name"/>
            <xsl:if test="position() != last()">, </xsl:if>
          </xsl:for-each>
          <xsl:text>|</xsl:text>
        </xsl:if>
        		return <xsl:value-of select="$method_impl_class"/>::<xsl:value-of select="str:replace(concat(translate(substring(parent::interface/@name, 1, 1), 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'), substring(parent::interface/@name, 2)), '.', '_')"/>.<xsl:value-of select="@name"/>(self<xsl:for-each select="arg[@direction='in']">
          <xsl:text>, </xsl:text>
          <xsl:value-of select="@name"/>
        </xsl:for-each>)
		end
  </xsl:template>

  <xsl:template match="interface/tp:docstring">
	# <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="signal/tp:docstring|method/tp:docstring">
    <xsl:for-each select="str:tokenize(., '&#xA;&#xD;')">
		# <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tp:enum/tp:docstring">
    <xsl:for-each select="str:tokenize(., '&#xA;&#xD;')">
	# <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="arg/tp:docstring">
    <xsl:variable name="p" select="parent::node()"/>
    <xsl:variable name="t" select="$p/@tp:type"/>
		# <xsl:value-of select="$p/@name"/> - <xsl:value-of select="."/><xsl:if test="$t"> (<xsl:value-of select="$t"/>)</xsl:if>
  </xsl:template>

  <xsl:template match="token">
    <xsl:value-of select="translate(substring(., 1, 1), $lcase, $ucase)"/><xsl:value-of select="translate(substring(., 2), $ucase, $lcase)"/>
  </xsl:template>

  <xsl:template match="tp:enum">
class <xsl:apply-templates select="str:tokenize(@name, '_')"/><xsl:text>
</xsl:text>
  <xsl:apply-templates select="tp:docstring"/>
<xsl:text>
</xsl:text>
<xsl:apply-templates select="tp:enumvalue"/>end
  </xsl:template>

  <xsl:template match="tp:enumvalue">
    <xsl:variable name="p" select="parent::node()"/>
    <xsl:variable name="t" select="$p/@type"/>
	<xsl:text>	</xsl:text>
	<xsl:value-of select="@suffix"/> = <xsl:choose>
<xsl:when test="$t = 'i'"><xsl:value-of select="@value"/></xsl:when>
<xsl:when test="$t = 's'">"<xsl:value-of select="@value"/>"</xsl:when>
</xsl:choose>
<xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>

