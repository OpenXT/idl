static inline int property_get_@INTERFACE_@_@PROPERTY_@_ ( xcdbus_conn_t *xc, const char *service, const char *objpath, @T@ *v )
{
    return @GETTER@ ( xc, service, objpath, "@INTERFACE@", "@PROPERTY@", v );
}
