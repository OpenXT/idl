static inline int property_set_@INTERFACE_@_@PROPERTY_@_ ( xcdbus_conn_t *xc, const char *service, const char *objpath, @T@ v )
{
    return @SETTER@ ( xc, service, objpath, "@INTERFACE@", "@PROPERTY@", v );
}
