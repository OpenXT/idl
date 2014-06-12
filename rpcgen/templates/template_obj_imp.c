#include <dbus/dbus-glib.h>
#include <stdlib.h>
#include <limits.h>

#include "@OBJECT_HEADER_FILE@"
#include "@MARSHALL_FILE@"

extern const DBusGObjectInfo dbus_glib_@object@_object_info;
DBusGObjectInfo dbus_glib_@object@_object_info_modified;

G_DEFINE_TYPE (@GLIB_OBJ@, @object@_object, G_TYPE_OBJECT);

enum {
  PROP_0,
  @enum_properties@
  N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };

static void
handle_set_property(GObject *obj, guint propid, const GValue *value, GParamSpec *spec)
{
    @GLIB_OBJ@ *self = @OBJECT@_OBJECT (obj);
    switch (propid) {
    @set_property_body@
    }
}

static void
handle_get_property(GObject *obj, guint propid, GValue *value, GParamSpec *spec)
{
    @GLIB_OBJ@ *self = @OBJECT@_OBJECT (obj);
    switch (propid) {
    @get_property_body@
    }
}

static void @object@_object_init (@GLIB_OBJ@ *obj)
{
}

static void @object@_object_class_init (@GLIB_CLASS@ *klass)
{
    GObjectClass *gc = G_OBJECT_CLASS(klass);
    @init_properties@
    gc->set_property = handle_set_property;
    gc->get_property = handle_get_property;
    g_object_class_install_properties(gc, N_PROPERTIES, obj_properties);
}

@GLIB_OBJ@ *@object@_create_glib_obj()
{
    dbus_glib_@object@_object_info_modified = dbus_glib_@object@_object_info;
    dbus_glib_@object@_object_info_modified.exported_signals = "\0";
    dbus_g_object_type_install_info(@OBJECT@_TYPE_OBJECT, &dbus_glib_@object@_object_info_modified);
    return g_object_new(@OBJECT@_TYPE_OBJECT, NULL);
}

@GLIB_OBJ@ *@object@_export_dbus(DBusGConnection *conn, const char *path)
{
    @GLIB_OBJ@ *obj = @object@_create_glib_obj();
    if (!obj) {
        return NULL;
    }
    dbus_g_connection_register_g_object(conn, path, G_OBJECT(obj));
    return obj;
}
