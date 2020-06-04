#include "lean/object.h"
#include "lean/io.h"
#include <unistd.h>

typedef lean::object obj;

extern "C" obj* lean_io_run_cmd(obj* str, obj* r) {
    auto res = system(lean::string_cstr(str));
    return lean_mk_io_result(lean::box(res));
}

extern "C" obj* lean_io_set_env(obj* envname, obj* envval, obj* r) {
    auto res = setenv(lean::string_cstr(envname),
                      lean::string_cstr(envval), true);
    return lean_mk_io_result(lean::box(res));
}

extern "C" obj* lean_io_chdir(obj* str, obj* r) {
    auto res = chdir(lean::string_cstr(str));
    return lean_mk_io_result(lean::box(res));
}

extern "C" obj* lean_io_remove(obj* str, obj* r) {
    auto res = remove(lean::string_cstr(str));
    return lean_mk_io_result(lean::box(res));
}
