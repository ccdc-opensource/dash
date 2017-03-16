
# We can get only here, if we are actually build on Windows and CCDC_WANT_DASH is set, so we can hard code an error
# if there's no intel fortran compiler:

if(NOT $ENV{CCDC_IFORTRAN_AVAILABLE})
    message(
        # FATAL_ERROR
        STATUS
        "DASH cannot be compiled without an Intel Fortran compiler"
    )
    return()
endif()

# Compiling DASH only works, if the make-tool is the Microsoft Visual Studio _IDE_!
# (nmake, jom or ninja based build are not supported)
#
# 1. These tools don't know how to build a ".vfproj" file
# 2. Since the .vfproj contains both debug and release settings, we'd need to have much more information avaiable than
#    we can get from looking at the current environment variables.

if(NOT MSVC_IDE)
    message(
        FATAL_ERROR
        "Compiling DASH requires to build using the Microsoft Visual Studio IDE. NMake, Jom, Ninja or similar make tools are not supported."
    )
endif()


set(DASH_BUILD_DIR      "${DASH_BINARY_DIR}/dash.dir")
set(VFPRO_FILE          "${DASH_BUILD_DIR}/PCDash.vfproj")

add_custom_target(
    dash ALL

    COMMAND
        devenv /build /config:$<CONFIG> ${VFPRO_FILE}

    WORKING_DIRECTORY
        ${DASH_BUILD_DIR}
)

add_dependencies(
    dash

    # Hard coded dependencies: TODO Somehow generate this from within build tool
    guilib_qt
    licencelib_qt
)
