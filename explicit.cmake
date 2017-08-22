ccdc_configure_file(PCDash_Main.f90)
ccdc_configure_file(variables.f90)

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

set(DASH_BUILD_DIR "${DASH_BINARY_DIR}/dash.dir")
file(TO_NATIVE_PATH "${DASH_BUILD_DIR}/PCDash.vfproj" VFPRO_FILE)

add_custom_target(
    dash ALL

    COMMAND
        devenv ${VFPRO_FILE} /Build $<CONFIG>

    WORKING_DIRECTORY
        ${DASH_BUILD_DIR}
)

add_dependencies(
    dash

    # Hard coded dependencies: TODO Somehow generate this from within build tool
    guilib_qt
    licencelib_qt
)
