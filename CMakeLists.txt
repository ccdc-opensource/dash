# Minimum required content of a CMakeLists.txt file
cmake_minimum_required(VERSION 3.19...3.21)

project(DASH
  VERSION
      4.0.0
  DESCRIPTION
    "DASH powder diffraction solution"
  LANGUAGES
    Fortran C
)

include(FetchContent)
include(GNUInstallDirs)

# Set up variables that will be replaced in source files
set(DASH_VERSION ${CMAKE_PROJECT_VERSION})
set(DASH_COPYRIGHT_YEAR "2021")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

# Configure CPack based package generation
set(CPACK_PACKAGE_NAME "DASH")
set(CPACK_PACKAGE_VENDOR "The Cambridge Crystallographic Data Centre")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/description.txt")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A versatile and interactive package for solving crystal structures from powder diffraction data")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.org/ccdc-opensource/dash")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
if(WIN32)
  set(PACKAGE_PLATFORM "win32")
elseif(UNIX AND NOT APPLE)
  set(PACKAGE_PLATFORM "linux")
endif()

# Do not name the package DASH if you value your debian or ubuntu installation
set(CPACK_DEBIAN_PACKAGE_NAME "dash-powder-diffraction")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "zlib1g, libpng16-16, libx11-6, libxext6, libxm4, libxt6, libice6, libsm6, libxmu6, libxft2, libxrender1, libfreetype6, libfontconfig1, libxinerama1")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "admin@ccdc.cam.ac.uk")

if($ENV{BUILD_NUMBER})
    set(CPACK_PACKAGE_FILE_NAME ${CPACK_PACKAGE_NAME}-${DASH_VERSION}-$ENV{BUILD_NUMBER}-${PACKAGE_PLATFORM})
    set(CPACK_SOURCE_PACKAGE_FILE_NAME ${CPACK_PACKAGE_NAME}-${DASH_VERSION}-$ENV{BUILD_NUMBER})
    set(CPACK_DEBIAN_FILE_NAME ${CPACK_DEBIAN_PACKAGE_NAME}-${DASH_VERSION}-$ENV{BUILD_NUMBER}-${PACKAGE_PLATFORM}.deb)
else()
    set(CPACK_PACKAGE_FILE_NAME ${CPACK_PACKAGE_NAME}-${DASH_VERSION}-developer-build-${PACKAGE_PLATFORM})
    set(CPACK_SOURCE_PACKAGE_FILE_NAME ${CPACK_PACKAGE_NAME}-${DASH_VERSION}-developer-build)
    set(CPACK_DEBIAN_FILE_NAME ${CPACK_DEBIAN_PACKAGE_NAME}-${DASH_VERSION}-developer-build-${PACKAGE_PLATFORM}.deb)
endif()
# set(CPACK_PACKAGE_INSTALL_DIRECTORY "")
set(CPACK_PACKAGE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/distribution/share/icons/dash/dash.ico)
set(CPACK_WIX_PRODUCT_ICON ${CMAKE_CURRENT_SOURCE_DIR}/distribution/share/icons/dash/dash.ico)
set(CPACK_WIX_PROGRAM_MENU_FOLDER .)
set(CPACK_PACKAGE_CHECKSUM SHA256)
set(CPACK_MONOLITHIC_INSTALL TRUE)
set(CPACK_PACKAGE_EXECUTABLES dash;DASH)
set(CPACK_VERBATIM_VARIABLES TRUE)
if(WIN32)
  set(CPACK_WIX_UPGRADE_GUID "f45138d2-73a1-4f1b-9099-85ecadd4f86e")
  set(CPACK_GENERATOR "7Z;WIX")
else()
  set(CPACK_GENERATOR "7Z;DEB")
endif()

include(CPack)

# Transform variables in .in files into their values (output goes to the build directory)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/src/PCDash_Main.f90.in" "${CMAKE_CURRENT_BINARY_DIR}/src/PCDash_Main.f90" @ONLY)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/src/variables.f90.in" "${CMAKE_CURRENT_BINARY_DIR}/src/variables.f90" @ONLY)

# Select the right library directory based on the compiler
set(DASH_RESOURCE ${CMAKE_CURRENT_SOURCE_DIR}/src/dash_gui_resources.rc)
if(WIN32)
  set(WINTERACTER_ROOT "c:/wint" CACHE STRING "Location where winteracter is installed")
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    if (${CMAKE_Fortran_COMPILER_ARCHITECTURE_ID} STREQUAL "x64")
      set(winteracter_LIBRARY_PATH ${WINTERACTER_ROOT}/lib.i64)
    else()
      set(winteracter_LIBRARY_PATH ${WINTERACTER_ROOT}/lib.if8)
    endif()
  endif()
endif()
if(UNIX AND NOT APPLE)
  find_package(X11 REQUIRED)
  find_package(Motif REQUIRED)
  find_package(Fontconfig REQUIRED)
  set(WINTERACTER_ROOT "/opt/winteracter" CACHE STRING "Location where winteracter is installed")
  set(CMAKE_RC_COMPILER "${WINTERACTER_ROOT}/bin/rc")
  set(CMAKE_RC_COMPILE_OBJECT "<CMAKE_RC_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
  set(CMAKE_RC_FLAGS "-cif8 -sm64")
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    set(winteracter_LIBRARY_PATH ${WINTERACTER_ROOT}/lib.i64)
  endif()
endif()

enable_language(RC)

# Create the DASH executable target
# Do note that dash is the standard shell on debian/ubuntu so case is important
add_executable(DASH WIN32)

target_include_directories(
  DASH
  PRIVATE
  ${WINTERACTER_ROOT}/include
  ${winteracter_LIBRARY_PATH}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${CMAKE_CURRENT_SOURCE_DIR}
)
target_link_directories(DASH PRIVATE ${winteracter_LIBRARY_PATH})

if(WIN32)
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
# DASH was built with /QxW, which has been superseded by /arch switches
target_compile_options(DASH PRIVATE "/arch:SSE2")
  endif()
endif()

if(WIN32)
target_link_libraries(
  DASH PRIVATE 
  winter.lib # Winteracter main library
)
else()
  target_link_libraries(
    DASH PRIVATE 
    wint # Winteracter main library
    ${MOTIF_LIBRARIES}
    X11::Xmu
    X11::Xft
    X11::Xinerama
    Fontconfig::Fontconfig
  )
endif()

target_sources(
  DASH PRIVATE
  # autogenerated files
  "${CMAKE_CURRENT_BINARY_DIR}/src/PCDash_Main.f90"
  "${CMAKE_CURRENT_BINARY_DIR}/src/variables.f90"
  # Main fortran files
  src/Align.f90
  src/ATMVAR.f90
  src/Basic_Date.f90
  src/Basic_Files.f90
  src/Basic_IO.f90
  src/Basic_Strings.f90
  src/Basic_Vectors.f90
  src/BatchMode.f90
  src/Ccslmain.f90
  src/Chi_sq_plot.F90
  src/create_fob.f90
  src/Declarations.f90
  src/Dialog_Routines.f90
  src/DICVAR.f90
  src/DICVO2.f90
  src/DICVO3.f90
  src/DICVO4.f90
  src/DICVO5.f90
  src/DICVO6.f90
  src/DICVO7.f90
  src/DICVOL91.f90
  src/error_message.f90
  src/Eval.f90
  src/External_RR.f90
  src/Fcn.f90
  src/Ffcalc.f90
  src/Fortic.f90
  src/forty.f90
  src/Fou.f90
  src/Frac2cart.f90
  src/FWHM.f90
  src/Generate_TicMarks.f90
  src/get_logref.f90
  src/GSAS.f90
  src/Init_Routines.f90
  src/Initialisation.f90
  src/Interface.f90
  src/LoadRAWFiles.f90
  src/LoadSDI.f90
  src/Mag.f90
  src/Main_Field_Changed_Routines.f90
  src/MCBack.f90
  src/MDB.f90
  src/Mogul.f90
  src/MONKEY1.F90
  src/MultiModal.f90
  src/multipeak_chisq.f90
  src/multipeak_fitter.f90
  src/MultiRun.f90
  src/Mvec.f90
  src/pawley_error_check.f90
  src/Pawley.f90
  src/PF_Simplex.f90
  src/Pf.f90
  src/plot_test.F90
  src/PO_VAR.f90
  src/PolyFitter_Subs.f90
  src/PolyLoadFiles.f90
  src/Pr.f90
  src/PRJVAR.f90
  src/Profile_Plot.f90
  src/ProjectSave.f90
  src/read_one_zm.f90
  src/REFVAR.f90
  src/Res2Mol2.f90
  src/RIETAN.f90
  src/Rietveld.f90
  src/RR_simopt.f90
  src/RRVAR.f90
  src/SA_Begin.f90
  src/SA_Defaults.f90
  src/SA_Dialogues.f90
  src/SA_main.f90
  src/SA_move_status.f90
  src/SA_output.f90
  src/SA_refresh.f90
  src/SA_restrain.f90
  src/SA_simopt.f90
  src/SA_soln_store.f90
  src/SA_structure_output.f90
  src/SA_subs.f90
  src/SA_summary.f90
  src/samabo.f90
  src/SAMVAR.f90
  src/SGDecode.f90
  src/SingleCrystal.f90
  src/SOLVAR.f90
  src/SpaceGroupDetermination.f90
  src/TAVAR.f90
  src/TOPAS.f90
  src/Tutorials.f90
  src/Upload_Cell_Constants.f90
  src/Upload_Widths.f90
  src/UserInputChecks.f90
  src/Utilities.f90
  src/Valchi.f90
  src/valchipro.f90
  src/Vicar.f90
  src/Wifd99.f90
  src/Wizard_routines.f90
  src/XtalFile.f90
  src/ZMVAR.f90

  # includes
  src/GLBVAR.INC
  src/Lattice.inc
  src/params.inc
  src/Poly_Colours.inc
  src/Reflns.inc
  src/SA_restrain.inc
  src/statlog.inc

  SGinc/ffcalctop.inc

  # Resources
  # this file is generated from src/dash_gui_resources.rc but winteracter provide no way to generate it automatically
  src/dash_gui_resources.f90
  src/dash_gui_resources.rc
  # ${DASH_COMPILED_RESOURCE}
)
# set_source_files_properties(
#   ${DASH_COMPILED_RESOURCE}
#   PROPERTIES
#   EXTERNAL_OBJECT true
#   GENERATED true
# )
# add_custom_command(
#   OUTPUT ${DASH_COMPILED_RESOURCE}
#   COMMAND ${WINTERACTER_RC} ${WINTERACTER_RC_ARGS} ${DASH_RESOURCE}
#   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
#   DEPENDS ${WINTERACTER_RC}
#   MAIN_DEPENDENCY ${DASH_RESOURCE}
#   VERBATIM
#   COMMAND_EXPAND_LISTS
#   COMMENT "Compiling resource ${DASH_RESOURCE}"
# )

# Installation section
# Everything here is installed relative to the value of CMAKE_INSTALL_PREFIX (this is CMake's default behaviour)
install(
    TARGETS  DASH
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Fetch current version of dash_requirements distribution from github
set(DASH_REQUIREMENTS_BRANCH "2021.3")
set(DASH_REQUIREMENTS_GITHUB_RELEASE "dash-requirements%2F2021.3")
if(WIN32)
set(DASH_REQUIREMENTS_PLATFORM "windows-dynamic-64")
set(DASH_REQUIREMENTS_BUILD "332413")
set(DASH_REQUIREMENTS_SHA256 "8752cdaca0cd549b2e9fbfa0931b5c98146adf4b6b2ff560f1a350fa5092bbcc")
set(DASH_REQUIREMENTS_EXTENSION "7z")
else()
# set(DASH_REQUIREMENTS_GITHUB_RELEASE "4.0.0")
set(DASH_REQUIREMENTS_BRANCH "main")
set(DASH_REQUIREMENTS_PLATFORM "linux-64")
set(DASH_REQUIREMENTS_BUILD "335532")
set(DASH_REQUIREMENTS_SHA256 "e9a25a948bad6794c30b5ab6a0312b10e7dec929b7c3b83823825561b01c2d49")
set(DASH_REQUIREMENTS_EXTENSION "tar.bz2")
endif()
FetchContent_Declare(
  dash_requirements
  URL      "https://github.com/ccdc-opensource/dash/releases/download/${DASH_REQUIREMENTS_GITHUB_RELEASE}/dash_requirements-${DASH_REQUIREMENTS_BUILD}-${DASH_REQUIREMENTS_BRANCH}-${DASH_REQUIREMENTS_PLATFORM}.${DASH_REQUIREMENTS_EXTENSION}"
  URL_HASH "SHA256=${DASH_REQUIREMENTS_SHA256}"
)
FetchContent_MakeAvailable(dash_requirements)

install(
    DIRECTORY   "${dash_requirements_SOURCE_DIR}/"
    DESTINATION ${CMAKE_INSTALL_BINDIR}/zmconv
    USE_SOURCE_PERMISSIONS
)

if(WIN32)
  FetchContent_Declare(
    extinction_symbol_exe
    URL      "https://github.com/ccdc-opensource/dash/releases/download/extinction_symbol_2009/ExtinctionSymbol.exe"
    URL_HASH "SHA256=399353ca13a35f40c898973a1e19ebe7ce12642f2e672a485c731d0f42c6bb16"
    DOWNLOAD_NO_EXTRACT YES
  )
  FetchContent_MakeAvailable(extinction_symbol_exe)

  install(
      DIRECTORY   "${extinction_symbol_exe_SOURCE_DIR}/"
      DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()

if(UNIX AND NOT APPLE)
  FetchContent_Declare(
    extinction_symbol
    URL      "https://www.markvardsen.net/projects/download/ExtSym2008Linux32bit.tar.gz"
    URL_HASH "SHA256=4eec93c1dd9f1aa94bee5eec8cdecfc19ece3acca60dda75d5bc197ed94b6239"
  )
  FetchContent_MakeAvailable(extinction_symbol)

  install(
      PROGRAMS "${extinction_symbol_SOURCE_DIR}/ExtSym"
      DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()

install(
    FILES       distribution/bin/RIETAN.cmd
    DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(
    DIRECTORY   expcli
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    COMPONENT
)

install(
    DIRECTORY   distribution/share/doc/
    DESTINATION ${CMAKE_INSTALL_DOCDIR}
)

install(
    DIRECTORY  distribution/share/icons
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}
)

install(
    DIRECTORY  distribution/share/dash
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}
)

# Install required intel fortran DLLs.
# InstallRequiredSystemLibraries currently has difficulties with our legacy fortran compiler
if(WIN32)
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    if (${CMAKE_Fortran_COMPILER_ARCHITECTURE_ID} STREQUAL "x64")
      set(INTEL_FORTRAN_COMMON_FILES "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/windows/redist/intel64_win/compiler")
    else()
      set(INTEL_FORTRAN_COMMON_FILES "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/windows/redist/ia32_win/compiler")
    endif()
    install(
      FILES
      "${INTEL_FORTRAN_COMMON_FILES}/libifcoremd.dll"
      "${INTEL_FORTRAN_COMMON_FILES}/libmmd.dll"
      "${INTEL_FORTRAN_COMMON_FILES}/svml_dispmd.dll"
      DESTINATION ${CMAKE_INSTALL_BINDIR}
      COMPONENT
    )
    endif()
endif()

