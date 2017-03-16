
ccdc_install_docs_dirs(
    ${basedir}
    DIRS        CSDS/dash/dash_files/
    DESTINATION Documentation/dash
    COMPONENT   ${component}
)

set(docs "${basedir}/Documentation")

install(
    FILES       ${CMAKE_BINARY_DIR}/bin/Release/DASH.exe
    DESTINATION ${basedir}
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_1.xye Tutorial_1.mol2
    DESTINATION "${docs}/Tutorial1/Data files"
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_2.xye Tutorial_2.mol2
    DESTINATION "${docs}/Tutorial2/Data files"
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_3.xye Tutorial_3-trans.mol2 Tutorial_3-cis.mol2
                Tutorial_3-ModelA.zmatrix Tutorial_3-ModelB.zmatrix
                Tutorial_3-ModelC.zmatrix
    DESTINATION "${docs}/Tutorial3/Data files"
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_4.xye Tutorial_4-half.mol2 Tutorial_4-full.mol2
    DESTINATION "${docs}/Tutorial4/Data files"
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_5.xye Tutorial_5.mol2 Tutorial_5-2.mol2
    DESTINATION "${docs}/Tutorial5/Data files"
    COMPONENT   ${component}
)

install(
    FILES       Tutorial_6.raw Tutorial_6-atoms.mol2 Tutorial_6-frags.mol2
    DESTINATION "${docs}/Tutorial6/Data files"
    COMPONENT   ${component}
)

install(
    FILES       "Extinction Symbol Documents/Advanced.asc"
                "Extinction Symbol Documents/Dopmmm.hkl"
                "Extinction Symbol Documents/ESMANUAL for DASH.DOC"
                "Extinction Symbol Documents/parameter_input.asc"
    DESTINATION "${docs}/Extinction Symbol Documents"
    COMPONENT   ${component}
)

install(
    FILES       "Unsupported Extras/MDash.exe"
                "Unsupported Extras/MDASH.pdf"
                "Unsupported Extras/ReadMe.txt"
    DESTINATION "${basedir}/Unsupported Extras"
    COMPONENT   ${component}
)

install(
    FILES       ExtinctionSymbol.exe SpaceGroupSymbols.dat TOPAS.inc
                RIETAN.cmd RIETAN2000.tem RIETANFP.tem
    DESTINATION ${basedir}
    COMPONENT   ${component}
)

install(
    DIRECTORY   ${MAIN_SOURCE_DIR}/cppbuilds_shared/icons/dash/
    DESTINATION ${basedir}/icons
    COMPONENT   ${component}
)

install(
    DIRECTORY   expcli
    DESTINATION ${basedir}
    COMPONENT   ${component}
)
