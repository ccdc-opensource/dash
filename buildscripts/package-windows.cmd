rmdir /Q /S dash
rmdir /Q /S bld
git clone ssh://git@github.com/ccdc-opensource/dash.git
cd dash
git checkout xmas_updates
cd ..

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

"C:\Program Files\CMake\bin\cmake.exe" ^
    -G Ninja ^
    -S dash ^
    -B bld ^
    -DWINTERACTER_ROOT=C:\winteracter\winteracter-14.10d ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/windows/bin/intel64/ifort.exe" ^
    -DCMAKE_INSTALL_PREFIX=install

"C:\Program Files\CMake\bin\cmake.exe" --build bld --target package
