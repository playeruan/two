
./build.sh
echo "> compiling source.two"
command time --format "%E (exit status %x)" ./two
echo "> done"
echo "> running executable"
command time --format "%E (exit status %x)" ./examples/out
echo "> done"
