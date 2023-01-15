nasm -felf64 test.asm -o test.o
ld test.o -o test -lc -e zip_main -dynamic-linker /lib64/ld-linux-x86-64.so.2
rm test.o
chmod +x ./test
./test
RES=$?
if [ -z "$1" ]; then
    rm ./test
fi
echo "Result: $RES"
