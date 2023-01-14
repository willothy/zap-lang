nasm -felf64 test.asm -o test.o
ld test.o -o test
rm test.o
chmod +x ./test
./test
RES=$?
rm ./test
echo "Result: $RES"
