if test -f "lib/printLib.c"
    then
        cd lib
        gcc -c printLib.c
        cd ..
        gcc lib/printLib.o koak.o
        ./a.out
fi