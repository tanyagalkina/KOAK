#include "stdio.h"

void printDouble(double number)
{
   printf("%g\n", number);
}

void printInt(int number)
{
    printf("%d\n", number);
}

void printBool(int bool)
{
    printf("%s\n",  bool ? "true" : "false");
}