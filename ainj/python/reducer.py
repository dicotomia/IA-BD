#!/usr/bin/env python3

import sys

provincia_actual = None
total = 0

for linea in sys.stdin:

provincia, cantidad = linea.strip().split("\t")

cantidad = int(cantidad)

if provincia == provincia_actual:

total += cantidad

else:

if provincia_actual is not None:
print(f"{provincia_actual}\t{total}")

provincia_actual = provincia
total = cantidad

if provincia_actual is not None:
print(f"{provincia_actual}\t{total}")
