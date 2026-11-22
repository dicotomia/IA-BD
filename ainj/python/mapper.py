#!/usr/bin/env python3

import sys
import csv

reader = csv.reader(sys.stdin)

for row in reader:

if not row:
continue

if row[0] == "id":
continue

provincia = row[2]

print(f"{provincia}\t1")
