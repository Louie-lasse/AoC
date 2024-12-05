import re

def lowest_index(s, i):
    ints = [s.find("mul(",i), s.find("do()",i), s.find("don't()",i)]
    ints = [v for v in ints if v >= 0]
    return min(ints) if ints else -1

def read(s: str):
    i = 0
    enabled = True
    while i < len(s):
        i = lowest_index(s,i)
        if i == -1:
            break

        if s[i:].startswith("do()"):
            enabled = True
            i += 1
            continue
        if s[i:].startswith("don't()"):
            enabled = False
            i += 1
            continue
        
        if not enabled:
            i += 1
            continue

        i += 4
        
        m = re.match(r"(\d+),(\d+)\)",s[i:])
        if not m:
            continue
        i1,i2 = m.group(1), m.group(2)
        yield int(i1), int(i2)
    return
        

with open("input.txt", "r") as f:
    lines = f.read()
    vals = [i1*i2
        for i1,i2 in read(lines)
    ]
    print(sum(vals))
    
