import re

def read(s: str):
    i = 0
    while i < len(s):
        i = s.find("mul(",i)
        if i == -1:
            break
        i += 4
        m = re.match(r"(\d+),(\d+)\)",s[i:])
        if not m:
            continue
        i1,i2 = m.group(1), m.group(2)
        yield int(i1), int(i2)
    return
        

with open("input.txt", "r") as f:
    lines = f.read().splitlines()
    vals = [i1*i2
        for line in lines
        for i1,i2 in read(line)
    ]
    print(sum(vals))
    
