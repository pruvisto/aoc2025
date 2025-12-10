import sys
import numpy as np
from cylp.cy import CyClpSimplex
from cylp.py.modeling.CyLPModel import CyLPModel, CyLPArray

def parse_machine(s):
    s = s.split(' ')
    lights = [c == '#' for i, c in enumerate(s[0].strip()[1:-1])]
    buttons = [[int(u) for u in t.strip()[1:-1].split(',')] for t in s[1:-1]]
    jolts = [int(u) for u in s[-1].strip()[1:-1].split(',')]
    return (lights, buttons, jolts)

def solve_machine(m):
    (lights, buttons, jolts) = m
    (n_vars, n_eqs) = (len(buttons), len(jolts))
    m = CyLPModel()
    x = m.addVariable('x', n_vars, isInt = True)

    m.objective = x.sum()
    m += x >= 0
    
    A = np.zeros((n_eqs, n_vars), dtype=np.int64)
    for i, b in enumerate(buttons):
        for j in b:
            A[j][i] = 1
    m += (CyLPArray(A) * x == CyLPArray(jolts))
    
    s = CyClpSimplex(m)
    cbcModel = s.getCbcModel()
    cbcModel.logLevel = 0
    cbcModel.solve()
    return int(cbcModel.objectiveValue)

def main():
    res = sum([solve_machine(parse_machine(l)) for l in sys.stdin])
    print(res)

if __name__ == "__main__":
    main()
    
