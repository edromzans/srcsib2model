'''
Para gerar o sib2pymod execute:

$ f2py3.7 --fcompiler=gnu95 --f90flags='-fno-automatic -finit-local-zero' -c *.f95 -m sib2pymod
'''
from sib2pymod import sib2

sib2(9999.9)
