'''
Para gerar o sib2pymod execute:

$ f2py3.7 --fcompiler=gnu95 --f90flags='-fno-automatic -finit-local-zero' -c *.f95 -m sib2pymod
'''
# import numpy as np
from sib2pymod import sib2

# trans_viva = 0.0170  # 0.0170
# trans_seca = 0.2000
# ref_viva = 0.0010
# ref_seca = 0.0010

nlinha = 744  # 79000  # 78889
z0d_opt = 2.
cc1_opt = 9.

ustar_c = sib2(z0d_opt, cc1_opt, nlinha)

# print(ts_rn_c)
