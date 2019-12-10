'''
Para gerar o sib2pymod execute:

$ f2py3.7 --fcompiler=gnu95 --f90flags='-fno-automatic -finit-local-zero' -c *.f95 -m sib2pymod
'''
import numpy as np
from sib2pymod import sib2


#mudanca de parametros
tran = np.empty([2, 2], dtype=np.float16, order='F')
# ############################################### trans   refl    trans   refl
tran[0, 0], tran[1, 0], tran[0, 1], tran[1, 1] = 0.0170, 0.2000, 0.0010, 0.0010

trans_viva = 0.0170  # 0.0170
trans_seca = 0.2000
ref_viva = 0.0010
ref_seca = 0.0010

nlinha = 78888 #79000  # 78889

# ts_rn_c = 0.
# ts_rn_c = np.empty([nlinha], dtype=np.float64, order='F')
# ts_rn_c[:] = -999.0

rn_c = sib2(trans_viva, trans_seca, ref_viva, ref_seca,
            nlinha)

# print(ts_rn_c)
