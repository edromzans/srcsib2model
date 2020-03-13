'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
# from sib2pymod import sib2
# from importlib import reload
import sib2pymod

ha_param = 23.63
z0d_param = 1.571
dd_param = 26.606
g2_param = 0.787
g3_param = 0.787
cc1_param = 8.12
cc2_param = 727.80
corb1_param = 7.89
corb2_param = 387.49

nlinha = 8784

# executa sib2 
[ustar_c, zlt_ts] = sib2pymod.sib2(ha_param, z0d_param, dd_param, g2_param,
                                   g3_param, cc1_param, cc2_param, corb1_param,
                                   corb2_param,
                                   nlinha)

print(ustar_c, zlt_ts)
