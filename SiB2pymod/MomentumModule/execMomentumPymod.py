'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
# from sib2pymod import sib2
# from importlib import reload
import sib2pymod

z0d_param = 1.571
dd_param = 26.606
cc1_param = 8.12
cc2_param = 727.80
nlinha = 8784

[ustar_c, zlt_ts] = sib2pymod.sib2(z0d_param, dd_param,
                                   cc1_param, cc2_param, nlinha)
print(ustar_c, zlt_ts)
