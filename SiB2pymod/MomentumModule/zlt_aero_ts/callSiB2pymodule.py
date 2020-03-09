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

# primeira rodada
ustar_c = sib2pymod.sib2(z0d_param, dd_param, cc1_param, cc2_param, nlinha)

# # Seleciona os valores validos
# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

# print(ustar_c[0:20])

# run1 = ustar_c

# # segunda rodada
# # sib2pymod = reload(sib2pymod)
# ustar_c = sib2pymod.sib2(z0d_param, dd_param, cc1_param, cc2_param, nlinha)

# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

# print(ustar_c[0:20])
# run2 = ustar_c

# '''Verificando efeito da heranca de valores da
# rodada anterior'''

# # diferencas entre a rodade 1 e 2
# plt.subplot(211)
# plt.scatter(run1, run2, c='black', marker='+', linewidth=0.5)
# plt.ylabel('1')
# plt.xlabel('2')
# plt.subplot(212)
# plt.plot((run1-run2), c='black', linewidth=0.5)
# plt.ylabel('run1 - run2')
# # plt.plot(np.abs(rn1-rn2))
# plt.show()
