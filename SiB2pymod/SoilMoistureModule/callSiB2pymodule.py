'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
# from sib2pymod import sib2
# from importlib import reload
import sib2pymod

bee1_param = 7.12
phsat1_param = -0.2
satco1_param = 5e-06
poros1_param = 0.515
nlinha = 744

# primeira rodada
www1 = sib2pymod.sib2(bee1_param, phsat1_param, satco1_param, poros1_param,
                      nlinha)

# # Seleciona os valores validos
# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

print(www1[0:20])

run1 = www1

# segunda rodada
# sib2pymod = reload(sib2pymod)

www1 = sib2pymod.sib2(bee1_param, phsat1_param, satco1_param, poros1_param,
                      nlinha)

# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

print(www1[0:20])
run2 = www1

'''Verificando efeito da heranca de valores da
rodada anterior'''

# diferencas entre a rodade 1 e 2
plt.subplot(211)
plt.scatter(run1, run2, c='black', marker='+', linewidth=0.5)
plt.ylabel('1')
plt.xlabel('2')
plt.subplot(212)
plt.plot((run1-run2), c='black', linewidth=0.5)
plt.ylabel('run1 - run2')
# plt.plot(np.abs(rn1-rn2))
plt.show()
