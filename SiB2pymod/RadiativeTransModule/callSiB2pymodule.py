'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
#from sib2pymod import sib2
#from importlib import reload
import sib2pymod

trans_viva_nir = 0.2
ref_viva_nir = 0.5
ref_solo_par = 0.11
ref_solo_nir = 0.225
# nlinha = 78890
nlinha = 744

# primeira rodada
Rn_C = sib2pymod.sib2(trans_viva_nir, ref_viva_nir,
                      ref_solo_par, ref_solo_nir, nlinha)

rn = Rn_C
print(rn)

# Seleciona os valores validos
posval = np.asarray(Rn_C > -99999.).nonzero()
posval = posval[0]
Rn_C = Rn_C[posval]

print(Rn_C[0:20])

rn1 = Rn_C

# segunda rodada
# sib2pymod = reload(sib2pymod)
Rn_C = sib2pymod.sib2(trans_viva_nir, ref_viva_nir,
                      ref_solo_par, ref_solo_nir, nlinha)

posval = np.asarray(Rn_C > -99999.).nonzero()
posval = posval[0]
Rn_C = Rn_C[posval]

print(Rn_C[0:20])
rn2 = Rn_C

'''Verificando efeito da heranca de valores da
rodada anterior'''

# diferencas entre a rodade 1 e 2
plt.subplot(211)
plt.scatter(rn1, rn2, c='black', marker='+', linewidth=0.5)
plt.ylabel('Rn1')
plt.xlabel('Rn2')
plt.subplot(212)
plt.plot((rn1-rn2), c='black', linewidth=0.5)
plt.ylabel('Rn1 - Rn2')
# plt.plot(np.abs(rn1-rn2))
plt.show()
