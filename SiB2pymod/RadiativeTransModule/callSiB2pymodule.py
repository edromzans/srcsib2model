'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
#from sib2pymod import sib2
#from importlib import reload
import sib2pymod

tran_1_1 = 0.0170
tran_2_1 = 0.2000
tran_1_2 = 0.0010
tran_2_2 = 0.0010
ref_1_1 = 0.0700
ref_2_1 = 0.5000
ref_1_2 = 0.1600
ref_2_2 = 0.3900
soref_1 = 0.11
soref_2 = 0.225
chil_param = 0.1

# nlinha = 78890
nlinha = 744

# primeira rodada
Rn_C = sib2pymod.sib2(tran_1_1, tran_2_1, tran_1_2, tran_2_2,
                      ref_1_1, ref_2_1, ref_1_2, ref_2_2, soref_1, soref_2,
                      chil_param, nlinha)

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
Rn_C = sib2pymod.sib2(tran_1_1, tran_2_1, tran_1_2, tran_2_2,
                      ref_1_1, ref_2_1, ref_1_2, ref_2_2, soref_1, soref_2,
                      chil_param, nlinha)

posval = np.asarray(Rn_C > -99999.).nonzero()
posval = posval[0]
Rn_C = Rn_C[posval]

print(Rn_C[0:20])
rn2 = Rn_C

'''Verificando efeito da heranca de valores da
rodada anterior'''

# diferencas entre a rodade 1 e 2
plt.subplot(311)
plt.plot(rn1, c='black', linewidth=0.5, label='1')
plt.plot(rn2, c='red', linewidth=0.5, label='2')
plt.ylabel('Rn')
plt.legend()

plt.subplot(312)
plt.scatter(rn1, rn2, c='black', marker='+', linewidth=0.5)
plt.ylabel('Rn1')
plt.xlabel('Rn2')

plt.subplot(313)
plt.plot((rn1-rn2), c='black', linewidth=0.5)
plt.ylabel('Rn1 - Rn2')
# plt.plot(np.abs(rn1-rn2))

plt.tight_layout()
plt.savefig('Heranca_radiacao.png', dpi=300, bbox_inches='tight')

plt.show()
