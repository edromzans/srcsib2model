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

bee26_param = 7.12
phsat26_param = -0.2
satco26_param = 5e-06

poros2_param = 0.530
poros3_param = 0.530
poros4_param = 0.530
poros5_param = 0.530
poros6_param = 0.530

nlinha = 26304

# primeira rodada
[www1, www2, www3, www4, www5, www6, www7, www8, www9, www10] = sib2pymod.sib2(
    bee1_param, phsat1_param, satco1_param, poros1_param,
    bee26_param, phsat26_param, satco26_param,
    poros2_param, poros3_param, poros4_param, poros5_param, poros6_param,
    nlinha)

# # Seleciona os valores validos
# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

print(www1[0:20])

run1 = www1

# segunda rodada
# sib2pymod = reload(sib2pymod)

[www1, www2, www3, www4, www5, www6, www7, www8, www9, www10] = sib2pymod.sib2(
    bee1_param, phsat1_param, satco1_param, poros1_param,
    bee26_param, phsat26_param, satco26_param,
    poros2_param, poros3_param, poros4_param, poros5_param, poros6_param,
    nlinha)



# posval = np.asarray(ustar_c > -99999.).nonzero()
# posval = posval[0]
# ustar_c = ustar_c[posval]

print(www1[0:20])
run2 = www1

'''Verificando efeito da heranca de valores da
rodada anterior'''

# diferencas entre a rodade 1 e 2
plt.subplot(311)
plt.plot(run1, c='black', linewidth=0.5, label='1')
plt.plot(run2, c='red', linewidth=0.5, label='2')
plt.ylabel('www')
plt.legend()

plt.subplot(312)
plt.scatter(run1, run2, c='black', marker='+', linewidth=0.5)
plt.ylabel('1')
plt.xlabel('2')

plt.subplot(313)
plt.plot((run1-run2), c='black', linewidth=0.5)
plt.ylabel('run1 - run2')
# plt.plot(np.abs(run1-run2))

plt.tight_layout()
plt.savefig('Heranca_umidadesolo.png', dpi=300, bbox_inches='tight')

plt.show()
