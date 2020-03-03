'''
Para gerar o sib2pymod execute:
$ f2py3.7 --fcompiler=gnu95 -c *.f95 -m sib2pymod
'''
import numpy as np
import matplotlib.pyplot as plt
# from sib2pymod import sib2
# from importlib import reload
import sib2pymod

gradm_param = 16.0
gmudmu_param = 1.0
greeness_param = 0.99
vmax_param = 105.0
nlinha = 8784

# primeira rodada

# [hc, le] = sib2pymod.sib2(gradm_param, gmudmu_param,
#                            greeness_param, vmax_param,
#                            nlinha)

[run1_hc, run1_lec] = sib2pymod.sib2(gradm_param, gmudmu_param,
                                     greeness_param, vmax_param,
                                     nlinha)

print('Hc----------------')
print(run1_hc[0:20])

print('LEc---------------')
print(run1_lec[0:20])


# segunda rodada

[run2_hc, run2_lec] = sib2pymod.sib2(gradm_param, gmudmu_param,
                                     greeness_param, vmax_param,
                                     nlinha)


# diferencas entre a rodade 1 e 2
plt.subplot(221)
plt.scatter(run1_hc, run2_hc, c='black', marker='+', linewidth=0.5)
plt.ylabel('1 (Hc)')
plt.xlabel('2')
plt.subplot(222)
plt.plot((run1_hc-run2_hc), c='black', linewidth=0.5)
plt.ylabel('(Hc) run1 - run2')

plt.subplot(223)
plt.scatter(run1_lec, run2_lec, c='black', marker='+', linewidth=0.5)
plt.ylabel('1 (LEc)')
plt.xlabel('2')
plt.subplot(224)
plt.plot((run1_lec - run2_lec), c='black', linewidth=0.5)
plt.ylabel('(LEc) run1 - run2')

plt.show()
