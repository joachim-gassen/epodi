"""
Code base:

https://python-advanced.quantecon.org/discrete_dp.html


Model and calibration:

Breuer and Windisch
Investment Dynamics and Earnings-Return Properties:A Structural Approach
Journal of Accounting Research 57(3) (2019): 639 - 674
https://onlinelibrary.wiley.com/doi/epdf/10.1111/1475-679X.12253
"""

import numpy as np
import quantecon as qe
import pandas as pd
from scipy.stats import norm
import scipy.sparse as sparse
from quantecon.markov import DiscreteDP
import progressbar

model = "BW"

if (model == "BW"):
    beta=1/(1.1)
    delta = 0.15
    psi_factor = 1
    rho = 0.7
    theta = 0.7
    z_upper_bar = 1.5
    sigma_z = 0.5 
    k_grid = "linear"
    k_size = 600
    z_size = 100
elif (model == "BWlog"): 
    beta=1/(1.1)
    delta = 0.15
    psi_factor = 1
    rho = 0.7
    theta = 0.7
    z_upper_bar = 1.5
    sigma_z = 0.5 
    k_grid = "log"
    k_size = 600
    z_size = 100
else:
    raise ValueError("Model not recognized")
    
pi = lambda k,z: z*k**theta
psi = lambda k,I: psi_factor*I**2/(2*k)
cf = lambda k,z,kprime: pi(k, z) - delta*k - psi(k, kprime - k) - (kprime - k)  

def norm_grid_probs(a, mn, sd) :
    mpoints = (a[:-1] + a[1:])/2
    a = norm(mn, sd).cdf(mpoints)
    return np.concatenate([a, [1.]]) - np.concatenate([[0],a]) 

if (k_grid == "linear"):
    grid_k = np.linspace(1, k_size, k_size)

if (k_grid == "log"):
    grid_k = np.logspace(0, np.log10(500), num = k_size)

if (k_grid == "depr"):
    grid_k = np.empty(k_size)
    for s in range(k_size):
        if (s==0):
            grid_k[0] = 1
        else:
            grid_k[s] = grid_k[s-1]/(1-delta)

grid_z = np.linspace(z_upper_bar - 4*sigma_z, z_upper_bar + 4*sigma_z, z_size)

n = len(grid_k) * len(grid_z)
m = len(grid_k) 

s_indices = np.repeat(range(n), m)
a_indices = np.tile(range(m), n)
pos_k = (s_indices/len(grid_z)).astype(int)
pos_z = s_indices - pos_k*len(grid_z)
R = cf(grid_k[pos_k], grid_z[pos_z], grid_k[a_indices])
mn_new_z = (1 - rho)*z_upper_bar + rho*grid_z[pos_z]

prob_zprime = np.empty((len(grid_z), len(grid_z)))
for z in range(len(grid_z)):
    prob_zprime[z] = norm_grid_probs(grid_z, (1 - rho)*z_upper_bar + rho*grid_z[z], sigma_z)    

Q = sparse.lil_matrix((n*m, n))

bar = progressbar.ProgressBar()
for s in bar(range(n)):
    for a in range(m):
        Q[s*m + a, (a*len(grid_z)):((a + 1)*len(grid_z))] = prob_zprime[pos_z[s*m + a]]
   
Q = Q.tocsr()

ddp = DiscreteDP(R, Q, beta, s_indices, a_indices)

results = ddp.solve(method='policy_iteration')

df = pd.DataFrame({
        'z': np.tile(grid_z, len(grid_k)),
        'k': np.repeat(grid_k, len(grid_z)),
        'kprime': grid_k[results.sigma],
        'v': results.v
    })

fpath = "data/grids/grid_%dk_%dz_%dkmax_%s.csv" % (len(grid_k), len(grid_z), max(grid_k), model)
df.to_csv(fpath, index = False)
