'''
Created on 2018年1月7日

@author: Allen
'''

import powerlaw
import pandas as pd
import pyodbc
import networkx as nx
from networkx.algorithms.approximation.clustering_coefficient import average_clustering

def build(network, mfile):
    g = nx.Graph()
    
    usrset = set()   
    with open(mfile) as f:
        for x in f.readlines ():
            s=x.strip('\n' )
            usrset.add(s)

    with open(network) as f:
        for x in f.readlines ():
            s = x.strip().split(",")
            if (s[0] in usrset) and (s[1] in usrset):
                    g.add_edge(s[1], s[0])
    return g

# analysis
mfile = 'D:/qjt/paper/social_influence/data/members_largest_comm6.txt'
network = 'D:/qjt/paper/social_influence/data/friends_yelp6.txt'
g = build(network,mfile)

c = average_clustering (g,100000)
print('clustering coefficient:',c)
print('nodes:%i' % len(g.nodes))
print('edges:%i' % len(g.edges))
avg =nx.average_shortest_path_length(g)
print("avg shortest path:%s", avg)

# --- power law ---
nlist = g.nodes()
dlist = g.degree(nlist)
clist=[]
for node in nlist:
    clist.append(dlist[node])

fit = powerlaw.Fit(clist)
print(fit.power_law.alpha)
R, p = fit.distribution_compare('power_law', 'exponential', normalized_ratio = True)
print(R,p)

