import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

G = nx.read_adjlist("results/adj_list.dat")

nx.draw_networkx(G)
plt.show()



